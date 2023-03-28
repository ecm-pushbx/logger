; Boot Message Logger Device Driver

; BSD 3-Clause License
; Copyright (c) 2023, Jerome Shidel

; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:

; 1. Redistributions of source code must retain the above copyright notice, this
;    list of conditions and the following disclaimer.

; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.

; 3. Neither the name of the copyright holder nor the names of its
;    contributors may be used to endorse or promote products derived from
;    this software without specific prior written permission.

; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; NASM 2.15.05, or later


use16


cpu 8086	; except for the section that writes to XMS memory, everything
		; else sticks to 8086 instructions. XMS requires a 286. But,
		; I may add support for a small conventional memory buffer
		; so it can run on older hardware.

org 0x0000

section .text

%include "common.inc"

%idefine XMS_DefSize 	256 		; Default KB size of XMS Log Buffer


struc tREQUEST
	.Length:	resb 1		; requested structure length
	.Unit:		resb 1		; unit number for request
	.Function:	resb 1		; requested function
	.Status:	resw 1		; return status
	.Reserved:	resb 8		; reserved
	.Descriptor:	resb 1		; +13, media descriptor BYTE
	.Address:  	resd 1		; +14, transfer address pointer DWORD
					; also, return address byte above
					; program to release after initialized
	.CommandLine:			; +18, init function DWORD
	.Count:		resw 1		; +18
	.Start:		resw 1		; +20
	.Drive:		resb 1		; +22
endstruc

; -----------------------------------------------------------------------------

DriverHeader:
istruc TDriverHeader
	at .ChainNext, 	dd -1			; pointer to next device driver
	at .Attributes,	dw 0x8000		; character device
	at .Strategy,	dw Driver.Strategy	; set tREQUEST block pointer
	at .Entry,	dw Driver.Routine	; driver interrupt call
	at .Name,	DeviceDriverID		; 8 character driver name

; -----------------------------------------------------------------------------
; In addition to the DeviceDriverID, the pointer is tested by LOG.COM to
; verify it is actually a running driver and not just as copy of the program
; residing in memory.

	at .Request,		dd -1		; Pointer to tREQUEST block

; -----------------------------------------------------------------------------
; Data that is interacted with directly by LOG.COM program

	at .Status,		dw sfInColor	; Device driver Status
						; default capture in color

	at .XMS.Driver,		dd -1		; Pointer to XMS driver
	at .XMS.Size,		dw XMS_DefSize	; Size in KB to allocate
	at .XMS.Head,		dd 0		; next buffer write position
	at .XMS.Tail,		dd -1		; first buffer read position
	at .XMS.Count,		dd 0		; bytes in buffer
	at .XMS.Max,		dd 0 		; size of buffer in bytes


						; XMS transfer Buffer
	at .XFR.Count,		dd 0		; byte count { must be even }
	at .XFR.SrcHandle,	dw 0		; 0 = conventional memory
	at .XFR.SrcAddr,	dd 0		; pointer to source buffer
	at .XFR.DstHandle,	dw -1		; XMS handle
	at .XFR.DstAddr,	dd 0		; pointer to destination

iend
; -----------------------------------------------------------------------------

%define Header(x) DriverHeader + TDriverHeader. %+ x
%define MaxTTLSize  80 * 2

; -----------------------------------------------------------------------------
; Other data private data

XfrTTLBuffer:	times MaxTTLSize db 0		; TTL Buffer

BIOSInt10:		dd -1			; Original BIOS Int 10
LastCaptured:		dw -1			; Last Screen Row Logged

; -----------------------------------------------------------------------------

Driver.Strategy:				; set request block pointer
	mov		[cs:Header(Request)], bx
	mov		[cs:Header(Request)+2], es
	retf

Driver.Routine:					; driver function dispatcher
	pushf
	push		ax
	push		di
	push		es

	les		di, [cs:Header(Request)]
	mov		al, [es:di+tREQUEST.Function]
	test		al, al			; test command 0, initialize
	jz		Initialize

Driver.Error:
	mov		ax, 0x8103		; set error & done bits, and
						; error code 3, unknown command
Driver.Return:
	mov		[es:di+tREQUEST.Status], ax
	pop		es
	pop		di
	pop		ax
	popf
	retf

; -----------------------------------------------------------------------------
; Device Driver Interrupt 10 Handler
; -----------------------------------------------------------------------------

DevInt10:
	pushf
	cli
	push		ds
	push		cs
	pop		ds	; mov ds, cs

	; is driver enabled?
	test		[Header(Status)], byte sfEnabled
	jz		.CheckModeChange

	; was mode previously changed?
	test		[Header(Status)], byte sfModeChange
	jz		.NoModeResetFlag

	; mode was changed, adjust settings
	push		es
	push		ax
	; clear ModeChange and DirectMethod bits
	and		[Header(Status)], byte 11111001b

	test		[Header(Status)], byte sfEGAorBetter
	jz		.SetTTLMethod
	; in a supported text mode?
	mov		ax, 0x0040
	push		ax
	pop		es
	mov		al, [es:0x0049]		; current video mode
	and		al, 0x7f
	cmp		al, 0x07		; is 80x25 mono?
	je		.SetDirectMethod
	cmp		al, 0x03		; not 80x? or 40x?
	ja		.SetTTLMethod
.SetDirectMethod:
	mov		[LastCaptured], word -1 ; set last captured line
	; or		[Header(Status)], sfDirectMode	; set direct method flag
	jmp		.SetMethodDone
.SetTTLMethod:
	mov		[Header(XFR.Count)], word 0 ; don't worry about high word
	mov		[Header(XFR.SrcAddr)], word XfrTTLBuffer
	mov		[Header(XFR.SrcAddr)+2], cs
.SetMethodDone:
	pop		ax
	pop		es

.NoModeResetFlag:
	; test capture method
	test		[Header(Status)], byte sfDirectMode ; direct method flag
	jnz		.CaptureDirect
	; simple TTL capture
	cmp		ah, 0x0e
	jne		.AdjustNone
	push		di
	; bh = page, for now don't care
	mov		di, [Header(XFR.Count)]
	mov		[XfrTTLBuffer+di], al	; character
	inc		di
	test		[Header(Status)], byte sfInColor
	jz		.NoColorAttrib
	mov		[XfrTTLBuffer+di], bl	; attribute
	inc		di
.NoColorAttrib:
	mov		[Header(XFR.Count)], di
	cmp		di, MaxTTLSize		; send if TTL buffer is full
	pop		di
	je		.SendTTLBuffer
	cmp		al, 0x0a		; also send on LF
	jne		.AdjustNone
.SendTTLBuffer:
	; In case mono capture, make sure we have an even number of bytes
	test		[Header(XFR.Count)], byte 1
	jz		.SendTTLNow
	; if not, then slap a zero on the end
	push		di
	mov		di, [Header(XFR.Count)]
	mov		[XfrTTLBuffer+di], byte 0
	inc		di
	mov		[Header(XFR.Count)], di
	pop		di
.SendTTLNow:
	call		SendToXMS
	jmp		.AdjustNone

.CaptureDirect:
	; Direct video capture
	; call		Capture

	cmp		ah, 0x07	; Scroll Down
	je		.AdjustScroll
	cmp		ah, 0x06	; Scroll Up
	je		.AdjustScroll
.CheckModeChange:
	test		ah, ah
	jnz		.AdjustNone
.AdjustModeChange:
	call		SendToXMS
	or		[Header(Status)], byte sfModeChange ; set flag
	jmp		.AdjustNone
.AdjustScroll:
	test		cx, cx
	jnz		.AdjustNotFull

.AdjustNotFull:
.AdjustNone:
	pop		ds
	popf
	jmp		far [cs:BIOSInt10]

; -----------------------------------------------------------------------------

SendToXMS:
	cmp		[Header(XFR.Count)], word 0
	je		.NoLogData

	; test if sfLogJam (stop when full) bit is set (only set by driver when
	; it is initialized) and if the Log is now Full (always once wraps)
	test 		[Header(Status)], byte sfLogJam
	jz		.NoStopWhenFull
	test 		[Header(Status)], byte sfLogFull
	jnz		.Done
.NoStopWhenFull:

	cpu	286
		pusha
		; set SI to first item in XFR record and populate record
		mov		si, Header(XFR.Count)
		mov		ax, [Header(XMS.Head)]
		mov		[Header(XFR.DstAddr)], ax
		mov		dx, [Header(XMS.Head)+2]
		mov		[Header(XFR.DstAddr)+2], dx

		; set bx:cx to end of write position
		mov		bx, dx		; copy high word of position
		mov		cx, [si]	; Count
		add		cx, ax		; add low word of position
		adc		bx, 0		; inc bx if needed

		; does it extend past buffer end and need wrapped?
		cmp		bx, [Header(XMS.Max)+2]
		jb		.SendWholeBuffer
		cmp		cx, [Header(XMS.Max)]
		jae		.SendSplitBuffer
	.SendWholeBuffer:
		call		.SendBuffer
		jmp		.SendDone
	.SendSplitBuffer:
		; ok we need to split the buffer and send it in two pieces

		; figure out how many bytes are in second send
		sub		cx, [Header(XMS.Max)]
		push		cx  			; save for later

		; figure out how many bytes in first part
		mov		bx, [si]	; [Header(XFR.Count)]
		sub		bx, cx

		; set buffer count for first part and send
		mov		[si], bx
		call		.SendBuffer

		; set buffer count for second part
		pop		cx
		mov		bx, [si]	; [Header(XFR.Count)]
		mov		[si], cx

		; adjust send buffer offset for part two and send
		push		bx	; we will restore it after call
		add		[Header(XFR.SrcAddr)], bx ; adjust send buffer
		call		.SendBuffer
		pop		bx
		sub		[Header(XFR.SrcAddr)], bx ; fix send buffer
		jmp		.SendDone

	.SendBuffer:

		push		si		; save just in case
		mov		ah, 0x0b	; func 0x0b, DS:SI->XFR Record
		call far 	[Header(XMS.Driver)]
		pop		si

		push		cs
		pop		ds		; mov ds, cs - just in case
		test		ax, ax		; test for XMS Error
		jz		.SendDone	; can't do much about it

		; update bytes written count
		mov		ax, [si]	; [Header(XFR.Count)]
		add		[Header(XMS.Count)], ax
		adc		[Header(XMS.Count)+2], word 0
		jnc		.NoCountOverflow
		mov		[Header(XMS.Count)], word 1 ; so it ain't 0
	.NoCountOverflow:

		; save current old head position
		push		word [Header(XMS.Head)+2]
		push		word [Header(XMS.Head)]

		; adjust head for next write
		add		[Header(XMS.Head)], ax
		adc		[Header(XMS.Head)+2], word 0

		; load new head - 1
		mov		cx, [Header(XMS.Head)]
		mov		bx, [Header(XMS.Head)+2]
		sub		cx, 1
		sbb		bx, 0

		; if needed, wrap head back to buffer start
		mov		ax, [Header(XMS.Head)+2]
		cmp		ax, [Header(XMS.Max)+2]
		jb		.NoHeadWrap
		mov		ax, [Header(XMS.Head)]
		cmp		ax, [Header(XMS.Max)]
		jb		.NoHeadWrap
		xor		ax, ax
		mov		[Header(XMS.Head)], ax
		mov		[Header(XMS.Head)+2], ax
		; if log wrapped, it is now in a full state and will remain
		; that way until cleared.
		or		[Header(Status)], byte sfLogFull
	.NoHeadWrap:

		; pop old head into dx:ax
		pop		ax
		pop		dx

		; if new (head - 1) is below tail, no need to move
		cmp		bx, [Header(XMS.Tail)+2]
		ja		.NoTailMove
		cmp		cx, [Header(XMS.Tail)]
		ja		.NoTailMove

		; if old head was above the tail, did not overwrite
		cmp		dx, [Header(XMS.Tail)+2]
		ja		.NotOverwriteTail
		cmp		ax, [Header(XMS.Tail)]
		ja		.NotOverwriteTail

		; we wrote the buffer tail and need to move it to the new head
		; because head is the next place we will write, so is still
		; oldest data in buffer.
		mov		ax, [Header(XMS.Head)]
		mov		[Header(XMS.Tail)], ax
		mov		ax, [Header(XMS.Head)+2]
		mov		[Header(XMS.Tail)+2], ax
		jmp		.NoTailMove
	.NotOverwriteTail:
		; since we did not overwrite the tail, the tail could be in
		; it's invalid initial state of -1. If so, point it at the
		; first byte in the buffer.
		cmp		[Header(XMS.Tail)], word -1
		jne		.NoTailMove
		cmp		[Header(XMS.Tail)+2], word -1
		jne		.NoTailMove
		xor		ax, ax
		mov		[Header(XMS.Tail)], ax
		mov		[Header(XMS.Tail)+2], ax

	.NoTailMove:
		ret
	.SendDone:
		popa
	cpu	8086

.Done:
	mov		[Header(XFR.Count)], word 0	; set current count to 0
.NoLogData:
	ret

; -----------------------------------------------------------------------------
; Released after Initialization
; -----------------------------------------------------------------------------

Initialize:
	; set pointer to first byte above program for DOS to free after init
	mov		[es:di+tREQUEST.Address+2], cs
	mov		[es:di+tREQUEST.Address], word Initialize

	; es, di and ax were saved earlier by the driver function dispatcher
	; interrupt. Now save the remaining general registers that we will use.
	push		ds
	push		si
	push		dx
	push		cx
	push		bx

	push		cs
	pop		ds	; mov ds, cs

	PrintMessage 	Banner

	push		es
	push		di

	; parse driver "command line" options
	les		di, [es:di+tREQUEST.CommandLine]
.SkipToParams:
	mov		al, [es:di]
	cmp		al, 0x20
	jbe		.AtParams
	inc		di
	jmp		.SkipToParams
.AtParams:
	ParseOptions	OptionTable, di

	; check if there were parameters, otherwise process defaults
	cmp		[HadOption], byte 0
	jne		.ParamsDone
	push		cs
	pop		es
	ParseOptions	OptionTable, DefaultOptions

	; done processing parameters
.ParamsDone:
	pop		di
	pop		es

	; Test for XMS Memory
	mov		ax, 0x4300
	int		0x2f
	cmp		al, 0x80
	je		.XMSPresent
	mov		dx, NoXMSDriver
	jmp		.FailedMessage

.XMSPresent:
	; Get Driver Address
	push		es
	mov		ax, 0x4310
	int		0x2f
	mov		[Header(XMS.Driver)], bx
	mov		[Header(XMS.Driver)+2], es
	pop		es

	; Allocate XMS Memory
	mov		ah, 0x09
	mov		dx, [Header(XMS.Size)]		; Size in KB
	call		far [Header(XMS.Driver)]
	mov		[Header(XFR.DstHandle)], dx	; save XMS Handle
	test		ax, ax
	jnz		.XMSAllocated
	mov		dx, NoXMSAlloc
	jmp		.FailedMessage

.XMSAllocated:
	; set byte size of XMS memory allocated
	mov		ax, [Header(XMS.Size)]
	mov		dx, 1024
	mul		dx
	mov		[Header(XMS.Max)], ax
	mov		[Header(XMS.Max)+2], dx

	; Save BIOS Int 10
	push		es
	mov		ax, 0x3510
	int		0x21
	mov		[BIOSInt10], bx
	mov		[BIOSInt10+2], es
	pop		es

	; Install Driver Interrupt 10 handler
	mov		dx, DevInt10
	mov		ax, 0x2510
	int		0x21

	; enable capture
	or		[Header(Status)], byte (sfModeChange + sfEnabled)

.Success:
	; print driver activated text
	Printmessage	Activated
	clc
	jmp		.Finished

.FailedMessage:
	PrintMessage	dx

.Failed:
	PrintMessage	NotActivated
	stc

.Finished:
	pushf
	PrintMessage	NewLine
	int		0x21		; repeat Newline message
	popf

	; restore additional general registers used during initialization
	pop		bx
	pop		cx
	pop		dx
	pop		si
	pop		ds

	mov		ah, 0x01		; done bit, no error bit
	jnc		Driver.Return

	; Free entire device driver by setting pointer to driver start
	mov		[es:di+tREQUEST.Address], word DriverHeader
	jmp		Driver.Error

; -----------------------------------------------------------------------------
Option_XMS:
	ByteAsChar	'X'
	jmp		Option_Done

Option_UMB:
	PrintMessage	NoUMBSupport
	jmp		Option_Done

Option_LOW:
	PrintMessage	NoLOWSupport
	jmp		Option_Done

Option_JAM:
	ByteAsChar	'J'
	jmp		Option_Done

Option_COLOR:
	ByteAsChar	'C'
	jmp		Option_Done

Option_MONO:
	ByteAsChar	'M'
	jmp		Option_Done

Option_Unknown:
	ByteAsChar	'?'
	PrintOptionText

Option_Done:
	; ByteAsChar	0x0d,0x0a
	mov		[HadOption], byte 1
	ret

; -----------------------------------------------------------------------------

OptionTable:
	dw		Option_XMS
	db		'XMS', 0
	dw		Option_UMB
	db		'UMB', 0
	dw		Option_LOW
	db		'LOW', 0
	dw		Option_JAM
	db		'JAM', 0
	dw		Option_COLOR
	db		'COLOR',0
	dw		Option_MONO
	db		'MONO',0

	dw		0, Option_Unknown

; -----------------------------------------------------------------------------

CommonCode

; -----------------------------------------------------------------------------

Banner:
	db	0x0d,0x0a
	db	'System Boot Message Logger, v0.1',0x0d,0x0a
	CopyrightText
	db	'$'

NoXMSDriver:
	db	'XMS driver not installed.',0x0d,0x0a,'$'

NoXMSAlloc:
	db	'Unable to allocate XMS memory.',0x0d,0x0a,'$'
NoUMBAlloc:
	db	'Unable to allocate UMB memory.',0x0d,0x0a,'$'

NoUMBSupport:
	db	'UMB memory not supported.',0x0d,0x0a,'$'
NoLOWSupport:
	db	'LOW memory not supported.',0x0d,0x0a,'$'

NotActivated:
	db	'Boot message logging not enabled.$'
Activated:
	db	'Boot message logging enabled.$'

NewLine:
	db 	0x0d,0x0a,'$'

DefaultOptions:
	db	'256 COLOR XMS 32 MONO UMB 16 MONO LOW',0

HadOption:
	db	0
