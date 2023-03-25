; Boot Message Logger

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
		; else sticks to 8086 instructions. XMS requires a 286, but
		; I may add support for a small conventional memory buffer
		; so it can run on older hardware.

org 0x0000

section .text

%include "common.inc"

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

	at .Status,		dw 0		; Device driver Status


	at .XMS.Driver,		dd -1		; Pointer to XMS driver
	at .XMS.Size,		dw 256		; Size in KB to allocate
	at .XMS.Head,		dd 0		; next buffer write position
	at .XMS.Tail,		dd 0		; first buffer read position
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
	mov		di, [Header(XFR.Count)]
	mov		[XfrTTLBuffer+di], al	; character
	inc		di
	mov		[XfrTTLBuffer+di], bl	; attribute
	inc		di
	mov		[Header(XFR.Count)], di
	cmp		di, MaxTTLSize		; send if TTL buffer is full
	pop		di
	je		.SendTTLBuffer
	cmp		al, 0x0a		; also send on LF
	jne		.AdjustNone
.SendTTLBuffer:
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
	je		.Done

	; still need to handle buffer wrapping!!!!!!!

	cpu	286
		pusha
		mov		si, Header(XFR.Count) ; First item in record
		mov		ax, [Header(XMS.Head)]
		mov		[Header(XFR.DstAddr)], ax
		mov		ax, [Header(XMS.Head)+2]
		mov		[Header(XFR.DstAddr)+2], ax
		mov		ah, 0x0b
		call far 	[Header(XMS.Driver)]
		test		ax, ax
		popa
		jz		.Done
		push		ax
		mov		ax, [Header(XFR.Count)]
		add		[Header(XMS.Count)], ax
		adc		[Header(XMS.Count)+2], word 0
		add		[Header(XMS.Head)], ax
		adc		[Header(XMS.Head)+2], word 0
		pop		ax
	cpu	8086

.Done:
	mov		[Header(XFR.Count)], word 0	; set current count to 0
	ret

; -----------------------------------------------------------------------------
; Released after Initialization
; -----------------------------------------------------------------------------

Initialize:
	; set pointer to first byte above program for DOS to free after init
	mov		[es:di+tREQUEST.Address+2], cs
	mov		[es:di+tREQUEST.Address], word Initialize

	; save remaining general registers not preserved in function dispatcher
	push		ds
	push		si
	push		dx
	push		cx
	push		bx

	push		cs
	pop		ds	; mov ds, cs

	; print driver banner text
	mov		dx, Message
	mov		ah, 0x09
	int		0x21

	; debugging - print "Command Line" of driver initialize
	; lds		si, [es:di+tREQUEST.CommandLine]
	; mov		ah, 0x02
	; mov		cx, 100
;.PrintLoop:
	; mov		dl, [si]
	; inc		si
	; cmp		dl, 0x0a
	; jbe		.PrintDone
	; int		0x21
	; dec		cx
	; jnz		.PrintLoop
;.PrintDone:
	; mov		dl, 0x0d
	; int		0x21
	; mov		dl, 0x0a
	; int		0x21

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
	; print driver banner text
	mov		dx, Activated
	mov		ah, 0x09
	int		0x21
	clc
	jmp		.Finished

.FailedMessage:
	mov		ah, 0x09
	int		0x21
.Failed:
	mov		dx, NotActivated
	mov		ah, 0x09
	int		0x21
	stc

.Finished:
	pushf
	mov		dx, NewLine
	mov		ah, 0x09
	int		0x21
	int		0x21
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

CommonCode

; -----------------------------------------------------------------------------

Message:
	db	0x0d,0x0a
	db	'System Boot Message Logger, v0.1',0x0d,0x0a
	CopyrightText
	db	'$'
NoXMSDriver:
	db	'XMS driver not installed.',0x0d,0x0a,'$'
NoXMSAlloc:
	db	'Unable to allocate XMS memory.',0x0d,0x0a,'$'
NotActivated:
	db	'Boot message logging not enabled.$'
Activated:
	db	'Boot message logging enabled.$'
NewLine:
	db 	0x0d,0x0a,'$'
