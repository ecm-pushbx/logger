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

Header:
	ChainNext: 	dd -1			; pointer to next device driver
	Attributes:	dw 0x8000		; character device
	Strategy:	dw Driver.Strategy	; set request block pointer
	Entry:		dw Driver.Routine	; driver interrupt call
	Name:		DeviceDriverID		; 8 character driver name

; -----------------------------------------------------------------------------
; In addition to the DeviceDriverID, the pointer is tested by LOG.COM to
; verify it is actually a running driver and not just as copy of the program
; residing in memory.

Request:		dd -1			; Pointer to tREQUEST block

; -----------------------------------------------------------------------------
; Data that is interacted with directly by LOG.COM program

Status:			dw 0			; Device driver Status
						; Bit 0 = active
						; Bit 1 = Mode Reset Flag
						; Bit 2 = DirectVideo Mode
						; Bit 3 = EGA or better

XMS:
	.Driver:	dd -1			; Pointer to XMS driver
	.Size:		dw 256			; Size in KB to allocate
	.Head:		dd 0
	.Tail:		dd 0
	.Count:		dd 0


XFR:						; XMS transfer Buffer
	.Count:		dd 0			; byte count { must be even }
	.SrcHandle:	dw 0			; 0 = conventional memory
	.SrcAddr:	dd 0			; pointer to source buffer
	.DstHandle:	dw -1			; XMS handle
	.DstAddr:	dd 0			; pointer to destination

; -----------------------------------------------------------------------------
; Other data private data

%define MaxTTLSize  80 * 2

XFR.Buffer:	times MaxTTLSize db 0		; TTL Buffer

BIOSInt10:		dd -1			; Original BIOS Int 10
LastCaptured:		dw -1			; Last Screen Row Logged

; -----------------------------------------------------------------------------

Driver.Strategy:				; set request block pointer
	mov		[cs:Request], bx
	mov		[cs:Request+2], es
	retf

Driver.Routine:					; driver function dispatcher
	pushf
	push		ax
	push		di
	push		es

	les		di, [cs:Request]
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
	test		[Status], byte 0x01
	jz		.CheckModeChange

	; was mode previously changed?
	test		[Status], byte 0x02
	jz		.NoModeResetFlag

	; mode was changed, adjust settings
	push		es
	push		ax
	and		[Status], byte 11111001b ; clear reset and method flags
	test		[Status], byte 00001000b ; is EGA or better?
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
	; or		[Status], 00000100b	; set direct method flag
	jmp		.SetMethodDone
.SetTTLMethod:
	mov		[XFR.Count], word 0 ; don't worry about high word
	mov		[XFR.SrcAddr], word XFR.Buffer
	mov		[XFR.SrcAddr+2], cs
.SetMethodDone:
	pop		ax
	pop		es

.NoModeResetFlag:
	; test capture method
	test		[Status], byte 00000100b ; direct method flag
	jnz		.CaptureDirect
	; simple TTL capture
	cmp		ah, 0x0e
	jne		.AdjustNone
	push		di
	mov		di, [XFR.Count]
	mov		[XFR.Buffer+di], al	; character
	inc		di
	mov		[XFR.Buffer+di], bl	; attribute
	inc		di
	mov		[XFR.Count], di
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
	jz		.AdjustNone
.AdjustModeChange:
	call		SendToXMS
	or		[Status], byte 00000010b ; set mode change flag
	jmp		.AdjustNone
.AdjustScroll:
	test		cx, cx
	jnz		.AdjustNotFull

.AdjustNotFull:
.AdjustNone:
	pop		ds
	popf
	jmp		far [cs:BIOSInt10]

SendToXMS:
	cmp		[XFR.Count], word 0
	je		.Done

	; still need to handle buffer wrapping!!!!!!!

	cpu	286
		pusha
		mov		si, XFR
		mov		ax, [XMS.Head]
		mov		[XFR.DstAddr], ax
		mov		ax, [XMS.Head+2]
		mov		[XFR.DstAddr], ax
		mov		ah, 0x0b
		call far 	[XMS.Driver]
		test		ax, ax
		popa
		jz		.Done
		push		ax
		mov		ax, [XFR.Count]
		add		[XMS.Count], ax
		adc		[XMS.Count+2], word 0
		add		[XMS.Head], ax
		adc		[XMS.Head+2], word 0
		pop		ax
	cpu	8086

.Done:
	mov		[XFR.Count], word 0	; set current count to 0
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
	mov		[XMS.Driver], bx
	mov		[XMS.Driver+2], es
	pop		es

	; Allocate XMS Memory
	mov		ah, 0x09
	mov		dx, [XMS.Size]		; Size in KB
	call		far [XMS.Driver]
	mov		[XFR.DstHandle], dx	; save XMS Handle
	test		ax, ax
	jnz		.XMSAllocated
	mov		dx, NoXMSAlloc
	jmp		.FailedMessage

.XMSAllocated:
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
	or		[Status], byte 00000011	; set enable and mode reset

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
	; Free driver
	mov		[es:di+tREQUEST.Address], word Header
	jmp		Driver.Error

CommonCode

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
