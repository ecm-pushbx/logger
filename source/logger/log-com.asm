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

cpu 8086

org 0x0100

section .text

%include 'common.inc'

%define XMS_Buffer_Size 1024
%define Header(x) TDriverHeader. %+ x

Initialize:
	push		cs
	pop		ds
	FindDeviceDriver
	jnc		DriverFound
	call		PrintVersion
	mov		dx, NoDriver

ErrorExit:
	mov		ah, 0x09
	int		0x21
	mov		ax,0x4c01
	int		0x21

DieXMSError:
	push		bx
	mov		dx, XMSError
	mov		ah, 0x09
	int		0x21
	pop		ax ; was bx
	WordAsHex	ax
	mov		dx, CRLF
	jmp		ErrorExit

; -----------------------------------------------------------------------------

DriverFound:
	call		PrintLog
	mov		ax, 0x4c00
	int 		0x21

; -----------------------------------------------------------------------------

PrintVersion:
	mov		dx, Message
	mov		ah, 0x09
	int		0x21
	ret

; -----------------------------------------------------------------------------

PrintLog:
	; if need be, disable driver
	and		[es:Header(Status)], bye 0xfe

	mov		ax,[es:Header(XMS.Count)]
	or		ax,[es:Header(XMS.Count)+2]
	jz		.Empty

	; prepare XMS transfer record
	mov		ax, [es:Header(XFR.DstHandle)]
	mov		[XFR.SrcHandle], ax
	mov		[XFR.DstHandle], word 0
	mov		[XFR.DstAddr], word XMS_Buffer
	mov		[XFR.DstAddr+2], cs

	; load first item pointer
	mov		ax, [es:Header(XMS.Tail)]
	mov		dx, [es:Header(XMS.Tail)+2]

	mov		si, XFR.Count	; set pointer to XFR record
.PrintLoop:
	; add stuff to buffer transfers of 1k blocks at a times

	; check if finished
	cmp		ax, [es:Header(XMS.Head)]
	jne		.MoreData
	cmp		dx, [es:Header(XMS.Head)+2]
	jne		.MoreData
	; finished printing
	jmp		.Done

.MoreData:
	mov		cx, 0x0002

	; set transfer record data

	mov		[XFR.Count], cx
	mov		[XFR.Count+2], word 0
	mov		[XFR.SrcAddr], ax
	mov		[XFR.SrcAddr+2], dx

	; save XMS pointer and count
	push 		cx
	push		ax
	push		dx

	; fetch data
	mov		ah, 0x0b
	call far 	[es:Header(XMS.Driver)]
	test		ax,ax
	;jz		DieXMSError	; this leaves stuff on the stack, but
					; that will be cleaned up by DOS at exit
					; Also, we are going to leave logging
					; turned off because of the error.

	; print just the character using DOS
	mov		ah, 0x02
	mov		dl, [XMS_Buffer]
	int		0x21

	; restore XMS pointer and count
	pop		dx
	pop		ax
	pop		cx

	; Next read position
	add		ax, 2
	adc		dx, 0

	; test if buffer wrap
	cmp		dx, [es:Header(XMS.Max)+2]
	jb		.PrintLoop
	cmp		ax, [es:Header(XMS.Max)]
	jb		.PrintLoop

	xor		ax, ax
	mov		dx, ax
	jmp		.PrintLoop


.Empty:
	mov		dx, LogEmpty
	mov		ah, 0x09
	int		0x21

.Done:
	ret

; -----------------------------------------------------------------------------

CommonCode

; -----------------------------------------------------------------------------

section .data

DriverID:
	DeviceDriverID

Message:
	db	'System Boot Message Log Utility, v0.1',0x0d,0x0a
	CopyrightText
	db	'$'
NoDriver:
	db	'LOGGER.SYS driver is not loaded.',0x0d,0x0a,'$'

LogEmpty:
	db	'Log is empty.'

CRLF:
	db 	0x0d,0x0a,'$'

XMSError:
	db	'XMS error #$'


; -----------------------------------------------------------------------------

section .bss

XFR:
	.Count:		resd 1		; byte count { must be even }
	.SrcHandle:	resw 1		; XMS Handle
	.SrcAddr:	resd 1		; pointer to source buffer
	.DstHandle:	resw 1		; ; 0 = conventional memory
	.DstAddr:	resd 1		; pointer to destination

XMS_Buffer:	resb 	XMS_Buffer_Size