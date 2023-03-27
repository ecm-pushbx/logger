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

%define XMS_Buffer_Size 2	; for now, a word at a time is good enough

%define Header(x) TDriverHeader. %+ x

jmp	Initialize

Max:	dd 0x0001fffa
Pos:    dd 0x0001fff5
Cnt:	dw 0x1000

Initialize:

	mov	bx, [Pos+2]
	mov	cx, [Pos]
	WordAsHex bx,cx
	ByteAsChar 0x0d,0x0a

	add	cx, [Cnt]
	adc	bx, 0
	WordAsHex bx,cx
	ByteAsChar 0x0d,0x0a

	mov	bx, [Cnt]
	sub	cx, [Max]
	sub	bx, cx

	WordAsHex  bx
	ByteAsChar 0x0d,0x0a
	WordAsHex  cx
	ByteAsChar 0x0d,0x0a


	FindDeviceDriver
	jnc		DriverFound
	call		PrintVersion
	mov		dx, NoDriver

ErrorExit:
	; Print Message
	mov		ah, 0x09
	int		0x21
	; Terminate with error code 1
	mov		ax,0x4c01
	int		0x21

DieXMSError:
	; Print XMS error message and code
	push		bx ; has error code
	mov		dx, XMSError
	mov		ah, 0x09
	int		0x21
	pop		ax ; was bx
	WordAsHex	ax
	mov		dx, CRLF
	jmp		ErrorExit

; -----------------------------------------------------------------------------

DriverFound:
	; save driver status
	mov		ax, [es:Header(Status)]
	mov		[OrgStat], ax

	; disable driver
	and		al , 0xfe			; Not sfEnabled bit flag
	mov		[es:Header(Status)], ax

	ParseOptions	OptionTable, 0x80		; cs:OptionTable
							; ds:CommandLine

	test		[Flags], byte 00000010b
	jnz		.HadOptions

	; empty command line, perform default function
	call		PrintLog

.HadOptions:

	; Terminate, no error
	mov		ax, 0x4c00
	int 		0x21

; -----------------------------------------------------------------------------

OptionTable:
	dw		Option_Off, 'OFF', 0
	dw		0

; -----------------------------------------------------------------------------

Option_Off:
	jmp		Option_Done

Option_Done:
	or		[Flags], byte 00000010b	; there was an option flag
	clc
	ret

; -----------------------------------------------------------------------------

PrintVersion:
	mov		dx, Message
	mov		ah, 0x09
	int		0x21
	ret

; -----------------------------------------------------------------------------

PrintLog:
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
	jz		DieXMSError	; this leaves stuff on the stack, but
					; that will be cleaned up by DOS at exit
					; Also, we are going to leave logging
					; turned off because of the error.

	; print just the character using DOS
	mov		ah, 0x02
	mov		dx, [XMS_Buffer]
	test		[es:Header(Status)], byte sfInColor
	jnz		.ColorPrint

	int		0x21
	; if second character is not zero, print it
	test		dh, dh
	jz		.PrintedChar
	mov		dl, dh
	int		0x21
	jmp		.PrintedChar

.ColorPrint:
	mov		dx, [XMS_Buffer]
	test		[Flags], byte 00000100b	; Color Printing Mode
	jz		.ColorIsSet
	cmp		dh, [LastColor]
	je		.ColorIsSet
	mov		[LastColor], dh

	; write ansi color change sequence
	xor		bh,bh
	push		dx
	mov		dx, AnsiPrefix
	mov		ah, 0x09
	int		0x21
	pop		dx
	push		dx
	mov		ah, 0x02
	; blink/intensity background?
	test		dh, 0x80
	jz		.SetBackground
	mov		dl, '5'
	int		0x21
	mov		dl, ";"
	int		0x21
.SetBackground:
	mov		dl, '4'
	int		0x21
	mov		cl, 4
	mov		al, dh
	shr		al, cl
	and		al, 7
	mov		bl, al
	mov		dl, [AnsiColors+bx]
	int		0x21
	mov		dl, ";"
	int		0x21
	; is intensity  foreground?
	test		dh, 0x08
	jz		.SetForeground
	mov		dl, '1'
	int		0x21
	mov		dl, ";"
	int		0x21
.SetForeground:
	mov		dl, '3'
	int		0x21
	mov		bl, dh
	and		bl, 7
	mov		dl, [AnsiColors+bx]
	int		0x21
	mov		dl, 'm'
	int		0x21
	pop		dx
.ColorIsSet:
	int		0x21

.PrintedChar:
	; restore XMS pointer and count
	pop		dx
	pop		ax
	pop		cx

	; Next read position
	add		ax, cx
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

AnsiPrefix:
	db	27,'[0;$'

AnsiColors:
	db	0x30,0x34,0x32,0x36,0x31,0x35,0x33,0x37

Flags:
	db	00000100b		; bit 0 = restore driver status on exit
					; bit 1 = Command line options provided
					; bit 2 = Color printing mode
LastColor:
	db	0


; -----------------------------------------------------------------------------

section .bss

OrgStat: 		resw 1		; Original Driver Status at Startup

XFR:
	.Count:		resd 1		; byte count { must be even }
	.SrcHandle:	resw 1		; XMS Handle
	.SrcAddr:	resd 1		; pointer to source buffer
	.DstHandle:	resw 1		; ; 0 = conventional memory
	.DstAddr:	resd 1		; pointer to destination

XMS_Buffer:	resb 	XMS_Buffer_Size