; Boot Message Logger Interface Utility

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

Initialize:
	push		cs
	pop		es
	ParseOptions	HelpTable, 0x81			; ds:OptionTable
							; es:CommandLine

	FindDeviceDriver
	jnc		DriverFound

	; test for special help or version request
	test		[Flags], byte ofShowHelp
	jz		.NotShowHelp
	call		PrintHelp
	jmp		ExitNoError
.NotShowHelp:
	test		[Flags], byte ofShowVersion
	jz		.NotShowVersion
	call		PrintVersion
	jmp		ExitNoError

.NotShowVersion:
	mov		dx, NoDriver

ErrorExit:
	PrintMessage	dx
	; Terminate with error code 1
	mov		ax,0x4c01
	int		0x21

DieXMSError:
	; Print XMS error message and code
	push		bx ; has error code
	PrintMessage	XMSError
	pop		ax ; was bx
	WordAsHex	ax
	mov		dx, CRLF
	jmp		ErrorExit

; -----------------------------------------------------------------------------

DriverFound:
	mov		[DriverSeg], es
	; save driver status
	mov		ax, [es:Header(Status)]
	mov		[OrgStat], ax

	; disable driver
	and		al , 0xfe			; Not sfEnabled bit flag
	mov		[es:Header(Status)], ax

	; test for special help or version request
	test		[Flags], byte ofShowHelp
	jz		.NotShowHelp
	call		PrintHelp
	jmp		.HadOptions
.NotShowHelp:
	test		[Flags], byte ofShowVersion
	jz		.NotShowVersion
	call		PrintVersion
.NotShowVersion:

	push		es
	push		cs
	pop		es
	ParseOptions	OptionTable, 0x81		; ds:OptionTable
							; es:CommandLine
	pop		es

	test		[Flags], byte ofHadOptions
	jnz		.HadOptions

	; empty command line, perform default function
	call		Option_Print

.HadOptions:
	; check if we should set driver to enabled/disabled state, or stay off
	test		[Flags], byte ofKeepStatus
	jz		ExitNoError
	mov		ax, [OrgStat]
	mov		[es:Header(Status)], ax

ExitNoError:
	; Terminate, no error
	mov		ax, 0x4c00
	int 		0x21

; -----------------------------------------------------------------------------

HelpTable:
	; used to pre-test for help and validate options
	dw		Option_Version
	db		'VERSION', 0
	dw		Option_IgnoreRest
	db 		'MSG', 0
	dw		Option_Help
	db 		'HELP', 0
	dw		Option_Help
	db 		'/HELP', 0
	dw		Option_Help
	db 		'/H', 0
	dw		Option_Help
	db 		'/?', 0
	dw		Option_Skip
	db		'OFF', 0
	dw		Option_Skip
	db	 	'ON', 0
	dw		Option_Skip
	db		'CLEAR', 0
	dw		Option_Skip
	db		'PRINT', 0
	dw		Option_Skip
	db	 	'ANSI', 0
	dw		Option_Skip
	db	 	'VIEW', 0
	dw		Option_Skip
	db	 	'STDIN', 0
	dw		0,Option_Bad	; catch all
; -----------------------------------------------------------------------------

OptionTable:
	dw		Option_Skip
	db 		'VERSION', 0
	dw		Option_Off
	db		'OFF', 0
	dw		Option_On
	db	 	'ON', 0
	dw		Option_Clear
	db		'CLEAR', 0
	dw		Option_Print
	db		'PRINT', 0
	dw		Option_Ansi
	db	 	'ANSI', 0
	dw		Option_View
	db	 	'VIEW', 0
	dw		Option_Msg
	db	 	'MSG', 0
	dw		Option_StdIn
	db	 	'STDIN', 0
	dw		0,Option_Bad ; catch all

; -----------------------------------------------------------------------------

Option_Help:
	or		[Flags], byte ofShowHelp
	or		[Flags], byte ofKeepStatus
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Version:
	or		[Flags], byte ofShowVersion
	jmp		Option_Done

; -----------------------------------------------------------------------------
Option_Bad:
	PrintMessage 	BadOptionPre
	PrintOptionText
	mov		dx, BadOptionPost
	jmp		ErrorExit

Option_IgnoreRest:
	mov		al, [es:di]
	cmp		al, 0x0d
	jbe		Option_Done
	inc		di
	jmp		Option_IgnoreRest
; -----------------------------------------------------------------------------

Option_Off:
	; force off regardless of what command line options are used.
	and		[OrgStat], byte 11111110b	; not sfEnabled
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_On:
	; force on regardless of what command line options are used.
	or		[OrgStat], byte sfEnabled
	or		[Flags], byte ofKeepStatus
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Clear:
	push		es
	mov		es, [DriverSeg]
	; reset log buffer control data
	mov		al, [OrgStat]
	and		al, 10111111b		; not sfLogFull
	mov		[es:Header(Status)], al ; clear sfLogFull bit
	mov		[OrgStat], al		; update original state
	xor		ax, ax
	mov		[es:Header(XMS.Count)], ax	; 0
	mov		[es:Header(XMS.Count)+2], ax	; 0
	mov		[es:Header(XMS.Head)], ax	; 0
	mov		[es:Header(XMS.Head)+2], ax	; 0
	dec		ax
	mov		[es:Header(XMS.Tail)], ax	; -1
	mov		[es:Header(XMS.Tail)+2], ax	; -1
	pop		es
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Print:
	push		es
	mov		es, [DriverSeg]
	and		[Flags], byte 11111011b ; not ofColorPrint
	call		PrintLog
	pop		es
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Ansi:
	push		es
	mov		es, [DriverSeg]
	or		[Flags], byte ofColorPrint
	call		PrintLog
	pop		es
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_View:
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Msg:
	jmp		Option_IgnoreRest

; -----------------------------------------------------------------------------

Option_StdIn:
	jmp		Option_Done

; -----------------------------------------------------------------------------
Option_Skip:
Option_Done:
	or		[Flags], byte ofHadOptions ; there was an option flag
	ret

; -----------------------------------------------------------------------------

PrintHelp:
	mov		dx, HelpText
	jmp		DoPrintMessage
; -----------------------------------------------------------------------------

PrintVersion:
	mov		dx, Banner

DoPrintMessage:
	PrintMessage
	ret

; -----------------------------------------------------------------------------

PrintLog:
	; When not sfLogJam mode, probably going to have printing skip forward
	; to first CRLF when the log is full. That would prevent displaying
	; partial lines when buffer has wrapped and is full.

	; test if log is empty. There are two methods of checking for that.
	; Either by checking the TAIL pointer is not -1, or by checking that
	; the Count is not 0. Checking the Count uses a couple less bytes.
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
	test		[Flags], byte ofColorPrint
	jz		.ColorIsSet
	cmp		dh, [LastColor]
	je		.ColorIsSet
	mov		[LastColor], dh

	; write ansi color change sequence
	xor		bh,bh
	push		dx
	PrintMessage 	AnsiPrefix
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
	PrintMessage 	LogEmpty

.Done:
	ret

; -----------------------------------------------------------------------------

CommonCode

; -----------------------------------------------------------------------------

section .data

DriverID:
	DeviceDriverID

Banner:
	db	'System Boot Message Log Utility, v0.1',0x0d,0x0a
	CopyrightText
	db	'$'
NoDriver:
	db	'LOGGER.SYS driver is not loaded.',0x0d,0x0a,'$'

BadOptionPre:
	db	'Invalid option "$'
BadOptionPost:
	db	'" provided to LOGGER.',0x0d,0x0a,'$'

LogEmpty:
	db	'Log is empty.'

CRLF:
	db 	0x0d,0x0a,'$'

XMSError:
	db	'XMS error #$'

AnsiPrefix:
	db	27,'[0;$'

HelpText:
	incbin  'help.inc'
	db	'$'

AnsiColors:
	db	0x30,0x34,0x32,0x36,0x31,0x35,0x33,0x37

Flags:
	db	ofColorPrint

LastColor:
	db	0

; -----------------------------------------------------------------------------

section .bss

DriverSeg:		resw 1		; Segment of Logger.sys driver

OrgStat: 		resw 1		; Original Driver Status at Startup

XFR:
	.Count:		resd 1		; byte count { must be even }
	.SrcHandle:	resw 1		; XMS Handle
	.SrcAddr:	resd 1		; pointer to source buffer
	.DstHandle:	resw 1		; ; 0 = conventional memory
	.DstAddr:	resd 1		; pointer to destination

XMS_Buffer:	resb 	XMS_Buffer_Size