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

%ifdef SINGLE_BINARY
	incbin "LOGGER.BIN"
%endif

%include 'common.inc'

%define Buffer_Size 2	; for now, a word at a time is good enough

%define COLOR_MESSAGE	0x0e
%define COLOR_STDIN	0x0f
%define COLOR_MONO	0x07
%define COLOR_SNAPSHOT  0x0b2d

%define Header(x) TDriverHeader. %+ x

Initialize:
	; ES & DS should already be pointed at CS.
	; push		cs
	; pop		ds
	; push		cs
	; pop		es

	ParseOptions	OptionTable, 0x81		; cs:OptionTable
							; es:CommandLine

	and		[Flags], byte ~ofPreTest

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
	test		[Flags], byte ofBadVersion
	jz		ErrorExit
	mov		dx, BadDriver

ErrorExit:
	test		[Flags+1], byte ofHushMode
	jnz		.NoMessage
	PrintMessage	dx
.NoMessage:
	; Terminate with error code 1
	mov		ax,0x4c01
	int		0x21

DieXMSError:
	; Print XMS error message and code, then terminate
	; this leaves stuff on the stack, but that will be cleaned up by DOS
	; at exit. Also, we are going to leave logging turned off because of
	; the error.
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

	; make sure buffer contents have been written.
	call		DriverFlush

	; disable driver
	and		al , ~sfEnabled
	mov		[es:Header(Status)], ax

	; test if StdIn is redirected
	mov		ax, 0x4400
	xor		bx, bx		; file handle 0 = StdIn
	int		0x21
	jc		.NoStdInput	; error
	test		dl, 0x80
	jnz		.NoStdInput	; StdIn is not redirected

	; send any standard input text to Log
	StdIn
	jc		.NoStdInput
	or		[Flags], byte ofStdIn + ofKeepStatus + ofHadOptions

	mov		ah, COLOR_STDIN
.LoopStdIn:
	test		[Flags+1], byte ofPassThru
	jz		.NoPassThru
	StdOut
.NoPassThru:
	call		AppendBuffer
	StdIn
	jnc		.LoopStdIn
	call		DriverFlush

.NoStdInput:

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

	; if there was StdIn added to the Log, do not default to viewer
	test		[Flags], byte ofStdIn
	jnz		.HadOptions
	; empty command line, perform default function
	%ifdef		DEBUG
		call	Option_Debug
	%else
		call	Option_View
	%endif

.HadOptions:
	; update driver RowSkip count
	push		es
	mov		ax, 0x0040
	mov		es, ax
	mov		al, [es:0x0050 + 1] 	; page 0 cursor row position
	pop		es
	xor		ah, ah
	; inc		ax ; because of "INC CX" in driver, would cause line to be skipped.
	mov		[es:Header(RowSkip)], ax

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

;DriverStatus:
;	push		es
;	mov		al, 0x10		; return current status in CX
						; DX:BX->Size info record
;	mov		es, [DriverSeg]
;	call		far [es:Header(Dispatch)]
;	pop		es
;	ret

DriverFlush:
	push		es
	push		ax
	mov		al, 0x12
	mov		es, [DriverSeg]
	call		far [es:Header(Dispatch)]
	pop		ax
	pop		es
	ret

DriverClear:
	push		es
	push		ax
	mov		al, 0x13
	mov		es, [DriverSeg]
	call		far [es:Header(Dispatch)]
	and		[OrgStat], byte ~sfLogFull
	pop		ax
	pop		es
	ret

; -----------------------------------------------------------------------------

OptionTable:
	; full name options
	dw		Option_Help
	db 		'HELP', 0
	dw		Option_Version
	db 		'VERSION', 0
	dw		Option_Status
	db	 	'INFORMATION', 0
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
%ifdef HTML_SUPPORT
	dw		Option_HTML
	db	 	'HTML', 0
%endif
	dw		Option_Msg
	db	 	'MESSAGE', 0
	dw		Option_View
	db	 	'VIEW', 0
	dw 		Option_Snapshot
	db		'SNAPSHOT', 0
	dw 		Option_PassThru
	db		'THRU', 0
	dw 		Option_Quiet
	db		'QUIET', 0
	; abbreviated options
	dw		Option_Version
	db 		'VER', 0
	dw		Option_Status
	db	 	'I', 0
	dw		Option_Clear
	db		'C', 0
	dw		Option_Print
	db		'P', 0
	dw		Option_Ansi
	db	 	'A', 0
%ifdef HTML_SUPPORT
	dw		Option_HTML
	db	 	'H', 0
%endif
	dw		Option_Msg
	db	 	'M', 0
	dw		Option_View
	db	 	'V', 0
	dw 		Option_Snapshot
	db		'S', 0
	dw 		Option_PassThru
	db		'T', 0
	dw 		Option_Quiet
	db		'Q', 0
%ifdef DEBUG
	dw		Option_Debug
	db		'DEBUG',0
	dw		Option_Debug
	db		'D',0
%endif
	; help alternative names
	dw		Option_Help
	db 		'/?', 0
	dw		Option_Help
	db 		'?', 0
	dw		0,Option_Bad ; catch all

; -----------------------------------------------------------------------------

%ifdef DEBUG
Option_Debug:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	or		[Flags], byte ofKeepStatus
	push		es
	mov		es, [DriverSeg]
	DebugStatus	es
	pop		es
	jmp		Option_Done
%endif

; -----------------------------------------------------------------------------

Option_Help:
	or		[Flags], byte ofShowHelp + ofKeepStatus
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_PassThru:
	or		[Flags], byte ofKeepStatus
	or		[Flags+1], byte ofPassThru ; sets high byte
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Quiet:
	or		[Flags+1], byte ofHushMode ; sets high byte
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Version:
	or		[Flags], byte ofShowVersion + ofKeepStatus
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
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	; force off regardless of what command line options are used.
	and		[OrgStat], byte ~sfEnabled
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_On:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	; force on regardless of what command line options are used.
	or		[OrgStat], byte sfEnabled
	or		[Flags], byte ofKeepStatus
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Clear:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	call		DriverClear
	jmp		Option_Done
; -----------------------------------------------------------------------------

Option_Print:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	push		es
	mov		es, [DriverSeg]
	and		[Flags], byte ~ofColorPrint
	call		PrintLOG
	pop		es
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Ansi:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	push		es
	mov		es, [DriverSeg]
	or		[Flags], byte ofColorPrint
	call		PrintLOG
	pop		es
	jmp		Option_Done

; -----------------------------------------------------------------------------

%ifdef HTML_SUPPORT
Option_HTML:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	push		es
	mov		es, [DriverSeg]
	call		PrintHTML
	pop		es
	jmp		Option_Done
%endif
; -----------------------------------------------------------------------------

Option_View:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	or		[Flags], byte ofKeepStatus

	push		es
	mov		es, [DriverSeg]
	push		di

	VideoSettings
	push		bx			; save video segment for later
	push		dx			; save regen size in words
	call		ScreenSave		; only happens in supported modes

	call		LogViewer

	pop		dx			; restore regin size in words
	pop		bx			; restore video segment
	call		ScreenRestore

	pop		di
	pop		es
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Snapshot:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	or		[Flags], byte ofKeepStatus

	%include "snapshot.inc"

; -----------------------------------------------------------------------------

Option_Msg:
	test		[Flags], byte ofPreTest
	jnz		Option_IgnoreRest
	or		[Flags], byte ofKeepStatus
	call		DriverFlush ; should already be empty, but won't hurt
	cld
	mov		ah, COLOR_MESSAGE
	jmp		.SkipIndent
.SkipChar:
	inc		di
.SkipIndent:
	cmp		[es:di], byte 0x20
	je		.SkipChar
.NextChar:
	mov		al, [es:di]
	inc		di
	call		AppendBuffer
	cmp		al, 0x20
	jb		.AppendDone
	jmp		.NextChar
.AppendDone:
	cmp		al, 0x0d
	jne		.SkipLF
	mov		al, 0x0a
	call		AppendBuffer
.SkipLF:
	call		DriverFlush
.Done:
	jmp		Option_Done

; -----------------------------------------------------------------------------

;Option_HotKey:
;	test		[Flags], byte ofPreTest
;	jnz		Option_Done
;	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Status:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	or		[Flags], byte ofKeepStatus
	PrintStatus	[DriverSeg]
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

FetchXMS:
	push		ax
	push		si
	and		al, 0xfe
	mov		si, XFR.Count
	mov		[si], word 0x0002
	mov		[si+2], word 0
	mov		[XFR.SrcAddr], ax
	mov		[XFR.SrcAddr+2], dx
	mov		ah, 0x0b
	push		dx	; just in case
	push		ds	; just in case
	push		es	; just in case
	call far 	[es:Header(XMS.Driver)]
	pop		es
	pop		ds
	pop		dx
	test		ax,ax
	jz		DieXMSError
	pop		si
	pop		ax
	mov		bx, [Buffer]
	test		al, 1 		; odd bytes are only requested when log
	jnz		.HighByte	; is stored in monochrome
	test		[OrgStat], byte sfInColor
	jz		.Monochrome
	clc
	ret
.HighByte:
	mov		bl, bh
.Monochrome:
	mov		bh, COLOR_MONO
	clc
	ret

; -----------------------------------------------------------------------------

StringToLog:
	; ah=color
	; ds:si->String
	cld
	push		si
.Next:
	lodsb
	cmp		al, 0x20
	jb		.Done
	cmp		al, '$'
	je		.Done
	call		AppendBuffer
	jmp		.Next
.Done:
	pop		si

	ret

; -----------------------------------------------------------------------------

AppendBuffer:
	push		es
	push		di
	mov		es, [DriverSeg]
	mov		di, [es:Header(XFR.Count)]
	mov		[es:Header(XFR.Buffer)+di], al	; character
	inc		di
	test		[es:Header(Status)], byte sfInColor
	jz		.NoColorAttrib
	mov		[es:Header(XFR.Buffer)+di], ah	; attribute
	inc		di
.NoColorAttrib:
	mov		[es:Header(XFR.Count)], di
	cmp		di, MaxXFRSize		; send if TTL buffer is full
	jne		.Done
	call		DriverFlush
.Done:
	pop		di
	pop		es
	ret

; -----------------------------------------------------------------------------

PrepareForXMS:
	; prepare XMS transfer record
	mov		ax, [es:Header(XFR.DstHandle)]
	mov		[XFR.SrcHandle], ax
	mov		[XFR.DstHandle], word 0
	mov		[XFR.DstAddr], word Buffer
	mov		[XFR.DstAddr+2], cs

	; load first item pointer
	mov		ax, [es:Header(XMS.Tail)]
	mov		dx, [es:Header(XMS.Tail)+2]

	mov		si, XFR.Count	; set pointer to XFR record
	; when Log had filled up, skip the first (probably partial) line in Log
	test 		es:Header(Status), byte sfLogFull
	jz		.DontSkipPartial
	call		FetchXMS
.SkippingToCR:
	cmp		bl, 0x0d
	je		.SkippingToLF
	cmp		bl, 0x0a
	je		.SkippingLF
	call		NextXMS
	jc		.SkipFailed
	jmp		.SkippingToCR
.SkippingToLF:
	call		NextXMS
	jc		.SkipFailed
	cmp		bl, 0x0a
	jne		.DontSkipPartial
.SkippingLF:
	call		NextXMS
	jnc		.DontSkipPartial
.SkipFailed:
	; failed somehow, just goto to start of Log
	call		PrepareForXMS
.DontSkipPartial:
	mov		[Viewer.Top], ax
	mov		[Viewer.Top+2], dx
	mov		[Viewer.Start], ax
	mov		[Viewer.Start+2], dx
	ret
; -----------------------------------------------------------------------------

%include "viewer.inc"
%include "out_log.inc"	; simple text and ansi log output

%ifdef HTML_SUPPORT
	%include 'out_html.inc'
%endif

CommonCode

; -----------------------------------------------------------------------------

section .data

%ifdef HOOK_AMIS
	AMIS_Signature
%else
	DriverID:
		DeviceDriverID
%endif

Banner:
	db	'Message Logging Interface Utility, v',VERSION,0x0d,0x0a
	CopyrightText
	db	'$'
NoDriver:
	db	'LOGGER.SYS driver is not loaded.',0x0d,0x0a,'$'

BadDriver:
	db 	'Incompatible LOGGER.SYS driver is loaded.',0x0d,0x0a,'$'

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

SnapshotStart:
	db	'begin snapshot$'
SnapshotEnd:
	db	'end snapshot$'


AnsiPrefix:
	db	27,'[0;$'

HelpText:
	incbin  'help.inc'
	db	'$'

AnsiColors:
	db	0x30,0x34,0x32,0x36,0x31,0x35,0x33,0x37

Flags:
	dw	ofPreTest

LastColor:
	db	0


; -----------------------------------------------------------------------------

section .bss

DriverSeg:		resw 1		; Segment of Logger.sys driver

OrgStat: 		resw 1		; Original Driver Status at Startup

StdOut_Buffer:		resb 1		; used by StdOut for PASSTHRU

XFR:
	.Count:		resd 1		; byte count { must be even }
	.SrcHandle:	resw 1		; XMS Handle
	.SrcAddr:	resd 1		; pointer to source buffer
	.DstHandle:	resw 1		; ; 0 = conventional memory
	.DstAddr:	resd 1		; pointer to destination

Buffer:			resw Buffer_Size; Transfer Buffer

Viewer:
	.Flags:		resw 1		; Viewer navigation control flags
	.Start:		resd 1		; First Whole Log Line
	.Top:		resd 1		; Top line on screen start
	.Bottom:	resd 1		; End of Bottom line.
	.LeftOfs:	resw 1		; Start position on lines
	.WidthMax:	resw 1		; Maximum Line Width

VideoData:
	resb TVideoData_size

VideoRegen:

