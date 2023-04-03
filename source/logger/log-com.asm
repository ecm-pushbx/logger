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

%define Buffer_Size 2	; for now, a word at a time is good enough

%define Header(x) TDriverHeader. %+ x

Initialize:
	push		cs
	pop		ds
	push		cs
	pop		es

	ParseOptions	OptionTable, 0x81		; cs:OptionTable
							; es:CommandLine

	and		[Flags], byte 01111111b		; not ofPreTest

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

	; make sure buffer contents have been written.
	call		far [es:Header(Flush)]

	%warning Should add a test for I/O redirection. \
	That way, only redirected text is captured and not keystrokes. \
	Maybe later.

	; send any standard input text to Log
	StdIn
	jc		.NoStdInput
	or		[Flags], byte ofStdIn
	mov		ah, 0x47
.LoopStdIn:
	call		AppendBuffer
	StdIn
	jnc		.LoopStdIn
	call		far [es:Header(Flush)]

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

OptionTable:
	dw		Option_Help
	db 		'HELP', 0
	dw		Option_Help
	db 		'/HELP', 0
	dw		Option_Help
	db 		'/H', 0
	dw		Option_Help
	db 		'/?', 0
	dw		Option_Version
	db 		'VERSION', 0
	dw		Option_Status
	db	 	'STATUS', 0
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
	dw		Option_Msg
	db	 	'MESSAGE', 0
	dw		Option_Msg
	db	 	'MSG', 0
	dw		Option_View
	db	 	'VIEW', 0
	dw 		Option_Snapshot
	db		'SNAPSHOT', 0
;	dw 		Option_HotKey
;	db		'HOTKEY', 0
%ifdef DEBUG
	dw		Option_Debug
	db		'DEBUG',0
%endif
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
	or		[Flags], byte ofShowHelp
	or		[Flags], byte ofKeepStatus
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Version:
	or		[Flags], byte ofShowVersion
	or		[Flags], byte ofKeepStatus
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
	and		[OrgStat], byte 11111110b	; not sfEnabled
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
	; reset logging buffer to clear it
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
	%ifdef	NO_SPLIT_SEND
		mov		[es:Header(XMS.Top)], ax	; 0
		mov		[es:Header(XMS.Top)+2], ax	; 0
	%endif
	%ifdef  DEBUG_DATA_SIZE
		mov		cx, DEBUG_DATA_SIZE
		xor		bx, bx
	.ClearDebug:
		mov		[es:Header(DebugData)+bx], ax
		add		bx, 2
		loop		.ClearDebug
	%endif
	dec		ax
	mov		[es:Header(XMS.Tail)], ax	; -1
	mov		[es:Header(XMS.Tail)+2], ax	; -1
	pop		es
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Print:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	push		es
	mov		es, [DriverSeg]
	and		[Flags], byte 11111011b ; not ofColorPrint
	call		PrintLog
	pop		es
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Ansi:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	push		es
	mov		es, [DriverSeg]
	or		[Flags], byte ofColorPrint
	call		PrintLog
	pop		es
	jmp		Option_Done

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
	call		ScreenSave

	call		LogViewer

	pop		dx			; restore regin size in words
	pop		bx			; restore video segment
	call		ScreenRestore

	pop		di
	pop		es
	jmp		Option_Done

; -----------------------------------------------------------------------------

LogViewer:
	call		ClearScreen
	; move cursor to top of screen
	mov		bh, [VideoData+TVideoData.Page]
	xor		dx, dx
	mov		ah, 0x02
	int		0x10

	mov		ax,[es:Header(XMS.Count)]
	or		ax,[es:Header(XMS.Count)+2]
	test		ax, ax
	jnz		.NotEmpty

	PrintMessage 	LogEmpty
	mov		[Viewer.Flags], byte vfIsEmptyLog + vfAtTop + \
			vfAtBottom + vfAtLeftmost + vfAtRightmost
	jmp		.WaitKeyPress
.NotEmpty:
	mov		[Viewer.Flags], byte vfAtTop + vfAtLeftMost

.WaitKeyPress:
	hlt
	mov		ah, 0x01
	int		0x16
	jz		.WaitKeyPress
	xor		ah, ah
	int		0x16
	cld
	mov		dx, ax
	mov		si, .KeyTable
.LookupKey:
	lodsw
	test		ax, ax
	jz		.WaitKeyPress	; not found
	mov		bx, ax
	lodsw
	cmp		ax, dx
	je		.KeyFound
	test		ah, ah
	jnz		.LookupKey
	cmp		al, dl
	je		.KeyFound
	jmp		.LookupKey
.KeyFound:
	cmp		bx, .Done
	je		.Done
	call		bx
	jmp		.WaitKeyPress

	; Navigation Keys
.AtRightmost:

	; WordAsHex	ax
	; ByteAsChar	0x0d,0x0a
	jmp		.WaitKeyPress
.Done:
	ret

.KeyTable:

	dw		.Done, 0x001b 	; Escape
	dw		.Done, 0x000d 	; Enter/Return
	dw		.Done, 0x000a 	; Enter/Return
	dw		.Done, 0x0003 	; Ctrl+C
	dw		.Up, 0x4800	; Up
	dw		.Up, 0x4900	; PgUp
	dw		.Up, 0x4700	; Home
	dw		.Down, 0x5000	; Down
	dw		.Down, 0x5100	; PgDn
	dw		.Down, 0x4f00	; End
	dw		.Left, 0x4b00	; Left
	dw		.Left, 0x7300	; Ctrl+Left
	dw		.Right, 0x4d00	; Right
	dw		.Right, 0x7400	; Ctrl+Left
	dw 		0			; end of list

.Up:
	test		[Viewer.Flags], byte vfAtTop
	jnz		.UpDone
.UpDone:
	ret
.Down:
	test		[Viewer.Flags], byte vfAtTop
	jnz		.DownDone
.DownDone:
	ret
.Left:
	test		[Viewer.Flags], byte vfAtTop
	jnz		.LeftDone
.LeftDone:
	ret
.Right:
	test		[Viewer.Flags], byte vfAtTop
	jnz		.RightDone
.RightDone:
	ret

; -----------------------------------------------------------------------------

ClearScreen:
	xor		al, al
	jmp		ScrollUp.Clearing

ScrollDown:
	mov		ax, 0x0701
	jmp		ScrollUp.Down

ScrollUp:
	mov		al, 0x01		; count
.Clearing:
	mov		ah, 0x06		; BIOS fn 0x06 Scroll Up
.Down:
	mov		bh, 0x07		; Fill Color Attribute
	xor		cx, cx			; Start X/Y
	mov		dl, [VideoData+TVideoData.Columns]
	mov		dh, [VideoData+TVideoData.Rows]
	push		ds ; BUG - https://fd.lod.bz/rbil/interrup/video/1006.html
	; push		bp ; dont care
	int		0x10
	; pop		bp
	pop		ds
	ret

; -----------------------------------------------------------------------------

ScreenSave:
	cmp		[VideoData+TVideoData.Direct], byte 0
	jnz		.Supported
	ret
.Supported:
	cld
	mov		cx, dx
	push		ds
	push		es
	push		cs
	pop		es
	mov		di, VideoRegen
	mov		si, [VideoData+TVideoData.Offset]
	push		bx
	pop		ds
	rep		movsw
	pop		es
	pop		ds
	ret

; -----------------------------------------------------------------------------

ScreenRestore:
	cmp		[VideoData+TVideoData.Direct], byte 0
	jnz		.Supported
	ret
.Supported:
	cld
	mov		cx, dx
	push		ds
	push		es
	mov		si, VideoRegen
	mov		di, [VideoData+TVideoData.Offset]
	push		cs
	pop		ds
	push		bx
	pop		es
	rep		movsw
	pop		es
	pop		ds
	; restore cursor position
	mov		bh, [VideoData+TVideoData.Page]
	call		CursorRestore
	ret


; -----------------------------------------------------------------------------

Option_Snapshot:
	test		[Flags], byte ofPreTest
	jnz		Option_Done
	or		[Flags], byte ofKeepStatus

	VideoSettings

	; since we know video segment, offset, columns and rows, we most likely
	; could just pull the screen text directly from video memory. But, this
	; doesn't need to be fast and should be more compatible. Although if it
	; were to pull it directly, it would save a couple bytes in code and
	; we would not need to restore the cursor position either.

	; some BIOS destroy di, si, bp, so preserve them
	; https://fd.lod.bz/rbil/interrup/video/1008.html#132
	; push		bp	; don't care
	; push		si	; don't care
	push		di

	mov		bh, [VideoData+TVideoData.Page]
	xor		dh, dh
.Rows:
	push		cx
	push		ax
	xor		dl, dl
	xor		bl, bl
	mov		cx, ax
.Columns:
	push		dx
	mov		ah, 0x02
	int		0x10
	mov		ah, 0x08
	int		0x10
	pop		dx
	cmp		ax, 0x0720	; default color, space
	je		.SkipSpace
	test		bl, bl
	jz		.NoneSkipped
	push		ax
	mov		ax, 0x0720
.FillSkips:
	call		AppendBuffer
	dec		bl
	jnz		.FillSkips
	pop		ax
.NoneSkipped:
	call		AppendBuffer
	jmp		.NextChar
.SkipSpace:
	inc		bl		; count skips
.NextChar:
	inc		dl		; next column
	loop		.Columns
	mov		al, 0x0d
	call		AppendBuffer ; add CR to end of line
	mov		al, 0x0a
	call		AppendBuffer ; add LF to end of line
	pop		ax
	pop		cx
	inc		dh 		; next row
	loop		.Rows
	call		FlushBuffer

	; restore cursor position for page
	call		CursorRestore
	; restore di, si, bp
	pop		di
	; pop		si	; don't care
	; pop		bp	; don't care

	jmp		Option_Done

; -----------------------------------------------------------------------------

CursorRestore:
	; bh=page
	push		bx
	mov		bl, bh
	xor		bh, bh
	add		bx, bx
	mov		dx, [VideoData+TVideoData.Position+bx]
	pop		bx
	mov		ah, 0x02
	int		0x10
	ret

; -----------------------------------------------------------------------------

Option_Msg:
	test		[Flags], byte ofPreTest
	jnz		Option_IgnoreRest
	; pushf		; shouldn't need this
	; cli
	or		[Flags], byte ofKeepStatus
	call		FlushBuffer  ; should already be empty, but won't hurt
	cld
	mov		ah, 0x07
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
	call		FlushBuffer
.Done:
	; popf
	jmp		Option_Done

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
	call		FlushBuffer
.Done:
	pop		di
	pop		es
	ret

; -----------------------------------------------------------------------------

FlushBuffer:
	push		es
	mov		es, [DriverSeg]
	call		far [es:Header(Flush)]
	pop		es
	ret

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

PrintLog:
	; When not sfLogJam mode, probably going to have printing skip forward
	; to first CRLF when the log is full. That would prevent displaying
	; partial lines when buffer has wrapped and is full.

	; test if log is empty. There are two methods of checking for that.
	; Either by checking the TAIL pointer is not -1, or by checking that
	; the Count is not 0. Checking the Count uses a couple less bytes.
	mov		ax,[es:Header(XMS.Count)]
	or		ax,[es:Header(XMS.Count)+2]
	test		ax, ax
	jz		.Empty

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
.PrintLoop:
	; add stuff to buffer transfers of 1k blocks at a times

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
	mov		dx, [Buffer]
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
	mov		dx, [Buffer]
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

%ifdef	NO_SPLIT_SEND
	; test if buffer wrap
	cmp		ax, [es:Header(XMS.Head)]
	jne		.CheckTop
	cmp		dx, [es:Header(XMS.Head)+2]
	je		.Done
.CheckTop:
	cmp		dx, [es:Header(XMS.Top)+2]
	jb		.PrintNext
	cmp		ax, [es:Header(XMS.Top)]
	jb		.PrintNext

	xor		ax, ax
	mov		dx, ax
%else
	; test if buffer wrap
	cmp		dx, [es:Header(XMS.Max)+2]
	jb		.PrintNext
	cmp		ax, [es:Header(XMS.Max)]
	jb		.PrintNext

	xor		ax, ax
	mov		dx, ax
%endif
	; jmp		.PrintNext

.PrintNext:
	; check if finished
	cmp		ax, [es:Header(XMS.Head)]
	jne		.MoreData
	cmp		dx, [es:Header(XMS.Head)+2]
	jne		.MoreData
	; finished printing
	jmp		.Done

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

AnsiPrefix:
	db	27,'[0;$'

HelpText:
	incbin  'help.inc'
	db	'$'

AnsiColors:
	db	0x30,0x34,0x32,0x36,0x31,0x35,0x33,0x37

Flags:
	db	ofPreTest

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

Buffer:			resw Buffer_Size; Transfer Buffer

Viewer:
	.Flags:		resw 1		; Viewer navigation control flags

VideoData:
	resb TVideoData_size

VideoRegen:

