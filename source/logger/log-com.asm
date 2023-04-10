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

%define COLOR_MESSAGE	0x0e
%define COLOR_STDIN	0x0f
%define COLOR_MONO	0x07

%define Header(x) TDriverHeader. %+ x

Initialize:
	push		cs
	pop		ds
	push		cs
	pop		es

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
	PrintMessage	dx
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
	call		far [es:Header(Flush)]

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
	or		[Flags], byte ofStdIn


	mov		ah, COLOR_STDIN
.LoopStdIn:
	test		[Flags+1], byte ofPassThru
	jz		.NoPassThru
	StdOut
.NoPassThru:
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
	dw		Option_Msg
	db	 	'MESSAGE', 0
	dw		Option_View
	db	 	'VIEW', 0
	dw 		Option_Snapshot
	db		'SNAPSHOT', 0
	dw 		Option_PassThru
	db		'THRU', 0
	; abbreviated options
	dw		Option_Help
	db 		'H', 0
	dw		Option_Version
	db 		'V', 0
	dw		Option_Status
	db	 	'I', 0
	dw		Option_Clear
	db		'C', 0
	dw		Option_Print
	db		'P', 0
	dw		Option_Ansi
	db	 	'A', 0
	dw		Option_Msg
	db	 	'M', 0
	dw		Option_View
	db	 	'V', 0
	dw 		Option_Snapshot
	db		'S', 0
	dw 		Option_PassThru
	db		'T', 0
%ifdef DEBUG
	dw		Option_Debug
	db		'DEBUG',0
	dw		Option_Debug
	db		'D',0
%endif
	; help alternative names
	dw		Option_Help
	db 		'/HELP', 0
	dw		Option_Help
	db 		'/H', 0
	dw		Option_Help
	db 		'/?', 0
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
	or		[Flags+1], byte ofPassThru
	or		[Flags], byte ofKeepStatus
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
	; reset logging buffer to clear it
	push		es
	mov		es, [DriverSeg]
	; reset log buffer control data
	mov		al, [OrgStat]
	and		al, ~sfLogFull
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
	and		[Flags], byte ~ofColorPrint
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
	call		ScreenSave		; only happens in supported modes

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

	mov		ax,[es:Header(XMS.Count)]
	or		ax,[es:Header(XMS.Count)+2]
	test		ax, ax
	jnz		.NotEmpty

	; move cursor to top/left
	xor		dx, dx
	mov		ah, 0x02
	int		0x10
	PrintMessage 	LogEmpty
	xor		dx, dx
	mov		ah, 0x02
	int		0x10

	mov		[Viewer.Flags], byte vfIsEmptyLog + vfAtTop + \
			vfAtBottom + vfAtLeftmost + vfAtRightmost
	jmp		.WaitKeyPress
.NotEmpty:
	mov		[Viewer.WidthMax], word 0

	call		.GoHome

.WaitKeyPress:
	IdleCPU
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

.Done:
	ret

.KeyTable:

	dw		.Done, 		0x001b 		; Escape
	dw		.Done, 		0x000d 		; Enter/Return
	dw		.Done, 		0x000a 		; Enter/Return
	dw		.Done, 		0x0003 		; Ctrl+C
	dw		.Up, 		0x4800		; Up
	dw		.PgUp,		0x4900		; PgUp
	dw		.HomeKey, 	0x4700		; Home
	dw		.Down, 		0x5000		; Down
	dw		.PgDn, 		0x5100		; PgDn
	dw		.EndKey, 	0x4f00		; End
	dw		.Left, 		0x4b00		; Left
	dw		.CtrlLeft, 	0x7300		; Ctrl+Left
	dw		.Right, 	0x4d00		; Right
	dw		.CtrlRight, 	0x7400		; Ctrl+Left
	dw 		0				; end of list

.HomeKey:
	test		[Viewer.Flags], byte vfAtTop
	jz		.GoHome
	ret
.GoHome:
	call		PrepareForXMS
	mov		[Viewer.Flags], byte vfAtTop + vfAtLeftMost + vfAtRightMost
	mov		[Viewer.LeftOfs], word 0
	mov		[Viewer.Top], ax
	mov		[Viewer.Top+2], dx
	jmp		DrawPage

.PgUp:
	test		[Viewer.Flags], byte vfAtTop
	jnz		.Done
	mov		cx, [VideoData+TVideoData.Rows]
	dec		cx
	or		[Viewer.Flags], byte vfNoDraw
.PgUpLoop:
	push		cx
	call		.Up
	pop		cx
	loop		.PgUpLoop
	and		[Viewer.Flags], byte ~vfNoDraw
	jmp		DrawPage

.Up:
	test		[Viewer.Flags], byte vfAtTop
	jnz		.Done
	call		PrevLine
	jmp		DrawPage

.EndKey:
	; or		[Viewer.Flags], byte vfNoDraw
	; call		.Down
	; and		[Viewer.Flags], byte ~vfNoDraw
	; test		[Viewer.Flags], byte vfAtBottom
	; jz		.EndKey
	; jmp		DrawPage
	test		[Viewer.Flags], byte vfAtBottom
	jnz		.Done
	call		.PgDn
	jmp		.EndKey

.PgDn:
	test		[Viewer.Flags], byte vfAtBottom
	jnz		.Done
	mov		cx, [VideoData+TVideoData.Rows]
	dec		cx
	or		[Viewer.Flags], byte vfNoDraw
.PgDnLoop:
	push		cx
	call		.Down
	pop		cx
	loop		.PgDnLoop
	and		[Viewer.Flags], byte ~vfNoDraw
	jmp		DrawPage
.Down:
	test		[Viewer.Flags], byte vfAtBottom
	jnz		.Done
	call		NextLine
	jmp		DrawPage

.CtrlLeft:
	test		[Viewer.Flags], byte vfAtLeftmost
	mov		[Viewer.LeftOfs], word 0
	jmp		DrawPage
.Left:
	test		[Viewer.Flags], byte vfAtLeftmost
	jnz		.Done
	dec		word [Viewer.LeftOfs]
	jmp		DrawPage

.CtrlRight:
	test		[Viewer.Flags], byte vfAtRightmost
	jnz		.Done
	or		[Viewer.Flags], byte vfNoDraw
.CtrlRightLoop:
	call		.Right
	test		[Viewer.Flags], byte vfAtRightmost
	jz		.CtrlRightLoop
	and		[Viewer.Flags], byte ~vfNoDraw
	jmp		DrawPage

.Right:
	test		[Viewer.Flags], byte vfAtRightmost
	jnz		.Done
	inc		word [Viewer.LeftOfs]
	jmp		DrawPage

; -----------------------------------------------------------------------------

PrevLine:
	and		[Viewer.Flags], byte ~vfAtBottom
	mov		ax, [Viewer.Top]
	mov		dx, [Viewer.Top+2]
	; previous line ended in either CR, LF or CRLF
	call		PrevXMS
	jc		.AtFirst
	cmp		bl, 0x0d	; if CR, skim to line start
	je		.Skimmer
	call		PrevXMS		; not CR, had to be LF, so fetch another
	jc		.AtFirst
	cmp		bl, 0x0d	; if previous not CR, then just LF ending
	jne		.Skimming
.Skimmer:
	call		PrevXMS
	jc		.AtFirst
.Skimming:
	cmp		bl, 0x0a
	je		.ForwardOne
	cmp		bl, 0x0d
	jne		.Skimmer
.ForwardOne:
	call		NextXMS
.AtFirst:
	mov		[Viewer.Top], ax
	mov		[Viewer.Top+2], dx
	ret

NextLine:
	and		[Viewer.Flags], byte ~vfAtTop
	mov		ax, [Viewer.Top]
	mov		dx, [Viewer.Top+2]
.NotEOL:
	call		NextXMS
	jc		.Failed ; should never happen
	cmp		bl, 0x0a
	jne		.CheckForCR
	call		NextXMS
	jmp		.Done
.CheckForCR:
	cmp		bl, 0x0d
	jne		.NotEOL
	call		NextXMS
	cmp		bl, 0x0a
	jne		.Done
	call		NextXMS
.Done:
	mov		[Viewer.Top], ax
	mov		[Viewer.Top+2], dx
.Failed:
	ret

; -----------------------------------------------------------------------------

;DrawTopRow:
;	mov		cx, 0x0001
;	jmp		DrawPage.StartAtTop

;DrawBottomRow:
;	mov		dx, [VideoData+TVideoData.Rows]
;	dec		dx
;	xchg		dl, dh
;	mov		ah, 0x02
;	int		0x10
;	mov		cx, 0x0001
;	mov		ax, [Viewer.Bottom]
;	mov		dx, [Viewer.Bottom+2]
;	jmp		DrawPage.StartElsewhere

; -----------------------------------------------------------------------------

DrawPage:
	mov		cx, [VideoData+TVideoData.Rows]

.StartAtTop:
	; move cursor to top/left
	xor		dx, dx
	mov		ah, 0x02
	int		0x10

	mov		ax, [Viewer.Top]
	mov		dx, [Viewer.Top+2]
	xor		di, di			; for direct video
; .StartElsewhere:
	call		FetchXMS
	or		[Viewer.Flags], byte vfAtRightmost
	and		[Viewer.Flags], byte ~vfAtLeftmost
	cmp		[Viewer.LeftOfs], word 0
	jne		.Rows
	or		[Viewer.Flags], byte vfAtLeftmost
.Rows:
	push		cx
	xor		si, si
	call		.SkipLeftOfs
	mov		cx, [VideoData+TVideoData.Columns]
.Columns:
	push		cx
	test		si, si
	jnz		.LineEnded
	cmp		bl, 0x0d
	je		.HitEOL
	cmp		bl, 0x0a
	jne		.Display
.HitEOL:
	inc		si
.LineEnded:
	mov		bx, 0x0720
.Display:
	test		[Viewer.Flags], byte vfNoDraw
	jnz		.NoDraw
	call		.DisplayChar
.NoDraw:
	test		si, si
	jnz		.NoFetch
	call		NextXMS
	jnc		.NoFetch
	inc		si
.NoFetch:
	pop		cx
	loop		.Columns

	; find start of next line
	call 		FetchXMS
	jc		.AtNextLine
	call		.FindEOL
.AtNextLine:

	test		si, si
	jnz		.AtRightmost
	and		[Viewer.Flags], byte ~vfAtRightmost
.AtRightmost:

	mov		[Viewer.Bottom], ax
	mov		[Viewer.Bottom+2], dx
	pop		cx
	loop		.Rows
 	ret

.SkipLeftOfs:
 	; skip left offset columns
	mov		cx, [Viewer.LeftOfs]
	test		cx, cx
	jz		.SkipDone
.Skip:
	test		si, si
	jnz		.SkipDone
	cmp		bl, 0x0d
	je		.SkipAll
	cmp		bl, 0x0a
	jne		.SkipChar
.SkipAll:
	inc		si
	jmp		.SkipDone
.SkipChar:
	call		NextXMS
	jc		.SkipDone
	loop		.Skip
.SkipDone:
	ret


.FindEOL:
	cmp		bl, 0x0a
	je		.AtEOL
	cmp		bl, 0x0d
	je		.MaybeEOL
	call 		NextXMS
	jc		.FoundEOL
	jmp		.FindEOL
.MaybeEOL:
	call 		NextXMS
	cmp		bl, 0x0a
	jne		.FoundEOL
.AtEOL:
	call 		NextXMS
.FoundEOL:
	ret

.DisplayChar:
	cmp		[VideoData+TVideoData.Direct], byte 1
	jne		.DisplayWithBIOS
	push		es
	mov		es, [VideoData+TVideoData.VSeg]
	mov		[es:di], bx
	inc		di
	inc		di
	pop		es
	ret

.DisplayWithBIOS:
 	push		ax
	push		dx
	push		bx ; char/attr
	; get x/y
	mov		ah, 0x03
	mov		bh, [VideoData+TVideoData.Page]
	int		0x10
	pop		ax ; was bx
	push		dx
	; print char at x/y
	mov		bl, ah
	mov		ah, 0x09
	mov		cx, 0x001
	int		0x10
	; move cursor to next position
	pop		dx
	inc		dl
	cmp		dl, [VideoData+TVideoData.Columns]
	jne		.SameRow
	xor		dl, dl
	inc		dh
	cmp		dh, [VideoData+TVideoData.Rows]
	jne		.SameRow
	xor		dh, dh
.SameRow:
	mov		ah, 0x02
	int		0x10
	pop		dx
	pop		ax
	ret

; -----------------------------------------------------------------------------

NextXMS:
	push		dx
	push		ax
	and		[Viewer.Flags], byte ~vfAtBottom

	%ifdef NO_SPLIT_BUFFER
		cmpdd		dx, ax, es:Header(XMS.Top)
	%else
		cmpdd		dx, ax, es:Header(XMS.Max)
	%endif
	jne		.NoWrap
	xor		ax, ax
	xor		dx, dx
	jmp		FetchXMS
.NoWrap:
	test		[es:Header(Status)], byte sfInColor
	jz		.OneByte
	inc		ax
	jnz		.OneByte
	inc		dx
.OneByte:
	inc		ax
	jnz		.Try
	inc		dx
.Try:
	cmpdd		dx, ax, es:Header(XMS.Head)
	jne		.LooksGood
	or		[Viewer.Flags], byte vfAtBottom
	pop		ax
	pop		dx
	stc
	ret
.LooksGood:
	pop 		bx
	pop		bx
	jmp		FetchXMS

; -----------------------------------------------------------------------------

PrevXMS:
	cmpdd		dx, ax, es:Header(XMS.Tail)
	jne		.CanDec
	or		[Viewer.Flags], byte vfAtTop
	stc
	ret
.CanDec:
	and		[Viewer.Flags], byte ~vfAtTop
	mov		bx, dx
	or		bx, ax
	test		bx, bx
	jnz		.NoWrap
	%ifdef NO_SPLIT_BUFFER
		mov	ax, [es:Header(XMS.Top)]
		mov	dx, [es:Header(XMS.Top)+2]
	%else
		mov	ax, [es:Header(XMS.Max)]
		mov	dx, [es:Header(XMS.Max)+2]
	%endif
	jmp		.TwoBytes
.NoWrap:
	test		[es:Header(Status)], byte sfInColor
	jz		.OneByte
.TwoBytes:
	dec		ax
	cmp		ax, 0xffff
	jne		.OneByte
	dec		dx
.OneByte:
	dec		ax
	cmp		ax, 0xffff
	jne		FetchXMS
	dec		dx
	jmp		FetchXMS

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

ClearScreen:
	xor		al, al
;	jmp		ScrollUp.Clearing
;
;ScrollDown:
;	mov		ax, 0x0701
;	jmp		ScrollUp.Down
;
;ScrollUp:
;	mov		al, 0x01		; count
;.Clearing:
	mov		ah, 0x06		; BIOS fn 0x06 Scroll Up
;	jmp		.Scroll
;.Down:
;.Scroll:
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

	call		PrepareForXMS
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
	db	'Message Logging Interface Utility, ',VERSION,0x0d,0x0a
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
	.Top:		resd 1		; Top line on screen start
	.Bottom:	resd 1		; End of Bottom line.
	.LeftOfs:	resw 1		; Start position on lines
	.WidthMax:	resw 1		; Maximum Line Width

VideoData:
	resb TVideoData_size

VideoRegen:

