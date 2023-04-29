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
	%ifdef DEBUG
	dw		ViewDebug, 	0x0004		; CTRL+D
	%endif
	dw 		0				; end of list

.HomeKey:
	test		[Viewer.Flags], byte vfAtTop
	jz		.GoHome
	ret
.GoHome:
	call		PrepareForXMS
	mov		[Viewer.Flags], byte vfAtTop + vfAtLeftMost + vfAtRightMost
	mov		[Viewer.LeftOfs], word 0

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
	test		[Viewer.Flags], byte vfAtBottom
	jnz		.Done
	call		LastPage
	jmp		DrawPage

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

; -----------------------------------------------------------------------------

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

LastPage:
	mov		ax, [es:Header(XMS.Head)]
	mov		dx, [es:Header(XMS.Head)+2]
	mov		[Viewer.Top], ax
	mov		[Viewer.Top+2], dx
	mov		cx, [VideoData+TVideoData.Rows]
.Repeat:
	call		PrevLine
	loop		.Repeat
	ret

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

NextXMSPos:
	; Next read position
	add		ax, 2
	adc		dx, 0

	; test if buffer wrap
	cmp		dx, [es:Header(XMS.Max)+2]
	jb		.Probably
	cmp		ax, [es:Header(XMS.Max)]
	jb		.Probably

	xor		ax, ax
	mov		dx, ax
	; jmp		.PrintNext

.Probably:
	; check if finished
	cmp		ax, [es:Header(XMS.Head)]
	jne		.HasNext
	cmp		dx, [es:Header(XMS.Head)+2]
	jne		.HasNext
.NoNext:
	stc
	ret
.HasNext:
	clc
	ret

; -----------------------------------------------------------------------------

NextXMS:

	push		dx
	push		ax
	and		[Viewer.Flags], byte ~vfAtBottom

	test		[es:Header(Status)], byte sfInColor
	jnz		.ColorMode

	test		ax, 1
	jz		.NextIsOdd
	and		al, 0xfe
	jmp		.ColorMode
.NextIsOdd:
	inc		ax
	jmp		.FetchOdd
.ColorMode:
	call		NextXMSPos
	jc		.Failed
.FetchOdd:
	pop 		bx
	pop		bx
	jmp		FetchXMS

.Failed:
	pop		ax
	pop		dx
	or		[Viewer.Flags], byte vfAtBottom
	stc
	ret

; -----------------------------------------------------------------------------

PrevXMS:
	cmpdd		dx, ax, Viewer.Start
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
	mov		ax, [es:Header(XMS.Max)]
	mov		dx, [es:Header(XMS.Max)+2]
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
	; push		bp ; BP bug, don't care
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

	; fall through to CursorRestore
	; call		CursorRestore
	; ret

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

%ifdef DEBUG
ViewDebug:
	call		ClearScreen
	call		PrepareForXMS
	call		FetchXMS
.Repeating:
	jc		.Done
	push		ax
	push		dx
	push		bx

	mov		al, bl
	mov		ah, 0x0e
	mov		bl, bh
	xor		bh, bh
	int		0x10
	pop		bx
	pop		dx
	pop		ax
	call		NextXMS
	jmp		.Repeating
.DoneMsg:
	db		'end of log$'
.Done:
	pop		cx
	PrintMessage	.DoneMsg
.WaitKeyPress:
	IdleCPU
	mov		ah, 0x01
	int		0x16
	jz		.WaitKeyPress
	xor		ah, ah
	int		0x16

	jmp		LogViewer

%endif
