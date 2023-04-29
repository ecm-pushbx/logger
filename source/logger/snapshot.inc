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

SnapShot:
	call		.SnapStart

	VideoSettings

	; since we know video segment, offset, columns and rows, we most likely
	; could just pull the screen text directly from video memory. But, this
	; doesn't need to be fast and should be more compatible. However, if it
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

	call		.SnapEnd

	call		FlushBuffer

	; restore cursor position for page
	call		CursorRestore
	; restore di, si, bp
	pop		di
	; pop		si	; don't care
	; pop		bp	; don't care

	jmp		Option_Done

.SnapStart:
	push		si
	push		ax
	push		cx
	mov		cx, 32
	mov		si, SnapshotStart
	jmp		.SnapMsg
.SnapEnd:
	push		si
	push		ax
	push		cx
	mov		cx, 33
	mov		si, SnapshotEnd
.SnapMsg:
	push		cx
	mov		ax, COLOR_SNAPSHOT
.First:
	call		AppendBuffer
	loop		.First
	mov		al, 0x20
	call		AppendBuffer
	call		StringToLog
	mov		al, 0x20
	call		AppendBuffer
	pop		cx
	mov		ax, COLOR_SNAPSHOT
.Second:
	call		AppendBuffer
	loop		.Second
	mov		al, 0x0d
	call		AppendBuffer ; add CR to end of line
	mov		al, 0x0a
	call		AppendBuffer ; add LF to end of line
	pop		cx
	pop		ax
	pop		si
	ret

