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

PrintHTML:
	mov		[LastColor], byte 0x07
	mov		[BackSpan], byte 0x0
	mov		[ForeSpan], byte 0x0
	PrintMessage	HTML.HEADER

	mov		ax,[es:Header(XMS.Count)]
	or		ax,[es:Header(XMS.Count)+2]
	test		ax, ax
	jz		.Empty

	call		PrepareForXMS

.PrintLoop:
	; maybe add stuff to buffer transfers of entire blocks at one time

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

	call		.PrintChar
	; if second character is not zero, print it
	test		dh, dh
	jz		.PrintedChar
	mov		dl, dh
	call		.PrintChar
	jmp		.PrintedChar

.ColorPrint:
	mov		dx, [Buffer]

	cmp		dl, 0x0d
	jne		.NotCR
	mov		dh, 0x07
.NotCR:
	cmp		dl, 0x0a
	jne		.NotLF
	mov		dh, 0x07
.NotLF:

	cmp		dh, [LastColor]
	je		.ColorIsSet
	mov		[LastColor], dh

	call		.ColorSpanOff

	push		ax
	push		bx
	push		dx
	and		dh, 0xf0
	cmp		dh, 0x00
	je		.BackIsDefault
	mov		al, dh
	push		cx
	mov		cl, 4
	shr		al, cl
	pop		cx
	mov		bl, 'b'
	mov		[BackSpan], byte 1
	call		.ColorSpanOn
.BackIsDefault:
	pop		dx
	push		dx
	and		dh, 0x0f
	cmp		dh, 0x07
	je		.ForeIsDefault
	mov		al, dh
	mov		bl, 'c'
	mov		[ForeSpan], byte 1
	call		.ColorSpanOn
.ForeIsDefault:
	pop		dx
	pop		bx
	pop		ax

.ColorIsSet:
	call		.PrintChar

.PrintedChar:
	; restore XMS pointer and count
	pop		dx
	pop		ax
	pop		cx

	call		NextXMSPos
	jnc		.MoreData
	jmp		.Done

.ColorSpanOff:
	; turn off any prior active color change spans
	push 		ax
	push		dx
	cmp 		[ForeSpan], byte 0
	jz		.ForeWasDefault
	mov		[ForeSpan], byte 0
	PrintMessage	HTML.ENDSPAN
.ForeWasDefault:
	cmp 		[BackSpan], byte 0
	je		.BackWasDefault
	mov 		[BackSpan], byte 0
	PrintMessage	HTML.ENDSPAN
.BackWasDefault:
	pop		dx
	pop		ax
	ret

.ColorSpanOn:
	push		dx
	push		ax
	PrintMessage	HTML.STARTSPAN
	ByteAsChar	bl
	pop		ax
	xor		ah, ah
	WordAsUInt	ax
	ByteAsChar	'"', '>'
	pop		dx
	ret

.PrintChar:
	push		ax
	push		dx
	push		si
	mov		si, HTML.TABLE
.Lookup:
	lodsb
	cmp		al, dl
	je		.Match
	test		al, al
	jz		.NoMatch
.Next:
	lodsb
	test		al, al
	jz		.Lookup
	jmp		.Next

.Match:
	lodsb
	test		al, al
	jz		.DoneReplace
	ByteAsChar	al
	jmp		.Match

.NoMatch:
	int 		0x21
.DoneReplace:
	pop		si
	pop		dx
	pop		ax
	ret

.Empty:
	PrintMessage 	LogEmpty

.Done:
	call		.ColorSpanOff
	PrintMessage	HTML.FOOTER
	ret



BackSpan:	db 0
ForeSpan:	db 0

HTML.HEADER:
	db '<html>'
	db '<style>'
	db 'body{background:#ccc;}'
	db '.logger-outer{display:inline-block;'
	db 'white-space:pre;font-family: monospace;'
	db 'background:#222;padding:0.5pc;border-radius:0.75pc;}'
	db '.logger-inner{background: black;color: gray;width:auto;}'
	db '.c0{color:#000;}'
	db '.c1{color:#00a;}'
	db '.c2{color:#0a0;}'
	db '.c3{color:#0aa;}'
	db '.c4{color:#a00;}'
	db '.c5{color:#a0a;}'
	db '.c6{color:#a50;}'
	db '.c7{color:#aaa;}'
	db '.c8{color:#555;}'
	db '.c9{color:#55f;}'
	db '.c10{color:#5f5;}'
	db '.c11{color:#5ff;}'
	db '.c12{color:#f55;}'
	db '.c13{color:#f5f;}'
	db '.c14{color:#ff5;}'
	db '.c15{color:#fff;}'
	db '.b0{background:#000;}'
	db '.b1{background:#00a;}'
	db '.b2{background:#0a0;}'
	db '.b3{background:#0aa;}'
	db '.b4{background:#a00;}'
	db '.b5{background:#a0a;}'
	db '.b6{background:#a50;}'
	db '.b7{background:#aaa;}'
	db '.b8{background:#000;}'
	db '.b9{background:#00a;}'
	db '.b10{background:#0a0;}'
	db '.b11{background:#0aa;}'
	db '.b12{background:#a00;}'
	db '.b13{background:#a0a;}'
	db '.b14{background:#a50;}'
	db '.b15{background:#aaa;}'
	db '</style>',0x0d,0x0a
	db '<body>'
	db '<div class="logger-outer"><div class="logger-inner">$'
HTML.FOOTER:
	db '</div></div></body></html>$'
HTML.ENDSPAN:
	db '</span>$'
HTML.STARTSPAN:
	db '<span class="$'
HTML.TABLE:
	db '>&gt;',0
	db '<&lt;',0
	db '&&amp;',0
	db 0x08,'&#x2408;',0
	dw 0
