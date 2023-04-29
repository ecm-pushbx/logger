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

PrintLOG:
	; test if log is empty. There are two methods of checking for that.
	; Either by checking the TAIL pointer is not -1, or by checking that
	; the Count is not 0. Checking the Count uses a couple less bytes.
	mov		ax,[es:Header(XMS.Count)]
	or		ax,[es:Header(XMS.Count)+2]
	test		ax, ax
	jz		.Empty

	call		PrepareForXMS
	mov		[LastColor], byte 0x07

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

	call		NextXMSPos
	jnc		.MoreData
	jmp		.Done

.Empty:
	PrintMessage 	LogEmpty

.Done:
	ret
