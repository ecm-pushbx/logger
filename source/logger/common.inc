; Boot Message Logger Shared Common Macros, Defines and Data

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
%imacro CopyrightText 0
	db	'Copyright (c)',0x20
	db 	%substr(__DATE__,1,4), ',', 0x20
	db 	'Jerome Shidel',0x0d,0x0a
	db	'BSD 3-Clause License',0x0d,0x0a,0x0a
%endmacro

%imacro DeviceDriverID 0
	db 'LOGGERxx'	; 8 Char Device Driver Name
%endmacro

%define DeviceDriverIDLength 8

; -----------------------------------------------------------------------------

%imacro FindDeviceDriver 0
; Not perfect, but good enough for now.
PROC_FindDeviceDriver:
	mov		bx, 0x0100
	mov		dx, ds
	cld
%%Scanning:
	push		bx
	pop		es
	mov		di, 0x09
	mov		si, DriverID
	mov		cx, DeviceDriverIDLength
%%Comparing:
	inc		di
	lodsb
	cmp		al, [es:di]
	jne		%%Next
	loop		%%Comparing
	inc		di
	; matched driver id string
	; check request pointer and interface pointer segments
	cmp		[es:di+2], word -1 ; -1 then driver is not initialized
	je		%%Next		   ; probably left over garbage
	cmp		[es:di+6], bx 	   ; bx <> driver seg then driver is
	jne		%%Next		   ; probably left over garbage
	clc
	jmp		%%Done
%%Next:
	inc		bx
	cmp		bx, dx
	jb		%%CheckFailed
	cmp		bx, 0xa000
	jae		%%CheckFailed
	mov		bx, 0xa000
%%CheckFailed:
	test		bx, bx
	jnz		%%Scanning
	stc
%%Done:
	; ax, bx, cx, dx, si, di = undefined
	; CY Set not found, es = undefined (0xffff)
	; CY clear driver found, es is driver segment
%endmacro

; -----------------------------------------------------------------------------

%imacro ByteAsChar 1-*
	%rep %0
		push	ax
		%ifnidni %1, dl
			push	dx
			mov	dl, %1
		%endif
		mov	ah, 0x02
		int	0x21
		%ifnidni %1, dl
			pop	dx
		%endif
		pop	ax
	%rotate 1
	%endrep
%endmacro

; -----------------------------------------------------------------------------

%macro INTERNAL_NibbleAsHex 0
PROC_NibbleAsHex:
	; low 4 bits of al
	push	ax
	push	dx
	and	al, 0x0f
	mov	dl, 0x30
	cmp	al, 0x09
	jbe	%%NotChar
	add	al, 0x07
%%NotChar:
	add	dl, al
	mov	ah, 0x02
	int	0x21
	pop	dx
	pop	ax
	ret
%endmacro

%imacro NibbleAsHex 1
	%define NEED_NibbleAsHex
	%ifnidni %1, al
		push	ax
		mov	al, %1
	%endif
	call	PROC_NibbleAsHex
	%ifnidni %1, al
		pop	ax
	%endif

%endmacro

; -----------------------------------------------------------------------------

%macro INTERNAL_ByteAsHex 0
PROC_ByteAsHex:
	push		cx
	push		ax
	mov		cl, 0x04
	shr		ax, cl
	NibbleAsHex 	al
	pop		ax
	pop		cx
	NibbleAsHex 	al
	ret
%endmacro

%imacro ByteAsHex 1
	%define NEED_ByteAsHex
	%ifnidni %1, al
		push	ax
		mov	al, %1
	%endif
	call	PROC_ByteAsHex
	%ifnidni %1, al
		pop	ax
	%endif
%endmacro

; -----------------------------------------------------------------------------

%macro INTERNAL_WordAsHex 0
PROC_WordAsHex:
	xchg		al, ah
	ByteAsHex	al
	xchg		al, ah
	ByteAsHex	al
	ret
%endmacro

%imacro WordAsHex 1
	%define NEED_WordAsHex
	%ifnidni %1, al
		push	ax
		mov	ax, %1
	%endif
	call	PROC_WordAsHex
	%ifnidni %1, al
		pop	ax
	%endif
%endmacro

; -----------------------------------------------------------------------------

%imacro ConditionalCode 1
	%ifdef NEED_%1
		%ifndef AVAIL_%1
			INTERNAL_%1
			%define AVAIL_%1
		%endif
	%endif
%endmacro

%imacro CommonCode 0
	ConditionalCode	WordAsHex
	ConditionalCode	ByteAsHex
	ConditionalCode	NibbleAsHex
%endmacro