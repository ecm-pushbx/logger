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

; %warning Using simple interrupt 0x2B hook for the device driver.

; -----------------------------------------------------------------------------

%imacro FindDeviceDriver 0

PROC_FindDeviceDriver:

	mov	ax, 0xfd00
	%ifdef		DEVICE_DRIVER
		mov	bx, TDriverHeader.Name
	%else
		mov	bx, DriverID
	%endif
	int	0x2b
	cmp	al, 0xfd
	jne	%%NotFound
	mov	es, bx		; driver segment
	mov	di, TDriverHeader.Name
	%ifdef		DEVICE_DRIVER
		mov	si, TDriverHeader.Name
	%else
		mov	si, DriverID
	%endif
	mov	cx, DriverIDLength / 2
	cld
	repe    cmpsw
	jne	%%NotFound
	clc
	jmp	%%Done
%%NotFound:
	stc
%%Done:
	; ax, bx, cx, dx, si, di = undefined
	; CY set if driver was not found, es = undefined
	; CY clear if driver was found, es is segment of driver
%endmacro

; -----------------------------------------------------------------------------

%imacro DriverInterruptHook 0

PROC_DriverInterruptHook:
	pushf
	cmp		ax, 0xfd00
	jne		%%Ignored


	push		ds
	push		es
	push		si
	push		di
	push		cx
	mov		si, bx		; ds:si -> caller's requested id
	push		cs
	pop		es
	mov		di, TDriverHeader.Name ; es:di -> our ID
	mov		cx, 0x0004
	cld
	repe		cmpsw		; compare the strings

	pop		cx
	pop		di
	pop		si
	pop		es
	pop		ds
	jne		%%Ignored

	popf

%%Matched:
	mov		bx, cs
	mov		al, 0xfd
	iret

%%Ignored:
	popf
	jmp		far [cs:OldIntHook]

%endmacro

; -----------------------------------------------------------------------------

%imacro InstallHook 0

	; Save old int 0x2b
	push		es
	mov		ax, 0x352b
	int		0x21
	mov		[OldIntHook], bx
	mov		[OldIntHook+2], es
	pop		es

	; Install Hook
	mov		dx, PROC_DriverInterruptHook
	mov		ax, 0x252b
	int		0x21

%endmacro
