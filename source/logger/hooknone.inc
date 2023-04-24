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

; %warning No interrupt hook, search device driver chain to locate driver.

; -----------------------------------------------------------------------------

%imacro FindDeviceDriver 0

; Not perfect or fast, But good enough for now. There is most likely a far
; better way to find out if the logging driver is loaded and its location in
; memory. But after trying a couple different things, I just settled on using
; this method. In part, to prevent a potential collision with hooking an
; interrupt or other type of probing.

PROC_FindDeviceDriver:

	; The DOS 5.x check needs moved to the COM/SYS initialization code

	mov		ax, 0x3001	; Get DOS Version
	int		0x21
	cmp		al, 0x05	; Major version

	; We need at least DOS 2.0. But since some other code needs DOS 5.0, we
	; can just fail without DOS 5.0+
	jb		%%Failed

%%WalkChain:
	mov		ah, 0x52	; Get SYSVARS List
	int		0x21		; es:bx->List

	; NUL is the first driver in the device driver chain
	; add		bx, 0x17	; offset to NUL device header, DOS 2.0
	; add		bx, 0x28	; offset to NUL device header, DOS 3.0
	add		bx, 0x22	; offset to NUL device header, DOS 3.1+

	mov		cx, 1000	; maximum number of chain links to walk
					; just so we don't somehow get stuck.
	%ifdef DEVICE_DRIVER
		mov		dx, cs
	%endif
	cld
%%NextDriver:
	%ifdef		DEVICE_DRIVER
		mov	ax, es		; check if this is our segment
		cmp	dx, ax
		je	%%Next

		mov	si, TDriverHeader.Name
	%else
		mov	si, DriverID
	%endif
	mov		di, bx
	add		di, TDriverHeader.Name

	push		cx
	mov		cx, 4
	repe		cmpsw
	pop		cx
	jne		%%Next

	CheckCompatible
	jmp		%%Done

%%Next:
	les		bx, [es:bx]	; get next driver in chain
	cmp		bx, -1
	je		%%NotFound
	loop		%%NextDriver

%%Failed:
	%ifdef		DEVICE_DRIVER
		; if we are a device driver and search fails, pretend we
		; found a version running to prevent loading.
		clc
		jmp		%%Done
	%endif

%%NotFound:
	stc
%%Done:
	; ax, bx, cx, dx, si, di = undefined
	; CY set if driver was not found, es = undefined
	; CY clear if driver was found, es is segment of driver
%endmacro

