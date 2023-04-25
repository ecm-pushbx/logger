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

%warning Using a very slow brute force search to locate driver.

; -----------------------------------------------------------------------------

%imacro FindDeviceDriver 0

; Not perfect or fast, But good enough for now. There is most likely a far
; better way to find out if the logging driver is loaded and its location in
; memory. But after trying a couple different things, I just settled on using
; this method. In part, to prevent a potential collision with hooking an
; interrupt or other type of probing.

PROC_FindDeviceDriver:

	mov		dx, ds
	mov		bx, 0xa000
	cld
%%Scanning:
	mov		es, bx
	mov		di, 0x0a ; The Driver ID is at offset 0x0a.
	%ifdef		DEVICE_DRIVER
		mov	si, TDriverHeader.Name
	%else
		mov	si, DriverID
	%endif

	mov		cx, DriverIDLength
	repe		cmpsb
	jne		%%Next
	; matched driver id string

	; check segment of flush pointer is set. If it is not, then it is most
	; likely RAM garbage fro a file copy or something. So, we will need to
	; keep looking for the driver.
	cmp		[es:TDriverHeader.Flush+2], word 0
	je		%%Next
	%ifndef DEVICE_DRIVER
		; verify driver interface/data record format version
		%ifdef NO_SPLIT_SEND
			cmp	[es:TDriverHeader.Format], word 0 ; has XMS.TOP field
		%else
			cmp	[es:TDriverHeader.Format], word 1 ; does not have XMS.TOP field
		%endif
			je	%%Found
			or	[Flags], byte ofBadVersion
			jmp	%%Next
		%%Found:
			and	[Flags], byte ~ofBadVersion
	%endif

	clc
	jmp		%%Done
%%Next:
	inc		bx
	cmp		bx, 0xa000
	je		%%Error
	cmp		bx, dx
	je		%%Next
	jmp		%%Scanning
%%Error:
	stc
%%Done:
	; ax, bx, cx, dx, si, di = undefined
	; CY set if driver was not found, es = undefined
	; CY clear if driver was found, es is segment of driver
%endmacro

