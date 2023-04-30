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

; %warning Using AMIS multiplex protocol for the device driver.

; -----------------------------------------------------------------------------

struc TAMIS_Signature
	.Vendor: 	resb 8			; 8 bytes
	.Product:	resb 8			; 8 bytes
endstruc

; -----------------------------------------------------------------------------

%imacro AMIS_Signature 0

DATA_AMIS_Signature:

istruc TAMIS_Signature
	at .Vendor,	VendorDriverID
	at .Product,	DeviceDriverID
iend

%ifdef DEVICE_DRIVER
	ProductName	; up to 64 bytes
%endif

%define AMIS(x) DATA_AMIS_Signature + TAMIS_Signature. %+ x
%define AMIS_Multiplex_ID PROC_DriverInterruptHook.Multiplex_ID

%endmacro

; -----------------------------------------------------------------------------

%imacro FindDeviceDriver 0

PROC_FindDeviceDriver:
	xor		bx, bx
%%Scanning:
	mov		ax, bx
	; push		bx		; should not need to preserve BX
	int		0x2d
	; pop		bx
	test		al, al
	%ifdef DEVICE_DRIVER
		jnz		%%InUse
		cmp		[AMIS_FREE], byte 0
		jne		%%Next
		mov		[AMIS_FREE], byte 1
		mov		[AMIS_Multiplex_ID], bh
		jmp		%%Next
	%else
		jz		%%Next
	%endif
%%InUse:
	cmp		al, 0xff
	jne		%%InvalidResponse
	cld
	mov		si, DATA_AMIS_Signature
	mov		es, dx
	mov		cx, 0x0008		; 16 bytes
	repe		cmpsw
	jne		%%Next
%%Found:
	CheckCompatible
	jmp		%%Done
%%Next:
	inc		bh
	test		bh,bh
	jnz		%%Scanning

%%InvalidResponse:
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
	IISP
	cmp		ah, 0
.Multiplex_ID: 		equ $ - 1
	jne		%%Ignore

	test		al, al
	jz		%%InstallCheck
	cmp		al, 0x02
	je		%%Uninstall
	jb		%%PrivateEntry	 ; 0x01
	cmp		al, 0x04
	je		%%InterruptList
	cmp		al, 0x06
	je		%%GetDriverInfo
	jmp		Dispatcher	; 0x05, 0x07+

%%Ignore:
	jmp		far [cs:.NextHandler]


%%InterruptList:
	mov		dx, cs
	mov		bx, %%HookedList
	iret

%%Uninstall:
	mov		al, 0x06 ; cannot remove, loaded in config.sys
	iret

%%PrivateEntry:
	mov		bx, [cs:TDriverHeader.Dispatch]
	jmp		%%Respond

%%GetDriverInfo:
	xor		ah, ah 		; loaded in config sys, cannot remove
	xor		bx, bx		; dx:bx -> first driver header
	jmp		%%Respond	; ah != 0

%%InstallCheck:
	mov		cx, VersionID
	mov		di, DATA_AMIS_Signature

%%Respond:
	mov		dx, cs
	mov		al, 0xff
	iret

%%HookedList:
	db		0x10
	dw		DevInt10
	db		0x2d
	dw		PROC_DriverInterruptHook

%endmacro

; -----------------------------------------------------------------------------

%imacro InstallHook 0

	; Save old int 0x2d
	push		es
	mov		ax, 0x352d
	int		0x21
	mov		[PROC_DriverInterruptHook.NextHandler], bx
	mov		[PROC_DriverInterruptHook.NextHandler+2], es
	pop		es

	; Install Hook
	mov		dx, PROC_DriverInterruptHook
	mov		ax, 0x252d
	int		0x21

%endmacro

