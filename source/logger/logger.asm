; Boot Message Logger

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

org 0x0000

section .text

%include "common.inc"

struc tREQUEST
	.Length:	resb 1		; requested structure length
	.Unit:		resb 1		; unit number for request
	.Function:	resb 1		; requested function
	.Status:	resw 1		; return status
	.Reserved:	resb 8		; reserved
	.Descriptor:	resb 1		; +13, media descriptor BYTE
	.Address:  	resd 1		; +14, transfer address pointer DWORD
					; also, return address byte above
					; program to release after initialized
	.CommandLine:			; +18, init function DWORD
	.Count:		resw 1		; +18
	.Start:		resw 1		; +20
	.Drive:		resb 1		; +22
endstruc

Header:
	ChainNext: 	dd -1			; pointer to next device driver
	Attributes:	dw 0x8000		; character device
	Strategy:	dw Driver.Strategy	; set request block pointer
	Entry:		dw Driver.Routine	; driver interrupt call
	Name:		DeviceDriverID		; 8 character driver name

Request:		dd -1			; Pointer to tREQUEST block

Driver.Strategy:				; set request block pointer
	mov		[cs:Request], bx
	mov		[cs:Request+2], es
	retf

Driver.Routine:					; driver function dispatcher
	pushf
	push		ax
	push		di
	push		es

	les		di, [cs:Request]
	mov		al, [es:di+tREQUEST.Function]
	test		al, al			; test command 0, initialize
	jz		Initialize
	; cmp		al, 0x15
	; jbe		Driver.Done
	mov		al, 0x03		; error code invalid function
Driver.Error:
	mov		ah, 0x81		; set error & done bits
	jmp		Driver.Return
Driver.Done:
	mov		ax, 0x0100		; done bit, no error, no code

Driver.Return:
	mov		[es:di+tREQUEST.Status], ax
	pop		es
	pop		di
	pop		ax
	popf
	retf

; Released after Initialization

Initialize:
	; set pointer to first byte above program for DOS to free after init
	mov		[es:di+tREQUEST.Address], word Initialize
	mov		[es:di+tREQUEST.Address+2], cs

	; save remaining general registers not preserved in function dispatcher
	push		ds
	push		si
	push		dx
	push		cx

	push		cs
	pop		ds	; mov ds, cs

	; debugging - print driver segment
	; WordAsHex	ds
	; ByteAsChar	0x0d,0x0a

	; print driver banner text
	mov		dx, Message
	mov		ah, 0x09
	int		0x21

	; debugging - print "Command Line" of driver initialize
	; lds		si, [es:di+tREQUEST.CommandLine]
	; mov		ah, 0x02
	; mov		cx, 100
;.PrintLoop:
	; mov		dl, [si]
	; inc		si
	; cmp		dl, 0x0a
	; jbe		.PrintDone
	; int		0x21
	; dec		cx
	; jnz		.PrintLoop
;.PrintDone:
	; mov		dl, 0x0d
	; int		0x21
	; mov		dl, 0x0a
	; int		0x21

	; restore additional general registers used during initialization
	pop		cx
	pop		dx
	pop		si
	pop		ds
	jmp		Driver.Error

CommonCode

Message:
	db	0x0d,0x0a
	db	'System Boot Message Logger, v0.1',0x0d,0x0a
	CopyrightText
	db	'$'
