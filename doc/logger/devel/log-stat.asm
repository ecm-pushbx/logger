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

use16

cpu 8086

org 0x0100

section .text

; At start up, these are not required under DOS and can be assumed.
	; push 	cs
	; pop	ds	; DS = CS
	; push	cs
	; pop   es	; ES = CS

%include "macros.inc"			; include some general purpose macros

; -----------------------------------------------------------------------------

; Check if Logger device driver is loaded and set it Multiplex ID number
Driver_Locate:
	xor		bx, bx		; initialize BH/BL to zero
	cld
.Scanning:
	mov		ax, bx		; set AH to next multiplex number, and
					; AL=0 for install function check

	int		0x2d		; Multiplexer

	; AMIS (Interrupt 0x2d) install check function 0, will return either
	; AL=0x00 for not in use or AL=0xFF when in use. When in use, it
	; will also return, CX=Version Number and DX:DI->Signature

	test		al, al		; if AL=0x00, multiplex is not in use
	jz		.Next
	cmp		al, 0xff	; if AL=0xff, multiplex is in use
	jne		.Next		; if not 0xff, it is an invalid response

	mov		si, LOGGER_SIG	; DS:SI->Logger signature
	mov		es, dx		; ES:DI->Returned multiplex signature
	; mov		dx, cx		; Could save version in DX for later
	mov		cx, 0x0008	; 16 bytes for comparing the signatures
	repe		cmpsw		; Compare the signatures
	je		.Found		; If matched, we found the driver
.Next:
	inc		bh		; inc BH for next multiplex to check
	test		bh,bh		; if BH=0, we tested all 256 numbers
	jnz		.Scanning	; if BH != 0, check the new multiplex

	mov		dx, NOT_FOUND	; DS:DX->Not found string
	mov		ah, 0x09	; Write string to StdOut
	int		0x21
	mov		ax, 0x4c01	; Terminate with exit code 1
	int		0x21

.Found:
	mov		[LOGGER_ID], bh	; Save Multiplex ID number for driver
	; mov		[LOGGER_VN], dx ; Save driver version number, don't care

; -----------------------------------------------------------------------------

; Get far call pointer to Logger function dispatch
	; mov		ah, [LOGGER_ID]	; Not needed, AH is still the Multiplex
	mov		al, 0x01	; Get Private Entry Point
	int		0x2d
	; if AL=0 it is not supported, AL=0xff it is supported. The current
	; Logger version and all future versions will support this function.
	; So, there is no need to check AL for support.
	; DX:BX->Point to driver's far call function dispatcher

	; The dispatcher supports all functions that are not specific to AMIS.
	; That would be all functions starting at AL=0x10 or higher.
	; They can be called through AMIS or directly through the far call to
	; the dispatcher. They provide the same return values in the same
	; registers. There could be many other programs that are hooked into
	; INT 0x2d and performance could be impacted.
	mov		[LOGGER_FC], bx
	mov		[LOGGER_FC+2], dx

; -----------------------------------------------------------------------------

; We will flush the log write buffer for good measure. You should really do
; this before interacting with the log.

	mov		al, 0x12	; Logger - Flush buffer
	int		0x2d

; -----------------------------------------------------------------------------

; Fetch the Log status
	mov		al, 0x10	; Logger - Get Status
	int		0x2d
	; returns CX=Status word; DX:BX->Size info record
	mov		[LOGGER_SF], cx ; Save Original Logger status flags
	mov		es, dx  	; ES:DI->Size info record

; -----------------------------------------------------------------------------
; Turn off logging so our status message is not recorded in the log.

	mov		al, 0x11	; Set Enabled
	xor		bl, bl		; Logging off
	int		0x2d

; -----------------------------------------------------------------------------

ShowStatus:

; Fetch the Log status
	;mov		al, 0x10	; Logger - Get Status
	;int		0x2d
	; returns CX=Status word; DX:BX->Size info record
	; Size Info Record:
	; resw 1	; +0, 16-bit Size of log buffer in Kilobytes
	; resd 1	; +2, 32-bit Size of log buffer in bytes
	; resd 1	; +6, 32-bit Number of bytes written to log. Primarily
	;		; to test if log is empty. If value overflows, it is
	;		; set to 1. So it is only 0 when the log is empty.
	;		; this value could exceed the log size. Use the status
	;		; word to check if the log at max capacity.


; driver status flags (low byte)
%idefine sfEnabled 	00000001b	; bit 0 = logging enabled
%idefine sfModeChange	00000010b	; bit 1 = INTERNAL USE
%idefine sfVESAChange	00000100b	; bit 2 = INTERNAL USE
%idefine sfDirectMode	00001000b	; bit 3 = advanced capture mode active
%idefine sfSupport	00010000b	; bit 4 = card supports advanced mode
%idefine sfInColor	00100000b	; bit 5 = include color attribs in log
%idefine sfLogJam	01000000b	; bit 6 = stop when full
%idefine sfLogFull	10000000b	; bit 7 = log at max capacity
; driver status flags (high byte) 	  bits 8-15 reserved

	; Display the log status

	mov		ax, [es:di]
	WordAsUInt	ax		; Print KB Size of Log
	mov		dx, MSG_ALLOC	; DS:DX->'Kb of memory allocated for'
	mov		ah, 0x09	; DOS Print String
	int		0x21
	mov		dx, MSG_COLOR	; DS:DX->'color'
	test		cx, sfInColor	; Is it Logging in color?
	jnz		.ShowColor
	mov		dx, MSG_MONO	; DS:DX->'monochrome'
.ShowColor:
	int		0x21
	mov		dx, MSG_LOGGING ; DS:DX->'logging and is'
	int		0x21
	mov		dx, MSG_ACTIVE	; DS:DX->'active'
	test		cx, sfEnabled	; Was logging active?
	jnz		.ShowActive
	mov		dx, MSG_DISABLED; DS:DX->'disabled'
.ShowActive:
	int		0x21
	mov		dx, MSG_END 	; DS:DX->'.'
	int		0x21

; -----------------------------------------------------------------------------
; If logging was enabled, turn it back on.

RestoreState:

	test		[LOGGER_SF], byte sfEnabled ; Was logging active?
	jz		.LeaveOff
	mov		ah, [LOGGER_ID]	; We need to get multiplex id
	mov		al, 0x11	; Set Enabled
	mov		bl, 0x01	; Turn Logging On
	int		0x2d

.LeaveOff:

; -----------------------------------------------------------------------------

; End program

	mov		ax, 0x4c01	; Terminate with exit code 0
	int		0x21

; -----------------------------------------------------------------------------

IncludeMacroCode			; Include the common macro code

; -----------------------------------------------------------------------------

section .data

LOGGER_SIG:	db 'J.SHIDEL'	; 8 character manufacturer ID
		db 'LOGGERxx'	; 8 character product ID

NOT_FOUND:	db 'Logging driver not found.$'		; $ terminated string

MSG_ALLOC:	db 'Kb of memory allocated for $'
MSG_COLOR:	db 'color$'
MSG_MONO:	db 'monochrome$'
MSG_LOGGING:	db ' logging and is $'
MSG_ACTIVE:	db 'active$'
MSG_DISABLED:	db 'disabled$'
MSG_END:	db '.',0x0d,0x0a,'$'

; -----------------------------------------------------------------------------

section .bss

LOGGER_ID:	resb 1		; AMIS multiplex number
; LOGGER_VN: 	resw 1		; Driver version number. Not Used.
LOGGER_FC:	resd 1		; Far call to driver dispatch function
LOGGER_SF:	resw 1		; Original Logger status flags
