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

org 0x0100

section .text

%include 'common.inc'

%define XMS_Buffer_Size 1024
%define Header(x) TDriverHeader. %+ x

Initialize:
	FindDeviceDriver
	jnc		DriverFound
	call		PrintVersion
	mov		dx, NoDriver
	mov		ah, 0x09
	int		0x21
	mov		ax,0x4c01
	int		0x21

; -----------------------------------------------------------------------------

DriverFound:
	call		PrintLog
	mov		ax, 0x4c00
	int 		0x21

; -----------------------------------------------------------------------------

PrintVersion:
	mov		dx, Message
	mov		ah, 0x09
	int		0x21
	ret

; -----------------------------------------------------------------------------

PrintLog:
	; save driver status
	mov		ax, [es:Header(Status)]
	push		ax

	; if need be, disable driver
	and 		al, 0xfe		; not sfEnabled
	mov		[es:Header(Status)], ax

	mov		ax,[es:Header(XMS.Count)]
	or		ax,[es:Header(XMS.Count)+2]
	jz		.Empty

.PrintLoop:



.Empty:
	mov		dx, LogEmpty
	mov		ah, 0x09
	int		0x21

.Done:

	; restore driver status
	pop		ax
	mov		[es:Header(Status)], ax
	ret

; -----------------------------------------------------------------------------

CommonCode

; -----------------------------------------------------------------------------

section .data

DriverID:
	DeviceDriverID

Message:
	db	'System Boot Message Log Utility, v0.1',0x0d,0x0a
	CopyrightText
	db	'$'
NoDriver:
	db	'LOGGER.SYS driver is not loaded.',0x0d,0x0a,'$'

LogEmpty:
	db	'Log is empty.',0x0d,0x0a,'$'

; -----------------------------------------------------------------------------

section .bss

XMS_Ptr:	resd 	1
XMS_Buffer:	resb 	XMS_Buffer_Size