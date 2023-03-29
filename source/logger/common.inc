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
	db 'LOGGERxx'	; 8 character driver name, space padded if needed
%endmacro

%define DriverIDLength 8

; driver status flags
%idefine sfEnabled 	00000001b
%idefine sfModeChange	00000010b
%idefine sfDirectMode	00000100b
%idefine sfEGAorBetter	00001000b
%idefine sfInColor	00010000b
%idefine sfLogJam	00100000b
%idefine sfLogFull	01000000b

; interface status and option flags
%idefine ofKeepStatus	00000001b	; bit 0 = restore driver status on exit
%idefine ofHadOptions	00000010b	; bit 1 = command line options provided
%idefine ofColorPrint	00000100b	; bit 2 = MONO/ANSI printing selected
%idefine ofShowVersion  00001000b	; bit 3 = print version information
%idefine ofShowHelp	00010000b	; bit 4 = print help and exit
%idefine ofPreTest	10000000b	; bit 7 = CmdLn Option Pre-Check

; -----------------------------------------------------------------------------

struc TDriverHeader
	.ChainNext: 	resd 1			; pointer to next device driver
	.Attributes:	resw 1			; character device
	.Strategy:	resw 1			; set request block pointer
	.Entry:		resw 1			; driver interrupt call
	.Name:		resb DriverIDLength	; 8 character driver name

	.Request:	resd 1			; Pointer to tREQUEST block

	.Status:	resw 1			; Device driver Status

	.XMS.Driver:	resd 1			; Pointer to XMS driver
	.XMS.Size:	resw 1			; Size in KB to allocate
	.XMS.Head:	resd 1			; next buffer write position
	.XMS.Tail:	resd 1			; first buffer read position
	.XMS.Count:	resd 1			; bytes in buffer
	.XMS.Max:	resd 1			; size of buffer in bytes

	.XFR.Count:	resd 1			; byte count { must be even }
	.XFR.SrcHandle:	resw 1			; 0 = conventional memory
	.XFR.SrcAddr:	resd 1			; pointer to source buffer
	.XFR.DstHandle:	resw 1			; XMS handle
	.XFR.DstAddr:	resd 1			; pointer to destination

endstruc

; -----------------------------------------------------------------------------

%imacro PUSHAG 0
	; 8086 is similar to, but not equivalent to PUSHA!
	; it excludes SP and uses a different order.
	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
%endmacro

%imacro POPAG 0
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
%endmacro

%imacro PUSHALL 0
	pushag
	push	bp
	push	es
	push	ds
%endmacro

%imacro POPALL 0
	push	ds
	push	es
	push	bp
	pushag
%endmacro

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
	mov		cx, DriverIDLength
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
	; CY clear driver found, es is segment of driver
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

%imacro WordAsHex 1-*
	%rep %0
		%define NEED_WordAsHex
		%ifnidni %1, ax
			push	ax
			mov	ax, %1
		%endif
		call	PROC_WordAsHex
		%ifnidni %1, ax
			pop	ax
		%endif
	%rotate 1
	%endrep
%endmacro

; -----------------------------------------------------------------------------

%macro INTERNAL_WordAsInt 0
PROC_WordAsInt:
	push		ax
	push		bx
	push		cx
	push		dx
	mov		bx, 0x000a
	mov		cx, 0x0001
%%Loop:
	cmp		ax, bx
	jae		%%OverNine
	push		ax
%%UnderTen:
	pop		ax
	add		al, 0x30
	ByteAsChar	al
	loop        	%%UnderTen
	jmp		%%Done
%%OverNine:
	inc		cx
	xor		dx, dx
	div		bx
	push		dx
	jmp		%%Loop
%%Done:
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	ret
%endmacro

%imacro WordAsInt 1
	%define NEED_WordAsInt
	%ifnidni %1, ax
		push	ax
		mov	ax, %1
	%endif
	call	PROC_WordAsInt
	%ifnidni %1, ax
		pop	ax
	%endif
%endmacro

; -----------------------------------------------------------------------------

%macro INTERNAL_ParseOptions 0
PROC_ParseOptions:
.MainLoop:
	mov	cx, di
.GetOptLoop:
	mov	dl, [es:di]
	;cmp	dl, 0x0d
	;jbe	.GetOptDone
	cmp	dl, 0x20
	jbe	.GetOptDone
	; ByteAsChar '*', dl, '*'
	inc	di
	jmp	.GetOptLoop
.GetOptDone:
	cmp	cx, di
	jne	.SearchOption
.GetOptNext:
	cmp	[es:di], byte 0x20
	je	.NotDone
	ret
.NotDone:
	inc	di
	jmp	.MainLoop

.SearchOption:
	push	si
	cld
.NextOpt:
	mov	bx, cx  ; start of current option text
.SearchLoop:
	lodsw
	test	ax, ax
	jz	.NotFound
	mov	dx, ax	; option function pointer
	jmp	.CompareLoop
.NotFound:
	lodsw
	mov	dx, ax  ; Catch all function
	jmp	.Matched
.CompareLoop:
	mov	ah, [es:bx]
	inc	bx
	cmp	ah, 0x61
	jb	.NotLowerCase
	cmp	ah, 0x7a
	ja	.NotLowerCase
	sub	ah, 0x20
.NotLowerCase:
	lodsb
	cmp	al, ah
	jne	.MissMatch
	cmp	bx, di
	jne	.CompareLoop
	mov	al, [si]
	test	al, al
	jnz	.MissMatch
.Matched:
	;ByteAsChar 'F'
	;WordAsHex  dx
	;ByteAsChar 0x0d,0x0a
	mov	si, cx
	call	dx
	jmp	.SearchDone
.MissMatch:
	test	al, al
	jz	.NextOpt
	lodsb
	jmp	.MissMatch
.SearchDone:
	pop	si
	jmp	.GetOptNext

%endmacro

%macro ParseOptions 2
	%define NEED_ParseOptions
	mov	si, %1
	%ifnidni %2, di
		mov	di, %2
	%endif
	call	PROC_ParseOptions
%endmacro

; -----------------------------------------------------------------------------

%imacro PrintOptionText 0
	cld
%%Loop:
	cmp		si, di
	je		%%Done
	es lodsb
	ByteAsChar	al
	jmp		%%Loop
%%Done:
%endmacro

; -----------------------------------------------------------------------------

%imacro OptionAsWord 0
	push	    	si
	xor         	ax, ax
	mov         	bl, [es:si]
	cmp		bl, '-'
	je		%%Negative
%%DecLoop:
	cmp	   	si, di
	jz	   	%%Success
	call		%%GetChar
	cmp        	bl, 'X'
	je         	%%AsHex
	sub        	bl, 0x30
	cmp	    	bl, 0x09
	ja	    	%%AsHex
	mov         	cx, 10
	mul         	cx
	xor         	bh, bh
	add         	ax, bx
	jc	    	%%Overflow
	test	    	dx, dx
	jnz	    	%%Overflow
	jmp        	%%DecLoop

%%GetChar:
	mov         	bl, [es:si]
	inc        	si
	cmp		bl, 0x61
	jb		%%NotLower
	cmp		bl, 0x7a
	ja		%%NotLower
	sub		bl, 0x20
%%NotLower:
	ret

%%AsHex:
	xor	    	ax, ax
	pop	    	si
	push        	si

%%HexLoop:
	cmp	   	si, di
	jz	    	%%Success
	call		%%GetChar
	cmp	    	bl, 'X'
	jne	    	%%NotAnX
	test	    	ax, ax
	jz	    	%%HexLoop
	jmp	    	%%Invalid
%%NotAnX:
	sub	    	bl, 0x30
	cmp         	bl, 0x0a
	jb	    	%%Shifting
	cmp	    	bl, 0x10
	jbe	    	%%Invalid
	cmp	    	bl, 0x16
	ja	    	%%Invalid
	sub	    	bl, 0x07
%%Shifting:
	clc
	mov	    	cx, 4
%%ShiftBits:
	rcl	    	ax, 1
	jc	    	%%Overflow
	loop	    	%%ShiftBits
	or	    	al, bl
	jmp	    	%%HexLoop

%%Invalid:
%%Overflow:
%%Negative:
%%Failure:
	stc
	jmp		%%Done
%%Success:
	clc
%%Done:
	pop		si
%endmacro

; -----------------------------------------------------------------------------

%imacro PrintMessage 0-1
	%if %0 = 1
		%ifnidni %1, dx
			mov	dx, %1
		%endif
	%endif
	mov	ah, 0x09
	int	0x21
%endmacro

; -----------------------------------------------------------------------------

%imacro PrintUsage 0

	PrintMessage	%%Buffer
	mov		ax, [es:TDriverHeader.XMS.Count]
	mov		dx, [es:TDriverHeader.XMS.Count+2]
	mov		bx, ax
	or		bx, dx
	test		bx, bx
	jz		%%Empty
	test 		[es:TDriverHeader.XMS.Max], byte sfLogFull
	jnz		%%Full
	mov		cx, [es:TDriverHeader.XMS.Max]
	mov		bx, [es:TDriverHeader.XMS.Max+2]
%%Percent:
	; calculate and display percentage
%%Reduce:
	; reduce both count and max to a single word
	push		di
	xor		di, di
	call		%%ShiftSwap
	call		%%ShiftSwap
	test		di, di
	pop		di
	jz		%%Reduced
	jmp		%%Reduce

%%ShiftSwap:
	; shift dx:ax right
	clc
	xchg		ax, dx
	rcr		ax, 1
	xchg		ax, dx
	rcr		ax, 1
	or		di, dx
	push		ax
	push		dx
	push		cx
	mov		cx, 100
	mul		cx
	or		di, dx
	pop		cx
	pop		dx
	pop		ax

	; swap dx:ax with bx:cx
	xchg		ax, cx
	xchg		dx, bx
	ret

%%Reduced:
	mov		bx, 100
	mul		bx
	div		cx
	WordAsInt	ax
	PrintMessage	%%IsPercent
	jmp		%%And
%%Empty:
	PrintMessage	%%IsEmpty
	jmp		%%And
%%Full:
	PrintMessage	%%IsFull
%%And:
	PrintMessage	%%Logging
	test		[OrgStat], byte sfEnabled
	jnz		%%On
	PrintMessage	%%IsOff
	PrintMessage	%%CRLF
	jmp		%%Done
%%On:
	PrintMessage	%%IsOn
	jmp		%%Done

%%Buffer:	db 'The buffer is $'
%%IsEmpty:	db 'empty$'
%%IsPercent:    db '% '
%%IsFull:	db 'full$'
%%Logging:	db ' and logging is $'
%%IsOff:	db 'disabled.$'
%%IsOn:		db 'active.'
%%CRLF:		db 0x0d,0x0a,'$'

%%Done:
%endmacro

; -----------------------------------------------------------------------------

%imacro PrintStatus 1
	%ifnidni %1, es
		push	es
		%ifidni %1, cs
			push	cs
			pop	es
		%else
			mov	es, %1
		%endif
	%endif
	WordAsInt	[es:TDriverHeader.XMS.Size]
	PrintMessage	%%KB
	cmp		[es:TDriverHeader.XFR.DstHandle], word 0
	je		%%NotXMS
	PrintMessage	%%XMS
	jmp		%%Allocated
%%NotXMS:

%%Allocated:
	PrintMessage	%%MEM
	test		[es:TDriverHeader.Status], byte sfInColor
	jnz		%%InColor
	mov		dx, %%MONO
	jmp		%%Logging
%%InColor:
	PrintMessage	%%COLOR
	mov		dx, %%LOG
%%Logging:
	PrintMessage	dx

	test		[es:TDriverHeader.Status], byte sfLogJAM
	jz		%%NoJAM
	PrintMessage	%%JAM
%%NoJAM:

	%ifnidni %1, cs
		PrintUsage
	%endif

	%ifnidni %1, es
		pop	es
	%endif
	jmp	%%Done

%%KB:	db	'KB of $'
%%MEM:	db	' of memory allocated for $'
%%XMS:
	db	'XMS$'

%%COLOR:db	'color$'
%%MONO: db	'monochrome'
%%LOG:	db	' logging.',0x0d,0x0a,'$'
%%JAM:	db	'Logging will stop when the log is full.',0x0d,0x0a,'$'

%%Done:
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
	ConditionalCode	ParseOptions
	ConditionalCode	WordAsInt
	ConditionalCode	WordAsHex
	ConditionalCode	ByteAsHex
	ConditionalCode	NibbleAsHex
%endmacro