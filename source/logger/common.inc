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
; experimental and development conditional defines.
; you should probably not change them.
; -----------------------------------------------------------------------------

; Include AMIS interrupt hook support in Device Driver
%define HOOK_AMIS

; include some debugging code and the DEBUG option in LOGGER.COM
;
; %define DEBUG

; include a debugging data buffer for checking values
;
;%define DEBUG_DATA_SIZE	8	; debug data words, size is words * 2

; Noticed some weirdness when using Direct Capture in 40 column modes under
; VirtualBox. There may also be issues on real hardware or other Virtual
; platforms. So, I'm going to have the driver just use TTL capture in those
; modes. Enable "DIRECT_40_COLUMNS" to use DirectMode mode instead.
;
; %define DIRECT_40_COLUMNS

; Do not enable, only works under FreeDOS when Driver is loaded in UMB.
; otherwise, system is very unstable if it boots at all.
;
; %define LOW_MEM_LOG

; HTML output support
%define HTML_SUPPORT

; -----------------------------------------------------------------------------

%ifdef HOOK_AMIS
	%include 'hookamis.inc'
%else
	%include 'hooknone.inc'
%endif

%define MaxXFRSize  80 * 2

; -----------------------------------------------------------------------------

%idefine VersionID 0x0001	; High byte = Major, Low Byte = Minor
%idefine Version '0.1-BETA'

%imacro CopyrightText 0
	%ifndef DEVICE_DRIVER
		db	'Copyright '
	%endif
	db 	'(c)', 0x20
	db 	%substr(__DATE__,1,4), ',', 0x20
	db 	'Jerome Shidel'
	%ifdef DEVICE_DRIVER
		db	', BSD 3-Clause License', 0x0d, 0x0a
	%else
		db 	0x0d, 0x0a
		db	'BSD 3-Clause License', 0x0d, 0x0a, 0x0a
	%endif
%endmacro

%idefine VendorDriverID db 'J.SHIDEL'	; 8 character driver vendor
%idefine DeviceDriverID db 'LOGGERxx'	; 8 character driver name
%idefine ProductName	db 'Message Logger',0	; ASCIIZ, 64 byte max

%idefine DriverIDLength 8

; driver status flags (low byte)
%idefine sfEnabled 	00000001b	; bit 0 = logging enabled
%idefine sfModeChange	00000010b	; bit 1 = mode changed, re-config driver
%idefine sfVESAChange	00000100b	; bit 2 = VESA Mode change flag
%idefine sfDirectMode	00001000b	; bit 3 = advanced capture mode active
%idefine sfSupport	00010000b	; bit 4 = card supports advanced mode
%idefine sfInColor	00100000b	; bit 5 = include color attribs in log
%idefine sfLogJam	01000000b	; bit 6 = stop when full
%idefine sfLogFull	10000000b	; bit 7 = log full
; driver status flags (high byte) 	  bits 8-15 reserved


; interface status and option flags (low byte) - internal use only
%idefine ofKeepStatus	00000001b	; bit 0 = restore driver status on exit
%idefine ofHadOptions	00000010b	; bit 1 = command line options provided
%idefine ofColorPrint	00000100b	; bit 2 = MONO/ANSI printing selected
%idefine ofShowVersion  00001000b	; bit 3 = print version information
%idefine ofShowHelp	00010000b	; bit 4 = print help and exit
%idefine ofStdIn	00100000b	; bit 5 = standard input added to log
%idefine ofBadVersion   01000000b	; bit 6 = incompatible driver version
%idefine ofPreTest	10000000b	; bit 7 = CmdLn Option Pre-Check
; interface status and option flags (high byte)  - internal use only
%idefine ofPassThru	00000001b	; bit 8 = also send any STDIN to STDOUT
%idefine ofHushMode	00000010b	; bit 9 = do not warn about no driver


; interface viewer navigation control flags (low byte)  - internal use only
%idefine vfAtTop	00000001b	; bit 0 = can not scroll up
%idefine vfAtBottom     00000010b	; bit 1 = can not scroll down
%idefine vfAtLeftmost   00000100b	; bit 2 = can not scroll left
%idefine vfAtRightmost  00001000b	; bit 3 = should not scroll right
%idefine vfNoDraw	00010000b	; bit 4 = draw for calculations only
%idefine vfIsEmptyLog   10000000b	; bit 7 = empty log

; -----------------------------------------------------------------------------

struc TDriverHeader
	.ChainNext: 	resd 1			; pointer to next device driver
	.Attributes:	resw 1			; character device
	.Strategy:	resw 1			; set request block pointer
	.Entry:		resw 1			; driver interrupt call
	.Name:		resb DriverIDLength	; 8 character driver name

	.Dispatch:	resd 1			; far call to driver functions
	.Status:	resw 1			; Device driver Status


	.XMS.Driver:	resd 1			; Pointer to XMS driver
	.XMS.Head:	resd 1			; next buffer write position
	.XMS.Tail:	resd 1			; first buffer read position

	; DO NOT REORDER Size, Count, Max! Driver returns a pointer to these
	; values in the dispatcher status request function.
	.XMS.Size:	resw 1			; Size in KB to allocate
	.XMS.Max:	resd 1			; size of buffer in bytes
	.XMS.Count:	resd 1			; total bytes written to buffer

	.XFR.Count:	resd 1			; byte count { must be even }
	.XFR.SrcHandle:	resw 1			; 0 = conventional memory
	.XFR.SrcAddr:	resd 1			; pointer to source buffer
	.XFR.DstHandle:	resw 1			; XMS handle
	.XFR.DstAddr:	resd 1			; pointer to destination

	.XFR.Buffer:	resb MaxXFRSize 	; TTL XFR Buffer

	.RowSkip:	resw 1			; Number of rows to skip if
						; using DirectMode.

%ifdef DEBUG_DATA_SIZE
	.DebugData:	resw DEBUG_DATA_SIZE
%endif

endstruc

; -----------------------------------------------------------------------------

struc TVideoData
	; data copied from BIOS memory area
	.Mode:		resb	1		; current video mode
	.Columns:	resw	1		; screen columns
	.Regen:		resw	1		; regen buffer size
	.Offset:	resw	1		; videop page offset
	.Position:	resw	8		; cursor page positions
	.Cursor:	resw	1		; cursor shape
	.Page:		resb	1		; current video page
	.EndBDA:				; end of copied video BIOS data
	; calculated data
	.Direct:	resb 	1		; direct video supported
	.Rows:		resw 	1		; rows on screen
	.VSeg:		resw 	1		; direct video segment
endstruc

; -----------------------------------------------------------------------------

struc TTransfer
	.Count:		resd 1		; byte count { must be even }
	.SrcHandle:	resw 1		; 0 = conventional memory, or Handle
	.SrcAddr:	resd 1		; pointer to source buffer
	.DstHandle:	resw 1		; XMS handle
	.DstAddr:	resd 1		; pointer to destination
endstruc

; -----------------------------------------------------------------------------

%idefine IdleCPU	hlt

; -----------------------------------------------------------------------------

%imacro	CheckCompatible 0
	%ifdef DEVICE_DRIVER
		clc
	%else
		clc
	%endif
%endmacro

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

%imacro CMPDD	3
	cmp 	%1, [%3+2]
	jne	%%Done
	cmp	%2, [%3]
%%Done:
%endmacro

; -----------------------------------------------------------------------------

%imacro IISP 0

.EntryPoint:		jmp	.EndOfHeaderIISP
.NextHandler:		dd	0		; Next Handler in the chain
			dw	0x424b		; IISP Signature
			db	0x00		; EOI Flag
			jmp	.FarReturnIISP
			times 7 db 0		; 7 Reserved bytes
.EndOfHeaderIISP:	jmp	.InterruptHandler
.FarReturnIISP:		retf
.InterruptHandler:
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

%macro INTERNAL_ByteAsBin 0
PROC_ByteAsBin:
	push		cx
	push		ax
	mov		cl, 0x08
%%Loop:
	test		al, 0x80
	jz		%%Zero
	ByteAsChar	'1'
	jmp		%%Next
%%Zero:
	ByteAsChar	'0'
%%Next:
	shl		al, 1
	loop		%%Loop
	pop		ax
	pop		cx
	ret
%endmacro

%imacro ByteAsBin 1
	%define NEED_ByteAsBin
	%ifnidni %1, al
		push	ax
		mov	al, %1
	%endif
	call	PROC_ByteAsBin
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

%macro INTERNAL_WordAsUInt 0
PROC_WordAsUInt:
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

%imacro WordAsUInt 1
	%define NEED_WordAsUInt
	%ifnidni %1, ax
		push	ax
		mov	ax, %1
	%endif
	call	PROC_WordAsUInt
	%ifnidni %1, ax
		pop	ax
	%endif
%endmacro

%imacro ByteAsUInt 1
	push		ax
	mov		al, %1
	xor		ah, ah
	WordAsUInt 	ax
	pop		ax
%endmacro

; -----------------------------------------------------------------------------

%macro INTERNAL_ParseOptions 0
PROC_ParseOptions:
.MainLoop:
	mov	cx, di
.GetOptLoop:
	mov	dl, [es:di]
	; check for separator
	cmp	dl, 0x20
	jbe	.GetOptDone	; anything that is a space or lower
	cmp	dl, 0x3b
	je	.GetOptDone	; semi-colon
	cmp	dl, 0x2c
	je	.GetOptDone	; comma
	; ByteAsChar '*', dl, '*'
	inc	di
	jmp	.GetOptLoop
.GetOptDone:
	cmp	cx, di
	jne	.SearchOption
.GetOptNext:
	; if it was a separator, continue processing
	cmp	[es:di], byte 0x20 ; space
	je	.NotDone
	cmp	[es:di], byte 0x2c ; comma
	je	.NotDone
	cmp	[es:di], byte 0x3b ; semi-colon
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
	test 		[es:TDriverHeader.Status], byte sfLogFull
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
	WordAsUInt	ax
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
	WordAsUInt	[es:TDriverHeader.XMS.Size]
	PrintMessage	%%KB
	mov		dx, [es:TDriverHeader.XMS.Driver+2]
	mov		ax, es
	cmp		ax, dx
	je		%%NotXMS
	PrintMessage	%%XMS
	jmp		%%Allocated
%%NotXMS:
	mov		dx, [es:TDriverHeader.XFR.DstHandle]
	cmp		dx, 0xa000
	jae		%%IsUMB
	PrintMessage	%%LOW
	jmp		%%Allocated
%%IsUMB:
	PrintMessage	%%UMB
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
%%MEM:	db	' memory allocated for $'
%%XMS:
	db	'XMS$'
%%UMB:
	db	'UMB$'
%%LOW:
	db	'LOW$'

%%COLOR:db	'color$'
%%MONO: db	'monochrome'
%%LOG:	db	' logging.',0x0d,0x0a,'$'
%%JAM:	db	'Logging will stop when the log is full.',0x0d,0x0a,'$'

%%Done:
%endmacro

; -----------------------------------------------------------------------------

%imacro DebugStatus 1
	push	  	dx
	push		ax
	push		cx
	push		bx
	PrintMessage 	%%DriverSeg
	WordAsHex 	%1
	PrintMessage 	%%EOL
	PrintMessage 	%%PreStat
	ByteAsBin 	[OrgStat]
	ByteAsChar	'/'
	ByteAsBin 	[es:TDriverHeader.Status]
	ByteAsChar	' ', '+', ' '
	ByteAsHex 	[OrgStat+1]
	PrintMessage 	%%EOL
	PrintMessage 	%%PreDriver
	WordAsHex 	[es:TDriverHeader.XMS.Driver+2]
	ByteAsChar	':'
	WordAsHex 	[es:TDriverHeader.XMS.Driver]
	PrintMessage 	%%EOL
	PrintMessage 	%%PreHandle
	WordAsHex 	[es:TDriverHeader.XFR.DstHandle]
	PrintMessage 	%%EOL

	PrintMessage 	%%PreXMSKB
	WordAsHex 	[es:TDriverHeader.XMS.Size]
	PrintMessage 	%%EOL
	PrintMessage 	%%PreXMSMax
	WordAsHex 	[es:TDriverHeader.XMS.Max+2], [es:TDriverHeader.XMS.Max]
	PrintMessage 	%%EOL
	PrintMessage 	%%PreXMSCount
	WordAsHex 	[es:TDriverHeader.XMS.Count+2], [es:TDriverHeader.XMS.Count]
	PrintMessage 	%%EOL
	PrintMessage 	%%PreXMSHead
	WordAsHex 	[es:TDriverHeader.XMS.Head+2], [es:TDriverHeader.XMS.Head]
	PrintMessage 	%%EOL
	PrintMessage 	%%PreXMSTail
	WordAsHex 	[es:TDriverHeader.XMS.Tail+2], [es:TDriverHeader.XMS.Tail]
	PrintMessage 	%%EOL
%ifdef DEBUG_DATA_SIZE
	PrintMessage 	%%PreData
	mov		cx, DEBUG_DATA_SIZE
	xor		bx,bx
%%Loop:
	mov		ax, [es:TDriverHeader.DebugData+bx]
	WordAsHex	ax
	add		bx, 2
	ByteAsChar	' '
	loop		%%Loop
	PrintMessage 	%%EOL
%endif
	pop		bx
	pop		cx
	pop		ax
	pop		dx
	jmp		%%Over
%%DriverSeg:	db	'DRIVER SEG: $'
%%PreStat:	db	'STATUS:     $'
%%PreData:	db	'DATA:       $'
%%PreDriver:	db	'MEMORY DRV: $'
%%PreHandle:	db	'MEMORY HDL: $'
%%PreXMSKB:	db	'ALLOCATED:  $'
%%PreXMSMax:	db	'MAX:        $'
%%PreXMSCount:	db	'COUNT:      $'
%%PreXMSTop:	db	'TOP:        $'
%%PreXMSHead:	db	'HEAD:       $'
%%PreXMSTail:	db	'TAIL:       $'

%%EOL:		db	0x0d,0x0a,'$'
%%Over:
%endmacro

; -----------------------------------------------------------------------------

%macro INTERNAL_StdIn 0
PROC_StdIn:
	push		dx
	push		ax
	mov		ah, 0x0b
	int		0x21
	cmp		al, 0
	je		%%NoInput
	mov		ah, 0x08
	int		0x21
	clc
	jmp		%%Done
%%NoInput:
	xor		al, al
	stc
%%Done:
	mov		dl, al
	pop		ax
	mov		al, dl
	pop		dx
	ret
	; CY set, no input al=0
	; CY clear, al=character
%endmacro

%imacro StdIn 0
	%define NEED_StdIn
	call	PROC_StdIn
%endmacro

; -----------------------------------------------------------------------------

%imacro StdOut 0
	pushf
	push		ax
	push		bx
	push		cx
	push		dx
	mov		[StdOut_Buffer], al
	mov		ah, 0x40
	mov		bx, 0x0001		; StdOut Handle
	mov		cx, bx			; Number of bytes
	mov		dx, StdOut_Buffer	; ds:dx -> buffer
	int		0x21
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
%endmacro

; -----------------------------------------------------------------------------

%macro INTERNAL_VideoSettings 0
PROC_VideoSettings:
	; copy video settings for later
	push		di
	push		si
	push		es
	push		ds

	push		cs
	pop		es
	mov		ax, 0x0040
	mov		ds, ax
	mov		si, 0x0049
	mov		di, VideoData
	mov		cx, TVideoData.EndBDA
	cld
	rep		movsb
	; check that we are in a compatible text mode to save screen
	mov		bx, 0xb000 	; mono segment
	mov		al, [0x0049]	; video mode
	cmp		al, 0x07
	je		.ModeOk
	mov		bx, 0xb800 	; color segment
	cmp		al, 0x03
	jbe		.ModeOk
	; possible support other modes like 132x50
	jmp		.ModeBad
.ModeOk:
	mov		ax, [0x004c]	; regen size
	cmp		ax, 0x4000
	ja		.ModeBad
	mov		[es:di], byte 1	; TVideoData.Direct
	inc		di
	push		ax
	xor		dx, dx
	shr		ax, 1
	mov		cx, [0x004a] 	; columns
	div		cx	     	; ax=rows
	xchg		cx, ax
	pop		dx		; was ax
	clc
	jmp		.Done
.ModeBad:
	xor		ax, ax
	stosb				; TVideoData.Direct
	stc
.Done:
	mov		[es:di], cx	; TVideoData.Rows
	mov		[es:di+2], bx	; TVideoData.VSeg
	pop		ds
	pop		es
	pop		si
	pop		di
	; VideoData always populated, VideoDirect = 1 compatible, 0 incompatible
	; CY set if incompatible to direct video mode or copy
	; CY clear if compatible, bx=video segment, dx=regen words, ax=columns,
	; cx=rows

	ret
%endmacro

%imacro VideoSettings 0
	%define NEED_VideoSettings
	call	PROC_VideoSettings
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
	ConditionalCode VideoSettings
	ConditionalCode StdIn
	ConditionalCode	WordAsUInt
	ConditionalCode	WordAsHex
	ConditionalCode	ByteAsHex
	ConditionalCode	ByteAsBin
	ConditionalCode	NibbleAsHex
%endmacro