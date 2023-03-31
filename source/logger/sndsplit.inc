; Boot Message Logger Device Driver

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

SendToXMS:
	cmp		[Header(XFR.Count)], word 0
	je		.NoLogData

	; test if sfLogJam (stop when full) bit is set (only set by driver when
	; it is initialized) and if the Log is now Full (always once wraps)
	test 		[Header(Status)], byte sfLogJam
	jz		.NoStopWhenFull
	test 		[Header(Status)], byte sfLogFull
	jnz		.Done
.NoStopWhenFull:

	pushag		; like pusha
	; set SI to first item in XFR record and populate record
	mov		si, Header(XFR.Count)		; address of XFR block
	mov		ax, [Header(XMS.Head)]
	mov		[Header(XFR.DstAddr)], ax
	mov		dx, [Header(XMS.Head)+2]
	mov		[Header(XFR.DstAddr)+2], dx

	; set bx:cx to end of write position
	mov		bx, dx		; copy high word of position
	mov		cx, [si]	; Count
	add		cx, ax		; add low word of position
	adc		bx, 0		; inc bx if needed

	; does it extend past buffer end and need wrapped?
	cmp		bx, [Header(XMS.Max)+2]
	jb		.SendWholeBuffer
	cmp		cx, [Header(XMS.Max)]
	jae		.SendSplitBuffer
.SendWholeBuffer:
	call		.SendBuffer
	jmp		.SendDone

.SendSplitBuffer:
	; ok we need to split the buffer and send it in two pieces

	; figure out how many bytes are in second send
	sub		cx, [Header(XMS.Max)]
	push		cx  			; save for later

	; figure out how many bytes in first part
	mov		bx, [si]	; [Header(XFR.Count)]
	sub		bx, cx

	; set buffer count for first part and send
	mov		[si], bx
	call		.SendBuffer

	; set buffer count for second part
	pop		cx
	mov		bx, [si]	; [Header(XFR.Count)]
	mov		[si], cx

	; move destination write address to buffer origin
	mov		ax, [Header(XMS.Head)]
	mov		[Header(XFR.DstAddr)], ax
	mov		dx, [Header(XMS.Head)+2]
	mov		[Header(XFR.DstAddr)+2], dx

	; adjust send buffer offset for part two and send
	push		bx	; we will restore it after call
	add		[Header(XFR.SrcAddr)], bx ; adjust send buffer
	call		.SendBuffer
	pop		bx
	sub		[Header(XFR.SrcAddr)], bx ; fix send buffer
	jmp		.SendDone

.SendBuffer:

	push		si		; save just in case
	mov		ah, 0x0b	; func 0x0b, DS:SI->XFR Record
	call far 	[Header(XMS.Driver)]
	pop		si

	push		cs
	pop		ds		; mov ds, cs - just in case
	test		ax, ax		; test for XMS Error
	jz		.NoTailMove	; can't do much about it

	; update bytes written count
	mov		ax, [si]	; [Header(XFR.Count)]
	add		[Header(XMS.Count)], ax
	adc		[Header(XMS.Count)+2], word 0
	jnc		.NoCountOverflow
	mov		[Header(XMS.Count)], word 1 ; so it ain't 0
.NoCountOverflow:

	; save current old head position
	push		word [Header(XMS.Head)+2]
	push		word [Header(XMS.Head)]

	; adjust head for next write
	add		[Header(XMS.Head)], ax
	adc		[Header(XMS.Head)+2], word 0

	; load new head - 2
	mov		cx, [Header(XMS.Head)]
	mov		bx, [Header(XMS.Head)+2]
	sub		cx, 2
	sbb		bx, 0

	; if needed, wrap head back to buffer start
	mov		ax, [Header(XMS.Head)+2]
	cmp		ax, [Header(XMS.Max)+2]
	jb		.NoHeadWrap
	mov		ax, [Header(XMS.Head)]
	cmp		ax, [Header(XMS.Max)]
	jb		.NoHeadWrap
	xor		ax, ax
	mov		[Header(XMS.Head)], ax
	mov		[Header(XMS.Head)+2], ax
	; if log wrapped, it is now in a full state and will remain
	; that way until cleared.
	or		[Header(Status)], byte sfLogFull
	; if sfLogJAM, disable logger
	test		[Header(Status)], byte sfLogJAM
	jz		.NoHeadWrap
	and		[Header(Status)], byte 11111110b	; not sfEnabled
.NoHeadWrap:

	; pop old head into dx:ax
	pop		ax
	pop		dx

	; if new (head - 2) is below tail, no need to move
	cmp		bx, [Header(XMS.Tail)+2]
	ja		.NoTailMove
	cmp		cx, [Header(XMS.Tail)]
	ja		.NoTailMove

	; if old head was above the tail, did not overwrite
	cmp		dx, [Header(XMS.Tail)+2]
	ja		.NotOverwriteTail
	cmp		ax, [Header(XMS.Tail)]
	ja		.NotOverwriteTail

	; we wrote the buffer tail and need to move it to the new head
	; because head is the next place we will write, so is still
	; oldest data in buffer.
	mov		ax, [Header(XMS.Head)]
	mov		[Header(XMS.Tail)], ax
	mov		ax, [Header(XMS.Head)+2]
	mov		[Header(XMS.Tail)+2], ax
	jmp		.NoTailMove
.NotOverwriteTail:
	; since we did not overwrite the tail, the tail could be in
	; it's invalid initial state of -1. If so, point it at the
	; first byte in the buffer.
	cmp		[Header(XMS.Tail)], word -1
	jne		.NoTailMove
	cmp		[Header(XMS.Tail)+2], word -1
	jne		.NoTailMove
	xor		ax, ax
	mov		[Header(XMS.Tail)], ax
	mov		[Header(XMS.Tail)+2], ax

.NoTailMove:
	ret

.SendDone:
	popag		; like popa

.Done:
	mov		[Header(XFR.Count)], word 0	; set current count to 0
.NoLogData:
	ret
