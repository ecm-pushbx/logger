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

; -----------------------------------------------------------------------------
; When NO_SLIT_SEND is defined, this code is used to transfer the buffer to log
; storage. It will only send the entire buffer at once. If there is not
; sufficient storage and sfLogJAM is not enabled. It will move to the first
; storage location in the buffer and write the contents there. If sfLogHAM is
; enabled. It will just stop and disable logging.
; -----------------------------------------------------------------------------

SendToXMS:
	; if send buffer is empty, we are done
	cmp		[Header(XFR.Count)], word 0
	jne		.TestFull
	ret

.TestFull:
	; test if sfLogJam (stop when full) bit is set (only set by driver when
	; it is initialized) and if the Log is now Full (always once wraps)
	test 		[Header(Status)], byte sfLogJam
	jz		.NeedToSend
	test 		[Header(Status)], byte sfLogFull
	jnz		.DoNotSend
	ret

.NeedToSend:

	pushag		; like pusha
	call		.BufferHead
	call		.BufferEnd

	; does it extend past buffer end and need wrapped?
	cmp		bx, [Header(XMS.Max)+2]
	jb		.OkToSend
	cmp		cx, [Header(XMS.Max)]
	jb		.OkToSend

	; Send won't fit, buffer is full
	or		[Header(Status)], byte sfLogFull
	test 		[Header(Status)], byte sfLogJam
	jz		.NoLogJam
	and		[Header(Status)], byte 11111110b	; not sfEnabled
	jmp		.SendDone
.NoLogJam:
	; YAY, we are allowed to wrap buffer

	; move TOP pointer to current HEAD dx:ax
	mov		[Header(XMS.Top)], ax
	mov		[Header(XMS.Top)+2], dx

	; move HEAD to buffer origin
	xor		ax, ax
	xor		dx, dx

.OkToSend:
	call		.SendBuffer

.SendDone:
	popag		; like popa
.DoNotSend:
	mov		[Header(XFR.Count)], word 0
	ret

.BufferHead:
	mov		ax, [Header(XMS.Head)]
	mov		dx, [Header(XMS.Head)+2]
	ret

.BufferEnd:
	; IN dx:ax = buffer start/head
	; set SI to first item in XFR record and populate record
	mov		si, Header(XFR.Count)		; address of XFR block
	; set bx:cx to end of write position
	mov		bx, dx		; copy high word of position
	mov		cx, [si]	; Count
	add		cx, ax		; add low word of position
	adc		bx, 0		; inc bx if needed

	; return bx:cx=end, si->count/xfr record
	ret

.SendBuffer:
	mov		[Header(XFR.DstAddr)], ax
	mov		[Header(XFR.DstAddr)+2], dx
	push		si		; save just in case
	mov		ah, 0x0b	; func 0x0b, DS:SI->XFR Record
	call far 	[Header(XMS.Driver)]
	pop		si

	push		cs		; mov ds, cs - just in case
	pop		ds
	test		ax, ax		; test for XMS Error
	jnz		.NoError	; can't do much about it
	ret

.NoError:
	mov		ax, [Header(XFR.DstAddr)]
	mov		dx, [Header(XFR.DstAddr)+2]
	call		.BufferEnd
	test		ax, ax
	jnz		.CheckPushTail
	test		dx, dx
	jnz		.CheckPushTail
	; wrote to first xms location, means: just started or wrapped
	push		ax
	test		[Header(Status)], byte sfLogFull ; only reset on clear
	jnz		.WrapAdjust
	; just starting out, so not wrapped, move tail to first word
	xor		ax, ax
	mov		[Header(XMS.Tail)], ax
	mov		[Header(XMS.Tail)+2], ax
	pop		ax
	jmp		.CheckTop
.WrapAdjust:
	; we just buffer wrapped, adjust to old HEAD
	push		dx
	call		.BufferHead
	mov		[Header(XMS.Top)], ax
	mov		[Header(XMS.Top)+2], dx
	pop		dx
.WrapAdjustDone:
	pop		ax
	jmp		.PushTail

.CheckPushTail:
	; not wrapped or just starting to write, check if tail needs pushed
	cmp		bx, [Header(XMS.Tail)+2]
	jb		.MaybePushTail
	cmp		cx, [Header(XMS.Tail)]
	jb		.CheckTop
.MaybePushTail:
	cmp		dx, [Header(XMS.Tail)+2]
	ja		.CheckTop
	cmp		ax, [Header(XMS.Tail)]
	ja		.CheckTop
.PushTail:
	; TAIL was over-written or we wrapped, adjust to end of write
	mov		[Header(XMS.Tail)], cx
	mov		[Header(XMS.Tail)+2], bx

.CheckTop:

	; set new HEAD from write END bx:cx
	mov		[Header(XMS.Head)], cx
	mov		[Header(XMS.Head)+2], bx

	; if write END > TOP, adjust TOP to write END
	cmp		bx, [Header(XMS.Top)+2]
	jb		.NoAdjustTop
	cmp		cx, [Header(XMS.Top)]
	jbe		.NoAdjustTop
	mov		[Header(XMS.Top)], cx
	mov		[Header(XMS.Top)+2], bx
.NoAdjustTop:

	; update total byte count written to buffer
	mov		ax, [si]
	add		[Header(XMS.Count)], ax
	adc		[Header(XMS.Count)+2], word 0
	jnc		.NoCountOverflow
	mov		[Header(XMS.Count)], word 1 ; so it ain't ever 0
.NoCountOverflow:

	ret
