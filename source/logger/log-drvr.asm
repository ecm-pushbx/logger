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

%define DEVICE_DRIVER

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

; -----------------------------------------------------------------------------

DriverHeader:
istruc TDriverHeader
	%ifdef SINGLE_BINARY
		at .ChainNext,  jmp near SkipDriver	; one binary jump past
							; driver to interface
	%else
		at .ChainNext, 	dd -1		; pointer to next device driver
	%endif
	at .Attributes,	dw 0x8000		; character device
	at .Strategy,	dw Initial.Strategy	; set tREQUEST block pointer
	at .Entry,	dw Initial.Routine	; driver interrupt call
	at .Name,	DeviceDriverID		; 8 character driver name

; -----------------------------------------------------------------------------
; Data that may be interacted with directly by the interface program (for now)

	at .Status,		dw 0		; Device driver Status

	at .Flush,		dd FlushFarCall	; Function to Flush the current
						; buffer to Log storage.

	at .XMS.Driver,		dd -1		; Pointer to XMS driver
	at .XMS.Size,		dw 32		; Default KB size to allocate
	at .XMS.Head,		dd 0		; next buffer write position
	at .XMS.Tail,		dd -1		; first buffer read position
	at .XMS.Count,		dd 0		; bytes in buffer
	at .XMS.Max,		dd 0 		; size of buffer in bytes

						; XMS transfer Buffer
	at .XFR.Count,		dd 0		; byte count { must be even }
	at .XFR.SrcHandle,	dw 0		; 0 = conventional memory
	at .XFR.SrcAddr,	dd 0		; pointer to source buffer
	at .XFR.DstHandle,	dw -1		; XMS handle, or
						; UMB/LOW memory buffer segment
	at .XFR.DstAddr,	dd 0		; pointer to destination

	at .XFR.Buffer,		db 0		; TTL XFR Buffer of MaxXFRSize

	at .RowSkip,		dw 0		; Number of rows to skip if
						; using DirectMode.

iend

%define Header(x) DriverHeader + TDriverHeader. %+ x

; -----------------------------------------------------------------------------

%ifdef HOOK_AMIS
	AMIS_Signature
%endif

; -----------------------------------------------------------------------------
; The standard DOS function calls to interact with the driver.
; Normally, you can save ES:BX when DOS calls the Driver.Strategy function.
; Then process the contents of that record when DOS calls the Entry routine.
; However, since the first Entry call is to initialize the driver and no further
; interaction through those DOS calls is needed or desired, we put that code
; elsewhere and change those header pointers to this dummy functions that
; simple return an "unsupported" error code.
; -----------------------------------------------------------------------------

Driver.Strategy:
	; set error & done bits, and error code 3, unknown command
	mov		[es:bx+tREQUEST.Status], word 0x8103
Driver.Routine:
	retf

; -----------------------------------------------------------------------------
; Device Driver Interrupt 10 Handler
; -----------------------------------------------------------------------------

DevInt10:
	IISP
	pushf
	cli
	push		ds
	push		cs
	pop		ds	; mov ds, cs


	; was mode previously changed?
	test		[Header(Status)], byte sfModeChange
	jz		.NoModeResetFlag

	and		[Header(Status)], byte ~sfModeChange

	test		[Header(Status)], byte sfSupport
	jz		.CaptureTTL

	call		ConfigCapture

.NoModeResetFlag:

	test		[Header(Status)], byte sfDirectMode ; direct method flag
	jz		.CaptureTTL

	call		DirectCapture

	jmp		.CheckModeChange

.CaptureTTL:
	; Simple BIOS TTL capture
	; is driver enabled?
	test		[Header(Status)], byte sfEnabled
	jz		.CheckModeChange

	cmp		ah, 0x0e
	jne		.CheckModeChange
	test		bh, bh			; if not page 0, don't capture
	jnz		.Done
	call		AppendBuffer
	jmp		.Done

.CheckModeChange:
	cmp		ax, 0x4f02		; set vesa mode
	jne		.NotVESAChange
	or		[Header(Status)], byte sfVESAChange ; set flag
	jmp		.ModeChanging
.NotVESAChange:
	test		ah, ah
	jnz		.Done
.ModeChanging:
	call		FlushBuffer
	or		[Header(Status)], byte sfModeChange ; set flag
.Done:
	pop		ds
	popf
	jmp		far [cs:.NextHandler]

; -----------------------------------------------------------------------------

%ifdef HOOK_AMIS
	DriverInterruptHook
%endif

; -----------------------------------------------------------------------------

AppendBuffer:
	push		di
	mov		di, [Header(XFR.Count)]
	mov		[Header(XFR.Buffer)+di], al	; character
	inc		di
	test		[Header(Status)], byte sfInColor
	jz		.NoColorAttrib
	mov		[Header(XFR.Buffer)+di], bl	; attribute
	inc		di
.NoColorAttrib:
	mov		[Header(XFR.Count)], di
	cmp		di, MaxXFRSize		; send if TTL buffer is full
	pop		di
	je		.SendTTLBuffer
	ret
.SendTTLBuffer:
	call		SendToLog
	ret

; -----------------------------------------------------------------------------
; FAR function for external Log interface program. This should go away. It
; should be replaced with a standard interrupt or function dispatcher call
; to the logging driver.
; -----------------------------------------------------------------------------

FlushFarCall:
	; far call to send buffer contents to Log storage. It is not effected
	; by driver enabled state.
	pushf
	push		ds
	push		cs
	pop		ds
	call		FlushBuffer
	pop		ds
	popf
	retf

; -----------------------------------------------------------------------------
; Buffer Flush routine that insures all data stored in the buffer or not yet
; processed in DirectMode has been written to the log.
; -----------------------------------------------------------------------------

FlushBuffer:
	test		[Header(Status)], byte sfDirectMode ; direct method flag
	jz		.SendBuffer
	call		DirectFlush
.SendBuffer:
	call		SendToLog
	ret

; -----------------------------------------------------------------------------
; Transfer buffer to Log storage.
; -----------------------------------------------------------------------------

SendToLog:
	; if send buffer is empty, we are done
	cmp		[Header(XFR.Count)+2], word 0
	jne		.TestFull
	cmp		[Header(XFR.Count)], word 0
	jne		.TestFull
	ret

.TestFull:
	; test if sfLogJam (stop when full) bit is set (only set by driver when
	; it is initialized) and if the Log is now Full (always once it wraps)
	test 		[Header(Status)], byte sfLogJam
	jz		.NeedToSend
	test 		[Header(Status)], byte sfLogFull
	jnz		.DoNotSend
	ret

.NeedToSend:

	pushag		; like pusha

	; make sure we have an even number of bytes
	test		[Header(XFR.Count)], byte 1
	jz		.EvenBytes
	; if not, then slap a zero on the end, assume send buffer is Driver's
	; buffer.
	;push		es
	;mov		es, [Header(XFR.SrcAddr)+ 2]
	mov		di, [Header(XFR.SrcAddr)]
	mov		bx, [Header(XFR.Count)]
	mov		[di+bx], byte 0
	; mov		[es:di+bx], byte 0
	inc		bx
	mov		[Header(XFR.Count)], bx
	; pop		es
.EvenBytes:

	call		.BufferHead
	call		.BufferEnd

	; does it extend past buffer end and need wrapped?
	cmpdd		bx, cx, Header(XMS.Max)
	jb		.OkToSend

	; Send won't fit, buffer is full
	or		[Header(Status)], byte sfLogFull
	test 		[Header(Status)], byte sfLogJam
	jz		.NoLogJam
	and		[Header(Status)], byte ~sfEnabled	; not sfEnabled
	jmp		.SendDone

.NoLogJam:
	; YAY, we are allowed to wrap buffer

	; ok we need to split the buffer and send it in two pieces

	; calculate distance from HEAD dx:ax to MAX bx:cx for first send
	mov		bx, [Header(XMS.Max)]	; MAX is greater than HEAD
	sub		bx, ax			; so, don't care about borrow

	; bytes not in first send will go into second send
	mov		cx, [si]
	sub		cx, bx

	; bx=first send
	; cx=second send

	; save size of first & second send for later
	push		bx
	push		cx

	; set buffer count for first part and send
	mov		[si], bx
	call		.SendBuffer

	; restore size of first and second SendToLog
	pop		cx
	pop		bx

	; set buffer count for second part and move HEAD
	mov		[si], cx
	xor		ax, ax
	xor		dx, dx

	; temporary adjust send buffer offset for part two and send
	push		bx	; we will restore it after call
	add		[Header(XFR.SrcAddr)], bx ; adjust send buffer
	call		.SendBuffer
	pop		bx
	sub		[Header(XFR.SrcAddr)], bx ; fix send buffer
	jmp		.SendDone

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
	pop		ax
	jmp		.PushTail

.CheckPushTail:
	; not wrapped or just starting to write, check if tail needs pushed
	cmpdd		bx, cx, Header(XMS.Tail)
	jb		.CheckTop
.MaybePushTail:
	cmpdd		dx, ax, Header(XMS.Tail)
	ja		.CheckTop
.PushTail:
	; TAIL was over-written or we wrapped, adjust to end of write
	mov		[Header(XMS.Tail)], cx
	mov		[Header(XMS.Tail)+2], bx

.CheckTop:
	; set new HEAD from write END bx:cx
	mov		[Header(XMS.Head)], cx
	mov		[Header(XMS.Head)+2], bx

	; update total byte count written to buffer
	mov		ax, [si]
	add		[Header(XMS.Count)], ax
	adc		[Header(XMS.Count)+2], word 0
	jnc		.NoCountOverflow
	mov		[Header(XMS.Count)], word 1 ; so it ain't ever 0
.NoCountOverflow:
	ret

; -----------------------------------------------------------------------------
; If direct video mode not supported by the hardware,
; this won't be needed after initialization
; -----------------------------------------------------------------------------

ConfigCapture:

	; mode was changed, adjust settings
	push		es
	push		ax
	push		bx
	push		cx
	push		dx

	; clear ModeChange and DirectMethod bits, assume TTL capture mode
	and		[Header(Status)], byte ~(sfModeChange + sfDirectMode)

	; mov		[Header(XFR.Count)], word 0 ; don't worry about high word
	mov		[Header(XFR.SrcAddr)], word Header(XFR.Buffer)
	mov		[Header(XFR.SrcAddr)+2], cs

	; if it was not a VESA mode, check standard modes
	test		[Header(Status)], byte sfVESAChange
	jz		.StandardModes
	and		[Header(Status)], byte ~sfVESAChange ; clear VESA flag

	; Since all the flags are up to date and we are in TTL mode, it should
	; be safe to call int 10 to fetch the current VESA mode.
	mov		ax, 0x4f03
	int 		0x10
	cmp		ax, 0x004f		; successful and supported
	jne		.SetMethodDone
	; Ignore some mode flags that may be in BH
	and		bh, 00011111b
	; BX modes 0-0xff are standard modes
	test		bh, bh
	jnz		.SetMethodDone
	mov		ax, 0x0040
	mov		es, ax
	mov		al, bl
	jmp		.CheckMode

.StandardModes:
	; is it a supported text mode?
	mov		ax, 0x0040
	mov		es, ax
	mov		al, [es:0x0049]		; current video mode
.CheckMode:
	and		al, 0x7f
	mov		bx, 0xb000 		; mono segment
	cmp		al, 0x07		; is 80x25 mono?
	je		.MaybeDirectMethod
	mov		bx, 0xb800 		; mono segment
	%ifndef DIRECT_40_COLUMNS
		cmp		al, 0x02		; is 40 column?
		jb		.SetMethodDone

	%endif
	cmp		al, 0x03		; not 80x? or 40x?
	jbe		.MaybeDirectMethod
	; possible support other modes like 132x50
	jmp		.SetMethodDone
.MaybeDirectMethod:
	mov		ax, [es:0x004c]		; regen size
	cmp		ax, 0x4000		; Sanity check, over 16k fail
	ja		.SetMethodDone
	xor		dx, dx
	shr		ax, 1
	mov		cx, [es:0x004a] 	; columns
	test		cx, cx
	jz		.SetMethodDone		; divide by zero, fail
	div		cx	     		; cx=columns,ax=rows
	cmp		ax, 13
	jb		.SetMethodDone		; rows < 13, fail
	cmp		cx, 132
	ja		.SetMethodDone		; over 132 columns, fail
	cmp		ax, 50
	ja		.SetMethodDone		; over 50 rows, fail
.SetDirectMethod:
	; mode supported
	mov		[DirectData.VSeg], bx
	mov		ch, al
	mov		[DirectData.MaxXY], cx
	mov		[Header(RowSkip)], word 0 ; set start row to 0
	or		[Header(Status)], byte sfDirectMode + sfSupport
.SetMethodDone:
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	pop		es
	ret

; -----------------------------------------------------------------------------

DirectCapture:
	push		es
	push		ax
	push		bx
	push		cx
	push		ax
	mov		ax, 0x0040
	mov		es, ax
;	mov		al, [0x0050+1]
;	cmp		ax,  [Header(RowSkip)]
;	jae		.CursorAboveSkip
;	cmp		[Header(RowSkip)], ax
;.CursorAboveSkip:
	pop		ax
	test 		ah, ah
	jz		.ModeChange
	cmp		ax, 0x0e0a		; write TTL LF
	jne		.NotLineFeedTTL
	test		bh, bh
	jnz		.Done			; Not LF on page 0
	mov		ax, [es:0x0050]		; page 0 cursor position
	inc		ah
	cmp		ah, [DirectData.MaxY]
	jb		.Done			; will it scroll up a line
	mov		al, 1
	jmp		.ScrollUp
.NotLineFeedTTL:
	cmp		[es:0x0062], byte 0 	; current video page
	jne		.Done			; if not on page 0, we are done
	cmp		ax, 0x0700		; scroll down all
	je		.ScreenClear
	cmp		ah, 0x06		; scroll up
	jne		.Done
.ScrollUp:
	test		al ,al
	jz		.ScreenClear		; scroll up all lines
	; scroll up AL count
	mov		cl, al
	call		DirectSendLines
	sub		bx, cx
	cmp		bx, 0
	jae		.AdjustSkip
	xor		bx, bx
.AdjustSkip:
	mov		[Header(RowSkip)], bx

	jmp		.Done

.ScreenClear:
	; check if window or full screen clear
	test		cx, cx
	jnz		.Done
	mov		cx, [DirectData.MaxXY]
	dec		cl
	dec		ch
	cmp		dl, cl
	jb		.Done
	cmp		dh, ch
	jb		.Done
	; is full screen clear
;	call		DirectSendFull
;	test		cx, cx
;	jz		.NoLinesSent
;	dec		cx
;.NoLinesSent:
;	mov		[Header(RowSkip)], cx
;	jmp		.Done
.ModeChange:
	call		DirectSendFull
	mov		[Header(RowSkip)], word 0; reset skip count
.Done:
	pop		cx
	pop		bx
	pop		ax
	pop		es
	ret


; -----------------------------------------------------------------------------

DirectSendFull:
	mov		cx, [es:0x0050]		; cursor position page 0
	; if cursor not column 0, inc total lines to send
	xchg		cl, ch
	test 		ch, ch
	jz		DirectSendLines
	inc		cl

DirectSendLines:
	xor		ch, ch
	push		cx
	xor		ax, ax
	mov		bx, [Header(RowSkip)]
	test		cx, cx
	jz		.Done			; line count 0, done
.Sending:
	cmp		bx, 0
	jg		.SkipSending
.SendNeeded:
	; if driver is not enabled, don't actually send to log?
	test		[Header(Status)], byte sfEnabled
	jz		.SkipSending
 	call		DirectSendRow
.SkipSending:
	dec		bx
	inc		ax
	loop		.Sending
	cmp		bx, 0
	jge		.SetSkip	; still more old Rows to skip
	xor 		bx, bx
.SetSkip:
	add		bx, ax
	mov		[Header(RowSkip)], bx
.Done:
	pop		cx
	ret

; -----------------------------------------------------------------------------

DirectFlush:
	call		SendToLog
	push		es
	push		ax
	push		bx
	push		cx
	mov		ax, 0x0040
	mov		es, ax
	call		DirectSendFull
	pop		cx
	pop		bx
	pop		ax
	pop		es
	ret

; -----------------------------------------------------------------------------

DirectSendRow:
	; ax=row (0+)
	push		ax
	push		bx
	mov		bl, [DirectData.MaxY]	; (1+)
	; xor		bh, bh
	; cmp		ax, bx
	cmp		al, bl	; rows count over 50 forbidden, so just check
				; lower byte.
	jae		.Skip	; -1 is 0x(ff)ff and ABOVE total screen rows
	; Row is 0 through (Rows - 1), we can send it
	push		cx
	push		dx
	push		si
	push		es

	mov		dx, [DirectData.VSeg]
	mov		es, dx

	; calculate offsets to start and end of line
	xor		ch, ch
	mov		cl, [DirectData.MaxX]	; cx=words per line (1+)
	mul		cx			; ax=words to start line offset
	mov		si, ax
	add		si, ax			; si=bytes to start line offset
	mov		dx, si			; dx=start line offset
	add		si, cx
	add 		si, cx
	dec		si
	dec		si			; si=offset to end of line
	mov		bx, 0x0720		; blank space
	std
%ifdef DIRECT_SEND
	%fatal 	Ability to send entire color line to log at once, not implemented.
%else
	test		[Header(Status)], byte sfInColor
	jnz		.ColorCount

.MonoCount:
	es lodsw
	cmp		al, bl
	jne		.Counted
	loop		.MonoCount
	jmp		.Counted
.ColorCount:
	es lodsw
	cmp		ax, bx
	jne		.Counted
	loop		.ColorCount
.Counted:
	cld
	test		cx, cx
	jz		.SendCRLF
	mov		si, dx
.Send:
	es lodsw
	mov		bl, ah
	call		AppendBuffer
	loop		.Send
%endif


.SendCRLF:
	mov		ax, 0x070d
	call		AppendBuffer
	mov		al, 0x0a
	call		AppendBuffer
	test		[Header(Status)], byte sfInColor
	jz		.Sent
	call		SendToLog

.Sent:
	pop		es
	pop		si
	pop		dx
	pop		cx

.Skip:
	pop		bx
	pop		ax
	ret


; -----------------------------------------------------------------------------

DirectData:
	.VSeg:		dw 0
	.MaxXY:
	.MaxX:		db 0
	.MaxY:		db 0

; -----------------------------------------------------------------------------
; Not needed if using XMS Memory. Only required for UMB and LOW memory LOG
; storage. Basically, the far call to the XMM driver is replaced with a call
; to this function. It is to just simplify reading and writing to LOW/UMB memory
; when the logger driver is configured to use one of those locations. Only,
; logger the driver and interface program will call this function. Therefore,
; No error or bounds checking is performed here. Many things in this function
; would be done differently if it were a "universal" memory drive interface.
;
; -----------------------------------------------------------------------------

Memory_Handler:
	; Since only function 0x0b will be called, don't bother testing for it
	; cmp		ah, 0x0b
	; je		.Transfer
	; xor		ax, ax
	; mov		bx, 0x80	; function not supported
	; retf
;.Transfer:

	pushf
	push		ds
	push		es
	push		si
	push		di
	push		cx
	push 		dx

	; DS:SI -> TTransfer record structure
	cmp		[si+TTransfer.SrcHandle], word 0
	jne		.SrcIsLog
.SrcIsMem:
	; could just discard upper word, but that would limit the LOW/UMB
	; buffer to under 64k. That would most likely be fine. But, it is
	; possible that for some strange and bizarre reason a larger LOG in
	; low memory could be needed. So we will just to convert it.
	; convert 32-bit offset to 20-bit address
	mov		ax, [si+TTransfer.DstAddr+2]
	mov		cl, 12
	shl		ax, cl
	mov		dx, ax
	mov		ax, [si+TTransfer.DstAddr]
	push		ax
	mov		cl, 4
	shr		ax, cl
	add		dx, ax
	pop		di ; was ax
	and		di, 0x000f
	add		dx, [si+TTransfer.DstHandle] ; contains buffer segment
	mov		es, dx
	; es:di -> destination LOG buffer
	mov		ax, [si] ; get byte count
	mov		cl, 1
	shr		ax, cl	; divide by 2 for word count
	mov		cx, ax
	; cx = word count to transfer
	lds		si, [si+TTransfer.SrcAddr]
	; ds:si = source buffer address

	cld
	rep		movsw
	jmp		.Done
.SrcIsLog:
	les		di, [si+TTransfer.DstAddr]
	; ds:si = destination buffer address
	; convert 32-bit offset to 20-bit address
	mov		ax, [si+TTransfer.SrcAddr+2]
	mov		cl, 12
	shl		ax, cl
	mov		dx, ax
	mov		ax, [si+TTransfer.SrcAddr]
	push		ax
	mov		cl, 4
	shr		ax, cl
	add		dx, ax
	pop		ax
	and		ax, 0x000f
	add		dx, [si+TTransfer.SrcHandle] ; contains buffer segment
	push		dx
	push		ax
	; source LOG buffer dx:ax on stack
	mov		ax, [si] ; get byte count
	mov		cl, 1
	shr		ax, cl	; divide by 2 for word count
	mov		cx, ax
	; cx = word count to transfer
	pop		si ; was ax
	pop		ds ; was dx
	; ds:si -> source LOG Buffer
	cld
	rep		movsw
	; jmp		.Done

.Done:
	pop		dx
	pop		cx
	pop		di
	pop		si
	pop		es
	pop		ds
	popf
	mov		ah, 0x01	; return ax != 0 for success
					; don't bother with error code in bx
	retf

; -----------------------------------------------------------------------------
; Not needed if keyboard snapshots are disabled by driver
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; Not needed after Initialization and always released.
; -----------------------------------------------------------------------------

Initialize:
	; set pointer to first byte above program for DOS to free after init
	mov		[es:di+tREQUEST.Address+2], cs
	mov		[es:di+tREQUEST.Address], word Initialize

	; es, di and ax were saved earlier by the driver function dispatcher
	; interrupt. Now save the remaining general registers that we will use.
	push		ds
	push		si
	push		dx
	push		cx
	push		bx

	push		cs
	pop		ds	; mov ds, cs

	PrintMessage 	Banner

	mov		[Header(Strategy)], word Driver.Strategy
	mov		[Header(Entry)], word Driver.Routine

	%ifdef SINGLE_BINARY
		mov		ax, -1
		mov		[Header(ChainNext)], ax
		mov		[Header(ChainNext)+2], ax
	%endif

	push		es
	push		di
	FindDeviceDriver
	pop		di
	pop		es
	jc		.NotLoaded
	PrintMessage	AlreadyRunning
	stc
	jmp		.Finished
.NotLoaded:

	mov		[Header(Flush)+2], cs; set far call segment for Flush
	push		es
	push		di

	; parse driver "command line" options
	les		di, [es:di+tREQUEST.CommandLine]
.SkipToParams:
	mov		al, [es:di]
	cmp		al, 0x20
	jbe		.AtParams
	inc		di
	jmp		.SkipToParams
.AtParams:
	ParseOptions	OptionTable, di

	; check if there were parameters, otherwise process defaults
	cmp		[HadOption], byte 0
	jne		.ParamsDone
	push		cs
	pop		es
	ParseOptions	OptionTable, DefaultOptions

	; done processing parameters
.ParamsDone:
	pop		di
	pop		es

	; If sfEnabled, memory was allocated, so activate driver
	test		[Header(Status)], byte sfEnabled
	jz		.Failed

	; perform Initial Capture Configuration
	call		ConfigCapture

	; if "XMS.Driver" segment is not our segment, we can dump the LOW/UMB
	; memory LOG handler and reduce the memory footprint by ~112 bytes.
	; this means DirectCapture will automatically be included regardless
	; wether or not it can be used.
	mov		dx, cs
	cmp		dx, [Header(XMS.Driver)+2]
	je		.KeepMemorySection
	mov		[es:di+tREQUEST.Address], word Memory_Handler

	; if Direct Capture is not supported, we don't need to keep it
	test		[Header(Status)], byte sfSupport
	jnz		.KeepMemorySection
	mov		[es:di+tREQUEST.Address], word ConfigCapture

.KeepMemorySection:

	; Save BIOS Int 10
	push		es
	mov		ax, 0x3510
	int		0x21
	mov		[DevInt10.NextHandler], bx
	mov		[DevInt10.NextHandler+2], es
	pop		es

	; Install Driver Interrupt 10 handler
	mov		dx, DevInt10
	mov		ax, 0x2510
	int		0x21

%ifdef HOOK_METHOD
	InstallHook
%endif

; .Success:

	PrintStatus	cs
	; PrintMessage	Activated
	%ifdef DEBUG
		PrintMessage 	LoadSeg
		WordAsHex 	cs
		PrintMessage	NewLine
	%endif
	clc
	jmp		.Finished

.Failed:
	PrintMessage	NotActivated
	stc

.Finished:
	; restore additional general registers used during initialization
	pop		bx
	pop		cx
	pop		dx
	pop		si
	pop		ds

	mov		ah, 0x01		; done bit, no error bit
	jnc		Initial.Return

	; Free entire device driver by setting pointer to driver start
	mov		[es:di+tREQUEST.Address], word DriverHeader
	jmp		Initial.Error

; -----------------------------------------------------------------------------
; The initial DOS function calls to interact with the driver, used only during
; initialization.
; -----------------------------------------------------------------------------

Initial.Strategy:				; set request block pointer
	mov		[cs:DriverRequest], bx
	mov		[cs:DriverRequest+2], es
	retf

Initial.Routine:					; driver function dispatcher
	pushf
	push		ax
	push		di
	push		es

	les		di, [cs:DriverRequest]
	mov		al, [es:di+tREQUEST.Function]
	test		al, al			; test command 0, initialize
	jz		Initialize

Initial.Error:
	mov		ax, 0x8103		; set error & done bits, and
						; error code 3, unknown command
Initial.Return:
	mov		[es:di+tREQUEST.Status], ax
	pop		es
	pop		di
	pop		ax
	popf
	retf

; -----------------------------------------------------------------------------

ConfigXMSDriver:
	; Test for XMS Memory
	mov		ax, 0x4300
	int		0x2f
	cmp		al, 0x80
	je		.XMSPresent
	mov		dx, NoXMSDriver
	stc
	ret

.XMSPresent:
	; Get Driver Address
	push		es
	mov		ax, 0x4310
	int		0x2f
	mov		[Header(XMS.Driver)], bx
	mov		[Header(XMS.Driver)+2], es
	pop		es
	clc
	ret

; -----------------------------------------------------------------------------

Option_XMS:
	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done

	call		ConfigXMSDriver
	jc		Option_Failed

	; Allocate XMS Memory
	mov		ah, 0x09
	mov		dx, [Header(XMS.Size)]		; Size in KB
	call		far [Header(XMS.Driver)]
	mov		[Header(XFR.DstHandle)], dx	; save XMS Handle
	test		ax, ax
	jnz		.XMSAllocated
	mov		dx, NoXMSAlloc
	jmp		Option_Failed

.XMSAllocated:
	; jmp		SuccessfulAlloc

; -----------------------------------------------------------------------------

SuccessfulAlloc:
	; set byte size of XMS memory allocated
	mov		ax, [Header(XMS.Size)]
	mov		dx, 1024
	mul		dx
	sub		ax, 0x0010
	sbb		dx, 0x0000
	mov		[Header(XMS.Max)], ax
	mov		[Header(XMS.Max)+2], dx
	or		[Header(Status)], byte sfEnabled ; Enable Driver
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_UMB:
	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done

; -----------------------------------------------------------------------------

UMB_Thru_DOS:
	mov		ax, 0x5800	; Get Alloc Strategy
	int		0x21
	jc		UMB_Thru_XMS
	push		ax		; save Strategy
	mov		ax, 0x5802	; Get UMB Link State
	int		0x21
	push		ax		; save Link State
	jnc		.LinkUMBs
.LinkFailed:
	pop		ax
	pop		ax
	jmp		UMB_Thru_XMS
.LinkUMBs:
	cmp		al, 0x01
	je		.SetStrategy
	ja		.LinkFailed
	mov		bx, 0x0001
	mov		ax, 0x5803
	int		0x21
	jc		.LinkFailed
.SetStrategy:
	mov		ax, 0x5801
	mov		bx, 0x0041	; best fit, high memory Only!
					; however, it may still allocate low
	int		0x21
	jc		.RestoreSettings

	inc		byte [LogInUMB]
	call		DOSAlloc
	test		[Header(Status)], byte sfEnabled
	jnz		.RestoreSettings
	dec		byte [LogInUMB]

.RestoreSettings:
	pop		bx	; was link state ax
	xor		bh, bh
	mov		ax, 0x5803
	int		0x21
	; jc 		Cannot Restore, this better not happen!
	pop		bx	; was strategy ax
	xor		bh, bh
	mov		ax, 0x5801
	int		0x21
	; jc 		Cannot Restore, this better not happen either!
	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done
;	jmp		Option_Done

; -----------------------------------------------------------------------------

UMB_Thru_XMS:
	; try to allocate UMB through XMS Driver for when DOS is not using UMB
	call		ConfigXMSDriver
	jc		Option_Failed
	mov		ax, [Header(XMS.Size)]	; Size in KB
	mov		bx, 0x0040
	mul		bx			; kb * 1024 / 16 = paragraphs
	xchg		ax, dx
	test		ax, ax
	jz		.TryUMB
	mov		dx, 0xffff
.TryUMB:
	mov		ah, 0x10
	xor		bx, bx
	call		far [Header(XMS.Driver)]
	test		ax, ax
	jnz		.UMBAllocated
	cmp		bl, 0xb0
	jne		.NotTooBig
	push		dx
	PrintMessage	MaxFreeUMB
	pop		ax
	xor		dx, dx
	mov		bx, 0x0040
	div		bx			; paragraphs * 16 / 1024 = kb
	WordAsUInt	ax
	PrintMessage	UMBKBytes
	jmp		.UMBFailed
.NotTooBig:
	cmp		bl, 0xb1
	jne		.UMBFailed
	; PrintMessage 	NoUMBAvail
	PrintMessage	NotEnoughUMB
	jmp		.UMBFailed
.UMBFailed:
	mov		dx, NoUMBAlloc
	jmp		Option_Failed

.UMBAllocated:
	; or		[Header(Status)], byte sfLogInUMB
	; bx = segment
	; dx = size, but don't care
	; jmp		SuccessfulAlloc
	jmp		Option_Done

; -----------------------------------------------------------------------------

DOSAlloc:
	mov		ax, [Header(XMS.Size)]	; Size in KB
	mov		bx, 0x0040
	mul		bx			; kb * 1024 / 16 = paragraphs
	test		dx, dx
	jnz		.AllocFail		; if dx <> 0, fail
	cmp		[LogInUMB], byte 0
	jne		.TryAlloc		; if requesting UMB, try
	%ifdef LOW_MEM_LOG
		mov		bx, cs
		cmp		bx, 0xa000
		jmp		.TryAlloc	; is driver is loaded high, try
	%endif

	jmp		.AllocFail
.TryAlloc:
	mov		bx, ax
	mov		ah, 0x48
	int		0x21			; ax=segment, if error bx=max
	jnc		.Allocated
.AllocFail:
	; CY can fe set or clear, sfEnabled is tested to check for success
	ret

.Allocated:
	cmp		[LogInUMB], byte 0
	je		.Success
	; when allocating Only a UMB, test if we got a upper memory block
	cmp		ax, 0xa000
	jae		.Success
	; not in upper memory, then free block
	push		es
	mov		es, ax
	mov		ah, 0x49
	int		0x21
	pop		es
	ret
.Success:
	; Switch to XMS like drive for LOW/UMB memory
	mov		[Header(XFR.DstHandle)], ax
	mov		[Header(XMS.Driver)], word Memory_Handler
	mov		[Header(XMS.Driver)+2], cs
	jmp		SuccessfulAlloc
; -----------------------------------------------------------------------------
%ifdef LOW_MEM_LOG

Option_LOW:
	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done
	call		DOSAlloc
	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done
	mov		dx, NotEnoughLOW
	jmp		Option_Failed

%endif
; -----------------------------------------------------------------------------

Option_JAM:
	or		[Header(Status)], byte sfLogJAM
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_COLOR:
	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done
	or		[Header(Status)], byte sfInColor
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_MONO:
	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done
	and		[Header(Status)], byte ~sfInColor ; not sfInColor
	jmp		Option_Done

; -----------------------------------------------------------------------------

Option_Unknown:
	OptionAsWord
	jc		Option_Bad
	test		ax, ax
	jz		Option_Bad

	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done

	mov		[Header(XMS.Size)], ax
	; fall through to Option_Done

; -----------------------------------------------------------------------------

Option_Done:
	mov		[HadOption], byte 1
	ret

; -----------------------------------------------------------------------------

Option_Bad:
	PrintMessage 	BadOptionPre
	PrintOptionText
	PrintMessage 	BadOptionPost
	; fall through to Option_Stop
	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done

; -----------------------------------------------------------------------------

Option_Stop:
	mov		dl, [es:di]
	cmp		dl, 0x0d
	jbe		Option_Done
	inc		di
	jmp		Option_Stop

; -----------------------------------------------------------------------------

Option_Failed:
	PrintMessage	dx
	jmp		Option_Done

; -----------------------------------------------------------------------------

OptionTable:
	dw		Option_XMS
	db		'XMS', 0
	dw		Option_UMB
	db		'UMB', 0
%ifdef LOW_MEM_LOG
	dw		Option_LOW
	db		'LOW', 0
%endif
	dw		Option_JAM
	db		'JAM', 0
	dw		Option_COLOR
	db		'COLOR',0
	dw		Option_MONO
	db		'MONO',0

	dw		Option_XMS
	db		'X', 0
	dw		Option_UMB
	db		'U', 0
%ifdef LOW_MEM_LOG
	dw		Option_LOW
	db		'L', 0
%endif
	dw		Option_JAM
	db		'J', 0
	dw		Option_COLOR
	db		'C',0
	dw		Option_MONO
	db		'M',0

	dw		0, Option_Unknown

; -----------------------------------------------------------------------------

CommonCode

; -----------------------------------------------------------------------------

%ifdef DEBUG
LoadSeg:
	db	'Driver loaded at segment: $'
%endif

Banner:
	db	0x0d,0x0a
	db	'Message Logger v',VERSION,' ' ; ,0x0d,0x0a
	CopyrightText
	db	'$'

BadOptionPre:
	db	'Invalid option "$'
BadOptionPost:
	db	'" provided.',0x0d,0x0a,'$'

NoXMSDriver:
	db	'XMS driver not installed.',0x0d,0x0a,'$'

NoXMSAlloc:
	db	'Unable to allocate XMS memory.',0x0d,0x0a,'$'
NoUMBAlloc:
	db	'Unable to allocate UMB memory.',0x0d,0x0a,'$'
MaxFreeUMB:
	db	'Largest free UMB is $'
UMBKBytes:
	db	'Kb.',0x0a,0x0d,'$'

NotEnoughUMB:
	db	'Insufficient UMB space available.',0x0a,0x0d,'$'
NotEnoughLOW:
	db	'Insufficient free LOW memory.',0x0a,0x0d,'$'


NotActivated:
	db	'Boot message logging not enabled.',0x0a,0x0d,'$'
;Activated:
;	db	'Boot message logging enabled.$'

AlreadyRunning:
	db	'Logger device driver is already loaded.' ; ,0x0a,0x0d,'$'

NewLine:
	db 	0x0d,0x0a,'$'

DefaultOptions:
%ifdef LOW_MEM_LOG
	db	'256 COLOR XMS 16 MONO UMB 16 MONO LOW',0
%else
	db	'256 COLOR XMS 16 MONO UMB',0
%endif

HadOption:
	db	0
LogInUMB:
	db 	0

%ifdef HOOK_AMIS
AMIS_FREE:
	db	0
%endif

DriverRequest:
	dd -1		; Pointer to tREQUEST block, for driver initialization.

%ifdef SINGLE_BINARY
SkipDriver:
%endif