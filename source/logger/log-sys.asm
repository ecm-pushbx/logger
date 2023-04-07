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
	at .ChainNext, 	dd -1			; pointer to next device driver
	at .Attributes,	dw 0x8000		; character device
	at .Strategy,	dw Driver.Strategy	; set tREQUEST block pointer
	at .Entry,	dw Driver.Routine	; driver interrupt call
	at .Name,	DeviceDriverID		; 8 character driver name

; -----------------------------------------------------------------------------
; In addition to the DeviceDriverID, the pointer is tested by LOG.COM to
; verify it is actually a running driver and not just as copy of the program
; residing in memory.

	at .Request,		dd -1		; Pointer to tREQUEST block

; -----------------------------------------------------------------------------
; Data that may be interacted with directly by the interface program
%ifdef	NO_SPLIT_SEND
	at .Format,		dw 0		; Record Format Identifier
%else
	at .Format,		dw 1		; Record Format Identifier
%endif

	at .Status,		dw 0		; Device driver Status

	at .Flush,		dd FlushBuffer	; Function to Flush the current
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

%ifdef	NO_SPLIT_SEND
	at .XMS.Top,		dd 0		; top unused buffer byte
%endif

iend
; -----------------------------------------------------------------------------

%define Header(x) DriverHeader + TDriverHeader. %+ x

; -----------------------------------------------------------------------------

BIOSInt10:		dd -1			; Original BIOS Int 10
LastCaptured:		dw -1			; Last Screen row sent to log

; -----------------------------------------------------------------------------
; The standard DOS function calls to interact with the driver
; -----------------------------------------------------------------------------

Driver.Strategy:				; set request block pointer
	mov		[cs:Header(Request)], bx
	mov		[cs:Header(Request)+2], es
	retf

Driver.Routine:					; driver function dispatcher
	pushf
	push		ax
	push		di
	push		es

	les		di, [cs:Header(Request)]
	mov		al, [es:di+tREQUEST.Function]
	test		al, al			; test command 0, initialize
	jz		Initialize

Driver.Error:
	mov		ax, 0x8103		; set error & done bits, and
						; error code 3, unknown command
Driver.Return:
	mov		[es:di+tREQUEST.Status], ax
	pop		es
	pop		di
	pop		ax
	popf
	retf

; -----------------------------------------------------------------------------
; Device Driver Interrupt 10 Handler
; -----------------------------------------------------------------------------

DevInt10:
	pushf
	cli
	push		ds
	push		cs
	pop		ds	; mov ds, cs

	; is driver enabled?
	test		[Header(Status)], byte sfEnabled
	jz		.CheckModeChange

	test		[Header(Status)], byte sfSupport
	jz		.CaptureTTL

	; was mode previously changed?
	test		[Header(Status)], byte sfModeChange
	jz		.NoModeResetFlag

	call		ConfigCapture

.NoModeResetFlag:
	; test capture method
	test		[Header(Status)], byte sfDirectMode ; direct method flag
	jz		.CaptureTTL

	call		DirectCapture
	jmp		.CheckModeChange

.CaptureTTL:
	; if not Direct Capture method, simple BIOS TTL capture
	cmp		ah, 0x0e
	jne		.CheckModeChange
	; bh = page, for now don't care
	call		AppendBuffer
	jmp		.Done

.CheckModeChange:
	test		ah, ah
	jnz		.Done
.AdjustModeChange:
	or		[Header(Status)], byte sfModeChange ; set flag
.Done:
	pop		ds
	popf
	jmp		far [cs:BIOSInt10]

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
	; cmp		al, 0x0a		; also send on LF
	; jne		.AdjustNone
.SendTTLBuffer:
	call		SendToLog
	ret

; -----------------------------------------------------------------------------
; FAR function for external Log interface program to call that insures all data
; stored in the buffer has been written to the log.
; -----------------------------------------------------------------------------

FlushBuffer:
	; far call to send buffer contents to Log storage. It is not effected
	; by driver enabled state.
	pushf
	push		ds
	push		cs
	pop		ds
	call		SendToLog
	pop		ds
	popf
	retf

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
	; it is initialized) and if the Log is now Full (always once wraps)
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

%ifdef NO_SPLIT_SEND
	; move TOP pointer to current HEAD dx:ax
	mov		[Header(XMS.Top)], ax
	mov		[Header(XMS.Top)+2], dx

	; move HEAD to buffer origin
	xor		ax, ax
	xor		dx, dx

%else
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
%endif

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
	; we just buffer wrapped, adjust to TOP old HEAD
%ifdef NO_SPLIT_SEND
	push		dx
	call		.BufferHead
	mov		[Header(XMS.Top)], ax
	mov		[Header(XMS.Top)+2], dx
	pop		dx
%else
	; with split buffer, no TOP and MAX never changes
%endif
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

%ifdef NO_SPLIT_SEND
	; if write END > TOP, adjust TOP to write END
	cmpdd		bx, cx, Header(XMS.Top)
	jb		.NoAdjustTop
	mov		[Header(XMS.Top)], cx
	mov		[Header(XMS.Top)+2], bx
.NoAdjustTop:
%else
	; with split buffer, end cannot be past MAX
%endif

	; update total byte count written to buffer
	mov		ax, [si]
	add		[Header(XMS.Count)], ax
	adc		[Header(XMS.Count)+2], word 0
	jnc		.NoCountOverflow
	mov		[Header(XMS.Count)], word 1 ; so it ain't ever 0
.NoCountOverflow:
	ret

; -----------------------------------------------------------------------------
; If direct video mode not supported, this won't be needed after initialization
; -----------------------------------------------------------------------------

ConfigCapture:

	; mode was changed, adjust settings
	push		es
	push		ax
	; clear ModeChange and DirectMethod bits
	and		[Header(Status)], byte ~(sfModeChange + sfDirectMode)

	; mov		[Header(XFR.Count)], word 0 ; don't worry about high word
	mov		[Header(XFR.SrcAddr)], word Header(XFR.Buffer)
	mov		[Header(XFR.SrcAddr)+2], cs

	; in a supported text mode?
	mov		ax, 0x0040
	mov		es, ax
	mov		al, [es:0x0049]		; current video mode
	and		al, 0x7f
	cmp		al, 0x07		; is 80x25 mono?
	je		.SetDirectMethod
	cmp		al, 0x03		; not 80x? or 40x?
	ja		.SetMethodDone
.SetDirectMethod:
	mov		[LastCaptured], word -1 ; set last captured line
	; or		[Header(Status)], sfDirectMode	; set direct method flag
.SetMethodDone:
	pop		ax
	pop		es
	ret

DirectCapture:
	ret

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
	mov		[BIOSInt10], bx
	mov		[BIOSInt10+2], es
	pop		es

	; Install Driver Interrupt 10 handler
	mov		dx, DevInt10
	mov		ax, 0x2510
	int		0x21

; .Success:

	PrintStatus	cs
	PrintMessage	Activated
	%ifdef DEBUG
		PrintMessage	NewLine
		PrintMessage 	LoadSeg
		WordAsHex 	cs
	%endif
	clc
	jmp		.Finished

.Failed:
	PrintMessage	NotActivated
	stc

.Finished:
	pushf
	PrintMessage	NewLine
	int		0x21		; repeat Newline message
	popf

	; restore additional general registers used during initialization
	pop		bx
	pop		cx
	pop		dx
	pop		si
	pop		ds

	mov		ah, 0x01		; done bit, no error bit
	jnc		Driver.Return

	; Free entire device driver by setting pointer to driver start
	mov		[es:di+tREQUEST.Address], word DriverHeader
	jmp		Driver.Error

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
	WordAsInt	ax
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
	mov		bx, ax
	test		dx, dx
	jz		.TryAlloc
	mov		bx, 0xffff
.TryAlloc:
	mov		ah, 0x48
	int		0x21			; ax=segment, if error bx=max
	jnc		.Allocated
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

Option_LOW:
	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done
	call		DOSAlloc
	test		[Header(Status)], byte sfEnabled
	jnz		Option_Done
	mov		dx, NotEnoughLOW
	jmp		Option_Failed

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
	PrintMessage 	BadOptionPre
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
	dw		Option_LOW
	db		'LOW', 0
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
	dw		Option_LOW
	db		'L', 0
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
	db	'System Boot Message Logger, v0.1',0x0d,0x0a
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
	db	'Boot message logging not enabled.$'
Activated:
	db	'Boot message logging enabled.$'

AlreadyRunning:
	db	'Logger device drive is already loaded.'

NewLine:
	db 	0x0d,0x0a,'$'

DefaultOptions:
	db	'256 COLOR XMS 16 MONO UMB 16 MONO LOW',0

HadOption:
	db	0
LogInUMB:
	db 	0
