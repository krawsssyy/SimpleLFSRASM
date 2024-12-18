.model tiny
; tap seq: x^31 + x^21 + x^11 + x^0
.code
org 100h
start:
	; init seed based on some time&date xors, as well with some xors from memory
	xor ax, ax
	mov ah, 2Ch
	int 21h
	xor cx, dx
	mov bx, cx
	mov ah, 2Ah
	int 21h
	xor cx, dx
	mov di, cx
	xor bx, word ptr cs:[80h]
	xor di, word ptr cs:[8Dh]
	; load seed
	xchg ax, bx
	xor bx, bx
	mov bx, offset register
	mov [bx], ax ;low part
	mov [bx + 2], di;high part
	; prepare loop to remove initial seed
	xor cx, cx
	mov cx, 32
	removeSeed:
		push offset register
		call processTap
		push ax
		push offset register
		call shiftAndAdd
	loop removeSeed

	;prepare actual loop
	xor cx, cx
	mov cx, 100
	generatorLoop:
		push offset register
		call processTap
		push offset register
		push ax
		call shiftAndAdd
		push ax
		call printBit
	loop generatorLoop


	mov ax, 4C00h
	int 21h

	processTap PROC NEAR
		push bp
		mov bp, sp
		push cx ; save due to being used as a counter
		xor bx, bx
		mov bx, [bp + 4] ; get arg from stack = register offset
		;low part
		xor ax, ax
		mov ax, [bx]
		;high part
		xor cx, cx
		mov cx, [bx + 2]
		;obtain bits necessary for the tap seq
		and ch, 80h; obtain 31st bit
		and cl, 20h ; obtain 21st bit
		and ah, 08h ; obtain 11th bit
		and al, 01h ; obtain 0th bit
		; get values of 0 and 1 for the bytes
		cmp ch, 80h
		je set_ch
		xor ch, ch
		jmp check_cl
		set_ch:
		mov ch, 01h

		check_cl:
		cmp cl, 20h
		je set_cl
		xor cl, cl
		jmp check_ah
		set_cl:
		mov cl, 01h

		check_ah:
		cmp ah, 08h
		je set_ah
		xor ah, ah
		jmp proc_tap
		set_ah:
		mov ah, 01h
		proc_tap:
		;process tap seq
		xor al, ah
		xor al, cl ; the returned bit is in al
		xor al, ch 
		xor ah, ah
		pop cx
		pop bp
		ret 2
	processTap endp

	shiftAndAdd PROC NEAR
		push bp
		mov bp, sp
		push cx ; save due to being used as a counter
		xor bx, bx
		mov bx, [bp+4] ; get first arg from stack = register offset
		;low part
		xor ax, ax
		mov ax, [bx]
		;high part
		xor cx, cx
		mov cx, [bx + 2]

		mov bx, ax
		mov dx, cx ; making copies of the values

		xor si, si
		mov si, [bp + 6] ; get second arg from stack = processed bit from tap seq

		shr ch, 1
		; check whether we leave ch as it is with 0 as MSB due to the shift or we need to add 1
		cmp si, 1
		je addMSB
		jmp doneMSB
		addMSB:
		or ch, 80h ; add 1 as MSB
		doneMSB: ; resume

		shr cl, 1
		and dh, 1
		ror dh, 1
		or cl, dh ; shift the second byte and add the last bit of the first byte as msb

		shr ah, 1
		and dl, 1
		ror dl, 1
		or ah, dl ; shift the third byte and add the last bit of the second byte as msb

		shr al, 1
		and bh, 1
		ror bh, 1
		or al, bh ; shift the last byte and add the last bit of the third byte as msb

		mov dl, bl ; save pseudo random generated bit
		and dl, 01h

		xor bx, bx
		mov bx, [bp + 4] ; obtain again the offset of the register

		mov [bx], ax
		mov [bx + 2], cx ; update register

		xor ax, ax
		mov al, dl ; make it return the bit in al
		pop cx
		pop bp
		ret 4
	shiftAndAdd endp

	printBit PROC NEAR
		push bp
		mov bp, sp
		xor dx, dx
		mov dx, [bp + 4]
		add dl, 30h
		xor ax, ax
		mov ah, 02h
		int 21h
		pop bp
		ret 2
	printBit endp

	register dd 0

end start

