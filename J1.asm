[org 0x100]
jmp start
		print_2_ch:
                   push ax
		           push cx
		           push di
                call commonthing
				mov di,1482
				mov word[es:di],ax
				mov di,2290
				mov word[es:di],ax
				mov di,2132
				mov word[es:di],ax
                mov cx,4
                mov di,1494
            l1fore:
                mov word[es:di],ax
                add di,160
                sub cx,1
                jnz l1fore
                mov di,2454
                mov cx,2

            l2fore:
                mov word[es:di],ax
                add di,4
                sub cx,1
                jnz l2fore
                mov di,1324
                mov cx,3
            l4fore:
                mov word[es:di],ax
                add di,4
                sub cx,1
                jnz l4fore
                mov di,2446
                mov cx,2
            l5fore:
                mov word[es:di],ax
                add di,4
                sub cx,1
                jnz l5fore
				pop di
		        pop cx
		        pop ax
                ret
print_2n_ch:
                   push ax
		           push cx
		           push di
                   call commonthing
                   mov cx,6
                   mov di,1326

            l1fori:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l1fori
                    pop di
		            pop cx
		            pop ax
                    ret
		print_6t_ch:
                   push ax
		           push cx
		           push di
                call commonthing
                mov cx,5
                mov di,1494
            l1for:
                mov word[es:di],ax
                add di,160
                sub cx,1
                jnz l1for
                mov di,1806
                mov cx,2

            l2for:
                mov word[es:di],ax
                add di,4
                sub cx,1
                jnz l2for
                mov di,1324
                mov cx,3
            l4for:
                mov word[es:di],ax
                add di,4
                sub cx,1
                jnz l4for
                mov di,2284
                mov cx,3
            l5for:
                mov word[es:di],ax
                add di,4
                sub cx,1
                jnz l5for
				pop di
		        pop cx
		        pop ax
                ret
		p_ist_character:
                    push ax
		            push cx
		            push di
                    call commonthing
                    mov cx,4
                    mov di,1514
			    l1fgo:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l1fgo
                    mov di,1356
                    mov cx,3
                l2fgo:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l2fgo
                    mov di,1526
                    mov cx,4
                l3fgo:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l3fgo
                    mov di,2156
                    mov cx,3

                l4fgo:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l4fgo
					pop di
		            pop cx
		            pop ax
                    ret
p_2nd_character:
                   push ax
		           push cx
		           push di
				   call commonthing
                   mov cx,3
                   mov di,1492
            l1forgo:
                   mov word[es:di],ax
                   add di,160
                   sub cx,1
                   jnz l1forgo
                   mov di,1336
                   mov cx,3
            l2forgo:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l2forgo
                    mov di,1976
                    mov cx,3
            l3forgo:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l3forgo
                    mov di,1656
                    mov cx,3
            l4forgo:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l4forgo
                    mov cx,3
                    mov di,1828
             l5forgo:
                     mov word[es:di],ax
                     add di,160
                     sub cx,1
                     jnz l5forgo
                     pop di
		             pop cx
		             pop ax
                     ret
no_change:
ret
soundSystem:
	push ax
	push bx
	mov al,182
	out 43h,al
	mov ax, 4560
	out 42h,al
	mov al,ah
	out 42h,al
	in al,61h
	or al,00000011
	out 61h,al
	mov bx,2
	pre:
	mov cx, dx
	pre1:
	dec cx
	jne pre1
	dec bx
	jne pre
	in al,61h
	and AL,11111100b
	out 61h,al 
	pop bx
	pop ax
	ret
commonthing:
            cld
        mov ax,0xb800
        mov es,ax
        mov ah,0x40
        mov al,0xDC;
		ret
                        ;-----------DISPLAY_OF_ROAD_NAME---------------
print_First_character:
        push ax
		push cx
		push di
       call commonthing
        mov cx,6
        mov di,180

loop1:
        mov word[es:di],ax
        add di,160
        sub cx,1
        jnz loop1
        mov di,184
        mov cx,3
loop2:
        mov word[es:di],ax
        add di,4
        sub cx,1
        jnz loop2
        mov di,352
        mov word[es:di],ax
        mov di,504
        mov cx,3
loop4:
        mov word[es:di],ax

        add di,4
        sub cx,1
        jnz loop4
        mov di,664
        mov word[es:di],ax
        mov di,828
        mov word[es:di],ax
        mov di,992
        mov word[es:di],ax
		pop di
		pop cx
		pop ax
        ret
print_2nd_character:
                    push ax
		            push cx
		            push di
                    call commonthing
                    mov cx,4
                    mov di,358
			    l1:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l1
                    mov di,202
                    mov cx,3
                l2:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l2
                    mov di,374
                    mov cx,4
                l3:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l3
                    mov di,1002
                    mov cx,3

                l4:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l4
					pop di
		            pop cx
		            pop ax
                    ret
print_3rd_character:
                    push ax
		            push cx
		            push di
                    call commonthing
                    mov cx,5
                    mov di,380
                l1for3:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l1for3
                    mov di,224
                    mov cx,3
                l2for3:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l2for3
                    mov cx,5
                    mov di,396
                l3for3:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l3for3
                    mov di,544
                    mov cx,3
                l4for3:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l4for3
					pop di
		            pop cx
		            pop ax
                    ret
print_4th_character:
                    push ax
		            push cx
		            push di
                    call commonthing
                    mov cx,6
                    mov di,242
            l1for4:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l1for4
                    mov di,246
                    mov cx,3
            l2for4:
                    mov word[es:di],ax

                    add di,4
                    sub cx,1
                    jnz l2for4
                    mov di,416
                    mov cx,4
            l3for4:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l3for4
                    mov di,1046
                    mov cx,3

            l4for4:
                   mov word[es:di],ax
                   add di,4
                   sub cx,1
                   jnz l4for4
				   pop di
		           pop cx
		           pop ax
                   ret;------------DISPLAY_OF_FIGHTER_NAME------------
print_First_ch:
                   push ax
		           push cx
		           push di
                   call commonthing
                   mov cx,6
                   mov di,1308
            l1forf:
                   mov word[es:di],ax
                   add di,160
                   sub cx,1
                   jnz l1forf
                   mov di,1312
                   mov cx,3
            l2forf:
                   mov word[es:di],ax
                   add di,4
                   sub cx,1
                   jnz l2forf
                   mov di,1792
                   mov cx,3
            l4forf:
                   mov word[es:di],ax
                   add di,4
                   sub cx,1
                   jnz l4forf
				   pop di
		           pop cx
		           pop ax
                   ret
print_2nd_ch:
                   push ax
		           push cx
		           push di
                   call commonthing
                   mov cx,6
                   mov di,1326

            l1forii:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l1forii
                    pop di
		            pop cx
		            pop ax
                    ret
print_3rd_ch:
                   push ax
		           push cx
		           push di
				   call commonthing
                   mov cx,3
                   mov di,1492
            l1forg:
                   mov word[es:di],ax
                   add di,160
                   sub cx,1
                   jnz l1forg
                   mov di,1336
                   mov cx,3
            l2forg:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l2forg
                    mov di,1976
                    mov cx,3
            l3forg:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l3forg
                    mov di,1656
                    mov cx,3
            l4forg:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l4forg
                    mov cx,3
                    mov di,1828
             l5forg:
                     mov word[es:di],ax
                     add di,160
                     sub cx,1
                     jnz l5forg
                     pop di
		             pop cx
		             pop ax
                     ret
print_4th_ch:
                   push ax
		           push cx
		           push di
                     call commonthing
                     mov cx,6
                     mov di,1354
                l1forh:
                     mov word[es:di],ax
                     add di,160
                     sub cx,1
                     jnz l1forh
                     mov cx,6
                     mov di,1370
                l2forh:
                     mov word[es:di],ax
                     add di,160
                     sub cx,1
                     jnz l2forh
                     mov di,1678
                     mov cx,3
                l3forh:
                     mov word[es:di],ax
                     add di,4
                     sub cx,1
                     jnz l3forh
					 pop di
		             pop cx
		             pop ax
                     ret
print_5th_ch:
                   push ax
		           push cx
		           push di
                    call commonthing
                    mov di,1376
                    mov cx,5
                l1fort:
                    mov word[es:di],ax
                    add di,4
                    sub cx,1
                    jnz l1fort
                    mov cx,5
                    mov di,1544
                l2fort:
                    mov word[es:di],ax
                    add di,160
                    sub cx,1
                    jnz l2fort
                    mov di,1678
                    mov cx,3
					 pop di
		             pop cx
		             pop ax
                    ret
print_6th_ch:
                   push ax
		           push cx
		           push di
                call commonthing
                mov cx,6
                mov di,1398
            l1foree:
                mov word[es:di],ax
                add di,160
                sub cx,1
                jnz l1foree
                mov di,1402
                mov cx,3

            l2foree:
                mov word[es:di],ax
                add di,4
                sub cx,1
                jnz l2foree
                mov di,1722
                mov cx,3
            l4foree:
                mov word[es:di],ax
                add di,4
                sub cx,1
                jnz l4foree
                mov di,2202
                mov cx,3
            l5foree:
                mov word[es:di],ax
                add di,4
                sub cx,1
                jnz l5foree
				pop di
		        pop cx
		        pop ax
                ret
print_7th_ch:
                   push ax
		           push cx
		           push di
                call commonthing
                mov cx,6
                mov di,1416
            l1forr:
                mov word[es:di],ax
                add di,160
                sub cx,1
                jnz l1forr
                mov di,1420
                mov cx,3
            l2forr:
                mov word[es:di],ax
                add di,4
                sub cx,1
                jnz l2forr
                mov di,1740
                mov cx,3
            l4forr:
                mov word[es:di],ax

                add di,4
                sub cx,1
                jnz l4forr
                mov di,1588
                mov word[es:di],ax

                mov di,1900
                mov word[es:di],ax
                mov di,2064
                mov word[es:di],ax
                mov di,2228
                mov word[es:di],ax
			    pop di
		        pop cx
		        pop ax
                ret

printGameName:
               call print_First_character	
               call print_2nd_character
               call print_3rd_character
               call print_4th_character
               call print_First_ch
               call print_2nd_ch
               call print_3rd_ch
               call print_4th_ch
               call print_5th_ch
               call print_6th_ch
               call print_7th_ch
               ret
strlen: ;return length in ax 
            push bp
            mov bp,sp
            push es
            push cx
            push di

            les di, [bp+4] 
            mov cx, 0xffff 
            mov al, 0 
            repne scasb 
            mov ax, 0xffff 
            sub ax, cx 
            dec ax 

            pop di
            pop cx
            pop es
            pop bp
            ret 4 

printstr: 
         push bp
         mov bp, sp
         push es
         push ax
         push cx
         push si
         push di
         push ds ; push segment of string
         mov ax, [bp+4]
         push ax ; push offset of string
         call strlen ; calculate string length 
         cmp ax, 0 ; is the string empty
         jz exit ; no printing if string is empty
         mov cx, ax ; save length in cx
         mov ax, 0xb800
         mov es, ax ; point es to video base
         mov al, 80 ; load al with columns per row
         mul byte [bp+8] ; multiply with y position
         add ax, [bp+10] ; add x position
         shl ax, 1 ; turn into byte offset
         mov di,ax ; point di to required location
         mov si, [bp+4] ; point si to string
         mov ah, [bp+6] ; load attribute in ah
         cmp ah,0x7f
         je style  
        cld ; auto increment mode
nextchar: lodsb ; load next char in al
         stosw 
        loop nextchar
        jmp exit
 style:
        cld ; auto increment mode
nextc: lodsb ; load next char in al
       stosw 
       call delay
       call delay
       call delay
       loop nextc
exit: 
        pop di
        pop si
        pop cx
        pop ax
        pop es
        pop bp
        ret 8
 
printnum: ;two inputs bp+6(1st input) is position where to print bp+4 is number 
        push bp
        mov bp, sp
        push es
        push ax
        push bx
        push cx
        push dx
        push di
        mov ax, 0xb800
        mov es, ax ; point es to video base
        mov ax, [bp+4] ; load number in ax
        mov bx, 10 ; use base 10 for division
        mov cx, 0 ; initialize count of digits
nextdigit: 
        mov dx, 0 ; zero upper half of dividend
        div bx ; divide by 10
        add dl, 0x30 ; convert digit into ascii value
        push dx ; save ascii value on stack
        inc cx ; increment count of values
        cmp ax, 0 ; is the quotient zero
        jnz nextdigit ; if no divide it again
        mov di,[bp+6]
 ;mov di,6*80
 ;add di, 65 ; point di to 65th column
 ;shl word di,1
nextpos: 
        pop dx ; remove a digit from the stack
        mov dh, 0x07 ; use normal attribute
        mov [es:di], dx ; print char on screen
        add di, 2 ; move to next screen location
        loop nextpos ; repeat for all digits on stack
        pop di
        pop dx
        pop cx
        pop bx
        pop ax 
        pop es
        pop bp
        ret 4
; timer interrupt service routine
timer: 
        push ax
        push dx
 ;cmp word [endGame],1
 ;je beep
 ;mov dx,400
 ;jmp no_beep
 ;beep:
 ;mov dx,65000
 ;no_beep:
 ;call soundSystem
        cmp word[pauseGame],1
        je ter
        add word [counter],1
        mov dx,[speedOfCars]
        cmp word [counter],dx
        jl ter
        inc word [cs:tickcount]; increment tick count
        cmp word [cs:tickcount],15
        jne go_on
        mov word [curveV],1
 go_on:
         mov word [curveV],0
        push word 0
        push word 6
        push word 65 
        call index_finder
        push word [cs:tickcount]
        call printnum ; print tick count
 ;cmp word [counter],30
 ;jne ter
        mov word [counter],0
 ter:
        mov al, 0x20
        out 0x20, al ; end of interrupt
        pop dx
        pop ax
        iret ; return from interrupt
scorecard:
       push es
       push ax
       push cx
       push di
       pushf
       mov ax,0xb800
       mov es,ax
       push word 64
       push word 3
       push word 0x07
       push string17
       call printstr
       call printGameSpeed

       mov di,5*80;5th row
       add di,65;65th column
       shl word di,1
       mov word [es:di],0x0753
       add di,2
       mov word [es:di],0x0743
       add di,2
       mov word [es:di],0x074f
       add di,2
       mov word [es:di],0x0752
       add di,2
       mov word [es:di],0x0745
       add di,2
       mov word [es:di],0x073a
       sub di,10
       add di,960
       mov word [es:di],0x0753
       add di,2
       mov word [es:di],0x0750
       add di,2
       mov word [es:di],0x0745
       add di,2
       mov word [es:di],0x0745
       add di,2
       mov word [es:di],0x0744
       add di,2
       mov word [es:di],0x073a
       sub di,10
       add di,960
       mov word [es:di],0x0746
       add di,2
       mov word [es:di],0x0755
       add di,2
       mov word [es:di],0x0745
       add di,2
       mov word [es:di],0x074c
       add di,2
       mov word [es:di],0x073a
       popf
       pop di
       pop cx
       pop ax
       pop es
flag:
       push es
       push ax
       push cx
       push di
       pushf
       cld
       mov ax,0xb800
       mov es,ax
       mov di,482
       mov ax,0x8fb3
       stosw
       mov ax,0x895c;0x805f
       mov cx,1;2
       rep stosw
       add di,156
       mov ax,0x8fb3
       stosw
       mov ax,0x892f;0x80c4
       mov cx,1
       rep stosw
       mov ax,0x89a7;0x80c4
       mov cx,1
       rep stosw
       add di,154
       mov ax,0x8fb3
       stosw
       popf
       pop di
       pop cx
       pop ax
       pop es
ret
smallCar:
       push es
       push ax
       push di
       pushf
       mov ax,0xb800
       mov es,ax
       mov ah,0x8e
       mov al,216;character
;body
       mov di,3524
       mov word[es:di],ax
       mov di,3526
       mov word[es:di],ax
       mov di,3684
       mov word[es:di],ax
       mov di,3686
       mov word[es:di],ax
;tyres
       mov ah,0x80
       mov al,233;character
       mov di,3522
       mov word[es:di],ax
       mov di,3528
       mov word[es:di],ax
       mov di,3682
       mov word[es:di],ax
              mov word[es:di],ax
       popf
       pop di
       pop ax
pop es
       ret
line_maker:
       push bp
       mov bp,sp
       push ax
       push cx
       mov ax,0720h
       mov cx,1
       rep stosw
       mov ax,8f20h
       mov cx,4
       rep stosw
       mov ax,0720h
       mov cx,1
       rep stosw
       pop cx
       pop ax
       pop bp
ret 2
P_scene_maker:
       push bp
       mov bp,sp
       push ax
       push cx
       mov ax,0x2020
       mov cx,3
       rep stosw
       mov ax,0x6020
       mov cx,13
       rep stosw
       mov ax,0xe020
       mov cx,8
       rep stosw
       pop cx
       pop ax
       pop bp
ret 2
scene_maker:
       push bp
       mov bp,sp
       push ax
       push cx
       mov ax,0xe020
       mov cx,8
       rep stosw
       mov cx,di
       cmp cx,42
       jl changer 
       mov ax,0x6020
       mov cx,13
       jmp simple
changer:
       mov ax,42
       sub ax,cx
       shr ax,1
       mov cx,13
       sub cx,ax
       mov ax,0x6020
simple:
       rep stosw
       mov ax,0x2020
       mov cx,3
       rep stosw
next1:
       pop cx
       pop ax
       pop bp
ret 2
road_maker:
       push bp
       mov bp,sp
       push cx
       push ax
       mov ax,6f08h
       stosw
       mov ax,8f20h
       mov cx,8
       rep stosw
       mov ax,[bp+4]
       cmp ax,0
       je skipp
       mov ax,0fdeh
       stosw
       mov ax,8f20h
       mov cx,6
       rep stosw
       mov ax,0fdeh
       stosw
       jmp no_skip
skipp:
       mov ax,8f20h
       mov cx,8
       rep stosw
no_skip:
       mov ax,8f20h
       mov cx,8
       rep stosw
       mov ax,6f08h
       stosw
       pop ax
       pop cx
       pop bp
ret 2
rock_maker:
       push bp
       mov bp,sp
       push ax
       push cx
       mov ax,[bp+4]
       cmp ax,0
       je no_rock
       cmp ax,2
       je rock
       jmp rock1
no_rock:
       mov ax,6020h
       mov cx,5
       rep stosw
       jmp ender
rock:
       mov ax,6020h
       mov cx,1
       rep stosw
       mov ax,6791h
       stosw
       mov ax,6020h
       mov cx,3
       rep stosw
       jmp ender
rock1:
       mov ax,6020h
       mov cx,2
       rep stosw
       mov ax,6791h
       stosw
       mov ax,6020h
       mov cx,2
       rep stosw
ender:
       pop cx
       pop ax
       pop bp
ret 2
check_even:
       push bp
       mov bp,sp
       push ax
       push bx
       mov bx,2
       mov ax,[bp+4]
       div bl
       cmp ah,0
       je zero
       mov word [bp+6],1;if number is even
       jmp no_thing
	   
zero:
       mov word [bp+6],0; if number is odd
no_thing:
       pop bx
       pop ax
       pop bp
ret 2
clr_scr:
       push ax
       push di
       push es
       pushf
       cld
       mov ax,0xb800
       mov es,ax
       mov ax,0720h
       mov di,0
loopcs:
		mov [es:di],ax
		add di,2
		cmp di,4000
		jne loopcs
		popf
		pop es
		pop di
		pop ax
		ret
		delay:
		push cx
		mov cx, 4 
		delay_loop1:
		push cx
		mov cx, 0xFFFF
		delay_loop2:
		loop delay_loop2
		pop cx
		loop delay_loop1
		pop cx
		ret
compare:
		push bp
		mov bp,sp
		push ax
		push bx
		mov ax,[bp+4]
		mov bx,7
		div bl
		cmp ah,0
		je no_one
		cmp ah,1
		je no_one
		cmp ah,2
		je no_one
		cmp ah,3
		je no_one
		cmp ah,4
		je no_one
		cmp ah,5
		je _two
		mov word [bp+6],1
		jmp not_hing
		no_one:
		mov word [bp+6],0
		jmp not_hing
		_two:
		mov word [bp+6],2
		not_hing:
		pop bx
		pop ax
		pop bp
ret 2
index_finder:;(row,col)
		push bp
		mov bp,sp
		push ax
		push dx
		mov dx,0
		mov ax,80
		mul word [bp+6]
		add ax,[bp+4]
		shl ax,1
		mov [bp+8],ax
		pop dx
		pop ax
		pop bp
ret 4
shift:;shifts the rows downwards within given indexes(r,c),(r2,c2)
		push bp
		mov bp,sp
		push cx
		push ds
		push es
		push ax
		push si
		push di
		pushf

		mov ax,0xb800
		mov es,ax
		mov ds,ax
		std
		push 0
		push word [bp+6]
		push word [bp+4]
		call index_finder
		pop di
		mov si,di
		sub si,160
		mov ax,160
		mov ax,[bp+6]
		sub ax,[bp+10]
		;add ax,1
		looop:
		push si
		push di
		mov cx,[bp+4]
		sub cx,[bp+8]
		rep movsw
		pop di
		pop si
		sub si,160
		sub di,160
		sub ax,1
		cmp ax,0
		jne looop

		popf
		pop di
		pop si
		pop ax
		pop es
		pop ds
		pop cx
		pop bp
ret 8
printBackground:
			push ax
			push es
			push di
			push cx
			push dx
			pushf
			mov ax,0xb800
			mov es,ax
			mov di,0
			mov dx,0
			cld
my_loop:
			push 0
			push dx
			call compare
			pop ax
			push ax
			call line_maker
			push ax
			call P_scene_maker
			push ax
			call road_maker
			push ax
			call rock_maker
			mov ax,0720h
			mov cx,19
			rep stosw
			add dx,1
			cmp di,4000
			jne my_loop
			call smallCar
			call flag
			call scorecard
			popf
			pop dx
			pop cx
			pop di
			pop es
			pop ax
ret
shiftExceptPlayerCar:
		push ax

		push 0
		call frontCollisionCheck
		pop word [endGame]
		cmp word [endGame],1
		je skipThis
		push word 20
		push word 5

		push word 24
		mov ax,[topLeft+2]
		sub ax,2
		push ax;one index before column number
		call shift

		push 20
		mov ax,[bottomRight+2]
		sub ax,2
		push ax;one index before column number

		push 24
		push 61
		call shift

		;topleft corner in input first
		push word 0;0th row
		push word 5;5th col
		;bottomRight corner
		push word 20;20th row
		push word 61;61st column
		call shift
		;push 0
		;push word[topLeft]
		;push word[topLeft+2]
		;call index_finder
		;pop di
		;sub di,2
		;mov si,di
		;sub di,160
		;movsw
		;add si,10
		;add di,10
		;movsw
		skipThis:
		pop ax
ret

moveBackground:
		push bp
		mov bp,sp
		push ax
		push es
		push di
		pushf
		cmp word [endGame],1
		je doNothing
		mov ax,0xb800
		mov es,ax
		std
		call shiftExceptPlayerCar
		push 0
		push word [bp+4]
		call compare
		pop ax
		mov di,[bp+6]
		push ax
		call rock_maker
		push ax
		call road_maker
		push ax
		call scene_maker

doNothing:
		popf
		pop di
		pop es
		pop ax
		pop bp
ret 4

PlayersCar:

			push ax
			push es
			push cx
			push di
			pushf
			mov ax,0xb800
			mov es,ax
			cld
			push 0
			push word [topLeft]
			push word [topLeft+2]
			call index_finder
			pop di
			;5F;cd for back
			;a7 for front
			;d7 for body
			;e9 dor tyres
			;0c for lights
			mov ax,8e0ch
			stosw
			add di,2
			stosw

			add di,152
			mov ax,80e9h
			stosw
			mov ax,0x82d7;b
			mov cx,3
			rep stosw
			mov ax,80e9h
			stosw

			add di,150
			mov ax,80e9h
			stosw
			mov ax,0x82d7;b
			mov cx,3
			rep stosw
			mov ax,80e9h
			stosw


			add di,152
			mov ax,8fcdh
			mov cx,3
			rep stosw

			popf
			pop di
			pop cx
			pop es
			pop ax
ret 

makeObject:
		push bp
		mov bp,sp
		push ax
		push es
		push cx
		push di
		pushf
		cmp word [endGame],1
		je dontMake
		mov ax,0xb800
		mov es,ax
		cld
		mov di,[leftSideOfRoad];82;62
		shl word di,1
		add di,1
		add di,[bp+4]
		push 0
		push di
		call check_even
		pop ax;if di is not even make it even
		add di,ax
		;5F for back
		;a7 for front
		;d7 for body
		;e9 dor tyres
		;a7 for lights

		mov ax,8f5fh
		mov cx,3
		rep stosw

		add di,152
		mov ax,80e9h
		stosw
		mov ax,84d7h
		mov cx,3
		rep stosw
		mov ax,80e9h
		stosw

		add di,150
		mov ax,80e9h
		stosw
		mov ax,84d7h
		mov cx,3
		rep stosw
		mov ax,80e9h
		stosw

		add di,152
		mov ax,8ea7h
		stosw
		add di,2
		mov ax,8ea7h
		stosw
		dontMake:
		popf
		pop di
		pop cx
		pop es
		pop ax
		pop bp
ret 2
; a function to generate random number in range [0-n]
; syntax to use
; push 0; make space for o/p
; push n ; that max of range
; call RANDNUM
RANDNUM2:
		push bp
		mov bp,sp
		push ax
		push cx
		push dx
		push bx
		MOV AH, 00h ; interrupts to get system time
		INT 1AH ; CX:DX now hold number of clock ticks since midnight
		mov ax, dx
		mov bx, 25173
		mul bx
		add ax, 13849
		xor dx, dx
		mov cx, [bp+4]
		shr ax,5
		inc cx
		div cx
		mov [bp+6], dx
		pop bx
		pop dx
		pop cx
		pop ax
		pop bp
ret 2
; it used seed variable
; you can change the values for same seed same random numbers will be generated
RANDNUM:
		push bp
		mov bp, sp
		push bx
		push dx
		push ax
		mov ax, 25173
		mul word [seed]
		add ax, 13849
		mov [seed], ax ; save the seed for the next call
		ror ax,8
		mov bx,[bp+4] ; maximum value
		inc bx
		mov dx,0
		div bx ; divide by max value
		mov [bp+6],dx ; return the remainder
		pop ax
		pop dx
		pop bx
		pop bp
		ret 2

		clear:
		push ax
		push es
		push di
		push cx
		pushf

		cld
		mov ax,0xb800
		mov es,ax
		push 0
		push word [topLeft]
		push word [topLeft+2]
		call index_finder
		pop di

		mov ax,8f20h
		stosw
		add di,2
		stosw

		add di,152
		mov ax,8f20h
		stosw
		mov ax,8f20h
		mov cx,3
		rep stosw
		mov ax,8f20h
		stosw

		add di,150
		mov ax,8f20h
		stosw
		mov ax,8f20h
		mov cx,3
		rep stosw
		mov ax,8f20h
		stosw


		add di,152
		mov ax,8f20h
		mov cx,3
		rep stosw

		popf
		pop cx
		pop di
		pop es
		pop ax
ret
leftCollisionCheck:
			push bp
			mov bp,sp
			push ax
			push es
			push di
			push bx
			push cx

			mov ax,0xb800
			mov es,ax

			push 0
			push word [topLeft]
			push word [topLeft+2]
			call index_finder
			pop di
			sub di,4
			mov cx,4
			c_loop:
			mov ax,0x80e9
			mov bx,[es:di]
			cmp ax,bx
			je found
			mov ax,0x8ea7
			mov bx,[es:di]
			cmp ax,bx
			je found
			add di,160
			loop c_loop
			mov word [bp+4],0
			jmp finish
			found:
			mov word [bp+4],1
			finish:

			pop cx
			pop bx
			pop di
			pop es
			pop ax
			pop bp
ret

rightCollisionCheck:
		push bp
		mov bp,sp
		push ax
		push es
		push di
		push bx
		push cx
		pushf

		mov ax,0xb800
		mov es,ax

		push 0
		push word [bottomRight]
		push word [bottomRight+2]
		call index_finder
		pop di
		sub di,2
		mov cx,4

		d_loop:
		mov ax,0x80e9
		mov bx,[es:di]
		cmp ax,bx
		je founder
		mov ax,0x8ea7
		mov bx,[es:di]
		cmp ax,bx
		je founder
		sub di,160
		loop d_loop

		mov word [bp+4],0
		jmp finisher

		founder:
		mov word [bp+4],1

		finisher:
		popf
		pop cx
		pop bx
		pop di
		pop es
		pop ax
		pop bp
ret 

frontCollisionCheck:
		push bp
		mov bp,sp
		push ax
		push es
		push di
		push bx
		push cx
		pushf

		mov ax,0xb800
		mov es,ax

		push 0
		mov ax,[topLeft]
		sub ax,1
		push ax
		push word [topLeft+2]
		call index_finder
		pop di
		mov ax,0x8ea7
		mov cx,2

		e_loop:
		mov bx,[es:di]
		cmp ax,bx
		je founded
		add di,4
		loop e_loop

		push 0
		push word [topLeft]
		push word [topLeft+2]
		call index_finder
		pop di
		sub di,2
		mov ax,0x8ea7
		cmp ax,[es:di]
		je founded
		mov ax,0x80e9
		cmp ax,[es:di]
		je founded
		sub di,160
		mov ax,0x8ea7
		cmp ax,[es:di]
		je founded
		mov ax,0x80e9
		cmp ax,[es:di]
		je founded
		add di,8
		cmp ax,[es:di]
		je founded
		mov ax,0x8ea7
		cmp ax,[es:di]
		je founded

		;add di,4
		;cmp ax,[es:di]
		;je founded
		;mov ax,0x80e9
		;cmp ax,[es:di]
		;je founded

		mov word [bp+4],0
		jmp finished

		founded:
		;mov word [es:di],0x0720
		mov word [bp+4],1

		finished:
		popf
		pop cx
		pop bx
		pop di
		pop es
		pop ax
		pop bp
ret 

printGameSpeed:
		push 0
		push 3
		push 75
		call index_finder
		push word [displayer]
		call printnum
		ret
		int9ISR:
		push ax
		push es
		push di
		push bx

		mov ax,0xb800
		mov es,ax


		in al,0x60
		cli
		cmp al,25
		jne NoPause
		mov word [pauseGame],1

		jmp ended

		NoPause:
		cmp al,19
		jne NoResume
		mov word [pauseGame],0

		NoResume:
		cmp word[pauseGame],0
		je carry_on
		jmp ended
		carry_on:

		cmp al,0x48;for up
		jne down_case
		cmp word [speedOfCars],1
		je dont_change
		sub word [speedOfCars],1
		add word [displayer],1
		call printGameSpeed
		dont_change:
		jmp ended

		down_case:
		cmp al,0x50;for down
		jne left_case
		cmp word [speedOfCars],5
		je dont_change
		add word [speedOfCars],1
		sub word [displayer],1
		call printGameSpeed
		jmp ended

		left_case:
		cmp al,75;for left
		jne right_case
		mov di,[topLeft+2]
		sub di,1
		cmp di,[leftSideOfRoad];0x6f08
		jle ended
		push 0
		call leftCollisionCheck
		pop ax
		cmp ax,1
		je GO
		call clear
		mov [topLeft+2],di
		mov di,[bottomRight+2]
		sub di,1;2
		mov [bottomRight+2],di
		call PlayersCar
		jmp ended

		right_case:
		cmp al,77;for right
		jne ended
		mov di,[bottomRight+2]
		add di,1
		cmp di,[rightSideOfRoad]
		jge ended
		push 0
		call rightCollisionCheck
		pop ax
		cmp ax,1
		je GO
		call clear
		mov [bottomRight+2],di
		add word [topLeft+2],1
		call PlayersCar
		jmp ended
		GO:
		mov word [endGame],1
		ended:

		pop bx
		pop di
		pop es
		pop ax
		sti
		jmp far [cs:oldisrInt9]



		save_oldisr:
		push ax
		push es
		xor ax, ax
		mov es, ax ; point es to IVT base
		mov ax, [es:9*4]
		mov [oldisrInt9], ax ; save offset of old routine
		mov ax, [es:9*4+2]
		mov [oldisrInt9+2], ax ; save segment of old routine
		mov ax, [es:8*4]
		mov [oldisrInt8], ax ; save offset of old routine
		mov ax, [es:8*4+2]
		mov [oldisrInt8+2], ax ; save segment of old routine
		pop es
		pop ax
		ret

		unhook_interrupts:
		push ax
		push es
		mov ax,0
		mov es,ax
		cli ; disable interrupts
		mov ax,[oldisrInt8]
		mov word [es:8*4],ax ; store offset at n*4
		mov ax,[oldisrInt8+2]
		mov [es:8*4+2], ax ; store segment at n*4+2
		mov ax,[oldisrInt9]
		mov word [es:9*4],ax ; store offset at n*4
		mov ax,[oldisrInt9+2]
		mov [es:9*4+2], ax ; store segment at n*4+2
		sti ; enable interrupts
		pop es
		pop ax
ret

hook_interrupts:
			push ax
			push es
			 xor ax, ax
			 mov es, ax ; point es to IVT base
			 cli ; disable interrupts
			 mov word [es:8*4], timer; store offset at n*4
			 mov [es:8*4+2], cs ; store segment at n*4+2
			 mov word [es:9*4], int9ISR; store offset at n*4
			 mov [es:9*4+2], cs ; store segment at n*4+2
			 sti ; enable interrupts
			pop es
			pop ax
			ret
			curve:
			push cx
			mov cx,10
			mov ax,120
			push word [leftSideOfRoad]
			push word [rightSideOfRoad]
			next_iteration:
			call delay
			call delay
			;cmp ax,112
			;je nott
			;cmp ax,108
			;je nott
			;cmp ax,110
			;je nott
			;cmp ax,114
			;je nott
			sub ax,2
			;push ax
			sub word [leftSideOfRoad],2
			sub word[rightSideOfRoad],2
			nott:
			push ax
			push bx
			call moveBackground
			add bx,1
			push 0
			push word [num]
			call RANDNUM
			pop dx
			cmp dx,4
			je doer
			jmp never
			doer:
			push 0
			push word [num]
			call RANDNUM
			pop dx
			cmp dx,1
			je never
			push 0
			push word 39
			call RANDNUM
			call makeObject
			push cx
			mov cx,3
			small_loop:
			push ax
			push bx
			call moveBackground
			add bx,1
			;sub ax,2
			sub word [leftSideOfRoad],2
			sub word [rightSideOfRoad],2
			loop small_loop
			pop cx
			never:
			loop next_iteration
			pop word [rightSideOfRoad]
			pop word [leftSideOfRoad]
			pop cx
			jmp return_back
			gameplay:
			push bx
			push cx
			push dx
			push ax
			push es
			mov bx,3
			mov dx,0
			mov ax,0xb800
			mov es,ax
			call printBackground
			call PlayersCar
			main_loop:
			mov cx,[speedOfCars]
			speedGenerator:
			call delay
			loop speedGenerator
			cmp word [endGame],1
			je getOut
			cmp word [pauseGame],1
			je nope
			;cmp word [tickcount],10
			;je curve
			return_back:
			push word 120
			push bx
			call moveBackground
			add bx,1
			push 0
			push word [num]
			call RANDNUM
			pop dx
			cmp dx,4
			je do
			jmp nope
			do:
			push 0
			push word [num]
			call RANDNUM
			pop dx
			cmp dx,1
			je nope
			push 0
			push word 39
			call RANDNUM
			call makeObject
			push word 120
			push bx
			call moveBackground
			add bx,1
			push word 120
			push bx
			call moveBackground
			add bx,1
			push word 120
			push bx
			call moveBackground
			add bx,1
			nope:
			cmp word [endGame],1
			je getOut
			jmp main_loop
			getOut:
			pop es
			pop ax
			pop dx
			pop cx
			pop bx
ret
simpleShift:
		push cx
		push es
		push di
		push si
		push ds
		pushf
		mov cx,0xb800
		mov es,cx 
		mov ds,cx
		Mov di,4000
		Mov si,4000
		sub si,160
		Mov cx,80*24
		Std
		Rep movsw
		popf
		pop ds
		pop si
		pop di
		pop es
		pop cx
ret
simplePrint:
		push bp
		mov bp,sp
		push word 5
		push word 2
		push word 0x07
		push word [bp+4]
		call printstr
		pop bp
		ret 2
		move:
		push cx
		call simpleShift
		call simpleShift
		mov cx,2
		looper:
		call delay
		loop looper
		pop cx
ret
take_instruction:
		push bp
		mov bp,sp
		push ax
		inst_again:
		mov ax,0
		int 0x16
		cmp ah,0x50
		jne inst_again
		push word [bp+4]
		call simplePrint
		call move
		pop ax
		pop bp
ret 2
clear_statement:
			push es
			push ax
			push di
			push cx
			pushf
			mov ax,0xb800
           mov es,ax
           mov cx,60
           push 0
           push 22
           push 20
           call index_finder
           pop di
           cld
           mov ax,0x0720
           rep stosw
           popf
           pop cx
           pop di
           pop ax
           pop es
ret
askForAgain:
           push ax
           push 21
           push 17
           push 0x62
           push string14
           call printstr
keep_Asking:
           mov ax,0
           int 0x16
           cmp ah,21;for y
           jne CheckForNo
           mov word [endGame],0
           jmp endIt
CheckForNo:
           cmp ah,16;for q
           jne keep_Asking
           mov word [endGame],1
endIt:
           pop ax
ret

setVariables:
           mov word [tickcount],0
           mov word [counter],0
           mov word [endGame],0
           mov word [pauseGame],0
           mov word [speedOfCars],3
           mov word [displayer],3
ret
helpManual:
           call clr_scr
           push 20
           push 20
           push 0x07
           push string12
           call printstr
           push string6
           call take_instruction
           call clear_statement
           push string7
           call take_instruction
           push string8
           call take_instruction
           push string9
           call take_instruction
           push string18
           call take_instruction
           push string19
           call take_instruction
           push string20
           call take_instruction
           push string21
           call take_instruction
           push string10
           call take_instruction
           push string11
           call take_instruction
           push string16
           call take_instruction
           push 20
           push 2
           push 0x07
           push string13
           call printstr
ask_again:
           mov ax,0
           int 16h
           cmp ah,14
           jne ask_again
           call clr_scr
           jmp main_menu
check_HighestScore:
           push ax
           mov ax,[tickcount]
           cmp ax,[highestScore]
           jl not_changed
           mov [highestScore],ax
           push 21
           push 4
           push 0x25
           push string5
           call printstr
           jmp end_now
not_changed:
           push word 21
           push word 3
           push word 0x25
           push string15
           call printstr
           push word 0
           push word 3
           push word 43
           call index_finder
           push word [highestScore]
           call printnum
end_now:
           pop ax
ret
delayer:
         push cx
		 mov cx,7
		 loop1st:	
		 call delay
		 loop loop1st
pop cx
ret
starter:
		 call clr_scr
		 call print_6t_ch
		 call delayer
		 call clr_scr
		 call print_2_ch	
		 call delayer
		 call clr_scr
		 call print_2n_ch
		 call delayer
		 call clr_scr	
		 call p_ist_character
		 call p_2nd_character
		 call delayer
		 call clr_scr
		 call delayer
		 ret
gameOver:
           call delay
           call delay
           call delay
           call delay
           call delay
           call delay
           call clr_scr
           push word 35
           push word 12
           push word 0x94
           push string3
           call printstr
           push word 35
           push word 6
           push word 0x94
           push string4
           call printstr
           push word 0
           push word 6
           push word 49
           call index_finder
           push word [cs:tickcount]
           call printnum
           call delay
           call delay
           call delay
           call delay
           call check_HighestScore
           call askForAgain
           cmp word [endGame],1
           je ret_address
           jmp setGame
setGame:
           call setVariables
           call starter
           call save_oldisr
           call hook_interrupts
           call gameplay
           call unhook_interrupts
           jmp gameOver

main_menu:
           push ax
           call printGameName
           push word 20
           push word 18
           push word 0x07
           push string1
           call printstr
           push word 20
           push word 20
           push word 0x07
           push string2
           call printstr

           push word 20
           push word 22
           push word 0x07
           push string0
           call printstr

           mov ah,0
           int 16h
           cmp ah,35
           je helpManual
           cmp ah,28
           je setGame
           cmp ah,1
           je terminate

           ret_address:
           pop ax
ret

start:
           call clr_scr
;mov [highestScore],ax
           mov ax, 1003h
           mov bx, 0
           int 10h
           call main_menu
terminate:
;mov dx, soundSystem ; end of resident portion
;add dx, 15 ; round up to next para
;mov cl, 4
;shr dx, cl ; number of paras
;mov ax, 0x3100 ; terminate and stay resident
;int 0x21
mov ax,0x4c00
int 21h
oldisrInt8: dd 0
oldisrInt9: dd 0
seed: dw 5
num: dw 4
endGame: dw 0
pauseGame: dw 0
leftSideOfRoad: dw 31
rightSideOfRoad: dw 57
highestScore: dw 0
tickcount: dw 0
counter: dw 0
topLeft: dw 21,50
bottomRight: dw 24,55
speedOfCars: dw 3
displayer: dw 3
string0: db 'Press "ESC" to exit',0
string1:db 'welcome to the game "ROAD FIGHTER!" ',0
string2:db 'Main Menu: press "ENTER" to play game, "H" for help manual',0
string3: db 'Game Over!',0
string4:db 'Your score is:',0
string5: db 'Congratulations! you have a new high score...',0
string6:db 'To move the car left press "left arrow key". ',0
string7: db'To move the car right press "right arrow key". ',0
string8: db 'To pause the game at any point press "P"',0
string9: db 'To resume the paused game press "R" ',0
string10: db 'Your aim is to avoid collision and beat your Highest Score',0
string11: db 'Game is over once you have collided with another car ',0
string12: db 'keep pressing "down arrow key" to get to next instruction',0
string13: db 'press "BACK SPACE" to go back to main menu',0
string14: db 'To play again press "Y" To Quit press "Q"',0
string15: db 'Your Highest Score Is:',0
string16: db 'Note: Highest Score is counted as long as you replay the game',0
string17: db 'Game Speed:',0
string18: db 'You can adjust the game speed during the game',0
string19: db 'To increase the game speed press "UP ARROW KEY"',0
string20: db 'To decrease the game speed press "DOWN ARROW KEY"',0
string21: db 'Minimum and Maximum game speeds allowed are 1 and 5 respectively',0
curveV: dw 0