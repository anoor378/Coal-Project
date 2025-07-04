[org 0x0100]
    jmp start

; Data Section
score: dw 0
lives: dw 3 
score_str: db 'Score: ', 0
lives_str: db 'Lives: ', 0
title_str: db 'SNAKE GAME', 0
gameover_str: db 'GAME OVER', 0	
restart_str: db 'Press SPACE to restart', 0 
blank_str: db '                      ', 0
exit_str: db 'Press ESC to exit', 0

direction: db 1          ; 0 = up, 1 = right, 2 = down, 3 = left

; Queue-based snake variables
snake_queue:	times 18 * 77 dw 0
head_pointer:   dw snake_queue + 0x0006
tail_pointer:   dw snake_queue + 0x0000

; Food variables
food_pos: dw 0          ; stores offset of current food
seed: dw 0xABDD         ; random seed

;-------------------------------------------------------------------------------------
; subroutine to clear the screen
clrscr:
    push es
    push ax
    push cx
    push di

    mov ax, 0xb800
    mov es, ax          ; point es to video base
    xor di, di          ; point di to top left column
    mov ax, 0x0720      ; space char in normal attribute
    mov cx, 2000        ; number of screen locations

    cld                 ; auto increment mode (increments DI, cx times)
    rep stosw           ; clear the whole screen (repeats cx times)

    pop di
    pop cx
    pop ax
    pop es
    ret

;-------------------------------------------------------------------------------------
; subroutine to calculate offset
; (80y + x) * 2
calculateOffset:
    push bp
    mov bp, sp
    push ax
    push bx

    mov ax, 80          ; characters per row
    mov bx, [bp + 6]    ; row
    mul bx              ; ax = row * 80
    add ax, [bp + 4]    ; col
    shl ax, 1           ; each character takes 2 bytes
    mov di, ax          ; di contains offset

    pop bx
    pop ax
    pop bp
    ret 4

;-------------------------------------------------------------------------------------
; subroutine to print rectangle
printRectangle:
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di

    mov ax, 0xb800
    mov es, ax          	; point es to video base

	; calculating the top-left corner offset
    push word [bp + 12] 	; push row (y)
    push word [bp + 10] 	; push col (x)
    call calculateOffset 	; result in DI

    mov ah, [bp + 4]    	; load attribute in ah
    mov cx, [bp + 6]    	; Calculate width of top line = right - left
    sub cx, [bp + 10]

topLine:		; left to right
    mov al, 0x2D        	; ASCII of '-'
    mov [es:di], ax     	; show this char on screen
    add di, 2           	; move to next screen location
    loop topLine        	; repeat the operation cx times

    mov cx, [bp + 8]
    sub cx, [bp + 12]		; Calculate height = bottom - top
    add di, 160				; moving DI to the start of the next line

rightLine:
    mov al, 0x7C        	; ASCII of '|'
    mov [es:di], ax     	; show this char on screen
    add di, 160         	; move down one row (next screen location)
    loop rightLine      	; repeat the operation cx times

    mov cx, [bp + 6]
    sub cx, [bp + 10]		; again calculating width (right - left) for bottom line
    sub di, 2

bottomLine:		; right to left
    mov al, 0x2D        	; ASCII of '-'
    mov [es:di], ax     	; show this char on screen
    sub di, 2           	; move to next screen location
    loop bottomLine     	; repeat the operation cx times

    mov cx, [bp + 8]
    sub cx, [bp + 12]
    sub di, 160

leftLine:
    mov al, 0x7C       		; ASCII of '|'
    mov [es:di], ax    		; show this char on screen
    sub di, 160        		; move to next screen location
    loop leftLine      		; repeat the operation cx times

    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 10

;-------------------------------------------------------------------------------------
; Subroutine to print a string
printString:
    push bp
    mov bp, sp
    push es
    push ax
    push cx
    push si
    push di
	
	push word [bp + 10]   ; push row
    push word [bp + 8]    ; push col
    call calculateOffset  ; result in DI

    mov ax, 0xb800
    mov es, ax         
	
    mov si, [bp + 6]    ; string address
    mov ah, [bp + 4]    ; attribute
    mov cx, 0           ; counter for string length

print_loop:
    lodsb               ; load next char from si to al
    cmp al, 0           ; check for null terminator
    je print_done
    stosw               ; store ax (char+attribute) at es:di (increments DI by 2) or printing char on screen
    inc cx
    jmp print_loop

print_done:
    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 8

;-------------------------------------------------------------------------------------
; Subroutine to update score display
updateScore:
    push ax
    push bx
    push cx
    push dx
    push di

    ; position for score value (after "SCORE: ")
    mov ax, 0xb800
    mov es, ax
    mov di, 16          

; Score is in the form of number (25), but we have to show it as ASCII digits on the screen. so we break the words in digits and puch them on the stack in reverse order 

    ; Convert score to ASCII
    mov ax, [score]
    mov bx, 10
    mov cx, 0
 
convert_loop:
    xor dx, dx
    div bx			; AX ÷ 10, DX = remainder (last digit)
    add dl, 0x30	; converting in ASCII
    push dx			; push the last extracted digit
    inc cx
    test ax, ax		; is qoutient = 0?
    jnz convert_loop

; Display score digits
display_loop:
    pop ax				; popping the digits to print
    mov ah, 0x0E        ; yellow on black
    mov [es:di], ax
    add di, 2
    loop display_loop

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;-------------------------------------------------------------------------------------
; Subroutine to update lives display
updateLives:
    push ax
    push bx
    push cx
    push dx
    push di

    ; position for lives value (after "LIVES: ")
    mov ax, 0xb800
    mov es, ax
    mov di, 154         

    ; Clear previous lives display
    mov ax, 0x0720     	; space character
    mov [es:di], ax

    ; Convert lives to ASCII
    mov ax, [lives]
    add al, 0x30		; converting in ASCII
    mov ah, 0x0E        ; yellow on black
    mov [es:di], ax

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret

;-------------------------------------------------------------------------------------
; Queue management subroutines
snake_enqueue:
    push bp
    mov bp, sp
    push ax
    push si

    add word [head_pointer], 2	 						; increment head pointer
    cmp word [head_pointer], snake_queue + (18*77*2) 	; reached end of queue
    jb not_end_of_head 									; if not, continue

    mov word [head_pointer], snake_queue 				; move to start of queue

not_end_of_head:
    mov ax, [bp + 4] 			; get new element to be added
    mov si, [head_pointer] 		; get head pointer
    mov [si], ax 				; store new element at head pointer

    pop si
    pop ax
    pop bp
    ret 2

snake_dequeue:
    push bp
    mov bp, sp
    push si

    mov si, [tail_pointer]		; get element to be removed
    mov si, [si] 				; get the element at tail pointer
    mov [bp + 4], si 			; store removed element in stack

    add word [tail_pointer], 2 							; increment tail pointer
    cmp word [tail_pointer], snake_queue + (18*77*2) 	; reached end of queue
    jb not_end_of_tail 									; if not, continue

    mov word [tail_pointer], snake_queue 	; move to start of queue

not_end_of_tail:
    pop si
    pop bp
    ret
	
;-------------------------------------------------------------------------------------
; Snake Movement subroutine (queue-based)
move_snake:
    push es
    push di

    push word 0xb800
    pop es

    ; Move head based on direction
    mov di, [head_pointer]	 ; fetches current head position and stores it in DI
    mov di, [di]
    mov word [es:di], 0x0A2A ; print body segment instead of head or change to head to body segment

    cmp byte [direction], 0  ; up
    je move_up
    cmp byte [direction], 1  ; right
    je move_right
    cmp byte [direction], 2  ; down
    je move_down
	
    ; default left
move_left:
    sub di, 2          ; move left one column
    jmp move_done
move_up:
    sub di, 160        ; move up one row
    jmp move_done
move_right:
    add di, 2          ; move right one column
    jmp move_done
move_down:
    add di, 160        ; move down one row
	
move_done:
    ; Check wall collision (play area: row 3-19, column 2-78)
	; These lines convert the screen memory offset (DI) into row and column numbers to check if the snake hits the wall
    mov ax, di
    mov bx, 160
    xor dx, dx
    div bx             ; ax = row, dx = column (divide ax by 160 to get row number)
    
    ; Check row boundaries 
    cmp ax, 3
    jl wall_collision
    cmp ax, 20
    jg wall_collision
    
    ; Check column boundaries 
    shr dx, 1          ; convert byte offset to column number (byte offset: 5 * 2 = 10 -> 5 / 2)
    cmp dx, 1
    jl wall_collision
    cmp dx, 78
    jg wall_collision

    ; Check self-collision
    call check_self_collision
    jz self_collision_detected

    ; Print new head position
    mov word [es:di], 0x0A4F 	; print head
    push di
    call snake_enqueue 			; enqueue new head position

    ; Check for food collision
    cmp di, [food_pos]
    jne no_food_collision
    
    ; Handle food consumption
    inc word [score]      	; increase score
    call updateScore      	; update score display
    call generate_food    	; generate new food
    jmp skip_dequeue      	; don't dequeue when growing
    
no_food_collision:
    push word 0xb800
    call snake_dequeue 			; dequeue tail position
    pop di
    mov word [es:di], 0x0720 	; clear tail position

skip_dequeue:
    pop di
    pop es
    ret

self_collision_detected:
    ; Clean up stack first
    pop di
    pop es
    
    call handle_wall_collision	; Then handle collision (same as wall collision)
    
    jmp game_loop	; Skip the rest of the move_snake routine

wall_collision:
    ; Clean up stack first
    pop di
    pop es
    
    call handle_wall_collision	; Then handle collision
    
    jmp game_loop	; Skip the rest of the move_snake routine
	
;-------------------------------------------------------------------------------------
; Handle wall collision
handle_wall_collision:
    dec word [lives]	; Decrease lives
    call updateLives
    
    cmp word [lives], 0
    je real_game_over
    
    call reset_snake	; Reset snake to starting position
	
reset_delay:
    nop
    loop reset_delay
    
    ret

real_game_over:
    ; Display GAME OVER prompt
    push 10
    push 35
    push gameover_str
    push 0x0C
    call printString
    
    ; Display restart prompt
    push 12
    push 30
    push restart_str
    push 0x0A
    call printString
	
    ; Display exit prompt
    push 14
    push 32
    push exit_str
    push 0x0E
    call printString
    
wait_for_restart:
    ; Wait for key press
    mov ah, 0
    int 0x16
    
    cmp al, ' '      ; check if SPACE was pressed (restart)
    je restart_game
    cmp ah, 0x01     ; check if ESC was pressed (exit)
    je exit_game
    
    jmp wait_for_restart  ; if neither, keep waiting
    
restart_game:
    ; Reset game state
    mov word [score], 0
    mov word [lives], 3
    call updateScore
    call updateLives
    
    ; Clear the game over messages
    push 10
    push 35
    push blank_str
    push 0x00
    call printString
    
    push 12
    push 30
    push blank_str
    push 0x00
    call printString
    
    push 14
    push 32
    push blank_str
    push 0x00
    call printString
    
    ; Reset the snake and start new game
    call reset_snake
    call generate_food
    jmp game_loop
	
;-------------------------------------------------------------------------------------
; Subroutine to check self collision
check_self_collision:
    push bp
    mov bp, sp
    push ax
    push cx
    push si
    
	; checking the length of the snake
    mov si, [tail_pointer]
    mov cx, [head_pointer]
    sub cx, si
    shr cx, 1           ; convert byte difference to word count
    
    ; If snake is very short, no collision possible
    cmp cx, 4
    jl no_collision_found
    
    ; Loop through snake segments (skip head)
collision_check_loop:
    mov ax, [si]		; store segment pos in ax
    cmp ax, di          ; compare with position to check
    je collision_found
    
    add si, 2           ; move to next segment
    cmp si, [head_pointer]
    jne not_wrapped
    mov si, snake_queue ; wrap around if needed
    
not_wrapped:
    loop collision_check_loop
    
no_collision_found:
    or ax, 1            ; clear zero flag
    jmp collision_done
    
collision_found:
    xor ax, ax          ; set zero flag
    
collision_done:
    pop si
    pop cx
    pop ax
    pop bp
    ret

;-------------------------------------------------------------------------------------
; Reset snake to starting position
reset_snake:
    push es
    push di
    push si
    push cx
    
    ; Clear current snake from screen
clear_loop:
    push word 0          	; dummy value for return
    call snake_dequeue    	; get position in stack
    pop di               	; get position from stack
    
    mov ax, 0xb800
    mov es, ax
    mov word [es:di], 0x0720 ; clear from screen
    
    mov si, [tail_pointer]
    cmp si, [head_pointer]
    jng clear_loop
    
    ; Reset queue pointers properly
    mov word [head_pointer], snake_queue + 6
    mov word [tail_pointer], snake_queue
    
    ; Initialize snake position (row 10, column 39)
    push word 10          ; row
    push word 39          ; col
    call calculateOffset
    
    ; Initialize queue with starting positions (4 segments)
    mov [snake_queue], di
    sub di, 2
    mov [snake_queue+2], di
    sub di, 2
    mov [snake_queue+4], di
    sub di, 2
    mov [snake_queue+6], di
    
    ; Draw new snake
    mov ax, 0xb800
    mov es, ax
    mov di, [snake_queue]
    mov word [es:di], 0x0A4F   ; head
    mov di, [snake_queue+2]
    mov word [es:di], 0x0A2A   ; body
    mov di, [snake_queue+4]
    mov word [es:di], 0x0A2A   ; body
    mov di, [snake_queue+6]
    mov word [es:di], 0x0A2A   ; body
    
    ; Reset direction to right
    mov byte [direction], 1
    
    ; Clear keyboard buffer to prevent stuck keys
    mov ah, 0x0C        ; flush buffer
    mov al, 0
    int 0x21
    
    pop cx
    pop si
    pop di
    pop es
    ret

exit_game:
    mov ax, 0x4c00     ; terminate program
    int 0x21

;-------------------------------------------------------------------------------------
; Random number generator
; new_seed = (old_seed * multiplier + increment) % 2^16
; Result is a 32-bit value stored in DX:AX (high word in DX, low word in AX)
random:
    mov ax, [seed]
    mov dx, 0xACED    ; multiplier
    mul dx
    add ax, 0x7E      ; increment
    mov [seed], ax    ; store new seed
    ret

;-------------------------------------------------------------------------------------
; Clear current food from screen
clear_food:
    push es
    push ax
    push di
    
    mov di, [food_pos]       ; get current food position
    cmp di, 0                ; check if there's food to clear (food_pos = 0)
    je done
    
    mov ax, 0xb800
    mov es, ax
    mov ax, 0x0720           ; ax = attribute (black bg, white fg) + space char
    mov [es:di], ax          ; clear the food
    
    mov word [food_pos], 0   ; mark as cleared
    
done:
    pop di
    pop ax
    pop es
    ret

;-------------------------------------------------------------------------------------
; Generate food at random position
generate_food:
    push ax
    push bx
    push cx
    push di
    
    call clear_food        	; First clear any existing food
    
    ; Get random position within play area (row 3-19, column 2-78)
	; Calculate row (3-19)
    call random           	; Get random number in AX
    mov bx, ax           	; save it in bx
	
    mov ax, bx            	; Copy to ax for division
    mov cx, 17            	; 17 possible rows
    xor dx, dx				; Clear DX (upper 16 bits of dividend)
    div cx					; quotient ignored and remainder will be stored
    add dx, 3             	; Convert 0-16 to 3-19
    mov bx, dx            	; Store row in BX

    ; Calculate column (2-78)
    call random
    mov cx, 77            ; 77 possible columns
    xor dx, dx
    div cx
    add dx, 2             ; Convert 0-76 to 2-78
    mov cx, dx            ; Store column in CX
    
    ; Calculate screen offset
    push bx               ; push row (3-19)
    push cx               ; push column (2-78)
    call calculateOffset
    mov [food_pos], di    ; store food position
    
    ; Print red '0' at food position
    mov ax, 0xb800
    mov es, ax
    mov ah, 0x0C          ; food color (red)
    mov al, '0'           ; food character
    mov [es:di], ax
    
    pop di
    pop cx
    pop bx
    pop ax
    ret

;-------------------------------------------------------------------------------------
start:
    call clrscr      ; clear screen

    ; Initialize queue pointers
    mov word [head_pointer], snake_queue + 6
    mov word [tail_pointer], snake_queue

    ; Draw game border 
    mov ax, 2
	push ax			; push top
	mov ax, 1
	push ax 		; push left
	mov ax, 20
	push ax 		; push bottom
	mov ax, 79
	push ax 		; push right number
	mov ax, 0x07	; white
	push ax 		; push attribute
	call printRectangle 	; call the rectangle subroutine
	
	; Draw header
    push 0           ; row 0
    push 1           ; column 2
    push score_str   ; "SCORE: "
    push 0x0E        ; yellow on black
    call printString

    push 0           ; row 0
    push 35          ; middle column
    push title_str   ; "SNAKE GAME"
    push 0x0B        ; bright cyan on black
    call printString

    push 0           ; row 0
    push 70          ; right side
    push lives_str   ; "LIVES: "
    push 0x0E        ; yellow on black
    call printString
		
	call updateLives	; display initial lives
    call updateScore	; display initial score 

    ; Initialize queue with starting positions (4 segments)
    mov [snake_queue], di
    sub di, 2
    mov [snake_queue+2], di
    sub di, 2
    mov [snake_queue+4], di
    sub di, 2
    mov [snake_queue+6], di
	
    call generate_food     ; generate initial food

    ; Draw initial snake
    mov ax, 0xb800
    mov es, ax
    mov di, [snake_queue]
    mov word [es:di], 0x0A4F   ; head
    mov di, [snake_queue+2]
    mov word [es:di], 0x0A2A   ; body
    mov di, [snake_queue+4]
    mov word [es:di], 0x0A2A   ; body
    mov di, [snake_queue+6]
    mov word [es:di], 0x0A2A   ; body

;-------------------------------------------------------------------------------------
; Main game loop
game_loop:
    ; Check for keyboard input (non-blocking)
	; int 0x16 with AH = 0x01 checks the keyboard buffer
    mov ah, 1
    int 0x16		; call BIOS interrupt
    jz no_key
	
	; int 0x16 with AH = 0x00 reads the key
    mov ah, 0
    int 0x16

    ; Check for extended key (arrow keys)
    cmp al, 0
    jne no_key

    ; Change direction based on arrow keys
    cmp ah, 0x48       ; up arrow
    je up_key
    cmp ah, 0x4D       ; right arrow
    je right_key
    cmp ah, 0x50       ; down arrow
    je down_key
	
    cmp ah, 0x4B       ; left arrow
    je left_key
    jmp no_key

up_key:
    cmp byte [direction], 2  ; prevent 180° turn
    je no_key
    mov byte [direction], 0
    jmp no_key
    
right_key:
    cmp byte [direction], 3  ; prevent 180° turn
    je no_key
    mov byte [direction], 1
    jmp no_key
    
down_key:
    cmp byte [direction], 0  ; prevent 180° turn
    je no_key
    mov byte [direction], 2
    jmp no_key
    
left_key:
    cmp byte [direction], 1  ; prevent 180° turn
    je no_key
    mov byte [direction], 3

no_key:
    call move_snake			; Move snake continuously

    ; Delay loop
    mov cx, 0x03FF  
delay_outer:
    mov dx, 0x005F
delay_inner:
    dec dx
    jnz delay_inner
    loop delay_outer

	jmp game_loop