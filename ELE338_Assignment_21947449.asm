
org 100h

;setting video mode
mov ah,00h         
int 10h

call createmenu

mov ch,32   ;hiding the cursor
mov ah,1
int 10h      

;reading from the keyboard

keyboard:
mov ah,00h
int 16h

cmp ah,4Dh ; to the right button
je ToTheRight

cmp ah,4Bh ; to the left button
je ToTheLeft

cmp ah,1ch;enter button
je enter                  

cmp ah,01h   ;esc entered
je activemenu6

jmp keyboard  ;if someone touched left,right or enter button,go back to the start  

;-----------------------------------------------------------
TotheRight:
     inc [activebutton]
     cmp [activebutton],1
     je activebutton1  
     cmp [activebutton],2
     je activebutton2  
     cmp [activebutton],3
     je activebutton3  
     cmp [activebutton],4
     je activebutton4    
     cmp [activebutton],5
     je activebutton5
     cmp [activebutton],6
     je activebutton6
jmp keyboard ;        

TotheLeft:
     dec [activebutton]
     cmp [activebutton],1
     je activebutton1  
     cmp [activebutton],2
     je activebutton2  
     cmp [activebutton],3
     je activebutton3  
     cmp [activebutton],4
     je activebutton4 
     cmp [activebutton],5
     je activebutton5
     cmp [activebutton],6
     je activebutton6
jmp keyboard ;           


enter:    
       
       
 ;------------------------------------------    
  cmp [activebutton],1       ;compare which menu entered from the user
  je activemenu1   
  
  cmp [activebutton],3
  je activemenu3   
  
  cmp [activebutton],2
  je activemenu2       
  
  cmp [activebutton],4
  je activemenu4    
  
  cmp [activebutton],5
  je activemenu5
 
  cmp [activebutton],6
  je activemenu6
                   
  ;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
  
activemenu1:  ;first menu 

 mov al,04h
  mov bh,0000_1111b  
  mov cx,0000
  mov dh,05d
  mov dl,40d
mov ah,06h
int 10h ; clear the screen 

  call newline

  call gettingnumber  
  
  mov [fibo],ax
  call newline
  
  
mov si, 1
mov bp, 0

mov cx,8 ; 

fibonacci: 
push cx
add bp,si 
mov ax,bp  
cmp ax,fibo ; compare if ax number grater than input
jg notfibo   
call print   

call newline
add si,bp
mov ax,si 
cmp ax,fibo
jg notfibo 
call print   

call newline  
pop cx
loop fibonacci
jmp fin

notfibo:
 hlt
fin:


ret   
;-------------------------------------------------------------                
                   
activemenu4: 
 mov al,04h
  mov bh,0000_1111b  
  mov cx,0000
  mov dh,05d
  mov dl,40d
mov ah,06h
int 10h ; clear the screen   
 


call newline   
call twodigit          ;ask 2-digit input stored at al
	 mov bh,al           ;i will check bh register if it is a prime number
	   
	 
	   
	   CMP bh,1         ; if bh is equal to 1 then it is not a prime  number
	   JLE noprime
	   
	   MOV CX,2
	   xor dx,ax
	   xor dx,dx
	   
	   MOV AL,bh       ;al<--bh
	   DIV CX
	   
	   MOV CX,AX            ;DX:AX / CX  = remainder = DX ,   
	   
	   checkprime:
	   CMP CX,2
	   JL PRIME         
	   AND AX,0
	   AND DX,0
	   MOV AL,bh              ;DX:AX / CX  = REM = DX , QUE = AX 
	   DIV CX 
	   DEC CX
	   CMP DX,0
	   JE noprime          ;if dx(remainfder is zero then it is not a prime number)
	   JMP checkprime              ;else
	   
	   
	   
	   
	   prime:
	  mov cx,0      
mov al,0    
mov bh,0
mov bl,0000_0010b   
mov cx,offset endprime1 - offset prime1
mov dl,0
mov dh,2
mov bp,offset prime1
mov ah,13h
int 10h  
	   JMP EXIT1
	   
	   
	   noprime:
	   mov cx,0      
mov al,0    
mov bh,0
mov bl,0000_0010b   
mov cx,offset endnotprime1 - offset notprime1
mov dl,0
mov dh,2
mov bp,offset notprime1
mov ah,13h
int 10h  
	  	   
	  EXIT1:	   
	  	   
		   
ret
;-------------------------------------------------------------

ret  
                                  
activemenu2:

 mov al,04h
  mov bh,0000_1111b  
  mov cx,0000
  mov dh,05d
  mov dl,40d
mov ah,06h
int 10h ; clear  screen  

mov ah,01h
int 21h
           

mov bh,0
mov bl,al   
sub bl,30h; ascii to integer
call newline

call factorial1                     
                                  
ret                                  
;-------------------------------------------------------------
  activemenu3:     
  mov al,04h
  mov bh,0000_1111b  
  mov cx,0000
  mov dh,05d
  mov dl,40d
mov ah,06h
int 10h ; clear the screen 
  call newline 
 
  call gettingnumber ;ask 3-digit input stored at ax

mov [special],ax
lea si,cx
mov cx,0000   

mov bx,0ffffh
l1:         
cmp cx,32  ;squre root of 32 is larger than 999 so it stops there
je finish4
add bx,02
inc cx   
sub ax,bx 

jnz l1

jmp finish2

finish4:
mov cx,0      
mov al,0    ;printing the instruction
mov bh,0
mov bl,0000_0010b   
mov cx,offset menu2word1end - offset menu2word1
mov dl,0
mov dh,2
mov bp,offset menu2word1
mov ah,13h
int 10h         

call newline    

mov ax,[special]
mov dx,2     
mov bx,dx
push dx

dongu: 
pop dx
inc dx
push dx
call sqaure 

cmp dx,ax
jg finish3
jmp dongu

finish3: 

dec bx
mov dx,bx
call sqaure    

mov [special2],bx  ; 
mov [special3],dx  ;i stored number that has square root at dx register  

call newline
mov ax,[special2]  ;printing smaller number that has a square root
call print   

call newline
mov ax,[special3]   ; 
call print


hlt
ret

mov dx,1

mov cx,dx

square:
add dx,dx
loop square 


finish2:


  

jmp finish ;

finish:       

mov [special1],cx ;squareroot of integer
call newline
mov ax,[special1]

call print      ;print data at ax register

ret
;----------------------------------------------------------
activemenu5:    

mov cx,0      
mov al,0    
mov bh,0
mov bl,0000_0010b   
mov cx,offset endabout - offset about
mov dl,0
mov dh,0
mov bp,offset about
mov ah,13h
int 10h    

mov dh,1
mov cx,offset endabout2 - offset about2
mov bp,offset about2
int 10h

mov dh,2
mov cx,offset endabout3 - offset about3
mov bp,offset about3
int 10h
ret  

activemenu6:

ret  
  
  
;procdures and variables 
;=====>  

;-----------------------------------------------------------------
createmenu proc   ; procedure for creating menu
    mov al,0             ;menu 1
    mov bh,0             ;page number
    mov bl,0111_0100b    ;red over gray 
    mov dl,2              ;line  ;*
    mov dh,0               ;column
    mov cx,offset menu1end - menu1   ;length of the message
    mov bp,offset menu1
    mov ah,13h
int 10h            ;   printing string

    mov al,0
    mov bh,0
    mov bl,0111_0100b
    mov dl,11        ;**
    mov dh,0
    mov cx,offset menu2end - menu2
    mov bp,offset menu2
    mov ah,13h
int 10h


    mov al,0
    mov bh,0
    mov bl,0111_0100b
    mov dl,21       ;***
    mov dh,0
    mov cx,offset menu3end - menu3
    mov bp,offset menu3
    mov ah,13h
int 10h        

    mov al,0
    mov bh,0
    mov bl,0111_0100b
    mov dl,31      ;****
    mov dh,0
    mov cx,offset menu4end - menu4
    mov bp,offset menu4
    mov ah,13h
int 10h            
    
    mov al,0
    mov bh,0
    mov bl,0111_0100b
    mov dl,31      ;****
    mov dh,2
    mov cx,offset exitmenuend - exitmenu
    mov bp,offset exitmenu
    mov ah,13h
int 10h    
    
    mov al,0
    mov bh,0
    mov bl,0111_0100b
    mov dl,2      ;****
    mov dh,2
    mov cx,offset aboutmenuend - aboutmenu
    mov bp,offset aboutmenu
    mov ah,13h
int 10h

ret

createmenu endp       
                 
;-------------------------------------------------------------
                 
activebutton1 proc  ;changing the colour of the active buttton 
    
    call createmenu    ;reseting coulours
    mov al,0             
    mov bh,0
    mov bl,0100_0111b    ;its colours are reversed 
    mov dl,2             
    mov dh,0
    mov cx,offset menu1end - menu1
    mov bp,offset menu1
    mov ah,13h
int 10h        
jmp keyboard      ;return to keybord for getting next input
ret
activebutton1 endp 
;------
activebutton2 proc  ;same as activebutton1 but for 2nd button  
    
    call createmenu
    mov al,0             
    mov bh,0
    mov bl,0100_0111b   ;**  
    mov dl,11             
    mov dh,0
    mov cx,offset menu2end - menu2
    mov bp,offset menu2
    mov ah,13h
int 10h
jmp keyboard             

ret
activebutton2 endp 
;---------
activebutton3 proc   
    
    call createmenu
    mov al,0             
    mov bh,0
    mov bl,0100_0111b     
    mov dl,21             
    mov dh,0
    mov cx,offset menu3end - menu3
    mov bp,offset menu3
    mov ah,13h
int 10h 
jmp keyboard            

ret
activebutton3 endp 
;------------
activebutton4 proc   
    
    call createmenu
    mov al,0             
    mov bh,0
    mov bl,0100_0111b     
    mov dl,31             
    mov dh,0
    mov cx,offset menu4end - menu4
    mov bp,offset menu4
    mov ah,13h
int 10h         
jmp keyboard    

ret
activebutton4 endp   

activebutton5 proc    
    call createmenu
    mov al,0
    mov bh,0
    mov bl,0100_0111b ;*****
    mov dl,2      
    mov dh,2
    mov cx,offset aboutmenuend - aboutmenu
    mov bp,offset aboutmenu
    mov ah,13h
int 10h
jmp keyboard             

ret
activebutton5 endp   


activebutton6 proc
    call createmenu  
    mov al,0
    mov bh,0
    mov bl,0100_0111b ;*****
    mov dl,31      
    mov dh,2
    mov cx,offset exitmenuend - exitmenu
    mov bp,offset exitmenu
    mov ah,13h
int 10h
jmp keyboard 
 
ret

activebutton6 endp 

;---------------------

menu1 db 1 dup(' ') ,"menu1" ,1 dup(' ') 
menu1end db 0 
menu2 db 1 dup(' '), "menu2" ,1 dup(' ') 
menu2end db 0 
menu3 db 1 dup(' '), "menu3" ,1 dup(' ') 
menu3end db 0  
menu4 db 1 dup(' '),"menu4" ,2 dup(' ') 
menu4end db 0                             
exitmenu db 1 dup(' ') ,"exit" ,1 dup(' ') 
exitmenuend db 0                            
aboutmenu     db 1 dup(' ') ,"aboutmenu" ,1 dup(' ')
aboutmenuend  db 0
activebutton db 0   
;-------------------------------------------------------------  

about db "1.menu:fibonacci numbers 2.menu: factorials"


endabout db 0
about2 db "3.square roots 4.menu:prime numbers "  
endabout2 db 0

about3 db "Muhammet Selman Kirmaci ,Hacettepe EE "  
endabout3 db  0

menu2word1 db "square root of an integer doesn't exist"

menu2word1end  db 0 



gettingnumber proc      ;for reading integer from the user ,input data will stored at ax register
        Start: 

   
    mov AH,0Ah
    mov DX, offset inputnumber
    int 21h                   ; reading string from the buffer 
    ;you need to press enter after writing your input

   mov si,2
   mov ax,0
   mov cl,inputnumber[1]

go:
   mov bh,10
   mul bh

   add al,inputnumber[si]
   sub al,48      ;ascii converter
   add si,1
   sub cl,1
  
   jne go 
 
 ret  
gettingnumber endp   

 inputnumber DB 4, ? , 4 dup (?)  
 special dw 0 
 special1 dw 0 
 special2 dw 0
 special3 dw 0
 
 newline proc    ;sets cursor position
    mov ah,02h    ; new line 
    mov dl,10d 
    int 21h       
    mov dl,13d 
    int 21h   
    mov ah,03h
    int 10h  
ret            

 endp  
 
 sqaure proc   ;takes the square 

mov cx,dx 
sub cx,1
mov bx,dx

square1: 

add dx,bx 

loop square1 
ret 
sqaure endp       
 
 twodigit PROC
    
    MOV AX, @DATA
    MOV DS, AX
    
    MOV AH, 9
    LEA DX, MSG
    INT 21H
    mov cx,2
    
    READ:
    MOV AH, 1
    INT 21H
    
    CMP AL, 13
    JE ENDOFNUMBER
    
    MOV VALUE, AL
    SUB VALUE, 48
    
    MOV AL, TOTAL
    MOV BL, 10
    MUL BL
    
    ADD AL, VALUE
    
    MOV TOTAL, AL
    
   loop READ

    ENDOFNUMBER:   
     ret
twodigit ENDP         

prime1 db "it is a prime number"
endprime1 db 0 

notprime1 db "it is not a prime number"
endnotprime1 db 0

MSG DB "ENTER A NUMBER: $"
TOTAL DB 0
VALUE DB 0
 
fibo dw 0
  
mesaj DB 16 DUP ('$') 
 
 
 factorial1 PROC
   

                            
    xor dx, dx                          ; DX:AX=1 (first multiplicand)
    mov ax, 1      ;starting with 1                     

   
    mov cx, 2                           ; multiplicator
    don1:
    call fact1                 ; DX:AX * CX -> DX:AX    
    add cx,1
    cmp cx, bx
    jbe don1                              ; While cx <= 10

    ; Print result
    mov di, OFFSET mesaj
    call fact2             ;
    mov dx, OFFSET mesaj
    mov ah, 9
    int 21h

    ; Exit
    mov ax, 4C00h
    int 21h
factorial1 ENDP

fact1 PROC                     ; DX:AX multiplicand, CX multiplier
    push dx

    mul cx                              ;multiply ax with cx store at ax:dx
    mov si, dx                          ;  high result at si register
    mov di, ax                          ; low result

    pop ax                              ; High word
    mul cx                              ; AX * CX -> DX:AX
    add ax, si                          ; Add high result from last mul to low result here
    adc dx, 0

    mov si, dx                          ; SI:DX:AX return value
    mov dx, ax
    mov ax, di
    ret                               
fact1 ENDP

fact2 PROC                       ; ARG DX:AX DWORD, DI: offset of string

    mov cs:target, di
    mov si, ax
    mov di, dx

    ; First Loop: get digits and push them
    mov cs:counter, 0
    mov bx, 10
    dongum1:
    inc cs:counter
    xor dx, dx
    mov ax, di                          
    mov cx, ax
    div bx                              ; DX:AX / BX -> AX Remainder DX
    mov di, ax                          ; Store new high word
    mul bx                              ; AX * BX -> DX:AX
    sub cx, ax                          ; sub highest CX-divisible value

    mov dx, cx
    mov ax, si                          
    div bx                              ; DX:AX / BX -> AX Remainder DX
    or dl, 30h                          ; Convert remainder to ASCII
    push dx                             ; Store remainder
    mov si, ax                          ; Store new low WORD

    or ax, di                           
    jnz dongum1                             

    ; Second Loop: get back digits in reversed order
    mov di, cs:target
    mov cx, cs:counter
    dongum2:
    pop ax
    mov [di], al
    inc di
    loop dongum2
    mov BYTE PTR [di], '$'              

    ret
    counter dw 0
    target dw 0
fact2 ENDP
  
  
  PRINT PROC     ; print data insied the ax register     
    
    mov cx,0
    mov dx,0
    model1:
       
        cmp ax,0
        je yaz1     

        mov bx,10       
                                
        div bx                        
        push dx             
    
        inc cx                  
        xor dx,dx
        jmp model1
    yaz1:
        
        cmp cx,0
        je bitis
  
        pop dx
        
        add dx,48   ;ascii converter
         
       
        mov ah,02h
        int 21h   ;print
         
       
        dec cx
        jmp yaz1
bitis:
ret
PRINT ENDP   