; Este es un programa que borra la pantalla.

_datos segment
	public _nivel
	public _nombreNivel

	_nivel dw 0		;el numero de nivel con el que se iterara
	_nombreNivel db "bitacXXX.TXT",0	;donde se almacena el nombre del archivo que se accedera
	_nam db "X.txt",0
	_nombreArchivo db "tablero.txt",0 
	_buffer db ?
	
	public _buffer
	;public _archivo2
	
_datos ends

escritura struc		;estructura para escribir a un archivo
	ba1 dw ?
	su1 dw ?
	ra1 dw ?
	escritura_num dw ?
	escritura_fila dw ?
	escritura_columna dw ?
	escritura_color1 dw ?
	escritura_color2 dw ?
ends

parametros struc

	
	ba dw ?
	su dw ?
	ra dw ?
	
	
	tablero dw 126 dup(?) ;Matriz de 14x9
						 ; si es -1 es rojo
						 ; si es  1 es blanco
						 ; si es  0 es vacio
	numeroDolmensBlancos dw ?
	numeroDolmensRojos   dw ?
	

parametros ends



.MODEL MEDIUM
_pila segment stack 'stack'

    dw 256 dup (?)

_pila ends


_codigo segment

    assume  cs:_codigo, ds:_datos, ss:_pila

	
	_existeNivel proc far
	public _existeNivel
	LOCAL nombre:byte:13,nivel:word=AUTO_SIZE
	push bp 
	mov bp,sp 
	sub sp,AUTO_SIZE 
		lea bx,[nombre]
		mov byte ptr [bx],'b'
		inc bx 
		mov byte ptr [bx],'i'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],'a'
		inc bx 
		mov byte ptr [bx],'c'
		inc bx 
		inc bx 
		inc bx 
		inc bx 
		mov byte ptr [bx],'.'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],'x'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],0
		mov [nivel],0
		
		_leerNivel_inicio:
		lea si,[nombre]
		mov ax,word ptr [nivel]		;obtener el nombre del archivo
		mov bh,10			;para obtener los digitos 
		div bh
		add ah,30h			;poner el caracter ascci de la letra residuo
		mov byte ptr [si+7],ah
		xor ah,ah
		div bh
		add ah,30h		
		;poner el caracter ascci de la letra residuo
		mov [si+6],ah
		xor ah,ah
		div bh
		add ah,30h			;poner el caracter ascci de la letra residuo
		mov [si+5],ah
		xor ah,ah			;ya se tiene el nombre del archivo
		inc [nivel]			;se incrementa el numero de nivel
		cmp word ptr [nivel],1000
		jnae _leerNivel_ContinuarNombre
		mov word ptr [nivel],0			;se reinicia la cuenta
		
		_leerNivel_ContinuarNombre:;el numero de nivel siguiente no es mil, se continua normalmente
		
		mov ah,3dh
		lea dx,[nombre]
		xor al,al
		int 21h					;abrir el archivo
		
		jnc _leerNivel_inicio		;el archivo esta, se pasa al siguiente
		mov ah,3ch 
		lea dx,[nombre]
		mov cx,0 
		int 21h 
		mov bx,ax 
		mov ah,3eh 
		int 21h 
		dec [nivel] 
		mov bx,word ptr [nivel]		;devuelve el numero de archivo que se tiene que crear
		mov ax,bx
		mov sp,bp 
		pop bp
		ret
	_existeNivel endp
	
	
	; _escribeNivel proc far
		; public _escribeNivel
		; LOCAL contx:word,nombre:byte:13,nivel:word=AUTO_SIZE
		; push bp 
		; mov bp,sp 
		; sub sp,AUTO_SIZE 
		; mov ax,[bp].c 
		; mov sp,bp 
		; pop bp
		; ret 2
	; _escribeNivel endp
	
	_pintaNivel1 proc far 
		public _pintaNivel1
		LOCAL buffer:byte:1,nombre:byte:6,contx:word,handle:word=AUTO_SIZE
		push bp
		mov bp,sp
		sub sp,AUTO_SIZE
		lea bx,[nombre]
		mov byte ptr [bx],'t'
		inc bx  
		mov byte ptr [bx],'.'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],'x'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],0
		
		;abrir el archivo 
		mov ah,3dh 
		lea dx,[nombre]
		xor al,al 		;lectura de archivo
		int 21h 
		mov word ptr [handle],ax 
		xor di,di 			;indexa por donde se va 
		mov ax,0b800h 
		mov es,ax 
		
		mov word ptr [contx],0		;numero de posiciones a pintar 
		;mov cx,0 
		xor si,si 
		cicloD:
		cmp word ptr [contx],2000
		jae salidaD
			mov ah,3fh 
			mov bx,word ptr [handle]
			mov cx,1 
			lea dx,_buffer
			int 21h 
			mov ah,0fh 
			mov dl,byte ptr _buffer
			mov al,dl
			
			mov es:[di],ax 
			inc di 
			inc di 
			inc si 
			inc word ptr [contx]
			jmp cicloD
			
			
		
		
		salidaD:
		mov ah,3eh 
		mov bx,word ptr [chandle]
		int 21h 
		mov sp,bp 
		pop bp
		ret 
	_pintaNivel1 endp

	
	_printax proc far
	public _printax
; imprime a la salida estándar un número que supone estar en el AX
; supone que es un número positivo y natural en 16 bits.
; lo imprime en decimal.

    
    push AX
    push BX
    push CX
    push DX

    xor cx, cx
    mov bx, 10
ciclo1PAX: xor dx, dx
    div bx
    push dx
    inc cx
    cmp ax, 0
    jne ciclo1PAX
    mov ah, 02h
ciclo2PAX: pop DX
    add dl, 30h
    int 21h
    loop ciclo2PAX
    pop DX
    pop CX
    pop BX
    pop AX
    ret
_printax endP

_getDI proc far
		public _getDI
		push ax 
		cmp dx,24
		jnb continuarDI
		mov dx,50	;reinicia las columnas
		dec bx		;aumenta las filas 
		continuarDI:
		dec bl 		;decrementa la fila en al que esta 
		mov al,80 
		mul bl		;obtiene la posicion
		add ax,dx	;le suma las columnas
		inc bl 
		dec dx 
		dec dx 
		shl ax,1			;*2
		mov di,ax 
		pop ax 
		ret
	_getDI endp 
	

		_pintaNivel proc far
	public _pintaNivel
	
		push bp 
		
		mov bp,sp 
		mov ax,0b800h
		mov es,ax 
		mov si,200
		mov ax,[bp].numeroDolmensRojos
		mov ah,0fh 
		add al,30h
		mov es:[si],ax 
		mov si,362 
		mov ax,[bp].numeroDolmensBlancos
		mov ah,0fh 
		;add al,30h
		mov es:[si],ax 
		mov si,250 ;0 es el primero que se metio
		mov ax,[bp].tablero[si]
		mov cx,0
		mov di,664 		;posicion de inicio del tablero 
		mov bx,17 
		mov dx,50		;fila y columna 
		cicloTablero:
		cmp cx,126 
		jae finCicloTablero
			mov ax,[bp].tablero[si]
			cmp al,2 		;si esta no activa
			jne cicCont1
			mov ah,19h 
			mov al,'X'
			jmp finTemp
			cicCont1:
			cmp al,0 
			jne cicCont2
			mov ah,2ah 		;pone lo invalido en azul 
			mov al,'0'
			jmp finTemp
			cicCont2:
			cmp al,1 
			jne cicCont3
			mov ah,7fh 		;pone la  posicion en blanco  
			mov al,'B'
			jmp finTemp
			cicCont3:
			mov ah,4ch
			mov al,'R'		;lo pone en rojo 
			finTemp:
			call _getDI		;obtiene el valor para el di, donde se pondra la imagen 
			mov es:[di],ax 
			inc cx 
			dec si 
			dec si			
			jmp cicloTablero 
		finCicloTablero:
		pop bp 
		ret
	_pintaNivel endp
		
	
	_pq proc far
	public _pq
	ARG X:WORD:4
	push bp
	mov bp,sp
	mov bx,[X]
	mov dl,[bp+6]
	add dl,30h
	mov ah,2  
	int 21h 
	mov dl,[bp+8]
	add dl,30h
	mov ah,2 
	int 21h 
	
	;int 21h 
	add bx,1 
	
	mov ah,0fh 
	add al,30h 
	mov dx, 0B800h
	mov es, dx
	mov bx,810
	mov es:[bx],ax
	
	mov sp,bp
	pop bp
	mov ax,6
	ret
	_pq endp
	
	
	_pq1 proc far
public _pq1
	push ss	
	push ax 
	ret
	_pq1 endp
	
	
	
	
	_rayos proc far
	public _rayos
	mov ax,0b800h
	mov es,ax 
	xor di,di 
	mov es:[di],0b35h
	ret 
	_rayos endp
	
		_limpiar proc far		;limpia lo que se puso
		public _limpiar
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f20h		;botar
		inc si 
		inc si 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		mov si,3370
		mov es:[si],0f20h			;muestra la entrada
	    add si,2 
		mov es:[si],0f20h
		mov sp,bp
		pop bp
		ret
	_limpiar endp

	
	_pasar proc far	;se encarga de recibir como entrada si se va a botar o no 
		public _pasar
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f42h		;botar
		inc si 
		inc si 
		mov es:[si],0f6fh
		add si,2 
		mov es:[si],0f74h
		add si,2 
		mov es:[si],0f61h
		add si,2 
		mov es:[si],0f72h
		add si,2 
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f53h		;(s/n)
		add si,2 
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f4eh 
		add si,2 
		mov es:[si],0f29h
		 
		_pasar_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _pasar_ciclo
		xor ah,ah 
		int 16h 
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		or al,00100000b
		cmp al,'x'
		jne _pasar_nox
		mov dx,42 
		jmp _pasar_salida_2
		_pasar_nox:
		cmp al,'s'
		jne _pasar_no
		mov dx,1 
		jmp _pasar_salida
		_pasar_no:
		mov dx,0 
		_pasar_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _pasar_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _pasar_salida_2
			cmp al,13
			jne _pasar_salida
		_pasar_salida_2:
		; mov si,3210
		; mov es:[si],0f20h		;botar
		; inc si 
		; inc si 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; mov si,3370
		; mov es:[si],0f20h			;muestra la entrada
	    ; add si,2 
		; mov es:[si],0f20h
		call _limpiar
		mov ax,dx			;retorna 0 si no, 1 si si
		mov sp,bp
		pop bp
		ret
	_pasar endp 
		
		
	_ttablero proc far			;tipo de tablero
	public _ttablero
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f4dh		;Mundo
		inc si 
		inc si 
		mov es:[si],0f75h
		add si,2 
		mov es:[si],0f6eh
		add si,2 
		mov es:[si],0f64h
		add si,2 
		mov es:[si],0f6fh
		add si,2 
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f56h		;(V/A/S)
		add si,2 
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f41h
		add si,2
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f53h 
		add si,2 
		mov es:[si],0f29h
		_tablero_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _tablero_ciclo
		xor ah,ah 
		int 16h 
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		or al,00100000b
		cmp al,'a'
		jne _tablero_no
		mov dx,2 				;alemania
		jmp _tablero_salida
		_tablero_no:
		cmp al,'v'
		jne _tablero_no2
		mov dx,1			;el vaticano 
		jmp _tablero_salida
		_tablero_no2:
		mov dx,3			;siberia
		jmp _tablero_salida
		_tablero_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _tablero_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _tablero_salida_2
			cmp al,13
			jne _tablero_salida
		_tablero_salida_2:
		
		call _limpiar
		mov ax,dx			;retorna 0 si no, 1 si si
		mov sp,bp
		pop bp
		ret
	_ttablero endp
	
	_ia proc far		;si lo hace la ia o el humano
		public _ia
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f51h		;Quien
		inc si 
		inc si 
		mov es:[si],0f75h
		add si,2 
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f65h
		add si,2 
		mov es:[si],0f6eh
		add si,2 
		mov es:[si],0f50h
		add si,2 
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f49h		;(V/A/S)
		add si,2 
		mov es:[si],0f41h
		add si,2
		mov es:[si],0f28h
		add si,2
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f29h
		add si,2 
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f48h
		add si,2
		mov es:[si],0f28h
		add si,2
		mov es:[si],0f68h
		add si,2 
		mov es:[si],0f29h
		add si,2 
		mov es:[si],0f29h
		_ia_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _ia_ciclo
		xor ah,ah 
		int 16h 
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		or al,00100000b
		cmp al,'x'
		jne _ia_nox
		mov dx,42 
		jmp _ia_salida_2
		_ia_nox:
		cmp al,'i'
		jne _ia_no
		mov dx,1 				;alemania
		jmp _ia_salida
		_ia_no:
		mov dx,0 
		_ia_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _ia_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _ia_salida_2
			cmp al,13
			jne _ia_salida
		_ia_salida_2:
		; mov si,3210
		; mov es:[si],0f20h		;botar
		; inc si 
		; inc si 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; mov si,3370
		; mov es:[si],0f20h			;muestra la entrada
	    ; add si,2 
		; mov es:[si],0f20h
		call _limpiar
		mov ax,dx			;retorna 0 si no, 1 si si
		mov sp,bp
		pop bp
		ret
	_ia endp
	
	
	
	_ai proc far		;si lo hace la ia o el humano
		public _ai
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f51h		;Quien
		inc si 
		inc si 
		mov es:[si],0f75h
		add si,2 
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f65h
		add si,2 
		mov es:[si],0f6eh
		add si,2 
		mov es:[si],0f50h
		add si,2 
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f49h		;(V/A/S)
		add si,2 
		mov es:[si],0f41h
		add si,2
		mov es:[si],0f28h
		add si,2
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f29h
		add si,2 
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f48h
		add si,2
		mov es:[si],0f28h
		add si,2
		mov es:[si],0f68h
		add si,2 
		mov es:[si],0f29h
		add si,2 
		mov es:[si],0f29h
		_ai_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _ai_ciclo
		xor ah,ah 
		int 16h 
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		or al,00100000b
		cmp al,'x'
		jne _ai_nox
		mov dx,42 
		jmp _ai_salida_2
		_ai_nox:
		cmp al,'i'
		jne _ai_no
		mov dx,1 				;alemania
		jmp _ai_salida
		_ai_no:
		mov dx,0 
		_ai_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _ai_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _ai_salida_2
			cmp al,13
			jne _ai_salida
		_ai_salida_2:
		; mov si,3210
		; mov es:[si],0f20h		;botar
		; inc si 
		; inc si 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; mov si,3370
		; mov es:[si],0f20h			;muestra la entrada
	    ; add si,2 
		; mov es:[si],0f20h
		call _limpiar
		mov ax,dx			;retorna 0 si no, 1 si si
		mov sp,bp
		pop bp
		ret
	_ai endp
	
	
	_sab proc far		;si lo hace la ia o el humano
		public _sab
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f51h		;Quien
		inc si 
		inc si 
		mov es:[si],0f75h
		add si,2 
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f65h
		add si,2 
		mov es:[si],0f6eh
		add si,2 
		mov es:[si],0f42h
		add si,2
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f49h		;(V/A/S)
		add si,2 
		mov es:[si],0f41h
		add si,2
		mov es:[si],0f28h
		add si,2
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f29h
		add si,2 
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f48h
		add si,2
		mov es:[si],0f28h
		add si,2
		mov es:[si],0f68h
		add si,2 
		mov es:[si],0f29h
		add si,2 
		mov es:[si],0f29h
		_sab_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _sab_ciclo
		xor ah,ah 
		int 16h 
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		or al,00100000b
		cmp al,'x'
		jne _sab_nox
		mov dx,42 
		jmp _sab_salida_2
		_sab_nox:
		cmp al,'i'
		jne _sab_no
		mov dx,1 				;alemania
		jmp _sab_salida
		_sab_no:
		mov dx,0 
		_sab_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _sab_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _sab_salida_2
			cmp al,13
			jne _sab_salida
		_sab_salida_2:
		; mov si,3210
		; mov es:[si],0f20h		;botar
		; inc si 
		; inc si 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; mov si,3370
		; mov es:[si],0f20h			;muestra la entrada
	    ; add si,2 
		; mov es:[si],0f20h
		call _limpiar
		mov ax,dx			;retorna 0 si no, 1 si si
		mov sp,bp
		pop bp
		ret
	_sab endp
	
	
	
	
	_ta proc far		;si lo hace la ia o el humano
		public _ta
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f44h		;Quien
		inc si 
		inc si 
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f65h
		add si,2 
		mov es:[si],0f6eh
		add si,2 
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f49h		;(V/A/S)
		add si,2 
		mov es:[si],0f41h
		add si,2
		mov es:[si],0f28h
		add si,2
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f29h
		add si,2 
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f48h
		add si,2
		mov es:[si],0f28h
		add si,2
		mov es:[si],0f68h
		add si,2 
		mov es:[si],0f29h
		add si,2 
		mov es:[si],0f29h
		_ta_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _ta_ciclo
		xor ah,ah 
		int 16h 
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		or al,00100000b
		cmp al,'x'
		jne _ta_nox
		mov dx,42 
		jmp _ta_salida_2
		_ta_nox:
		cmp al,'n'
		jne _ta_no
		mov dx,1 				;alemania
		jmp _ta_salida
		_ta_no:
		cmp al,'o'
		jne _ta_no2
		mov dx,2 				;alemania
		jmp _ta_salida
		_ta_no2:
		cmp al,'s'
		jne _ta_no
		mov dx,3 				;alemania
		jmp _ta_salida
		_ta_no3:
		cmp al,'e'
		jne _ta_no4
		mov dx,4 				;alemania
		jmp _ta_salida
		_ta_no4:
		mov dx,0 
		_ta_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _ta_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _ta_salida_2
			cmp al,13
			jne _ta_salida
		_ta_salida_2:
		mov si,3210
		mov es:[si],0f20h		;botar
		inc si 
		inc si 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		mov si,3370
		mov es:[si],0f20h			;muestra la entrada
	    add si,2 
		mov es:[si],0f20h
		;call _limpiar
		mov ax,dx			;retorna 0 si no, 1 si si
		mov sp,bp
		pop bp
		ret
	_ta endp
	
	_color proc far			;color de la pieza a poner
		public _color
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f43h		;color
		inc si 
		inc si 
		mov es:[si],0f6fh
		add si,2 
		mov es:[si],0f6ch
		add si,2 
		mov es:[si],0f6fh
		add si,2 
		mov es:[si],0f72h
		add si,2 
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f42h		;(b/r)
		add si,2 
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f52h 
		add si,2 
		mov es:[si],0f29h
		
		_color_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _color_ciclo
		xor ah,ah 
		int 16h 
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		or al,00100000b
		cmp al,'x'
		jne _color_nox
		mov dx,42 
		jmp _color_salida_2
		_color_nox:
		cmp al,'b'
		jne _color_no
		mov dx,1 
		jmp _color_salida
		_color_no:
		mov dx,0 
		_color_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _color_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _color_salida_2
			cmp al,13
			jne _color_salida
		_color_salida_2:
		; mov si,3210
		; mov es:[si],0f20h		;botar
		; inc si 
		; inc si 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; mov si,3370
		; mov es:[si],0f20h			;muestra la entrada
	    ; add si,2 
		; mov es:[si],0f20h
		call _limpiar
		mov ax,dx			;retorna 0 si r, 1 si b
		mov sp,bp
		pop bp
		ret
	_color endp 
	
	_colorNorte proc far			;color de la pieza a poner
		public _colorNorte
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f43h		;color
		inc si 
		inc si 
		mov es:[si],0f6fh
		add si,2 
		mov es:[si],0f6ch
		add si,2 
		mov es:[si],0f6fh
		add si,2 
		mov es:[si],0f72h
		add si,2 
		mov es:[si],0f4eh 
		add si,2 
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f42h		;(b/r)
		add si,2 
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f52h 
		add si,2 
		mov es:[si],0f29h
		
		_colorNorte_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _colorNorte_ciclo
		xor ah,ah 
		int 16h 
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		or al,00100000b
		cmp al,'x'
		jne _colorNorte_nox
		mov dx,42 
		jmp _colorNorte_salida_2
		_colorNorte_nox:
		cmp al,'b'
		jne _colorNorte_no
		mov dx,1 
		jmp _colorNorte_salida
		_colorNorte_no:
		mov dx,0 
		_colorNorte_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _colorNorte_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _colorNorte_salida_2
			cmp al,13
			jne _colorNorte_salida
		_colorNorte_salida_2:
		; mov si,3210
		; mov es:[si],0f20h		;botar
		; inc si 
		; inc si 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; mov si,3370
		; mov es:[si],0f20h			;muestra la entrada
	    ; add si,2 
		; mov es:[si],0f20h
		call _limpiar
		mov ax,dx			;retorna 0 si r, 1 si b
		mov sp,bp
		pop bp
		ret
	_colorNorte endp
	
	_direccion2 proc far
		public _direccion2
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f44h		;direccion
		add si,2
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f72h
		add si,2 
		mov es:[si],0f65h
		add si,2 
		mov es:[si],0f63h
		add si,2 
		mov es:[si],0f63h
		add si,2 
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f6fh
		add si,2 
		mov es:[si],0f6eh
		add si,2 
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f4eh		;(n/o/s/e)
		add si,2 
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f4fh 
		add si,2 
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f53h 
		add si,2
		mov es:[si],0f2fh
		add si,2 
		mov es:[si],0f45h 
		add si,2
		mov es:[si],0f29h
		_direccion_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _direccion_ciclo
		xor ah,ah 
		int 16h 
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		or al,00100000b
		cmp al,'x'
		jne _direccion_nox
		mov dx,42 
		jmp _direccion_salida_2
		_direccion_nox:
		cmp al,'n'
		jne _direccion_no
		mov dx,1 
		jmp _direccion_salida
		_direccion_no:
		cmp al,'o'
		jne _direccion_no2
		mov dx,2 
		jmp _direccion_salida
		_direccion_no2:
		cmp al,'s'
		jne _direccion_no3
		mov dx,3 
		jmp _direccion_salida
		_direccion_no3:
		cmp al,'e'
		;jne _direccion_no
		mov dx,4 
		jmp _direccion_salida
		
		_direccion_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _direccion_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _direccion_salida_2
			cmp al,13
			jne _direccion_salida
		_direccion_salida_2:
		; mov si,3210
		; mov es:[si],0f20h		;botar
		; inc si 
		; inc si 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2 
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; add si,2
		; mov es:[si],0f20h
		; mov si,3370
		; mov es:[si],0f20h			;muestra la entrada
	    ; add si,2 
		; mov es:[si],0f20h
		call _limpiar
		mov ax,dx			;retorna 0 si r, 1 si b
		mov sp,bp
		pop bp
		ret
	_direccion2 endp
	
	_letra proc far 
	public _letra
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f46h		;Fila
		inc si 
		inc si 
		mov es:[si],0f69h
		add si,2 
		mov es:[si],0f6ch
		add si,2 
		mov es:[si],0f61h
		add si,2 
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f41h		;(A,B,..,I)
		add si,2 
		mov es:[si],0f2ch		;la coma
		add si,2
		mov es:[si],0f42h		
		add si,2 
		mov es:[si],0f2ch		;la coma
		add si,2
		mov es:[si],0f2eh
		add si,2
		mov es:[si],0f2eh
		add si,2 
		mov es:[si],0f2ch
		add si,2
		mov es:[si],0f49h
		add si,2 
		mov es:[si],0f29h
		
		_letra_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _letra_ciclo
		xor ah,ah 
		int 16h 
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		or al,00100000b
		cmp al,'x'
		jne _letra_nox
		mov dx,42 
		jmp _letra_salida_2
		_letra_nox:
		cmp al,'a'
		jne _letra_no
		mov dx,1 
		jmp _letra_salida
		_letra_no:
		cmp al,'b'
		jne _letra_no2
		mov dx,2 
		jmp _letra_salida
		_letra_no2:
		cmp al,'c'
		jne _letra_no3
		mov dx,3 
		jmp _letra_salida
		_letra_no3:
		cmp al,'d'
		jne _letra_no4
		mov dx,4 
		jmp _letra_salida
		_letra_no4:
		cmp al,'e'
		jne _letra_no5
		mov dx,5 
		jmp _letra_salida
		_letra_no5:
		cmp al,'f'
		jne _letra_no6
		mov dx,6 
		jmp _letra_salida
		_letra_no6:
		cmp al,'g'
		jne _letra_no7
		mov dx,7 
		jmp _letra_salida
		_letra_no7:
		cmp al,'h'
		jne _letra_no8
		mov dx,8 
		jmp _letra_salida
		_letra_no8:
		mov dx,9 
		_letra_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _letra_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _letra_salida_2
			cmp al,13
			jne _letra_salida
		_letra_salida_2:
		mov si,3210
		mov es:[si],0f20h		;botar
		inc si 
		inc si 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		mov si,3370
		mov es:[si],0f20h			;muestra la entrada
	    add si,2 
		mov es:[si],0f20h
		;call _limpiar
		mov ax,dx			;retorna 0 si r, 1 si b
		mov sp,bp
		pop bp
		ret
	_letra endp 
	
	
	_numero proc far 
	public _numero
		push bp
		mov bp,sp
		mov ax,0b800h 
		mov es,ax 
		mov si,3210
		mov es:[si],0f43h		;Fila
		add si,2
		mov es:[si],0f6fh
		add si,2 
		mov es:[si],0f6ch
		add si,2 
		mov es:[si],0f75h
		add si,2 
		mov es:[si],0f6dh 
		add si,2 
		mov es:[si],0f6eh 
		add si,2 
		mov es:[si],0f20h 		;espacio
		add si,2 
		mov es:[si],0f28h
		add si,2 
		mov es:[si],0f30h		;(0,2,..,13)
		add si,2 
		mov es:[si],0f2ch		;la coma
		add si,2
		mov es:[si],0f31h		
		add si,2 
		mov es:[si],0f2ch		;la coma
		add si,2
		mov es:[si],0f2eh
		add si,2
		mov es:[si],0f2eh
		add si,2 
		mov es:[si],0f2ch
		add si,2
		mov es:[si],0f31h
		add si,2 
		mov es:[si],0f33h
		add si,2 
		mov es:[si],0f29h
		
		_numero_ciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _numero_ciclo
		xor ah,ah 
		int 16h 
		cmp al,'x'
		jne _numero_nox
		mov dx,42 
		jmp _numero_salida_2
		_numero_nox:
		mov dl,al 
		sub dl,30h 
		
		mov si,3370
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada
		add si,2 
		_numero_preciclo: mov ah, 01   ; revisa si hay una tecla pendiente
		int 16h
		jz _numero_preciclo
		xor ah,ah 
		int 16h 
		cmp al,10
		je _numero_salida_2
		cmp al,13 
		je _numero_salida_2
		mov ah,0fh 
		mov es:[si],ax			;muestra la entrada  
		
		
		mov dh,al 
			sub dh,30h 
			mov al,dl 
			mov dl,10
			mul dl 
			add al,dh 
		xor ah,ah 
			mov dx,ax 
		
		_numero_salida:
			mov ah, 01   ; revisa hasta que se presione enter
			int 16h
			jz _numero_salida
			xor ah,ah 
			int 16h 
			cmp al,10 
			je _numero_salida_2
			cmp al,13
			jne _numero_salida
		_numero_salida_2:
		mov si,3210
		mov es:[si],0f20h		;botar
		inc si 
		inc si 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2 
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		add si,2
		mov es:[si],0f20h
		mov si,3370
		mov es:[si],0f20h			;muestra la entrada
	    add si,2 
		mov es:[si],0f20h
		;call _limpiar
		mov ax,dx			;retorna 0 si r, 1 si b
		mov sp,bp
		pop bp
		ret
	_numero endp 
	
	
	_ayuda proc far 
		public _ayuda
		LOCAL abuffer:byte:1,anombre:byte:6,acontx:word,ahandle:word=AUTO_SIZE
		push bp
		mov bp,sp
		sub sp,AUTO_SIZE
		lea bx,[anombre]
		mov byte ptr [bx],'v'
		inc bx  
		mov byte ptr [bx],'.'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],'x'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],0
		
		;abrir el archivo 
		mov ah,3dh 
		lea dx,[anombre]
		xor al,al 		;lectura de archivo
		int 21h 
		mov word ptr [ahandle],ax 
		xor di,di 			;indexa por donde se va 
		mov ax,0b800h 
		mov es,ax 
		
		mov word ptr [acontx],0		;numero de posiciones a pintar 
		;mov cx,0 
		xor si,si 
		ayudacicloD:
		cmp word ptr [acontx],2000
		jae ayudasalidaD
			mov ah,3fh 
			mov bx,word ptr [ahandle]
			mov cx,1 
			lea dx,_buffer
			int 21h 
			mov ah,0fh 
			mov dl,byte ptr _buffer
			mov al,dl
			
			mov es:[di],ax 
			inc di 
			inc di 
			inc si 
			inc word ptr [acontx]
			jmp ayudacicloD
			
			
		
		
		ayudasalidaD:
		mov ah,3eh 
		mov bx,word ptr [ahandle]
		int 21h 
		mov ah,1 
		int 16h 
		jz ayudasalidaD
		xor ah,ah 
		int 16h
		
		mov sp,bp 
		pop bp
		ret 
	_ayuda endp
	
	_acerca proc far 
		public _acerca
		LOCAL bbuffer:byte:1,bnombre:byte:6,bcontx:word,bhandle:word=AUTO_SIZE
		push bp
		mov bp,sp
		sub sp,AUTO_SIZE
		lea bx,[bnombre]
		mov byte ptr [bx],'u'
		inc bx  
		mov byte ptr [bx],'.'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],'x'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],0
		
		;abrir el archivo 
		mov ah,3dh 
		lea dx,[bnombre]
		xor al,al 		;lectura de archivo
		int 21h 
		mov word ptr [bhandle],ax 
		xor di,di 			;indexa por donde se va 
		mov ax,0b800h 
		mov es,ax 
		
		mov word ptr [bcontx],0		;numero de posiciones a pintar 
		;mov cx,0 
		xor si,si 
		acercacicloD:
		cmp word ptr [bcontx],2000
		jae acercasalidaD
			mov ah,3fh 
			mov bx,word ptr [bhandle]
			mov cx,1 
			lea dx,_buffer
			int 21h 
			mov ah,0fh 
			mov dl,byte ptr _buffer
			mov al,dl
			
			mov es:[di],ax 
			inc di 
			inc di 
			inc si 
			inc word ptr [bcontx]
			jmp acercacicloD
			
			
		
		
		acercasalidaD:
		mov ah,3eh 
		mov bx,word ptr [bhandle]
		int 21h 
		mov ah,1 
		int 16h 
		jz acercasalidaD
		xor ah,ah 
		int 16h
		
		mov sp,bp 
		pop bp
		ret 
	_acerca endp
	
	_opciones proc far 
		public _opciones
		LOCAL cbuffer:byte:1,cnombre:byte:6,ccontx:word,chandle:word=AUTO_SIZE
		push bp
		mov bp,sp
		sub sp,AUTO_SIZE
		lea bx,[cnombre]
		mov byte ptr [bx],'w'
		inc bx  
		mov byte ptr [bx],'.'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],'x'
		inc bx 
		mov byte ptr [bx],'t'
		inc bx 
		mov byte ptr [bx],0
		
		;abrir el archivo 
		mov ah,3dh 
		lea dx,[cnombre]
		xor al,al 		;lectura de archivo
		int 21h 
		mov word ptr [chandle],ax 
		xor di,di 			;indexa por donde se va 
		mov ax,0b800h 
		mov es,ax 
		
		mov word ptr [ccontx],0		;numero de posiciones a pintar 
		;mov cx,0 
		xor si,si 
		opcionescicloD:
		cmp word ptr [ccontx],2000
		jae opcionessalidaD
			mov ah,3fh 
			mov bx,word ptr [chandle]
			mov cx,1 
			lea dx,_buffer
			int 21h 
			mov ah,0fh 
			mov dl,byte ptr _buffer
			mov al,dl
			
			mov es:[di],ax 
			inc di 
			inc di 
			inc si 
			inc word ptr [ccontx]
			jmp opcionescicloD
			
			
		
		
		opcionessalidaD:
		mov ah,3eh 
		mov bx,word ptr [chandle]
		int 21h 
		mov ah,1 
		int 16h 
		jz opcionessalidaD
		xor ah,ah 
		int 16h
		cmp al,31h 
		jne _opciones2
		mov ax,1 
		jmp _opcionesfin
		_opciones2:
		cmp al,32h 
		jne _opciones3
		mov ax,2 
		jmp _opcionesfin
		_opciones3:
		mov ax,3 
		_opcionesfin:			;retorna un uno si es jugar, 2 si es ayuda, 3 si es acercade
		mov sp,bp 
		pop bp
		ret 
	_opciones endp
	
	 
		
	
	
	
 _inicio: 
 mov ax, _datos
         mov ds, ax
         mov ax, _pila
         mov ss, ax	
		
		

         mov ax, 4C00h
         int 21h
     
_codigo ends

end _inicio