#make_bin#

#LOAD_SEGMENT=FFFFh#
#LOAD_OFFSET=0000h#

#CS=0000h#
#IP=0000h#

#DS=0000h#
#ES=0000h#

#SS=0000h#
#SP=FFFEh#

#AX=0000h#
#BX=0000h#
#CX=0000h#
#DX=0000h#
#SI=0000h#
#DI=0000h#
#BP=0000h#

; add your code here
        jmp     st1 
        db     	1022 dup(0)
;IVT end
;Data Segment
		
		ADCA equ 00h		;ADC address bit A = 0
		ADCB equ 01h		;ADC address bit B = 0
		ADCC equ 02h		;ADC address bit C = 0

		Hum_set_min equ 0003h
		Hum_set_max equ 0004h
		Hum_pres equ 0005h
		Temp equ 0006h	
		DegC equ 0007h
		TempBCD equ 0008h
		PercH equ 0009h
		PerBCD equ 000Ah
		HumBCD equ 000Bh
		0FF0h dw ISR_40
;Data Segment end

		prta_55_1 equ 00h		;8255_1 - Humidity & ADCEOC PPI
		prtb_55_1 equ 02h
		prtc_55_1 equ 04h
		creg_55_1 equ 06h

		prta_55_2 equ 08h		;8255_2 - Temperature & LCD PPI
		prtb_55_2 equ 0Ah
		prtc_55_2 equ 0Ch
		creg_55_2 equ 0Eh
		
		cnt0_53 equ 10h			;8253
		cnt1_53 equ 12h
		cnt2_53 equ 14h
		creg_53 equ 16h 
		
		add0_59 equ  18h
		add1_59 equ  1Ah
		
		
		
;main program
          
st1:	cli 
; intialize ds, es,ss to start of RAM
        mov       ax,0200h
        mov       ds,ax
        mov       es,ax
        mov       ss,ax
        mov       sp,0FFFEH
		mov si,ADCA
		mov [si],00000000b
		mov si,ADCB
		mov [si],00000010b
		mov si,ADCC
		mov [si],00000100b
		
;Initializing 8253
		mov al , 00110110b ;initialising counter 0 in mode 3 with count = 5
		out creg_53 , al
		mov al,01110100b
		out creg_53 ,al
		mov al , 10110100b
		out creg_53 ,al
		
		mov al , 05h
		out cnt0_53 , al
		mov al , 00h
		out cnt0_53 , al
		mov al,50h
		out cnt1_53 ,al
		mov al,0C3h
		out cnt1_53 ,al
		mov al,70h
		out cnt2_53,al
		mov al, 17h
		out cnt2_53,al

;8255 Initialize
		mov al,92h	; 8255-1 Humidity & ADCEOC port A,B input    C output
		out creg_55_1,al
		mov al, prta_55_1
		mov dl, al
		mov al,82h	; 8255-2 Temperature & LCD port B input  A,C output  
		out creg_55_2,al 	
		mov al, dl			
		out prtc_55_2,al
;8259 Initialize
		mov al,00010011b
		out add0_59,al
		mov al,01000000b
		out add1_59,al
		mov al,00000001b
		out add1_59,al
		mov al,00000000b
		out add1_59,al
		

;initializing LCD display.
;create delay of more than 100ms after power on
		mov cx, 100
		call DELAY_MS
		   
		mov al,10010010b
		out creg_55_1,al   ;initialising 8255_1
		SEND 30h, 0, 0, 1
		SEND 30h, 0, 0, 5
		SEND 30h, 0, 0, 1
		SEND 38h, 0, 0, 5
		;written the display options.
		SEND 8h, 0, 0, 1
		SEND 1h, 0, 0, 1
		;clear screen and cursor home
		SEND 6, 0, 0, 2
;end of initialisation.
;to turn on display
		SEND 0Fh, 0, 0, 1
		call WORKING_
		
;/////////////////////Macros
;To change Address on ADC
ADCABCTchange MACRO A
		PUSH BX
		PUSH DX
		PUSH CX
		MOV CX, A
		; Change ADCA
		MOV si, ADCA
		mov dl, [si]
		XOR DL, 1
		MOV [si], dl
		;Decide to change ADCB every even address
		mov ax, cx
		mov dl, 2
		div dl
		cmp ah, 0
		jnz donbT
		mov si, ADCB
		mov dl, [si]
		XOR DL, 1
		MOV [si], DL
		;Decide to change ADCC every at count 4
donbT: 	cmp cx, 4
		jnz doncT
		mov si, ADCC
		mov dl, [si]
		XOR DL, 1
		MOV [si], DL	
doncT:
		POP CX
		POP DX
		POP BX
		ENDM

ADCABCHchange MACRO A
		PUSH BX
		PUSH DX
		PUSH CX
		MOV CX, A
		; Change ADCA
		MOV si, ADCA
		mov dl, [si]
		XOR DL, 1
		MOV [si], dl
		;Decide to change ADCB every even address
		mov ax, cx
		mov dl, 2
		div dl
		cmp ah, 0
		jnz donbH
		mov si, ADCB
		mov dl, [si]
		XOR DL, 1
		MOV [si], DL
		;Decide to change ADCC every at count 4
donbH: 	cmp cx, 4
		jnz doncH
		mov si, ADCC
		mov dl, [si]
		XOR DL, 1
		MOV [si], DL	
doncH:
		POP CX
		POP DX
		POP BX
		ENDM
;To send command or data to LCD
SEND MACRO PA_DATA, LCD_E, LCD_RS_RW, DELAY
		PUSH CX
		PUSH DX
		
		in al, prta_55_1
		mov dl, al
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		
		MOV AL, 0
		OR AL, LCD_E				;Temp PC7 - LCD Enable Bit
		mov cl, 1
		ror al, cl
		or al, dl					;PA6 - Relay
		OUT prtc_55_2, AL
		MOV AL, 0
		OR AL, LCD_RS_RW			;Humid PC7 - RS ____ PC6 - R/W
		mov cl, 2
		ror al, cl
		OUT prtc_55_1, AL
		
		MOV AL, 80H					;LCD Enable Bit - Set
		or al, dl
		OUT prtc_55_2, AL
		
		MOV CX, DELAY				;Delay count
		call DELAY_MS
		MOV AL, PA_DATA				;PA_DATA to PORTA
		OUT prta_55_2, AL
		NOP
		NOP							;Small Delay
		
		MOV AL, 0					;LCD Enable Bit - Clear
		or al, dl
		OUT prtc_55_2, AL
		MOV CX, 1				;Delay count
		call DELAY_MS
		POP DX
		POP CX
		ENDM
;//////////////////////End of Macros


;Get Temp values from sensors
		in al, prta_55_1
		mov dl, al
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		mov al,82h			;Re-initialising 8255-2 Temperature & LCD port B input  A,C output  
		out creg_55_2,al
		mov ax,0
		mov al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		mov bx,0
		mov cx,0            ; To get values from 5 sensors
		
X1:		;Initialize ADCT and subsequent Address
		mov si, ADCA
		mov al, [si]		;BSR address A
		out creg_55_2, al
		mov si, ADCB		;BSR address B
		mov al, [si]
		out creg_55_2, al
		mov si, ADCC		;BSR address C
		mov al, [si]
		out creg_55_2, al
		inc cx
		ADCABCTchange cx		

		;ALE - Address Latch enable
		mov al, 00001011b	;PC5 - ALE = 1
		out creg_55_2, al
		
		mov al, 00001001b	;PC4 = Start = 1 pulse
		out creg_55_2, al
		NOP					;Delay of 3 Machine cycles = 3*200ns

		mov al, 00001010b	;PC5 - ALE = 0
		out creg_55_2, al
		
		mov al, 00001000b	;PC4 = Start = 0 pulse
		out creg_55_2, al
	
		in al, prta_55_1
		mov dl, al
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		mov al,82h			; Re-initialising 8255-2 Temp and LCD port B input    A, C output
		out creg_55_2,al
		mov al, 0
		or al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		
Y2:		;Wait for EOC
		in al, prta_55_1		;PA0 of 8255 1 is ADCTEOC					
		and al, 1				;Mask all but PA0
		cmp al, 1				;if EOC
		jnz Y2
		
		;Read Value from ADCT			
		in al, prta_55_1
		mov dl, al
		mov al, 8			;PC3 = ADC Output Enable = 1 
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		or al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		
		in al, prta_55_1
		mov dl, al
		mov al, 0			;PC3 = ADC Output Enable = 0 
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		or al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		
		in al, prta_55_1
		mov dl, al
		mov al, 8			;PC3 = ADC Output Enable = 1 
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		or al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		
		in al,prtb_55_2		;Value from ADCT stored in PORTB 8255_Temp
		mov ah, 0
		add bx,ax			;Sensor values aggregated

		;Output Disable
		in al, prta_55_1
		mov dl, al
		mov al, 0			;PC3 = ADC Output Disable = 0
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		or al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		
		cmp cx, 4			;Check whether 5 sensors read
		jnz X1

		;//////dividing by 5///////
		mov ax,bx            
		mov bl,4
		div bl
	 	mov si, Temp
	 	mov [si], al                         ; register al has final value - Stored in Temp
		
;Resetting ADC addresses
		MOV DL, 00000000b
		mov si, ADCA
		MOV [si], DL	
		MOV DL, 00000010b
		mov si, ADCB
		MOV [si], DL	
		MOV DL, 00000100b
		mov si, ADCC
		MOV [si], DL	
		
;Set Hum_set_min and  Hum_set_max according to Temp
		
		;LMT85 Vout is inversely proportional to Temp
		;Greater Temp ==> Lower Voltage ==> Lower 8-bit value
		;Humidity 30.68mv/%RH and 0% = 0.958V
		mov si, Temp        ;Check value of Temp stored
		mov al, [si]
		mov ah, 0
		cmp ax, 004Eh			;Temp = 5 degC = 1.527V
		jl	t1
		mov si, Hum_set_min
		mov [si], 5Ch		;RH 27.5%	=	1.8023V
		mov si, Hum_set_max
		mov [si], 60h		;RH 30%	=	1.879V
		jmp rangeset
		
t1:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 004Ch			;Temp = 10 degC = 1.486V
		jl	t2
		mov si, Hum_set_min
		mov [si], 60h		;RH 30%	=	1.879V
		mov si, Hum_set_max
		mov [si], 64h		;RH 32.5% =	1.9557V
		jmp rangeset
		
t2:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 004Ah			;Temp = 15 degC = 1.446V
		jl	t3
		mov si, Hum_set_min
		mov [si], 64h		;RH 32.5% =	1.9557V
		mov si, Hum_set_max
		mov [si], 68h		;RH 35%	=	2.0325V
		jmp rangeset
		
t3:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 0048h			;Temp = 20 degC = 1.405V
		jl	t4
		mov si, Hum_set_min
		mov [si], 68h		;RH 35% =	2.0325V
		mov si, Hum_set_max
		mov [si], 6Ch		;RH 38% =	2.1246V
		jmp rangeset
		
t4:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 0046h			;Temp = 25 degC = 1.365V
		jl	t5
		mov si, Hum_set_min
		mov [si], 6Ch		;RH 38% =	2.1246V
		mov si, Hum_set_max
		mov [si], 70h		;RH 40% =	2.186V
		jmp rangeset
		
t5:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 0043h			;Temp = 30 degC = 1.324V
		jl	t6
		mov si, Hum_set_min
		mov [si], 70h		;RH 40% =	2.186V
		mov si, Hum_set_max
		mov [si], 74h		;RH 43% =	2.2781V
		jmp rangeset
		
t6:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 0041h			;Temp = 35 degC = 1.283V
		jl	t7
		mov si, Hum_set_min
		mov [si], 74h		;RH 43% =	2.2781V
		mov si, Hum_set_max
		mov [si], 78h		;RH 45% =	2.3386V
		jmp rangeset

t7:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 003Dh			;Temp = 45 degC = 1.201V
		jl	t8
		mov si, Hum_set_min
		mov [si], 78h		;RH 45% =	2.3386V
		mov si, Hum_set_max
		mov [si], 7Ch		;RH 48%	=	2.4316V
		jmp rangeset

t8:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 0039h			;Temp = 55 degC = 1.118V
		jl	t9
		mov si, Hum_set_min
		mov [si], 7Ch		;RH 48% =	2.4316V
		mov si, Hum_set_max
		mov [si], 80h		;RH 50%	=	2.493V
		jmp rangeset
		
t9:		;Temp greater than 55degC 
		mov si, Hum_set_min
		mov [si], 80h		;RH 50%	=	2.493V
		mov si, Hum_set_max
		mov [si], 84h		;RH 53% =	2.5851V

rangeset: 
;////////////////Humidity!!!!
;Get Humidity values from sensors	
		mov bx,0
		mov cx,0            ; To get values from 7 sensors
		mov al,92h			; Re-initialising 8255-1 Humidity & ADCEOC port A,B input    C output
		out creg_55_1,al
		
X2:		;Initialize ADCH and subsequent Address
		mov si, ADCA
		mov al, [si]		;BSR address A
		out creg_55_1, al
		mov si, ADCB		;BSR address B
		mov al, [si]
		out creg_55_1, al
		mov si, ADCC		;BSR address C
		mov al, [si]
		out creg_55_1, al
		inc cx
		ADCABCHchange cx		
		
		;ALE - Address Latch enable
		mov al, 00001011b	;PC5 - ALE = 1
		out creg_55_1, al
		
		mov al, 00001001b	;PC4 = Start = 1 pulse
		out creg_55_1, al
		NOP					;Delay of 3 Machine cycles = 3*200ns

		mov al, 00001010b	;PC5 - ALE = 0
		out creg_55_1, al
		
		mov al, 00001000b	;PC4 = Start = 0 pulse
		out creg_55_1, al
	

		mov al,92h			; Re-initialising 8255-1 Humidity & ADCEOC port A,B input    C output
		out creg_55_1,al
		mov al, 0
		out prtc_55_1, al
		
Y4:		;Wait for EOC
		in al, prta_55_1		;PA1 of 8255 1 is ADCHEOC					
		and al, 2				;Mask all but PA1
		cmp al, 2				;if EOC
		jnz Y4
		
		
		;Read Value from ADCT		
		mov al, 8h			;PC3 = ADC Output Enable = 1 
		out prtc_55_1, al

		mov al, 0h			;PC3 = ADC Output Enable = 0 
		out prtc_55_1, al
		NOP

		mov al, 8h			;PC3 = ADC Output Enable = 1 
		out prtc_55_1, al
		
		in al,prtb_55_1		;Value from ADCH stored in PORTB 8255_Hum
		mov ah, 0
		add bx,ax			;Sensor values aggregated
		
		;Output Disable
		mov al, 0h			; PC3 = ADC Output Disable = 0
		out prtc_55_1, al
		
		cmp cx, 4			;Check whether 7 sensors read
		jnz X2
		;//////dividing by 7///////
		mov ax,bx            
		mov bl,4
		div bl
	 	mov si, Hum_pres
	 	mov [si], al                         ; register al has final value - Stored in Hum_pres
		
;Resetting ADC addresses
		MOV DL, 00000000b
		mov si, ADCA
		MOV [si], DL	
		MOV DL, 00000010b
		mov si, ADCB
		MOV [si], DL	
		MOV DL, 00000100b
		mov si, ADCC
		MOV [si], DL	

;Glow LED if Hum_pres < Hum_set_min
		mov ax, 0
		mov bx, 0
		mov si, Hum_pres        ;Check value of Hum_pres stored
		mov al, [si]
		mov si, Hum_set_min
		mov bl, [si]
		cmp ax, bx
		jl	z2
		mov al,82h			;Re-initialising 8255-2 Temperature & LCD port B input  A,C output  
		out creg_55_2,al
		mov al, 0			;PC6 = 0 = Humid ON
		out prtc_55_2, al
		jmp displaying 
		
z2:;Stop LED if Hum_pres > Hum_set_max
		mov ax, 0
		mov bx, 0
		mov si, Hum_pres        ;Check value of Hum_pres stored
		mov al, [si]
		mov si, Hum_set_max
		mov bl, [si]
		cmp ax, bx
		jg	displaying
		mov al,82h			;Re-initialising 8255-2 Temperature & LCD port B input  A,C output  
		out creg_55_2,al
		mov al, 40h			;PC6 = 1 = Humid OFF
		out prtc_55_2, al

displaying:
		CALL DISPLAYING_
ISR_40:
		
		ADCABCT1change MACRO A
		PUSH BX
		PUSH DX
		PUSH CX
		MOV CX, A
		; Change ADCA
		MOV si, ADCA
		mov dl, [si]
		XOR DL, 1
		MOV [si], dl
		;Decide to change ADCB every even address
		mov ax, cx
		mov dl, 2
		div dl
		cmp ah, 0
		jnz donbT1
		mov si, ADCB
		mov dl, [si]
		XOR DL, 1
		MOV [si], DL
		;Decide to change ADCC every at count 4
donbT1: 	cmp cx, 4
		jnz doncT1
		mov si, ADCC
		mov dl, [si]
		XOR DL, 1
		MOV [si], DL	
doncT1:
		POP CX
		POP DX
		POP BX
		ENDM

ADCABCHchange1 MACRO A
		PUSH BX
		PUSH DX
		PUSH CX
		MOV CX, A
		; Change ADCA
		MOV si, ADCA
		mov dl, [si]
		XOR DL, 1
		MOV [si], dl
		;Decide to change ADCB every even address
		mov ax, cx
		mov dl, 2
		div dl
		cmp ah, 0
		jnz donbH1
		mov si, ADCB
		mov dl, [si]
		XOR DL, 1
		MOV [si], DL
		;Decide to change ADCC every at count 4
donbH1: 	cmp cx, 4
		jnz doncH1
		mov si, ADCC
		mov dl, [si]
		XOR DL, 1
		MOV [si], DL	
doncH1:
		POP CX
		POP DX
		POP BX
		ENDM
;To send command or data to LCD
SEND1 MACRO PA_DATA, LCD_E, LCD_RS_RW, DELAY
		PUSH CX
		PUSH DX
		
		in al, prta_55_1
		mov dl, al
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		
		MOV AL, 0
		OR AL, LCD_E				;Temp PC7 - LCD Enable Bit
		mov cl, 1
		ror al, cl
		or al, dl					;PA6 - Relay
		OUT prtc_55_2, AL
		MOV AL, 0
		OR AL, LCD_RS_RW			;Humid PC7 - RS ____ PC6 - R/W
		mov cl, 2
		ror al, cl
		OUT prtc_55_1, AL
		
		MOV AL, 80H					;LCD Enable Bit - Set
		or al, dl
		OUT prtc_55_2, AL
		
		MOV CX, DELAY				;Delay count
		call DELAY_MS
		MOV AL, PA_DATA				;PA_DATA to PORTA
		OUT prta_55_2, AL
		NOP
		NOP							;Small Delay
		
		MOV AL, 0					;LCD Enable Bit - Clear
		or al, dl
		OUT prtc_55_2, AL
		MOV CX, 1				;Delay count
		call DELAY_MS
		POP DX
		POP CX
		ENDM
;//////////////////////End of Macros


;Get Temp values from sensors
		in al, prta_55_1
		mov dl, al
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		mov al,82h			;Re-initialising 8255-2 Temperature & LCD port B input  A,C output  
		out creg_55_2,al
		mov ax,0
		mov al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		mov bx,0
		mov cx,0            ; To get values from 5 sensors
		
X11:		;Initialize ADCT and subsequent Address
		mov si, ADCA
		mov al, [si]		;BSR address A
		out creg_55_2, al
		mov si, ADCB		;BSR address B
		mov al, [si]
		out creg_55_2, al
		mov si, ADCC		;BSR address C
		mov al, [si]
		out creg_55_2, al
		inc cx
		ADCABCT1change cx		

		;ALE - Address Latch enable
		mov al, 00001011b	;PC5 - ALE = 1
		out creg_55_2, al
		
		mov al, 00001001b	;PC4 = Start = 1 pulse
		out creg_55_2, al
		NOP					;Delay of 3 Machine cycles = 3*200ns

		mov al, 00001010b	;PC5 - ALE = 0
		out creg_55_2, al
		
		mov al, 00001000b	;PC4 = Start = 0 pulse
		out creg_55_2, al
	
		in al, prta_55_1
		mov dl, al
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		mov al,82h			; Re-initialising 8255-2 Temp and LCD port B input    A, C output
		out creg_55_2,al
		mov al, 0
		or al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		
Y21:		;Wait for EOC
		in al, prta_55_1		;PA0 of 8255 1 is ADCTEOC					
		and al, 1				;Mask all but PA0
		cmp al, 1				;if EOC
		jnz Y21
		
		;Read Value from ADCT			
		in al, prta_55_1
		mov dl, al
		mov al, 8			;PC3 = ADC Output Enable = 1 
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		or al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		
		in al, prta_55_1
		mov dl, al
		mov al, 0			;PC3 = ADC Output Enable = 0 
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		or al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		
		in al, prta_55_1
		mov dl, al
		mov al, 8			;PC3 = ADC Output Enable = 1 
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		or al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		
		in al,prtb_55_2		;Value from ADCT stored in PORTB 8255_Temp
		mov ah, 0
		add bx,ax			;Sensor values aggregated

		;Output Disable
		in al, prta_55_1
		mov dl, al
		mov al, 0			;PC3 = ADC Output Disable = 0
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		or al, dl			;Reset Port C of Temp ADC
		out prtc_55_2, al
		
		cmp cx, 4			;Check whether 5 sensors read
		jnz X11

		;//////dividing by 5///////
		mov ax,bx            
		mov bl,4
		div bl
	 	mov si, Temp
	 	mov [si], al                         ; register al has final value - Stored in Temp
		
;Resetting ADC addresses
		MOV DL, 00000000b
		mov si, ADCA
		MOV [si], DL	
		MOV DL, 00000010b
		mov si, ADCB
		MOV [si], DL	
		MOV DL, 00000100b
		mov si, ADCC
		MOV [si], DL	
		
;Set Hum_set_min and  Hum_set_max according to Temp
		
		;LMT85 Vout is inversely proportional to Temp
		;Greater Temp ==> Lower Voltage ==> Lower 8-bit value
		;Humidity 30.68mv/%RH and 0% = 0.958V
		mov si, Temp        ;Check value of Temp stored
		mov al, [si]
		mov ah, 0
		cmp ax, 004Eh			;Temp = 5 degC = 1.527V
		jl	t11
		mov si, Hum_set_min
		mov [si], 5Ch		;RH 27.5%	=	1.8023V
		mov si, Hum_set_max
		mov [si], 60h		;RH 30%	=	1.879V
		jmp rangeset1
		
t11:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 004Ch			;Temp = 10 degC = 1.486V
		jl	t2
		mov si, Hum_set_min
		mov [si], 60h		;RH 30%	=	1.879V
		mov si, Hum_set_max
		mov [si], 64h		;RH 32.5% =	1.9557V
		jmp rangeset1
		
t21:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 004Ah			;Temp = 15 degC = 1.446V
		jl	t31
		mov si, Hum_set_min
		mov [si], 64h		;RH 32.5% =	1.9557V
		mov si, Hum_set_max
		mov [si], 68h		;RH 35%	=	2.0325V
		jmp rangeset1
		
t31:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 0048h			;Temp = 20 degC = 1.405V
		jl	t41
		mov si, Hum_set_min
		mov [si], 68h		;RH 35% =	2.0325V
		mov si, Hum_set_max
		mov [si], 6Ch		;RH 38% =	2.1246V
		jmp rangeset1
		
t41:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 0046h			;Temp = 25 degC = 1.365V
		jl	t51
		mov si, Hum_set_min
		mov [si], 6Ch		;RH 38% =	2.1246V
		mov si, Hum_set_max
		mov [si], 70h		;RH 40% =	2.186V
		jmp rangeset1
		
t51:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 0043h			;Temp = 30 degC = 1.324V
		jl	t61
		mov si, Hum_set_min
		mov [si], 70h		;RH 40% =	2.186V
		mov si, Hum_set_max
		mov [si], 74h		;RH 43% =	2.2781V
		jmp rangeset1
		
t61:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 0041h			;Temp = 35 degC = 1.283V
		jl	t71
		mov si, Hum_set_min
		mov [si], 74h		;RH 43% =	2.2781V
		mov si, Hum_set_max
		mov [si], 78h		;RH 45% =	2.3386V
		jmp rangeset1

t71:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 003Dh			;Temp = 45 degC = 1.201V
		jl	t81
		mov si, Hum_set_min
		mov [si], 78h		;RH 45% =	2.3386V
		mov si, Hum_set_max
		mov [si], 7Ch		;RH 48%	=	2.4316V
		jmp rangeset1

t81:		mov si, Temp
		mov al, [si]
		mov ah, 0
		cmp ax, 0039h			;Temp = 55 degC = 1.118V
		jl	t91
		mov si, Hum_set_min
		mov [si], 7Ch		;RH 48% =	2.4316V
		mov si, Hum_set_max
		mov [si], 80h		;RH 50%	=	2.493V
		jmp rangeset1
		
t91:		;Temp greater than 55degC 
		mov si, Hum_set_min
		mov [si], 80h		;RH 50%	=	2.493V
		mov si, Hum_set_max
		mov [si], 84h		;RH 53% =	2.5851V

rangeset1: 
;////////////////Humidity!!!!
;Get Humidity values from sensors	
		mov bx,0
		mov cx,0            ; To get values from 7 sensors
		mov al,92h			; Re-initialising 8255-1 Humidity & ADCEOC port A,B input    C output
		out creg_55_1,al
		
X21:		;Initialize ADCH and subsequent Address
		mov si, ADCA
		mov al, [si]		;BSR address A
		out creg_55_1, al
		mov si, ADCB		;BSR address B
		mov al, [si]
		out creg_55_1, al
		mov si, ADCC		;BSR address C
		mov al, [si]
		out creg_55_1, al
		inc cx
		ADCABCHchange1 cx		
		
		;ALE - Address Latch enable
		mov al, 00001011b	;PC5 - ALE = 1
		out creg_55_1, al
		
		mov al, 00001001b	;PC4 = Start = 1 pulse
		out creg_55_1, al
		NOP					;Delay of 3 Machine cycles = 3*200ns

		mov al, 00001010b	;PC5 - ALE = 0
		out creg_55_1, al
		
		mov al, 00001000b	;PC4 = Start = 0 pulse
		out creg_55_1, al
	

		mov al,92h			; Re-initialising 8255-1 Humidity & ADCEOC port A,B input    C output
		out creg_55_1,al
		mov al, 0
		out prtc_55_1, al
		
Y41:		;Wait for EOC
		in al, prta_55_1		;PA1 of 8255 1 is ADCHEOC					
		and al, 2				;Mask all but PA1
		cmp al, 2				;if EOC
		jnz Y41
		
		
		;Read Value from ADCT		
		mov al, 8h			;PC3 = ADC Output Enable = 1 
		out prtc_55_1, al

		mov al, 0h			;PC3 = ADC Output Enable = 0 
		out prtc_55_1, al
		NOP

		mov al, 8h			;PC3 = ADC Output Enable = 1 
		out prtc_55_1, al
		
		in al,prtb_55_1		;Value from ADCH stored in PORTB 8255_Hum
		mov ah, 0
		add bx,ax			;Sensor values aggregated
		
		;Output Disable
		mov al, 0h			; PC3 = ADC Output Disable = 0
		out prtc_55_1, al
		
		cmp cx, 4			;Check whether 7 sensors read
		jnz X21
		;//////dividing by 7///////
		mov ax,bx            
		mov bl,4
		div bl
	 	mov si, Hum_pres
	 	mov [si], al                         ; register al has final value - Stored in Hum_pres
		
;Resetting ADC addresses
		MOV DL, 00000000b
		mov si, ADCA
		MOV [si], DL	
		MOV DL, 00000010b
		mov si, ADCB
		MOV [si], DL	
		MOV DL, 00000100b
		mov si, ADCC
		MOV [si], DL	

;Glow LED if Hum_pres < Hum_set_min
		mov ax, 0
		mov bx, 0
		mov si, Hum_pres        ;Check value of Hum_pres stored
		mov al, [si]
		mov si, Hum_set_min
		mov bl, [si]
		cmp ax, bx
		jl	z21
		mov al,82h			;Re-initialising 8255-2 Temperature & LCD port B input  A,C output  
		out creg_55_2,al
		mov al, 0			;PC6 = 0 = Humid ON
		out prtc_55_2, al
		jmp displaying1 
		
z21:;Stop LED if Hum_pres > Hum_set_max
		mov ax, 0
		mov bx, 0
		mov si, Hum_pres        ;Check value of Hum_pres stored
		mov al, [si]
		mov si, Hum_set_max
		mov bl, [si]
		cmp ax, bx
		jg	displaying1
		mov al,82h			;Re-initialising 8255-2 Temperature & LCD port B input  A,C output  
		out creg_55_2,al
		mov al, 40h			;PC6 = 1 = Humid OFF
		out prtc_55_2, al

displaying1:
		CALL DISPLAYING_
		mov cx,0FFE0H
		call DELAY_MS
		mov al,00100000b
		
		
		out add0_59,al
		IRET
		
		
		
		
		
		
		

		


;/////////////////////////PROCS
DELAY_1_MS PROC NEAR
		PUSH CX
		MOV CX, 273
delay1:	LOOP delay1		
		POP CX
		RET
DELAY_1_MS ENDP

DELAY_MS PROC NEAR
		;CX has MS Delay
delay:	CALL DELAY_1_MS
		LOOP delay
		RET
DELAY_MS ENDP      

WRITE PROC NEAR
		;WRITE ASCII 
		push cx
		push dx
		
		mov cx,1
		call DELAY_MS
		
		in al, prta_55_1
		mov dl, al
		and dl, 40h			;RelayH -> PA6 - 8255H _ 1
		
		mov al, 0H
		or al, dl
		out prtc_55_2, al		;Disable LCD
		mov cx, 1
		call DELAY_MS
		
		
		MOV AL, 80H
		out prtc_55_1,al  		;writing Rs and Rw
		NOP
		
		mov al, 80H
		or al, dl
		out prtc_55_2, al		;Enable LCD
		mov cx, 1
		call DELAY_MS
		
		
		mov al,bl				;ASCII value in BL
		out prta_55_2,al 
		NOP
		NOP
		mov al, 0H
		or al, dl
		out prtc_55_2, al		;Disable LCD
		mov cx, 1
		call DELAY_MS
		
		pop dx
		pop cx
		RET
WRITE ENDP

CLS PROC NEAR
		SEND 1, 0, 0, 2		;Send 1 for 2 ms
		RET
CLS ENDP

WORKING_ PROC NEAR
		call CLS
		MOV BL, 'S'
		CALL WRITE
		MOV BL, 'e'
		CALL WRITE
		MOV BL, 'n'
		CALL WRITE
		MOV BL, 's'
		CALL WRITE
		MOV BL, 'i'
		CALL WRITE
		MOV BL, 'n'
		CALL WRITE
		MOV BL, 'g'
		CALL WRITE
		MOV BL, '.'
		CALL WRITE
		MOV BL, '.'
		CALL WRITE
		MOV BL, '.'
		CALL WRITE
		RET
WORKING_ ENDP

DISPLAYING_ PROC NEAR
		
;Finding Value of Temp in degC
;T = 162*(80-Temp)/(68)
	 	mov si, Temp
	 	mov al, [si]                         ; register al has final value - Stored in Temp
		mov bl, 80
		sub bl, al			;bl = 80 - Temp
		mov al, bl			;al = 80 - Temp
		mov bl, 162
		mul bl				;ax = 162*(80-Temp)
		mov bl, 68
		div bl				;al = 162*(80-Temp)/68
		mov bl, 0
		add bl, al			;bl = 162*(80-Temp)/68
		mov si, DegC		;Temp in degC stored in DegC
		mov [si], bl
		
;ASCII written to LCD
;1 - Converting Decimal to BCD
;BCD = Dec + 6*Dec/10
		mov al, bl
		mov ah, 0
		mov bl, 10
		div bl
		mov bl, al
		mov al, 6
		mov ah, 0
		mul bl
		mov si, DegC
		mov bl, [si]
		add bl, al
		mov si, TempBCD
		mov [si], bl
;Storing BCD value		
		
;Writing to LCD
		CALL CLS
		MOV BL, 'T'
		CALL WRITE
		MOV BL, ':'
		CALL WRITE
		mov si, TempBCD
		mov bl, [si]
		and bl, 0F0h		;Ten's Character
		mov cl, 4
		ror bl, cl
		add bl, 48			;ASCII value of Ten's character
		CALL WRITE
		
		mov si, TempBCD
		mov bl, [si]
		and bl, 0Fh		;Unit's Character
		add bl, 48		;ASCII value of Unit's character
		CALL WRITE
		
		mov bl, 0DFh	;ASCII of Deg symbol
		CALL WRITE                       
		mov bl, 'C'     ;ASCII of 'C'
		CALL WRITE
		mov cx, 4
space:	mov bl, 20h		;ASCII of Space
		CALL WRITE
		dec cx
		jnz space
;Temp written to LCD
;Finding Value of Humidity in %
;Rh = (Hum_pres-49)*75/118
		mov si, Hum_pres
	 	mov al, [si]                         ; register al has final value - Stored in Hum_pres
		mov bl, 49
		sub al, bl			;al = Hum_pres - 49
		mov bl, 75
		mul bl				;ax = 75*(Hum_pres - 49)
		mov bl, 118
		div bl				;al = 75*(Hum_pres - 49)/118
		mov si, PercH		;Humidity in % stored in PercH
		mov [si], al
;ASCII written to LCD
;1 - Converting Decimal to BCD
;BCD = Dec + 6*Dec/10
		mov ah, 0
		mov bl, 10
		div bl
		mov bl, al
		mov al, 6
		mov ah, 0
		mul bl
		mov si, PercH
		mov bl, [si]
		add bl, al
		mov si, HumBCD
		mov [si], bl
;Storing BCD value		
;Writing to LCD
		mov bl, 'R'
		CALL WRITE
		mov bl, 'H'
		CALL WRITE
		mov bl, '-'
		CALL WRITE
		mov si, HumBCD
		mov bl, [si]
		and bl, 0F0h		;Ten's Character
		mov cl, 4
		ror bl, cl
		add bl, 48			;ASCII value of Ten's number
		CALL WRITE
		mov si, HumBCD
		mov bl, [si]
		and bl, 0Fh		;Unit's Character
		add bl, 48		;ASCII value of Unit's number
		CALL WRITE
		mov bl, 25h		;ASCII of % symbol
		CALL WRITE
		mov cx, 500
		call DELAY_MS
		RET
;Humidity written to LCD	
DISPLAYING_ ENDP
;////////////////////END OF PROCS