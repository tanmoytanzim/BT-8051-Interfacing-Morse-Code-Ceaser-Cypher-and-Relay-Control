
; DATA area
;----------------------------------------------------------------
                ;DATA    30h, 31h
pattern         EQU     30h       ; 8-bit shift-accumulator
length          EQU     31h       ; 1–5 symbols received
LCD_DATA        EQU     P1
;----------------------------------------------------------------
; VECTOR table
;----------------------------------------------------------------
                ORG     0000h
                LJMP    START
                ORG     0023h    ; Serial interrupt vector
ISR_SERIAL:    
		
                CLR     TI
                CLR     RI        ; clear TX flag (if you send in ISR)
                MOV     A, SBUF
                ;ACALL PUT_CHAR   	; get Rx’d ASCII char
                MOV     R3, A     ; save for later

    ;—— handle “.” —————————————————————————————
                CJNE    A, #'.', CHECK_DASH
                MOV P0, 50H
		ACALL DELAY500MS
		MOV P0, #0
                ACALL PUT_CHAR
                LCALL BUZZ
                CLR     C
                MOV     A, pattern
                RLC     A
                MOV     pattern, A
                INC     length
                LJMP    ISR_EXIT


    ;—— handle “–” —————————————————————————————
CHECK_DASH:     CJNE    A, #'-', CHECK_TERM
		MOV P0, 50h
		ACALL DELAY1S
		MOV P0, #0
		ACALL PUT_CHAR
		LCALL BUZZ
                SETB    C
                MOV     A, pattern
                RLC     A
                MOV     pattern, A
                INC     length
                LJMP    ISR_EXIT

    ;—— handle terminators ————————————————————————
     ; space = 20h, CR = 0Dh, LF = 0Ah
CHECK_TERM:     CJNE    A, #' ', NOT_CR
                ACALL PUT_CHAR
                
                ACALL   DECODE_LETTER
                ;MOV     A, #' '
                ;ACALL   PUT_CHAR
                LJMP    ISR_EXIT
NOT_CR:         CJNE    A, #0Dh, CHECK_END
                ACALL   DECODE_LETTER
                LJMP    ISR_EXIT
CHECK_END:      CJNE A, #'0', PROCESS_RELAY ;PROCESS_CYPHER
		ACALL LINE
		LJMP ISR_EXIT
		

PROCESS_RELAY:  ;Input 3-both LEDS, 1,2- one LED

		CJNE A,#'3',NOT_ALL   
        	SETB P2.0
        	SETB P2.1           
        	LJMP ISR_EXIT

NOT_ALL:  	CLR P2.0
       		CLR P2.1
	  	SJMP CHECK_1ST
CHECK_1ST: 	CJNE A,#'1',CHECK_2ND
		SETB P2.0
		CLR P2.1
		LJMP ISR_EXIT
CHECK_2ND: 	CJNE A,#'2', PROCESS_MOTOR
		SETB P2.1
		CLR P2.0
		LJMP ISR_EXIT            ; Otherwise, turn OFF LED

PROCESS_MOTOR: ;Input 4 for slow spin, 5 for fast spin
        	CJNE A, #'4', FAST  ; Check if '4' (slow)
SLOW: 		MOV R4,#20
        	SJMP ROTATE

FAST: 		CJNE A, #'5', NEXT   ; Check if '5' (fast)
		MOV R4,#10
        	SJMP ROTATE

ROTATE:
        ; Step 1
        MOV A, #08H
        ACALL OUTPORT
        LCALL DELAY1

        ; Step 2
        MOV A, #018H
        ACALL OUTPORT
        LCALL DELAY1

        ; Step 3
        MOV A, #010H
        ACALL OUTPORT
        LCALL DELAY1

        ; Step 4
        MOV A, #030H
        ACALL OUTPORT
        LCALL DELAY1

        ; Step 5
        MOV A, #020H
        ACALL OUTPORT
        LCALL DELAY1

        ; Step 6
        MOV A, #060H
        ACALL OUTPORT
        LCALL DELAY1

        ; Step 7
        MOV A, #040H
        ACALL OUTPORT
        LCALL DELAY1

        ; Step 8
        MOV A, #048H
        ACALL OUTPORT
        LCALL DELAY1

; Send value to Port 2
OUTPORT:          
        MOV P2, A
        SJMP ISR_EXIT

NEXT:   ;CLR RI
        SJMP PROCESS_CIPHER
        
PROCESS_CIPHER:
 	
 		CJNE  A, #'A', PC_NOT_A
        	MOV   A, #'Z'
        	ACALL PUT_CHAR
        	ACALL SAVE_CHAR
        	SJMP  ISR_EXIT

PC_NOT_A:
    ; — if below 'A' or above 'Z', treat as terminator
    CLR   C
    SUBB  A, #'A'
    JC    C_CHECK_TERM     ; A < 'A'
    CLR   C
    SUBB  A, #'Z'
    JNC   C_CHECK_TERM     ; A > 'Z'

    ; — now it's B…Z, do the -1 shift
    MOV   A, R3
    CLR   C
    SUBB  A, #1
    ACALL PUT_CHAR
    ACALL SAVE_CHAR
    SJMP  ISR_EXIT

C_CHECK_TERM:
    MOV   A, R3
    CJNE  A, #' ', C_NOT_CR
        ACALL PUT_CHAR
        ACALL LINE
        SJMP ISR_EXIT
C_NOT_CR:
    CJNE  A, #0Dh, ISR_EXIT
    ACALL LINE
    SJMP ISR_EXIT		


ISR_EXIT:      
                RETI

;----------------------------------------------------------------
; DECODE_LETTER:
;    if length=0 ? RET
;    else idx = lengthOffset[length] + pattern
;         A = decodeTable[idx]
;         PUT_CHAR(A)
;         clear pattern & length
;----------------------------------------------------------------
DECODE_LETTER:
                MOV     A, length
                JZ      DL_DONE

                ; fetch offset = 1<<length
                MOV     DPTR, #lengthOffset
                MOVC    A, @A+DPTR    ; A = lengthOffset[length]
                MOV     R0, A

                ; compute final index
                MOV     A, pattern
                ADD     A, R0         ; A = idx

                ; lookup decodeTable[idx]
                MOV     DPTR, #decodeTable
                MOVC    A, @A+DPTR

                ;ACALL   PUT_CHAR
                ACALL SAVE_CHAR

                MOV     pattern, #0
                MOV     length, #0
DL_DONE:
                RET

;----------------------------------------------------------------
; lengthOffset: index = length (0…5) ? gives 2^length
;----------------------------------------------------------------


;----------------------------------------------------------------
; UART-putchar subroutine: A?SBUF, wait TI
;----------------------------------------------------------------
PUT_CHAR:
                ACALL DATAWRT
                RET
SAVE_CHAR:
		;ACALL PUT_CHAR
		MOV @R1, A
		INC R1
		MOV @R1, #0
		RET
LINE:           
		MOV R1, #70H
		MOV A, #0C0H
		ACALL COMNWRT
		UP:
		MOV A, @R1
		JZ DOWN
		ACALL DATAWRT
		INC R1
		SJMP UP
		DOWN:
		RET
		
;----------------------------------------------------------------
; START: basic init, enable serial interrupt
;----------------------------------------------------------------
START:

MOV P0, #0H

RS  	EQU P2.0
RW  	EQU P2.1 
E  	EQU P2.2
   
   ;MOV  SP, #90H 
   MOV  PSW, #00H 
   
LCD_IN:  
   MOV  A, #38H   ;init. LCD 2 lines, 5x7 matrix 
   ACALL COMNWRT   ;call command subroutine 
   ACALL  DELAY   ;give LCD some time 
   MOV   A, #0FH   ;dispplay on, cursor on 
   ACALL COMNWRT   ;call command subroutine 
   ACALL  DELAY   ;give LCD some time 
   MOV  A, #01    ;clear LCD 
   ACALL COMNWRT   ;call command subroutine 
   ACALL  DELAY   ;give LCD some time 
   MOV  A, #06H   ;shift cursor right 
   ACALL COMNWRT   ;call command subroutine 
   ACALL  DELAY   ;give LCD some time 
   MOV  A, #80H   ;cursor at line 1 postion 4 
   ACALL COMNWRT   ;call command subroutine 
   ACALL  DELAY   ;give LCD some time 
                ;MOV     SP, #70h
   MOV DPTR, #MSG1
   LCALL LCD_MSG
   
   LCALL DELAY
   
   MOV A, #0C0H
   LCALL COMNWRT
   MOV DPTR, #MSG2
   LCALL LCD_MSG
   LCALL DELAY
   LCALL DELAY 
   LCALL DELAY1S 
   MOV A, #01H
   ACALL COMNWRT	
   MOV DPTR, #MSG3
   LCALL LCD_MSG
   
                LCALL REC_FUNC
       
		
		CJNE A, #'1', NOT_MODE_1
		SJMP MORSE
		NOT_MODE_1:
		CJNE A, #'2', NOT_MODE_2
		SJMP RELAY_MOTOR
		NOT_MODE_2:
		CJNE A, #'3', NOT_MODE_3
		SJMP CYPHER
		NOT_MODE_3: 
		SJMP LCD_IN
		
                
MORSE:
                
                MOV A, #01H
                LCALL COMNWRT
                MOV DPTR, #MSG8
                ACALL LCD_MSG
                ACALL DELAY1S
                LCALL REC_FUNC
                
                CLR C
    		SUBB A, #30h
    		
    		MOV DPTR, #bitpattern
    		MOVC A, @A+DPTR
    		
    		MOV 50h, A
                
                
                
                
                
                MOV A, #01H
                LCALL COMNWRT
                LCALL DELAY
                LCALL DELAY
                MOV DPTR, #MSG4
                ACALL LCD_MSG
                ACALL DELAY
                ACALL DELAY 
                ACALL DELAY1S
                MOV A, #01H
                ACALL COMNWRT
                
                MOV pattern, #0
                MOV length,  #0
		MOV R1, #70H
                ; Serial setup for 9600-8-N-1 (assuming 11.0592 MHz)
               	MOV TMOD,#20H ; timer 1 mode 2 is selected
		MOV TH1,#0FDH ; baud rate
		MOV SCON,#50H ; serial mode 1 10 bit total isn, 8db, 1STOPb
		CLR TI ; making TI reg zero
		SETB TR1 ; starting timer 1
                SETB ES
                SETB EA

                ; stay here forever
HERE:           SJMP    HERE

RELAY_MOTOR:
		MOV A, #01H
		LCALL COMNWRT
		LCALL DELAY
		MOV DPTR, #MSG5
		ACALL LCD_MSG
		ACALL DELAY
                ACALL DELAY 
                ACALL DELAY1S
                MOV A, #01H
                ACALL COMNWRT
                
                ;Process Initialization in ISR:
                MOV P2, #00H
                
                ;Re-trigger
                MOV TMOD,#20H ; timer 1 mode 2 is selected
		MOV TH1,#0FDH ; baud rate
		MOV SCON,#50H ; serial mode 1 10 bit total isn, 8db, 1STOPb
		CLR TI ; making TI reg zero
		SETB TR1 ; starting timer 1
                SETB    ES
                SETB    EA
                
		SJMP $
		
CYPHER: 
		MOV A, #01H
		LCALL COMNWRT
		LCALL DELAY
		MOV DPTR, #MSG6
		ACALL LCD_MSG
		ACALL DELAY
                ACALL DELAY 
                ACALL DELAY1S
                MOV A, #01H
                ACALL COMNWRT
                
                ;Process Initialization in ISR:
                
                
                
                ;Re-trigger
                MOV TMOD,#20H ; timer 1 mode 2 is selected
		MOV TH1,#0FDH ; baud rate
		MOV SCON,#50H ; serial mode 1 10 bit total isn, 8db, 1STOPb
		CLR TI ; making TI reg zero
		SETB TR1 ; starting timer 1
                SETB    ES
                SETB    EA
                
		
		SJMP $

LCD_MSG:
        CLR     A              ; Clear A just in case
NEXT_CHAR:
        MOVC    A, @A+DPTR     ; Load character from code memory using DPTR
        JZ      END_PRINT      ; If A == 0, end of message
        LCALL   DATAWRT        ; Display the character on LCD
        INC     DPTR
        CLR A           ; Move to next character
        SJMP    NEXT_CHAR      ; Repeat
END_PRINT:
        RET
 

 
COMNWRT: 
   LCALL READY   ;send command to LCD 
   MOV  P1, A    ;copy reg A to port 1 
   CLR  RS    ;RS=0 for command 
   CLR  RW    ;R/W=0 for write 
   SETB  E    ;E-1 for high pulse  
   ACALL DELAY   ;give LCD some time 
   CLR  E    ;E=0 for H-to-L pulse 
   RET 

 
DATAWRT: 
   LCALL READY   ;write data to LCD 
   MOV  P1, A    ;copy reg A to port1 
   SETB  RS    ;RS=1 for data 
   CLR  RW    ;R/W=0 for write 
   SETB  E    ;E=1 for high pulse 
   LCALL DELAY   ;give LCD some time 
   CLR  E    ;E=0 for H-to-L pulse 
   RET 
   
READY:  
   SETB  P1.7 
   CLR  RS 
   SETB  RW 
WAIT:  
   CLR  E 
   LCALL DELAY 
   SETB  E 
   JB  P1.7, WAIT 
   RET 
 
DELAY:  MOV  R3, #50   ;50 or higher for fast CPUs 
HERE2:  MOV  R4, #255   ;R4=255 
HERE3:   DJNZ  R4, HERE3   ;stay untill R4 becomes 0 
        DJNZ   R3, HERE2 
        RET 

BUZZ:
	SETB P0.6
	ACALL DELAY500MS
	CLR P0.6
	ACALL DELAY500MS
	RET

REC_FUNC:
                MOV TMOD,#20H ; timer 1 mode 2 is selected
		MOV TH1,#0FDH ; baud rate
		MOV SCON,#50H ; serial mode 1 10 bit total isn, 8db, 1STOPb
		CLR TI ; making TI reg zero
		SETB TR1 ; starting timer 1
		
		ACALL DELAY

		CLR RI ; register involved in receiving data from bluetooth and ensuring it
		REP: JNB RI, REP
		ACALL DELAY
		ACALL DELAY
		CLR RI

		MOV A,SBUF
		RET
        
DELAY1: 
    MOV B,R4           
    MOV R3, B
D1: MOV R2, #255
D2: MOV R1, #255
D3: DJNZ R1, D3
    DJNZ R2, D2
    DJNZ R3, D1
    RET        
        
        
DELAY1S:
    MOV R3, #20       ; outer loop (20x)
DELAY_LOOP1:
    MOV R2, #250      ; middle loop (250x)
DELAY_LOOP2:
    MOV R5, #200      ; inner loop (200x)
DELAY_LOOP3:
    DJNZ R5, DELAY_LOOP3
    DJNZ R2, DELAY_LOOP2
    DJNZ R3, DELAY_LOOP1
    RET   
; DELAY500MS: Creates approximately 500ms delay at 12 MHz
DELAY500MS:
    MOV R3, #10        ; Outer loop - 10 times
LOOP1:
    MOV R2, #250       ; Middle loop - 250 times
LOOP2:
    MOV R5, #200       ; Inner loop - 200 times
LOOP3:
    DJNZ R5, LOOP3     ; Inner loop
    DJNZ R2, LOOP2     ; Middle loop
    DJNZ R3, LOOP1     ; Outer loop
    RET     
        
        
ORG     0300h
lengthOffset:  DB      0, 2, 4, 8, 16, 32

;----------------------------------------------------------------
; decodeTable: 64 entries, idx = (1<<len)+pattern, for len=1…5, pattern<2^len
;----------------------------------------------------------------
decodeTable:
    ; 0–1 unused
    DB  0, 0
    ; len=1 (idx=2–3): E, T
    DB  'E','T'
    ; len=2 (4–7): I, A, N, M
    DB  'I','A','N','M'
    ; len=3 (8–15): S, U, R, W, D, K, G, O
    DB  'S','U','R','W','D','K','G','O'
    ; len=4 (16–31): H, V, F, –, L, –, P, J, B, X, C, Y, Z, Q, –, –
    DB  'H','V','F',0,'L',0,'P','J'
    DB  'B','X','C','Y','Z','Q',0,0
    ; len=5 (32–63): digits & prosigns (fill as needed)
    DB  '5','4',0,'3',0,'2',0,'1'
    DB  '6',0,0,'7',0,'8','9','0'
    ; rest = 0
    DB  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    DB  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 
    
   ORG 400H        ; You can start at any ROM location that doesn't conflict
MSG1:   DB 'BT INTERFACING', 0
MSG2:   DB 'Group C2-G4', 0
MSG3:   DB 'Select a mode', 0
MSG4:   DB 'Morse mode selected', 0
MSG5:   DB 'Relay & Motor selected', 0
MSG6:   DB 'Cypher selected', 0
MSG8:   DB 'Select LED', 0

ORG 500H
bitpattern:
DB 1H, 2H, 4H, 8H, 10H, 20H, 40H, 80H
           
END
