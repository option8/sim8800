; SIM8800
;	Altair 8800 simulator by Charles Mangin, 2019
;8080 emulator code:
;	APPLE-80 - an 8080 simulator-debug package Copyright (c) 1979 by Dann Mccreary

					DSK SIM8800
					ORG $9000		

; ----------------------------------------------------------------------------
; TODO:
;	optimize for speed, 65C02 - target is Enhanced IIe, 128K RAM	
;	port to 65816? 24bit addresses, 16 bit registers, 64k contiguous AUX, 2.8mhz
;
; ----------------------------------------------------------------------------

ZERO				EQU $00
SPEED           	EQU $02			; ???
INTE            	EQU $03			; INTERRUPT ENABLE - 00 == interrupts enabled, 01 == no interrupts
PSW             	EQU $04			; PROCESSOR STATUS
SIMA            	EQU $05			; ACCUMULATOR
SIMM            	EQU $06			; DUMMY MEMORY REGISTER
SIML            	EQU $07			; HL REG PAIR
SIMH            	EQU $08			; HL REG PAIR
SIME            	EQU $09			; DE REG PAIR
SIMD            	EQU $0A			; DE REG PAIR
SIMC            	EQU $0B			; BC REG PAIR
SIMB            	EQU $0C			; BC REG PAIR
SPL             	EQU $0D			; STACK POINTER L
SPH             	EQU $0E			; STACK POINTER H
PCL             	EQU $0F			; 8080 PROGRAM COUNTER L
PCH             	EQU $10			; 8080 PROGRAM COUNTER H
DECIT           	EQU $11			; DOUBLE PRECISION -1
INCIT           	EQU $13			; DOUBLE PRECISION 0001
FLAG            	EQU $15			; USED BY SIMULATOR
DESTDA          	EQU $16			; DESTINATION DATA
DEST            	EQU $17			; DESTINATION INDEX
SRC             	EQU $18			; SOURCE INDEX

OPCODE          	EQU $1A			; CURRENT OPCODE
;PSPEED          	EQU $1B			; TRACE SPEED FROM PADDLE0
PNT             	EQU $1C			; DATA POINTER
SCR             	EQU $1E			; SCRATCHPAD REGISTER

TEXTTOP				EQU	$22			; text window top edge

HPOS            	EQU $24			; HORIZONTAL CURSOR POSITION
VPOS            	EQU $25			; VERTICAL CURSOR POSITION
CURPOS          	EQU $28			; CURSOR POSITION ADDRESS
KSWL				EQU	$38			; keyboard input routine
KSWH				EQU	$39			; normally $FD1B
	
HIPCH				EQU	$FE			; is the PC above 32k limit?	

KBDBUF          	EQU $C000       ; KEYBOARD BUFFER
KBDSTROBE       	EQU $C010       ; KEYBOARD STROBE
SPKR				EQU	$C030		; CLICK
LORES				EQU	$C050
TXTSET				EQU	$C051
MIXCLR				EQU	$C052
MIXSET				EQU	$C053
	
TXTPAGE1   			EQU	$C054
TXTPAGE2   			EQU	$C055

RAMRDOFF			EQU	$C002	;	Read enable main memory from $0200-$BFFF
RAMRDON				EQU	$C003	;	Read enable aux memory from $0200-$BFFF
RAMWRTOFF			EQU	$C004	;	Write enable main memory from $0200-$BFFF
RAMWRTON			EQU	$C005	;	Write enable aux memory from $0200-$BFFF
ALZTPOFF			EQU	$C008	;	Enable main memory from $0000-$01FF & avl BSR
ALTZPON				EQU	$C009	;	Enable aux memory from $0000-$01FF & avl BSR

; READ HIGH BIT:			
RAMRD				EQU	$C013	;	0=main $0200-$BFFF active reads 1=aux active
RAMWRT				EQU	$C014	;	0=main $0200-$BFFF active writes 1=aux writes
ALTZP				EQU	$C016	;	1=aux $0000-$1FF+auxBSR 0=main available

AUXMOVE				EQU	$C311	; 
; The beginning address of the block to be moved must be stored at locations A1L ($3C) and A1H ($3D) and the ending address at
; A2L ($3E) and A2H ($3F). Finally, the destination address must be stored at A4L ($42) and A4H ($43). 
; If the carry flag is set, then the move will be performed from main memory to auxiliary memory. 
; If it is clear, the move will take place in the opposite direction. 

	
CLRLORES			EQU	$F832		; clear low res screen
PREAD           	EQU $FB1E       ; PREAD returns a number that represents the position of a hand control.
SETTXT          	EQU $FB39       ; Set Text Mode
SETGR		  		EQU $FB40		; mixed text/graphics Mode
BASCALC         	EQU $FBC1       ; calculate base address
BELL2           	EQU $FBE4       ; 60hz bell
VTAB				EQU	$FC22       ; Sets the cursor vertical position (from CV)
HOME            	EQU $FC58       ; HOME
CLREOL          	EQU $FC9C       ; say end of line
WAIT				EQU	$FCA8		; simple delay
KEYIN				EQU	$FD1B
RDKEY           	EQU $FD0C       ; read keyboard input

CROUT           	EQU $FD8E       ; Print CR
PRBYTE          	EQU $FDDA       ; Prints byte as 2 hex digits
COUT            	EQU $FDED       ; character output
BELL            	EQU $FF3A       ; outputs ^G
OLDRST          	EQU $FF59       ; set screen mode and init

MLI    				EQU	$BF00   	; ProDOS system call
OPENCMD				EQU	$C8			; OPEN command index
READCMD				EQU	$CA			; READ command index
SET_MARKCMD			EQU	$CE			; SET_MARK index
CLOSECMD			EQU	$CC			; CLOSE command index
WRITECMD			EQU	$CB			; HERE THERE BE DRAGONS
; ----------------------------------------------------------------------------

; DO AUXMOVE from $9000 to $B000 -> aux $9000
					LDA #$00
					STA $3C
					STA $3E
					STA $42
					LDA #$90
					STA $3D
					STA $43
					LDA #$B0
					STA $3F
					SEC
					JSR AUXMOVE
; magic done.					



					JSR	HOME
					JSR CHOOSER				; determine what program/basic/bootloader to load

					JSR LOADPROGRAM			; load it, based on ACC value
						
					JSR	HOME
					JSR	INSTRUCTIONS		; copyright and instructions
	
					JSR	ALTAIRSCREEN		; Draw GR screen
		
; INIT
INIT				JMP SETUP

; ----------------------------------------------------------------------------
; MAIN
MNSTRT  			JMP	DISPLAYLOOP    ; setup and display first frame

; ----------------------------------------------------------------------------

SIM80 				LDA #$00
					STA HIPCH			; reset HIPCH for each go-round
					; **** add $10 to PCH
					LDA PCH
					STA DISPLAYBYTES	; high byte to first bank of LEDs.
					CMP #$7F			; above 32k limit?
					BCC ADC10
					ROL	HIPCH			; carry into HIPCH indicates we schlepped above 32K
					SEC					; gotta set carry again.
					SBC #$80			; subtract #$80 - probably could do AND 7F
					CLC
					
ADC10				ADC #$10
					STA PCH
					; ****

		  			JSR	MAIN           ; execute OPCODE

					; **** subtract $10 from PCH
					SEC
					LDA PCH
					SBC #$10
					
					LDX HIPCH			; were we reading PC from AUX?
					BEQ SBC10			; nope, skip
					CLC
					ADC #$80
					ROR HIPCH			; HIPCH 1 into carry, leaving 00
SBC10				STA PCH				; store resulting PCH
					;****

        			LDA	PSW            ; processor status
        			AND	#$D7           ; AND 1101 0111 - clear MI and HLTA bits
        			ORA	#$02           ; OR 0000 0010 - set WO bit
        			STA	PSW            ;
        			CLI	               ; clear interrupts

					JSR SIMDSK			; operate virtual disk drive

        			RTS	               ; return
	
; ----------------------------------------------------------------------------
; EXECUTE ONE 8080 OPCODE
MAIN   				SEI	               ;  DON'T ALLOW INTERRUPTS
       				CLD	               ;  CLEAR DECIMAL MODE
       				LDX	#$00           ;  LOAD INDEX
 					; **** add $10 to SIMH
					CLC
					LDA SIMH

; compare to 8f, subtract 80

					CMP #$7F			; if it's >=32K boundary after add $10, get from AUX
					BCC STASIMM2
					STA RAMRDON			; set AUX READ

					SBC #$80			; subtract #$80
					CLC					; read from AUX, still write to MAIN
										; add #$80
										; set MAIN READ
STASIMM2			ADC #$10
					STA SIMH

					

STASIMM      		LDA	(SIML,X)       ;  FETCH MEMORY *** from (HL) register pair ***
     				STA	SIMM           ;  crashes II if SIMH is C0 softswitch area										
					
					; **** subtract $10 from SIMH
DECSIMH				SEC					; then subtract #$10... oof.
					LDA SIMH
					SBC #$10
					STA SIMH
					;****

					LDA RAMRD			; if RAMRD high bit 
					BPL CHECKPCH			; low = RAMRDOFF
					STA RAMRDOFF		; high = turn it off, etc
					CLC					; add #$80 to SIMH again
					LDA SIMH

					ADC #$80			
					STA SIMH								

CHECKPCH								; is PC looking above #$8FFF?
					LDA HIPCH
					BEQ LDAPC			; nope, skip
					STA RAMRDON			; read from AUX					

LDAPC       		LDA	(PCL,X)        ;  FETCH INSTRUCTION **** this should come from real $1000-$8fff

					PHA					; opcode to stack
					LDA RAMRD			; if RAMRD high bit 
					BPL	STAOPCODE	
					STA RAMRDOFF		; high = turn it off, etc
					

STAOPCODE			PLA					; get the opcode back
					
       				STA	OPCODE         ;  
       				EOR	#$76           ;  
       				TAX	               ;  
       				LDY	#$01           ;  
       				LDA	PNT            ;  
       				AND	INTE           ;  IS THIS A HALT?
       				BPL	CKINT          ;  
       				STY	INTE           ;  
       				STY	PNT            ;  
       				ORA	#$C7           ;  
       				STA	OPCODE         ;  
       				DEY	               ; 
; CHECK FOR INTERRUPT	
CKINT   			LDA	INTE           ; 
        			BNE	NINT           ; 
        			DEC	INTE           ; 
; ENABLE INTERRUPT	
NINT    			TXA	               ; 
        			BNE	NOHLT          ; 
        			DEY	               ; 
; HALT?	
NOHLT   			TYA	               ; 
        			BEQ	ITR            ; 
        			JSR	INCPC          ; 
; SET COUNTER/INDEX	
ITR     			LDX	#$02           ; 
        			LDA	OPCODE         ; 
DIV     			PHA	               ; 
        			AND	#$07           ; 
        			EOR	#$07           ; 
        			STA	ZERO,X        ; 
        			PLA	               ; 
        			LSR	A              ; 
        			LSR	A              ; 
        			LSR	A              ; 
        			DEX	               ; 
        			BNE	DIV            ; 
        			STX	FLAG           ; 
        			LSR	A              ; 
        			BCS	IMI            ; 
        			BEQ	FIRST          ; 
IMARL   			LDA	#$11           ; no, arilog
        			ADC	ZERO+$01      ; SIMA
        			TAX	               ; 
        			LDA	#$00           ; set dest=A
        			STA	ZERO+$01      ; SIMA
IMI     			BEQ	MOVE           ; 
SECOND				LDA	#$07           ; offset for 2nd (page?)
FIRST				ADC	SPEED          ; 
        			TAX	               ; Transfer to Index
        			INX	               ; 
	
	
MOVE   				TXA	               ; SET HIGH ADDRESS 
       				ASL	A              ; 
       				TAX	               ; 
       				LDA	XFERTBL,X		; GET LO ADOR
       				INX					; 
       				PHA					; SET LO ADDRESS
       				LDA	XFERTBL,X		; 
       				PHA	               ; 
       				LDX	SPEED          ; GET SOURCE INDEX
       				
NOFETCH				LDY	SIMA,X         ; GET SOURCE DATA IN Y *** 
       				
       				LDX	ZERO+$01      ; GET DEST INDEX
       				LDA	SIMA,X         ; 
       				STA	ZERO          ; SAVE DESTINATION DATA
       				LDA	#$44           ; SET 6502 STATUS
       				PHA	               ; 
       				TXA	               ; DEST IND, IN A & X 
       				RTI	               ; GO INTERPRET
       									;	RTS to stack pointer instruction

; ----------------------------------------------------------------------------
IN     				LSR	A              ; 
       				BEQ	XCHG           ; 
       				BCC	INPUT          ; 
       				TAY	               ; 
       				JSR	CALL65         ; 
       				STA	SIMA           ; 
       				RTS	               ; 

; ----------------------------------------------------------------------------
; CALL 6502 SUBROUTINE
CALL65 				LDA	(PCL),Y        ; 
       				PHA	               ; 
       				DEY	               ; 
       				BPL	CALL65         ; 
       				JSR	DBLINC         ; 
       				LDA	#$00           ; 
       				PHA	               ; 
       				LDA	SIMA           ; 
       				RTI	               ; 

; ----------------------------------------------------------------------------
XCHG    			JSR	RPSCR         ;
        			LDX	#$04           
        			LDY	#$02           
        			JSR	RPRP          ;
        			LDX	#$04           
        			JMP	SCRRP         ;

; ----------------------------------------------------------------------------
XTHL    			JSR	RPSCR         ;
        			JSR	SPPNT         ;
        			LDX	#$02           
        			JSR	MEMRP         ;
        			LDX	#$13           
        			BNE	RPMEM         ;
; ----------------------------------------------------------------------------

IO     				LSR	A              ; which inst?
       				BNE	LBL            ;
       				STX	INTE           ; set/clear INTE
       				RTS	               ;

; ----------------------------------------------------------------------------
LBL     			LDX	#$02           ; point to HL
        			BCC	IN             ; is it input
        			LSR	A              ; no, is it xthl?
        			BEQ	XTHL           ; yes
        			BCS	JMPJUMPUN         ; is it jump?
OUTPUT  			DEC	FLAG           ; no, flag an output
INPUT   			JSR	IMM            ; get port #
        			LDA	SRC            ; SRC = port number
        			AND #$0F			; clear high bits (FF => 0F) because why keep up with 256 ports when we only need 16?
        			ASL	A              ; double index for hi/lo byte in table
        			TAX	               ; port table offset in X
        			LDY	#$02           ;
IOPLP   			LDA	IOTBLOUT,X     ; get IO address
        			STA	FLAG,Y         ; store in pointer
        			INX	               ; bump index
        			DEY	               ; decrement counter
        			BNE	IOPLP          ; 
        			INY	               ; point to data dir, reg
PNTPRT  			DEY	               ; point to port
        			LSR	FLAG			; FLAG is FF on OUTPUT, 00 on INPUT
        			BCC	IOIN           ; do input if carry clear. 
        			LDA	SIMA           ;  - if carry set, OUTPUT
IOOUT				
					STA	(DESTDA),Y     ; OUTPUT TO PORT
					JSR SIOOUT			; TELL SIO I HAVE PUT A BYTE TO SEND
        			BCS	IOEX           ; should return with Carry set???
        			
IOIN    			
					INC DESTDA			; switch from PORT-OUT to PORT-IN
					TYA
					PHA
					JSR SIOIN			; TELL SIO I WANT A BYTE - Y=PORT
					PLA
					TAY
					LDA	(DESTDA),Y     ; input from port
        			STA	SIMA           ; store in SIMA

IOEX    			RTS	               ;

JMPJUMPUN			JMP JUMPUN			; spaghetti
; ----------------------------------------------------------------------------
ARIM    			JSR	IMM            ;
        			CLC	               ;
        			JMP	IMARL          ;
        			
JMPCALLUN			JMP CALLUN			; spaghetti
; ----------------------------------------------------------------------------
PUSH   				LSR	A             	; IS THIS A PUSH? 
       				BCS	SKP           	; YES, GO PUSH
       				CMP	#$03          	; NO, IS THIS A CALL? 
       				BEQ	JMPCALLUN        ; YES, GO CALL
       				JMP	UNDEF         	; NO, UNDEFINED

SKP    				ASL	A             	;MAKE AN INDEX
       				TAX	              	;IS IT PUSH PSW?
       				BNE	PUSHT         	; NO, KEEP INDEX 
       				DEX	              	;YES, ADJUST INDEX

PUSHT  				TXA	              	; TEMP SAVE - x=0A on set return, gets PCL/H
       				PHA	              	;
       				JSR	DECSP         	; DECREMENT STACK POINTER 
       				JSR	SPPNT         	; IT BECOMES POINTER 
       				PLA	              	; RECOVER TEMP SAVE
       				TAX	              	;



RPMEM  				LDY	#$01          	; CLEAR INDEX

					; compare to 8f, THEN add 10 if necessary

					;CLC
					LDA DESTDA,Y		; $04 + $12 = $16 DESTDA
					STA DISPLAYBYTES	; high byte to first bank of LEDs.
					CMP #$7F			; over 9000?
					BCC RPMEM2			; nope, skip math

					SBC #$80			; off by $10 ??? *** 
					STA RAMWRTON		; write to AUX
					STA DESTDA,Y					

RPMEM2				CPX #$02			; **** 
					BNE RPLP			; **** add $10 to SRC+1 ON SHLD, X=2
										; E5 PUSH H AXY=02/02/01 
										; 22 SHLD	AXY=02/01/01
					LDA OPCODE			; is it PUSH H or SHLD? <-- corner case!
					CMP #$22
					BNE STADESTDA
					LDA DESTDA,Y
					CLC					; ADD 10 to destination on SHLD
					ADC #$10
					STA DESTDA,Y		; ****
			
STADESTDA			LDA RAMWRT			; if writing to aux, add 80 back and write to main
					BPL RPLP
					CLC
					LDA DESTDA,Y
					ADC #$80
					STA DESTDA,Y
					STA RAMWRTOFF		; write to MAIN
					
RPLP   				LDA	SIMM,X        	;GET NEXT RP DATA
					PHA					; hang onto it
					LDA DESTDA+1
					STA DISPLAYBYTES	; high byte to first bank of LEDs.

					CMP #$8F			; over 9000 or under 1000?
					BCS STADESTDA3		; over 9000
					CMP #$10			
					BCS	STADESTDA2		; Over 1000, skip

STADESTDA3			SEC

					SBC #$80
					STA RAMWRTON
					STA DESTDA+1
					
STADESTDA2			PLA					; get RP data back
					STA	(DESTDA),Y    	;STORE IN MEMORY 

					LDA RAMWRT			; if writing to aux, add 80 back and write to main
					BPL RPLP2
					CLC
					LDA DESTDA+1

					ADC #$80
					STA DESTDA+1
					STA RAMWRTOFF		; write to MAIN

RPLP2       		DEX	              	; 
       				DEY	              	;
       				BEQ	RPLP          	;

					CPX #$00			; **** 
					BNE RTS				; **** reset SRC+1 ON SHLD, X=2
					INY
					INY
					SEC
					LDA DESTDA,Y		; $04 + $12 = $16 DESTDA
					SBC #$10
					PHA					; hold that thought...
					LDA RAMWRT			; writing to AUX? turn that off.
					BPL RPLP3
					CLC
					LDA DESTDA,Y
					ADC #$80
RPLP3				STA RAMWRTOFF		
					PLA					; and we're back.
					STA DESTDA,Y		; ****


RTS    				RTS	              	;
; ----------------------------------------------------------------------------
JMP    				CLV	             	; INDICATE A JUMP

CALL 				JSR	CONDIT       	; TEST CONDITION 
       				BEQ	CALLUN       	; MET
       				JMP	DBLINC       	; NOT MET, increment PC
 
; ----------------------------------------------------------------------------
JUMPUN  			CLV	             	; INDICATE A JUMP 
CALLUN				PHP					; preserve overflow, etc

CALLUN3		  		JSR	PCPNT        	; GET NEXT 2 BYTES 


CALLUN4        		LDX	#$13         	; INTO SCRATCH
        			JSR	MEMRP        	;  

 					; **** add $10 to SRC+1
					CLC
					LDA SRC+1
					PHA
					STA DISPLAYBYTES	; high byte to first bank of LEDs.
					CMP #$7F			; is above 32k?
					BCC CALLUN5
					LDA #$01
					STA HIPCH			; if destination PCH is above 32k?
CALLUN5				PLA
					CLC
					ADC #$10			; CLEARS OVERFLOW?
					STA SRC+1
					;****

					PLP					; get overflow, etc.

CALLUN2				BVC	JM           	; JUMP on overflow clear
        			JSR	DBLINC       	; otherwise? BUMP PC twice
 
SVR     			LDX	#$0A         	; SAVE RETURN for RST

					LDA HIPCH			; return address should be HIPCH as well
					BEQ SVR1
					LDA PCH
					CLC
					ADC #$80
					STA PCH

        			; **** subtract 10 before setting return point
SVR1				SEC
					LDA PCH
					SBC #$10
					STA PCH
					;****

        			JSR	PUSHT        	; ON STACK **** <--- and if SP is >9000?

        			; **** add 10 after setting return point ???
					CLC
					LDA PCH
					ADC #$10
					STA PCH
					;****

JM      			LDX	#$0A         	; POINT TO PC

					; **** doing a JUMP or CALL - reset HIPCH
					LDA #$00
					STA HIPCH
					; **** doing a JUMP or CALL - reset HIPCH

        			JMP	SCRRP        	;  GIVE IT NEW ADDRESS

JMPSVR				JMP SVR				; SPAGHETTI!

; ----------------------------------------------------------------------------
RETURN  			JSR	CONDIT  		; TEST CONDITION 
        			BNE	RTS      		; NOT MET
RETUN   			
										; if SPH > 8F or < 10, subtract 80, read from AUX, write to AUX !!!
					; **** doing a JUMP or CALL - reset HIPCH
					LDA #$00
					STA HIPCH

					; **** doing a JUMP or CALL - reset HIPCH

					LDA SPH
					STA DISPLAYBYTES	; high byte to first bank of LEDs.

					CMP #$8F
					BCS RETUN1
					CMP #$10
					BCS RETUN2
					SEC
RETUN1				
					SBC #$80		; *** SPH off by $10?
					STA RAMRDON
					STA RAMWRTON
					STA SPH


										
        			; **** add $10 to (SP) before grabbing return point
RETUN2				CLC
					LDY #$01
					LDA (SPL),Y			; get byte at (SPL/H)
					ADC #$10
					STA (SPL),Y
					;****
					
					LDA RAMRD			; if reading/writing AUX, switch back to MAIN
					BPL RETUN3
					LDA SPH				; return SPH to high value
					CLC
					ADC #$80		; *** SPH off by $10?
					STA SPH
					STA RAMRDOFF
					STA RAMWRTOFF

RETUN3				LDX	#$0A     		; POP RETURN OFF 
        			BNE	POPIT    		; STACK INTO PC    
        			    			
RST     			LDA	OPCODE   		; CONVERT INSTRUCTION 
        			AND	#$38     		; TO RESET VECTOR 
        			STA	SRC      		; STORE IT IN SCRATCH 
        			LDA #$00
        			STA HIPCH			; no longer need to inc the PCH if we're resetting to 0038
        			LDA	#$10     		; HIGH RESET VECTOR
        			STA	SRC+1     		;  ??? SCR+1 ???
        			BCC	JMPSVR      		;
; ----------------------------------------------------------------------------
POP     			LSR	A        		; POP? 
        			BCC	OTH      		; NO
        			ASL	A        		; 
        			TAX	         		; POP PSW?
        			BNE	POPIT    		; NO, KEEP INDEX 
        			DEX	         		; YES, ADJUST INDEX 

POPIT   			TXA	         		; TEMP SAVE INDEX
        			PHA	         		; 
        			
         			
        			JSR	SPPNT    		; PULL OUT STACK DATA 
        			PLA	         		; BUMP SP
        			TAX	         		; 
        			JSR	MEMRP    		; SP IS POINTER 
ICISP   			JSR	ISP      		; RECOVER TEMP
ISP     			LDX	#$08     		; 
        			JMP	INC      		; 
		
; ----------------------------------------------------------------------------
OTH     			LDX	#$02           	; POINT TO HL 
        			LDY	#$08           	; ASSUME SPHL
        			ASL	A              	; IS IT SPHL?
        			BEQ	SPHL           	; YES, GO DO IT 
        			CMP	#$04           	; NO, WHAT IS IT?
        			BEQ	UNDEF          	; UNDEFINED 
        			BPL	RETUN          	; RETURN
        			LDY	#$0A           	; PCHL
        			
; **** ; if it's a PCHL, add $10 to SIMH to offset subtracting after the operation.

        			LDA SIMH			; ****
        			CLC					; ****
        			ADC #$10			; ****
        			STA SIMH			; ****
        			
SPHL    			JMP	RPRP           	; 
 
; ----------------------------------------------------------------------------
CONDIT 	
					LSR	A            	; MAKE INDEX TO MASK. TABLE
        			TAX	             	; 
        			LDA	MSKTBL,X     	; GET CONDITION FLAG MASK
        			AND	PSW          	; IS CONDITION SET* 
        			BCS	RTN          	; LEAVE Z FLAG SET
        			EOR	MSKTBL,X     	; OR REVERSE IT
RTN     			RTS	             	;  
	
; ----------------------------------------------------------------------------
NOP     			LDA	OPCODE         ; CHECK INSTRUCTION
        			BEQ	RTN            ; Pointer says 00 == NOP
UNDEF   			;LDY	#$0C           ; ILLEGAL OPCODE, UNDEFINED
        			;LDX	#$0A           ; 
        			;JSR	INCDEC         ; 
	       			;JMP	MNSTRT         ; ignore and move on.
					;JSR	BELL			; Beep, then RTS
					RTS					; RTS on illegal/undefined opcode ? 

; ----------------------------------------------------------------------------
DADLXI  			LSR	A              ; DROP LSB
        			ASL	A              ; 
        			BNE	NSP            ; USE SP?
        			LDA	#$08           ; YES, POINT TO IT
        			
        			
NSP     			PHA	               ; SAVE POINTER a=8
        			TXA	               ; GET DESTINATION INDEX 
        			LSR	A              ; DAD?
        			BCS	LXI            ; NO, GO DO LXI
DAD     			PLA	               ; YES, RECOVER POINTER a=8
        			TAY	               ; 
        			LDX	#$02           ; SET HL AS DESTINATION 
        			
	       			LDA OPCODE			; **** subtract 10 from SPH before working with it / DAD SP
					CMP #$39			; if we're working with stack pointer	
	       			BNE DAD2				
	       			LDA SPH
	       			SEC
	       			SBC #$10
	       			STA SPH				; **** subtract 10 from SPH before working with it
        			
DAD2				JSR	INCDEC         ; GO ADD RP & HL
 
	       			PHA					;  **** if we're working with DAD SP
	       			PHP					; preserve carry set, etc
	       			LDA OPCODE			
	       			CMP #$39				
	       			BNE CARRY2
	       			LDA SPH				; **** add 10 to SPH after working with it
	       			CLC
	       			ADC #$10
	       			STA SPH				
CARRY2	       		PLP					; reset carry, etc
					PLA					; **** add 10 to SPH after working with it
        			
CARRY	  			TAY	               ; SAVE RESULT
        			ROL	A              ; BRING IN CARRY
        			EOR	FLAG           ; CHG TO BORROW IF SUBTRACT
        			LSR	PSW            ; 
        			LSR	A              ; RESTORE CARRY(BORROW) 
        			ROL	PSW            ; 
        			TYA	               ; RECOVER RESULT
      								

        			RTS	
        								
; ----------------------------------------------------------------------------
LXI     			JSR	PCPNT         	; PC INTO PNT
        			PLA	              	; RECOVER POINTER
        			TAX	              	; USE TO INDEX DESTINATION RP
        			JSR	MEMRP         	; MOVE IMMEDIATE DATA TO RP
; if RP is SP, add 10 to SPH
					; ****
					LDA OPCODE
					CMP #$31
					BNE LXI2
					CLC
					LDA SPH
					ADC #$10
					STA SPH
					; ****
LXI2				JMP	DBLINC        	; BUMP PROGRAM COUNTER

; ----------------------------------------------------------------------------
LDSTR   			BIT	FOUR           ; INDIRECT? 
        			BEQ	DIRECT         ; NO
        			LSR	A              ; STAX? 
        			BCC	LDAX           ; NO
STAX    			
					INX					; **** add 10 to DESTDA
					CLC					
					LDA PSW,X
					STA DISPLAYBYTES	; high byte to first bank of LEDs.
					CMP #$7F			; over 9000?
					BCC STAX1			; nope, carry on
					SBC	#$80			; yep, subtract 80
					STA RAMWRTON		; write AUX
					CLC
STAX1				ADC #$10


STAX2				STA PSW,X			; store updated destination address
					DEX					; ****
					LDA	SIMA           ; YES,STORE A
        			STA	(PSW,X)        ; PSW = 04 + 12 = 16 = destDA
					INX					; **** reset DESTDA
					SEC					
					LDA PSW,X
					SBC #$10
					LDY RAMWRT			; load Y? Y not!?
					BPL	STAX3
					CLC
					ADC #$80
					STA RAMWRTOFF		; back to MAIN		
STAX3				STA PSW,X			
					DEX					; ****


        			RTS	               ; 

; ----------------------------------------------------------------------------
DIRECT  			PHA	             	; TEMP SAVE
        			JSR	PCPNT        	; PC TO POINTER
        			JSR	DBLINC       	; 
        			LDX	#$13         	; 
        			JSR	MEMRP        	; 

; **** if LHLD, add 10 to SRC before grabbing memory contents
					LDA OPCODE
					CMP #$2A
					BNE DIRECT2
					LDA SRC+1
					CLC
					ADC #$10
					STA SRC+1
; **** if LHLD, add 10 to SRC before grabbing memory contents

DIRECT2        		LDX	#$11         	; 
        			JSR	SCRRP        	; 
        			PLA	             	; RECOVER TEMP 
        			LSR	A            	; LOAD?
        			BCS	STORE        	; NO, STORE
        			ASL	A            	; 
        			TAX	             	; 
        			BEQ	LDAD         	; 
        			JMP	MEMRP        	; 

; ----------------------------------------------------------------------------
LDAD   				LDX	#$11           ; 

LDAX				CLC					; ****
					INX
					LDA SIMA,X

					STA DISPLAYBYTES	; high byte to first bank of LEDs.

					CMP #$7F			; over 9000?
					BCC LDAX1
					SBC #$80
					STA RAMRDON
					
					CLC
LDAX1				ADC #$10
					STA SIMA,X		
					
						
LDAX2				DEX					; ****
					
   					LDA	(SIMA,X)       ; 
       				STA	SIMA           ; 

					SEC					; ****
					INX
					LDA SIMA,X
					SBC #$10
					STA SIMA,X	
					
					LDA RAMRD
					BPL LDAX3
					STA RAMRDOFF		; back to MAIN
					LDA SIMA,X
					CLC
					ADC #$80
					STA SIMA,X
							
LDAX3				DEX					; ****
					
					RTS	               ; 
	
; ----------------------------------------------------------------------------
STORE   			ASL	A             	; MAKE INDEX
        			TAX	              	; 
        			BEQ	STAD          	; STORE A DIRECT?

        			JMP	RPMEM         	; NO, STORE HL DIRECT SHLD

STAD    			LDX	#$12           ; POINT TO POINTER
        			JMP	STAX           ; jump to STAX

; ----------------------------------------------------------------------------

INXDCX  			LDY	#$0C           ; SET FOR DECREMENT
        			LSR	A              ; 
        			BCC	DRP            ; YES
        			LDY	#$0E           ; NO, SET FOR INCR
DRP     			ASL	A              ; DROP LSB 
        			BNE	NOR            ; FOR SP?
        			LDA	#$08           ; YES, POINT TO SP 
NOR     			TAX	               ; USE AS INDEX
        			JMP	INCDEC         ; 

; ----------------------------------------------------------------------------
DCR     			SEC	               ; SET FOR DECREMENT
        			DEC	FLAG           ; 
INR     			LDY	#$01           ; SET FOR INCREMENT
BTH     			JSR	ADDR           ; GO ADD
        			JMP	STATUS2        ; SET STATUS, NOT CARRY



; ----------------------------------------------------------------------------
MVI     			JSR	IMM            ; GET IMMEDIATE BYTE
        			JMP	MOVE           ; MOVE IT TO DESTINATION

; ----------------------------------------------------------------------------
MOVIT  				TYA	               ; GET SOURCE DATA 
MVIT1  				DEX	               ; IS DEST MEMORY?
       				BNE	NMEM           ; NO
       				
       				INX 				; ****
       				PHA					; ****
       				LDA SIML,X			; **** SIMH
					STA DISPLAYBYTES	; high byte to first bank of LEDs.
					CMP #$7F			; above 8000?
										; above 9000 after adding $10 !!!
					BCC MVIT2
       				STA RAMWRTON		; set AUX WRITE
       				SBC #$80			; SBC #$80
					CLC					; ****
       		
MVIT2       		ADC #$10			; add #$10

       									; store in AUX
       									; add #$80
       									; set MAIN WRITE
       				
       				
       				
STASIMH       		STA SIML,X 			; ****
       				PLA					; ****
       				DEX					; ****
       				       				
       				STA	(SIML,X)       ; YES STORE IN MEMORY 
       				INX 				; ****
       				PHA					; **** byte into stack

					LDA RAMWRT			
					BPL	STASIMH2
					LDA SIML,X
					STA RAMWRTOFF		; AUX OFF, write to MAIN
					CLC
					ADC #$80
					STA SIML,X 

STASIMH2       		SEC					; ****
       				LDA SIML,X			; ****
       				SBC #$10			; **** subtract 10
       				STA SIML,X 			; ****
       				
NOMOV       		PLA					; ****
       				DEX					; ****
       				
NMEM   				STA	SIMM,X         ; STORE IN DEST REGISTER
       				RTS	               ;   

; ----------------------------------------------------------------------------
ADC    				CLV	               ; FLAG W/CARRY
       				BVC	ADD            ;

CMP    				LDX	#$13           ; CHG DEST TO SCR
       				BNE	SUB            ; SO ONLY PSW IS AFFECTED

SBB    				CLV	               ; FLAG W/BORROW
SUB    				DEC	FLAG           ; F L A G = FF FOR SUBTRACT
       				SEC	               ;

ADD    				JMP	ADCRY         ; 

; ----------------------------------------------------------------------------
ORA    				TYA	               ; GET SOURCE DATA
       				ORA	SIMA           ; LOGICAL OR
       				BCC	SOM            ; SET FOR A CLEAR AUX CRY

XRA    				TYA	               ; G E T SOURCE DA T A 
       				EOR	SIMA           ; LOGICAL EXCLUSIVE OR 
SOM    				LDY	#$00           ; 00 WON'T SET ANY FLAGS 
       				BEQ	ALL            ; GO SAVE RESULT

ANA    				TYA	               ;  G E T SOURCE DA T A
       				PHA	               ;  SAVE IT
       				ORA	SIMA           ;  LOGICAL OR OF BIT
       				ASL	A              ;  T H R E E BECOMES AUX CARRY
       				AND	#$10           ;    
       				TAY	               ;  
       				PLA	               ; GET SOURCE DATA
       				AND	SIMA           ; LOGICAL AND

ALL    				STY	SRC+1          ; SAVE FLAG SETTING DATA 
       				STA	SIMA           ; LOGICAL RESULT INTO A
       				JSR	STATUS2        ; SET STATUS
       				LDA	#$EE           ; SELECT STATUS BITS TO CLEAR 
CYAC   				AND	PSW            ; CLEAR SELECTED BITS
       				ORA	SRC+1          ; SET SELECTED BITS
STPSW  				STA	PSW            ; SAVE NEW PSW
       				RTS	               ;






; ----------------------------------------------------------------------------
ROTATE  			LDY	SIMA           ; GET ACCUMULATOR
        			LSR	A              ; IS THIS CMC OR 8TC?
        			BNE	ROCOMP         ; NO, IT IS A ROTATE .OR CMA
        			LDA	PSW            ; YES, CHANGE CARRY 
        			EOR	#$01           ; CMC
        			BCC	STPS           ; 
        			ORA	#$01           ; STC
STPS    			BVS	STPSW          ; 
; ----------------------------------------------------------------------------

ROCOMP  			BCS	LEFT           ; LEFT OR RIGHT? 
        			LSR	A              ; CMA?
        			BNE	ROT            ; NO, ROTATE
        			TYA	               ; YES, COMMPLEMFNT A
        			EOR	#$FF           ;
        			STA	SIMA           ;  DOESN* T SET STATUS 
        			RTS	               ; 

; ----------------------------------------------------------------------------
ROT    				TYA	               ; GET ACCUMULATOR
       				BCS	RRC            ; 
       				LDA	PSW            ; GET 8080 CARRY
RRC    				LSR	A              ; 
       				TYA	               ; GET ACCUMULATOR 
       				ROR	A              ; ROTATE RIGHT
       				BVS	JCRY           ; GO STORE&SET CARRY
; ----------------------------------------------------------------------------
LEFT   				LSR	A              ; DECIMAL ADJUST? IYES# GO 00 IT
       				BEQ	DAA            ;  N 0 , GET ACCUMULATOR 
       				TYA	               ; JRLC?
       				BCS	RLC            ;  NO#GET 8080 CARRY
       				LDA	PSW            ; MOVE IT INTO MSNIB
       				ROR	A              ;  
       				ROR	A              ;  
RLC    				ASL	A              ;  MOVE IT INTO CARRY
       				TYA	               ;  G E T ACCUMULATOR
       				ROL	A              ; MOVE IT LEFT
JCRY   				STA	SIMA           ;  SAVE IT
       				JMP	CARRY          ; GO SET STATUS
 
; ----------------------------------------------------------------------------
DAA    				CLC	               ;
       				PHP	               ;PRE3ERVE STATUS | 
       				TYA	               ;GET SOURCE DATA
       				STA	ZERO          ; PREP FOR ADD
       				AND	#$0F           ;LOOK AT LSNI8
       				ADC	#$06           ;IF  *0A WILL CAUSE AUX CRY
       				ORA	PSW            ;OR I F AC I S ALREADY SET 
       				AND	#$10           ; EITHER SET?
       				BEQ	NOSIX          ;NO, DON  T ADJUST LSNIB
       				LDA	#$06           ; YES# ADJUST IT
NOSIX  				TAY	               ; 
       				ADC	ZERO          ;   G E T SOURCE
       				BCS	SXTY           ;
       				ADC	#$60           ;113 MSNIB NOW >  A0T IIF SO* CARRY IS SET
       				ROL	A              ;G E T CARRY
       				ORA	PSW            ;OR WITH 8080 CARRY
       				LSR	A              ;I S EITHER SETT
       				BCC	DA             ;NO# DON'T ADJUST MSNIB l
SXTY   				TYA	               ;YES* ADJUST MSN!B I
       				ADC	#$5F           ;(5F + CARRY = 60)
       				TAY	               ;
DA     				LDX	#$00           ;DESTINATION IS A
       				PLP	               ;RESTQRE STATUS
ADCRY  				JSR	ADDR           ; 
       				JSR	CARRY          ;


STATUS2 	
					TAY	               ; SAVE RESULT
        			ROL	PSW            ; CLEAR P3W SIGN BIT 
        			ASL	A              ; PUT NEW SIGN IN CARRY 
        			STA	FLAG           ;  C L E A R L S B OF F L A G 
        			LDA	PSW            ;  PUT NEW SIGN IN P3N
        			ROR	A              ;
SGN     			ORA	#$46           ; PRESET Z*P 8 PRESET
        			TAX	               ; 3AVE IN X
        			TYA	               ;  RECOVER WORD
        			BEQ	DONE           ;  I F ZERO*ALL DONE
FLIP    			INC	FLAG           ; IFLIP FLAG |
PAR     			LSR	A              ; TEST EACH BIT I
        			BEQ	ALL2           ; NO MORE BITS
        			BCC	PAR            ; 
ALL2    			BCS	FLIP           ; 
        			LSR	FLAG           ; TEST FLAG 
        			TXA	               ; RECOVER P3w 
        			AND	#$BF           ; CLEAR Z 
        			BCS	REC            ; PARITY EVENT 
        			AND	#$FB           ; N0* CLEAR P 
REC     			TAX	               ; BACK TO X
DONE    			STX	PSW            ; STORE AS PSW
        			RTS	               ;  

; ----------------------------------------------------------------------------

PCPNT 				LDX	#$0A          	; **** 0A + 05/06 = 0F = PCL/PCH
RPPNT 				LDY	#$11          	; 05 + 11 = 16 = store at DESTDA
      				BNE	RPRP          	; never zero, branch always
RPSCR 				LDY	#$13          	; store at SRC instead

; moves scratchpad or other RP bytes to PC, vice versa
RPRP									

	  				LDA	SIMA,X        	; load SOURCE BYTE L
      				STA	SIMA,Y        	; store at DESTINATION L
      				LDA	SIMM,X        	; load SOURCE BYTE H
 										
      				CPY #$08			; **** if SPHL
					BNE RPRP4			; ****
					CLC					; ****
					ADC #$10			; ****
					JMP RPRP2
					
RPRP4				CPX #$0A			; X=0A, PC IS POINTER. 
					BNE RPRP2			
					LDA HIPCH			; HIPCH = 1, inc resulting pointer by 70
					BEQ RPRP3			
					CLC
					LDA	SIMM,X
					ADC #$80			; 70 or 80? oof...
					JMP RPRP2

RPRP3				LDA	SIMM,X

RPRP2				STA	SIMM,Y        	; store at DESTINATION H

RPRTS      			RTS	            	;

; ----------------------------------------------------------------------------
SCRRP  				TXA	               ; RP is destination (RP is a Register Pair)
       				TAY	               ; 0A on CALL (for return address???)
       				LDX	#$13           ; SCR is source
       				BNE	RPRP          	; never zero, branch always
       				
SPPNT  				LDX	#$08           ; SP is Source
       				BNE	RPPNT         	;  jump RPPNT
       				
DBLINC 				JSR	INCPC          ; Increments PC twice
; ADVANCE COUNTER	

INCPC  				LDX	#$0A           ; INC PC - $0A + $05 = $0F = PCL
INC    				LDY	#$0E           ; point to INC data - $0E + $05 = $13 = INCIT (0001)
INCDEC 				CLC	               ; 								
        								
INDE   				LDA	SIMA,X         ; get register/PC value
       				ADC	SIMA,Y         ; add value from 
       				STA	SIMA,X         ; save
       				INY	               ; bump indexes
       				INX	               ; 
       				TYA	               ; check index 0E->0F
       				AND	#$01           ; last pass? - increment HI byte
       				BNE	INDE           ; not done, loop

       				RTS	               ; yes

; ----------------------------------------------------------------------------
DECSP  				JSR	DSP            ; DEC SP twice
DSP    				LDX	#$08           ; DEC SP
DEC    				LDY	#$0C           ; point to DEC data
       				BNE	INCDEC         ; go do it
; ----------------------------------------------------------------------------
MEMRP 				TYA					; grab source pointer offset


					LDA DESTDA+1
					
					STA DISPLAYBYTES	; high byte to first bank of LEDs.

					CMP #$8F			; *** off by $10 *** WTF ***
					BCS AUXMEMRP
					CMP #$10			; less than $10, rolled over from $F0
					BCS MRLP1
AUXMEMRP			STA RAMRDON			; read from AUX
					SEC
					SBC #$80
					STA DESTDA+1

MRLP1				LDY	#$01           ; move data in memory

MRLP  				LDA	(DESTDA),Y     ; get next byte
      				STA	SIMM,X         ; store in destination
      				DEX	               ; bump indices
      				DEY	               ;
      				BEQ	MRLP           ;

					LDA RAMRD			; if we're still reading from AUX, add 80 back
					BPL MRLPRTS
					STA RAMRDOFF		; read from MAIN
					LDA DESTDA+1
					CLC
					ADC #$80
					STA DESTDA+1
					

					
MRLPRTS				RTS	               ;

; ----------------------------------------------------------------------------
IMM 				LDX	#$00           ; GET BYTE POINTED AT
										; BY PROGRAM COUNTER
					LDA HIPCH			; if HIPCH, get it from AUX
					BEQ IMM2
					STA RAMRDON			; AUX on

IMM2    			LDA	(PCL,X)        
    				STA	SRC            ; SAVE IT IN SCR
    				LDA	#$13           ; MAKE SCR THE SOURCE INDEX
    				STA	SPEED          ; ??? should SPEED be SCR?
    				STA RAMRDOFF		; back to main
    				JSR	INCPC          ; BUMP PC
    				LDX	#$00           ; POINT TO INTERP PAGE 2
    				RTS	               ; 

; ----------------------------------------------------------------------------
ADDR    			TYA	              	;
        			EOR	FLAG          	;
        			TAY	              	;
        			AND	#$0F          	;
        			STA	SRC           	;
        			ROL	A             	;
        			BVS	WOCRY         	; 
        			EOR	PSW           	;
WOCRY				PHA	              	;
        			LSR	A             	;
        			LDA	ZERO          	; 
        			AND	#$0F          	;
        			ADC	SRC           	;
        			AND	#$10          	;
        			STA	SRC+1         
        			LDA	#$EF          	;
        			JSR	CYAC          	; 
        			PLA	              	;
        			LSR	A             	;
        			TYA	              	;
        			ADC	ZERO          	; 
        			JMP	MVIT1         	;

; ----------------------------------------------------------------------------
; TABLE OF PSW MASKS
MSKTBL				DB	$80					; SIGN BIT MASK 
FOUR				DB	$04,$01,$40   		; PARITY BIT MASK, CARRY BIT MASK, ZERO BIT MASK

; XFER TABLE
XFERTBL 
					DB	>MOVIT,<MOVIT		; MOVIT
					DB	>ROTATE,<ROTATE		; ROTATE
					DB	>MVI,<MVI			; MVI
					DB	>DCR,<DCR			; DCR
					DB	>INR,<INR			; INR
					DB	>INXDCX,<INXDCX		; INXDCX
					DB	>LDSTR,<LDSTR		; LDSTR
					DB	>DADLXI,<DADLXI		; DADLXI
					DB	>NOP,<NOP			; NOP
					DB	>RST,<RST			; RST
					DB	>ARIM,<ARIM			; ARIM
					DB	>PUSH,<PUSH			; PUSH
					DB	>CALL,<CALL			; CALL
					DB	>IO,<IO				; IO
					DB	>JMP,<JMP			; JMP
					DB	>POP,<POP			; POP
					DB	>RETURN,<RETURN		; RETURN
					DB	>CMP,<CMP			; CMP
					DB	>ORA,<ORA			; ORA
					DB	>XRA,<XRA			; XRA
					DB	>ANA,<ANA			; ANA
					DB	>SBB,<SBB			; SBB
					DB	>SUB,<SUB			; SUB
					DB	>ADC,<ADC			; ADC
					DB	>ADD,<ADD			; ADD
        
XFERTBL3
					DB	$FF,$FF,$FF,$FF,$FF,$FF,$FF    
	
L0BA6  				DB	$FF    
L0BA7  				DB	$FF    
BRKTBL				DB	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
       				DB	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF




; ----------------------------------------------------------------------------
INCHPOS				INC	HPOS			;increment HPOSition = cursor right
EDLIN   								; edit line of registers?
					LDA	KBDSTROBE		; clear keyboard strobe                              
READKEY2	
					JSR	RDKEY			; read key
					CMP	#$95			; right arrow?
					BEQ	NEXTCHAR		; cursor over 1
					CMP	#$A0			; SPACE
					BEQ	WRITEKEY		; write space to screen
					CMP	#$8D			; CR
					BNE	BKSPC			; 
					RTS					; if CR, done.

; ----------------------------------------------------------------------------
BKSPC   			CMP	#$88           ; BACKSPACE? There's no *backspace*? left arrow?
        			BNE	CMPKEY          ; not left arrow, jump down...
        			DEC	HPOS           ; decrement cursor position
        			BPL	EDLIN      		; new line? jump back to read key
        			STX	HPOS           ; otherwise, store X in cursor position - but where does X come from?
        			BNE	INCHPOS        ; increment cursor position?
CMPKEY				TAY	               ; hold keychar in Y temporarily.
        			AND	#$60           ; renders anything 20 30 40 ???
        			BEQ	READKEY2		; if zeroes out, not a number or A-F. ???
        			TYA	               ; keychar back to A
        			JSR	CKNUMS          ; if a number or A-F, returns with Carry Set
        			BCC	READKEY2		; carry clear, not a number 0-F, start over
        			TYA	               ;
        			LDY	HPOS           ;
WRITEKEY	
					STA	(CURPOS),Y     ; store keystroke on screen.
NEXTCHAR									; cursor over 1
					CPX	HPOS           ; if X > current HPOS, 
        			BCS	INCHPOS        ; increment HPOS and start over
        			LDA	#$FF           ; otherwise, 
        			STA	HPOS           ; set HPOS to FF
        			BNE	INCHPOS        ; then increment to 00



; ----------------------------------------------------------------------------
; I/O TABLE
; 0/10 = SIO STATUS = for left bank of switches
; 1/11 = SIO DATA = to put a byte at lower left of screen

; 08 = Disk Enable/Status channel
; 09 = Disk control/sector position channel
; 0A = Disk data channel

; FF/0F = panel switches

IOTBLOUT			DB	>SIOSTATUSOUT,<SIOSTATUSOUT,>SIODATAOUT,<SIODATAOUT
					DB	$C0,$30,$C0,$30,$C0,$30,$C0,$30,$C0,$30,$C0,$30
     				DB	>DSKSELECT,<DSKSELECT,>DSKCONTROL,<DSKCONTROL,>DSKWRITE,<DSKWRITE
     				DB	$C0,$30,$C0,$30,$C0,$30,$C0,$30
     				DB	>DISPLAYBYTES,<DISPLAYBYTES

IOTBLIN				DB	>SIOSTATUSIN,<SIOSTATUSIN,>SIODATAIN,<SIODATAIN
					DB	$C0,$30,$C0,$30,$C0,$30,$C0,$30,$C0,$30,$C0,$30
     				DB	>DSKSTATUS,<DSKSTATUS,>DSKSECTORIN,<DSKSECTORIN,>DSKREAD,<DSKREAD
     				DB	$C0,$30,$C0,$30,$C0,$30,$C0,$30
     				DB	>SWITCHBYTES,<SWITCHBYTES

; ----------------------------------------------------------------------------
; SETUP 8080 REGISTERS

SETUP				JSR	REGSETUP
					JSR	UPDATESWITCHES ; update the switches in display.
				
					SEC	
					JSR	WAITLED				; wait LED on 
				
					CLC					; clear carry for PCH display
					JSR	UPDATELEDS

					LDA BASICBUFFERLO	; locate buffer start
					STA $CE
					LDA BASICBUFFERHI	; 
					STA $CF


; DISPLAY INTERFACE
DISPLAYLOOP 

					JSR	CLRSCRN        ;
					CLC					; clear carry for PCH display
					JSR	UPDATELEDS		; put displaybytes to LED pixels
	
					BIT	KBDSTROBE      ;
; KEYBOARD INPUT
READKEY 
					LDA	KBDBUF         ; 
        			CMP	#$A0           ; space
        			BEQ	STEP          ; to step one cycle at a time
        			CMP	#$D3           ; S
        			BEQ	STEP          ; to step one cycle at a time
        			CMP	#$D4           ; T
        			BEQ	TKEY           ; 
        			CMP	#$D2           ; R
        			BEQ	JMPRKEY           ; 
        			CMP	#$C7           ; G
        			BEQ	GKEY           ; 
        			CMP	#$CE           ; N
        			BNE	CARET          ; 
					JMP	NEXT

; ----------------------------------------------------------------------------
; DECREMENT COUNTER, EXAMINE
CARET   			CMP	#$DE           ; CARET ^
        			BNE	IKEY           ; not ^, check for I
        			LDX	#$0A           ; 10 ?
        			LDY	#$0C           ; 13 ?
        			JSR	INCDEC         ; decrement counter
					JMP	EXAMINE2	
JMPINSTRUCTIONS
					JMP	SHOWINST	
JMPDOBRKS	
					JMP	DOBRKS			; too much spaghetti

; ----------------------------------------------------------------------------
; I for instructions
IKEY   				CMP	#$C9           ; I key
					BEQ	JMPINSTRUCTIONS
					CMP	#$D8			; X for EXamine
					BEQ	JMPEXAMINE		; reads switches, puts to PC, displays bytes
					CMP	#$D0			; P for DePosit/PUT
					BEQ	JMPDEPOSIT		; reads switches, puts byte into memory at PC
       				CMP	#$CF			; O for depOsit Next
					BEQ	JMPDEPOSITNEXT
       				BNE	KKEY			; not O? check for K

; ----------------------------------------------------------------------------

JMPRKEY				JMP	RKEY			; spaghetti...
			
	
KKEY				CMP	#$CB           ; K key
					BEQ	JMPDOBRKS         ; do breakpoint edits

; not a function key, CHECK IF IT'S a switch key.
       
; *** convert keyboaRD ASCII TO HEX, JSR to switch setting
					JSR	ISITHEX			; returns with CARRY SET IF HEX 0-F
											; and accumulator in Y
					BCS	JMPSWITCH
					JSR	CKINTE      	; otherwise check if SCRATCHPAD==key?        
					JMP	READKEY        ; back to read keys


; ----------------------------------------------------------------------------
STEP	; flip up swITCH 2, BOTTOM row. "Single Step"
					LDX	#$02			; switch 2	
					LDY	#$01			; up
					JSR	BOTTOMROW
					JSR	SIM80          ; Do 1 instruction
					SEC					; set carry for SIMD display
					
					JSR	DISPLAYREGD		; do reg D, then update LEDs
					
					JSR	BOTTOMROWRST	; reset the switch locations
					JMP	DISPLAYLOOP    ; update display


; ----------------------------------------------------------------------------
; TRACE MODE
TKEY
					JSR LINE24			; setup for write to last line
					BIT KBDSTROBE		; clears out the keyboard
										
TKEY2				JSR	CLRSCRN			; clears the text screen
					JSR	CTLC			; check for ^C or switch flipping - ^C returns 0
      				BEQ	JMPDISPLAY    	; ^C, break out and display current PC
      				JSR	CKINTE			; otherwise, check scratch register = accumulator - interrupt?
     				JSR	SIM80			; do one instruction
     				
					SEC					; set carry for SIMD display
      				JSR	DISPLAYREGD		; do reg D, then update LEDs
     				JSR	NEXTPC          ;
      				BEQ	JMPDISPLAY    	; error out on 00 - or is this checking breakpoints?
      				BNE	TKEY2           ; else loop for another step

; ----------------------------------------------------------------------------

	; G for GO - RUN MODE	
GKEY    			CLC	
					JSR	WAITLED			; turn off the WAIT lamp
					LDA	SWITCHBYTES
					STA	SWITCHSCRATCH	; store the switch settings

					JSR LINE24			; setup for write to last line
					BIT KBDSTROBE		; clears out the keyboard



GKEY2				JSR PCHFIX			; WTAF???
					JSR	SIM80          	; do one instruction

        			LDX	PNT+$01        	; load next instruction PNT Hi???
        			BEQ	CTLC2          	; if it's 00 ???
      				
        			JSR	NEXTPC          ; otherwise get next instruction, check for breakpoint
        			BEQ	JMPDISPLAY     	; if returns 0 jmp out to displayloop
CTLC2				CLC
					JSR	STOPG			; check for ^C, don't do switch keys
        			BEQ	JMPDISPLAY     	; jmp to displayloop on ^C
					BMI JMPCTRLB		; if sign set, got a ^B

        			JSR	CKINTE      	; check if SCRATCHPAD=accumulator? ck interrupts
        			JMP	GKEY2           ; 


; ----------------------------------------------------------------------------

JMPDEPOSIT
					JMP	DEPOSIT


JMPEXAMINE
					JMP	EXAMINE      
JMPDEPOSITNEXT	
					JMP	DEPOSITNEXT

; ----------------------------------------------------------------------------
; REGISTER MODIFY
RKEY   				LDA	#$15           ; down to line 21
       				JSR	BASCALC        ; 
       				LDX	#$1C           ; 28?
       				JSR	EDLIN        
       				JSR	EDITREGS       ; 
	
JMPCTRLB			JMP CTRLB			; bload, read file into IOIN				
	
JMPSWITCH	
					STA	KBDSTROBE
					JSR	SETSWITCH		; toggle the switch indicated by ACCUMULATOR
					JMP	DISPLAYLOOP    ; back to display/idle mode
	
	
JMPDISPLAY			LDA TEXTTOP			; came from CTLC
					BNE	STATUSLINE		; possibly coming from full screen terminal
										; if full screen, TEXTTOP is 0
										; texttop is 0, 
					JSR SETGR			; need to set GR mode, load lo-res Altair screen
					JSR ALTAIRSCREEN	;
					JSR LINE24			; sets TEXTTOP to 20
										
STATUSLINE			JSR PRLEGEND		; print REGs legend
					SEC					; show D register
					JSR	UPDATELEDS		; update LEDs on run mode? slows down too much.
					LDA	SWITCHSCRATCH	; switch bank 1 back to normal
					STA	SWITCHBYTES
	
					SEC	
					JSR	WAITLED				; wait LED on 
					JMP	DISPLAYLOOP    ; 

		




CTLC				CLC					; clear carry for checking
					LDA	KBDBUF			; read keyboard for ^C
	
										; *** convert keyboard ASCII to HEX, JSR to switch setting
					JSR	ISITHEX			; returns with CARRY SET IF HEX 0-F
											; and accumulator in Y
					BCC	NOSWITCH		; otherwise, check for ^C			
	
					JSR	SETSWITCH		; toggle the switch indicated by ACCUMULATOR
					BIT KBDSTROBE
					RTS					; and return
					
NOSWITCH			LDA	KBDBUF			; read keyboard for ^C
					AND	#$7F			;
CHECKC				CMP	#$03			; compare to ^C
					RTS					; returns with Z bit set (result ZERO) if CTL-C

STOPG				LDA	KBDBUF			; read keyboard for ^C
					AND	#$7F			; 0111 1111 - clears high bit.

CHECKF				CMP #$06			; ^F for full SCREEN
					BNE CHECKB			; not F, might be B
					JSR CTRLF			; is F - do fullscreen
					LDA #$01			; clears zero bit
					RTS

CHECKB				CMP #$02			; CTRL B = load BASIC Program
					BNE CHECKC			; nope? Check ^C
					LDA #$FF			; clears zero bit, sets sign bit
					RTS

		
; ----------------------------------------------------------------------------
DOBRKS				LDA	#$16           ;                            
        			JSR	BASCALC        ;                            
        			LDA	#$0E           ;                            
        			STA	HPOS           ;                            
        			LDX	#$0B           ;                            
PRBRK   			LDA	BREAKPOINTS,x  ; print "BREAKPOINTS"                         
        			JSR	COUT           ; 
        			DEX	               ; 
        			BNE	PRBRK          ; 
        			LDA	#$17           ; down a line to print addresses
        			JSR	BASCALC        ; 
        			LDX	#$00           ; 
        			STX	HPOS           ; 
        			LDA	PNT+$01        ; pointer+1 == 0?
        			BEQ	READREGS          ; 
PRBRKS  			LDA	BRKTBL,X       ; print breakpoint addresses
        			JSR	PRBYTE         ; 
        			INX	               ; 
        			TXA	               ; 
        			LSR	A              ; 
        			BCS	PRBRKS          ; 
        			SEC	               ; 
        			SBC	PNT+$01        ; 
        			BEQ	READREGS          ; 
        			LDA	#$A0           ; space
        			JSR	COUT           ; print space between breakpoints.
        			JMP	PRBRKS         ; 

; ----------------------------------------------------------------------------
READREGS						; Read in edited registers ???
					LDX	#$26           ; 
        			JSR	EDLIN      		; clear strobe
        			JSR	CLREOL         ; clear to end of line
        			LDA	#$00           ; zero out
        			TAY	               ; 
        			TAX	               ; 
        			STA	PNT+$01        ; next instruction = 0 ???
        			STA	FLAG           ; flag = 0
EACHCHAR   	
					LDA	(CURPOS),Y     ; load ACCUMULATOR with char at cursor position
        			CMP	#$A1           ; compare with printable ascii (non-space)
        			INY	               ; next char on line
        			BCC	DONELINE       ; <A1 is space. skip over.
        			CMP	#$BA           ; BA=colon 
        			BCC	ATOHEX          ; <BA is a number
        			ADC	#$08           ; not a number, add 8 + carry (A=#$41, ==#$4A)
ATOHEX  			AND	#$0F           ; Get lower nibble (convert ascii to hex) 
        			STA	SRC            ; 
        			LDA	BRKTBL,X       ;
        			ASL	A              ; 
        			ASL	A              ; 
        			ASL	A              ; 
        			ASL	A              ; 
        			ORA	SRC            ; 
        			STA	BRKTBL,X       ;
        			INC	FLAG           ; 
        			LDA	FLAG           ; 
        			LSR	A              ; 
        			BCS	DONELINE          ; 
        			INX	               ; 
        			TXA	               ; 
        			LSR	A              ; 
        			BCS	DONELINE          ; 
        			INC	PNT+$01        ; 
        			LDA	#$08           ; 
        			CMP	PNT+$01        ; 
        			BEQ	DONEREAD          ; 
DONELINE		
					CPY	#$27           ; done with full line?
        			BCC	EACHCHAR       ; nope. go back to next char
DONEREAD	
					JMP	DISPLAYLOOP    ; done reading registers.

; ----------------------------------------------------------------------------
NEXTPC 				LDX	PNT+$01        ; next instruction
       				BEQ	INXRTS         ; instruction == 00, NOP?
       				TXA	               ; 
       				ASL	A              ; 
       				TAX	               ; 
NEXTPCH				LDA	PCH            ; load counter Hi
       				CMP	L0BA6,X        ; 
       				BNE	XMINUS2			; 
       				LDA	PCL            ; 
       				CMP	L0BA7,X        ; 
       				BNE	XMINUS2			; 
       				LDY	#$73           ; 
       				JMP	BELL2          ; BEEP! (BREAKPOINT) jump to bell and RTS
	
; ----------------------------------------------------------------------------
XMINUS2				DEX	               ; decrement x
       				DEX	               ; twice
       				BNE	NEXTPCH          ; still not zero, ???
INXRTS 				INX	               ; If ZERO, increment X and RTS
       				RTS	               ; x=1

; ----------------------------------------------------------------------------
CKINTE			; chECK FOR CTRL+(P-W) interrupt?
					CMP	SCR            ; A == scratchpad?
       				BEQ	RTS2          ; yes, RTS
       				STA	SCR            ; no, store A in scratchpad
       				AND	#$60           ; AND 0110 0000 = if zeroes out, not a number or A-F. ???
       				BNE	RTS2          ; both bits zero, RTS
       				LDA	KBDBUF         ; otherwise load KEYBOARD
       				ASL	A              ; shift left?
       				ASL	A              ; multiply by 2
       				ASL	A              ; three times.
       				CMP	#$B9           ; 1011 1001
       				BCS	RTS2          ; greater than #$B9 == ctrl+(P-W) key set for reset
       				STA	PNT            ; pointer to reset target for interrupt 0-7 
RTS2   				RTS	               ; 

; ----------------------------------------------------------------------------
; SETUP 8080 REGISTERS
REGSETUP 	
        			LDX	#$00           	; ZERO
        			
        			STX	PNT+$01        	; pointer Hi
        			STX	PNT            	; pointer Lo
        			STX	PCL            	; PC LO
        			STX	PCH            	; PC HI ****
        			STX HIPCH			; is PCH above $7F?
        			STX	SCR            	; scratchpad
        			STX SIODATAIN		; clear SIO DATA for use
        			STX SIODATAOUT		; clear SIO DATA for use
        			INX	               	; ONE
        			STX	INTE           	; Interrupt enable = 1
        			INX	               	; TWO
        			STX	PSW            	; processor status = 00000010
	
        			LDX	#$FF           ; $11-$12 == #$ff
        			STX	DECIT          ; 
        			STX	DECIT+$01      ; 
        			STX SPL
        			INX	               ; 
        			STX	INCIT+$01      ; $13-$14 == #$00
        			STX	HPOS           ; cursor to 0
        			INX	               ; 
        			STX	INCIT          ; $13-$14 == 01 00
		
					LDX #$0F
					STX SPH			
			
        			RTS	               ; 0E74 60                                      
; ----------------------------------------------------------------------------
DSKSPIN				LDX DSKSECTOR		; looking for a specific sector, but no way to tell which one
					INX					; increment DSKSECTOR through #$1F					

					CPX #$20			; SECTOR 32
					BEQ RESETTRACKPOINTER ; set everything to 00

					STX DSKSECTOR		; set new DSKSECTOR
					TXA					; into ACCUMULATOR
					CLC

SETSECTORIN			ROL					; ROL and set as DSKSECTORIN
					STA DSKSECTORIN		; bit 0 always low == sector TRUE (ready to read/write)

					LDA TRACKPOINTER	; add 137 bytes to TRACKPOINTER
					CLC
					ADC #$89			; add 137
					STA TRACKPOINTER
					BCC	DSKSPIN2
					INC TRACKPOINTER+1
DSKSPIN2			LDA #$00			; reset to read/write at beginning of next sector
					STA SECTOROFFSET
					RTS

RESETTRACKPOINTER	LDA BASICBUFFERLO
					STA	TRACKPOINTER
					LDA BASICBUFFERHI	
					STA TRACKPOINTER+1
					LDA #$00
					STA DSKSECTOR
					STA DSKSECTORIN
					STA SECTOROFFSET
					RTS				

DSKIN				; get data from virtual DSK track
					LDY SECTOROFFSET		; which byte to grab
					LDA TRACKPOINTER
					STA $0
					LDA TRACKPOINTER+1
					STA $1
					LDA ($0),Y				; grab byte from track pointer + offset
					STA DSKREAD
					INC SECTOROFFSET		; ready for next read
					RTS

; virtual serial interface
SIOOUT				; JUST GOT A BYTE FROM SIM80
					LDA SRC			; if output is 00, setting status.
					BEQ SIORTS		; 00 -> ignore for now
					
					AND #$0F		; 1x => 0x
					CMP #$01		; is it on port 1 or 11? 
					BNE DSKOUT		; if not, is it output to DSK?
					
					; LOAD BYTE FROM SIODATA
					LDA SIODATAOUT		; load SIO DATA

					; translate CR/LF into newline.
					; CR = 0D
					; LF = 0A
					CMP #$0A
					BEQ SIORTS 			; ignore LF, just do CR.

					ORA #$80			; add the high bit?
					
					
					; OUTPUT TO SCREEN
					JSR COUT				; output char
					
SIORTS				SEC
					RTS

SIOIN				; SIGNALLED FROM IOIN THAT SIM80 LOOKING FOR A BYTE.
					; looking for a byte from IN 1 or 0?
					LDX #$00
					LDA SRC			; if input is 00, checking SIO status.
					
					CMP #$ff		; sense switches. 
					BEQ SENSEIN
					
					CMP #$0A		; looking for DISK DATA
					BEQ DSKIN	
					
					CMP #$09		; looking for DISK SECTOR
					BEQ DSKSPIN		; "spin" the disk
					
					CMP #$10		; SIO2 on port 10/11. SIO1 on port 0/1
					BCC SIO1IN		
					INX 
					INX				; use SIO2 status bytes instead of SIO1
					JMP SIO2IN	
					
SIO1IN				;AND #$0F		; kill hi nibble so 10==00
					BNE SIOKEY		; 00? status check. Otherwise, expects a byte.
					STA SIODATAIN	; clear the pipes on status check, 00 to data
					JMP SIOREADY	; set status to ready for I/O
					
					; CHECK KEYBOARD
SIOKEY				LDA KBDBUF		; read keyboard
					STA KBDSTROBE	; clear strobe

					; IF KEY WAITING, LOAD IT IN SIODATA
					BPL SIORTS			; no key. RTS.
;					BPL CHECKKSW		; if bit8/carry, has keyboard input. else check for BASIC LOAD
					STA SIODATAIN		; put ascii in DATA

					; SET SIOSTATUS TO 00 = HAVE DATA
					LDA SIOSTATUSBYTES,X		; set STATUS to 00 - HAVE DATA/READY FOR OUTPUT
					STA SIOSTATUSIN
					RTS

					; NO KEY WAITING
					; SET SIOSTATUS 01 = STILL WAITING INPUT
;CHECKKSW			JSR KSW				; CHECK FOR KSW INPUT FROM BASIC LOAD
;										; should only run on input and if src = 1?
;					BPL SIORESET		; if high bit set, have a byte from BASIC LOAD. otherwise, return

SIOREADY			; SET SIOSTATUS TO 00 = HAVE DATA
					LDA SIOSTATUSBYTES,X		; set STATUS to 00 - HAVE DATA/READY FOR OUTPUT
					STA SIOSTATUSIN
					RTS

SIORESET			INX
					LDA SIOSTATUSBYTES,X		; 01 = WAITING FOR INPUT/READY FOR OUTPUT
					STA SIOSTATUSIN	;
					RTS

SENSEIN				AND #$0F
					CLC
					ROL 
					TAY
					LDA IOTBLIN,Y		; sense switches up/down don't have an IN/OUT version
					STA DESTDA+1	
					INY			
					LDA IOTBLIN,Y
					STA DESTDA
					RTS


SIO2IN				AND #$0F		; #$10 -> #$00
					BEQ SIO2STATUS
					; SIO2 data input
					LDA KBDBUF		; get the key
					STA KBDSTROBE	; clear the strobe
					STA SIODATAIN	; store it in datain buffer
					JMP SIORESET	; input done. set status to waiting for next check.


SIO2STATUS			; looking for status of SIO2 - have a byte, byte sent, or 00 waiting
					LDA KBDBUF					; if KBDBUF bit 8 hi, have a byte
					BPL	SIORESET				; bit8 lo, set status to byte sent/waiting
					JMP SIOREADY				; bit 8 hi, has keyboard input.
					
DSKOUT				CMP #$0A					; port 0A -> output to DSK
					BNE SIORTS					; not 0A? -> ignore

					LDY SECTOROFFSET		; which byte of the current sector to push to
					LDA TRACKPOINTER		; offset within track ($89xSECTOR)
					STA $0				
					LDA TRACKPOINTER+1
					STA $1
					LDA DSKWRITE			; load the byte
					STA ($0),Y				; set byte at track pointer + offset
					INC SECTOROFFSET		; ready for next read
					RTS


; ----------------------------------------------------------------------------


FULLSCREEN			ASC "   FULL SCREEN MODE: CONTROL-C TO EXIT  ",00
LASTLINEBUF			DS	41,$00			; 40 bytes to store last line of split screen text. followed by zero to stop.

CTRLF				LDA TEXTTOP		; control-F for full-screen terminal
					BEQ CTRLFRTS	; already in full-screen. ignore.

					LDX #$27		; copy last line of text from $7d0
BUFFERLINE			LDA $7D0,X
					STA LASTLINEBUF,X
					DEX
					BPL BUFFERLINE

					BIT TXTSET		; set text Mode
					LDA #$00
					STA	TEXTTOP		; to make scroll full screen.
					JSR HOME		; clear low res screen

; Ask for 80 columns?
					JSR ASK80		; Returns with ZERO on YES
					BNE CTRLFGO
					LDA #$00		; clears out the Y/N from accumulator
					JSR $C300

CTRLFGO				LDY #$3F
					JSR $FE86

					LDA #$00
					STA $F3			; no blinking text?

					LDY #>FULLSCREEN
					LDA #<FULLSCREEN
					JSR STROUT				;Y=String ptr high, A=String ptr low
					JSR CROUT

					LDY #$FF
					JSR $FE86

					LDY #>LASTLINEBUF
					LDA #<LASTLINEBUF
					JSR STROUT				;Y=String ptr high, A=String ptr low
					JSR CROUT

CTRLFRTS			RTS



LINE24				LDA #$17		; put cursor at line 24
					STA VPOS
					JSR VTAB
					LDA #$00		; beginning of line
					STA HPOS
					LDA #$14		; scroll window only lines 20-24	
					STA TEXTTOP
					RTS

; ----------------------------------------------------------------------------

ASK80STRING			ASC "80 COLUMN MODE, Y/N?",00
		
ASK80				LDY #>ASK80STRING
					LDA #<ASK80STRING
					JSR STROUT				;Y=String ptr high, A=String ptr low
					JSR CROUT
					BIT KBDSTROBE			; clear ^F out of buffer
					
ASK80LOOP			LDA KBDBUF				; check for keydown
					AND #$80				; high bit set?
					BNE ASK80YN				; 
					JMP ASK80LOOP			; got a key?
ASK80YN				LDA KBDBUF
					STA KBDSTROBE
					CMP #$D9				; Y
					STA KBDSTROBE
					RTS




; ----------------------------------------------------------------------------
EDITREGS 
        			LDY	#$00           ;
        			JSR	SETREG         ;
        			STA	PSW            ;
        			INY	               ;
        			JSR	SETREG         ;
        			STA	SIMA           ;
        			INY	               ;
        			JSR	SETLOHI        ;
        			STA	SIMC           ;
        			STX	SIMB           ;
        			INY	               ;
        			JSR	SETLOHI        ;
        			STA	SIME           ;
        			STX	SIMD           ;
        			INY	               ;
        			JSR	SETLOHI        ;
        			STA	SIML           ;
        			STX	SIMH           ;
        			INY	               ;
        			JSR	SETLOHI        ;
        			STA	SPL            ;
        			STX	SPH            ;
        			INY	               ;
        			JSR	SETLOHI        ;
        			STA	PCL            ;
        			STX	PCH            ;
        			RTS	               ;

; ----------------------------------------------------------------------------
SETLOHI 
					JSR	SETREG         ;
SETREG 				TAX	               ;
       				LDA	#$01           ;
       				STA	FLAG           ;
       				LDA	#$00           ;
SETSRC 				STA	SRC            ;
       				LDA	(CURPOS),Y     ;
       				INY	               ;
       				CMP	#$BA           ;
       				BCC	ATOHEX2          ;
       				ADC	#$08           ;
ATOHEX2				AND	#$0F           ; ASCII 0-F to hex
       				ADC	SRC            ;
       				DEC	FLAG           ;
       				BNE	RTS3           ;
       				ASL	A              ;
       				ASL	A              ;
       				ASL	A              ;
       				ASL	A              ;
       				BCC	SETSRC          ;
RTS3   				RTS	               ;

; ----------------------------------------------------------------------------


PCHFIX
 					; **** add $10 to PCH
					CLC
					LDA PCH
					ADC #$10
					STA PCH
					; ****
					STA DISPLAYBYTES	; high byte to first bank of LEDs.

					CMP #$8F			; if HIPCH, read from AUX
					BCC PCHFIX2
					STA RAMRDON
					SBC #$80
					STA PCH
					LDA #$01
					STA HIPCH

PCHFIX2        		LDA	(PCL),Y        ; print registers values

        			STA	OPCODE         ;              
					
					LDA HIPCH
					BEQ PCHFIX3
					STA RAMRDOFF
					CLC
					LDA PCH
					ADC #$80
					STA PCH

 					; **** subtract $10 from PCH
PCHFIX3				SEC
					LDA PCH
					SBC #$10
					STA PCH
					; ****
					RTS
					


; CLEAR AND REDRAW SCREEN
CLRSCRN 	
        			LDX	#$00           ;  
PRREGS 	
		        	LDY	#$00           ;                       
        			STY	HPOS           ; back to left side to  
        			                      
 					; **** add $10 to PCH
					CLC
					LDA PCH
					ADC #$10
					STA PCH
					; ****
					STA DISPLAYBYTES	; high byte to first bank of LEDs.

					CMP #$8F			; if HIPCH, read from AUX
					BCC PROPCODE
					STA RAMRDON
					SBC #$80
					STA PCH
					LDA #$01
					STA HIPCH

PROPCODE        	LDA	(PCL),Y        ; print registers values

        			STA	OPCODE         ;              
					
					LDA HIPCH
					BEQ PROPCODE2
					STA RAMRDOFF
					CLC
					LDA PCH
					ADC #$80
					STA PCH

 					; **** subtract $10 from PCH
PROPCODE2			SEC
					LDA PCH
					SBC #$10
					STA PCH
					; ****
					
					LDA	#$15           ; move cursor to line 22                                   
        			JSR	BASCALC        ;   
        			LDA	PSW            ;   
        			JSR	PRREG          ; PS = Processor Status byte
        			LDA	SIMA           ;   
        			JSR	PRREG          ; AC = Accumulator 
        			LDA	SIMB           ;   
        			LDX	SIMC           ;   
        			JSR	PRLOHI         ; BC = B & C Registers  
        			LDA	SIMD           ;  
        			LDX	SIME           ;   
        			JSR	PRLOHI         ; DE = D & E Registers
        			LDA	SIMH           ;   
        			LDX	SIML           ;   
        			JSR	PRLOHI         ; HL = H and L Registers
        			LDA	SPH            ;   
        			LDX	SPL            ;   
        			JSR	PRLOHI         ; SP = Stack Pointer HiLo
        			LDA	PCH            ; 
        			LDX	PCL            ; 
        			JSR	PRLOHI         ; PC = SIM80 Program Counter Hi and Lo 
        			LDA	OPCODE         ;    
        			JSR	PRREG          ; OP = current opcode 
        			LDA	#$01           ;   
        			CMP	INTE           ; INTE = Interrupt Enable (IE)
        			BNE	BRKYN          ; not 1, set zero for output.
        			LDA	#$00           ;   
BRKYN   			JSR	PRREG          ; Y or N on breakpoints
        			LDA	#$CE           ;   
        			LDX	PNT+$01        ;   
        			BEQ	BRKPTS         ;   
        			LDA	#$59           ;   
BRKPTS  			JSR	COUT           ;   
	
GRABLEDS	
					LDX	PCL            ; 
					STX	DISPLAYBYTES+1
					LDA	OPCODE         ;    
					STA	DISPLAYBYTES+2
	
					RTS	
;/CLRSCRN

PRLEGEND			LDA	#$14           ; down to line 20
       				JSR	BASCALC
					LDX #$00
					STX HPOS
NEXTLEGEND       	LDA	LEGEND,x       	; 
       				JSR	COUT           	;
       				INX					;
       				CPX	#$27           	;
       				BNE	NEXTLEGEND      ;

					RTS	


; Display bytes as LEDS
UPDATELEDS			BCC	DISPLAYPCH		; carry clear, show PCH
	
DISPLAYREGD			;LDA	DISPLAYBYTES           	; carry set, show SIMD
					BCS	STABYTE			
DISPLAYPCH 			LDA	PCH					
					STA	DISPLAYBYTES 		; DISPLAY LAST MEMORY ACCESS (LDAX/STAX/ETC) WHILE TRACE
STABYTE
					JSR GRABLEDS			; update display bytes from registers.
					LDA	#$01				; down one row to use COUT
					JSR	BASCALC
					LDA	#$16				; over 22 for COUT
					STA	HPOS

					LDX	#$02				; opcode byte
					JSR BYTELOOP

					LDA	#$03				; down one row to use COUT
					JSR	BASCALC
					LDA	#$06				; over 22 for COUT
					STA	HPOS

					LDX #$00				; PC HI
					JSR BYTELOOP

					LDX #$01				; PC LO
					JSR BYTELOOP

					RTS                    ;   DONE DISPLAYING

NEXT				JSR	INCPC          		; next instruction
					LDY	#$00				; down
					JMP	EXAMINE2	
EXAMINE	
					LDY	#$01			; up
					LDA	SWITCHBYTES
					STA	PCH
					LDA	SWITCHBYTES+1
					STA	PCL
EXAMINE2		
					LDX	#$03			; switch 3	
					JSR	BOTTOMROW
					JSR	BOTTOMROWRST	; reset the switch locations
					JMP	DISPLAYLOOP        ; back to read keys
	
	
DEPOSITNEXT			
					JSR	INCPC			; increment program counter, then dePosit
					LDY	#$00			; up
					JMP	DEPSW
DEPOSIT		
					LDY	#$01			; up
DEPSW				LDX	#$04			; switch 4	
					JSR	BOTTOMROW
					
					CLC					; add 10 to PCH before deposit
					LDA PCH
					ADC #$10
					STA PCH
					
					LDX	#$00
					LDA	SWITCHBYTES+1
					STA	(PCL,X)        ;  
					
					SEC					; subtract 10 after
					LDA PCH
					SBC #$10
					STA PCH		
					
					JSR	BOTTOMROWRST	; reset the switch locations
					CLC					; clear carry for PCH display
					JSR	UPDATELEDS
					JMP	DISPLAYLOOP        ; back to read keys




; ---------------------------------------------------------------------------

ISITHEX							
					TAY	               ; hold keychar in Y.
        			AND	#$60           ; renders anything 20 30 40 ???
        			BEQ	DONEHEX			; if zeroes out, not a number or A-F. ???
        			TYA	               ; keychar back to A
        			JSR	CKNUMS          ; if a number or A-F, returns with Carry Set
        			BCC	DONEHEX			; carry clear, not a number 0-F, start over
DONEHEX				RTS	
	
	
CKNUMS				EOR	#$B0           ; B0 = ascii 0, 
					CMP	#$0A           ; compare to 10, number from 0 - 9
					BCC	SETCARRY       ; set carry and RTS (is a number)
					ADC	#$88           ; 
					CMP	#$FA           ; < FA not a letter A-F
					RTS	               ; 

; ----------------------------------------------------------------------------
SETCARRY 
					SEC                    ;
					RTS                    ;

; ---------------------------------------------------------------------------
SETSWITCH	
					TYA					; Y back to accum.
      				CMP	#$BA           ; BA=colon 
      				BCC	ATOHEX3          ; <BA is a number
      				ADC	#$08           ; not a number, add 8 + carry (A=#$41, ==#$4A)
ATOHEX3				AND	#$0F           ; Get lower nibble (convert ascii to hex) 
	
					TAX					; A becomes loop number to set switch bit.
					INX					; 0 becomes 1 (loop)
	
WHICHBANK			CMP	#$08			; A >/= 8, and we do SWITCHBYTES+0 carry set
					LDA	#$00			; A < 8, and we do SWITCHBYTES+1 carry clear
				
					ADC	#$00			; if carry, 0. If not, 1
					EOR	#$01			; flip bit 1
				
					CMP	#$01			; carry set, >7, so needs an extra loop
					BCS	SETBANK			; carry clear, 0-7
									
					INX					
SETBANK				TAY					; bank 0 or 1
	
					SEC					; set carry bit to rotate into accumulator
					LDA	#$00
ROTATEIN			ROL					; rotate the bit into 
					DEX	
					BNE	ROTATEIN
				
					EOR	SWITCHBYTES,Y
					STA	SWITCHBYTES,Y
				
					JSR	UPDATESWITCHES	; update the display
				
					RTS	

; ---------------------------------------------------------------------------

; Display bytes as sWITCHES
UPDATESWITCHES
		BIT			SPKR			; click

					LDA	#$06				; down row to use COUT
					JSR	BASCALC
					LDA	#$06				; over for COUT
					STA	HPOS
					LDX #$00				; PC HI
					JSR SWITCHLOOP

					LDA	#$06				; down row to use COUT
					JSR	BASCALC

					LDX #$01				; PC LO
					JSR SWITCHLOOP
					RTS
			

SWITCHLOOP			LDY	#$08				; do a byte's worth
					LDA SWITCHBYTES,X		; put the switch status into scratch
					STA SWITCHSCRATCH,X
SWLOOP				LDA	#$AA				; gray
					JSR	COUT				; output gray between switches
					LDA #$25				; switch DOWN
					CLC						; clear carry
					ROL SWITCHSCRATCH,X		; rotate opcode, hi bit into carry
					BCC	SETSW				; if Carry clear, skip
					LDA #$26				; switch UP
SETSW				JSR	COUT				; output to screen
					DEY						; done all 8 bits?
					BNE SWLOOP
											; bottom pixels

					LDA SWITCHBYTES,X		; put the switch status into scratch
					STA SWITCHSCRATCH,X

					LDA	#$07				; down row to use COUT
					JSR	BASCALC
					LDA	HPOS				; over for COUT
					SEC
					SBC #$10
					STA	HPOS

SWITCHLOOP2			LDY	#$08				; do a byte's worth
					LDA SWITCHBYTES,X		; put the switch status into scratch
					STA SWITCHSCRATCH,X
SWLOOP2				LDA	#$AA				; gray
					JSR	COUT				; output gray between switches
					LDA #$A6				; switch DOWN
					CLC						; clear carry
					ROL SWITCHSCRATCH,X		; rotate opcode, hi bit into carry
					BCC	SETSW2				; if Carry clear, skip
					LDA #$A5				; switch UP
SETSW2				JSR	COUT				; output to screen
					DEY						; done all 8 bits?
					BNE SWLOOP2
					BIT SPKR				; click
	
					RTS




; ---------------------------------------------------------------------------
BYTELOOP			LDY	#$08				; do a byte's worth
BITLOOP				LDA	#$AA				; gray
					JSR	COUT				; output gray between lights
					LDA #$A8				; LED OFF
					CLC						; clear carry
					ROL DISPLAYBYTES,X		; rotate opcode, hi bit into carry
					BCC	SETLED				; if Carry clear, skip
					LDA #$A1				; LED ON
SETLED				JSR	COUT				; output to screen
					DEY						; done all 8 bits?
					BNE BITLOOP
					RTS


WAITLED				LDY #$A8				; light off
					BCC SETWAIT				; Carry clear, lamp off
					LDY #$A1				; Carry set, light on
SETWAIT				LDA	#$03				; down one row to use COUT
					JSR	BASCALC
					LDA	#$02				; over 22 for COUT
					STA	HPOS
					TYA
					JSR	COUT				; output to screen
					RTS	
					


; ----------------------------------------------------------------------------
BOTTOMROW									; flip switches
								
											; six switches X = number of loops	
											; Y =1 for up, =0 for down.				
					BIT	SPKR			; click
        			LDA	#$04    		; start at column 4
        			STA	HPOS    
BRSWLOOP									; bottom row switch loop.
					INC	HPOS
					INC	HPOS			; over two spaces
					INC	HPOS			; over two spaces
					DEX					; loop X times
					BNE	BRSWLOOP		; switch last one up/down
	
					LDA	#$0A 			; row 10
        			JSR	BASCALC 
					LDA	#$55			; #$55 is down
					CPY	#$0				; is Y 0 or 1?
					BEQ	SWDOWN1
					LDA	#$52			; #$52 is up
			
SWDOWN1				JSR	COUT			; put switch pixels
	
					LDA	#$0B			; Down to row 11
        			JSR	BASCALC 
					DEC	HPOS			; back over 1 px
					LDA	#$A2			; #$A2 is down
					CPY	#$0				; is Y 0 or 1?
					BEQ	SWDOWN2
					LDA	#$A5			; #$A5 is up/center
SWDOWN2				JSR	COUT			; put switch pixels
	
					BIT	SPKR			; click
					LDA	#$80			
					JSR	WAIT			; delay to show switch action.
					RTS	
		


BOTTOMROWRST					
					LDX	#$04			; four of six switches X = number of loops	
					LDA	#$05    		; start at column 4
					STA	HPOS    
BRRSLOOP	
					LDA	#$0A 			; row 10
					JSR	BASCALC 
	
					INC	HPOS
					INC	HPOS			; over two spaces
	
					LDA	#$25			; #$25 is centered
					JSR	COUT			; put switch pixels
	
					LDA	#$0B			; Down to row 11
					JSR	BASCALC 
					DEC	HPOS			; back over 1 px
	
					LDA	#$A5			; #$A5 is center
					JSR	COUT			; put switch pixels
	
					DEX					; loop X times
					BNE	BRRSLOOP		; switch last one up/down


					RTS



; ---------------------------------------------------------------------------
PRLOHI  			JSR	PRBYTE         ; prints a pair of registers
        			TXA	               ;
PRREG   			JSR	PRBYTE         ; print hex of one register, followed by space (#$A0)
        			LDA	#$A0           ;
        			JMP	$FDED          ; JMP to COUT with RTS ?

MOVETXT								; move the current text page to alt text page
					LDA	#$00			; Setup pointers to move memory
					STA	$3C			; $3C and $3D for source start
					LDA	#$04
					STA	$3D

					LDA	#$FF
					STA	$3E			; $3E and $3F for source end
					LDA	#$07
					STA	$3F			; 

					LDA	#$00
					STA	$42			; $42 and $43 for destination
					LDA	#$08			; $1000 == destination
					STA	$43
					LDA	#$00			; Clear ACC, X,Y for smooth operation
					TAX
					TAY
					JSR	$FE2C    		; F8ROM:MOVE	; Do the memory move
					RTS

ALTAIRSCREEN
									; move graphic data to $4000

					LDA	ALTAIRLO		; Setup pointers to move memory
					STA	$3C			; $3C and $3D for source start
					LDA	ALTAIRHI
					STA	$3D

					LDA	ALTAIRLO
					STA	$3E			; $3E and $3F for source end
					LDA	ALTAIRHI
					CLC
					ADC	#$04			; add $400 to start == end of graphic
					STA	$3F			; 

					LDA	#$00
					STA	$42			; $42 and $43 for destination
					LDA	#$50			; $4000 == destination
					STA	$43
					LDA	#$00			; Clear ACC, X,Y for smooth operation
					TAX
					TAY
					JSR	$FE2C    		; F8ROM:MOVE	; Do the memory move



					LDA	#$15			; Kill 80-Column mode whether active or not
					JSR	$FDED    		; F8ROM:COUT
					
					STA	$C050   		; rw:TXTCLR	; Set Lo-res page 1, mixed graphics + text
					STA	$C053   		; rw:MIXSET
					STA	$C054   		; rw:TXTPAGE1
					STA	$C056   		; rw:LORES

										; display the data from $4000 at $400					
RESETVPTR			LDA	#$00			; Move titlepage from $4000 to $400 (screen)
					STA	$FE				; pointer for where we are at vertically on screen
					TAY					; Y-Reg used for indexing across (horiz) screen
VERTICALPTR			LDA	$FE				; pointer for where we are at vertically on screen
					JSR	$F847    		; F8ROM:GBASCALC
	
					LDA	$26	
					STA	$FA				; $FA is our offset GBASL Byte (Source data titlepage)
	
					LDA	$27				; Add 04 w/ Carry to get to $4000 where graphic data is
					ADC	#$4C	
					STA	$FB				; $FB is our offset GBASH Byte (Source data titlepage)
						
GRABSTORAGE			LDA	($FA),Y			; Grab from storage
					STA	($26),Y			; Put to screen
					INY	
					CPY	#$28			; #$28 past the width of screen?
					BNE	GRABSTORAGE		; No?  Back for another round
					LDA	#$00	
					TAX	
					TAY	
	
						
					INC	$FE				; Next line down vertically
					LDA	#$00	
					TAX	
					TAY
					LDA	$FE
					CMP	#$15			; #$18 bottom of screen - go to line 21 for legend
					BNE	VERTICALPTR		; No? Go back and do next line down
					
					
					LDA	#$00
					STA	KBDSTROBE    		; r:KBDSTRB 	; Clear keyboard strobe
					
									; LOOP HERE TO WAIT FOR KEYPRESS ???

					RTS			; We now return you to your regular programming

;/ALTAIRSCREEN



**************************************************
*	writes instructiONS
**************************************************

COPYRIGHT			ASC	"APPLE-80 COPYRIGHT 1979 BY DANN MCCREARY",00	; ascii copyright info
LINE1				ASC "     SIM8800 BY CHARLES MANGIN, 2019", 00

LINE2				ASC "KEYBOARD COMMANDS:",00

LINE3				ASC "    "
					INV	'0-9'
					ASC	", "
					INV	'A-F'	 
					ASC	" TOGGLES SWITCHES 0-15",00

LINE4				ASC "    "
					INV 'X'	 
					ASC	" TO EXAMINE MEMORY",00

LINE5				ASC "    "
					INV 'N'	 
					ASC	" FOR NEXT ADDRESS, "	 
					INV '^'
					ASC	" FOR PREVIOUS",00

LINE6				ASC "    "
					INV 'P'	 
					ASC	" TO DEPOSIT BYTE INTO MEMORY",00

LINE6A				ASC "    "
					INV 'O'	 
					ASC	" TO DEPOSIT NEXT BYTE",00

LINE7				ASC "    "
					INV 'SPACE'
					ASC	" OR "
					INV	'S'	 
					ASC	" TO EXECUTE 1 STEP",00

LINE8				ASC "    "
					INV 'T'	 
					ASC	" TO START INTERACTIVE TRACE",00

LINE9				ASC "    "
					INV 'G'	 
					ASC	" TO RUN PROGRAM",00

LINE9A				ASC "    "
					INV 'CTRL-C'	 
					ASC	" TO STOP RUN/TRACE",00

LINE9B				ASC "    "
					INV 'CTRL-F'	 
					ASC	" TO RUN FULL SCREEN",00


LINE10				ASC "    "
					INV 'K'	 
					ASC	" TO SET BREAKPOINTS",00

LINE11				ASC "    "
					INV 'R'	 
					ASC	" TO EDIT REGISTERS",00

LINE12				ASC "    "
					INV 'I'	 
					ASC	" FOR THESE INSTRUCTIONS",00

ANYKEY				ASC "        PRESS SPACE TO CONTINUE",00


INSTRUCTIONSTBL		DB	#>LINE2,#<LINE2,#>LINE3,#<LINE3,#>LINE4,#<LINE4,#>LINE5,#<LINE5,#>LINE6,#<LINE6
					DB	#>LINE6A,#<LINE6A,#>LINE7,#<LINE7,#>LINE8,#<LINE8,#>LINE9,#<LINE9,#>LINE9B,#<LINE9B
					DB	#>LINE9A,#<LINE9A,#>LINE10,#<LINE10,#>LINE11,#<LINE11,#>LINE12,#<LINE12


CHOOSER1			ASC	"PRESS A NUMBER TO LOAD A PROGRAM:",00
CHOOSER2				ASC "   1 FOR KILL-THE-BIT",00
CHOOSER3				ASC "   2 FOR 4K BASIC",00
CHOOSER4				ASC "   3 FOR 8K BASIC",00
CHOOSER5				ASC "   4 FOR MICROCHESS",00
CHOOSER6				ASC "   5 FOR DBL ROM AT $00",00
CHOOSER7				ASC "   0 FOR NO PROGRAM",00

CHOOSERTBL			DB	>CHOOSER1,<CHOOSER1,>CHOOSER2,<CHOOSER2,>CHOOSER3,<CHOOSER3
					DB	>CHOOSER4,<CHOOSER4,>CHOOSER5,<CHOOSER5,>CHOOSER6,<CHOOSER6



CHOOSER				LDX #$00

CHOOSERLOOP			LDY CHOOSERTBL,X
					INX
					LDA CHOOSERTBL,X
					JSR STROUT				;Y=String ptr high, A=String ptr low
					JSR CROUT
					INX
					CPX #$0C
					BNE CHOOSERLOOP

CHOOSERWAITLOOP		LDX #$00
					LDA KBDBUF				; check for keydown
					CMP #$AF				; greater or equal B0 = number?
					BCS ISITNUMBER
					JMP CHOOSERWAITLOOP		; less than B0, NaN
ISITNUMBER			CMP #$B6				; less than B6 = 0-5 ?
					BCC CHOOSERDONE			; number 0-5, set it in X
					JMP CHOOSERWAITLOOP		; loop for next key
					
CHOOSERDONE			SEC
					SBC #$B0				; convert Bx => 0x
					TAX
					RTS



SHOWINST
					STA TXTPAGE2			; alternate page
					JSR	SETTXT				; sets text mode
				
INSTLOOP			LDA KBDBUF				; check for keydown
					CMP #$A0
					BEQ RETURNSIM		; 
					JMP INSTLOOP		; got a key?

RETURNSIM			STA KBDSTROBE			; clear key
					STA LORES
					STA MIXSET
					STA TXTPAGE1			; main text/gr page
					JMP DISPLAYLOOP			; back to it.


INSTRUCTIONS		LDA #$00
					STA VPOS
					JSR VTAB

					LDY #>COPYRIGHT
					LDA #<COPYRIGHT
					JSR STROUT				;Y=String ptr high, A=String ptr low
					JSR CROUT
								
					LDY #>LINE1
					LDA #<LINE1
					JSR STROUT				;Y=String ptr high, A=String ptr low
					JSR CROUT

					LDA #$06
					STA VPOS
					JSR VTAB

					LDX #$00
PRINSTLOOP			LDY INSTRUCTIONSTBL,X
					INX
					LDA INSTRUCTIONSTBL,X
					JSR STROUT				;Y=String ptr high, A=String ptr low
					JSR CROUT
					INX
					CPX #$1C
					BNE PRINSTLOOP

					LDA #$17
					STA VPOS
					JSR VTAB
					LDY #>ANYKEY
					LDA #<ANYKEY
					JSR STROUT				;Y=String ptr high, A=String ptr low


ALTAIRLOOP			LDA KBDBUF				; check for keydown
					CMP #$A0
					BEQ STARTSIM		; 
					JMP ALTAIRLOOP		; got a key?

STARTSIM			STA KBDSTROBE

										; copy text page to alt text page for later display
					JSR MOVETXT					
	

					JSR HOME
					RTS					; We now return you to your regular programming
				

;/INSTRUCTIONS


STROUT  			STA	$00
					STY $01
					LDY #$00
		
CHAROUT				LDA	($0),Y 
					BEQ DONESTR			; char == 00, EOL
					JSR	COUT			; print char
					INY					; next char
					BNE	CHAROUT			; just to be sure it stops after FF chars.
DONESTR				RTS



**************************************************
*	Load basic program, then "type" it to screen
**************************************************

CTRLB				STA KBDSTROBE		; clear strobe
					LDX #$00			; PAPERTAPE
					JSR BLOAD			; loads text into buffer after sim8800

					LDA #$00
					STA BASBUFPNT		; reset basic buffer pointer to 00
										
					LDA BASICBUFFERLO	; locate buffer start
					STA $CE
					LDA BASICBUFFERHI	; 
					STA $CF
					
					JMP GKEY2			; back to G mode.
					
					
;KSW					STY $CD				; SAVE Y
;					LDY BASBUFPNT		; byte counter
;GETBASICBUFFER		LDA ($CE),Y			; get byte from buffer			
;					BEQ KILLKSW			; if it's EOF (00) set keyboard control back
;					
;					LDA SRC				; if looking for input byte on 01/11 ?
;					AND #$0F			; kill hi nibble
;					CMP #$01
;					BNE SETHIBIT		; otherwise, set hi bit to say "data ready"
;					
;					LDA ($CE),Y			; get byte AGAIN?
;					ORA #$80			; sets high bit of incoming text
;					STA SIODATAIN			; returns with high bit set...
;
;					INC BASBUFPNT		; next pointer address
;					BNE SETHIBIT	
;INCBASICBUFFER		INC $CF				; rolled over, get next hi byte
;			
;SETHIBIT			LDY $CD				; RESTORE Y
;					ORA #$80			; sets high bit of incoming text
;					RTS
;
;KILLKSW				LDY $CD				; RESTORE Y
;					LDA #$00			; defeats BPL
;					RTS

**************************************************

PAPERTAPE		DB	ENDNAME-NAME 			;Length of name
NAME    		ASC	'/SIM8800/PAPERTAPE' 	;followed by the name
ENDNAME 		EQU	*

FILENAME1		DB	ENDNAME1-NAME1 			;Length of name
NAME1    		ASC	'/SIM8800/KILLTHEBIT'	;followed by the name
ENDNAME1 		EQU	*

FILENAME2		DB	ENDNAME2-NAME2 			;Length of name
NAME2    		ASC	'/SIM8800/BASIC'		;followed by the name
ENDNAME2 		EQU	*

FILENAME3		DB	ENDNAME3-NAME3 			;Length of name
NAME3    		ASC	'/SIM8800/BASIC8K'		;followed by the name
ENDNAME3 		EQU	*

FILENAME4		DB	ENDNAME4-NAME4 			;Length of name
NAME4    		ASC	'/SIM8800/CHESS'		;followed by the name
ENDNAME4 		EQU	*

FILENAME5		DB	ENDNAME5-NAME5 			;Length of name
NAME5    		ASC	'/SIM8800/DBL'			;followed by the name
ENDNAME5 		EQU	*

FILENAMESLO		DB	<PAPERTAPE,<FILENAME1,<FILENAME2,<FILENAME3,<FILENAME4,<FILENAME5
FILENAMESHI		DB	>PAPERTAPE,>FILENAME1,>FILENAME2,>FILENAME3,>FILENAME4,>FILENAME5

READLOCLO		DB	<BASICBUFFER,$00,$00,$00,$00,$00
READLOCHI		DB	>BASICBUFFER,$10,$10,$10,$10,$10				; remember to add $10


**************************************************
LOADPROGRAM		BEQ NOPROGRAM				; x=0, no program to load.
				JSR BLOAD					; loads BASIC

				LDA BASICBUFFERLO			; locate BASICBUFFERLO
				CLC
				ADC TRANSFERRED				; ADD TRANSFERRED
				STA $00						; put BASICBUFFERLO,$00
				LDA BASICBUFFERHI			; add BASICBUFFERHI + TRANSFERRED+1
				ADC	TRANSFERRED+1			; put BASICBUFFERHI,$01
				STA $01
				LDY #$00					; Y=0
				TYA
				STA ($00),Y					; put #$00 ($0),Y 

NOPROGRAM		RTS            				; Otherwise done

**************************************************

BLOAD										; file gets chosen by LDX before JSR BLOAD
				LDA FILENAMESLO,X
				STA OPENFILENAME
				LDA FILENAMESHI,X
				STA OPENFILENAME+1

				LDA READLOCLO,X
				STA READLOC
				LDA READLOCHI,X
				STA READLOC+1

		   		JSR	OPEN    				;open "DATA"
       			JSR READ
       			BNE ERROR					; error if return !=0
				JSR CLOSE
       			BNE ERROR					; error if return !=0
				
				RTS

OPEN 			JSR	MLI       				;Perform call
       			DB	OPENCMD    				;CREATE command number
       			DW	OPENLIST   				;Pointer to parameter list
       			BNE	ERROR     				;If error, display it
       			LDA REFERENCE
       			STA READLIST+1
       			STA CLOSELIST+1
       			RTS				

READ			JSR MLI
				DB	READCMD
				DW	READLIST
				RTS

CLOSE			JSR MLI
				DB	CLOSECMD
				DW	CLOSELIST
				RTS
				
OPENLIST		DB	$03						; parameter list for OPEN command
OPENFILENAME	DB	$00,$00
				DA	MLI-$400				; buffer snuggled up tight with PRODOS
REFERENCE		DB	$00						; reference to opened file

READLIST		DB	$04
				DB	$00						; REFERENCE written here after OPEN
READLOC			DB	$00,$00					; write to end of sim8800
				DB	$FF,$FF					; read as much as $FFFF - should error out with EOF before that.
TRANSFERRED		DB	$00,$00				

CLOSELIST		DB	$01
				DB	$00
				
ERROR  			JSR	PRBYTE    				;Print error code
       			JSR	CROUT     				;Print a carriage return
       			JSR BELL
       			RTS				

**************************************************

DSKLOAD										
		   		JSR	DSKOPEN   				; open dsk file
		   		JSR SETDSKOFFSET			; SET_MARK
		   		JSR READDSK
       			BNE ERROR					; error if return !=0
				JSR CLOSE
       			BNE ERROR					; error if return !=0
				
				RTS

DSKOPEN 		JSR	MLI       				;Perform call
       			DB	OPENCMD    				;CREATE command number
       			DW	DSKOPENLIST   				;Pointer to parameter list
       			BNE	ERROR     				;If error, display it
       			LDA DSKREFERENCE
       			STA DSKREADLIST+1
       			STA CLOSELIST+1
       			STA MARKLIST+1
       			RTS				

SETDSKOFFSET	;LDA DSKTRACK				; get track number
				;BEQ RESETOFFSET				; if zero, rezero MARK and move along
				LDA #$00
RESETOFFSET		STA DSKOFFSET				; rezero the offset
				STA DSKOFFSET+1
				STA DSKOFFSET+2
				LDX	DSKTRACK				; how many loops to INC offset by?
				BEQ SET_MARK				; zero = skippity
				
MARKLOOP		LDA	DSKOFFSET				; should be zero to start *** magic happens ***
				CLC
				ADC #$20					; multiply $1120 by track number
				STA DSKOFFSET
				LDA	DSKOFFSET+1				; store result in DSKOFFSET
				ADC #$11
				STA DSKOFFSET+1
				BCC MARKLOOP2
				INC DSKOFFSET+2
MARKLOOP2		DEX							;*** magic happens ***
				BNE MARKLOOP
				
SET_MARK		JSR MLI
				DB SET_MARKCMD
				DW MARKLIST
				BNE ERROR
				RTS


READDSK			JSR MLI
				DB	READCMD
				DW	DSKREADLIST
				RTS

OUTTODSK		JSR	DSKOPEN   				; open dsk file
		   		JSR SETDSKOFFSET			; SET_MARK
		   		JSR WRITEDSK
       			BNE ERROR					; error if return !=0
				JSR CLOSE
       			BNE ERROR					; error if return !=0
				
				RTS




DSKOPENLIST		DB	$03						; parameter list for OPEN command
DSKOPENFILENAME	DB	<DSKFILE,>DSKFILE
				DA	MLI-$400				; buffer snuggled up tight with PRODOS
DSKREFERENCE	DB	$00						; reference to opened file

MARKLIST		DB	$02						; 2 parameters for set_mark
				DB	$00						; mark REFERENCE
DSKOFFSET		DB	$00,$00,$00				; 3 bytes, lo to hi

DSKREADLIST		DB	$04
				DB	$00						; REFERENCE written here after OPEN
DSKREADLOC		DB	<BASICBUFFER,>BASICBUFFER					; write to end of sim8800
				DB	$20,$11					; read $1120 bytes at a time (137*32)
DSKTRANSFERRED	DB	$00,$00				

DSKFILE			DB	ENDDSKNAME-DSKNAME 			;Length of name
DSKNAME    		ASC	'/SIM8800/ZORK.DSK' 		;followed by the name
ENDDSKNAME 		EQU	*

WRITEDSK		JSR MLI
				DB	WRITECMD
				DW	DSKREADLIST				; same parameters for read/write
				RTS

**************************************************
;	Defaults:
;	DSKTRACK == 0
;	set DSKSTATUS = A5 : *** 1 == FALSE, 0 == TRUE ***
;		bit 0 = not ready to write  1
;		bit 1 = head move allowed  0
;		bit 2 = head status not on 1
;		bit 5 = interrupt disabled = 1
;		bit 6 = 0 - on track 0
;		bit 7 = 1 - data not ready yet
;	
;	set DSKSECTORIN = 0
;	
;	on DSKCONTROL out - if bit 2 = 1, then load 137 bytes from DSKTRACK.
;	
;	OPEN DISKBASIC41, SET_MARK = DSKTRACK, READ 32*137 bytes into BASICBUFFER
;	
;	
**************************************************

DSKSELECT			DB	$00				; port 08 out - latches and enables drive - ignored for now, only enabling 1 drive (0)
DSKSTATUS			DB	$A5				; port 08 in - indicates drive status 1010 0101 *** 0=TRUE, 1=FALSE ***
										; not ready to write, move enable, head off, n/a, n/a, interrupt disable, on track 0, no data
DSKCONTROL			DB	$00				; port 09 out - controls drive functions *** 1=TRUE, 0=FALSE ***
DSKSECTORIN			DB	$00				; port 09 in - indicates disk sector position - start at 00
										; 0-31 offset by 1 bit, bit 0=0 when on sector 0, 1 otherwise.
										
DSKWRITE			EQU	$FA				; port 0A out - data to write
DSKREAD				EQU	$FB				; port 0A in - data to read

DSKTRACK			DB	$00				; 32 sectors ($20) per track. Start at track 00 (4K=$1120 per track)
DSKSECTOR			DB	$00				; 137 bytes ($89) per sector. Start at sector 00	
										; *** use this for math, DSKSECTORIN for comms.
SECTOROFFSET		DB 	$00				; how far into the sector to grab bytes
TRACKPOINTER		DB 	$00,$00			; how far into the track to look for sector start

**************************************************

SIMDSK				LDA DSKCONTROL		; see what the last operation wants from the drive
					BEQ DSKRTS			; ZERO? Do nothing.
					TAX					; hang onto DSKCONTROL byte
					
					AND #$03			; one/two LO bits set? 00000011
					BNE	MOVEHEAD		; moving head in/out
					
					TXA					; get DSKCONTROL back
					CMP #$08			; bits 2/3 set, load/unload head 00001000
					BEQ UNLOADHEAD	

					CMP #$80
					BEQ WRITESEQUENCE	; == #$80 for DSK write???
					
					AND #$04			; == 04 TRUE = load HEAD
					BEQ DSKRTS			; anything else?

LOADHEAD			LDA	DSKSTATUS		; if bit 0=0 then, yes. Write was initiated at least.
					TAX
					AND #$01			; if it's 1, no need to write
					BNE LOADHEAD2
					
					TXA					; otherwise, write the track to DSK file
					ORA #$01
					STA DSKSTATUS		; set bit 0 to disable write for next pass
					
					JSR OUTTODSK		; write the track with MLI

LOADHEAD2			JSR DSKLOAD			; LOAD TRACK TO BUFFER AFTER WRITING CURRENT TRACK BACK TO DISK

					LDA DSKSTATUS		; DSKCONTROL == #$04
					AND	#$7B			; 0111 1011B .....0.. = head on, data waiting
;					ORA	#$02			; 0000 0010B head not movable - unnecessary
					STA DSKSTATUS		; move off, head on, 

					JMP DSKRTS

UNLOADHEAD			LDA DSKSTATUS		; DSKCONTROL == #$08
					ORA	#$04			; 00000100B		.....10. = move on, head off
					AND	#$FD			; 11111101B
					STA DSKSTATUS

DSKRTS				LDA #$00
					STA DSKCONTROL		; reset DKSCONTROL to prevent loopity
					RTS

WRITESEQUENCE		
						
					LDA	DSKSTATUS		; unset bit 0 to enable write
					AND #$FC			; set head loaded true. STATUS bit 2 = 0
					STA DSKSTATUS		; 11111100

										; set sector status true. SECTOR bit 0 = 0

										; bytes OUT on 0A
					
					JMP DSKRTS

MOVEHEAD								; DID WE WRITE TO THIS TRACK?
					LDA	DSKSTATUS		; if bit 0=0 then, yes. Write was initiated at least.
					TAX
					AND #$01			; if it's 1, no need to write
					BNE MOVEHEAD2
					
					TXA					; otherwise, write the track to DSK file
					ORA #$01
					STA DSKSTATUS		; set bit 0 to disable write for next pass
					
					JSR OUTTODSK		; write the track with MLI
										
MOVEHEAD2			LDA DSKCONTROL		; move which way?
					ROR					; bit 0 into carry
					BCS	STEPIN			; bit 0 set, move to HIGHER track #

STEPOUT				LDA DSKTRACK		; if already on track 0, ignore.
					BEQ SETTRACK0		; make sure TRACK0 bit is set?
					DEC DSKTRACK		; otherwise next lower track
					BEQ SETTRACK0		; 
					JSR DSKLOAD			; LOAD TRACK N TO BUFFER
					JMP DSKRTS

SETTRACK0			LDA	DSKSTATUS		; 		
					AND #$BF			; 10111111 unset bit 6 (track 0=TRUE) 
					STA DSKSTATUS
					JSR DSKLOAD			; LOAD TRACK 0 TO BUFFER
					JMP DSKRTS

STEPIN				LDA DSKTRACK
					CMP #$4D			; can't go higher than 77
					BEQ DSKRTS
					INC DSKTRACK
					LDA DSKSTATUS
					ORA	#$40			;01000000 - sets bit 6, not on TRACK0
					STA DSKSTATUS		
					JSR DSKLOAD			; LOAD TRACK N TO BUFFER
					JMP DSKRTS





**************************************************




ALTAIRLO			DB	<ALTAIRSCREENDATA
ALTAIRHI			DB	>ALTAIRSCREENDATA

ALTAIRTABLE			DA	ALTAIRLO,ALTAIRHI

ALTAIRSCREENDATA
					HEX	66,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,66
					HEX	66,AA,AA,AA,AA,AA,AF,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,AA,66
					HEX	66,FF,F5,FF,F5,FF,F5,F5,F5,FF,F5,FF,FF,F5,FF,F5,FF,F5,FF,F5,FF,F5,FF,FF,F5,FF,FF,FF,F5,FF,FF,FF,F5,FF,FF,FF,F5,FF,FF,66
					HEX	00,00,00,00,00,00,00,00,66,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,A1,AA,A1,AA,A1,AA,A1,AA,A1
					HEX	AA,A1,AA,A1,AA,A1,AA,66,66,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA
					HEX	AA,AA,AA,AA,AA,AA,AA,66,66,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A,6A
					HEX	6A,6A,6A,6A,6A,6A,6A,66,00,00,00,00,00,00,00,00

					HEX	66,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,66
					HEX	66,AA,25,AA,AA,AA,AA,25,AA,AA,25,AA,AA,25,AA,AA,25,AA,AA,25,AA,AA,25,AA,AA,AA,AA,25,AA,AA,AA,25,AA,AA,AA,AA,AA,AA,AA,66
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,66,AA,A8,AA,A8,AA,AA,A1,AA,A1,AA,A1,AA,A1,AA,A1,AA,A1,AA,A1,AA,A1,AA,A1,AA,A1,AA,A1,AA,A1,AA,A1
					HEX	AA,A1,AA,A1,AA,A1,AA,66,66,AA,A6,AA,AA,AA,AA,A5,AA,AA,A5,AA,AA,A5,AA,AA,A5,AA,AA,A5,AA,AA,A5,AA,AA,AA,AA,A5,AA,AA,AA,A5
					HEX	AA,AA,AA,AA,AA,AA,AA,66,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00

					HEX	66,AA,AA,AA,AA,AA,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,66
					HEX	66,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,FA,66
					HEX	D0,D3,A0,C1,C3,A0,C2,A0,C3,A0,A0,C4,A0,C5,A0,A0,C8,A0,CC,A0,A0,A0,D3,D0,A0,A0,A0,D0,C3,A0,A0,CF,D0,A0,C9,D4,A0,C2,CB,A0
					HEX	00,00,00,00,00,00,00,00,66,AA,AA,AA,AA,AA,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF,AA,AF
					HEX	AA,AF,AA,AF,AA,AF,AA,66,66,FF,FF,55,FF,FF,55,FF,FF,55,55,55,FF,FF,55,FF,FF,55,FF,55,F5,5F,FF,FF,5F,FF,FF,FF,5F,FF,FF,FF
					HEX	5F,FF,FF,FF,5F,FF,FF,66,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
		
					HEX	66,AA,AA,AA,AA,AA,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,26,AA,66
					HEX	66,FF,55,FF,55,FF,55,FF,FF,FF,55,FF,FF,55,FF,55,FF,55,FF,55,FF,55,FF,F5,5F,F5,FF,F5,5F,F5,FF,55,FF,55,FF,55,FF,55,FF,66
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,66,AA,AF,AF,AF,FA,AA,A5,AA,A5,AA,A5,AA,A5,AA,A5,AA,A5,AA,A5,AA,A5,AA,A5,AA,A5,AA,A5,AA,A5,AA,A5
					HEX	AA,A5,AA,A5,AA,A5,AA,66,66,FF,55,F5,55,FF,55,5F,5F,FF,55,FF,FF,55,F5,55,FF,55,FF,55,F5,5F,FF,55,FF,55,FF,55,FF,55,FF,55
					HEX	FF,55,FF,55,FF,55,FF,66,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
					HEX	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00


DISPLAYBYTES		DB	$00,$00,$00		; for the "graphical" display
										; PCH/REGD PCL OPCODE
SWITCHBYTES			DB	$00,$00

SWITCHSCRATCH		DB	$00,$00			; state of switches. Should all be OFF to start

SIOSTATUSOUT		DB	$00				; probably unnecessary
SIOSTATUSIN			DB	$81				; not ready for IO

SIODATAOUT			DB	$00				; Virtual Serial Interface, status buffer, DATA buffer
SIODATAIN			DB	$00				; Virtual Serial Interface, status buffer, DATA buffer

SIOBUFFER			DB	$00				; TEMP data					

SIOSTATUSBYTES		DB	$00,$01			; status settings for "Have data" and "waiting data"
SIO2STATUSBYTES		DB	$01,$02			; has a byte, last byte sent

; SIO2
;Receive Data Register Full (RDRF), Bit 0 - Receive Data Register Full indicates 
;that received data has been transferred to the Receive Data Reqister. RDRF is 
;cleared after a read of the Receive Data Register or by a maiter reset. 
;The cleared or empty state indicates that the contents of the Receive Data Register are not current.

;Transmit Data Register Empty (TDRE), Bit 1 - The Transmit Data Register Empty bit being set high 
;indicates that the Transmit Data Register contents have been transferred and that new data may be entered. 
;The low state indicates that the register is full and that transm'ission of a new character has not begun 
;since the last write data command.




; PS AC...
LEGEND				DB	$D0,$D3,$A0,$C1,$C3,$A0,$C2
      				DB	$A0,$C3,$A0,$A0,$C4,$A0,$C5,$A0
      				DB	$A0,$C8,$A0,$CC,$A0,$A0,$A0,$D3
      				DB	$D0,$A0,$A0,$A0,$D0,$C3,$A0,$A0
      				DB	$CF,$D0,$A0,$C9,$D4,$A0,$C2,$CB
; APPLE 80...MCCREARY	
TITLEBAR 			DB	$A0,$C1,$D0,$D0,$CC,$C5,$AD,$B8
					DB	$B0,$A0,$C3,$CF,$D0,$D9,$D2,$C9
					DB	$C7,$C8,$D4,$A0,$B1,$B9,$B7,$B9
					DB	$A0,$C2,$D9,$A0,$C4,$C1,$CE,$CE
					DB	$A0,$CD,$C3,$C3,$D2,$C5,$C1,$D2,$00
; "BREAKPOINTS" (BACKWARDS)
BREAKPOINTS 		DB	$D9,$D3,$D4,$CE,$C9,$CF,$D0,$CB
					DB	$C1,$C5,$D2,$C2

BASICBUFFERLO		DB <BASICBUFFER
BASICBUFFERHI		DB >BASICBUFFER
BASBUFPNT			DB 00 00
BASICBUFFER			DB 00
