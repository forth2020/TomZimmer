\ Assembler labels for 8052 
hex
\ SFR addresses
080  constant P0       \ PORT 0
081  constant SP       \ STACK POINTER
082  constant DPL      \ DATA POINTER - LOW BYTE
083  constant DPH      \ DATA POINTER - HIGH BYTE
087  constant PCON     \ POWER CONTROL
088  constant TCON     \ TIMER CONTROL
089  constant TMOD     \ TIMER MODE
08A  constant TL0      \ TIMER 0 - LOW BYTE
08B  constant TL1      \ TIMER 1 - LOW BYTE
08C  constant TH0      \ TIMER 0 - HIGH BYTE
08D  constant TH1      \ TIMER 1 - HIGH BYTE
090  constant P1       \ PORT 1
098  constant SCON     \ SERIAL PORT CONTROL
099  constant SBUF     \ SERIAL PORT BUFFER
0A0  constant P2       \ PORT 2
0A8  constant IE       \ INTERRUPT ENABLE
0B0  constant P3       \ PORT 3
0B8  constant IP       \ INTERRUPT PRIORITY
0C8  constant T2CON    \ TIMER 2 CONTROL
0CA  constant RCAP2L   \ TIMER 2 CAPTURE REGISTER - LOW BYTE
0CB  constant RCAP2H   \ TIMER 2 CAPTURE REGISTER - HIGH BYTE
0CC  constant TL2      \ TIMER 2 - LOW BYTE
0CD  constant TH2      \ TIMER 2 - HIGH BYTE
0D0  constant PSW      \ PROGRAM STATUS WORD
0E0  constant ACC      \ ACCUMULATOR
0F0  constant B        \ MULTIPLICATION REGISTER

\ Bit addresses
180  constant P0.0     \ P0.0
181  constant P0.1     \ P0.1
182  constant P0.2     \ P0.2
183  constant P0.3     \ P0.3
184  constant P0.4     \ P0.4
185  constant P0.5     \ P0.5
186  constant P0.6     \ P0.6
187  constant P0.7     \ P0.7
188  constant IT0      \ TCON.0 - EXT. INTERRUPT 0 TYPE
189  constant IE0      \ TCON.1 - EXT. INTERRUPT 0 EDGE FLAG
18A  constant IT1      \ TCON.2 - EXT. INTERRUPT 1 TYPE
18B  constant IE1      \ TCON.3 - EXT. INTERRUPT 1 EDGE FLAG
18C  constant TR0      \ TCON.4 - TIMER 0 ON/OFF CONTROL
18D  constant TF0      \ TCON.5 - TIMER 0 OVERFLOW FLAG
18E  constant TR1      \ TCON.6 - TIMER 1 ON/OFF CONTROL
18F  constant TF1      \ TCON.7 - TIMER 1 OVERFLOW FLAG
190  constant P1.0     \ P1.0
191  constant P1.1     \ P1.1
192  constant P1.2     \ P1.2
193  constant P1.3     \ P1.3
194  constant P1.4     \ P1.4
195  constant P1.5     \ P1.5
196  constant P1.6     \ P1.6
197  constant P1.7     \ P1.7
198  constant RI       \ SCON.0 - RECEIVE INTERRUPT FLAG
199  constant TI       \ SCON.1 - TRANSMIT INTERRUPT FLAG
19A  constant RB8      \ SCON.2 - RECEIVE BIT 8
19B  constant TB8      \ SCON.3 - TRANSMIT BIT 8
19C  constant REN      \ SCON.4 - RECEIVE ENABLE
19D  constant SM2      \ SCON.5 - SERIAL MODE CONTROL BIT 2
19E  constant SM1      \ SCON.6 - SERIAL MODE CONTROL BIT 1
19F  constant SM0      \ SCON.7 - SERIAL MODE CONTROL BIT 0
1A0  constant P2.0     \ P2.0
1A1  constant P2.1     \ P2.1
1A2  constant P2.2     \ P2.2
1A3  constant P2.3     \ P2.3
1A4  constant P2.4     \ P2.4
1A5  constant P2.5     \ P2.5
1A6  constant P2.6     \ P2.6
1A7  constant P2.7     \ P2.7
1A8  constant EX0      \ IE.0 - EXTERNAL INTERRUPT 0 ENABLE
1A9  constant ET0      \ IE.1 - TIMER 0 INTERRUPT ENABLE
1AA  constant EX1      \ IE.2 - EXTERNAL INTERRUPT 1 ENABLE
1AB  constant ET1      \ IE.3 - TIMER 1 INTERRUPT ENABLE
1AC  constant ES       \ IE.4 - SERIAL PORT INTERRUPT ENABLE
1AD  constant ET2      \ IE.5 - TIMER 2 INTERRUPT ENABLE
1AF  constant EA       \ IE.7 - GLOBAL INTERRUPT ENABLE
1B0  constant P3.0     \ P3.0
1B1  constant P3.1     \ P3.1
1B2  constant P3.2     \ P3.2
1B3  constant P3.3     \ P3.3
1B4  constant P3.4     \ P3.4
1B5  constant P3.5     \ P3.5
1B6  constant P3.6     \ P3.6
1B7  constant P3.7     \ P3.7
1B0  constant RXD      \ P3.0 - SERIAL PORT RECEIVE INPUT
1B1  constant TXD      \ P3.1 - SERIAL PORT TRANSMIT OUTPUT
1B2  constant INT0     \ P3.2 - EXTERNAL INTERRUPT 0 INPUT
1B3  constant INT1     \ P3.3 - EXTERNAL INTERRUPT 1 INPUT
1B4  constant T0       \ P3.4 - TIMER 0 COUNT INPUT
1B5  constant T1       \ P3.5 - TIMER 1 COUNT INPUT
1B6  constant WR       \ P3.6 - WRITE CONTROL FOR EXT. MEMORY
1B7  constant RD       \ P3.7 - READ CONTROL FOR EXT. MEMORY
1B8  constant PX0      \ IP.0 - EXTERNAL INTERRUPT 0 PRIORITY
1B9  constant PT0      \ IP.1 - TIMER 0 PRIORITY
1BA  constant PX1      \ IP.2 - EXTERNAL INTERRUPT 1 PRIORITY
1BB  constant PT1      \ IP.3 - TIMER 1 PRIORITY
1BC  constant PS       \ IP.4 - SERIAL PORT PRIORITY
1BD  constant PT2      \ IP.5 - TIMER 2 PRIORITY
1C8  constant CAP2     \ T2CON.0 - CAPTURE OR RELOAD SELECT
1C9  constant CNT2     \ T2CON.1 - TIMER OR COUNTER SELECT
1CA  constant TR2      \ T2CON.2 - TIMER 2 ON/OFF CONTROL
1CB  constant EXEN2    \ T2CON.3 - TIMER 2 EXTERNAL ENABLE FLAG
1CC  constant TCLK     \ T2CON.4 - TRANSMIT CLOCK SELECT
1CD  constant RCLK     \ T2CON.5 - RECEIVE CLOCK SELECTT
1CE  constant EXF2     \ T2CON.6 - EXTERNAL TRANSITION FLAG
1CF  constant TF2      \ T2CON.7 - TIMER 2 OVERFLOW FLAG
1D0  constant P        \ PSW.0 - ACCUMULATOR PARITY FLAG
1D2  constant OV       \ PSW.2 - OVERFLOW FLAG
1D3  constant RS0      \ PSW.3 - REGISTER BANK SELECT 0
1D4  constant RS1      \ PSW.4 - REGISTER BANK SELECT 1
1D5  constant F0       \ PSW.5 - FLAG 0
1D6  constant AC       \ PSW.6 - AUXILIARY CARRY FLAG
1D7  constant CY       \ PSW.7 - CARRY FLAG
1E0  constant ACC.0    \ ACC.0
1E1  constant ACC.1    \ ACC.1
1E2  constant ACC.2    \ ACC.2
1E3  constant ACC.3    \ ACC.3
1E4  constant ACC.4    \ ACC.4
1E5  constant ACC.5    \ ACC.5
1E6  constant ACC.6    \ ACC.6
1E7  constant ACC.7    \ ACC.7

