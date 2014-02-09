| Tastier Machine                                                | ARM                         |
| -------------------------------------------------------------- | --------------------------- |
| HALT - Ends execution of current program                       | n/a                         |
| DUP - ```x := POP; PUSH x; PUSH x```                           | MOV Rd, x                   |
| NOP - no operation                                             | NOP                         |
| ADD - ```x := POP; y := POP; PUSH (y + x)```                   | ADD Rd, y, x                |
| SUB - ```x := POP; y := POP; PUSH (y - x)```                   | SUB Rd, y, x / RSB Rd, x, y |
| MUL - ```x := POP; y := POP; PUSH (y * x)```                   | MUL Rd, y, x                |
| DIV - ```x := POP; y := POP; PUSH (y / x)```                   | N/A - Subroutine needed     |
| EQU - ```x := POP; y := POP; UPDATE STATUS (x==y)```           | CMP x, y                    |
| LSS - ```x := POP; y := POP; UPDATE STATUS (y<x)```            | CMP x, y                    |
| GTR - ```x := POP; y := POP; UPDATE STATUS (y>x)```            | CMP x, y                    |
| NEG - ```x := POP; PUSH (-x)```                                | NEG Rd, x                   |
| RET - ```addr := POP; PC := addr```                            | B addr                      |
| READ - Read input data, place in memory, push address to stack | N/A - Further design req    |
| WRITE - Pop addr, print value in memory at that location       | N/A - Further design req    |
| LEAVE - Set PC to base pointer, restore old base pointer       | BX addr + call convention   |
| STOG x - ```y := POP; MEM[x] := y```                           | STR y, [x]                  |
| LOADG x - ```y := MEM[x]; PUSH y```                            | LDR y, [x]                  |
| CONST x - ```PUSH x```                                         | MOV Rd, #x / LDR Rd, =x     |
| ENTER x - Store old link register, save PC to LR, branch       | BL addr + call convention   |
| JMP x   - ```PC := x```                                        | B x                         |
| FJMP x  - ```if (STATUS == FALSE) PC := x ELSE NOP```          | B{cond} x                   |
