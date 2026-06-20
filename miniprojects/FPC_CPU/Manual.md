# Manual for FPC_CPU

### Editor

The Editor is oriented to the Lazarus IDE and supports the following keycommands:

| Command | Description | 
| ---     | ---
| F5      | toggle breakpoint
| F7      | compile and step / step
| F9      | compile and run
| CTRL+F2 | halt
| CTRL+#  | toggle comment of current line

### Assembler

This user manual for FPC_CPU contains the list of supported assembler commands.

All commands use the following format:

\<keyword\>" "\<left operand\>","\<right operand\>

Any text after ";" is treated as a comment and ignored. Internally, the CPU works with 32-bit integer values. When using one of the jump commands, you need to specify a label. The syntax for a label is:

\<label\>":"

The left operand must always be a register. The right operand can be a register or a constant value.

| Command | Description |
| ---     | ---         |
| ADD     | Adds the left and right operands and stores the result in the left operand.
| AND     | Performs a bitwise AND on the left and right operands and stores the result in the left operand.
| CALL    | saves program counter onto the stack and calls a subfunction
| CMP     | Compares the left and right operands and updates the flags accordingly.
| DIV     | Performs a integer division
| HLT     | Halts program execution.
| JC      | Jumps to the specified label if the carry flag is set.
| JMP     | Jumps unconditionally to the specified label.
| JNC     | Jumps to the specified label if the carry flag is not set.
| JNZ     | Jumps to the specified label if the zero flag is not set.
| JZ      | Jumps to the specified label if the zero flag is set.
| LOAD    | Loads the value from the memory address in the right operand into the left register.
| MOV     | Copies the right operand (register or constant) into the left register.
| MUL     | Performs a integer multiplication
| NOT     | Performs a bitwise NOT on the left operand and stores the result in the left operand.
| NOP     | Performs no operation.
| OR      | Performs a bitwise OR on the left and right operands and stores the result in the left operand.
| POP     | Pops a value from stack and stores it into the given register
| PUSH    | Pushs the value of a given register onto the stack
| RET     | Takes the topmost value from stack and sets programcounter to this value
| SHL     | Shifts the left operand left by the number of bits specified in the right operand.
| SHR     | Shifts the left operand right by the number of bits specified in the right operand.
| STORE   | Stores the value from the left register at the memory address specified in the right operand.
| SUB     | Subtracts the right operand from the left operand and stores the result in the left operand.
| XOR     | Performs a bitwise XOR on the left and right operands and stores the result in the left operand.