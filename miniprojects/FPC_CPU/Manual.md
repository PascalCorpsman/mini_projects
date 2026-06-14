# Manual for FPC_CPU

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
| CMP     | Compares the left and right operands and updates the flags accordingly.
| HLT     | Halts program execution.
| JMP     | Jumps unconditionally to the specified label.
| JNZ     | Jumps to the specified label if the zero flag is not set.
| JZ      | Jumps to the specified label if the zero flag is set.
| LOAD    | Loads the value from the memory address in the right operand into the left register.
| MOV     | Copies the right operand (register or constant) into the left register.
| NOT     | Performs a bitwise NOT on the left operand and stores the result in the left operand.
| NOP     | Performs no operation.
| OR      | Performs a bitwise OR on the left and right operands and stores the result in the left operand.
| SHL     | Shifts the left operand left by the number of bits specified in the right operand.
| SHR     | Shifts the left operand right by the number of bits specified in the right operand.
| STORE   | Stores the value from the left register at the memory address specified in the right operand.
| SUB     | Subtracts the right operand from the left operand and stores the result in the left operand.
| XOR     | Performs a bitwise XOR on the left and right operands and stores the result in the left operand.