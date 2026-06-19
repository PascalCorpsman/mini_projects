# 🧠 CPU Emulator – Practice Tasks

This collection contains a set of exercises designed to help understand how a simple CPU works.

The tasks are ordered by difficulty and focus on different core concepts:
- Registers & data movement
- Arithmetic & logic operations
- Control flow (jumps)
- Stack usage
- Function calls
- Memory access

---

# 🟢 Level 1 – Basics (Registers & ALU)

## Task 1: Swap Two Registers
Swap the values of register A and B.

**Example:** A = 5 B = 3 → A = 3 → B = 5

**Hint:**
You can use the stack or a temporary register.

---

## Task 2: Simple Addition
Add two numbers stored in registers A and B. Store the result in A.

---

## Task 3: Bit Shift
Shift the value in register A to the left and then to the right.

**Goal:**
Understand how `SHL` and `SHR` affect values.

---

# 🟡 Level 2 – Control Flow (Jumps & Loops)

## Task 4: Counter Loop
Count from 1 to N using a loop.

**Goal:**
Use `CMP`, `JZ`, `JNZ`, or similar jump instructions.

---

## Task 5: Sum of Natural Numbers
Compute the sum from 1 to N using a loop (not the mathematical formula).

**Example:** N = 5 → Result = 15

**Concepts:**
- Looping
- Accumulation

---

## Task 6: Find Maximum
Compare two values (A and B) and store the larger one in A.

---

# 🟠 Level 3 – Stack Usage

## Task 7: Swap Using Stack
Swap A and B using only stack operations (`PUSH`, `POP`).

---

## Task 8: Stack-Based Addition
Push two constants onto the stack, pop them into registers, and add them.

**Example:** PUSH 5 PUSH 3 POP A POP B ADD A,B

---

# 🔴 Level 4 – Functions (CALL & RET)

## Task 9: Simple Function Call
Write a function that adds 2 to register A.

**Example:** CALL addTwo

---

## Task 10: Function with Parameter (Stack)
Pass a value via the stack to a function that doubles it.

**Concepts:**
- Parameter passing
- Stack usage

---

## Task 11: Implement Multiplication Using a Function

Write a function that multiplies two values **without using a dedicated multiply instruction**.

Pass the two input values via the stack.  
The function should compute the result (e.g., using repeated addition), push the result onto the stack, and return.

**Concepts:**
- Parameter passing via stack
- Stack usage (PUSH/POP)
- Function calls (CALL/RET)
- Loop-based multiplication (repeated addition)

---


# 🔴 Level 5 – Advanced Challenges

## Task 12: Factorial (Iterative)
Compute the factorial of N using a loop.

**Example:** 5! = 120

---

## Task 13: Fibonacci Sequence
Generate the Fibonacci sequence up to N.

**Example:** 0, 1, 1, 2, 3, 5, 8, ...

---

## Task 14: Nested Function Calls (Optional)
Use multiple function calls (CALL/RET) and verify correct return behavior.

---

# 🟣 Level 6 – Memory Operations (LOAD / STORE)

## Task: Load a Value from Memory
Load a value from a memory address into a register.

**Example:** Memory[100] = 5 → A = 5

**Concepts:**
- Memory addressing
- LOAD instruction

---

## Task: Store a Value in Memory
Store a value from a register into memory.

**Example:** A = 42 → Memory[101] = 42

**Concepts:**
- STORE instruction
- Data persistence

---

## Task: Copy Memory Value
Load a value from one memory address and store it into another.

**Example:** Memory[100] → Memory[101]

---

## Task: Add Value from Memory
Load a value from memory and add it to a register.

**Example:** A = 3 Memory[100] = 5 → A = 8

---

## Task: Sum Two Memory Values
Read two values from memory, add them, and store the result back in memory.

**Example:** Memory[100] = 4 Memory[101] = 6 → Memory[102] = 10

---

## Task: Sum 1..N from Memory
Load N from memory, compute the sum from 1 to N, and store the result back in memory.

**Concepts:**
- Memory + loops + arithmetic combined

---

# 💡 Bonus Ideas

- Detect stack underflow/overflow
- Implement a small stack-based calculator
- Optimize code for minimal register usage

---

# 🎯 Goal

These exercises are designed to help you understand:

- How data moves inside a CPU
- How limited registers affect program design
- How control flow works at a low level
- How stacks enable function calls

---

Enjoy exploring the inner workings of a CPU 🚀