# FPC_CPU

Demo that visualized the execution of assembler code in a CPU (using a 4 stage pipeline). This also means, if no pipelining is used each and every command needs 4 clock ticks.

![](preview.png)


The demo code is "more" or less this in FreePascal:

```pascal
Function sumArray(aLength: integer; pElement: PInteger): integer;
Begin
  result := 0;
  While aLength > 0 Do Begin
    result := result + pElement^;
    dec(aLength);
    inc(pElement);
  End;
End;

Var
  a: Array Of Integer;
  mem119: integer;
Begin
  setlength(a, 3);
  a[0] := 1;
  a[1] := 2;
  a[2] := 3;

  mem119 := sumArray(3, @a[0]);
End;
```   

See the [Manual](Manual.md) for more.
If you need inspiration of what to implement with the CPU maybe you take a look [here](Practice_tasks.md).

Features:
- Execution of ASSEMBLER Code (step by step or automatic mode)
- Optional Pipelining
- Breakpoints
- Memory / Stack
- Function Calls

Dependencies:
- none