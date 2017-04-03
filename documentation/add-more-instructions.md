# Adding More Instructions to an Existing Superoptimizer

## Getting Started
1. Set up GreenThumb (see the top-level [README](../README.md#setup)).
2. Try to get the ARM cooperative superoptimizer running (see the top-level [README](../README.md#running)). 
3. Familiarize yourself with the structure of GreenThumb by
   - Read [Greenthumb: Superoptimizer Construction Framework (CC'16)](http://www.eecs.berkeley.edu/~mangpo/www/papers/greenthumb_cc2016.pdf).
   - Skim through [Extending GreenThumb to a New ISA](new-isa.md). No need to try it out. Just read.

This instruction will walk you through how to add a new instruction to any superoptimizer built using GreenThumb. However, this documentation will use LLVM IR, which corresponds to directory `llvm` in this repo, as a concrete example. 

<a name="Prerequisite"></a>
## Prerequisite Knowledge

### LLVM
#### Bits
We define LLVM as a 32-bit architecture. That means each variable is 32-bit. This is defined at field `bitwidth` of `llvm-machine.rkt`. Search for `(set! bitwidth 32)`.

#### Program State
In our LLVM ISA, a program state is minimal. It only includes values of variables and memory in the program. We represent a LLVM program state as:
```racket
(progstate (vector x0 x1 x2 ...)
           (vector (vector v00 v01 v02 v03) ...)
           #(object:memory%))
```
where `progstate` is a macro for vector. The following concrete program state:
```racket
(progstate (vector 0 0 999) (vector (vector 3 3 3 3)) #(object:memory%))
```
represents a state of program with three 32-bit variables, and one 4 x 32-bit vector variables, and an unbounded memory. The first two variables (ID 0 and 1) are 0, and the third variable (ID 2) has value 999. The vector variable (ID 0) is <1, 1, 1, 1>.  Variables of a particular type in a program we optimize are assigned to unique IDs starting from 0. Note that the framework treats IDs of normal variables and vector variables as they are in different spaces, so a normal variable with ID 0 and a vector variable with ID 0 refer to different variables.

#### Liveness Information
When checking the equivalence of two expressions, we need to specify which locations in the output program states that need to be equal. Essentially, we only care about the live parts of the output program state (live-out). For example, when optimize the following code:
```
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
```
we do not care what the values of %in and %1 are at the end, but only care about the value of %out. In this case, the live-out only includes %out. Inside our framework, we use the same data structure that represents a program state to represent a liveness information. Instead of containing 32-bit numbers like program state, live-out contains boolean values. For example, say %in, %1, and %out have ID 0, 1, and 2 respectively. The live-out that only includes %out is represented by:
```racket
(progstate (vector #f #f #t) (vector) #t)
```

Here is another sample with vector instructions. 
```
%out = add <4 x i32> %1, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
```
We would like to find an equivalent program with respect to a vector variable `%out` (ID 1), so the live-out is:
```racket
(progstate (vector) (vector #f #t) #t)
```

If live-out at memory field is set to #t, output memory of a spec program and a candidate program have to be equivalent. Their memory are equivalent if their entire memory are the same. Currently, we do not support checking equivalence on just some parts of memory if we use the provided memory object.

### ARM
#### Bits
We define ARM as a 32-bit architecture; each register is 32-bit.

#### Program State
In ARM, a program state include registers, memory, and flag z.

We represent an ARM program state as:
```racket
(progstate (vector r0 r1 ...)  #(object:memory%) z)
```
We use flag `z` to represent most condition flags (including N, C, Z, and V flags) as follows:
```
z = 0 | eq
z = 1 | neq
z = 2 | x < y && sign(x) == sign(y)
z = 3 | x > y && sign(x) == sign(y)
z = 4 | x < 0 && y >= 0
z = 5 | y < 0 && x >= 0
```

`tst` instruction sets `z` to either 0 or 1. `cmp` instruction can set `z` to any value except 1.

##### Liveness Information

We use `progstate` to represent liveness similar to in LLVM.

<a name="Support"></a>
## How to Support More Instructions
To support more instructions, we need to modify several files as we will explain in this section.

### 1. Program State
We may need to modify our program state representation, `progstate` in `llvm-machine.rkt` to support more instructions. For example, in order to support flags (e.g. carry flag), we will need to modify our program state representation in `llvm-machine.rkt`:
- define a new program state element type by calling `define-progstate-type`
- add more arguments to `progstate` macro for flags
- modify the method `progstate-structure` to include flags

See [Steps 1.2 & 1.3 of Extending GreenThumb to a New ISA](new-isa.md#step1.2) for more details.
To support `mul`, we do not need to change our program state.

### 2. Opcodes
Add `'mul` to `'rrr-commute` instruction class. If the new instruction does not belong to any existing instruction class, we have to define a new instruction class in `llvm-machin.rkt` by calling `define-instruction-class`. If we need a new operand type, call `define-arg-type`.  See [Steps 1.4 & 1.5 of Extending GreenThumb to a New ISA](new-isa.md#step1.4) for more details.

### 3. Parser & Printer
If the new instruction does not follow the default instruction format `%var0 = opcode %var1, %var2, ...` or `store %var1, %var2`, we have to modify the parser in `llvm-parser.rkt` to parse the instruction, and the methods `encode-inst`, `decode-inst`, and `print-syntax-inst` in `llvm-printer.rkt`. To support `mul`, we do not need to alter the parser and printer. See [Section B and Step 2 in Extending GreenThumb to a New ISA](new-isa.md#secB) for more details.

### 4. Simulator
##### Method `interpret`
Modify the `interpret` method in `llvm-simulator-rosette.rkt` and `llvm-simulator-racket.rkt` to interpret the new instruction. To support `mul`, we need the case in the main conditional expression in `interpret`:
```racket
[(inst-eq `mul)   (rrr bvmul)]
```
Function `rrr` has already been defined. If you compare function `rrr` and `rri`, you will notice the difference between using the third argument as a variable and as a constant; r stands for variable (or register), and i stands for constant.
Next, define `bvmul` outside `interpret` function:
```racket
(define bvmul (bvop *))
```
Notice that `bvop` applies its argument lambda function to x and y, and then applies `finitize-bit` to the result. `(finitize-bit x)` calls `(finitize x bit)`, where the `finitize` function truncates overflowed `x` to `bit` bits and convert `x` to a signed number. Our convention is that every value in `progstate` has to be in the signed format (e.g. -1 instead of 2^32 - 1 for a 32-bit number). This is because we need the racket simulator to be consistent with the constraint solver we use, which works with signed numbers. Therefore, always call `finitize-bit` after performing an arithmetic operation.

##### Method `performance-cost`
Currently, we have a very simple performance model. The performance cost of a program is equal the number of instructions in the program excluding `nop`. Modify this method for a more precise performance model.

##### Testing the Simulator
Test if the simulator interpret the new instruction correctly by using `test-simulator.rkt`. 
- Modify `(define code ...)` to include the new instruction. 
- Set `input-state` to a desired input program state. 
- Run the file using either command line or DrRacket. 
- Inspect the output program state. 
- Then, uncomment out Phase B--E (one by one). Run again to make sure that it does not throw any exception. If an error occurs, there is something wrong.

##### Additional Notes
If we have to make many modifications to the simulator, it might be easier to only modify `llvm-simulator-rosette.rkt`. Once it works correctly, copy the entire file `llve-simulator-rosette.rkt` to `llvm-simulator-racket.rkt`, and replace **any appearance** of `rosette` with `racket`.

##### Testing the Superoptimizer

Test if the symbolic, stochastic, and enumerative superoptimizers can synthesize the new instruction by using `test-search.rkt`. Set `code` definition to a program with only the new instruction and another dummy instruction (such as add 0) to create an obviously inefficient program. Add or remove `?` in `sketch` definition to define how many instructions an output program may contain; one `?` represents one instruction.

Run each of the symbolic, stochastic, and enumerative search at a time.
- For the stochastic search, make sure to provide the correct live-in information when testing.
- When the stochastic and enumerative search find a better program, they should print `FOUND!!!` followed by the program.
- The symbolic and enumerative search should return with an optimized program quickly (for program with one instruction).
- For the stochastic search, we might have to wait a little longer. If we don't see it printing `FOUND!!!` within a minute, something may be wrong.

To test that the backward search of the enumerative search works properly, run the enumerative search again but on a larger program (four instructions) that can be optimized to three instructions. This is because the enumerative search only performs the backward search when it tries to synthesize programs with three instructions or more. Make sure to add `?` in `sketch` definition if an output program contains more instructions.
