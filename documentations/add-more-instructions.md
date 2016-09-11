# Adding More Instructions to LLVM IR Superoptimizer

## Getting Started
1. Set up GreenThumb (see the top-level [README](../README.md)).
2. Try to get the ARM cooperative superoptimizer running (see the top-level [README](../README.md)). 
3. Familiarize yourself with the structure of GreenThumb by
   - Watch this demo [video](https://youtu.be/3l7Z7kB5p3g) on how to build an LLVM IR superoptimizer using GreenThumb.
   - Read Section 'Brief Instructions' in [Extending GreenThumb to New ISA](new-isa.md).
   - Skim through [Greenthumb: Superoptimizer Construction Framework (Tech Report)](http://www.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-8.pdf). No need to try it out. Just read.

This instruction will walk you through how to add a new instruction to any superoptimizer built using GreenThumb. However, this documentation will use LLVM IR, which corresponds to directory `llvm` in this repo, as a concrete example. 

<a name="Prerequisite"></a>
## Prerequisite Knowledge

### LLVM
#### Bits
We define LLVM as a 32-bit architecture. That means each variable is 32-bit. This is defined at field `bitwidth` of `llvm-machine.rkt`. Search for `(set! bitwidth 32)`.

#### Program State
In our LLVM ISA, a program state is minimal. It only includes values of variables and memory in the program. We represent a LLVM program state as:
```
(vector (vector v0 v1 v2 ...) #(object:memory%))
```
For example, the following concrete program state:
```
(vector (vector 0 0 999) #(object:memory%))
```
represents a state of program with three variables and unbounded memory. The first two variables (ID 0 and 1) are 0, and the third variable (ID 2) has value 999. Variables in a program we optimize are assigned to unique IDs starting from 0.

In order to support flags (e.g. carry flag), we will need to extend this representation to include them (i.e. add more entries in the vector).

#### Liveness Information
When checking the equivalence of two expressions, we need to specify which locations in the output program states that need to be equal. Essentially, we only care about the live parts of the output program state (live-out). For example, when optimize the following code:
```
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
```
we do not care what the values of %in and %1 are at the end, but only care about the value of %out. In this case, the live-out only includes %out. Inside our framework, we use the same data structure that represents a program state to represent a liveness information. Instead of containing 32-bit numbers like program state, live-out contains boolean values. For example, say %in, %1, and %out have ID 0, 1, and 2 respectively. The live-out that only includes %out is represented by:
```
(vector (vector #f #f #t) #t)
```

If live-out at memory field is set to #t, output memory of a spec program and a candidate program have to be equivalent. Their memory are equivalent if their entire memory are the same. Currently, we do not support checking equivalence on just some parts of memory if we use the provided memory object.

### ARM
#### Bits
We define ARM as a 32-bit architecture; each register is 32-bit.

#### Program State
In ARM, a program state include registers, stack memory (for temporary variables), flag z, and special register fp (frame pointer). 
Notice that unlike LLVM, we use a vector to represent stack memory for ARM.
To support heap memory, we recommend using the provided memory object like in our LLVM superoptimizer.

We represent an ARM program state as:
```
(progstate (vector r0 r1 ...) (vector m0 m1 ...) z fp)
```
where `progstate` is a struct. `fp` is used for accessing memory. We use a vector of 32-bit numbers to represent memory; m0 is a 32-bit chunk of memory. We use flag `z` to represent most conditions as follows:
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

We do not use progstate to represent liveness information for ARM instruction. Instead, the liveness is represented as a pair of lists, where the first list contains live registers' IDs and the second list contains live memory locations. We consider z flag and pointer fp to always be live, so we exclude liveness of z and fp from our liveness representation. For example,
```
(cons '(0 2) '(1))
```
means r0 and r2 are live, and memory at location 1 (m1) is live.

<a name="Support"></a>
## How to Support More Instructions
To support more instructions, we need to modify several files as we will explain in this section.

### 1. Program State
We may need to add more fields in our program state representation, `progstate` (which is a vector) in `llvm-machine.rkt` to support more instructions. For example, we may need fields to represent flags. To support `mul`, we do not need to change `progstate`.

### 2. Opcodes
Add `mul` to the list of `opcodes` in `llvm-machine.rkt`. Search for `(set! opcodes '#(...))`.

### 3. Parser & Printer
If the new instruction does not follow the default instruction format `%var0 = opcode %var1, %var2, ...` or `store %var1, %var2`, we have to modify the parser in `llvm-parser.rkt` to parse the instruction, and the methods `encode-inst`, `decode-inst`, and `print-syntax-inst` in `llvm-printer.rkt`. To support `mul`, we do not need to alter the parser and printer.

### 4. Simulator
##### Method `interpret`
Modify the `interpret` method in `llvm-simulator-rosette.rkt` and `llvm-simulator-racket.rkt` to interpret the new instruction. To support `mul`, we need the case in the main conditional expression in `interpret`:
```
[(inst-eq `mul)   (rrr bvmul)]
```
Function `rrr` has already been defined. If you compare function `rrr` and `rri`, you will notice the difference between using the third argument as a variable and as a constant; r stands for variable (or register), and i stands for constant.
Next, define `bvmul` outside `interpret` function:
```
(define bvmul (bvop *))
 ```
Notice that `bvop` applies its argument lambda function to x and y, and then applies `finitize-bit` to the result. `(finitize-bit x)` calls `(finitize x bit)`, where the `finitize` function truncates overflowed `x` to `bit` bits and convert `x` to a signed number. Our convention is that every value in `progstate` has to be in the signed format (e.g. -1 instead of 2^32 - 1 for a 32-bit number). This is because we need the racket simulator to be consistent with the constraint solver we use, which works with signed numbers.

##### Method `performance-cost`
Currently, we have a very simple performance model. The performance cost of a program is equal the number of instructions in the program excluding `nop`. Modify this method for a more precise performance model.

##### Testing the Simulator
Test if the simulator interpret the new instruction correctly by using `test-simulator.rkt`. 
- Modify `(define code ...)` to include the new instruction. 
- Set `input-state` to a desired input program state. 
- Run the file using either command line or DrRacket. 
- Inspect the output program state. 
- Then, comment out Phase C. Run again to make sure that it does not throw any exception. If an error occurs, there is something wrong.

##### Additional Notes
If we have to make many modifications to the simulator, it might be easier to only modify `llvm-simulator-rosette.rkt`. Once it works correctly, copy the entire file `llve-demo-simulator-rosette.rkt` to `llvm-simulator-racket.rkt`, and replace **any appearance** of `rosette` with `racket`.

### 5. Additional Changes for Superoptimizer

Once we have a simulator working properly, we need to modify a few more functions and definitions in `llvm-machine.rkt` to enable a superoptimizer.

##### Field `classes`
We must categorize the new instruction according to its arguments' type. Search for `(set! classes ...)`. `classes` is a vector of lists of opcodes. Each list of opcodes contains opcodes that have the same arguments' types. For example, `or xor add ...` are in the same list (class) because they take two arguments that are variables. To support `mul`, we put `mul` in the same class with `or xor add ...`. If the new instruction does not belong to any existing class, we have to create a new class for it. 
Note that our framework automatically associates class IDs according to the order.

##### Method `get-arg-types`
If we create a **new class** for the new instruction, we have to modify the method `get-arg-types` to return arguments' types for the new class. 

##### Method `get-arg-ranges`
If we create a **new argument type** in `get-arg-types`, we have to modify the method `get-arg-ranges` to return values of arguments to try in the stochastic and enumerative search. For example, `add#` takes one output argument of type 'var-o' (output variable), one input argument of type 'var-i' (input variable) and another input argument of type 'const'. `get-arg-ranges` returns that an argument of type 'var-i' can take any value in `var-range` that is live-in; an argument of type 'var-o' can take any value in `var-range` that is live-out.
An argument of type 'const' can take any value in `const-range`, which only includes a few values. In fact, the constant argument can take any 32-bit value, but we limit the stochastic and enumerative to try a few because they will take forever to synthesize any program if they try all 32-bit values.

The method `analyze-args-inst` may add more values to `const-range`, `bit-range`, `var-range`, etc. after analyzing the input spec program.

##### Method `analyze-args-inst`
If we create a **new argument type** in `get-arg-types`, we have to modify the method `analyze-args-inst`. The purpose of the method is to include arguments (e.g. variables, constants, etc.) appearing in the spec program into the collection of arguments our stochastic and enumerative superoptimizers should try. For example, currently the candidate programs that the stochastic superoptimizer creates only include variables, constants appearing in the spec program, and some default constants. This is controlled by `analyze-args-inst`. Note that the symbolic search does not use this method and always tries all possible arguments' values.

##### Method `update-live` & `update-live-backward`
If we create a **new class** for the new instruction, we may have to modify the method `update-live`. Essentially, `update-live` should set all locations of the program state modified by a given instruction to be live (i.e. `#t` for llvm). In the current implementation, for any instruction that is not `nop` and `store`, we mark that the output is live. Therefore, if we add a new instruction whose first argument is not an output variable, we will have to modify `update-live` to handle this instruction correctly.

If we create a **new argument type** that is not a constant, we have to modify the method `update-live-backward` as well.

##### Additional Changes for Enumerative Search
The enumerative search requires more efforts. If we add **new argument type**, we will need to modify method `generate-inst` to generate instructions in `llvm-enumerator.rkt` and method `interpret-inst` to interpret an instruction backward in `llvm-inverse.rkt`. If you are at this stage, please contact `mangpo@eecs.berkeley.edu`. We are working on eliminating this step entirely if possible.

##### Testing the Superoptimizer

Test if the symbolic, stochastic, and enumerative superoptimizers can synthesize the new instruction by using `test-search.rkt`. Set `code` definition to a program with only the new instruction and another dummy instruction (such as add 0) to create an obviously inefficient program. Add or remove `?` in `sketch` definition to define how many instructions an output program may contain; one `?` represents one instruction.

Run each of the symbolic, stochastic, and enumerative search at a time.
- For the stochastic search, make sure to provide the correct live-in information when testing.
- When the stochastic and enumerative search find a better program, they should print `FOUND!!!` followed by the program.
- The symbolic and enumerative search should return with an optimized program quickly (for program with one instruction).
- For the stochastic search, we might have to wait a little longer. If we don't see it printing `FOUND!!!` within a minute, something may be wrong.

To test that the backward search of the enumerative search works properly, run the enumerative search again but on a larger program (four instructions) that can be optimized to three instructions. This is because the enumerative search only performs the backward search when it tries to synthesize programs with three instructions or more. Make sure to add `?` in `sketch` definition if an output program contains more instructions.