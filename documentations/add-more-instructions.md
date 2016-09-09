# Adding More Instructions to LLVM IR Superoptimizer

## Getting Started
1. Set up GreenThumb (see the top-level [README](../README.md)).
2. Try to get the ARM cooperative superoptimizer running (see the top-level [README](../README.md)). 
3. Familiarize yourself with the structure of GreenThumb by
   - Watch this demo [video](https://youtu.be/3l7Z7kB5p3g) on how to build an LLVM IR superoptimizer using GreenThumb.
   - Read Section 'Brief Instructions' in [Extending GreenThumb to New ISA](new-isa.md).
   - Skim through [Greenthumb: Superoptimizer Construction Framework (Tech Report)](http://www.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-8.pdf). No need to try it out. Just read.

<a name="Prerequisite"></a>
## Prerequisite Knowledge

#### Program State
In our llvm-demo ISA, a program state is minimal. It only includes values of variables in the program (no memory). We represent a LLVM program state as:
```
(vector v0 v1 v2 ...)
```
For example, the following concrete program state:
```
(vector 0 0 999)
```
represents a state of program with three variables. The first two variables (ID 0 and 1) are 0, and the third variable (ID 2) has value 999. Variables in a program we optimize are assigned to a unique ID starting from 0.

In order to support memory instructions and distinguish flags (e.g. carry flag), we will need to extend this representation to include memory and flags.

#### Live-out Information
When checking the equivalence of two expressions, we need to specify which locations in the output program states that need to be equal. Essentially, we only care about the live parts of the output program state (live-out). For example, when optimize the following code:
```
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
```
we do not care what the values of %in and %1 are at the end, but only care about the value of %out. In this case, the live-out only includes %out. Inside our framework, we use the same data structure that represents a program state to represent a liveness information. Instead of containing 32-bit numbers like program state, live-out contains boolean values. For example, say %in, %1, and %out have ID 0, 1, and 2 respectively. The live-out that only includes %out is represented by:
```
(vector #f #f #t)
```

<a name="Support"></a>
## How to Support More Instructions
To support more instructions, we need to modify several files as we will explain in this section.

#### 1. Program State
We may need to add more fields in our program state representation, `progstate` (which is a vector) in `llvm-demo-machine.rkt` to support more instructions. For example, we may need fields to represent memory. To support `mul`, we do not need to alter `progstate`.

#### 2. ASM Opcodes
Add `mul` to the list of `opcodes` in `llvm-demo-machine.rkt`. Search for `(set! opcodes '#(...))`.

#### 3. ASM Parser & Printer
If the new instruction does not follow the default instruction format `opcode arg1, arg2, ...`, we have to modify the parser in `llvm-demo-parser.rkt` to parse the instruction, and the methods `encode-inst`, `decode-inst`, and `print-syntax-inst` in `llvm-demo-printer.rkt`. To support `mul`, we do not need to alter the parser and printer.

---------------------------------------------------

#### 4. ASM Simulator
##### Method interpret
Modify the `interpret` method in `llvm-demo-simulator-rosette.rkt` and `llvm-demo-simulator-racket.rkt` to interpret the new instruction. To support `mul`, we need the case in the main conditional expression in `interpret`:
```
[(inst-eq `mul)   (rrr bvmul)]
```
Function `rrr` has already been defined. If you compare function `rrr` and `rri`, you will notice the difference between using the third argument as variable and as constant; r stands for variable (or register), and i stands for constant.
Next, define `bvmul` outside `interpret` function:
```
(define bvmul (bvop *))
 ```
Notice that `bvop` applies its argument lambda function to x and y, and then applies `finitize-bit` to the result. `(finitize-bit x)` calls `(finitize x bit)`, where the `finitize` function truncates overflowed `x` to `bit` bits and convert `x` to a signed number. Our convention is that every value in `progstate` has to be in the signed format (e.g. -1 instead of 2^32 - 1 for a 32-bit number). This is because we need the racket simulator to be consistent with the constraint solver we use which works with signed numbers.

##### Method performance-cost
Currently, we have a very simple performance model. The performance cost of a program is equal the number of instructions in the program excluding `nop`. Modify this method for a more precise performance model.

##### Testing the Simulator
Test if the simulator interpret the new instruction correctly by using `test-simulator.rkt`. 
- Modify `(define code ...)` to include the new instruction. 
- Set `input-state` to a desired input program state. 
- Run the file using either command line or DrRacket. 
- Inspect the output program state. 
- Then, comment out Phase C. Run again to make sure that it does not throw any exception. If an error occurs, there is something wrong.

##### Additional Notes
If we have to make many modifications to the simulator, it might be easier to only modify `llvm-demo-simulator-rosette.rkt`. Once it works correctly, copy the entire file `llve-demo-simulator-rosette.rkt` to `llvm-demo-simulator-racket.rkt`, and replace **any appearance** of `rosette` with `racket`.

#### 5. Additional Changes for Superoptimizer

In `llvm-demo-machine.rkt`, we need to modify the following.

##### `classes`
We must categorize the new instruction according to its arguments' type. Search for `(set! classes ...)`. `classes` is a vector of lists of opcodes. Each list of opcodes contains opcodes that have the same arguments' types. For example, `or xor add ...` are in the same list (class) because they take two arguments that are variables. To support `mul`, we put `mul` in the same class with `or xor add ...`. If the new instruction does not belong to any existing group, we have to create a new group for it. 
Note that our framework automatically associates class IDs according to the order.

##### `get-arg-types`
If we create a **new class** for the new instruction, we have to modify the method `get-arg-types` to return arguments' types for the new class. 

##### `get-arg-ranges`
If we create a **new argument type** in `get-arg-types`, we have to modify the method `get-arg-ranges` to return values of arguments to try in the stochastic and enumerative search. For example, `add#` takes one output argument of type 'var-o' (output variable), one input argument of type 'var-i' (input variable) and another input argument of type 'const'. `get-arg-ranges` returns that an argument of type 'var-i' can take any value in `var-range` that is live; similar for 'var-o'.
An argument of type 'const' can take any value in `const-range`, which only includes a few values. In fact, the constant argument can take any 32-bit value, but we limit the stochastic and enumerative to try a few because they will take forever to synthesize any program if they try all 32-bit values.

The method `analyze-args-inst` may add more values to `const-range`, `bit-range`, `reg-range`, etc. after analyzing the input spec program.

##### `analyze-args-inst`
If we create a **new argument type** in `get-arg-types`, we have to modify the method `analyze-args-inst`. The purpose of the method is to include arguments (e.g. registers, constans, etc.) appearing in the spec program into the collection of arguments our stochastic and enumerative superoptimizers should try. For example, currently the candidate programs that the stochastic superoptimizer creates only include registers, constants appearing in the spec program, and constants 0 and 1. This is controlled by `analyze-args-inst`. Note that the symbolic search does not use this method and always tries all possible arguments' values.

##### `update-live` & `update-live-backward`
If we create a **new class** for the new instruction, we may have to modify the method `update-live`. Essentially, `update-live` should set all locations of the program state modified by a given instruction to #t (true). In the current implementation, for any instruction that is not 'nop', we mark that the output is live. Therefore, if we add a new instruction whose first argument is not an output variable, we will have to modify `update-live` to handle this instruction correctly.

If we create a **new argument type** for variables (apart from 'var-i' and 'var-o'), we have to modify the method `update-live-backward`. If a new argument type is for constant, then we don't have to modify the function.

##### Testing the Superoptimizer

Test if the symbolic, enumerative, and stochastic superoptimizers can synthesize the new instruction by using `test-search.rkt`. Set `code` to a program with only the new instruction and another dummy instruction (such as `addl 0`) to create an obviously inefficient program. Run each of the symbolic, enumerative, and stochastic search at a time. The symbolic search should return with an optimized program quickly. For the stochastic search, when it finds a better program, it should print 'NEW! best-correct-program'. If we don't see it printing such phase within a minute, something may be wrong. ***Enumerative search***