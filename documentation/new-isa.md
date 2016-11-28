# Extending GreenThumb to a New ISA

## Getting Started
1. Set up GreenThumb (see the top-level [README](../README.md#setup)).
2. Try to get the ARM cooperative superoptimizer running (see the top-level [README](../README.md#running)). 
3. Read [Greenthumb: Superoptimizer Construction Framework (CC'16)](http://www.eecs.berkeley.edu/~mangpo/www/papers/greenthumb_cc2016.pdf).
   
## Step-by-Step Instruction
The framework utilizes inheritance to provide retargetability. The top level directory contains the superclasses to be extended. File `xxx.rkt` contains the implementation of superclass `xxx%`. For the purpose of a demonstration, assume we want to construct a superoptimizer for simplified ARM (called `armdemo`). To extend the framework , we have to implement class `armdemo-xxx%` extending class `xxx%` (in `armdemo/armdemo-xxx.rkt` file).

First, run the setup script with the name of the ISA:
```
./setup.py armdemo
```
The script will generate `armdemo` directory that contains
- `armdemo-xxx.rkt` programs. A program `armdemo-xxx.rkt` is a skeleton code defining the class  `armdemo-xxx%` extending the superclass `xxx%`. The generated code also includes the declarations of the required methods the users have to implement along with comments explaining the methods and examples. 
- `test-simulator.rkt` program for testing the ISA simulator
- `test-search.rkt` program for testing the individual search techniques
- `main.rkt` and `optimize.rkt` programs for running the complete cooperative search launching multiple search instances

The setup script generates example implementations of the required functions that accompany this documentation. The generated code includes `?` hooks that indicate locations that developers need to modify. If you use DrRacket as an editor, it will complain about `?`, and you can easily jump to `?`. For the purpose of the demo, if we remove those `?`s and simply use the example code, this demo should work out of the box.

In this documentation, we will walk you through building a superoptimizer in 7 steps under multiple sections.

[**Section A. ISA Basic Description**](#secA)
- [**Step 1**](#step1): Extend `machine%`. 

[**Section B. Program Intermediate Representations**](#secB)
- [**Step 2**](#step2): Extend `parser%` and `printer%`. Test the parser and printer using `test-simulator.rkt`.

[**Section C. ISA Semantics**](#secC)
- [**Step 3**](#step3): Extend `simulator-rosette%` and `simulator-racket%`. Test the simulators using `test-simulator.rkt`.

[**Section D. Enabling Search**](#secD)
- [**Step 4**](#step4): Extend `symbolic%`. Test the symbolic search using `test-searh.rkt`
- [**Step 5**](#step5): Extend `stochastic%`. Test the stochastic search using `test-searh.rkt`
- [**Step 6**](#step6): Extend `forwardbackward%`. Test the enumerative search using `test-searh.rkt`
- [**Step 7**](#step7): Enalbe the cooperative search by implementing:
   - method `info-from-file` of `armdemo-parser%`
   - methods `config-from-string-ir` and `output-string-constraint` of `armdemo-printer%`
   - method `len-limit` of `armdemo-symbolic%` and `armdemo-forwardbackward%`

<a name="secA"></a>
### A. ISA Basic Description
We must define the description of the ISA, including its opcodes, the ISA bitwidth, and the structure of the program state.

<a name="step1"></a>
**Step 1: machine.** 
We define such information in the class `armdemo-machine%`, which extends the class `machine%`. 

<a name="step1.1"></a>
**Step 1.1: bitwidth.** 
First, we define how many bits are used to represent the smallest unit of value by setting the field `bitwidth`. Since we are supporting ARM 32-bit arithmetic and logical instructions, we set bitwidth to 32.

<a name="step1.2"></a>
**Step 1.2: program state structure.** 
Next, we must define the structure of a program state (machine state/CPU state) and types of elements a program state contains. Say our `armdemo` processor contains 32-bit registers and memory (no flags). Therefore, there are two types of elements in our program state: register and memory. For each type of program state element, we need to name it using Racket [symbol](https://docs.racket-lang.org/reference/symbols.html). In this demo, we name register type `'reg`. For memory, GreenThumb provides special objects including memory, so the name of memory type can be obtained by calling `(get-memory-type)`. See more information about provided special objects [here](special-objects.md). 

Now we are ready to define the structure of a program state by overriding `progstate-structure` method. The program state structure must be defined in terms of program state elements' types.
For example, we define an `armdemo` program state as follows:
```racket
 (define (progstate-structure)
   (progstate (for/vector ([i config]) 'reg)
              (get-memory-type)))
```
where `progstate` is defined at the top of `armdemo-machine.rkt` as a macro for creating a vector with two entries. The first entry of our program state contains a vector of register's values, and the second entry contains memory. 

Notice that in an actual processor, the number of registers is fixed, but the number of registers in a piece of code we want to optimize is usually less than the number of available registers. Therefore, we use `config` to represent number of registers of interest. The smaller the number of registers (smaller program state in general) the smaller the search space. Now, you may ask, how does `config` get set? We can manually set `config` when creating `armdemo-machine%` in `test-simulator.rkt` and `test-search.rkt`. We can also extract `config` by analyzing the code we want to optimize (which will be explained later in [Step 7.2](#step7.2))

In general, a program state must be a **mutable object**, so we use vectors instead of lists (Racket list is immutable). This is crucial for the bidirectional search strategy in the enumerative superoptimizer.

<a name="step1.3"></a>

**Step 1.3: program state element type.** 
Next, we have to provide more information about each type of a program state element. In particular, we have to tell GreenThumb how to get and set a value of a specific program state element and what the valid values of such element. We do this by calling:
```racket
(define-progstate-type <element-type>
  #:get <a function to get a value>
  #:set <a function to set a value>
  #:min <optional minimum valid value (inclusive)>
  #:max <optional maximum valid value (inclusive)>)
```
For memory type, we define:
```racket
(define-progstate-type (get-memory-type)
  #:get (lambda (state) (progstate-memory state))
  #:set (lambda (state val) (set-progstate-memory! state val)))
```
For register type, we define:
```racket
(define-progstate-type 'reg 
  #:get (lambda (state arg) (vector-ref (progstate-regs state) arg))
  #:set (lambda (state arg val) (vector-set! (progstate-regs state) arg val)))
```
Since there are multiple elements of type register, the get and set functions take in a program state and an additional argument `arg` indicating which register it should reference. This additional argument is coming from operands in an instruction.

We use `#:min` and `#:max` for elements that do not take all values of the defined bitwidth. For example, if our program state includes a flag that only takes value 0 and 1, we can use `#:min` and `#:max` for such type of element.

These get and set functions are used for many tasks in the stochastic and enumerative search such as interpreting an instruction backward, updating liveness forward and backward, obtaining valid instruction operands, and obtaining value instructions.

<a name="step1.4"></a>

**Step 1.4: instruction operand type.** 
Next, we must define operand types of instructions. In particular, we have to tell GreenThumb which values the stochastic and enumerative superoptimizer should try when synthesizing an instruction. We do this by calling:
```racket
(define-arg-type <operand-type> (lambda (config) <a list of values>))
```
For `armdemo`, there are 3 types of instruction operands: `'reg` (register), `'const` (32-bit constant), and `'bit` (shifting constant, so it's values are less than 32) defined as follows:
```racket
(define-arg-type 'reg (lambda (config) (range config)))
(define-arg-type 'const (lambda (config) '(0 1 -1 -2 -8)))
(define-arg-type 'bit (lambda (config) '(0 1)))
```
Note that lists of values we define here are values the stochastic and enumerative superoptimizers try during synthesizing candidate programs. Although`'const` actually ranges from 0 to 2^32-1, we do not want the stochastic and enumerative superoptimizers to try all 2^32 values, so instead we inform them to only try the values we specified here. Additionally, GreenThumb analyzes an input program to be optimized, and automatically adds more values to these lists according to their operand types if those values appear in the input program.

Furthermore, notice that `'reg` is used as both a program state element type and an instruction operand type. When this happens, the value of an instruction operand of type `'reg` will be used as the additional argument for the get and set functions when accessing a program state element of type `'reg` (as described in [Step 1.3](#step1.3)). If we name this operand type differently, GreenThumb may not be able to pass the value of the operand to the get and set functions. See [Section Connection between Instruction Operand and Program State Element in Advanced Usage], if you need to name the operand type differently.

For other operand types, we may name them freely. However, GreenThumb treats `'const` and `'bit` specially. In particular, the enumerative search works in a reduced-bitwidth domain (4 bits instead of 32 bits). Therefore, the enumerative search converts the input source code into the reduced-bitwidth version. If an operand has type `'const` or `'bit`, GreenThumb will convert the constant appropriately for its kind. For example, value 31 of type `'bit` will be converted to 3; shifting by 31 bits in 32-bit domain is equivalent to shifting by 3 bits in 4-bit domain. Large numbers of type `'const` will be converted into 4-bit numbers by masking the lower 4 bits. If an operand has a type other than `'const` or `'bit`, GreenThumb will preserve its value during conversion.

<a name="step1.5"></a>

**Step 1.5: instruction classes.** Next, we define instructions in the ISA.
We must inform GreenThumb how many opcodes there are in an instruction by calling:
```racket
(init-machine-description <number of opcodes per an instruction>)
```
For many ISAs, there is one opcode in an instruction. For the purpose of the demo, `armdemo` has one opcode in an instruction. However, in practice, `arm` has multiple opcodes in an instruction, including a base opcode, a conditional suffix, and an optional shift. See [Section Instruction with Multiple Opcodes in Advanced Usage], if there are more than one opcode in an instruction.

Let's now define instructions in our ISA using the previously defined operand types and program state element types. 
Instructions of a same class are defined together. Instructions are in the same class if their operands have the same types; and their inputs from and outputs to a program state also have the same types. An instruction class can be defined as:
```racket
(define-instruction-class 
  <class name> <a list of opcodes>
  #:args <a list of operand types> #:ins <a list of inputs> #:outs < a list of outputs> 
  #:commute <optional pair of commutative operands>)
```
For example, we define:
```racket
(define-instruction-class 'rrr-commute '(add xor)
  #:args '(reg reg reg) #:ins '(1 2) #:outs '(0) #:commute '(1 . 2))
(define-instruction-class 'rri '(add# xor#)
  #:args '(reg reg const) #:ins '(1) #:outs '(0))
(define-instruction-class 'load '(load)
  #:args '(reg reg) #:ins (list 1 (get-memory-type)) #:outs '(0))
```
The lists given to `#:ins` and `#:outs` can contain numbers (which refer to operands from `#:args`) and program state element types. 

For instructions `add` and `xor`, `#:args '(reg reg reg)` informs that these instructions have 3 operands. `#:ins '(1 2)` informs that these instructions take two inputs, the values of program state elements specified by operands 1 and 2 (index starting from 0). `#:outs '(0)` informs that their output is stored in the program state element specified by operand 0. `#:commute '(1 . 2)` informs that operands 1 and 2 are commutative.

For instruction `add#`, we define `#:ins '(1)` instead of `#:ins '(1 2)`. This is because although `'const` (operand 2) is an input to `add#`, but `'const` is not a part of program state. GreenThumb only needs to know about inputs to and outputs from an instruction that are parts of program state. However, we can still define `#:ins '(1 2)` for `add#`, and GreenThumb will remove `2` from the list automatically. Note that in fact, `add#` is not an ARM instruction; it corresponds to instruction `add rX, rX, immediate`. However, we need to distinguish `add rX, rX, immediate` from `add rX, rX, rX`. Therefore, we need to create a different opcode name for `add rX, rX, immediate`.

For instruction `load`, we define `#:ins (list 1 (get-memory-type))` because there is an additional implicit input which is memory.

Finally, we must call the following function after defining all instruction classes.
```racket
(finalize-machine-description)
```

<a name="secB"></a>
### B. Program Intermediate Representations

GreenThumb provides an instruction representation, defined as `(struct inst (op args))`. An instruction representation is a building block for constructing program representations. GreenThumb uses three levels of program representations:

**Source** is a plain-text source. For example, the program `p` below is an ARM program in the source format:
```
lsr r0, r0, 3 ; r0 = r0 >> 3
lsl r0, r0, 3 ; r0 = r0 << 3
```

**String IR** is an IR after parsing a source, which is a vector of `inst`. Each `inst` includes an opcode in its field `op` and a vector of arguments in its field `args`. Opcodes and arguments are represented as strings. The program `p` in the string-IR format is:
```racket
(vector (inst "lsr" (vector "r0" "r0" "3"))
        (inst "lsl" (vector "r0" "r0" "3")))
```

**Encoded IR** is an IR after encoding a String IR. It is also a vector of `inst`, but its `op` and `args` fields contain integer IDs instead of strings. An opcode ID is an integer indexing into the `opcodes` vector in machine%. A register ID 'X' is an integer that maps to register "rX". For constants, we simply convert strings to numbers. The program `p` in the encoded-IR format may looks like:
```racket
(vector (inst 3 (vector 0 0 3))
        (inst 4 (vector 0 0 3)))
```
Note that "lsr r0, r0, 3" and "lsr r0, r0, r3" should have different opcode IDs. Our convention is to use `'lsr#` for the one with the constant operand and `'lsr` for the one with the register operand when defining instruction classes ([Step 1.5]($step1.5)).

All components except `parser%` and `printer%` work with an encoded IR, because it enables representing programs with bitvector logic formulas used in the symbolic search and equivalence validator (which verifies the equivalence of two programs).


<a name="step2"></a>

**Step 2: Parser and Printer** Since `parser%` and `printer%` are responsible for converting sources to encoded IRs and vice versa, we must extend them by implementing:
- class `armdemo-parser%`, which parses `armdemo` IR source code into string-IR format.
- three methods in the class `armdemo-printer%`: `print-syntax-inst` prints string-IR program in source format; `encode-inst` converts string-IR to encoded-IR format; and `decode-inst` converts encoded-IR to string-IR format.

Apart from being able to parse source code with complete instructions, the parser should be able to parse string "?" as `(inst #f #f)` for both String-IR and encoded-IR formats. This is used as a place holder for an instruction to be synthesized during testing.
See example code generated from the setup script on how to implement these methods including handling "?". After implementing these methods, use `test-simulator.rkt` to test the parser and printer. 

<a name="secC"></a>
### C. ISA Semantics
In order for GreenThumb to understand the semantics of the new ISA and evaluate the performance of different code fragments, we have to implement an functional simulator and define its performance model.


<a name="step3"></a>

**Step 3: Simulator** We must implement the methods `interpret` and `performance-cost` of the classes `armdemo-simulator-racket%` and `armdemo-simulator-rosette%`. The implementations of these two classes are in fact identical except that the former is implemented in `#lang racket`, while the latter in `#lang s-exp rosette`. The Racket simulator is used to interpret sequences of instructions on concrete inputs in the stochastic and enumerative search. The Rosette simulator is used by the symbolic search and equivalence validator. Although the Rosette simulator can also be used in the stochastic and enumerative search, it is slower than the Racket simulator. See example code generated from the setup script how to implement these methods. After implement these methods, use `test-simulator.rkt` to test the simulator. uncomment more code to test this.

##### Cautions
1. Rosette supports most Racket operations but not all. For example, Rosette does not support any symbolic hash operation or some vector operations (e.g. vector-copy). If you wish to use some of these handy functions, take a look at provided functions in `ops-rosette.rkt`, which can be used instead of the original functions. We also provide the same functions but implemented more efficiently in `ops-racket.rkt`, which can be used in `simulator-racket%`.
2. Notice the provided function `(finitize-bit x)`.  `(finitize-bit x)` calls `(finitize x bit)`, where the `finitize` function truncates overflowed `x` to `bit` bits and convert `x` to a signed number. Our convention is that every value in `progstate` has to be in the signed format (e.g. -1 instead of 2^32 - 1 for a 32-bit number). This is because we need the racket simulator to be consistent with the constraint solver we use, which works with signed numbers. Therefore, always call `finitize-bit` after performing an arithmetic operation.

##### Additional Note
It might be easier to only modify `armdemo-simulator-rosette.rkt`. Once it works correctly, copy the entire file `armdemo-simulator-rosette.rkt` to `armdemo-simulator-racket.rkt`, and replace any appearance of rosette with racket.

<a name="secD"></a>
### D. Enabling Search

<a name="step4"></a>
**Step 4: Symbolic Search** Once the simulator is working. The symbolic search should work right out of the box! Use `test-search.rkt` to test the symbolic search on a small code fragment. Make sure to test the search for every instruction.


<a name="step5"></a>
**Step 5: Stochastic Search**
We need to extend the method `(correctness-cost state1 state2 live)` of the class `stochastic%`. [STOKE (ASPLOS'13)](https://raw.githubusercontent.com/StanfordPL/stoke/develop/docs/papers/asplos13.pdf)
suggests a correctness cost to be the number of non-matching bits between the live outputs of program states `state1` and `state2` with rewards for correct (or nearly correct) values in wrong locations. 

`stochastic%` provides the method `(correctness-cost-base state1 state2 live diff-bits)` to calculate the suggested correctness cost when
each of `state1` and `state2` is a vector of values, given a lambda function `diff-bits` that counts number of non-matching bits between two values. Thus, we use `correctness-cost-base` for the register part of `armdemo` program state. 

The memory object `memory-racket%` also provides the method `(correctness-cost other-memory diff-bits bitwidth)`. For each memory location that has beeen modified, it counts the number of non-matching bits of the values from the two memory objects. Thus, we call this method for the memory part of `armdemo` program state. 

Once we implement this method, uncomment the stochastic search section in `test-search.rkt` and run it to test the stochastic search.


<a name="step6"></a>

**Step 6: Enumerative Search**
If our ISA has memory accessing instructions, we must extend the method `update-progstate-ins-load` and `update-progstate-ins-store` of the class `machine%`. This function is used for executing memory instruction backward, in particular, for modifying the input parts of a program state. See comments and examples in the generated code. For more complicated load and store instructions, see [Customize Inverse Interpreter for Load/Store Instructions in Advanced Usage](advanced-usage.md#inverse-load-store).
Once we implement these methods, uncomment the enumerative search section in `test-search.rkt` and run it to test the enumerative search.

#### Tips on Testing the Superoptimizer
Set `code` definition to a program with only a newly added instruction and another dummy instruction (such as add 0) to create an obviously inefficient program. Add or remove `?` in `sketch` definition to define how many instructions an output program may contain; one `?` represents one instruction.

Run each of the symbolic, stochastic, and enumerative search at a time.
- When the stochastic and enumerative search find a better program, they should print `FOUND!!!` followed by an output program.
- The symbolic and enumerative search should return with an optimized program quickly (for program with one instruction).
- For the stochastic search, we might have to wait a little longer. If we don't see it printing `FOUND!!!` within a minute, something may be wrong.

To test that the backward search of the enumerative search works properly, run the enumerative search again but on a larger program (four instructions) that can be optimized to three instructions. This is because the enumerative search only performs the backward search when it tries to synthesize programs with three instructions or more. Make sure to add more `?` in `sketch` definition if an output program contains more instructions.


<a name="step7"></a>

**Step 7: Cooperative Search**
To get the coopeative search working, we have to do the following.

**Step 7.1: Window Decomposition**
To help a search instance determine an appropriate size of a window in the context-aware window decomposition, we must implement the method `(len-limit)` of `armdemo-forwardbackward%` and `armdemo-symbolic%` to return L, the size of code fragment (the number of instructions) that can be synthesized within one minute. The cooperative search
varies the window size used for the different search instances; in particular, it uses L, 2L, 3L, and 4L window sizes.

<a name="step7.2"></a>
**Step 7.2: Liveness Information and Machine Config**
To invoke the cooperative superoptimizer, we run `optimize.rkt` and pass in the filename of code to be optimized. Along with the file containing code to be optimized, the superoptimizer expects another file with the same name appended by ".info" that contains live-out infomation and additional information if necessary. Therefore, the superoptimizer needs to parse the info file, so we must implement:
- the method `(info-from-file filename)` in `armdemo-parser.rkt` to parse live-out information from a given file. The file may contain more information such as preconditions of the inputs.
- the method `(output-constraint-string live)` in `armdemo-printer.rkt`. The argument `live` that is passed to this method is an output from `info-from-file` method. This method should convert `live` into a string that is evaluated to a program state that contains #t and #f, where #t indicates that the corresponding element in the program state is live.

The superoptimizer also needs to set the `config` field of the `machine%` class because we no longer create the machine object manually as in `test-search.rkt`. We must implement the method `(config-from-string-ir program)` to analyze a given program and returns a machine config. `program` is given in the string-IR format.

**Step 7.3: Running the Cooperative Search**
Run the superoptimizer using `armdemo/optimize.rkt`. The usage of `armdemo/optimize.rkt` is the same as the usage of `arm/optimizer.rkt` as explained in the top-level [README](../README.md#running).
Note that even if we do not implement all search techniques, for instance, we only implement `armdemo-stochastic%`, we can still use `optimize.rkt` to run the stochastic search instances in parallel, communicating to each other about the best program. 
