# Advanced Usage
- [Connection between Instruction Operand and Program State Element](#connect-operand-progstate)
- [Vector Instructions](#vector)
- [Instruction with Multiple Opcodes](#multiple-opcodes)
- [Additional Pruning in Enumerative Search](#pruning)
- [Customize Inverse Interpreter for Load/Store Instructions](#inverse-load-store)

<a name="connect-operand-progstate"></a>
### Connection between Instruction Operand and Program State Element
As described in [Step 1.4 of Extending GreenThumb to a New ISA](new-isa.md#step1.4), if an instruction operand type is the same as a program state element type, the operand from an instruction will be used for getting and setting the program state element. However, sometimes we may want to define multiple instruction operand types that connects to the same program state element type. 

For example, consider these two instructions from ARM:
```
add r0, r0, r0
ldr r0, [sp, #24]
```
We can treat registers r0 and sp as the same operand type, but it is unlikely that r0 will be used as an memory address like sp. Therefore, when we synthesize an instruction, we may not want to try putting r0 as an address operand. To do so, we need different instruction operand types for registers, but they both should link to the same program state element `'reg`. GreenThumb supports this by allowing developers to explicitly link an instruction operand type to a program state element type:
```racket
(define-arg-type <operand-type> (lambda (config) <a list of values>) #:progstate <progstate-type>)
```

For ARM, we define:
```racket
(define-arg-type 'reg (lambda (config) (range config)) #:progstate 'reg)
(define-arg-type 'reg-sp (lambda (config) '(13)) #:progstate 'reg)       ;; sp = r13

(define-instruction-class 'rrr-commute-shf '(add and orr eor)
  #:args '(reg reg reg) #:ins '(1 2) #:outs '(0) #:commute '(1 . 2))
(define-instruction-class 'load# '(ldr#)
  #:args '(reg reg-sp addr) #:ins `(1 ,(get-memory-type)) #:outs '(0))
```
Notice that the second operand of `'ldr#` is `'reg-sp`.

<a name="vector"></a>
### Vector Instructions

##### Program State Element for Vector Register/Variable
Typically, we define a program state element as a primitive entity with the same number of bits as the defined bitwidth ([Step 1.1 of Extending GreenThumb to a New ISA](new-isa.md#step1.1)). In this section, we describe how to define a program state element as a collection of primitive entities. For example, we will define a program state element type `vec4` (vector register with 4 lanes) to be a vector of 4 primitive entities. To do so, we simply pass an additional argument `#:structure` when calling `define-progstate-type` ([Step 1.3 of Extending GreenThumb to a New ISA](new-isa.md#step1.3)) as follows:
```racket
 (define-progstate-type
      'vec4
      #:structure (for/vector ([i 4]) 'x) ;; a vector contains 4 primitive entities
      #:get ...
      #:set ...)
```
The expression passed to `#:structure` should evaluate to a collection of `'x`, where `'x` represents a primitive entity.

##### Instruction Operand for Vector Register/Variable
Next, before defining vector instructions, we need to define an instruction operand type associated with vector registers ([Step 1.4 of Extending GreenThumb to a New ISA](new-isa.md#step1.4)). In this example, we define:
```racket
(define-arg-type 'reg  (lambda (config) (range (car config)))) ;; # of scalar registers = (car config)
(define-arg-type 'vec4 (lambda (config) (range (cdr config)))) ;; # of vector registers = (cdr config)
```
##### Instruction Operand for Vector Constant
A scalar instruction may contain constants, while a vector instruction may contain vector constants. Therefore, we may need another type of instruction operand type. For example, in LLVM, we can add a vector variable to a vector of ones: `%out = add <4 x i32> %in, <i32 1, i32 1, i32 1, i32 1>`. In this case, we define an operand type for vector constants as:
```racket
(define-arg-type 'const-vec4
  (lambda (config) (list (vector 0 0 0 0)
                         (vector 1 1 1 1))))
```
Recall that the stochastic search and enumerative search will try to construct instructions from the values from this list, but the symbolic search is not.

##### Instruction Class for Vector Instruction
Now we can define an intruction class for vector add `vadd` and vector multiply `vmul` as follows:
```racket
(define-instruction-class
 'rrr-commute-vec4
 '(vadd vmul)
 #:scalar '(add mul) #:vector-width 4
 #:args '(vec4 vec4 vec4)
 #:ins '(1 2) #:outs '(0) #:commute '(1 . 2))
```
This is similar to defining instruction classes for scalar instructions except the additional arguments `#:scalar` and `#:vector-width`. When defining vector instructions, we have to inform GreenThumb their corresponding scalar version and vector width. If developers do not provide such information, the backward search part of the enumerative will not work. If a vector instruction has no corresponding scalar instruction, developers must create a fake scalar instruction, and set the cost of such instruction to be very high to prevent the superoptimizer from using it to synthesize output programs.

**Important note:** This vector support only works when there is one opcode per instruction (i.e. `(init-machine-description 1)`). If there are more than one opcodes per instruction, please contact mangpo@eecs.berkeley.edu


##### Instruction Skeleton (For Symbolic Search)
Typically, the framework automatically infers an instruction skeleton from the defined instruction classes by treating all instruction operands to be primitive numbers. However, vector instructions may contain non-primitive numbers as operands, such as vector constants; a register/variable (either scalar or vector) is considered to be a primitive number because we use a single ID which is a number to represent a register/variable. Becase of vector constants, we have to manually provide the instruction skeleton by overriding the method `(gen-sym-inst)` of the class `symbolic%`. The skeleton can be constructed by using the provided lambda functions `sym-op` and `sym-arg`. The framework expects a program skeleton to be an `inst` with `(sym-op)` as its opcode and `(sym-arg)` as its arguments.

For example, the default `(gen-sym-inst)` for scalar instructions may look like:
```racket
(define/override (gen-sym-inst)
  (define n (get-field max-number-of-args machine))
  (inst (sym-op)
        (for/vector ([i n]) (sym-arg))))
```
However, `(sym-arg)` represents a privitive number. Since we know that the first operand cannot be is always a register/variable (either scalar or vector), vector constants cannot be the first operand. Therefore, we define the first entry of operands (or arguments) to be `(sym-arg)` as before, but the rest are `(sym-arg-vec)`; we define `(sym-arg-vec)` to either be `(sym-arg)` or a vector of four `(sym-arg)`s.
```racket
(define/override (gen-sym-inst)
  (define n (get-field max-number-of-args machine))
  (inst (sym-op)
        (vector-append (vector (sym-arg))
                       (for/vector ([i (sub1 n)]) (sym-arg-vec)))))
                       
(define (sym-arg-vec) ;; either sym-arg or 4 x sym-arg
  (define-symbolic* is-num boolean?) ;; decision variable
  (if is-num          
      (sym-arg)
      (vector (sym-arg) (sym-arg) (sym-arg) (sym-arg))))
```

##### Working Example
Our LLVM IR superoptimizer supports a few vector instructions. See `llvm/llvm-machine.rkt` on how to define vector instructions and `llvm/llvm-symbolic.rkt` on how to define the instruction skeleton.

<a name="multiple-opcodes"></a>
### Instruction with Multiple Opcodes
So far, we assume that an instruction can contain only one opcode, but in some ISAs, this is not the case. An ARM instruction has up to 3 opcodes: a base opcode, a conditional suffix, and an optional shift opcode (e.g. `addeq r0, r0, r1, asr #1`). GreenThumb allows developers to define instructions with multiple opcodes using the same `define-instruction-class`. Typically, when an instruction consists of one opcode, we define the instruction using:
```racket
(init-machine-description 1)
(define-instruction-class 
  <class name> <a list of opcodes>
  #:args <a list of operand types> #:ins <a list of inputs> #:outs <a list of outputs> 
  #:commute <optional pair of commutative operands>)
```
When an instruction consists of `n` opcodes, we define the instruction using:
```racket
(init-machine-description n)
(define-instruction-class 
  <class name> (list l_0 l_1 .. l_n-1)
  #:required (list r_0 r_1 ... r_n-1)
  #:args (list a_0 a_1 ... a_n-1) #:ins (list i_0 i_1 ... i_n-1) #:outs <a list of outputs> 
  #:commute <optional pair of commutative operands>)
  
;; l_x = a list of opcodes type x
;; r_x = #t or #f indicating if opcodes type x are required (#t) or optional (#f)
;; a_x = a list of operands for opcodes type x
;; i_x = a list of inputs for opcodes type x
```
This definition will define instructions with all combinations of opcodes from `l_0`, `l_1`, ..., `l_n-1`.
For example, in ARM ISA, the instructions `addeq r0, r0, r1, asr #1` and `orrhi r0, r0, r1m lsl #1` are defined in this class:
```racket
(define-instruction-class 'rrr-commute-shf
  (list '(add and orr eor) '(eq ne ls hi cc cs lt ge) '(asr lsl lsr ror))
  #:required '(#t #f #f)
  #:args '((reg reg reg) () (reg)) #:ins '((1 2) (z 0) (3)) #:outs '(0) #:commute '(1 . 2))
```
`#:required` indicates which opcode types are required and optional. Since the second and third types of opcodes are optional, this instruction class include an instruction like `add r0, r0, r1` as well. The second entry `(z 0)` of `#:ins` indicates that when an instruction contains `eq ne ls hi cc cs lt` or `ge`, flag `z` in a program state is an input to this instruction. The first instruction operand is also considered as an input because if the conditional is evaluated to false, the value of the first operand remains the same.

Consider another instruction class of ARM that includes `asreq r0, r0, r1` and `asr r0, r0, r1`:
```racket
(define-instruction-class 'rrr
  (list '(asr lsl lsr ror sdiv udiv uxtah) '(eq ne ls hi cc cs lt ge))
  #:required '(#t #f)
  #:args '((reg reg reg) ()) #:ins '((1 2) (z 0)) #:outs '(0))
```
Since base opcodes `asr lsl lsr ror sdiv udiv uxtah` do not have the optional shift, we exclude the shift opcodes from the definition entirely. Note that we must put a list of opcodes in the right entry (according to the previously defined instruction classes); for example, we should not swap the order of `'(asr lsl lsr ror sdiv udiv uxtah)` and `'(eq ne ls hi cc cs lt ge)`.

Once we call `(finalize-machine-description)`, the field `opcodes` of the object `machine%` will be set to `(vector opcodes_0 opcodes_1 ... opcodes_n-1)` where `opcodes_x` is a vector of all opcode names of type x. `(get-opcode-id name)` method of `machine%` object converts a vector of opcode names (`'#(name_0 name_1 ... name_n-1)`) to a vector of opcode IDs (`'#(id_0 id_1 ... id_n-1)`), and `(get-opcode-name id)` method is the inverse. For an instruction that does not consist of all types of opcodes, the ID of a missing opcode of any type is -1; for example, the opcode ID of `asr r0, r0, r1` is `'#(7 -1 -1)`.

<a name="pruning"></a>
### Additional Pruning in Enumerative Search
The enumerative search prunes the search space by considering liveness information. It only creates candidate programs that only use the live parts of program states. However, it may miss some other pruning strategies.

GreenThumb allows developers to filter out instructions to try by extending the methods `get-pruning-info` and `filter-with-pruning-info` of the class `enumerator%`.

**`(get-pruning-info state)`** takes in a program state and return an information that may be useful for pruning. The output of this method will be passed to `filter-with-pruning-info`. The default implementation returns #f.

**`(filter-with-pruning-info opcode-pool prune-in prune-out-list #:no-args [no-args #f] #:try-cmp [try-cmp #f])`** takes in a list of opcode IDs (`opcode-pool`), the pruning info of an input state (`prune-in`) and a list of the pruning info of output states (`prune-out-list`). This method should return a list of opcode IDs from `opcode-pool` that can potentially transition an input state whose pruning info is `prune-in` to one of the output states whose pruning info are `prune-out-list`. If an input state is unknown, `prune-in` = #f. If output states are unknown, `prune-out-list` = #f.

For example, we extend `get-pruning-info` of `arm-enumerator%` to return the `z` flag of a given program state. We then extend `filter-with-pruning-info` so that if `prune-in` (the z flag of the input state) is *not* in `prune-out-list` (a list of z flags of the output states), we should return instructions that can potentially change the z flag. Therefore, we filter out opcode IDs that are not correspond to `tst` or `cmp` in this scenario. We also implement another pruning strategy when `prune-in` (z flag) is -1 (unknown), we should not use any conditional suffix, so we filter out opcode IDs that has conditional suffix. See `arm-enumerator.rkt` for the implementation.

The optional argument `no-args` is set to #t, when the enumerative search uses this method to enumerate instructions to generate tables that memorize inverse behaviors. The optional argument `try-cmp` is set to #t, when the enumerative search wants to try compare instructions (e.g. `cmp` and `tst`); don't filter out compare instructions. If `try-cmp` is set to #f, we should filter out compare instructions.

<a name="inverse"></a>
### Customize Inverse Interpreter
Developers may want to implement their own function for interpreting an instruction backward. For example, when we use the default inverse interpret function for ARM, the enumerative search took a very long time to generate the tables that memorize inverse behaviors of instructions. This is because for each combination of a base opcode and an optional shift, there are many conditional suffixes; as a result, the default implementation generates inverse behaviors for them all despite the fact that a conditional suffix behave the same regardless of a base opcode. 

Therefore, we made two modifications:
- We only generate tables of inverse behaviors of instructions without a conditional suffix. To do this, we simply extend `filter-with-pruning-info` of `arm-enumerator%` such that when the argument `no-args` is set to #t, we only return opcodes without a conditional suffix. 
- Since we don't memorize inverse behaviors of conditional opcodes, we have to implement their inverse behaviors manually by extending the `interpret-inst` method of the `inverse%` class. See `arm-inverser.rkt` for the implementation. Keep in mind that the inverse interpretation of an instruction happens in the reduced-bitwidth domain (4-bit).

<a name="inverse-load-store"></a>
### Customize Inverse Interpreter for Load/Store Instructions
Recall that we have to extend the methods `update-progstate-ins-load` and `update-progstate-ins-store` of the class `machine%` to enable the enumerative search to interpret load and store instructions backward. For `llvm` and `armdemo`, the implementations of these two methods are straightforward, but it is more complicated in `arm`.

Consider an LLVM load instruction `%1 = load i32, i32* %0` and the method `(update-progstate-ins-load my-inst addr mem state)`. In this method, the address `addr` is given, so we can simply set `%0` to `addr`. However, consider an ARM load instruction `ldr r0 [sp, r1]`. Given an address, we can't simply set `sp` to `addr`, but we need to set `sp` and `r1` to all combination of values whose sum is `addr`. See `arm-machine.rkt` on how we implement these methods for ARM. Keep in mind that the inverse interpretation of an instruction happens in the reduced-bitwidth domain (4-bit).
