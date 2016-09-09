# Extending GreenThumb to a New ISA

## Getting Started
1. Set up GreenThumb (see the top-level [README](../README.md)).
2. Try to get the ARM cooperative superoptimizer running (see the top-level [README](../README.md)). 
3. Watch this demo [video](https://youtu.be/3l7Z7kB5p3g) on how to build an LLVM IR superoptimizer using GreenThumb.
4. Read Section 'Extending GreenThumb to a New ISA' below.
5. Follow [Greenthumb: Superoptimizer Construction Framework (Tech Report)](http://www.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-8.pdf).
   
## Brief Instructions
The framework utilizes inheritance to provide retargetability. The top level directory contains the superclasses to be extended. File `xxx.rkt` contains the implementation of superclass `xxx%`. Assume (without loss of generality) that we want to construct a superoptimizer for LLVM IR. To extend the framework , we have to implement class `llvm-xxx%` extending class `xxx%` (in `llvm/llvm-xxx.rkt` file).

Before start implementing the extension, run the setup script with the name of the ISA:
```
./setup.py llvm
```
The script will generate `llvm` directory that contains
- `llvm-xxx.rkt` programs. A program `llvm-xxx.rkt` is a skeleton code defining the class  `llvm-xxx%` extending the superclass `xxx%`. The generated code also includes the declarations of the required methods the users have to implement along with comments explaning the methods and their arguments.
- `test-simulator.rkt` program for testing the ISA simulator
- `test-search.rkt` program for testing the individual search techniques
- `main.rkt` and `optimize.rkt` programs for running the complete cooperative search launching multiple search instances

Now, we can start implementing our superoptimizer in the following order. The more detailed explanations about the classes and methods that need to be extended can be found at http://www.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-8.pdf.

- **Step 1**: Extend `machine%`. Open `llvm/llvm-machine.rkt` and complete the implementation before the "for stochastic and enumerative" section.
- **Step 2**: Extend `parser%` and `printer%`. Complete the implementation before the "for cooperative search" section. Use `test-simulator.rkt` to test the parser and the printer.
- **Step 3**: Extend `simulator-rosette%`. Use `test-simulator.rkt` and uncomment the next test to run the ISA simulator in Rosette. Then, copy the required methods implemented for `simulator-rosette%` to `simulator-racket%`. Use `test-simulator.rkt` to run the ISA simulator in Racket.
- **Step 4**: Extend `symbolic%`. Then, use `test-search.rkt` to test the symbolic search on a small code fragment.
- **Step 5**: Extend `stochastic%`, and implement more methods in `machine.rkt`; complete the implementation in the "for stochastic and enumerative" section in `llvm/llvm-machine.rkt`. Then, uncomment the stochastic search section in `test-search.rkt` and run it.
- **Step 6**: Extend `forwardbackward%`, `enumerator%`, and `inverse%` to enable the enumerative search. Then, uncomment the enumerative search section in `test-search.rkt` and run it.
- **Step 7**: To enable the cooperative search, we need to implement a few more methods:
   - methods `config-from-string-ir` and `output-string-constraint` of `llvm-printer%`
   - method `info-from-file` of `llvm-parser%`
   - method `len-limit` of `llvm-symbolic%` and `llvm-forwardbackward%`

Now, we can run our LLVM IR cooperative superoptimizer, similar to the way we run the ARM superoptimizer in the earlier section, using the generated `optimize.rkt`. Note that even if we do not implement all search techniques, for instance, we only implement `llvm-stochastic%`, we can still use `optimize.rkt` to run the stochastic search instaces in parallel, communicating to each other about the best program. 

#### Caution
GreenThumb relies on Rosette to verify correctness and perform symbolic search. Therefore, `simulator-rosette%` must be implemented in Rosette, which support most Racket operations but not all. For example, Rosette does not support any symbolic hash operation or some vector operations (e.g. vector-copy). If you wish to use some of these handy functions, take a look at provided functions in `ops-rosette.rkt`, which can be used instead of the original functions. We also provide the same functions but implemented more efficiently in `ops-racket.rkt`, which can be used in `simulator-racket%`.
