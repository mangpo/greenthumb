# Greenthumb: Superoptimizer Construction Framework

GreenThumb is an extensible framework for constructing superoptimizers. It is designed to be easily extended to a new target ISA using inheritance. GreenThumb is implemented in Racket.
The top level directory contains ISA-independent files, which implement the superclasses to be extended. We have built superoptimizers for ARM, GreenArrays GA144 (GA), and a small subset of LLVM IR. Directories `arm`, `GA`, and `llvm-demo` contain ISA-sepcific files for ARM, GA, and LLVM IR respectively.

## References

- [Video](https://youtu.be/3l7Z7kB5p3g) demonstrates how to build a LLVM IR superoptimizer using GreenThumb.
- [Greenthumb: Superoptimizer Construction Framework (CC'16)](http://www.eecs.berkeley.edu/~mangpo/www/papers/greenthumb_cc2016.pdf) explains the framework overview.
- [Greenthumb: Superoptimizer Construction Framework (Tech Report)](http://www.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-8.pdf) explains the overview of how to extend the framework to support a new ISA.
- [Scaling Up Superoptimization (ASPLOS'16)](http://www.eecs.berkeley.edu/~mangpo/www/papers/lens-asplos16.pdf) explains the search strategy provided by GreenThumb.

## Software Prerequisites
- **Racket**: Download and install drracket from https://racket-lang.org/download/. Include the installed 'bin' directory, which contains racket, drracket, raco, and etc., to the environment path.
- **Rosette**: Download [Rosette v1.1](https://github.com/emina/rosette/releases/tag/v1.1) and follow the instruction to install in Rosette's README file. Don't forget to put Z3 executable in rosette/bin, as GreenThumb depends on Z3 (but not CVC4).
- **Python**

## Setting Up
```
git clone https://github.com/mangpo/greenthumb.git
cd greenthumb
make
```

`path.rkt` will be generated.


## Running an Existing Superoptimizer
For example, we will walk through how one can run the ARM supertoptimizer.

### Inputs
1. A code file, containing a straight-line ARM code. For example, see `arm/programs/p14_floor_avg_o0.s`.
2. A meta-data file, containing live-in and live-out information. The name of this file has to be the same as the code file appended with '.info', for example, `arm/programs/p14_floor_avg_o0.s.info`. The content of the meta-data file for ARM should look like the following:
```
0
0,1
```
The first line indicates live-out registers, and the second line indicates live-in regsiters. In this example, a live-out register is R0, and live-in registers are R0 and R1. Note that currently the tool only supports R registers.

### Run

You can superoptimize one of the example programs or your own program by running
```
cd arm
racket optimize.rkt <search-type> <search-mode> -c <number-of-search-instances> \
  -t <time-limit-in-sec> <file_to_optimize>
```

- `<search-type>` can be `--stoch` (stochastic search), `--solver` (symbolic search), `--enum` (enumerative search) or `--hybrid` (cooperative search).
- When using `--solver` or `--enum`, `<search-mode>` is either 
  - `-l` or `--linear`: optimizing by reducting cost decementally,
  - `-b` or `--binary`: optimizing by binary searching on the cost, or
  - `-p` or `--partial`: optimizing by synthesizing small parts of the given code using context-aware window decomposition. Option `--partial` is recommended.
- When using `--stoch`, `<search-mode>` is either 
  - `-s` or `--synthesize`: each search instance starts the search from a random program, or
  - `-o` or `--optimize`: each search instance starts the search from the original input program.

For example, to optimize `arm/programs/p14_floor_avg_o0.s` using cooperative search running all search techniques using eight search instances for an hour, run

```
racket optimize.rkt --hybrid -p -c 8 -t 3600 programs/p14_floor_avg_o0.s
```

Run `racket optimize.rkt --help` to see all supported arguments and what their default values are.

### Outputs
An output directory containing `driver-<id>.rkt` files will be created. The default name of the output directory is `output`. Use `-d` flag to specify the output directory's name. Each `driver-<id>.rkt` file runs a search instance. 

Each `driver-<id>.rkt`
- updates the shared file `best.s`, which always contains the current best program, if the search instance finds a better program than the current best.
- updates `summary` file, which contains the statistic of best programs found at different points of time.
- writes debug and error messages to `driver-<id>.log` 

At the beginning, the search driver will report the numbers of search instances allocated for different search techniques.

```
SEARCH INSTANCES
----------------
stoch:	2 instances
sym:	2 instances
enum:	4 instances

ID 0-1: stoch (optimize)          << driver-0 and 1 run stochastic search.
ID 2-2: sym (window=L)            << driver-2 runs symbolic search.
ID 3-3: sym (window=2L)           << driver-3 runs symbolic search.
ID 4-4: enum (no-decomposition)   << driver-4 runs enumerative search (no window).
ID 5-6: enum (window=L)           << driver-5 and 6 run enumerative search.
ID 7-7: enum (window=2L)          << driver-7 run enumerative search.
```

X in (window=X) indicates the size of window used in the context-aware window decomposition.

Then, the search driver will report the status of the search every 10 seconds.
```
// This first part is the statistics from all stochastic search instances.
elapsed time:   74
mutate time:    0.080541	<< (time spent on mutating)/total
simulate time:  0.126095	<< (time spent on interpreting)/total
check time:     0.054595	<< (time spent on checking actual outputs against expected outputs)/total
validate time:  0.702649	<< (time spent on validating the correctness using solver)/total

iterations/s:      5324.89
best cost:         5       << best cost despite the correctness
best correct cost: 5       << best performance cost considering the correctness
best correct time: 27      << time in seconds to find the best correct proposal

Mutate     Proposed  Accepted  Accepted/Proposed
opcode     0.217529  0.029182  0.134155
operand    0.220791  0.000276  0.001250
swap       0.061042  0.009864  0.161608
inst       0.043268  0.000167  0.003859
nop        0.175311  7.187534  0.000409
shf        0.282056  0.008171  0.028971
cond-type	 0.0       0.0       0

accept-count:        0.047734  << rate of accepting mutated programs
accept-higher-count: 0.000457  << rate of accepting mutated programs with higher cost

// This second part summarizes the charateristics of the best program found so far.
=============== SUMMARY ===============
cost:	3            << cost of the best program found so far
len:	3            << length of the best program found so far
time:	15           << time in seconds to find the best program
output/0/driver-7    << the best program is found by driver-7 (enumerative search in this example).
```

Press Ctrl-C to end the process early or wait until time is up. At the end, the search driver will print out the optimized program. Below is the output program when optimizing p14_floor_avg_o0.s.

```
OUTPUT
------
and r2, r0, r1
eor r3, r0, r1
add r0, r2, r3, asr 1
```

## Extending GreenThumb to a New ISA
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

## Reduce Memory Usage
To reduce the memory usage and the overhead of Racket translating program to bytecode, we can precompile our superoptimizer application to bytecode by running in command line:
```
raco make <list_of_flies_to_be_compiled>
```

## Inquery and Bug Report
If you are interested in using GreenThumb, please feel free to contact mangpo [at] eecs.berkeley.edu. If there is enough interest, we can write a very detailed step-by-step tutorial!
