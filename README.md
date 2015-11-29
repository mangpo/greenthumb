# Greenthumb: Superoptimizer Construction Framework

GreenThumb is an extensible framework for constructing superoptimizers. It is designed to be easily extended to a new target ISA using inheritance. GreenThumb is implemented in Racket.
The top level directory contains ISA-independent files, which implement the superclasses to be extended. We have built superoptimizers for ARM, GreenArrays GA144 (GA), and a small subset of LLVM IR. Directories `arm`, `GA`, and `llvm-demo` contain ISA-sepcific files for ARM, GA, and LLVM IR respectively.

## Software Prerequisites
- **Racket**: Download and install the latest drracket from https://racket-lang.org/download/. Include the installed racket bin directory to the environment path. The bin directory should contain racket, drracket, raco, and etc.
- **Rosette**: Download and follow the instruction to install rosette from https://github.com/emina/rosette
- **Python**

## Setting Up
Clone the repository: 
```
git clone https://github.com/mangpo/greenthumb.git
```

After obtaining all the source code, under supertopimizer directory run 
```
make
```

This will generate `path.rkt`.

Racket program can be executed without compiling. However, compiling is recommended since it will reduce the overhead of translating program source to bytecode. As a result, the program will run faster and use less memory. Compile  by running
```
raco make <list_of_flies_to_be_compiled>
```

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
The superoptimizer will create an output directory that contains `driver-<id>.rkt` files. The default output directory is named `output`. You can specify output directory name using `-d` flag. The number of `driver-<id>.rkt` files is equal to the number of search instances specified. Each `driver-<id>.rkt` file is used for executing a search instance. 

Each `driver-<id>.rkt`
- updates the most optimal program found so far in `driver-<id>.best` and `best.s` (best of all search instances)
- updates `summary` file, which contains statistic of best programs found at different points of time
- writes debug and error messages to `driver-<id>.log` 
- updates the statistic information in `driver-<id>.stat` every 1000 iterations by overwriting the old content if using the stochastic search.

At the beginning, the search driver will report the numbers of search instances allocated for different search techniques.

```
SEARCH INSTANCES
----------------
stoch:	2 instances
sym:	2 instances
enum:	4 instances

ID 0-1: stoch (optimize)         << driver-0 and 1 run stochastic search.
ID 2-2: sym (window=L/2)         << driver-2 runs symbolic search.
ID 3-3: sym (window=L)           << driver-3 runs symbolic search.
ID 4-4: enum (no-decomposition)  << driver-4 runs enumerative search (no window).
ID 5-6: enum (window=L/2)        << driver-5 and 6 run enumerative search.
ID 7-7: enum (window=L*2)        << driver-7 run enumerative search.
```

X in (window=X) indicates the size of window used in the context-aware window decomposition.

Then, every 10 second, the search driver will print the aggregated statistic of the search from all search instances. Here is an example of the statistic.
```
// This first part is the statistics from all stochastic search instances.
elapsed-time:   60
mutate-time:    0.011061	<< time spent on mutating/total
simulate-time:  0.848529	<< time spent on interpreting/total
check-time:     0.058881	<< time spent on checking the correctness against test cases/total
validate-time:  0.035559	<< time spent on validating the correctness using solver/total

validate-count: 0.000061	<< number of validation invocations/total count
correct-count:  0.000057	<< number of validation invocations that returns correct/total count

iterations:         167418
iterations/s:       3720.4
best-cost:          5       << best cost despite the correctness
best-correct-cost:  5       << best performance cost considering the correctness
best-correct-time:  37      << time in seconds to find the best correct proposal

Mutate      Proposed            Accepted            Accepted/Proposed
opcode      0.1286808272868922  0.11044402877693389 0.8582788213717369
operand     0.1471035672160758  0.12721613915365462 0.8648066227162993
swap        0.327319674532673   0.32115689667891    0.9811719907684687
inst        0.20960571357568197 0.13376954061918497 0.6381960602943442
nop         0.11775171864359524 0.11425287866768653 0.9702862937695304
shf         0.10282148787005306 0.0502050022464683  0.4882734464017671
cond-type   0.0                 0.0                 0

accept-count:           0.879237    << rate of accepting mutated programs
accept-higher-count:    0.000005    << rate of accepting mutated programs with higher cost

// This second part summarizes the charateristics of the best program found so far.
=============== SUMMARY ===============
cost:	3              << cost of the best program found so far
len:	3              << length of the best program found so far
time:	16             << time in seconds to find the best program
output/0/driver-5      << the best program is found by driver-<id> (5 in this case).
```

Hit Ctrl-C to end the process early or wait until time is up. At the end, the search driver will print out the optimized program. Below is the output program when optimizing p14_floor_avg_o0.s.

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

Now, we can start implementing our superoptimizer in the following order.
- **Step 1**: Extend `machine%`. Open `llvm/llvm-machine.rkt` and complete the implementation before the "for stochastic and enumerative" section.
- **Step 2**: Extend `parser%` and `printer%`. Use `test-simulator.rkt` to test the parser and the printer.
- **Step 3**: Extend `simulator-rosette%`. Use `test-simulator.rkt` and uncomment the next test to run the ISA simulator in Rosette. Then, copy the required methods implemented for `simulator-rosette%` to `simulator-racket%`. Use `test-simulator.rkt` to run the ISA simulator in Racket.
- **Step 4**: Extend `symbolic%`. Then, use `test-search.rkt` to test the symbolic search on a small code fragment.
- **Step 5**: Extend `stochastic%`, and implement more methods in `machine.rkt`; complete the implementation in the "for stochastic and enumerative" section in `llvm/llvm-machine.rkt`. Then, uncomment the stochastic search section in `test-search.rkt` and run it.
- **Step 6**: Extend `forwardbackward%`, `enumerator%`, and `inverse%` to enable the enumerative search. Then, uncomment the enumerative search section in `test-search.rkt` and run it.
- **Step 7**: To enable the cooperative search, we need to implement a few more methods:
   - method `output-string-constraint` of `llvm-machine%`
   - method `info-from-file` of `llvm-parser%`
   - method `len-limit` of `llvm-symbolic%` and `llvm-forwardbackward%`

Now, we can run our LLVM IR cooperative superoptimizer, similar to the way we run the ARM superoptimizer in the earlier section, using the generated `optimize.rkt`. Note that even if we do not implement all search techniques, for instance, we only implement `llvm-stochastic%`, we can still use `optimize.rkt` to run the stochastic search instaces in parallel, communicating to each other about the best program.

## Inquery and Bug Report
Please contact mangpo [at] eecs.berkeley.edu
