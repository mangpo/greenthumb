# Greenthumb: Superoptimizer Construction Framework

GreenThumb is an extensible framework for constructing superoptimizers. It is designed to be easily extended to a new target ISA using inheritance. GreenThumb is implemented in Racket.
The top level directory contains ISA-independent files, which implement the superclasses to be extended. We have built superoptimizers for ARM, GreenArrays GA144 (GA), and a small subset of LLVM IR. Directories `arm`, `GA`, and `llvm` contain ISA-specific files for ARM, GA, and LLVM IR respectively.

## References

- [Greenthumb: Superoptimizer Construction Framework (CC'16)](http://www.eecs.berkeley.edu/~mangpo/www/papers/greenthumb_cc2016.pdf) explains the framework overview.
- [Scaling Up Superoptimization (ASPLOS'16)](http://www.eecs.berkeley.edu/~mangpo/www/papers/lens-asplos16.pdf) explains the search strategy provided by GreenThumb.

## Software Prerequisites
- **Racket**: Download and install drracket from https://racket-lang.org/download/. Include the installed 'bin' directory, which contains racket, drracket, raco, and etc., to the environment path.
- **Rosette**: Download [Rosette v1.1](https://github.com/emina/rosette/releases/tag/v1.1) and follow the instruction to install in Rosette's README file. Don't forget to put Z3 executable in rosette/bin, as GreenThumb depends on Z3 (but not CVC4).
- **Python**

<a name="setup"></a>
## Setting Up
```
git clone https://github.com/mangpo/greenthumb.git
cd greenthumb
make
```

`path.rkt` will be generated.

<a name="running"></a>
## Running an Existing Superoptimizer
For example, we will walk through how one can run the ARM supertoptimizer.

### Inputs
1. A code file, containing a straight-line ARM code. For example, see `arm/programs/p14_floor_avg_o0.s`.
2. A meta-data file, containing live-in and live-out information. The name of this file has to be the same as the code file appended with '.info', for example, `arm/programs/p14_floor_avg_o0.s.info`. The content of the meta-data file for ARM should look like the following:
```
0
0,1
```
The first line indicates live-out registers, and the second line indicates live-in registers. In this example, a live-out register is R0, and live-in registers are R0 and R1. Note that currently the tool only supports R registers. Live-in information is no longer used in GreenThumb 2.0.

### Run

You can superoptimize one of the example programs or your own program by running
```
cd arm
racket optimize.rkt <search-type> <search-mode> -c <number-of-search-instances> \
  -t <time-limit-in-sec> <file_to_optimize>
```

- `<search-type>` can be `--stoch` (stochastic search), `--sym` (symbolic search), `--enum` (enumerative search) or `--hybrid` (cooperative search).
- When using `--sym` or `--enum`, `<search-mode>` is either 
  - `-l` or `--linear`: optimizing by reducing cost incrementally,
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

iterations/s:      480.18
best cost:         5       << best cost despite the correctness
best correct cost: 5       << best performance cost considering the correctness
best correct time: 41      << time in seconds to find the best correct proposal

Mutate     Proposed  Accepted  Accepted/Proposed
opcode	   0.282484  0.058890  0.208473
operand	   0.182736  0.018227  0.099747
swap	   0.266824  0.092875  0.348078
inst	   0.118275  0.017906  0.151396
nop	       0.149678  0.015671  0.10470257303795003

accept-count:        0.047734  << rate of accepting mutated programs
accept-higher-count: 0.000457  << rate of accepting mutated programs with higher cost

// This second part summarizes the charateristics of the best program found so far.
// This section only appears if the superoptimizer found better program(s).
=============== SUMMARY ===============
cost:	3            << cost of the best program found so far
len:	3            << length of the best program found so far
time:	44           << time in seconds to find the best program
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

## Reduce Memory Usage
To reduce the memory usage and the overhead of Racket translating program to bytecode, we can precompile our superoptimizer application to bytecode by running in command line:
```
raco make <list_of_flies_to_be_compiled>
```

For example, in `ARM` directory, run:
```
raco make test-search.rkt ../parallel-driver.rkt
```

## More Documentation
- [Extending GreenThumb to a New ISA](documentation/new-isa.md)
- [Adding more instructions to an existing superoptimizer](documentation/add-more-instructions.md)
- [Special Objects for Program State](documentation/special-objects.md)
- [Advanced Usage](documentation/advanced-usage.md)


## Inquery and Bug Report
If you are interested in using GreenThumb, please feel free to contact mangpo [at] eecs.berkeley.edu.
