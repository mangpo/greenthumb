# Greenthumb: Superoptimizer Construction Framework

The framework consists of 2 parts: machine-independent and machine-dependent. The outermost directory contains machine-independent components. We have built superoptimizers for GreenArrays GA144, ARM, and very small subset of NEON as examples. 'arm' directory contains ARM's dependent files, and so on.

## Software Prerequisites
### Racket
1. Download and install the latest drracket from https://racket-lang.org/download/.
2. Include the installed racket bin directory to the environment path. The bin directory should contain racket, drracket, raco, and etc.

### Rosette
Download and install rosette from https://github.com/emina/rosette

### Python

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

## Running ARM superoptimizer

### Input
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
racket optimize.rkt <search-type> <symbolic-mode> <mutate-mode> <cost-function> \
-c <number-of-cores> -t <time-limit-in-sec> <file_to_optimize>
```

- `<search-type>` can be `--stoch` (mutation-based search), `--solver` (symbolic search), or `--hybrid` (both).
- When using --solver, `<symbolic-mode>` should be specified to `-l` or `--linear` (optimizing by reducting cost decementally); `-b` or `--binary` (optimizing by binary searching on the cost); `-p` or `--partial` (optimizing by synthesizing small parts of the given code).
- When using --stoch, `<mutate-mode` should be specified to `-s` or `--synthesize` (each synthesis thread starts the search from a random program); `-o` or `--optimize` (each synthesis thread starts the search from the original input program).
- `<cost-function>` can be `--base` (basic) or `--inter` (more advanced, using intermediate values).

For example, to optimize `arm/programs/p14_floor_avg_o0.s` using mutation-based search with random starting points and advanced cost function on eight theads for an hour, run

```
racket optimize.rkt --stoch -s --inter -c 8 -t 3600 programs/p14_floor_avg_o0.s
```

Run `racket optimize.rkt --help` to see all supported arguments and what their default values are.

### Output
#### Output directory and files
The program will create an output directory that contains `driver-<id>.rkt` files. The default output directory is named `output`. You can specify output directory name using `-d` flag. The number of driver-<id>.rkt files is equal to the number cores specified. Each driver-<id>.rkt file is used for executing an independent synthesis instance. 

Each driver-<id>.rkt 
- updates the statistic information to `driver-<id>.stat` every 1000 iterations by overwriting the old content
- write debug and error messages to `driver-<id>.log` 
- updates the most optimal program found so far to `driver-<id>.best` and `best.s` (global optimal)
- updates `summary` file, which contains statistic of best programs found at different points of time.

Every 10 second, the main thread will print the aggregated statistic of the search from all synthesis instances. Here is an example of the statistic.
```
elapsed-time:   10883
mutate-time:    0.011061	<< time spent on mutating/total
simulate-time:  0.848529	<< time spent on interpreting/total
check-time:     0.058881	<< time spent on checking the correctness against test cases/total
validate-time:  0.035559	<< time spent on validating the correctness using solver/total

validate-count: 0.000061	<< number of validation invocation/total count
correct-count:  0.000057	<< number of validation invocation that returns correct/total count

iterations:         89777000
iterations/s:       8249.287880180098
best-cost:          6       << best cost despite the correctness
best-correct-cost:  6       << best of the correct proposals
best-correct-time:  6328    << time in seconds to find the best correct proposal

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
output-id:              9           << best program is the output of driver-<id> (9 in this case)
                                    which can be found at driver-9.best or best.s
```

