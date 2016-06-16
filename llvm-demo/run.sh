racket optimize.rkt --stoch -s -c 32 -t 3600 -d results/taps-stoch-s programs/taps.ll
racket optimize.rkt --enum -l -c 1 -t 3600 -d results/taps-enum-l-1 programs/taps.ll
racket optimize.rkt --enum -l -c 1 -t 3600 -d results/taps-enum-l-2 programs/taps.ll
racket optimize.rkt --enum -p -c 32 -t 3600 -d results/taps-enum-p programs/taps.ll
racket optimize.rkt --hybrid -p -c 32 -t 3600 -d results/taps-hybrid-p programs/taps.ll
