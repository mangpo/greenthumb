type=hybrid
mode=s
# for name in complexC
# do
#     for t in 1 2
#     do
# 	echo "$name $t"
# 	racket optimize.rkt --hybrid -$mode -c 32 -t 100 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
#     done
# done
t=1

name = complexC
racket optimize.rkt --$type -$mode -c 32 -t 100 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log

name = rrotate
racket optimize.rkt --$type -$mode -c 32 -t 1200 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log

name = interp
racket optimize.rkt --$type -$mode -c 32 -t 1600 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log

#sh run2.sh
