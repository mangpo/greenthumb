name=$1

function e {
    echo "$@" >&2
    $@
}

mode=s
cost=inter
type=hybrid

w=100
for t in 1 2 3
do
    e racket optimize.rkt --$type -$mode --$cost -c 16 -t 3600 -d results/$name-$type-$cost-w$w-$mode-$t programs/$name.s > results/$name-$type-$cost-w$w-$mode-$t.log
done

w=30
for t in 1 2 3
do
    e racket optimize.rkt --$type -$mode --$cost -c 16 -t 1200 -d results/$name-$type-$cost-w$w-$mode-$t programs/$name.s > results/$name-$type-$cost-w$w-$mode-$t.log
done

w=16
for t in 1 2 3
do
    e racket optimize.rkt --$type -$mode --$cost -c 16 -t 720 -d results/$name-$type-$cost-w$w-$mode-$t programs/$name.s > results/$name-$type-$cost-w$w-$mode-$t.log
done
