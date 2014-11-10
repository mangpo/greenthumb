name=$1
w=$2
timeout=$3

function e {
    echo "$@" >&2
    $@
}

mode=s
cost=inter
type=hybrid

for t in 2
do
    e racket optimize.rkt --$type -$mode --$cost -w $w -c 16 -t $timeout -d results/$name-$type-$cost-w$w-t$timeout-$mode-$t programs/$name.s > results/$name-$type-$cost-w$w-t$timeout-$mode-$t.log
done
