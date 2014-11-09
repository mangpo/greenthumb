name=$1
timeout=$2
w=$3

echo "RUN $name ----------------------------------"

function e {
    echo "$@" >&2
    $@
}

type=hybrid
mode=s
cost=inter

for t in 1
do
    e racket optimize.rkt --$type -$mode --$cost -c 16 -w $w -t $timeout -d results/$name-$type-$cost-w$w-$mode-$t programs/$name.s > results/$name-$type-$cost-w$w-$mode-$t.log
done
