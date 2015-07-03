name=$1
timeout=$2

echo "RUN $name ----------------------------------"

function e {
    echo "$@" >&2
    $@
}

cost=inter
type=enum
mode=p

for t in 1
do
    e racket optimize.rkt --$type -$mode --$cost -c 32 -t $timeout -d results/$name-$type-$cost-$mode-$t programs/$name.s > results/$name-$type-$cost-$mode-$t.log
done
