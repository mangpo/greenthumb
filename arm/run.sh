name=$1
timeout=$2

echo "RUN $name ----------------------------------"

function e {
    echo "$@" >&2
    $@
}

cost=base
type=$3
mode=$4

for t in 2
do
    e racket optimize.rkt --$type -$mode --$cost -c 32 -t $timeout -d results/$name-$type-$cost-$mode-$t programs/$name.s > results/$name-$type-$cost-$mode-$t.log
done
