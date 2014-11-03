name=$1
timeout=$2

echo "RUN $name ----------------------------------"

function e {
    echo "$@" >&2
    $@
}

mode=s
cost=inter
for type in hybrid stoch
do
    for t in 1 2 3
    do
	e racket optimize.rkt --$type -$mode --$cost -c 16 -t $timeout -d results/$name-$type-$cost-$mode-$t programs/$name.s > results/$name-$type-$cost-$mode-$t.log
    done
done

type=stoch
cost=base
for mode in s o
do
    for t in 1 2 3
    do
	e racket optimize.rkt --$type -$mode --$cost -c 16 -t $timeout -d results/$name-$type-$cost-$mode-$t programs/$name.s > results/$name-$type-$cost-$mode-$t.log
    done
done

