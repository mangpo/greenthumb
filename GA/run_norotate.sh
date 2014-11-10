name=$1
timeout=$2

echo "RUN $name ----------------------------------"

function e {
    echo "$@" >&2
    $@
}


type=stoch
for cost in inter
do
    for mode in s
    do
        for t in 1
        do
	    e racket optimize.rkt --$type -$mode --$cost -c 16 -t $timeout -d results/$name-$type-$cost-norot-$mode-$t programs/$name.s > results/$name-$type-$cost-norot-$mode-$t.log
        done
    done
done

