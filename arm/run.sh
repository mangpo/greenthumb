name=$1
timeout=$2

echo "RUN $name ----------------------------------"

function e {
    echo "$@" >&2
    $@
}

type=$3
mode=$4

for t in 1 2 3
do
    e racket optimize.rkt --$type -$mode -c 32 -t $timeout -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
done
