type=hybrid
mode=s
for name in complexA complexB complexC fir rrotate shaf
do
    for t in 1 2
    do
	echo "$name $t"
	racket optimize.rkt --hybrid -$mode -c 8 -t 100 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
    done
done

sh run2.sh
