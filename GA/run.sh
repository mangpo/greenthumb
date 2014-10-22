type=hybrid+1p+half
mode=s
for name in complexC
do
    for t in 1 2
    do
	echo "$name $t"
	racket optimize.rkt --hybrid -$mode -c 24 -t 150 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
    done
done

sh run2.sh
