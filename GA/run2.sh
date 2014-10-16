type=hybrid
mode=s
for name in fir rrotate
do
    for t in 1 2
    do
	echo "$name $t"
	racket optimize.rkt --hybrid -$mode -c 8 -t 600 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
    done
done

sh run3.sh
