type=hybrid+2p+half11
mode=s
for name in iii_ex
do
    for t in 1 2
    do
	echo "$name $t"
	racket optimize.rkt --hybrid -$mode -c 24 -t 1600 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
    done
done

sh run5.sh
