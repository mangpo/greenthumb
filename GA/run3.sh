type=hybrid+1p+half
mode=s
for name in complexA rrotate shaf
do
    for t in 1 2
    do
	echo "$name $t"
	racket optimize.rkt --hybrid -$mode -c 24 -t 1000 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
    done
done

sh run4.sh
