type=half
mode=s
for name in interp
do
    for t in 1 2
    do
	echo "$name $t"
	racket optimize.rkt --stoch -$mode -c 24 -t 3600 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
    done
done

sh run7.sh
