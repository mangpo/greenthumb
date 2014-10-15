type=livein_half
for name in p22_parity #p23_count p18_is_power p21_cycle p22_parity 
do
    for mode in s #o
    do
	for t in 1 2
	do
	    echo $name
	    racket optimize.rkt --live-out 0 --live-in 0 --stoch -$mode -c 8 -t 3600 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
	done
    done
done

sh run2.sh
