type=livein_half

for name in p22_parity_o0 p23_count_o0 p24_roundpower_o0 p17_off_right_o0 
do
    for mode in s #o
    do
	for t in 1 2
	do
	echo "$name mode=$mode"
	racket optimize.rkt --live-out 0 --live-in 0 --dead-mem --stoch -$mode -c 24 -t 3600 -d /scratch/mangpo/results/$name-$type-$mode-$t programs/$name.s > /scratch/mangpo/results/$name-$type-$mode-$t.log
	done
    done
done

sh run_kaofang4.sh
