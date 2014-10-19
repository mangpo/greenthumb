type=half
name=p17_off_right 
mode=s
t=1
echo "$name type=$type"
racket optimize.rkt --live-out 0 --dead-mem --stoch -$mode -c 24 -t 3600 -d /scratch/mangpo/results/$name-$type-$mode-$t programs/$name.s > /scratch/mangpo/results/$name-$type-$mode-$t.log

type=livein_half
echo "$name type=$type"
racket optimize.rkt --live-out 0 --live-in 0 --dead-mem --stoch -$mode -c 24 -t 3600 -d /scratch/mangpo/results/$name-$type-$mode-$t programs/$name.s > /scratch/mangpo/results/$name-$type-$mode-$t.log

sh run_kaofang4.sh
