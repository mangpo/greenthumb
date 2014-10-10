type=half
for name in p18_is_power p21_cycle p22_parity p23_count
do
    echo $name
    racket optimize.rkt --live-reg 0 --stoch -s -c 8 -t 3600 -d results/$name-$type-1 programs/$name.s > results/$name-$type-1.log
    racket optimize.rkt --live-reg 0 --stoch -s -c 8 -t 3600 -d results/$name-$type-2 programs/$name.s > restuls/$name-$type-2.log
done

sh run2.sh