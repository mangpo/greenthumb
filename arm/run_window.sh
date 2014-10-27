type=hybrid
mode=s
extra=window

name=p17_p21_o0
for w in 100 50
do
    for t in 1 2
    do
    racket optimize.rkt --live-out 0 --live-in 0,1,2,3 --dead-mem -w $w --$type -$mode -c 32 -t 3600 -d results/$name-$extra$w-$t programs/$name.s > results/$name-$extra$w-$t.log
    done
done

name=p17_p21
for w in 100 9
do
    for t in 1 2
    do
        racket optimize.rkt --live-out 0 --live-in 0,1,2,3 --dead-mem -w $w --$type -$mode -c 32 -t 3600 -d results/$name-$extra$w-$t programs/$name.s > results/$name-$extra$w-$t.log
    done
done

sh dummy.sh