type=hybrid
mode=s
extra=window

name=p14_p15_p17_p21_o0

w=100
for t in 1 2
do
    echo "$name w=$w t=$t"
    racket optimize.rkt --live-out 0 --live-in 0,1,2,3,4,5 --dead-mem -w $w --$type -$mode -c 32 -t 3600 -d results/$name-$extra$w-$t programs/$name.s > results/$name-$extra$w-$t.log
done

w=80
for t in 1 2
do
    echo "$name w=$w t=$t"
    racket optimize.rkt --live-out 0 --live-in 0,1,2,3,4,5 --dead-mem -w $w --$type -$mode -c 32 -t 1200 -d results/$name-$extra$w-$t programs/$name.s > results/$name-$extra$w-$t.log
done

# name=p17_p21
# for w in 100 9
# do
#     for t in 1 2
#     do
#         racket optimize.rkt --live-out 0 --live-in 0,1,2,3 --dead-mem -w $w --$type -$mode -c 32 -t 3600 -d results/$name-$extra$w-$t programs/$name.s > results/$name-$extra$w-$t.log
#     done
# done

sh dummy.sh