x=half
for file in rrotate complexA complexB complexC interp
do
    racket programs/$file.rkt > $file-$x-1.log
    mv output $file-$x-1.log
    racket programs/$file.rkt > $file-$x-2.log
    mv output $file-$x-2.log
done