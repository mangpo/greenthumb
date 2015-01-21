for name in p13_sign_o0
do
    bash run.sh $name 60
done

for name in p14_floor_avg_o0 p15_ceil_avg_o0 p17_off_right_o0 p19_exchange_o0 p21_cycle_o0 p22_parity_o0 p23_count_o0 p24_roundpower_o0
do
    bash run.sh $name 3600
done

bash dummy.sh