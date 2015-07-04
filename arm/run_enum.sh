for name in p21_cycle_o0 p22_parity_o0 p23_count_o0
do
    bash run.sh $name 3600
done

for name in p10_nlz_eq_o0 p20_next_higher_o0 p24_roundpower_o0 p19_exchange_o0
do
    bash run.sh $name 400
done

bash dummy.sh
