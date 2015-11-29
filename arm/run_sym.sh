for name in p21_cycle_o0 p22_parity_o0 p23_count_o0 p24_roundpower_o0 p10_nlz_eq_o0 p11_nlz_lt_o0 p12_nlz_le_o0 p18_is_power_o0 p19_exchange_o0 p20_next_higher_o0 
do
    bash run_extra.sh $name 3600 sym p
done

for name in p14_floor_avg_o0 p15_ceil_avg_o0
do
    bash run_extra.sh $name 1600 sym p
done

for name in p13_sign_o0 p16_max_o0 p17_off_right_o0 p25_smmul_o0                           
do
    bash run_extra.sh $name 300 sym p
done

bash dummy.sh
