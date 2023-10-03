import numpy as np
from numpy import ndarray

from src.initialiser import *
from joblib import Parallel, delayed

contact_matR = np.loadtxt(open("input/age_contact_matrix.csv", "rb"), delimiter=",", skiprows=1, usecols=(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))
flight_mat = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]


def simulation(param, pop_size, N_years, output_name, i, seasonality):
    print(i)
    global_res = GlobalRes(res_size=20, v_x=0.99, v_y=1.85, sigma=1.14, time=0)
    pop = Population(seasonality, param, pop_size)

    inc_arr = np.empty([N_years, 80])
    pk_array = np.empty([N_years])

    for i in range(N_years):
        pop.simulate_importations(global_res)
        for j in range(365):
            pop.time_step(global_res)

        ## Extract diagnostics
        # Attack rate
        inc_rates = pop.get_attack_rate()
        inc_arr[i, :] = inc_rates

        # Peak week
        pk_wk = pop.get_peak_week()
        pk_array[i] = pk_wk

        # Update year long detals
        pop.clear_all_infections()
        pop.birth_death()
        global_res.sim_drift(365.0 * (i + 1))

    with open("output/{}_ar".format(output_name), "ab") as f:
        np.savetxt(f, inc_arr, delimiter=',')
    with open("output/{}_pk_wk".format(output_name), "ab") as f:
        np.savetxt(f, [pk_array], delimiter=',')


pop_size = 80000 #160000
N_years = 40 #160
N_iter = 20 #128
n_jobs = -1

output1 = "parameter_selection"

for i in range(11):
    seas = -(0.1+0.02*i)
    print(seas)
    for j in range(11):
        beta = 4.5 + 0.2*j
        print(beta)
        param1 = Params(beta, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat)
        Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i, seas) for i in range(N_iter))