import numpy as np
from numpy import ndarray

from src.initialiser import *
from joblib import Parallel, delayed

contact_matR = np.loadtxt(open("input/age_contact_matrix.csv", "rb"), delimiter=",", skiprows=1, usecols=(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))
contact_matU = np.loadtxt(open("input/age_contact_uniform.csv", "rb"), delimiter=",", skiprows=1, usecols=(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))
flight_mat = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

def simulation(param, pop_size, N_years, output_name, i):
    print(i)
    global_res = GlobalRes(res_size=20, v_x=0.99, v_y=1.85, sigma=1.14, time=0)
    pop = Population(-0.25, param, pop_size)

    inc_arr = np.empty([N_years, 80])

    for i in range(N_years):
        pop.simulate_importations(global_res)
        for j in range(365):
            pop.time_step(global_res)

        # Extract diagnostics
        inc_rates = pop.get_attack_rate()
        inc_arr[i, :] = inc_rates

        # Update year long detals
        pop.clear_all_infections()
        pop.birth_death()
        global_res.sim_drift(365.0 * (i + 1))

    with open("output/{}.csv".format(output_name), "ab") as f:
        np.savetxt(f, inc_arr, delimiter=',')


pop_size = 20000
N_years = 80
N_iter = 8
n_jobs = -1

### Baseline scenario ##################################################################################################
output1 = "demonstration"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.039)
np.savetxt("output/{}.csv".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))


exit()