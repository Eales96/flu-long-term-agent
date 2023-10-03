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

    with open("output/{}txt".format(output_name), "ab") as f:
        np.savetxt(f, inc_arr, delimiter=',')


pop_size = 80000 #160000
N_years = 160 #160
N_iter = 256 #128
n_jobs = -1

### Baseline scenario ##################################################################################################
output1 = "baseline"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.039)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "baseline_low"
param1 = Params(5.0, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.039)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "baseline_high"
param1 = Params(5.75, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.039)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

#### Waning immunity ###################################################################################################
output1 = "05_waning_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_omega=2)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "1_waning_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_omega=1.0)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "2_waning_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_omega=0.5)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "3_waning_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_omega=0.33)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "4_waning_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_omega=0.25)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "5_waning_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_omega=0.20)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

### Antigenic seniority ################################################################################################
# Realistic age-mixing
output1 = "0_antigenic_seniority"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.00)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "1_antigenic_seniority"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.01)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "2_antigenic_seniority"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.02)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "3_antigenic_seniority"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.03)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "5_antigenic_seniority"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.05)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "6_antigenic_seniority"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.06)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

########################################################################################################################
#Uniform age_mixing
output1 = "0_antigenic_seniority_uniform"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matU, flight_contacts=flight_mat, hi_tau=0.00)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "baseline_uniform"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matU, flight_contacts=flight_mat)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "6_antigenic_seniority_uniform"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matU, flight_contacts=flight_mat, hi_tau=0.06)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))


### Long-term immunity ################################################################################################

output1 = "10_longterm_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matU, flight_contacts=flight_mat, hi_mu1=1)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "15_longterm_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matU, flight_contacts=flight_mat, hi_mu1=1.5)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "25_longterm_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matU, flight_contacts=flight_mat, hi_mu1=2.5)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "30_longterm_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matU, flight_contacts=flight_mat, hi_mu1=3)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "35_longterm_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matU, flight_contacts=flight_mat, hi_mu1=3.5)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "40_longterm_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matU, flight_contacts=flight_mat, hi_mu1=4.0)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))


### Slow ones ##########################################################################################################
output1 = "7_antigenic_seniority"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.07)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "05_longterm_immunity"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matU, flight_contacts=flight_mat, hi_mu1=0.5)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

output1 = "8_antigenic_seniority"
param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_matR, flight_contacts=flight_mat, hi_tau=0.08)
np.savetxt("output/{}txt".format(output1), [])
Parallel(n_jobs=n_jobs)(delayed(simulation)(param1, pop_size, N_years, output1, i) for i in range(N_iter))

exit()