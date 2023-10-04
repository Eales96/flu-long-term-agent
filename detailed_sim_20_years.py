import numpy as np
from numpy import ndarray

from src.initialiser import *
import matplotlib.pyplot as plt
import seaborn
import numpy as np
from joblib import Parallel, delayed
np.random.seed(1234567890)
np.random.seed(111)

pop_size = 80000

contact_mat = np.loadtxt(open("input/age_contact_matrix.csv", "rb"), delimiter=",", skiprows=1, usecols=(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))
#contact_mat = np.loadtxt(open("input/age_contact_uniform.csv", "rb"), delimiter=",", skiprows=1, usecols=(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))
flight_mat = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

param1 = Params(5.5, 0.33, 0.00005, contact_matrix=contact_mat, flight_contacts=flight_mat, hi_tau=0.039)
res1 = GlobalRes(res_size=20,  v_x=0.99, v_y=1.85, sigma=1.14, time=0)
pop1 = Population(-0.25, param1, pop_size)

print("Beginning testing now")


#################################################################################################################


res_list = []
inf_list = []

date = []

inc_arr = np.empty([20, 80])
res_arr = np.empty([20, 40])
res_mu_arr = np.empty([2, 20])

for i in range(20):
    print(i)
    pop1.simulate_importations(res1)
    for j in range(365):
        pop1.time_step(res1)
        inf_list.append(pop1.get_inf())
        date.append(i+j/365.0)

    # Extract diagnostics
    inc_rates = pop1.get_attack_rate()
    inc_arr[i,:] = inc_rates
    res_list.append(res1.res)

    str_inc = pop1.get_incidence_curve(res1)

    print(str_inc)
    with open("output/inc_year_{}.csv".format(i), "ab") as f:
        np.savetxt(f, str_inc, delimiter=',')

    res_mu_arr[0, i] = res1.mu_x
    res_mu_arr[1, i] = res1.mu_y

    for j in range(20):
        res_arr[j, i*2] = res1.res[j].ag_x
        res_arr[j, (i * 2 +1)] = res1.res[j].ag_y



    # Update year long detals
    pop1.clear_all_infections()
    pop1.birth_death()
    res1.sim_drift(365.0 * (i + 1))

with open("output/res_year_{}.csv".format(0), "ab") as f:
    np.savetxt(f, res_arr, delimiter=',')

with open("output/res_mean_{}.csv".format(0), "ab") as f:
    np.savetxt(f, res_mu_arr, delimiter=',')
