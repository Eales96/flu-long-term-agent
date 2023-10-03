from src.strain import Strain
import numpy as np


def calculate_distance(strain_a: Strain, strain_b: Strain):
    return np.sqrt((strain_a.ag_x - strain_b.ag_x) ** 2 + (strain_a.ag_y - strain_b.ag_y) ** 2)


def calculate_reactivity(distance, sigma, tau, num):
    cross_reactivity = np.exp(-distance * sigma)
    antigenic_seniority = np.exp(-tau * num)
    return cross_reactivity * antigenic_seniority


def calculate_sus(reactivity):
    return 1 / (1 + np.exp(-2 * reactivity + 4))


def calculate_cop(k_lt, alpha, beta):
    hiat = 10 * pow(2, k_lt - 1)
    return 1 - 1 / (1 + np.exp(beta * (np.log(hiat) - alpha)))


def calculate_cross_reactivity(distance, sigma):
    return max(0, 1 - sigma * distance)


def calculate_waning_rate(tsi, omega):
    return max(0, 1 - (tsi * omega / 365))


def calculate_antigenic_seniority(n_inf, tau):
    return max(0, 1 - tau * n_inf)


def calculate_contact_rates(age_cati, age_catj, contact_matrix):
    return contact_matrix[age_cati][age_catj]


def calculate_flight_contact_rates(age_cat, flight_contact):
    return flight_contact[age_cat]

