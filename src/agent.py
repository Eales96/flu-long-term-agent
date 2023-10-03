from src.strain import Strain
from src.immunity_functions import *


class Agent:

    def __init__(self, age: int):
        self.current_infection = None
        self.infections = []
        self.infection_dates = []
        self.vaccinations = []
        self.vaccination_dates = []
        self.age = age

        # Update if more detailed age_structure used
        if age < 5:
            self.age_cat = 0
        elif age < 10:
            self.age_cat = 1
        elif age < 15:
            self.age_cat = 2
        elif age < 20:
            self.age_cat = 3
        elif age < 25:
            self.age_cat = 4
        elif age < 30:
            self.age_cat = 5
        elif age < 35:
            self.age_cat = 6
        elif age < 40:
            self.age_cat = 7
        elif age < 45:
            self.age_cat = 8
        elif age < 50:
            self.age_cat = 9
        elif age < 55:
            self.age_cat = 10
        elif age < 60:
            self.age_cat = 11
        elif age < 65:
            self.age_cat = 12
        elif age < 70:
            self.age_cat = 13
        elif age < 75:
            self.age_cat = 14
        else:
            self.age_cat = 15


    def reset_agent(self, age: int):
        self.current_infection = None
        self.infections = []
        self.infection_dates = []
        self.vaccinations = []
        self.vaccination_dates = []
        self.age = age

        # Update if more detailed age_structure used
        if age < 5:
            self.age_cat = 0
        elif age < 10:
            self.age_cat = 1
        elif age < 15:
            self.age_cat = 2
        elif age < 20:
            self.age_cat = 3
        elif age < 25:
            self.age_cat = 4
        elif age < 30:
            self.age_cat = 5
        elif age < 35:
            self.age_cat = 6
        elif age < 40:
            self.age_cat = 7
        elif age < 45:
            self.age_cat = 8
        elif age < 50:
            self.age_cat = 9
        elif age < 55:
            self.age_cat = 10
        elif age < 60:
            self.age_cat = 11
        elif age < 65:
            self.age_cat = 12
        elif age < 70:
            self.age_cat = 13
        elif age < 75:
            self.age_cat = 14
        else:
            self.age_cat = 15

    def is_infected(self):
        infected = True
        if self.current_infection is None:
            infected = False
        return infected

    def strain_infected(self):
        return self.current_infection

    def new_infection(self, s: Strain, t: float):
        self.current_infection = s
        self.infections.append(s)
        self.infection_dates.append(t)

    def clear_infection(self):
        self.current_infection = None

    def time_since_infection(self, t: float):
        tsi = t - self.infection_dates[-1]
        return tsi

    def number_of_infections(self):
        return len(self.infection_dates)

    def number_of_vaccinations(self):
        return len(self.vaccinations)

    def new_vaccination(self, s: Strain, t: float):
        self.vaccinations.append(s)
        self.vaccinations.append(s)
        self.vaccinations.append(t)

    def calculate_immunity2(self, s: Strain, t: float, p):
        num_inf = self.number_of_infections()
        titre = 0.0
        if num_inf > 0:
            for i in range(num_inf):
                tsi = t - self.infection_dates[i]
                dist = calculate_distance(s, self.infections[i])
                ags = calculate_antigenic_seniority(i, p.hi_tau)
                lt_cr = calculate_cross_reactivity(dist, p.hi_sigma1)
                st_cr = calculate_cross_reactivity(dist, p.hi_sigma2)
                wane = calculate_waning_rate(tsi, p.hi_omega)
                titre += ags * (lt_cr * p.hi_mu1 + st_cr * p.hi_mu2 * wane)

        immunity = 1 - calculate_cop(titre, p.cop_alpha, p.cop_beta)
        return immunity

    def calculate_immunity(self, s: Strain, t: float, p):
        num_inf = self.number_of_infections()
        titre = 0.0
        if num_inf > 0:
            for i in range(num_inf):
                tsi = t - self.infection_dates[i]
                dist = calculate_distance(s, self.infections[i])

                ags = calculate_antigenic_seniority(i, p.hi_tau)
                lt_cr = 0.0
                st_cr = 0.0
                wane = 0.0

                if dist < 7.7:
                    lt_cr = calculate_cross_reactivity(dist, p.hi_sigma1)
                if tsi < 365/p.hi_omega: #463
                    st_cr = calculate_cross_reactivity(dist, p.hi_sigma2)
                    wane = calculate_waning_rate(tsi, p.hi_omega)

                titre += ags * (lt_cr * p.hi_mu1 + st_cr * p.hi_mu2 * wane)

        immunity = 1 - calculate_cop(titre, p.cop_alpha, p.cop_beta)
        return immunity

    def happy_birthday(self):
        self.age += 1
        if self.age in [5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75]:
            self.age_cat += 1

