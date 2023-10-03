from src.agent import Agent
from src.global_resovior import GlobalRes
from src.params import Params
import numpy as np
from src.immunity_functions import *


class Population:
    def __init__(self, seasonality: float, params: Params, pop_size: int):
        self.seasonality = seasonality
        self.params = params
        self.pop_size = pop_size
        self.inf = []
        self.sus = []
        self.mov = []
        self.date = 1

        # Update to assign different ages
        for i in range(pop_size):
            new_age = i - 80 * (i // 80)
            self.sus.append(Agent(new_age))

    def get_sus(self):
        return len(self.sus)

    def get_inf(self):
        return len(self.inf)

    def get_mov(self):
        return len(self.mov)

    def get_pop(self):
        return len(self.sus) + len(self.inf)

    def get_prop_sus(self):
        return self.get_sus() / self.get_pop()

    def get_prop_inf(self):
        return self.get_inf() / self.get_pop()

    # Get random functions might be needed

    def get_random_sus(self):
        return np.random.randint(0, self.get_sus())

    def get_random_inf(self):
        return np.random.randint(0, self.get_inf())

    def get_n_random_sus(self, n: int):
        return np.random.randint(0, self.get_sus(), n)

    def get_n_random_inf(self, n: int):
        return np.random.randint(0, self.get_inf(), n)

    # Functions defining one day time steps
    def step_forward(self):

        # add to date#
        pass

    def record_contacts(self):
        total_contact_rate = self.get_inf() * self.get_prop_sus() * self.params.beta * self.get_seasonality()
        new_contacts = np.random.poisson(total_contact_rate)
        return new_contacts

    def simulate_contacts(self):
        for i in range(self.record_contacts()):
            if self.get_sus() and self.get_inf() > 0:
                index = self.get_random_inf()
                sndex = self.get_random_sus()
                inf_agent = self.inf[index]
                sus_agent = self.sus[sndex]

                # Add some terms determining risk of infection
                strain = inf_agent.current_infection

                prob_infection = sus_agent.calculate_immunity(strain, self.date, self.params) * \
                                 calculate_contact_rates(sus_agent.age_cat, inf_agent.age_cat,
                                                         self.params.contact_matrix)

                if np.random.rand() < prob_infection:
                    sus_agent.new_infection(strain, self.date)
                    del self.sus[sndex]
                    self.mov.append(sus_agent)

    def finalise_infections(self):
        for i in range(self.get_mov()):
            mov_agent = self.mov[0]
            del self.mov[0]
            self.inf.append(mov_agent)

    def record_recoveries(self):
        total_recovery_rate = self.get_inf() * self.params.gamma
        new_recoveries = np.random.poisson(total_recovery_rate)
        return new_recoveries

    def simulate_recoveries(self):
        for i in range(self.record_recoveries()):
            if self.get_inf() > 0:
                index = self.get_random_inf()
                inf_agent = self.inf[index]
                inf_agent.clear_infection()
                del self.inf[index]
                self.sus.append(inf_agent)

    def record_importations(self):
        total_importation_rate = self.get_sus() * self.params.import_rate
        new_importations = np.random.poisson(total_importation_rate)
        return new_importations

    def simulate_importations(self, res: GlobalRes):
        for i in range(self.record_importations()):
            if self.get_sus() > 0:
                sndex = self.get_random_sus()
                sus_agent = self.sus[sndex]

                strain = res.get_random_strain()

                # Add some terms determining risk of infection
                prob_infection = sus_agent.calculate_immunity(strain, self.date, self.params) *\
                                 calculate_flight_contact_rates(sus_agent.age_cat, self.params.flight_contacts)

                if np.random.rand() < prob_infection:
                    sus_agent.new_infection(strain, self.date)
                    del self.sus[sndex]
                    self.inf.append(sus_agent)

    def get_seasonality(self):
        return 1 + self.seasonality * np.cos(2 * np.pi * self.date / 365)

    def time_step(self, res: GlobalRes):
        self.simulate_importations(res)
        self.simulate_contacts()
        self.simulate_recoveries()
        self.finalise_infections()
        self.date += 1

    # Functions defining one year time steps
    def birth_death(self):
        for i in range(self.get_sus()):
            if self.sus[i].age == 79:
                self.sus[i].reset_agent(0)
            else:
                self.sus[i].happy_birthday()

        del_count = 0
        for i in range(self.get_inf()):
            if self.inf[i - del_count].age < 79:
                self.inf[i - del_count].happy_birthday()
            else:
                self.inf[i - del_count].reset_agent(0)

                self.sus.append(self.inf[i - del_count])
                del self.inf[i - del_count]
                del_count += 1

    def clear_all_infections(self):
        for i in range(self.get_inf()):
            inf_agent = self.inf[0]
            inf_agent.clear_infection()
            del self.inf[0]
            self.sus.append(inf_agent)

    # Functions defining summary statistics that might be needed for visualisation
    def get_attack_rate(self):

        count = [0] * 80

        for i in range(self.get_sus()):
            agent = self.sus[i]
            age = agent.age
            inf_dates = agent.infection_dates
            inf_this_yr = [x for x in inf_dates if x > self.date-365]
            count[age] += len(inf_this_yr)

        for i in range(self.get_inf()):
            agent = self.inf[i]
            age = agent.age
            inf_dates = agent.infection_dates
            inf_this_yr = [x for x in inf_dates if x > self.date-365]
            count[age] += len(inf_this_yr)

        return count

    def get_incidence_curve(self, res: GlobalRes):
        str_inc = np.zeros([res.res_size, 365])

        for i in range(self.get_sus()):
            agent = self.sus[i]
            inf_dates = agent.infection_dates

            n_inf = len([x for x in inf_dates if x > self.date - 365])

            if n_inf != 0:
                inf_dates = agent.infection_dates[-n_inf:]
                infections = agent.infections[-n_inf:]
                for j in range(len(infections)):
                    res_index = res.res.index(infections[j])
                    str_inc[res_index, int(inf_dates[j] - self.date + 365)] += 1

        return str_inc

    def get_peak_week(self):
        str_inc = np.zeros([53])

        for i in range(self.get_sus()):
            agent = self.sus[i]
            inf_dates = agent.infection_dates

            n_inf = len([x for x in inf_dates if x > self.date-365])

            if n_inf != 0:
                inf_dates = agent.infection_dates[-n_inf:]
                infections = agent.infections[-n_inf:]
                for j in range(len(infections)):
                    # edit
                    str_inc[int((inf_dates[j] - self.date + 365)//7)] += 1

        return np.argmax(str_inc)

    def get_shannon_diversity(self, res: GlobalRes):
        pass

