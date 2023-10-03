import numpy as np
from src.strain import Strain


class GlobalRes:

    def __init__(self, res_size: int, v_x: float, v_y: float, sigma: float, time: int):
        # v_x is exponential rate for jumps along antigenic dimension one.
        # v_y is the variance of gaussian jumps in antigenic dimension two.
        self.res_size = res_size
        self.v_x = v_x
        self.v_y = v_y
        self.sigma = sigma
        self.mu_x = 0.0
        self.mu_y = 0.0
        self.time_last_updated = time
        self.res = []
        for i in range(self.res_size):
            rnd_x = np.random.normal(self.mu_x, self.sigma)
            rnd_y = np.random.normal(self.mu_y, self.sigma)

            self.res.append(Strain(rnd_x, rnd_y))

    def sim_drift(self, time):
        exp_rate = self.v_x*(time - self.time_last_updated)/365
        jump_rate = np.sqrt(self.v_y*(time - self.time_last_updated)/365)
        self.mu_x += np.random.exponential(1/exp_rate)
        self.mu_y += np.random.normal(0.0, jump_rate)
        self.time_last_updated = time

        self.res = []
        for i in range(self.res_size):
            rnd_x = np.random.normal(self.mu_x, self.sigma)
            rnd_y = np.random.normal(self.mu_y, self.sigma)
            self.res.append(Strain(rnd_x, rnd_y))

    def get_random_strain(self):
        index = np.random.randint(0, self.res_size)
        return self.res[index]

    def get_index_of_strain(self):
        pass