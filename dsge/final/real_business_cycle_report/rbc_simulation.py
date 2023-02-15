import math
from random import uniform
from random import seed
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# set seed for reproducibility 
SEED = 48534

# GLOBAL VARIABLES
A = 4
B = 0.3
delta = 0.025

def ct(kt, zt):
    """
    Computes optimal consumption in time t
    Inputs: 
      kt: capital at time t
      zt: random productivity shock at time t 
    """
    return (B * math.sqrt(kt) + zt + (1 - delta) * kt) / (1 + A)

def it(kt, zt):
    """
    Computes optimal investment in time t
    Inputs: 
      kt: capital at time t
      zt: random productivity shock at time t 
    """
    return (A * (B * math.sqrt(kt) + zt) - (1 - delta) * kt) / (1 + A)

def ktplus1(kt, it):
    """
    Computes  kt+1 in time t
    Inputs: 
      kt: capital at time t
      it: optimal investment at time t 
    """
    return (1 - delta) * kt + it

def simulate(startyr=1971, endyr=2020, k0=3.7, distrib="unif", save = True, plot=True):
    """
    simulates business cycles
    """
    years = list(range(startyr, endyr + 1))
    ## initialize values 
    n = len(years)
    investment = [0] * n # container for simulated i
    consumption = [0] * n # conatiner for simulated c
    capital = [0] * n # container for capital 
    kt = k0 

    ## actual simulation
    for i in range(n):
        global SEED
        SEED += 1
        seed(SEED)
        np.random.seed(SEED)

        # define stochastic shock
        if distrib == "unif":
            zt = uniform(0, 1)
        elif distrib == "norm":
            zt = abs(np.random.normal(0.5, 0.017))

        # store all variablevalues
        investment[i] = it(kt, zt)
        consumption[i] = ct(kt, zt)
        capital[i] = kt
        # update capital
        kt = ktplus1(kt, investment[i])

    gdp = [x + y for x, y in zip(consumption, investment)] # gdp is consumption + investment

    ## save data 
    if save:
        if distrib == "unif":
            data_filename = "data/sim_unif_shock.csv"  
        elif distrib == "norm":
            data_filename = "data/sim_normal_shock.csv"
        df = pd.DataFrame(
            {'year': years,
            'investment': investment,
            'consumption': consumption,
            'capital': capital,
            'gdp': gdp
            }
        )
        df.to_csv(data_filename)

    ## plot results 
    if plot:
        if distrib == "unif":
            suffix = "Unif[0,1]"  
        elif distrib == "norm":
            suffix = "Normal(0.5, 0.017)"

        fig, axs = plt.subplots(2)
        fig.suptitle('Simulation Results')
        axs[0].plot(years, consumption, label="consumption")
        axs[0].plot(years, investment, label="investment")
        axs[0].plot(years, [0] * n, linestyle="--", color="black")
        axs[0].legend()
        axs[0].set_title("Simulated Consumption and Investment")
        axs[1].plot(years, gdp, label = "GDP", color="black")
        axs[1].set_title("Business Cycle (GDP), zt~{}".format(suffix))
        axs[1].legend()
        plt.show()

if __name__=="__main__":
    distrib = input("specify distribution of shock (either 'unif' for uniform or 'norm' for normal): ")
    simulate(1971, 2020, 3.7, distrib, True, True)

