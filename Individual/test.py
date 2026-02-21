import numpy as np
import pandas as pd
import math
import random
import os
import matplotlib.pyplot as plt


def getShinPrices(p: list, z: float):
    p = np.array(p)
    termOne = np.sqrt(z*p + (1-z)*np.power(p, 2))
    termTwo = 0
    for i in range(len(p)):
        termTwo += np.sqrt(z*p[i] + (1-z)*np.power(p[i], 2))
    return termOne * termTwo


odds = [0.5, 0.5]
z = 0
x = np.linspace(0, 1, 100)

probs = [[x[i], 1-x[i]] for i in range(len(x))]
prices = []
for i in range(len(x)):
    prices.append(getShinPrices(probs[i], z))

fig, ax = plt.subplots()

xplot = [probs[i][0] for i in range(len(probs))]
yplot = [prices[i][0] for i in range(len(prices))]
yplot2 = [prices[i][1] for i in range(len(prices))]

ax.plot(xplot, yplot, label=r"$\pi_1$")
ax.plot(xplot, yplot2, label=r"$\pi_2$")
ax.set_ylabel(r"$\pi_i$")
ax.set_xlabel(r"$p_1$")
ax.legend()
ax.set_title("Shin (1993) Prices for Bets on a Two-horse Race (z=0.2)")
plt.grid()
plt.savefig("shin1993prices.pdf")
plt.show()

print(prices[50])
