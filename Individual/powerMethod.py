import numpy as np

def f(odds: list, k: float) -> float:
    total = 0
    for odd in odds:
        total += (1/odd)**k
    return total - 1

def df(odds: list, k: float) -> float:
    total = 0
    for odd in odds:
        total += (1/odd)**k * np.log(1/odd)
    return total

odds = [2.7, 3, 2.8]

xn = 1
xn1 = 0
epsilon = 1e-8

while np.abs(xn1 - xn) > epsilon:
    xn = xn1
    xn1 = xn - (f(odds, xn) / df(odds, xn))

print(xn1)
newOdds = np.power(odds, xn1)
print(newOdds)
print(np.sum(np.divide(1, odds)))
print(np.sum(np.divide(1, newOdds)))
print(np.divide(1, newOdds))

