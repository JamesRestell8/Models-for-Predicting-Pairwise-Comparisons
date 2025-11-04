import math
import random


class Match:
    def __init__(self, homeAttack, homeDefence, awayAttack, awayDefence, homeAdv, rho, maxGoals):
        self.homeAttack = homeAttack
        self.homeDefence = homeDefence
        self.awayAttack = awayAttack
        self.awayDefence = awayDefence
        self.homeAdv = homeAdv
        self.rho = rho
        self.maxGoals = maxGoals
        self.lambda_ = self.homeAttack * self.awayDefence * homeAdv
        self.mu = self.homeDefence * self.awayAttack
    
    def tau(self, x: int, y: int) -> float:
        if x == 0 and y == 0:
            return 1 - self.lambda_* self.mu * self.rho
        if x == 0 and y == 1:
            return 1 + self.lambda_ * self.rho
        if x == 1 and y == 0:
            return 1 + self.mu * self.rho
        if x == 1 and y == 1:
            return 1 - self.rho
        return 1

    def probJoint(self, homeGoals, awayGoals):
        return self.tau(homeGoals, awayGoals) * ((self.lambda_**homeGoals * math.exp(-self.lambda_)) / (math.factorial(homeGoals))) * ((self.mu**awayGoals * math.exp(-self.mu)) / (math.factorial(awayGoals)))
    
    @staticmethod
    def print2Darray(array):
        for i in range(len(array)):
            for j in range(len(array[i])):
                print(array[i][j], end=" ")
            print("")
    
    def getScoreMatrix(self):
        # 0-0 0-1 0-2 0-3 0-4
        # 1-0 1-1
        # 2-0     2-2
        # 3-0         3-3
        # 4-0             4-4
        scorelineProbs = []
        for i in range(self.maxGoals):
            thisLine = []
            for j in range(self.maxGoals):
                thisLine.append(round(self.probJoint(i, j), ndigits = 5))
            scorelineProbs.append(thisLine)

        return self.normaliseProbMatrix(scorelineProbs)
    
    def simulateResult(self):
        scorelineProbs = self.getScoreMatrix()
        scorelineProbs = self.normaliseProbMatrix(scorelineProbs)
        value = random.random()
        total = 0
        i = 0
        j = 0
        while total < value:
            total += scorelineProbs[i][j]
            if total > value:
                return [i, j]
            if j+1 == len(scorelineProbs[i]):
                j = 0
                i += 1
            else:
                j += 1
        return [self.maxGoals+1, self.maxGoals+1]
    
    def getOutcomeProbs(self):
        probs = self.getScoreMatrix()
        home = 0
        draw = 0
        away = 0
        for i in range(len(probs)):
            for j in range(len(probs[i])):
                if i == j:
                    draw += probs[i][j]
                if i < j:
                    away += probs[i][j]
                if i > j:
                    home += probs[i][j]
        return [home, draw, away]

    @staticmethod
    def normaliseProbArray(array):
        total = 0
        for item in array:
            total += item
        return [item / total for item in array]
    
    @staticmethod
    def normaliseProbMatrix(array):
        total = 0
        for i in range(len(array)):
            for j in range(len(array[i])):
                total += array[i][j]
        
        for i in range(len(array)):
            for j in range(len(array[i])):
                array[i][j] = array[i][j] / total
        return array