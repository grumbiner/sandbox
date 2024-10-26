from math import *
import numpy as np
import csv
import matplotlib
import matplotlib.pyplot as plt

# Some global parameters:
nobs = 579
nparameters = 6

######################################################################
#The score can be any function of the errors. RMSE is used here for demonstration 
#   purposes, but in truth, it could be anything. 
# exponential(delta) is fine (and will give very strange results in the 
#   evolution -- try it)
def score(delta, start, end, tolerance = 0):
    tmp = delta[start:end]
    tmp *= tmp
    return sqrt(sum(tmp)/(end-start+1))
    #tmp = np.exp(delta[start:end])
    #return sum(tmp)/(end-start+1)

#make a prediction from variables in the matchup x, using constants in the list y
#First prediction method:
def predict1(x,y):
    nx = len(x.values)
    #will ignore matchup[0] (day of observation) and matchup[6], the error term, 
    #  in making prediction
    #Starting point: simple multi-linear regression, a bias term plus 
    #  weights(y[k]) times the predictors
    pred = y[0]
    for k in range(1,nx-1):
        pred += x.values[k]*y[k]
    #print(pred, x.values[nx-1], x[nx-1])
    return (x.values[nx-1]+pred)

#take a set of matchups and evaluate (ultimately, to score) the predictions from predict1
#  note that we're now applying a start and end time -- the training period
def evaluator1(z, start, end, y):
    deltas = np.zeros((end-start+1))
    for k in range (start, end):
        deltas[k-start] = predict1(z[k],y)
    #print(deltas)
    return score(deltas,0,len(deltas))

#n.b. would be desirable to have a general evaluator that takes the 
#    prediction function as an argument as well

#Function to evolve the next generations -- mutation only in this one
def evolve(weights, sdevs):
    #sdevs[0]   = np.random.lognormal(sdevs[0],0.25)
    for k in range (0,len(weights)):
        weights[k] = np.random.normal(weights[k],sdevs[k])


############ data demo:
day = np.zeros((nobs))
t2m_gfs = np.zeros((nobs))
td_gfs = np.zeros((nobs))
thick_gfs = np.zeros((nobs))
rh_gfs = np.zeros((nobs))
speed = np.zeros((nobs))
obs_t2m = np.zeros((nobs))
obs_td = np.zeros((nobs))
terr = np.zeros((nobs))
tderr = np.zeros((nobs))
#could also make up a class 'matchup', and have 579 of those

with open('testin.csv') as csvfile:
    k = 0
    sreader = csv.reader(csvfile, delimiter=",")
    for line in sreader:
        day[k] = float(line[0])
        t2m_gfs[k] = float(line[1])
        td_gfs[k] = float(line[2])
        thick_gfs[k] = float(line[3])
        rh_gfs[k] = float(line[4])
        speed[k] = float(line[5])
        obs_t2m[k] = float(line[6])
        obs_td[k] = float(line[7])
        terr[k] = float(line[8])
        tderr[k] = float(line[9])
        k += 1
csvfile.close()

########################

class matchup:
    #values is a set (tuple or np.ndarray) of parameter values in the matchup
    def __init__(self, values): 
        self.values = values
    
    #display element by element the values of the matchup
    def show(self):
        n = len(self.values)
        for k in range(0,n):
            print(k,self.values[k])
    
    #extract the k-th parameter from the values tuple
    def __getitem__(self,k):
        return(self.values[k])
          
########################

# Now bring in the data for real work:
matchup_set = []
  
with open('testin.csv') as csvfile:
    k = 0
    sreader = csv.reader(csvfile, delimiter=",")
    for line in sreader:
        day = float(line[0])
        t2m_gfs = float(line[1])
        td_gfs = float(line[2])
        thick_gfs = float(line[3])
        rh_gfs = float(line[4])
        speed = float(line[5])
        obs_t2m= float(line[6])
        obs_td = float(line[7])
        terr = float(line[8])
        tderr = float(line[9])
        
        #Note that obs_td, obs_t2m, tderr are being ignored. They can be 
        #       added to the list.
        #  n.b.: note that it is terr that is used, not t2m itself. 
        #Model and observation are well-enough correlated that it is the increment 
        #which makes more sense to predict [Krasnopolsky,20NNN]
        m = matchup((day,t2m_gfs,td_gfs,thick_gfs,rh_gfs,speed,terr))
        matchup_set.append(m)
        k += 1
    
csvfile.close()
######################## ######################## ########################

######################## ######################## ########################
class critter:
    score = float(99.0)
    
    def __init__(self, nparm): 
        self.weights = np.zeros((nparm))
        self.sdevs = np.zeros((nparm))
        #self.score = evaluate(matchups, start, end, self.weights)
    
    def init(self, weights, sdevs):
        for k in range(0,len(weights)):
          self.weights[k] = weights[k]
          self.sdevs[k]   = sdevs[k] 
            
    def copy(self, x):
        for k in range(0,len(x.weights)):
          self.weights[k] = x.weights[k]
          self.sdevs[k]   = x.sdevs[k]
        #self.show()

    #display element by element the weights and sdevs
    def show(self):
        n = len(self.weights)
#        for k in range(0,n):
        for k in range(0,1):
            print(k,self.weights[k], self.sdevs[k])
            
    def evolve(self):
        evolve(self.weights, self.sdevs)
    
    def skill(self, matchups, start, end):
        self.score = evaluator1(matchups, start, end, self.weights)

######################## ######################## ########################
#Active program -- initialize and seed the population
npopulation = 10
population = []
for k in range (0,npopulation):
    population.append(critter(nparameters))
    
weights = np.zeros((nparameters))
sdevs   = np.zeros((nparameters))
sdevs[0] = 1.0
for k in range (0,npopulation):
    weights[0] = np.random.normal(0,1)
    population[k].init(weights,sdevs)
    print(k, population[k].weights[0], population[0].weights[0] )


#recall that the matchup_set is holding the matchups
smin = 9999.
kbest = int(99)

for k in range (0,npopulation):
    population[k].skill(matchup_set, 0, 364)
    if (population[k].score < smin):
        kbest = k
        smin = population[k].score 
    
print("k, smin = ",kbest, smin)
population[kbest].show()
######################## ######################## ########################
#swap best in to all slots
#then evolve a new raft of critters from that
#evaluate them
#repeat until limit of generations or happy
genmax = int(300)
population[kbest].show()
population[0].sdevs[0] = 0.25

for gen in range(0,genmax):
    population[0].copy(population[kbest])
    population[0].score = population[kbest].score
    score_best = float(population[0].score)
    smin = score_best
    kbest = 0
    for k in range (1, npopulation):
        population[k].copy(population[0])
        population[k].evolve()
        population[k].skill(matchup_set,0,364)
        if (population[k].score < score_best):
            kbest = k
            smin = population[k].score
    if (kbest != 0):
        print("new best ",gen, kbest, smin, score_best)
        population[kbest].show()
        
population[kbest].show()

print("\n")
print(evaluator1(matchup_set, 365,nobs,population[kbest].weights))

