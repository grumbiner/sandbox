from math import *
import numpy as np
import csv
import matplotlib
import matplotlib.pyplot as plt

# Some global parameters:
nobs = 579
#nparameters = 6
nparameters = 12
npopulation = 10
genmax = int(100*1000)
train_start = int(0)
train_end   = int(364)

from evolution1 import *

######################## ######################## ########################
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
#Active program -- initialize and seed the population
population = []
for k in range (0,npopulation):
    population.append(critter(nparameters))
    
weights = np.zeros((nparameters))
sdevs   = np.zeros((nparameters))
population[0].init(weights, sdevs)
print("uncorrected score in training period: ",population[0].skill(matchup_set, train_start, train_end) )
print("uncorrected score in evaluation period: ",population[0].skill(matchup_set, train_end+1, nobs), flush=True )

sdevs[0] = 1.0
for k in range (1,nparameters):
  sdevs[k] = 0.1

for k in range (0,npopulation):
  for l in range (0, int(nparameters/2) ):     #initialize only the linear part
    weights[l] = np.random.normal(0,1)
  population[k].init(weights,sdevs)

#recall that the matchup_set is holding the matchups
smin = 9999.
kbest = int(npopulation)
for k in range (0,npopulation):
    population[k].skill(matchup_set, train_start, train_end)
    if (population[k].score < smin):
        kbest = k
        smin = population[k].score 
    
print("initial kbest, smin = ",kbest, smin)
population[kbest].show()

######################## ######################## ########################
#swap best in to all slots
#then evolve a new raft of critters from that
#evaluate them
#repeat until limit of generations or happy
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
        population[k].skill(matchup_set, train_start, train_end)
        if (population[k].score < score_best):
            kbest = k
            smin = population[k].score
    if (kbest != 0):
        print("new best ",gen, kbest, smin, score_best)
        population[kbest].show()
        
population[kbest].show()

print("best score in training period: ",score_best,"\n")
print("score in the untrained period: ",population[kbest].skill(matchup_set, train_end+1, nobs))
