from math import *
import numpy as np
import csv
import matplotlib
import matplotlib.pyplot as plt

# Some global parameters:
nobs = 579
nparameters = 6

npopulation = 10
per_second  = 72     # estimate of number of generations per second 
                     # WCOSS ~108, Xeon E5 3.7 GHz ~72 (macOS), ...
          
genmax = int(60*per_second)

train_start = int(0)
train_end   = int(364)
np.random.seed(0)    # for reproducibility

from evolution3 import *

######################## ######################## ########################
# Now bring in the data for real work:
matchup_set = []
  
with open('testin.csv') as csvfile:
    k = 0
    sreader = csv.reader(csvfile, delimiter=",")
    for line in sreader:
        day     = float(line[0])
        t2m_gfs = float(line[1])
        td_gfs  = float(line[2])
        thick_gfs = float(line[3])
        rh_gfs  = float(line[4])
        speed   = float(line[5])
        obs_t2m = float(line[6])
        obs_td  = float(line[7])
        terr    = float(line[8])
        tderr  = float(line[9])
        
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
bests      = []       # Save all then-best versions
for k in range (0,npopulation):
    population.append(critter(nparameters))

weights = np.zeros((nparameters))
sdevs   = np.zeros((nparameters))
bests.append(critter(nparameters))
bests[0].init(weights, sdevs, 99.)
nbests = 1

#for reference, take the raw gfs output's score:
population[0].init(weights, sdevs, 99.)
score_gfs = population[0].skill(matchup_set, train_start, train_end) 

print("uncorrected score in training period: ",
         population[0].skill(matchup_set, train_start, train_end) )
print("uncorrected score in evaluation period: ",
         population[0].skill(matchup_set, train_end+1, nobs), flush=True )
population[0].show_fcst(matchup_set, train_start, train_end)

population[0].weights[0] = -2.0
population[0].show_fcst(matchup_set, train_start, train_end)
population[0].weights[0] = 0.0

print("\n",flush=True)

#Initializing the standard deviations for evolution ----------
#For the bias
sdevs[0] = 1.0
#For linear terms
for k in range (1,int(6)):
  sdevs[k] = 1.0

#For quadratic terms
#for k in range (int(6), nparameters):
#  sdevs[k] = 0.0125

#Initialize the population itself now -------------------------
for k in range (0,npopulation):
  weights[0] = np.random.normal(0,sdevs[0])
  for l in range (1, int(6) ):     #initialize only the linear part
    weights[l] = np.random.normal(0,sdevs[l])
  population[k].init(weights,sdevs, 99.)

#recall that the matchup_set is holding the matchups
#Find our first 'best' -- noting that we aren't saving raw gfs as an example
smin = 9999.
kbest = int(npopulation)
for k in range (0,npopulation):
    population[k].skill(matchup_set, train_start, train_end)
    if (population[k].score < smin):
        kbest = k
        smin = population[k].score 

#Start accumulating our best critters
bests.append(critter(nparameters))
bests[nbests].init(population[kbest].weights, population[kbest].sdevs, population[kbest].score)
nbests += 1

population[kbest].show()
print("initial kbest, smin = ",kbest, smin, flush=True)


######################## ######################## ########################
#      Now carry out the (mutation-only) evolution
#swap best in to all slots
#then evolve a new raft of critters from that
#evaluate them
#repeat until limit of generations or happy

for gen in range(0,genmax):
    #print("generation ", gen, flush=True)

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
            bests.append(critter(nparameters))
            bests[nbests].init(population[kbest].weights, population[kbest].sdevs, population[kbest].score)
            nbests += 1
    if (kbest != 0):
        if (score_gfs != 0):
          print("new best ",gen, kbest, smin, score_best, smin/score_gfs, flush=True)
        else:
          print("new best ",gen, kbest, smin, score_best, flush=True)
        population[kbest].show()
        

######################## ######################## ########################
if (score_gfs != 0):
  print("best score in training period ",gen, kbest, smin, score_best, smin/score_gfs, flush=True)
else:
  print("best score in training period ",gen, kbest, smin, score_best, flush=True)
print("score in the untrained period: ",population[kbest].skill(matchup_set, train_end+1, nobs))

print("found ",nbests,"new bests along the way\n")
for k in range (0, nbests):
  bests[k].show()
  print("\n")


print("Forecasts in the training period:")
population[0].show_fcst(matchup_set, train_start, train_end)
print("Untrained forecasts:")
population[0].show_fcst(matchup_set, train_end, nobs)
