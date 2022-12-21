import sys
from math import *
import numpy as np


######################################################################
from scores import *
######################################################################

#First prediction method:
def predict1(x,y):
    nx = x.length

    #let's ignore matchup[0] (day of observation) and matchup[6], the error term, 
    #  in making prediction
    #Starting point: simple multi-linear regression, a bias term plus 
    #  weights(y[k]) times the predictors
    pred = y[0]
    for k in range(1,nx-1):
        pred += x.values[k]*y[k]
    return (pred)


######################## ######################## ########################
class matchup:
    length = int(0)

    #values is a set (tuple or np.ndarray) of parameter values in the matchup
    def __init__(self, values): 
        self.values = values
        self.length = len(values)
    
    #display element by element the values of the matchup
    def show(self, fout = sys.stdout):
        for k in range(0,self.length):
            print(k,self.values[k], file = fout)
        print(self.score, file = fout, flush = True)
    
    #extract the k-th parameter from the values tuple
    def __getitem__(self,k):
        return(self.values[k])
          
######################## ######################## ########################
class critter:
    score = float(99.0)
    length = int(1)
    
    def __init__(self, nparm): 
        self.weights = np.zeros((nparm))
        self.sdevs = np.zeros((nparm))
        self.length = nparm
    
    def init(self, weights, sdevs, score ):
        self.score = score
        for k in range(0, self.length):
          self.weights[k] = weights[k]
          self.sdevs[k]   = sdevs[k] 
            
    def copy(self, x):
        self.score = x.score
        for k in range(0, self.length):
          self.weights[k] = x.weights[k]
          self.sdevs[k]   = x.sdevs[k]

    #display element by element the weights and sdevs
    def show(self, fout = sys.stdout):
        n =  self.length
        print("k (value, sd)", file = fout)
        for k in range(0,n):
            #print("{:.3f}".format(self.weights[k]), " sd: " 
            #      "{:.3f}".format(self.sdevs[k]),   " ", file = fout, end='')
            print(k, "(", "{:.3f}".format(self.weights[k]), "{:.3f}".format(self.sdevs[k]), ")",   file = fout)
        print("score ","{:.3f}".format(self.score), " ", file = fout, end='\n')
        print(flush = True, file = fout)
            
    #Function to evolve the next generations -- mutation only in this one
    def evolve(self):
        #self.sdevs[0]   = np.random.lognormal(0, self.sdevs[0])
        for k in range (0, int(6) ):
            self.sdevs[k]   = np.random.lognormal(0, self.sdevs[k])
        for k in range (0, self.length):
            self.weights[k] = np.random.normal(self.weights[k],self.sdevs[k])
    
    #take a set of matchups and evaluate (ultimately, to score) the predictions 
    #  from predict1 note that we're now applying a start and end time 
    #  -- the training period
    def skill(self, matchups, start, end, metric = 0, tolerance = 0.):
        #print("hello from skill, metric, tolerance = ",metric, tolerance, flush=True)
        ndelta = (end-start+1)
        deltas = np.zeros(ndelta)
        pred   = np.zeros(ndelta)
        obs    = np.zeros(ndelta)
        for k in range (start, end):
           obs[k-start]  = matchups[k].values[6] 
           pred[k-start] = predict1(matchups[k],self.weights)
   
        deltas = obs-pred
        self.score = score(obs, pred, deltas, 0, ndelta, metric, tolerance)
        return(self.score)

    #Show the parameters and the prediction:
    def show_fcst(self, matchups, start, end):
        ndelta = (end-start)
        pred   = np.zeros(ndelta)
        deltas = np.zeros(ndelta)
        for k in range (start, end):
           pred[k-start] = predict1(matchups[k],self.weights)
           deltas[k-start] = matchups[k].values[6] - pred[k-start]

        for k in range (0, ndelta):
           print(matchups[k+start].values[0], 
                 matchups[k+start].values[1],
                 matchups[k+start].values[2],
                 matchups[k+start].values[3],
                 matchups[k+start].values[4],
                 matchups[k+start].values[5],
                 matchups[k+start].values[6], #Observed
                 " zzz ",pred[k], deltas[k], 
                         abs(deltas[k]) < abs(matchups[k].values[6])    )
        print("mean rms ",deltas.mean(), sqrt(sum(deltas*deltas)/ndelta) )

######################## ######################## ########################
