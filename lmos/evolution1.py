from math import *
import numpy as np

######################################################################
#The score can be any function of the errors. RMSE is used here for demonstration 
#   purposes, but in truth, it could be anything. 
# exponential(delta) is fine (and will give very strange results in the 
#   evolution -- try it)
#Interface to various scoring methods:
def score(obs, pred, delta, start, end, tolerance = 0):
    #return score_loss(obs, pred, delta, start, end, tolerance)
    return score_mae(delta, start, end, 3.0)

#RMS -- default score
def score_rms(delta, start, end, tolerance = 0):
    tmp = delta[start:end]
    tmp *= tmp
    return sqrt(sum(tmp)/(end-start))

#Mean
def score_mean(delta, start, end, tolerance = 0):
    tmp = delta[start:end]
    return abs(sum(tmp)/(end-start))

#Mean absolute error
def score_mae(delta, start, end, tolerance = 0):
    tmp = abs(delta[start:end])
    count = len(tmp)
    if (tolerance != 0.0):
      count = 0
      for k in range(0, end-start):
        if (tmp[k] < tolerance):
          tmp[k] = 0.0
        else:
          count += 1
    return (sum(tmp)/count)

#Mean3
def score_mean3(delta, start, end, tolerance = 0):
    tmp = delta[start:end]
    return pow(abs(sum(tmp*tmp*tmp)/(end-start)),1./3.)

#Mean4
def score_mean4(delta, start, end, tolerance = 0):
    tmp = delta[start:end]
    return pow(abs(sum(tmp*tmp*tmp*tmp)/(end-start)),1./4.)

#Number of losses -- fewer is better
def score_loss(obs, pred, delta, start, end, tolerance):
    count = 0
    for k in range (start, end):
       if (abs(delta[k]) > abs(obs[k]) ):
         count += 1
    return count
    

#make a prediction from variables in the matchup x, using constants in the list y
#n.b. would be desirable to have a general evaluator that takes the 
#    prediction function as an argument as well

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
    def show(self):
        for k in range(0,self.length):
            print(k,self.values[k])
    
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
    
    def init(self, weights, sdevs):
        for k in range(0, self.length):
          self.weights[k] = weights[k]
          self.sdevs[k]   = sdevs[k] 
            
    def copy(self, x):
        for k in range(0, self.length):
          self.weights[k] = x.weights[k]
          self.sdevs[k]   = x.sdevs[k]
        #self.show()

    #display element by element the weights and sdevs
    def show(self):
        n =  self.length
        for k in range(0,n):
        #for k in range(0,1):
            print(k,self.weights[k], self.sdevs[k])
            
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
    def skill(self, matchups, start, end):
        ndelta = (end-start+1)
        deltas = np.zeros(ndelta)
        pred   = np.zeros(ndelta)
        obs    = np.zeros(ndelta)
        for k in range (start, end):
           obs[k-start]  = matchups[k].values[6] 
           pred[k-start] = predict1(matchups[k],self.weights)
   
        deltas = obs-pred
        self.score = score(obs, pred, deltas, 0, ndelta)
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
