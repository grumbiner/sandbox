from math import *
import numpy as np

######################################################################
#The score can be any function of the errors. RMSE is used here for default 
#   but in truth, it could be anything. 
#It is desirable, however, that scores be consistent as to whether larger is better or not
#Robert Grumbine
#4 October 2020

NMETRIC = int(7)

RMS   = int(0)
MEAN  = int(1)
MAE   = int(2)
RM3   = int(3)
RM4   = int(4)
NLOSS = int(5)
VICKIE = int(6)

def scoreall(obs, pred, delta, start, end, tolerance):
    scores = np.zeros(NMETRIC)
    for k in range (1,NMETRIC+1):
       scores[k-1] = score(obs, pred, delta, start, end, k, tolerance)
    return scores

#Interface to various scoring methods:
def score(obs, pred, delta, start, end, metric = 0, tolerance = 0):
    if (metric == RMS):
      return score_rms(delta, start, end, tolerance)
    elif (metric == MEAN):
      return score_mean(delta, start, end, tolerance)
    elif (metric == MAE):
      return score_mae(delta, start, end, tolerance) 
    elif (metric == RM3):
      return score_mean3(delta, start, end, tolerance)
    elif (metric == RM4):
      return score_mean4(delta, start, end, tolerance)
    elif (metric == NLOSS):
      print("metric nloss not currently working, continuing with RMS", flush=True)
      return score_rms(delta, start, end)
      #return score_loss(delta, start, end, tolerance)
    elif (metric == VICKIE):
      return score_mae(delta, start, end, tolerance = 3.0)
    else:
      print("unknown metric ",metric, " continuing with RMS", flush=True)
      return score_rms(delta, start, end)

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
