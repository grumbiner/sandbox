#import sys
#import os
#import datetime
import csv

from math import *
import numpy as np

#--------------------------------------
sc   = np.zeros((10804))
days = np.zeros((10804))

with open('solar.csv') as csvfile:
  sreader = csv.reader(csvfile, delimiter=",")
  k = 0
  for line in sreader:
    days[k] = float(k)
    sc[k] = float(line[1])
    k+= 1
print("Mean Sc = ",sc.mean() )
sc -= sc.mean()
print(sc.max(), sc.min(), sc.std() )

#--------------------------------------
#import scipy
#import itertools
#import pandas as pd

from scipy.special import factorial
import statsmodels.api as sm
#exit()

results = sm.OLS(sc, days).fit()
print(results.summary() )
print(sm.tsa.stattools.acf(sc, nlags=365))

