import sys
import os
import datetime
from math import *

import csv
import numpy as np
import matplotlib
import matplotlib.pyplot as plt

lead = int(sys.argv[1])
level_score = float(sys.argv[2]) #might be good to verify that this is integer multiple of 0.05
idate1 = int(sys.argv[3])
idate2 = int(sys.argv[4])
title1 = sys.argv[5]
title2 = sys.argv[6]
base1 = sys.argv[7]
base2 = sys.argv[8]   # base is directory path down to score files 


(yy,mm,dd) = (int(int(idate1)/10000), int(int(idate1)%10000)/100, int(idate1)%100)
start_date1 = datetime.date(int(yy), int(mm), int(dd))
(yy,mm,dd) = (int(int(idate2)/10000), int(int(idate2)%10000)/100, int(idate2)%100)
start_date2 = datetime.date(int(yy), int(mm), int(dd))
dt = datetime.timedelta(1)

#critical levels ...? all levels? hemisphere selection?
#define the indices to the contingency table .csv lines:
region = 0
level = 1
a11 = 2
a12 = 3
a21 = 4
a22 = 5
pod = 6
far = 7
fcr = 8
correct = 9
threat = 10
bias = 11 #area over crit in model vs. in obs

#For score vs. forecast lead through all leads, for a given critical level:
days = np.zeros((lead))
score1 = np.zeros((lead))
score2 = np.zeros((lead))

#For score vs. crit level for each lead
critical_level1 = np.zeros((20))
threat_index1   = np.zeros((20))
critical_level2 = np.zeros((20))
threat_index2   = np.zeros((20))

valid_date1 = start_date1
valid_date2 = start_date2
#fig, ax = plt.subplots()
#ax.set(xlabel = "Cutoff Concentration", ylabel = 'threat score [0:1]')
#ax.set(title = title2+"_v_"+title1+' NH threat score '+start_date2.strftime("%Y%m%d"))

for i in range (0,lead):
  valid_date1 = valid_date1 + dt 
  valid_date2 = valid_date2 + dt 
  flead = (valid_date1 - start_date1).days

#Should be a function rather than duplicate code RG
  name1 = base1+"/score.n."+valid_date1.strftime("%Y%m%d")+"f"+start_date1.strftime("%Y%m%d")+".csv"
  if (not os.path.exists(name1)):
    print("missing ",name1)
  else:    #at each forecast lead, plot the curve of threat vs. cutoff
    with (open(name1)) as csvfile:
      sreader = csv.reader(csvfile, delimiter=',')
      k = -1
      days[i] = flead
      for line in sreader:
        k += 1
        if (k < 20): continue #first 20 are for global stats
        critical_level1[k-20] = float(line[level])
        threat_index1[k-20] = float(line[threat])
        if (float(critical_level1[k-20]) == level_score):
          score1[i] = threat_index1[k-20]

  name2 = base2+"/score.n."+valid_date2.strftime("%Y%m%d")+"f"+start_date2.strftime("%Y%m%d")+".csv"
  if (not os.path.exists(name2)):
    print("missing ",name2)
  else:    #at each forecast lead, plot the curve of threat vs. cutoff
    with (open(name2)) as csvfile:
      sreader = csv.reader(csvfile, delimiter=',')
      k = -1
      days[i] = flead
      for line in sreader:
        k += 1
        #print("k = ",k,line)
        if (k < 20): continue #first 20 are for global stats
        critical_level2[k-20] = float(line[level])
        threat_index2[k-20] = float(line[threat])
        if (float(critical_level2[k-20]) == level_score):
          score2[i] = threat_index2[k-20]

  #Now have this lead in hand, plot the curve:
  fig, ax = plt.subplots()
  ax.set(xlabel = "Cutoff Concentration", ylabel = 'threat score [0:1]')
  ax.set(title = title2+"_v_"+title1+' Forecast lead '+str(flead)+' NH threat score '+start_date2.strftime("%Y%m%d"))
  ax.plot(critical_level1, threat_index1, color="blue", label="bm2")
  ax.plot(critical_level2, threat_index2, color="green", label="bm3")
  ax.legend()
  ax.grid()
  plt.savefig("threat_"+str(flead)+"_dy_"+title2+"_"+title1+"_from_"+start_date2.strftime("%Y%m%d")+".png")
  plt.close()

#done with day by day, now plot summary vs. cutoff:
fig,ax = plt.subplots()
ax.set(xlabel = "Forecast lead, days", ylabel = 'threat score [0:1]')
ax.set(title = title1 + " vs " + title2 +"Threat score for critical = "+str(level_score)+" from "+start_date2.strftime("%Y%m%d"))
ax.plot(days,score1, color="blue", label="bm2")
#print(score1)
ax.plot(days,score2, color="green", label="bm3")
#print(score2)
ax.legend()
ax.grid()
plt.savefig("threat_"+str(level_score)+"_"+title1+"_v_"+title2+"_f"+start_date2.strftime("%Y%m%d")+".png")
plt.close()
