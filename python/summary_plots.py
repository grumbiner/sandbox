import sys
import os
import datetime
from math import *

import csv
import numpy as np
import matplotlib
import matplotlib.pyplot as plt

lead = int(sys.argv[1])
idate = int(sys.argv[2])
level_score = float(sys.argv[3]) #might be good to verify that this is integer multiple of 0.05


(yy,mm,dd) = (int(int(idate)/10000), int(int(idate)%10000)/100, int(idate)%100)
start_date = datetime.date(int(yy), int(mm), int(dd))
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
score = np.zeros((lead))

#For score vs. crit level for each lead
critical_level = np.zeros((20))
threat_index = np.zeros((20))

#for yy in (2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018):
for yy in ( 2012, 2013 ):
 for mm in ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12):
   for dd in ( 1, 15 ):
      start_date = datetime.date(int(yy), int(mm), int(dd))
      print("start date = ",start_date.strftime("%Y%m%d") )
      start_int = int(start_date.strftime("%Y%m%d"))
  
      valid_date = start_date
      for i in range (0,lead):
        valid_date = valid_date + dt 
        fname = ("out."+start_date.strftime("%Y%m%d")+"/score.n."+valid_date.strftime("%Y%m%d")+"f"+start_date.strftime("%Y%m%d")+".csv")
        #print(fname)
        flead = (valid_date - start_date).days
        if (not os.path.exists(fname)):
          print("missing ",valid_date.strftime("%Y%m%d"))
        else:    #at each forecast lead, plot the curve of threat vs. cutoff
          #print("can plot forcast lead ",flead," for ",valid_date.strftime("%Y%m%d"))
          with (open(fname)) as csvfile:
            sreader = csv.reader(csvfile, delimiter=',')
            k = -1
            days[i] = flead
            for line in sreader:
              k += 1
              #print("k = ",k,line)
              if (k < 20): continue #first 20 are for global stats
              critical_level[k-20] = float(line[level])
              threat_index[k-20] = float(line[threat])
              #print(critical_level[k-20], threat_index[k-20])
              if (float(critical_level[k-20]) == level_score):
                score[i] = threat_index[k-20]
          #Now have this lead in hand, plot the curve:
          fig, ax = plt.subplots()
          ax.set(xlabel = "Cutoff Concentration", ylabel = 'threat score [0:1]')
          ax.set(title = 'Forecast lead '+str(flead)+' NH threat score')
          ax.plot(critical_level, threat_index)
          ax.grid()
          #fig.show()
          plt.savefig("threat_"+str(flead)+"_dy_"+"from_"+start_date.strftime("%Y%m%d")+".png")
          plt.close()

      #done with day by day, now plot summary vs. cutoff:
      fig,ax = plt.subplots()
      ax.set(xlabel = "Forecast lead, days", ylabel = 'threat score [0:1]')
      ax.set(title = "Threat score for critical = "+str(level_score)+" from "+start_date.strftime("%Y%m%d"))
      ax.plot(days,score)
      ax.grid()
      #fig.show()
      plt.savefig("threat_"+str(level_score)+"_f"+start_date.strftime("%Y%m%d")+".png")
      plt.close()

#around here, construct the summary graphics
