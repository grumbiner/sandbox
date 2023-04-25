import sys
import os

import datetime
import csv

from math import *
import numpy as np

import matplotlib
import matplotlib.pyplot as plt

#dirbase='bm3.verf/out.'
dirbase = sys.argv[1]
figtitle = sys.argv[2]
lead = 35
dt = datetime.timedelta(1)

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

for mm in range (1,13):
  for dd in ('01', '15'):
    counts = np.zeros((20,lead))
    sums   = np.zeros((20,lead))
    sumsq  = np.zeros((20,lead))
    for yy in range (2011,2019):
      tag = "{0:4d}".format(yy) + "{0:02d}".format(mm) + dd
      start_date = datetime.date(int(yy), int(mm), int(dd))
      dirname = dirbase 
      if (os.path.exists(dirname)):
        for flead in range (int(0),int(lead)):
          valid_date = start_date + (flead+1)*dt
          fname = dirname + "/score."+valid_date.strftime("%Y%m%d") + "f" + start_date.strftime("%Y%m%d") + ".csv" 
          #print(str(flead)+" "+ fname)
          if (os.path.exists(fname)):
            with (open(fname)) as csvfile:
              sreader = csv.reader(csvfile, delimiter = ',')
              k = -1
              for line in sreader:
                k += 1
                counts[int(k), flead] += 1
                sums[int(k), flead]   += float(line[threat])
                sumsq[int(k), flead]  += float(line[threat])*float(line[threat])
          #except: (nothing, move on to next)

      else:
        print("no directory ",dirname)

    print("done_"+tag,counts.max() )
    sums  /= counts
    sumsq /= counts
    days = np.zeros((lead))
    mean = np.zeros((lead))
    rmse = np.zeros((lead))
    var = np.zeros((lead))
    with (open('summary_'+tag+'.csv', 'w', newline='') ) as csvfile:
      fieldnames = ['lead', 'mean', 'rms', 'var'];
      writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
      writer.writeheader()

      for i in range (0,lead):
        days[i] = i+1
        mean[i] = sums[3,i]
        rmse[i] = sqrt(sumsq[3,i])
        var[i]  = sqrt(sumsq[3,i]-sums[3,i]*sums[3,i])
        print("0.15 ","{0:5.3f}".format(sums[3,i]), " ","{0:5.3f}".format(sqrt(sumsq[3,i])), " ", "{0:6.4f}".format(sqrt(sumsq[3,i]-sums[3,i]*sums[3,i]))  )
        writer.writerow({'lead': days[i], 'mean': mean[i], 'rms': rmse[i], 'var':var[i]})


    #Now ready to plot:
    fig,ax = plt.subplots()
    ax.set(xlabel = "Forecast lead, days", ylabel = "threat score [0:1]")
    ax.set(title = figtitle +" Summary for "+tag+" critical level = 0.15 ")
    plt.ylim(0.5,1.0)
    ax.plot(days, mean, color="blue", label = "mean")
    #ax.plot(days, rmse, color="green", label = "rms")
    ax.legend()
    ax.grid()
    plt.savefig("summary_"+tag+".png")
    plt.close()

    fig,ax = plt.subplots()
    ax.set(xlabel = "Forecast lead, days", ylabel = "sqrt(variance) [0:1]")
    ax.set(title = figtitle + " Summary for "+tag+" critical level = 0.15 ")
    plt.ylim(0,0.075)
    ax.plot(days, var, color="blue", label = "sqrt(variance)")
    ax.legend()
    ax.grid()
    plt.savefig("summary_var_"+tag+".png")
    plt.close() 
