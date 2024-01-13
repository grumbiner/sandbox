import numpy as np
import csv

year = np.zeros((142))
temp = np.zeros((142))
tdot = np.zeros((142))

with open('1880-2021.csv') as csvfile:
  sreader = csv.reader(csvfile, delimiter=',')
  k = 0
  for line in sreader:
    year[k] = k
    temp[k] = line[1]
    #print(year[k], ' ',temp[k])
    k += 1

tdot[141] = 0
for k in range (0,141):
  tdot[k] = temp[k+1] - temp[k]
  print(k,temp[k], tdot[k])

import matplotlib
import matplotlib.pyplot as plt
fig, ax = plt.subplots()
#ax.set(xlabel='Lead time, days', ylabel = 'edge rms (km)')
#ax.set(title='Forecast vs observation edge location rms (km)')
#ax.plot(year,temp)

ax.scatter(temp, tdot)

ax.grid()
#fig.show()

plt.savefig("tdot.png")
print(tdot.mean()*10," degrees/decade " )
