import numpy as np
sc = np.zeros((10804))
days = np.zeros((10804))
fcstlen = int(35)
fcstlen = int(7)
scavg = np.zeros(int(10804/fcstlen))

import csv
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

import matplotlib.pyplot as plt

#fig,ax = plt.subplots()
#ax.set(title = 'sc', xlabel='days')
#ax.plot(days, sc)
#ax.grid()
#plt.show()

import datetime
dates = []
dt = datetime.timedelta(1)
start = datetime.date(1979,1,1)
#print(start)
#print('dt = ', dt)

for i in range (0,len(scavg)):
  scavg[i] = sc[i*fcstlen:i*fcstlen+fcstlen].mean()
  dates.append( start )
  tmp = datetime.timedelta(i*fcstlen)
  dates[i] += tmp
#  print(dates[i])

print("scavg max min: ",scavg.max(), scavg.min() )
fig,ax = plt.subplots()
ax.set(title = 'sc', xlabel='days')
ax.plot(dates, scavg)
ax.grid()
plt.show()

#exit()

from scipy import signal

autocorr = signal.fftconvolve(scavg, scavg[::-1], mode='full')
fig, ax_mag = plt.subplots()
#
n=int(len(autocorr)/2)
leads = len(autocorr) - n - 1
#leads=int(365*5.0/fcstlen)
#leads=int(9*30.4/fcstlen) # 9 month seasonal fcst
leads=int(35*2./fcstlen)   # subseasonal fcst
trim_acor = autocorr[n:n+leads+1]
amax = trim_acor.max() 
trim_acor /= amax
ax_mag.plot(np.arange(0,leads+1), trim_acor)
ax_mag.set_title('Autocorrelation')
ax_mag.grid()
plt.show()

exit()

#Spectrogram:
f,t,Sxx = signal.spectrogram(scavg, fs=1./float(fcstlen) )
plt.pcolormesh(t,f,Sxx)
plt.ylabel('Frequency (cpd)')
plt.xlabel('time (dy)')
plt.grid()
plt.show()

#for years: f,Pxx = signal.periodogram(scavg, fs=365./float(fcstlen) )
f,Pxx = signal.periodogram(scavg, fs=1./float(fcstlen) )
print("max periodogram ",f.max(), Pxx.max() )
plt.semilogy(f, Pxx)
plt.ylim([1.e-2,2.e3])
#plt.xlim([0,0.5/float(fcstlen) ])
#plt.xlim([0,1./100.])   #for days
plt.xlim([0,1./500.])   #for days
#plt.ylim([1.e-3,5])
#plt.xlim([0,0.25])   #for years
plt.grid()
plt.xlabel('frequency (cpy)')
plt.ylabel('pds W**2/cpy')
plt.show()
