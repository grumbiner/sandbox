import numpy as np
sc = np.zeros((10804))
days = np.zeros((10804))

import csv
with open('solar.csv') as csvfile:
  sreader = csv.reader(csvfile, delimiter=",")
  k = 0
  for line in sreader:
    days[k] = float(k)
    sc[k] = float(line[1])
    k+= 1

#print(sc[525])
#print(len(sc), k)
print("Mean Sc = ",sc.mean() )
sc -= sc.mean()
#print("average = ",sc.mean() )
print(sc.max(), sc.min() )

#import matplotlib
import matplotlib.pyplot as plt

fig,ax = plt.subplots()
ax.set(title = 'sc', xlabel='days')
ax.plot(days, sc)
ax.grid()
plt.show()

from scipy import signal

autocorr = signal.fftconvolve(sc, sc[::-1], mode='full')
#fig, (ax_orig, ax_mag) = plt.subplots(2,1)
fig, ax_mag = plt.subplots()
#ax_orig.plot(days, sc)
#ax_orig.grid()

#ax_mag.plot(np.arange(-len(sc)+1,len(sc)), autocorr)
n=int(len(autocorr)/2)
#trim_acor = autocorr[n:len(autocorr)]
#ax_mag.plot(np.arange(0,len(autocorr)-n), trim_acor)

leads=2000
leads=56
trim_acor = autocorr[n:n+leads+1]
amax = trim_acor.max() 
trim_acor /= amax
ax_mag.plot(np.arange(0,leads+1), trim_acor)

ax_mag.set_title('Autocorrelation')
ax_mag.grid()
#fig.tight_layout()
plt.show()

exit()

#f,t,Sxx = signal.spectrogram(sc)
#plt.pcolormesh(t,f,Sxx)
#plt.ylabel('Frequency (cpd)')
#plt.ylim([0.,0.2])
#plt.xlabel('time (dy)')
#plt.grid()
#plt.show()

f,Pxx = signal.periodogram(sc)
plt.semilogy(f, Pxx)
plt.ylim([1.e-3,1e2])
plt.xlim([0,0.4])
plt.grid()
plt.xlabel('frequency (cpd)')
plt.ylabel('pds W**2/cpd')
plt.show()





exit(1)
from spectrum import *
#from spectrum import TimeSeries, Periodogram, data_cosine
#f,t,Sxx = signal.spectrogram(sc)

ts = TimeSeries(sc, sampling=1)
print("sc max min: ",sc.max(), sc.min() )
ts.plot()

p = Periodogram(sc, sampling=1)
p.data
p.sampling
print(p)
p.plot(marker='o')
#p.plot(marker='o', filename="p.png")
