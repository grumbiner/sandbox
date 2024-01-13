import numpy as np
import netCDF4

sun = netCDF4.Dataset("ssi.nc", 'r')
ntime = len(sun.dimensions['time'])
nwavelength = len(sun.dimensions['wavelength'])

#Watts/meter^2
tsi = sun.variables['TSI'][:]
print(tsi.max(), tsi.min(), len(tsi) )

# nm
wavelength = sun.variables['wavelength'][:]
bandwidth  = sun.variables['Wavelength_Band_Width'][:]
print('wavelength: ',wavelength.max(), wavelength.min(), len(wavelength)  )
print('bandwidth:  ',bandwidth.max(), bandwidth.min(), len(bandwidth)    )

import matplotlib 
import matplotlib.pyplot as plt

#fig,ax = plt.subplots()
#ax.set(xlabel = 'Wavelengths (nm)', ylabel = 'bandwidth (nm)' )
#plt.xlim(0,20000.)
#ax.plot(wavelength, bandwidth)
#ax.grid()
#fig.show()
#plt.savefig('a.png')
#plt.close()
#exit(0)


#watts per meter squared per nm
ssi = sun.variables['SSI'][:,:] 

#sum = 0.0
#for i in range (0,nwavelength):
#  #print(i,wavelength[i],ssi[:,i].max()*bandwidth[i], ssi[:,i].min()*bandwidth[i] )
#  print(i,wavelength[i],ssi[:,i].max(), ssi[:,i].min(), ssi[:,i].std() )
#  sum += ssi[:,i].max()*bandwidth[i]
#print(sum)

#Integrate over GFS bands
#wavenumbers cm^-1
#wvnum1 = (2600.0, 3251.0, 4001.0, 4651.0, 5151.0, 6151.0, 7701.0,  8051.0,12851.0,16001.0,22651.0,29001.0,38001.0,  820.0) 
#wvnum2 = (3250.0, 4000.0, 4650.0, 5150.0, 6150.0, 7700.0, 8050.0, 12850.0,16000.0,22650.0,29000.0,38000.0,50000.0, 2600.0)
#print(len(wvnum1), len(wvnum2))

#Edit to close up the gaps:                                                              **Important**
wvnum1 = (2600.0, 3250.1, 4000.1, 4650.1, 5150.1, 6150.1, 7700.1,  8050.1,12850.1,16000.1,22650.01,29000.1,38000.1,  820.0) 
wvnum2 = (3250.0, 4000.0, 4650.0, 5150.0, 6150.0, 7700.0, 8050.0, 12850.0,16000.0,22650.0,29000.0, 38000.0,50000.0, 2600.0)

#convert to nm bands:
bandl = np.zeros((len(wvnum1)))
bandu = np.zeros((len(wvnum2)))
center = np.zeros((len(wvnum2)))

for i in range (0, len(wvnum1)):
  bandu[i] = 1.e-2/wvnum1[i]*1.e9 #upper = longer wavelength
  bandl[i] = 1.e-2/wvnum2[i]*1.e9
  center[i] = (bandu[i]+bandl[i])/2.

shortest = bandl.min()
longest = bandu.max()
sum_band = np.zeros((len(wvnum2)))
dtband = np.zeros((ntime, len(wvnum2)))
time   = np.zeros((ntime))

for date in range (0, ntime):
  sum_tooshort = 0.0
  sum_toolong = 0.0
  sum_gap     = 0.0
  sum_band = np.zeros((len(wvnum2)))
  for i in range (0, nwavelength):
    energy = ssi[date,i]*bandwidth[i]
  
    if (wavelength[i] > longest):
      sum_toolong += energy 
    elif (wavelength[i] < shortest):
      sum_tooshort += energy
    else:
      found = False
      for k in range (0,len(bandl)):
        if (wavelength[i] >= bandl[k] and wavelength[i] <= bandu[k]): 
          sum_band[k] += energy
          found = True
      if (not found):
        sum_gap += energy
        print('date, gap ',date, i,wavelength[i], 1.e-2/(wavelength[i]*1.e-9), energy)
  
  print('date short, long, gap ',date, sum_tooshort, sum_toolong, sum_gap)
  time[date] = date
  for k in range (0,len(bandl)):
    print(date, k, (bandl[k]+ bandu[k])/2., sum_band[k])
    dtband[date,k] = sum_band[k]


import matplotlib 
import matplotlib.pyplot as plt

fig,ax = plt.subplots()
ax.set(xlabel = 'Band Centers (nm)', ylabel = 'energy')
#plt.xlim(0,20000.)
ax.plot(center, dtband[0,:] )
ax.grid()
fig.show()
plt.savefig('a.png')
plt.close()
