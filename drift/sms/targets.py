#!/usr/bin/python

#Python3 -- important to add urllib.request, decode
import os
import re
import urllib
import urllib.request
import datetime
from datetime import date
from datetime import timedelta

#---------------------------------------------------------
if (os.path.exists('SIDFEx_targettable.txt')):
   fin = open('SIDFEx_targettable.txt','r')
else :
   outfile = open('SIDFEx_targettable.txt','w')
   web = urllib.request.urlopen('https://swift.dkrz.de/v1/dkrz_0262ea1f00e34439850f3f1d71817205/SIDFEx_index/SIDFEx_targettable.txt')
   data = web.read()
   outfile.write(data.decode('utf-8'))
   outfile.close()
   web.close()
   fin = open('SIDFEx_targettable.txt','r')

alpha = datetime.datetime(2020, 1, 1)
beta = alpha.now()
dt = timedelta(seconds=3600*(3+6)) #first 3 are for time window, 
                                   #the 6 are because GFS finishes 5 hours past nominal UTC cycle time
fout = open('seaice_edge.t00z.txt','w')

#20 = length of header
i = 0
x = 'blank'
while not (x == ''):
  x = fin.readline()
  if (i >= 20 and x != '' ):
    words = x.split()
    TargetID = words[0]
    current  = (words[1] == 'TRUE')
    latest_year = float(words[2])
    latest_day  = float(words[3])
    latest_lat  = float(words[4])
    latest_lon  = float(words[5])

    if (current):
      #do stuff
      tmp = datetime.datetime(int(latest_year), 1,1)
      tmp += timedelta(seconds=(latest_day-1)*86400)
      dt2 = (beta-tmp) 
      if ( dt2 >= dt):
        print("too old ",TargetID, tmp, dt2)
      else:
        print("dt =  ",TargetID, tmp, dt2)
        print(latest_lat, latest_lon, TargetID, latest_year, latest_day, file=fout)

  i += 1
