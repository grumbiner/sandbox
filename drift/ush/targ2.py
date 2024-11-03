#!/usr/bin/python

#Python3 -- important to add urllib.request, decode
import os
import sys
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

dawn = datetime.datetime(int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4]) )
dt = timedelta(seconds=3600*(3+6)) #first 3 are for time window, 
                                   #the 6 are because GFS finishes 5 hours 
                                   #   past nominal UTC cycle time
fout = open('seaice_edge.t00z.txt'+sys.argv[1]+sys.argv[2]+sys.argv[3]+sys.argv[4],'w')

#20 = length of header
header_len = 20
i = 0
x = 'blank'
while not (x == ''):
  x = fin.readline()
  if (i >= header_len and x != '' ):
    words = x.split()
    TargetID = words[0]
    current  = (words[1] == 'TRUE')
    latest_year = float(words[2])
    latest_day  = float(words[3])
    latest_lat  = float(words[4])
    latest_lon  = float(words[5])

    if (current):
      if (not TargetID == 'POLARSTERN01' and not TargetID == 'POLARSTERN02'  ):
        #do stuff
        tmp = datetime.datetime(int(latest_year), 1,1)
        tmp += timedelta(seconds=(latest_day-1)*86400)
        dt2 = (dawn-tmp) 
        if ( dt2 >= dt):
          print("too old ",TargetID, tmp, dt2)
        else:
          print("dt =  ",TargetID, tmp, dt2)
          print(latest_lat, latest_lon, TargetID, latest_year, latest_day, file=fout)

      else: #polarstern
        outfile = open(TargetID+'.txt','w')
        web = urllib.request.urlopen('https://swift.dkrz.de/v1/dkrz_0262ea1f00e34439850f3f1d71817205/SIDFEx_index/observations/'+TargetID+'.txt')
        data = web.read()
        outfile.write(data.decode('utf-8'))
        outfile.close()
        web.close()
        fintarget = open(TargetID+'.txt','r')

        #now read forward in time through file
        y = fintarget.readline() #header

        #go through observations and print out all that are more recent than the dawn of time
        while not (y == ''):
          y = fintarget.readline()
          if (y == ''):
             fintarget.close()
             break
          words = y.split()
          obs_year   = float(words[0])
          obs_day    = float(words[1])
          obs_date  = datetime.datetime(int(obs_year),1,1)
          obs_date += timedelta(seconds=(obs_day-1)*86400)
          obs_lat = float(words[2])
          obs_lon = float(words[3])
##normal scan
          if ((obs_date >= dawn) ):
            fout = open('seaice_edge.t00z.txt.'+obs_date.strftime("%Y%m%d%H"),'a')
            print(obs_lat, obs_lon, TargetID, obs_year, obs_day, file=fout)
            fout.close()


  i += 1
