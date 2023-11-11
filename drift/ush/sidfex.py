#!/usr/bin/python
import os
import sys
import datetime
from math import *
import numpy as np

# Note, behaving as python2
#----------------------------------------
import const
from skiles import *

#----------------------------------------
if (os.path.exists('forecast.points')) :
  refpts = skiles2_points('forecast.points') 
else:
<<<<<<< HEAD
  print "Could not find forecast.points file"
=======
  print("Could not find forecast.points file")
>>>>>>> sidfex
  exit(1)

if (os.path.exists('fcstin')) :
  fin = open('fcstin','r')
else:
<<<<<<< HEAD
  print "could not find fcstin file"
  exit(1)

if (os.path.exists('/Volumes/Data') ):
  fcst_path = '/Volumes/Data/model_output/polar/'
  sidfex_base = '/Volumes/ncep/sidfex/out/'
  if ( not os.path.exists(sidfex_base) ):
    os.system('mkdir -p '+sidfex_base)
#else if
#fcst_path = '/u/Robert.Grumbine/onoscrub/final.drift.archive/fcsts/'
#sidfex_base = '/u/Robert.Grumbine/rgdev/sidfex/out/'
=======
  print("could not find fcstin file")
  exit(1)

individual = False
if (os.path.exists('/Volumes/Data') ):
  fcst_path = '/Volumes/Data/model_output/polar/'
  sidfex_base = '/Volumes/ncep/sidfex/out/'
elif (os.path.exists('/gpfs/dell2') ):
  fcst_path = '/u/Robert.Grumbine/noscrub/drift/ascii/fcsts/'
  sidfex_base = '/u/Robert.Grumbine/rgdev/sidfex/out/'
  #fcst_path = '/u/Robert.Grumbine/noscrub/com/mmab/developer/seaice_drift.'
else:
  print("no known system to work with!")
  exit(1)

if ( not os.path.exists(sidfex_base) ):
  x = os.system('mkdir -p '+sidfex_base)
  if (x != 0):
    print("failed to make output directory! ",sidfex_base)
    exit(2)
>>>>>>> sidfex

#----------------------------------------
GroupID="ncep001"
MethodID="freedrift-ensmean"
EnsMemNum=1

ll = latpt()
#----------------------------------------

x = 'blank'
while not (x == ''):

  x = fin.readline()
  if (x == ''):
    break

  words = x.split()
  TargetID=words[0]
  InitYear=words[1]
  hour   = words[2]
  minute = words[3]
  DOY = words[4]
  lat = words[5]
  lon = words[6]

  sidfex_name = sidfex_base+GroupID+'_'+MethodID+'_'+TargetID+'_'+InitYear+'-'+DOY+'_'+"{0:03d}".format(EnsMemNum)+'.txt'
  if ( not os.path.exists(sidfex_name) ):
    date8 = datetime.date(int(InitYear)-1,12,31) + datetime.timedelta(float(DOY))
    do_fcst = True
  else:
    print('already have forecast ',sidfex_name)
    sys.stdout.flush()
    do_fcst = False

#-----------------------------------------------------------
  #reading in a forecast file:
  day = int(float(DOY))
  date8 = datetime.date(int(InitYear)-1,12,31) + datetime.timedelta(float(DOY))
  fcst_name = 'sk2.'+date8.strftime("%Y%m%d")
  fcst_name = fcst_path + fcst_name
  if ( not os.path.exists(fcst_name)):
    fcst_path = '/u/Robert.Grumbine/noscrub/com/mmab/developer/seaice_drift.'
    fcst_name = fcst_path+date8.strftime("%Y%m%d")+'/global.'+date8.strftime("%Y%m%d")
    if ( not os.path.exists(fcst_name)):
      print("could not find "+fcst_name)
 
  if (do_fcst and os.path.exists(fcst_name) ):
    forecast = skiles2_forecast(fcst_name)
    n = forecast.nearest(lat, lon, 55.) #Find the forecast point closest to this point
    if (n < 0) :
      #print('no point close enough to given buoy')
      sys.stdout.flush()
      continue
    #else :
    #  forecast.fcst[n].show()
    f = open(sidfex_name,'w')
    #SIDFEx header:
    f.write( 'GroupID: '+str(GroupID)+'\n' )
    f.write( 'MethodID: '+str(MethodID)+'\n' )
    f.write( 'TargetID: '+str(TargetID)+'\n' )
    f.write( 'InitYear: '+str(InitYear)+'\n' )
    f.write( 'InitDayOfYear: '+str(DOY)+'\n' )
    f.write( 'InitLat: '+str(lat)+'\n' )
    f.write( 'InitLon: '+str(lon)+'\n' )
    f.write( 'EnsMemNum: '+"{0:03d}".format(EnsMemNum)+'\n' )
    f.write( '### end of header'+'\n')
    f.write( 'Year  DayOfYear  Lat      Lon'+'\n')
    llstart = latpt(lat, lon)
    for i in range (int(0), forecast.days):
      #ll          = latpt(forecast.fcst[n].lat, forecast.fcst[n].lon)
      bearing_dir = forecast.fcst[n+i*int(forecast.npoints)].wdir - 180.0
      #debug f.write(str(n)+' ' + str(llstart.lat)+' '+str(llstart.lon)+' '+
      #debug          str(bearing_dir)+' '+str(forecast.fcst[n+i*forecast.npoints].dist) )
      if (bearing_dir < 0 ):
        bearing_dir += 360.
      if (bearing_dir > 360. ):
        bearing_dir -= 360.
      ll = llstart.bearing_from(forecast.fcst[n+i*int(forecast.npoints)].dist, bearing_dir)
      f.write( str(InitYear)+'      '+str(day+i+1)+' '+str(ll.lat)+' '+str(ll.lon)+'\n' )
  else:
    print('cannot or do not need to ',do_fcst, fcst_name)
    sys.stdout.flush()

