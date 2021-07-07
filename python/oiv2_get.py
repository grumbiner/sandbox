#!/usr/bin/python3

#Python3 -- important to add urllib.request, decode
import os
import re

#--------------------------------
import datetime
from datetime import date
from datetime import timedelta

dt = datetime.timedelta(1)
startdate = datetime.datetime(2021,5,1)
today = date.today()
enddate = today
enddate -= 12*dt

#---------------------------------------------------------
import urllib
import urllib.request
baseurl='https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/'
tdate = startdate
while (tdate.date() < enddate):
#while (tdate < datetime.datetime(1990,9,1) ):
  ym=tdate.strftime("%Y%m")
  ymd=tdate.strftime("%Y%m%d")
  #print(tdate, ym, ymd)
  #print(baseurl+ym+'/oisst-avhrr-v02r01.'+ymd+'.nc')

  oname = 'oisst-avhrr-v02r01.'+ymd+'.nc'
  if (not os.path.exists(oname)):
    print("getting ",oname)
    web = urllib.request.urlopen(baseurl+ym+'/oisst-avhrr-v02r01.'+ymd+'.nc')
    data = web.read()
    outfile = open('oisst-avhrr-v02r01.'+ymd+'.nc','w+b')
    outfile.write(data)
    outfile.close()
    web.close()
  else:
    print("have ",oname)

  tdate += dt

#---------------------------------------------------------
