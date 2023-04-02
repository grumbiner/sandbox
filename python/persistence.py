#python
import os
import datetime

#Observations:
obs_base="/scratch2/NCEPDEV/climate/Robert.Grumbine/nsidc.nc/"
pole="north/"
ptag="n"

dt=datetime.timedelta(1)

for yy in range (2011,2017+1):
  for mm in range (1,12+1):
    for dd in 1,15:
      for lead in range (1,35+1):
        basetag   =  datetime.datetime(yy,mm,dd).strftime("%Y%m%d")
        validtag  =  (datetime.datetime(yy,mm,dd)+lead*dt).strftime("%Y%m%d")
        yvalid    =  (datetime.datetime(yy,mm,dd)+lead*dt).strftime("%Y")
        yfrom     = datetime.datetime(yy,mm,dd).strftime("%Y")
        print("./persistence "+obs_base+pole+yfrom+"/seaice_conc_daily_"+ptag+"h_f17_"+basetag+"_v03r01.nc "+ obs_base+pole+yvalid+"/seaice_conc_daily_"+ptag+"h_f17_"+validtag+"_v03r01.nc > score."+validtag+"f"+basetag+".csv")
             
    
