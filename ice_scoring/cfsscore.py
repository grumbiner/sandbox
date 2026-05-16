#python
import os
import datetime

#Forecasts:
base="/scratch3/NCEPDEV/stmp2/Denise.Worthen/BenchIce/gfs."
base="/scratch3/NCEPDEV/stmp2/Denise.Worthen/BenchCFSv2/cfs."

#Observations:
obs_base="/scratch4/NCEPDEV/ocean/save/Denise.Worthen/IceData/"
pole="south/"
ptag="s"

dt=datetime.timedelta(1)

#.20130815
#ice2012020112.01.2012020100.subset.nc
        #print(datetime(yy,mm,dd),datetime(yy,mm,dd)+lead*dt)
        #print(datetime.datetime(yy,mm,dd).strftime("%Y%m%d") ,  dt,lead,
        #(datetime.datetime(yy,mm,dd)+lead*dt).strftime("%Y%m%d") )
for yy in range (2012,2017+1):
  for mm in range (1,12+1):
    for dd in 1,15:
# cfsv2 has all time levels in the data file, just need to list out verification files (all on one line)
      basetag   =  datetime.datetime(yy,mm,dd).strftime("%Y%m%d")
      dirname   =  base+basetag
      filename  =  dirname+'/'+'cfsv2.'+basetag+"00.cice5grid.nc"
#cfsv2.2012010100.cice5grid.nc
      try:
        fcst = open(filename,"r")
      except :
        print("failed to open forecast ",filename)
        continue
      print("./score_cfsv2 "+filename+" \\")
      for lead in range (0,35):
        validtag  =  (datetime.datetime(yy,mm,dd)+lead*dt).strftime("%Y%m%d")
        yvalid    =  (datetime.datetime(yy,mm,dd)+lead*dt).strftime("%Y")
        valid_fname = obs_base+pole+yvalid+"/seaice_conc_daily_"+ptag+"h_f17_"+validtag+"_v03r01.nc"
        try:
          obs = open(valid_fname,"r")
        except :
          print("failed to open validation ",valid_fname)
          continue
        print(valid_fname+" \\") 
   
      print(" > cfsv2_score.f"+basetag) 
