import os
import sys
import datetime

#Arguments:
#   start_date verification_date forecast_dir_path
#   or
#   start_date number_days_forward time_delta_days forecast_dir_path

from verf_files import *

exdir = "./exec/"
#data files:
#  seaice_alldist.bin
#  seaice_gland5min
#execs:
# cscore_edge
# find_edge_nsidc
# find_edge_ncep
# find_edge_ims
# find_edge_cfsv2
# solo_ncep

##################### ------------- 
#------------------------------------------------------------------
def get_obs(initial_date, valid_date, imsverf, ncepverf, nsidcverf, 
             imsdir, ncepdir, nsidcdir):
  retcode = int(0)
  initial    = int(initial_date.strftime("%Y%m%d"))
  valid      = int(valid_date.strftime("%Y%m%d"))
  moninitial = int(initial_date.strftime("%Y%m"))
  monvalid   = int(valid_date.strftime("%Y%m"))
  yearinitial = int(initial_date.strftime("%Y"))
  yearvalid   = int(valid_date.strftime("%Y"))
  return retcode

def solo_score(fcst, fdate):
  if (fcst == "nsidc"): return 0
  fname = fcst+"."+fdate.strftime("%Y%m%d")
  if (os.path.exists(fname)):
    cmd = (exdir + "solo_" +fcst+" fix/seaice_gland5min "+fname)
    print("integrals for ",fcst)
    sys.stdout.flush()
    x = os.system(cmd)
    if (x != 0):
      print("command ",cmd," returned error code ",x)
    return x 
  else:
    print("could not find ",fname)
    return 1

def edge_score(fcst, fdate, obs, obsdate):
  retcode = int(0)
  fname   = fcst+"_edge."+fdate.strftime("%Y%m%d")
  obsname = obs +"_edge."+obsdate.strftime("%Y%m%d")
  outfile = ("edge." + fcst + "." + obs + "." +fdate.strftime("%Y%m%d") 
                + "."+obsdate.strftime("%Y%m%d") )
  print('edge_score ',fname,' ',obsname,' ',outfile)
  if (os.path.exists(fname) and os.path.exists(obsname) and not 
      os.path.exists(outfile) ):
    cmd = (exdir + "cscore_edge fix/seaice_alldist.bin "+fname+" "+obsname +
           " 50.0 > " + outfile )
    x = os.system(cmd)
    if (x != 0):
      print("command ",cmd," returned error code ",x)
      sys.stdout.flush()
      retcode += x
#  else:
#    print('something missing '+fname+' '+obsname+' '+outfile)

  return retcode

def score_nsidc(fcst_dir, nsidcdir, fdate, obsdate):
  retcode = int(0)
  vyear = int(obsdate.strftime("%Y"))

  #valid_fname = fcst_dir+'ice'+obsdate.strftime("%Y%m%d")+'00.01.'+fdate.strftime("%Y%m%d")+'00.nc'
  valid_fname = fcst_dir+'ice'+obsdate.strftime("%Y%m%d")+'00.01.'+fdate.strftime("%Y%m%d")+'00.subset.nc'

  if (os.path.exists(exdir + 'score_nsidc')):
    print("Have the fcst vs. nsidc scoring executable")
    sys.stdout.flush()
    pole="north/"
    ptag="n"
    obsname = (nsidcdir + pole + str(vyear) + "/seaice_conc_daily_"+ptag+"h_f17_"+
                        obsdate.strftime("%Y%m%d")+"_v03r01.nc" )
    cmd = (exdir+"score_nsidc "+valid_fname+" "+obsname+ " fix/skip_hr" + " > score."+
                ptag+"."+obsdate.strftime("%Y%m%d")+"f"+fdate.strftime("%Y%m%d")+".csv")
    x = os.system(cmd)
    if (x != 0):
      print("command ",cmd," returned error code ",x)
      retcode += x

  else:
    print("No score_nsidc executable")
    sys.stdout.flush()
    retcode += 1

  return retcode

def score_cfsv2(fcst_dir, nsidcdir, fdate, obsdate):
  retcode = int(0)
  vyear = int(obsdate.strftime("%Y"))

  valid_fname = fcst_dir+'cfsv2.'+obsdate.strftime("%Y%m%d")

  if (os.path.exists(exdir + 'score_cfsv2')):
    print("Have the cfsv2 vs. nsidc scoring executable")
    sys.stdout.flush()
    pole="north/"
    ptag="n"
    obsname = (nsidcdir + pole + str(vyear) + "/seaice_conc_daily_"+ptag+"h_f17_"+
                        obsdate.strftime("%Y%m%d")+"_v03r01.nc" )
    cmd = (exdir+"score_cfsv2 "+valid_fname+" "+obsname+ " fix/skip_hr" + " > score."+
                ptag+"."+obsdate.strftime("%Y%m%d")+"f"+fdate.strftime("%Y%m%d")+".csv")
    x = os.system(cmd)
    if (x != 0):
      print("command ",cmd," returned error code ",x)
      retcode += x

  else:
    print("No score_cfsv2 executable")
    sys.stdout.flush()
    retcode += 1

  return retcode

#--------------- Utility Functions --------------------------------
def parse_8digits(tag):
  tmp = int(tag)
  (yy,mm,dd) = (int(int(tmp)/10000),int((int(tmp)%10000)/100),int(tmp)%100)
  tag_out = datetime.date(int(yy), int(mm), int(dd))
  return tag_out

#----------------- Data declarations -----------------------------
# Directories w. verification data
dirs = {
  'imsdir' : '',
  'ncepdir' : '',
  'cfsv2_dir' : '',
  'nsidcdir' : ''
}

# Known machines:
machines = {
  'RG_Home'       : '/Volumes/ncep',
  'HERA'          : '/scratch1',
  'WCOSS_C'       : '/etc/SuSE-release',
  'WCOSS_DELL_P3' : '/gpfs/dell2'
}
#----------------- Data declarations -----------------------------

# Determine which known machine we're on, if any:
mlist = []
machine=""
for x in machines:
  mlist += [x]
  if (os.path.exists(machines[x]) ):
    machine = (x)
    break

if not machine:
    print('ice verification is currently only supported on: %s' % ' '.join(machines))
    raise NotImplementedError('Cannot auto-detect platform, ABORT!')

# Establish paths to verification data:
if (machine == 'HERA'):
  dirs['imsdir'] = '/home/Robert.Grumbine/clim_data/ims/'
  dirs['ncepdir'] = '/home/Robert.Grumbine/clim_data/ice5min/'
  dirs['nsidcdir'] = '/home/Robert.Grumbine/clim_data/nsidc.nc/'
  dirs['cfsv2_dir'] = '/home/Robert.Grumbine/clim_data/cfs.verf/'
elif (machine == 'WCOSS_C'):
  dirs['imsdir'] = '/u/Robert.Grumbine/noscrub/ims/'
  dirs['ncepdir'] = '/u/Robert.Grumbine/noscrub/sice/'
  dirs['nsidcdir'] = '/u/Robert.Grumbine/noscrub/nsidc/'
elif (machine == 'WCOSS_DELL_P3'):
  dirs['imsdir'] = '/u/Robert.Grumbine/noscrub/ims/'
  dirs['ncepdir'] = '/u/Robert.Grumbine/noscrub/sice/'
  dirs['nsidcdir'] = '/u/Robert.Grumbine/noscrub/nsidc/'
elif (machine == 'RG_Home'):
  dirs['imsdir'] = '/Volumes/ncep/allconc/ims/'
  dirs['ncepdir'] = '/Volumes/ncep/allconc/ice5min/'
  dirs['nsidcdir'] = '/Volumes/ncep/allconc/nsidc_nc/'
else:
  print('ice verification is currently only supported on: %s' % ' '.join(machines))
  raise NotImplementedError('Cannot find verification data directory, ABORT!')

#Test on whether we have verf data directories
nsidcverf = os.path.exists(dirs['nsidcdir'])
ncepverf = os.path.exists(dirs['ncepdir'])
imsverf  = os.path.exists(dirs['imsdir'])
cfsv2verf  = os.path.exists(dirs['cfsv2_dir'])
if (not nsidcverf and not ncepverf and not imsverf):
  print('no ice verification directory is present, aborting')
  raise NotImplementedError('Cannot find any verification data directories, ABORT!')

#---------------------------- Begin program ---------------------

# dates, times -- initial date/time, verification date-time, or initial, 
#     lead range, and delta

#the +1 is for the command name itself, which is sys.argv[0]
if (len(sys.argv) == 3+1):
  print("Initial date and verification time")
  sys.stdout.flush()
  initial_date = parse_8digits(sys.argv[1])
  valid_date   = parse_8digits(sys.argv[2])
  fcst_dir     = sys.argv[3]
  single = True
  print(initial_date, " ", valid_date)
  sys.stdout.flush()
elif (len(sys.argv) == 4+1):
  initial_date = parse_8digits(sys.argv[1])
  lead         = int(sys.argv[2])
  dt       = datetime.timedelta(int(sys.argv[3]));
  fcst_dir = sys.argv[4]
  single = False
#RG Note: a timedelta is days and hh:mm:ss, fix arguments to handle 
#         hours as a delta
  print("Date, max lead, delta", initial_date," ",lead," ", dt)
  sys.stdout.flush()
else:
  print("wrong number of arguments")
  raise NotImplementedError('need 3 or 4 args, last being forecast directory')

#===============================================================================
#If a single verification, then one thing, else, create many single verifications:
if (single):
#IMS:
  if (imsverf): 
    x = get_ims(initial_date, dirs['imsdir'])
    if (x != 0):
      print("could not get file for ims verification, turning off imsverf\n")
      imsverf = False
    x = get_ims(valid_date, dirs['imsdir'])
    if (x != 0):
      print("could not get file for ims verification, turning off imsverf\n")
      imsverf = False
#NCEP -- grib/grib2
  if (ncepverf):
    x = get_ncep(initial_date, valid_date, dirs['ncepdir'])
    if (x != 0):
      print("could not get file for ncep verification, turning off ncepverf\n")
      ncepverf = False
#NSIDC -- netcdf
  if (nsidcverf):
    x = get_nsidc(initial_date, valid_date, dirs['nsidcdir'])
    if (x != 0):
      print("could not get file for nsidc verification, turning off nsidcverf\n")
      nsidcverf = False

  x = get_obs(initial_date, valid_date,
         imsverf, ncepverf, nsidcverf, dirs['imsdir'], dirs['ncepdir'], 
               dirs['nsidcdir'])
  if (x != 0):
    print("get_obs failed for ",initial_date.strftime("%Y%m%d")," ", valid_date.strftime("%Y%m%d")," ",x)
    obs = False
  else:
    obs = True

#Model Forecast -- cfs, coupled
  x = get_cfsv2(initial_date, valid_date, fcst_dir, "cfsv2")
  if (x != 0):
    print("get_fcst failed for ",initial_date.strftime("%Y%m%d")," ", valid_date.strftime("%Y%m%d")," ",x)
    fcst = False
  else:
    fcst = True


  #now call verification with dirs, fcst, verf logicals
  #print("working with observed data \n")
  if (imsverf):
    solo_score("ims", valid_date)
    ims_edge(initial_date.strftime("%Y%m%d"))
    ims_edge(valid_date.strftime("%Y%m%d"))
  if (ncepverf):
    solo_score("ncep", valid_date)
    ncep_edge(initial_date.strftime("%Y%m%d"))
    ncep_edge(valid_date.strftime("%Y%m%d"))
  if (nsidcverf):
    solo_score("nsidc", valid_date) #-- still to work out NH/SH vs. single input
    nsidc_edge(initial_date.strftime("%Y%m%d"), 0.40, dirs['nsidcdir'] )
    nsidc_edge(valid_date.strftime("%Y%m%d"), 0.40, dirs['nsidcdir'] )
  if (fcst):
    cfsv2_edge(initial_date.strftime("%Y%m%d"), valid_date.strftime("%Y%m%d"), "cfsv2")
    edge_score("cfsv2", valid_date, "ims", valid_date)
    edge_score("cfsv2", valid_date, "nsidc_north_edge", valid_date)
    edge_score("cfsv2", valid_date, "ncep", valid_date)
    if (nsidcverf): 
      score_cfsv2(dirs['cfsv2_dir'], dirs['nsidcdir'], initial_date, valid_date)
    

    print("\n")
    sys.stdout.flush()

