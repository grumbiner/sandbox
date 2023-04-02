import os
import sys
import datetime

#Arguments:
#   start_date verification_date forecast_dir_path
#   or
#   start_date number_days_forward time_delta_days forecast_dir_path

from ims_verf import *

##################### ------------- 
#------------------------------------------------------------------
def get_fcst(initial_date, valid_date, fcst_dir):
  retcode = int(0)
  initial = int(initial_date.strftime("%Y%m%d"))
  valid   = int(valid_date.strftime("%Y%m%d"))
  fname = fcst_dir+'/ice'+str(valid)+'00.01.'+str(initial)+'00.nc'

  if (os.path.exists(fname)):
    print("Have forecast file for ",valid)
  else:
    retcode += 1
    print("Do not have forecast file ",fname)
  return retcode


#------------------------------------------------------------------
def get_nsidc(initial_date, valid_date, nsidcdir):
  retcode = int(0)
#  $nsidcdir/north/yearinitial/seaice_conc_daily_nh_f17_$tag8_v03r01.nc
#  $nsidcdir/south/yearinitial/seaice_conc_daily_sh_f17_$tag8_v03r01.nc
#  $nsidcdir/north/yearvalid/seaice_conc_daily_nh_f17_$tag8_v03r01.nc
#  $nsidcdir/south/yearvalid/seaice_conc_daily_sh_f17_$tag8_v03r01.nc
  return 1

def nsidc_edge(initial):
  retcode = int(0)
  fname = 'nsidc.'+str(initial)
  cmd = exdir + 'find_edge_nsidc ' + fname + ' > nsidc_edge.' + str(initial)
  os.system(cmd)
  x = os.system(cmd)
  if (x != 0): retcode += x
  return retcode


def ncep_edge(initial):
  retcode = int(0)
  fname = 'ncep.'+str(initial)
  cmd = exdir + 'find_edge_ncep ' + fname + ' > ncep_edge.' + str(initial)
  os.system(cmd)
  x = os.system(cmd)
  if (x != 0): retcode += x
  return retcode


def get_ncep(initial_date, valid_date, ncepdir):
  retcode = int(0)
  return 1

def get_obs(initial_date, valid_date, imsverf, ncepverf, nsidcverf, imsdir, ncepdir, nsidcdir):
  retcode = int(0)
  initial    = int(initial_date.strftime("%Y%m%d"))
  valid      = int(valid_date.strftime("%Y%m%d"))
  moninitial = int(initial_date.strftime("%Y%m"))
  monvalid   = int(valid_date.strftime("%Y%m"))
  yearinitial = int(initial_date.strftime("%Y"))
  yearvalid   = int(valid_date.strftime("%Y"))

  return retcode

#--------------- Utility Functions --------------------------------
def parse_8digits(tag):
  tmp = int(tag)
  (yy,mm,dd) = (int(int(tmp)/10000),int((int(tmp)%10000)/100),int(tmp)%100)
  #debug print(tmp,' ',yy,' ',mm,' ',dd)
  tag_out = datetime.date(int(yy), int(mm), int(dd))
  return tag_out

#----------------- Data declarations -----------------------------

# Determine whether we're on a known machine:
machines = {
  'RG_Home'       : '/Volumes/ncep',
  'THEIA'         : '/scratch3',
  'WCOSS_C'       : '/etc/SuSE-release',
  'WCOSS_DELL_P3' : '/gpfs/dell2'
}

mlist = []
machine=""
#debug    if not machine:
#debug      print("machine is empty string")
#debug    else:
#debug      print("string non-empty")

for x in machines:
  mlist += [x]
#debug      print("x, path",x,machines[x])
  if (os.path.exists(machines[x]) ):
    machine = (x)
    # debug print('machine = ',machine,' path = ', machines[x])
    break

#debug    print(mlist)
#debug    print('machine = ',machine, 'x = ', x, str(x))

if not machine:
    print ('ice verification is currently only supported on: %s' % ' '.join(machines))
    raise NotImplementedError('Cannot auto-detect platform, ABORT!')

# Establish paths to verification data:
dirs = {
  'imsdir' : '',
  'ncepdir' : '',
  'nsidcdir' : ''
}
#debug: print(dirs)
if (machine == 'THEIA'):
  dirs['imsdir'] = '/home/Robert.Grumbine/save/ims/'
  dirs['ncepdir'] = '/home/Robert.Grumbine/save/ice5min/'
  dirs['nsidcdir'] = '/home/Robert.Grumbine/save/nsidc.nc/'
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
  print ('ice verification is currently only supported on: %s' % ' '.join(machines))
  raise NotImplementedError('Cannot find verification data directory, ABORT!')

#Test on whether we have verf data directories
nsidcverf = os.path.exists(dirs['nsidcdir'])
ncepverf = os.path.exists(dirs['ncepdir'])
imsverf  = os.path.exists(dirs['imsdir'])
#debug print("tests ",nsidcverf, ncepverf, imsverf)
if (not nsidcverf and not ncepverf and not imsverf):
  print('no ice verification directory is present, aborting')
  raise NotImplementedError('Cannot find any verification data directories, ABORT!')

#---------------------------- Begin program ---------------------


# dates, times -- initial date/time, verification date-time, or initial, lead range, and delta
#print(sys.argv)

#the +1 is for the command name itself, which is sys.argv[0]
if (len(sys.argv) == 3+1):
  print("Initial date and verification time")
  initial_date = parse_8digits(sys.argv[1])
  valid_date   = parse_8digits(sys.argv[2])
  fcst_dir     = sys.argv[3]
  single = True
  print(initial_date, " ", valid_date)
elif (len(sys.argv) == 4+1):
  initial_date = parse_8digits(sys.argv[1])
  lead         = int(sys.argv[2])
  dt       = datetime.timedelta(int(sys.argv[3]));
  fcst_dir = sys.argv[4]
  single = False
#RG Note: a timedelta is days and hh:mm:ss, fix arguments to handle hours as a delta
  print("Date, max lead, delta", initial_date," ",lead," ", dt)
else:
  print("wrong number of arguments")
  raise NotImplementedError('need 3 or 4 args, last being forecast directory')

#debug print("single = ",single)

#====================================================================================
#If a single verification, then one thing, else, create many single verifications:
if (single):
#IMS:
  if (imsverf): 
    x = get_ims(initial_date, imsdir)
    if (x != 0):
      print("could not get file for ims verification, turning off imsverf\n")
      imsverf = False
    x = get_ims(valid_date, imsdir)
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
         imsverf, ncepverf, nsidcverf, dirs['imsdir'], dirs['ncepdir'], dirs['nsidcdir'])
  if (x != 0):
    print("get_obs failed for ",initial_date.strftime("%Y%m%d")," ",
           valid_date.strftime("%Y%m%d")," ",x)
    obs = False
  else:
    obs = True

#Model Forecast
  x = get_fcst(initial_date, valid_date, fcst_dir)
  if (x != 0):
    print("get_fcst failed for ",initial_date.strftime("%Y%m%d")," ",
           valid_date.strftime("%Y%m%d")," ",x)
    fcst = False
  else:
    fcst = True

  #now call verification with dirs, fcst, verf logicals
    if (imsverf):
      ims_edge(initial_date.strftime("%Y%m%d"))
    if (ncepverf):
      ncep_edge(initial_date.strftime("%Y%m%d"))
    if (nsidcverf):
      nsidc_edge(initial_date.strftime("%Y%m%d"))
    if (fcst):
      print("\n")

else:
  for d in range (1,lead+1):
    valid_date = initial_date + d*dt
    x = get_obs(initial_date, valid_date,
           imsverf, ncepverf, nsidcverf, dirs['imsdir'], dirs['ncepdir'], dirs['nsidcdir'])
    if (x != 0):
      print("get_obs failed for ",initial_date.strftime("%Y%m%d")," ",
             valid_date.strftime("%Y%m%d")," ",x)
      obs = False
    else:
      obs = True

    x = get_fcst(initial_date, valid_date, fcst_dir)
    if (x != 0):
      print("get_fcst failed for ",initial_date.strftime("%Y%m%d")," ",
             valid_date.strftime("%Y%m%d")," ",x)
      fcst = False
    else:
      fcst = True

    #now call verification with dirs, fcst, verf logicals
    if (imsverf):
      ims_edge(initial_date.strftime("%Y%m%d"))
    if (ncepverf):
      ncep_edge(initial_date.strftime("%Y%m%d"))
    if (nsidcverf):
      nsidc_edge(initial_date.strftime("%Y%m%d"))
    if (fcst):
      print("\n")

