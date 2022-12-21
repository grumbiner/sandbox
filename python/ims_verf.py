import os
import sys
import datetime

#ims tools

exdir = "./"
##################### ------------- 

#------------------------------------------------------------------
def get_ims(initial_date, imsdir):
  retcode = int(0)
  initial    = int(initial_date.strftime("%Y%m%d"))

  ims_file_start = imsdir + "imssnow96." + str(initial) + ".grib2"

  if (os.path.exists(ims_file_start) ):
    fname = 'ims.'+str(initial)

    if (not os.path.exists(fname)):
      cmd='wgrib2 '+ims_file_start + "| grep ICEC | wgrib2 -i " + ims_file_start + " -no_header -order we:ns -bin " + fname
      print(cmd)
      x = os.system(cmd)
      if (x != 0): retcode += x

  else:
    print("cannot get_ims file ",ims_file_start)
    retcode += 1

  return retcode

def ims_edge(initial):
  retcode = int(0)
  fname = 'ims.'+str(initial)
  cmd = exdir + 'find_edge_ims ' + fname + ' > ims_edge.' + str(initial)
  os.system(cmd)
  x = os.system(cmd)
  if (x != 0): retcode += x
