import os
import sys

from math import *

import datetime

import numpy as np
import numpy.ma as ma

from filtering import *

# noodle satobs class and descendents
sat_lr = match.amsr2_lr()
tmp = match.match(sat_lr)

def near(x, y, delta):
    return(fabs(x-y) < delta)

#----------------------------------------------------------
fout = open(sys.argv[1], "a")

start = datetime.datetime(2020,12,1)
end   = datetime.datetime(2023,3,31)
#end   = datetime.datetime(2022,1,4)
dt = datetime.timedelta(1)

trips = 0
while (start <= end and trips < 3000):
  tag=start.strftime("%Y%m%d")
  filename = "dev/amsr2."+tag+"/lr.txt."+tag
  print(filename)
  count = 0
  if (os.path.exists(filename)):

    try:
        fin = open(filename, "r")
    except:
        print("failed to open input",filename)
        exit(1)

    #for line in fin:
    #  tmp.matched_read(line)
    lrtmp = match.raw_read(fin)
    fin.close()

    for tmp in lrtmp:
      #if (near(tmp.obs.latitude,46.5,5.) and near(tmp.obs.longitude,-48.5, 5.) ):
      if (near(tmp.obs.latitude,47.5,7.5) and near(tmp.obs.longitude,-52.5, 7.5) ):
        tmp.show(fout)
        #debug: tmp.show()
        count += 1
    
    print("found ", count, "newfie points", flush=True)

  trips += 1
  start += dt

#---------------------------------------------------------------------
fout.close()

