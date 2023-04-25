import os
import sys
import datetime

#Arguments:
#   start_date verification_date forecast_dir_path
#   or
#   start_date number_days_forward time_delta_days forecast_dir_path

from verf_files import *

exbase=os.environ['EXDIR']

print('exbase = ',exbase)

