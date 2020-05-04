import sys
from running import *


dstring = sys.argv[1]
tstring = sys.argv[2]

d = str_to_distance(dstring)
t = str_to_time(tstring)

show(d, t)
show(5000., 2200.)

