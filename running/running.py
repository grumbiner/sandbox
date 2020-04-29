import re
# Tools for working on running-related information
# Robert Grumbine
# 3 November 2018

#------------------------------------------------------------------
#  Constants:
rods_to_feet    = 16.5
meters_to_feet  = (39.37 / 12.)
marathon        = 42195.0               #meters

meters_to_yards = meters_to_feet / 3.
mi              = 5280. / meters_to_feet
mi_to_km        = mi / 1000.0

#conversion functions -- RG.  Add

#Race distances:
standard_distances = (200., 400., 800., 1000., 1200., 1500., 1600., mi,
  2000., 2400., 3000., 3200., 2.*mi, 4000., 3.*mi, 5000., 4.*mi, 8000., 5.*mi,
  10000., 15*1000., 10.*mi, marathon/2., marathon, 50*1000.)
addl_distances = (50., 55., 60., 100., 300., 600.)
# approx WR times:


#dictionary:
#dict(marathon,42195.; half,marathon/2.; mile,mi)

#-----------------pace conversion---------------------------------------
def from_reference(d, t_ref = 2000., l_reference = 5000., alpha_1 = 1.115, alpha_2 = 1.07):
  if (d < 200.):
    print ("too much of a sprint to work with for now ",d)
    t = float(0.)
  elif (d > l_reference):
    t = float(t_ref*pow((d / l_reference ), alpha_2))
  else:
    t = float(t_ref*pow((d / l_reference ), alpha_1))
  return t

def to_reference(d, t, l_reference = float(5000.), alpha_1 = 1.115, alpha_2 = 1.07):
#  show(d, t)
#  Finding reference time from given time/distance pair
  #under 200 m is special
  if (d < 200.):
    print ("too much of a sprint to work with for now ",d)
    t_ref = float(0.)
  elif (d > float(l_reference)):
    t_ref = float(t*pow((l_reference / d), alpha_2))
  else:
    t_ref = float(t*pow((l_reference / d), alpha_1))
  return t_ref


#----------------formatting-----------------------------------------
def t_to_string(t):
#time management:
  if (t < 60.):
    time_out = "{:8.2f}".format(t )
  elif (t < 3600.):
    string1 = "{:05.2f}".format(t % 60 )
    tmp = (t - (t % 60. )) / 60.
    string2 = "{:2d}".format(int(tmp % 60))
    time_out = string2+":"+string1
  else:
    string1 = "{:05.2f}".format(t % 60 )
    tmp = (t - (t % 60. )) / 60.
    string2 = "{:02d}".format(int(tmp % 60))
    tmp = (tmp - (tmp % 60.)) / 60.
    time_out = str(int(tmp))+":"+string2+":"+string1
  return time_out

def d_to_string(d):
#distance management:
  rem = d % 100.
  if (rem < 1.):
    if (d > 3200.):
      dist_out =("{:4.1f}".format(int(d/1000.))+" km")
    else:
      dist_out =("{:5d}".format(int(d))+" m")
  else:
    rem = d % mi
    if (rem < 5. or (mi - rem) < 5.):
      dist_out = ("{:4.1f}".format(d/mi)+" mi")
    else:
      dist_out = ("{:4.2f}".format(d/marathon)+ " marathon ")
  return dist_out

def show(d, t):
  time = t_to_string(t)
  dist = d_to_string(d)
  print (dist , time)

def str_to_distance(dstring):
  s = re.findall(r'\d+\.?\d*', dstring)
  distance = float(s[0])
  if ("km" in dstring ):
    distance *= 1000.
  elif ("mi" in dstring ):
    distance *= mi_to_km*1000.
  elif ("m" in dstring ):
    distance *= 1.
  elif ("k" in dstring ):
    distance *= 1000.
  else:
    distance *= 1.
  return float(distance)

def str_to_time(tstring):
  m = re.split(':', tstring)
  tau = 0.0
  for i in range (0, len(m) ):
    tau = float(m[i]) + 60.*tau
  return float(tau)

###########################################################################
class runner:
  def __init__(self, d = 0., t = 0., l_ref = 5000., name = "", gender= 'm'):
    self.alpha_1  = 1.115 #from 200m to lref
    self.alpha_2  = 1.07  #from lref up
    self.l_ref = float(l_ref)
    #200m to lref (5k, 3200?) using alpha_1 for middle distance runner
    #over lref (3200?) use alpha_2
    self.t_ref =  to_reference(d, t, self.l_ref, self.alpha_1, self.alpha_2)
    self.name  = name
    self.gender = 'm'     #or f

#----------------------------------==--------------------------------------
# world record 5k times
elite_male   = runner(5000., 756.)
elite_female = runner(5000., 850.)

###########################################################################
