#!/Library/Frameworks/Python.framework/Versions/3.7/bin/python3
from math import *

#######################################################
def tfreeze(salinity):
# Millero formula as given in Gill, 1982
 
  a1 = -0.0575
  a2 =  1.710523E-3 
  a3 = -2.154996E-4

  return(salinity*(a1+a2*sqrt(salinity)+a3*salinity))

def dew_point(t, rh):
  if (t > 0):
    const = dict(b=17.368, c=238.88)
  else:
    const = dict(b=17.966, c=247.15)

  pa = rh / 100. * exp(const['b']*t / (const['c'] + t))
  dp = const['c'] * log(pa) / (const['b'] - log(pa))

  return dp

#######################################################
# class grid.py
# class latpt.py
def oiv2(lat, lon):
  dlat = 0.25
  dlon = 0.25
  firstlat = -89.875
  firstlon = 0.125
  j = round( (lat - firstlat)/dlat )
  i = round( (lon - firstlon)/dlon )
  return (j,i)

def rg12th(lat, lon):
  dlat = -1./12.
  dlon = 1./12.
  firstlat = 90. - dlat/2.
  firstlon = dlon/2.
  if (lon < 0):
    lon += 360.
  j = round( (lat - firstlat)/dlat )
  i = round( (lon - firstlon)/dlon )
  return (j,i)


#############################################
# Parse a string to a latitude or longitude value
def llparse(x):
  # start with labelled string
  if (x[-1] == 'N' or x[-1] == 'E'):
    tmp = float(x[0:-1])
  elif (x[-1] == 'S' or x[-1] == 'W'):
    tmp = -float(x[0:-1])
  # else assume it's a clean number
  else:
    tmp = float(x)
#RG: Would be a good idea to have the option of enforcing some longitude standards
  return tmp 
  
#Define standards by the minimum allowed longitude, -360, -180, 0
def lon_standards(x, lonmin = -999., lonmax = 999.):
  tmp = x
  if (tmp < lonmin):
    while(tmp < lonmin):
      tmp += 360.
  if (tmp > lonmax):
    while (tmp > lonmax):
      tmp -= 360.
  return tmp

# Distance between two lat-long points
def arcdis(lat1, lon1, lat2, lon2):
#  return(abs(lat2-lat1)+abs(lon2-lon1))
  earth_radius = 6371.2
  rpdg = pi / 180.0
  ab = (90. - lat1)*rpdg
  ac = (90. - lat2)*rpdg

  dlon = abs(lon1 - lon2)
  if (abs(dlon) > 180.):
    lon2minus = lon2 - 360.
    dlonminus = abs(lon1 - lon2minus)
    lon2plus = lon2 + 360.
    dlonplus = abs(lon1 - lon2plus)
    if (dlonminus < dlon):
      dlon = dlonminus
    elif (dlonplus < dlon):
      dlon = dlonplus
    else:
      print("failed in longitude fix ",lon1, lon2)

  bc = dlon*rpdg

  arg = cos(ab)*cos(ac) + sin(ab)*sin(ac)*cos(bc)
  if (arg > 1):
    arg = 1
  if (arg < -1):
    arg = -1

  return(earth_radius*acos(arg))


#######################################################
def harmonic_pred(date, ampl, phase):
  harmonic_start = datetime.date(2011, 1, 1)
  PERIOD = 365.259635
  dt = date.toordinal() - harmonic_start.toordinal()
  nharmonics = 4
  x = 0.0
  for k in range (0,nharmonics):
    x += ampl[k]*cos(float(dt*k)*2.*pi/PERIOD - phase[k])
  return x



#######################################################
# The gaussian functions check for field being vaguely gaussian --
#    1) mean is 'near' the center of the range
#    3) skew is 'near' 0
#    4) kurtosis is 'near' 0 (the 3 of a true gaussian)

  def gaussian(self, multiplier = 0.5):
    toler = self.sd*multiplier
    if (abs((self.max - self.min)/2. - self.mean) < toler):
      #print('field is vaguely symmetric')
      return True
    else:
      #print('field is not even remotely symmetric')
      return False

  def gaussian3(self, toler = 0.5):
    if (abs(self.skew) < toler):
      return True
    else:
      return False

  def gaussian4(self, toler = 3.):
    if (abs(self.kurtosis) < toler):
      return True
    else:
      return False

#-----------------------------------
import math
#Robert Grumbine
#1 June 2018

class const:
    rpdg           = math.pi / 180. 
    dgpr           = 180. / math.pi
    nmtokm = 1.852

    earth_radius   = 6371.2   # km -- sphere
    earth_spheroid = 6378.137 # km -- oblate spheroid (WGS84)
    eccen2         = 0.00669438
#Computed parameters
    eccen        = math.sqrt(eccen2)
    ps_chi1  = ( eccen2/2. + 5.*eccen2*eccen2/24. + eccen2*eccen2*eccen2/12.);
    ps_chi2  = ( 7.*eccen2*eccen2/48. + 29.*eccen2*eccen2*eccen2/240.);
    ps_chi3  = ( 7.*eccen2*eccen2*eccen2/120. );
    ps_ll    = math.pow( math.pow(1.+eccen,1.+eccen) * math.pow(1.-eccen,1.-eccen) , 1./2.);

#from 
##ifdef WGS84
#  const double parameters::rearth  = 6378.137e3;
#  const double parameters::eccen2 = 0.00669438;
#  #define ECCEN2 0.00669438
##else
#  const double parameters::rearth  = 6378.160e3; //earth definition is from
#  const double parameters::eccen2 = 0.006694604; //GRIB standard
#  #define ECCEN2 0.006694604
##endif

albedo = float(0.80)
sigma = 5.67e-8
sc = 1366.0

def t_black_body(albedo):
  return pow( (1-albedo)/4./sigma*sc , 0.25)

######################################################################
#The score can be any function of the errors. RMSE is used here for demonstration
#   purposes, but in truth, it could be anything.
# exponential(delta) is fine (and will give very strange results in the
#   evolution -- try it)
#Interface to various scoring methods:
def score(obs, pred, delta, start, end, metric = 0, tolerance = 0):
    if (metric == RMS):
      return score_rms(delta, start, end, tolerance)
    elif (metric == MEAN):
      return score_mean(delta, start, end, tolerance)
    elif (metric == MAE):
      return score_mae(delta, start, end, tolerance)
    elif (metric == RM3):
      return score_mean3(delta, start, end, tolerance)
    elif (metric == RM4):
      return score_mean4(delta, start, end, tolerance)
    elif (metric == NLOSS):
      print("metric nloss not currently working, continuing with RMS", flush=True)
      return score_rms(delta, start, end)
      #return score_loss(delta, start, end, tolerance)
    elif (metric == VICKIE):
      return score_mae(delta, start, end, tolerance = 3.0)
    else:
      print("unknown metric ",metric, " continuing with RMS", flush=True)
      return score_rms(delta, start, end)

#RMS -- default score
def score_rms(delta, start, end, tolerance = 0):
    tmp = delta[start:end]
    tmp *= tmp
    return sqrt(sum(tmp)/(end-start))

#Mean
def score_mean(delta, start, end, tolerance = 0):
    tmp = delta[start:end]
    return abs(sum(tmp)/(end-start))

#Mean absolute error
def score_mae(delta, start, end, tolerance = 0):
    tmp = abs(delta[start:end])
    count = len(tmp)
    if (tolerance != 0.0):
      count = 0
      for k in range(0, end-start):
        if (tmp[k] < tolerance):
          tmp[k] = 0.0
        else:
          count += 1
    return (sum(tmp)/count)

#Mean3
def score_mean3(delta, start, end, tolerance = 0):
    tmp = delta[start:end]
    return pow(abs(sum(tmp*tmp*tmp)/(end-start)),1./3.)

#Mean4
def score_mean4(delta, start, end, tolerance = 0):
    tmp = delta[start:end]
    return pow(abs(sum(tmp*tmp*tmp*tmp)/(end-start)),1./4.)

#Number of losses -- fewer is better
def score_loss(obs, pred, delta, start, end, tolerance):
    count = 0
    for k in range (start, end):
       if (abs(delta[k]) > abs(obs[k]) ):
         count += 1
    return count

#############################################


def trivial_prime(x, primes):
  tmp = x
  isprime = True
  for i in range(0,len(primes)):
    if (tmp%primes[i] == 0 ):
      isprime = False
    if (not isprime):
      break
  return isprime,primes[i]

def factor(x, primes):
  tmp = x
  f = trivial_prime(x, primes)
  if (not f[0]):
    return f

  maxprime = int(primes[len(primes)-1])
  top_trial = int(sqrt(x))
  for i in range(maxprime+2, top_trial+1, 2):
    f = factor(i, primes)
    if (f[0] == True):
      k = int(i)
      primes.append(k)
      print(k," is prime",flush=True)
      if (x%k == 0):
        print(k," divides ",x,flush=True)
        return(False, k)

  isprime = True
  for i in range(0,len(primes)):
    if (tmp%primes[i] == 0 ):
      isprime = False
      print("factor: ",i,flush=True)
    if (not isprime):
      break
  return isprime,primes[i]

##Demo program using above:
## start up a set of primes
#primes = []
#k = int(2)
#primes.append(k)
#k = 3
#primes.append(k)
#n = int(2*3*5*7*11*13*17*19*23*29*31*37*41*43+1)
#
##full prime factorization:
#print("n starts: ",n)
#f = factor(n,primes)
#while(not f[0]):
#  n = int(n / f[1])
#  print("number of primes ",len(primes))
#  f = factor(n,primes)
#if (f[0]):
#  print("n is prime ",n)
#  #k = n
#  #primes.append(k)

#############################################################
def strprec(x):
  if (abs(x) < 1.e-3):
    strpmin = "{:.5e}".format(x)
  else:
    strpmin = "{:.5f}".format(x)
  return strpmin

#--------------- Utility Functions --------------------------------
def parse_8digits(tag):
  tmp = int(tag)
  (yy,mm,dd) = (int(int(tmp)/10000),int((int(tmp)%10000)/100),int(tmp)%100)
  tag_out = datetime.date(int(yy), int(mm), int(dd))
  return tag_out

