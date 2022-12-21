# Parse Hurdat data format files
# Robert Grumbine 13 July 2021

import csv
import datetime
import numpy

#class hurdat:
non_value = -999.0
NE = 0
SE = 1
SW = 2
NW = 3
epoch = datetime.datetime(1851,1,1)
epoch_start = epoch.toordinal()

class storm:

  def __init__(self, basin, number, year, name, nentries):
    self.basin  = basin
    self.number = int(number)
    self.year   = int(year)
    self.name   = name
    self.nentries = int(nentries)
    self.dataline = []
    self.wmax = non_value
    self.pmin = non_value
#storm_data:
#  dtime, record_id, status
#  lat, lon, wind (kt), slp (mb) 
#  qr_35, qr_50, qr_64 [4]  -- quadrantal radii

  def add_data(self, dataline):
    x = dataline_parse(dataline) #x is class 'storm_data'
    self.dataline.append(x)

  def show_evolution(self):
    for i in range (0,self.nentries-1):
      dwind = non_value
      dslp  = non_value
      obstime_delta = (self.dataline[i+1].dtime - self.dataline[i].dtime)/datetime.timedelta(seconds=3600) 
      #RG: Important to check wind, slp for nonvalues
      if (self.dataline[i+1].wind > 0 and self.dataline[i].wind > 0):
        dwind = self.dataline[i+1].wind - self.dataline[i].wind
      if (self.dataline[i+1].slp != non_value and  self.dataline[i].slp != non_value):
        dslp  = self.dataline[i+1].slp  - self.dataline[i].slp

      print(self.basin,"{:2d}".format(self.number),self.year,
           "{:5.1f}".format(self.dataline[i].lat), "{:6.1f}".format(self.dataline[i].lon),
           "{:6.1f}".format(dwind), "{:6.1f}".format(dslp),
            self.dataline[i].wind, self.dataline[i].slp,
            self.dataline[i].dtime, self.dataline[i].dtime.toordinal()-epoch_start,
            obstime_delta )

  def has_quadrantals(self):
    return (self.dataline[0].qr_35[0] != non_value)
    
  def was_hurricane_force(self):
    if (self.max_wind() > 64):
      return True
    else: 
      return False

  def was_storm_force(self):
    if (self.max_wind() > 34):
      return True
    else: 
      return False

  def max_wind(self):
    if (self.wmax == non_value):
      tmp = 0.
      for i in range(0,self.nentries):
        if (self.dataline[i].wind > tmp):
          tmp = self.dataline[i].wind
      if (tmp == 0):
        return non_value
      else:
        self.wmax = tmp

    return self.wmax

  def min_pressure(self):
    if (self.pmin == non_value):
      tmp = 2000.
      for i in range(0,self.nentries):
        if (self.dataline[i].slp < tmp):
          tmp = self.dataline[i].slp
      if (tmp == 0):
        return non_value
      else:
        self.pmin = tmp

    return self.pmin

class storm_data:
  def __init__(self, dtime, id, status, lat, lon, wind, slp, qr_35, qr_50, qr_64):
    self.dtime = dtime
    self.id    = id
    self.status = status
    self.lat   = lat
    self.lon   = lon
    self.wind  = wind
    self.slp   = slp
    self.qr_35 = qr_35
    self.qr_50 = qr_50
    self.qr_64 = qr_64


# pair the parses to writes -- 
#   e.g. filter full data set to something of interest, then have it to work on directly
def header_parse(line):
  basin  = line[0][0:2]
  number = int(line[0][2:4])
  year   = int(line[0][4:8])
  name   = line[1]
  nentries = line[2]
  #print(basin, number, year, name, nentries)
  x = storm(basin, number, year, name, nentries)
  return(x) 

def dataline_parse(dataline):
  year  = int(dataline[0][0:4])
  month = int(dataline[0][4:6])
  day   = int(dataline[0][6:8])
  hh    = int(dataline[1][0:3]) #time in utc HHMM
  mm    = int(dataline[1][3:5])
  dtime = datetime.datetime(year, month, day, hh, mm)
  #if (mm != 0):
  #  print("dtime = ",dtime)
  record_id = dataline[2]
  status    = dataline[3]
  #parse N/S at end of char list
  if (dataline[4][-1] == "N"):
    lat = float(dataline[4][0:-1])
  else:
    lat = -1.*float(dataline[4][0:-1])
    print('south, lat = ',lat)

  if (dataline[5][-1] == "W"):
    lon = -float(dataline[5][0:-1])
  else:
    lon = float(dataline[5][0:-1])
  #print('lon = ',lon)

  wind  = float(dataline[6])
  slp   = float(dataline[7])
  #if (slp != non_value):
  #  print("wind slp ",wind, slp)
  #else:
  #  print("wind ",wind)
  # Quadrantal radii vs. critical wind speeds of 35, 50, 64 kt
  # NE, SE, SW, NW in order
  qr_35 = numpy.zeros((4))
  qr_50 = numpy.zeros((4))
  qr_64 = numpy.zeros((4))
  
  qr_35[NE] = float(dataline[8])
  qr_35[SE] = float(dataline[9])
  qr_35[SW] = float(dataline[10])
  qr_35[NW] = float(dataline[11])
  qr_50[NE] = float(dataline[12])
  qr_50[SE] = float(dataline[13])
  qr_50[SW] = float(dataline[14])
  qr_50[NW] = float(dataline[15])
  qr_64[NE] = float(dataline[16])
  qr_64[SE] = float(dataline[17])
  qr_64[SW] = float(dataline[18])

  storm = storm_data(dtime, record_id, status, lat, lon, wind, slp, qr_35, qr_50, qr_64)

  return storm


# Pass the input file 
# RG: make member of hurdat class?
def file_parse(fname):
  fin = open(fname, 'r')
  #RG: Test on success
  sreader = csv.reader(fin, delimiter=",")
  allstorms = []

  newstorm = True
  k = 0
  for line in sreader:
    if (newstorm ):
      headline = line;
      x = header_parse(headline)  #x is class 'storm'
      #print(x.name, x.nentries)
      newstorm = False
    else:
      dataline = line
      #print("parser k = ",k," dataline = ",dataline)
      x.add_data( dataline)
      k += 1
      if (k == int(x.nentries) ):
        allstorms.append(x)
        newstorm = True
        k = 0

  return allstorms
 

##########################################################################

allstorms = file_parse("hurdat2-1851-2019-052520.txt")
# print("found ",len(allstorms)," storms")

for i in range (0,len(allstorms)):
#  print(allstorms[i].name, allstorms[i].number, allstorms[i].year, allstorms[i].max_wind() , allstorms[i].min_pressure() )

#  if (allstorms[i].nentries > 1):
#    allstorms[i].show_evolution()

  if (allstorms[i].has_quadrantals()):
    allstorms[i].show_evolution()
    

