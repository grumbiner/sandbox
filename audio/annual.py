from math import *
import numpy as np
import simpleaudio as sa
#--------------------------------------------------------
from music import *
from harmonic import *

#--------------------------------------------------------
def power(a, omega = 1.):
#a
#a**2
#a**2/omega
#a**2/omega**2
  return(a**2)


# 'instrument' -- to be a class
# Annual and its harmonics
annual_ampls = [21.7, 0.62, 0.42, 0.32, 0.31, 0.27, 0.22, 0.22, 0.21]
annual_harms = [1, 3, 2, 5, 9, 7, 6, 4, 13]
ampl_min = min(annual_ampls)

# Diurnal and its neighbors -- for piano
diurnal_ampls = [7.00, 0.465, 1.117, 0.714, 0.218 ]
diurnal_harms = [365, 367, 366, 364, 363 ]

# Diurnal and its harmonics:
hd_ampls = [7.00, 1.383, 0.427, 0.271 ]
hd_harms = [1, 2, 3, 4 ]

vol = 1

#-------------------------------------------------------------
base = note(music.quarter_note, note.parse('C4'), vol)

#volsum = 0.0
#for k in range(0,len(annual_ampls)):
#  volsum += power(annual_ampls[k])
#print("volsum = ",volsum)
dummy = note(music.quarter_note, note.parse('C4'), vol)
#dummy.from_harmonics(annual_ampls, annual_harms)
dummy.from_piano(diurnal_ampls, diurnal_harms)


##Now append the overtones:
n = []
#for i in range(len(annual_ampls)):
#  n.append(note())
#n[0].set( base )
#
#for k in range(1, len(annual_harms) ):
#  #base.add_overtone(annual_harms[k], (annual_ampls[k]/ampl_min) )
#  #base.add_overtone(annual_harms[k], (annual_ampls[k]/ampl_min)**2 )
#  # A^2/omega^2 weight
#  #base.add_overtone(annual_harms[k], (annual_ampls[k]/ampl_min)**2 / annual_harms[k] )
#  base.add_overtone(annual_harms[k], (annual_ampls[k]/ampl_min)**2 / annual_harms[k]**2 )
#  base.normalize(vol)
#  n[k].set( base )
##
##This establishes C4 on the new instrument, which can(?) then be shifted in to other notes
#for k in range(0, len(annual_harms)):
#  n[k].normalize(vol)

base.from_harmonics(annual_ampls, annual_harms)

#----------------------------------------------------------
y = note(music.quarter_note, note.parse('C1'), vol)
z = note()
zz = note()

base.shift("G4",y)
base.shift("A4",z)
base.shift("G4",zz, music.half_note)
#RG: How to deal with slurs? triplets?

x = np.append(base.note, base.note)
x = np.append(x, y.note)
x = np.append(x, y.note)
x = np.append(x, z.note)
x = np.append(x, z.note)
x = np.append(x, zz.note)

#---------- Play -----------------------
audio = music.max_volume * x
audio = audio.astype(np.int16)
play_obj = sa.play_buffer(audio, 1, 2, music.fs)
play_obj.wait_done()
