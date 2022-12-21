import sys
import os

from math import *
import numpy as np
import simpleaudio as sa
#--------------------------------------------------------
from new    import *
from harmonic import *

#--------------------------------------------------------
# 'instrument' -- to be a class
## Annual and its harmonics
#annual_ampls = [21.7, 0.62, 0.42, 0.32, 0.31, 0.27, 0.22, 0.22, 0.21]
#annual_harms = [1, 3, 2, 5, 9, 7, 6, 4, 13]
ampls = []
phase = []
harms = []
fin = open(sys.argv[1], "r")
for line in fin:
  words = line.split(" ")
  ampls.append(float(words[1]))
  harms.append(float(words[0]))
  phase.append(float(words[2])*np.pi/180.)

for i in range(len(ampls)):
  print(harms[i], ampls[i], phase[i])

#-------------------------------------------------------------
base = note(music.quarter_note, note.parse('C4'), phase = phase[0])

#base.from_harmonics(ampls, harms, phase)
base.from_piano(ampls, harms, phase)

#----------------------------------------------------------
y = note(music.quarter_note, note.parse('C1'))
z = note()
zz = note()

base.shift("G4",y)
base.shift("A4",z)
base.shift("G4",zz, music.half_note)
#RG: How to deal with slurs? triplets?

song = np.append(base.note, base.note)
song = np.append(song, y.note)
song = np.append(song, y.note)
song = np.append(song, z.note)
song = np.append(song, z.note)
song = np.append(song, zz.note)

#---------- Play -----------------------
audio = music.max_volume * song
audio = audio.astype(np.int16)
play_obj = sa.play_buffer(audio, 1, 2, music.fs)
play_obj.wait_done()
