from math import *
import numpy as np
import simpleaudio as sa
#--------------------------------------------------------
from music import *
#from harmonic import *

#--------------------------------------------------------
# 'instrument' -- to be a class
# Diurnal cycle and its harmonics
ampls = [7.00, 1.383, 0.427, 0.271]
harms = [1, 2, 3, 4 ]
ampl_min = min(ampls)

vol = 1
base = note(music.quarter_note, note.parse('C4'), vol)
n = []
for i in range(len(ampls)):
  n.append(note())
n[0].set( base )

#Now append the overtones:
for k in range(1, len(harms) ):
  base.add_overtone(harms[k], (ampls[k]/ampl_min)**2 / harms[k] )
  base.normalize(vol)
  n[k].set( base )

#This establishes C4 on the new instrument, which can(?) then be shifted in to other notes
for k in range(0, len(harms)):
  n[k].normalize(vol)

#----------------------------------------------------------
y = note(music.quarter_note, note.parse('C1'), vol)
base.shift("G4",y)

x = np.append(base.note, base.note)
x = np.append(x, y.note)
x = np.append(x, y.note)

#---------- Play -----------------------
audio = music.max_volume * x
audio = audio.astype(np.int16)
play_obj = sa.play_buffer(audio, 1, 2, music.fs)
play_obj.wait_done()
