from math import *
import numpy as np
import simpleaudio as sa
#--------------------------------------------------------
from music import *

#--------------------------------------------------------
# 'instrument' -- to be a class
# diurnal and its neighbors
vol       = 1
harm_base = 365.
ampls     = [7.00, 0.46, 1.117, 0.714, 0.218]
harms     = [365,   367,   366,   364,   363]

volsum = 0.
for i in range(len(harms)):
  volsum += note.power(ampls[i])
base  = note(music.quarter_note, note.parse('C4'), note.power(ampls[0])/volsum )

n = []
for i in range(len(ampls)):
  n.append(note())
n[0].set( base )

#Now append the overtones:
for k in range(1, len(harms) ):
  base.add_ratio(harms[k]/harm_base, note.power(ampls[k])/volsum) 
  n[k].set( base )

#This establishes C4 on the new instrument, which can(?) then be shifted in to other notes
base.normalize(vol)
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
