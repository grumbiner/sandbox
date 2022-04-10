from math import *
import numpy as np
import simpleaudio as sa
import csv
import matplotlib
import matplotlib.pyplot as plt

#--------------------------------------------------------
from music import *

#--------------------------------------------------------

temp = np.zeros((1000))
with open("beta") as csvfile:
  k = 0
  sreader = csv.reader(csvfile,delimiter=",")
  for line in sreader:
    temp[k] = float(line[1])
    k += 1

print("k ",k)
print("stats ",temp.max(), temp.min(), temp.mean() )
temp -= temp.mean()
print("stats ",temp.max(), temp.min(), temp.mean() )
 

#fig, ax = plt.subplots()
#ax.plot(temp)
#ax.grid()
#plt.savefig("temp.png")

from harmonic import *

# #frequencies, #data points
xtemp = harmonics(200, 1000)
xtemp.analyze(temp)

for i in range(0, xtemp.nfreqs):
  print(i+1,xtemp.ampl[i], xtemp.phase[i], 24./(xtemp.omega[i]*(24/(2.*pi))), 365/(xtemp.omega[i]*8760/(2.*pi)) )

ts = range(0,len(xtemp.ampl))
fig, ax = plt.subplots()
ax.plot(ts, xtemp.ampl)
ax.grid()
plt.savefig("xtemp.png")

#fig2,ax2 = plt.subplots()
#nx = 365*4
#ax2.plot(ts[nx-12:min(nx+12,len(xtemp.ampl))], xtemp.ampl[nx-12:min(nx+12,len(xtemp.ampl)) ])
#ax2.grid()
#plt.savefig("xtemp_quartdiurnal.png")

exit(0)


#amplitudes = np.zeros((len(amp)))
#for k in range(0,len(amp)):
#  amplitudes[k] = amp[k]
#print("amp.max ",amplitudes.max(), amplitudes.min() )

#y = note(len(sums)/music.fs, 0, 0.0)
#y = y.from_csv(sums, 1.0)
#y.extend(5)
#print("y = ",y)

#y.extend(nfold)
#x = y.note
#for i in range(0,nfold*5):
#  x = np.append(x,y.note)
#print(x.max(), x.min(), len(x), npts, x.var() )

#audio = x
#audio = audio.astype(np.int16)
#play_obj = sa.play_buffer(audio,1,2,music.fs)
#play_obj.wait_done()

#exit(0)


#Twinkle, Twinkle: ---------------------------------------
vol = 0.1
n = []
n.append( note(music.quarter_note, note.parse('C4'), vol) )
n.append( note(music.quarter_note, note.parse('C4'), vol) )
n.append( note(music.quarter_note, note.parse('G4'), vol) )
n.append( note(music.quarter_note, note.parse('G4'), vol) )
n.append( note(music.quarter_note, note.parse('A4'), vol) )
n.append( note(music.quarter_note, note.parse('A4'), vol) )
n.append( note(music.half_note, note.parse('G4'), vol) )

n.append( note(music.quarter_note, note.parse('F4'), vol) )
n.append( note(music.quarter_note, note.parse('F4'), vol) )
n.append( note(music.quarter_note, note.parse('E4'), vol) )
n.append( note(music.quarter_note, note.parse('E4'), vol) )
n.append( note(music.quarter_note, note.parse('D4'), vol) )
n.append( note(music.quarter_note, note.parse('D4'), vol) )
n.append( note(music.half_note, note.parse('C4'), vol) )

x = np.append(n[0].note,n[1].note)
for i in range (2,len(n)):
  x = np.append(x, n[i].note)
  
fig, ax = plt.subplots()
ax.plot(x[0:len(x):480])
ax.grid()
plt.savefig("twinkle.png")
#exit(0)

# Convert for play:
# _signed_ ints in 16 bits
audio = x * music.max_volume

audio = audio.astype(np.int16)
play_obj = sa.play_buffer(audio, 1, 2, music.fs)
play_obj.wait_done()

#--------------------------------------------------------
#print(music.tones['C'])
#note.parse('C4')
#d = note.parse('D4')
#print('cref = ',music.cref, music.scale)