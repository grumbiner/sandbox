from math import *
import numpy as np
import simpleaudio as sa

#--------------------------------------------------------

class music:
  fs = 48000
 #fs = 44100
  max_volume   = (2**15 - 1)

  bpm   = 120.
  scale = 440.                 # A above middle C 
  cref  = scale * 2**(-9./12.) # C4, 261.63 
  
  quarter_note   = 60./bpm           #seconds
  half_note      = 2.*quarter_note
  whole_note     = 2.*half_note
  eighth_note    = quarter_note / 2.
  sixteenth_note = eighth_note / 2.

#temper: even-temper, pentatonic, heptonic, ...
  #map:
  # C/B#, C#/Db, D, D#/Eb, E, F/E#, F#/Gb, G, G#/Ab, A, A#/Bb, B/Cb
  # [0,11]
  tones = {
     'C'  : 0,
     'B#' : 0,
     'C#' : 1,
     'Db' : 1,
     'D'  : 2,
     'D#' : 3,
     'Eb' : 3,
     'E'  : 4,
     'E#' : 5,
     'F'  : 5,
     'F#' : 6,
     'Gb' : 6,
     'G'  : 7,
     'G#' : 8,
     'Ab' : 8,
     'A'  : 9,
     'A#' : 10,
     'Bb' : 10,
     'B'  : 11,
     'Cb' : 11 
   }
  # C = first tone, cycle (c,d,e,f,g,a,b)

#--------------------------------------------------------

class note(music):

  # volume = range [0,1] real
  def  __init__(self, duration, frequency, volume):
    self.duration  = duration
    self.frequency = frequency
    self.volume    = volume
    ts = np.linspace(0, duration, int(duration*self.fs), False)
    self.note = np.sin(self.frequency*ts*2.*np.pi)**1
    self.note -= self.note.min()
    self.note *= self.volume

  def add_overtone(self, harmonic, proportion):
    freq = harmonic * self.frequency
    ts   = np.linspace(0, self.duration, int(self.duration*self.fs), False)
    tmp  = np.sin(freq*ts*2.*np.pi)
    tmp -= tmp.min()
    tmp *= self.volume*proportion
    self.note += tmp

  def normalize(self, mag):
    delta = self.note.max() - self.note.min()
    self.note -= self.note.min()
    if (delta != mag):
      self.note *= (mag / delta)

  #add white noise
  def noise(self, mag):
    tmp = np.zeros(len(self.note) )
    tmp = np.random.uniform(low=-1., high=+1., size=len(self.note))
    tmp *= mag
    self.note += tmp

  def parse(name):
  #name as in C#4 (c sharp, 4th octave, i.e. middle C sharp)
  # C = first tone, cycle (c,d,e,f,g,a,b)
    octave = int(name[-1])
    x = music.tones[name[0:-1] ]
    #print(name, octave, x, music.cref)
    freq = music.cref
    n = octave-4
    freq *= 2**(n)
    freq *= 2**(x/12.)
    return freq

  def from_csv(self, ampls, mag):
    print("ampls range: ",ampls.max(), ampls.min() )
    ampls -= ampls.min()
    ampls /= ampls.max() # [0,1]
    ampls *= mag * (music.max_volume - 1)
    ampls += 1
    
    #ampls = np.exp(ampls)
    #ampls -= ampls.min()
    #ampls /= ampls.max()
    #ampls *= music.max_volume
    self.note      = ampls
    self.duration  = len(ampls/music.fs)
    self.frequency = 0
    self.volume    = mag
    return self
    
  def extend(self, ratio):
    ampls = self.note
    self.note      = np.zeros((len(self.note))*ratio)
    self.duration *= ratio
    for k in range(0,len(self.note) ):
      self.note[k] = ampls[int(k/ratio)]
        
#--------------------------------------------------------
import csv

tdew = np.zeros((24*365))
temp = np.zeros((24*365))
wdir = np.zeros((24*365))
wspd = np.zeros((24*365))
with open("a.csv") as csvfile:
  k = 0
  sreader = csv.reader(csvfile,delimiter=",")
  for line in sreader:
    tdew[k] = float(line[0])
    temp[k] = float(line[1])
    wdir[k] = float(line[2])
    wspd[k] = float(line[3])
    k += 1

#print("k ",k)
#print("stats ",tdew.max(), temp.max(), wdir.max(), wspd.max(), wspd.mean() )
tdew -= tdew.mean()
temp -= temp.mean()
wdir -= wdir.mean()
wspd -= wspd.mean()
#print("max ",tdew.max(), temp.max(), wdir.max(), wspd.max() )
#print("min ",tdew.min(), temp.min(), wdir.min(), wspd.min() )
 
#exit(0)

import matplotlib
import matplotlib.pyplot as plt
#fig, ax = plt.subplots()
#ax.plot(temp)
##ax.plot(tdew, temp)
#ax.grid()
#plt.savefig("temp.png")

class harmonics:
#length = data record length, or, alternatively, cycle length
#nfreqs = number of frequencies, taking, for now, all to be 
#         harmonics of the fundamental (sledge hammer approach to fourier transform)
  def __init__(self, nfreqs, length):
    self.fsin    = np.zeros((nfreqs))
    self.fcos    = np.zeros((nfreqs))
    self.period  = float(length)
    self.sum_sin = np.zeros((nfreqs))
    self.sum_cos = np.zeros((nfreqs))
    self.ampl    = np.zeros((nfreqs))
    self.phase   = np.zeros((nfreqs))
    self.nfreqs  = int(nfreqs)
    self.omega   = np.zeros((nfreqs))
    for i in range(0, nfreqs):
      self.omega[i] = 2.*pi*(i+1)/self.period #fundamental period

  def analyze(self, obs):
    for step in range (0, len(obs)):
      for i in range(0, self.nfreqs):
        self.fsin[i]     = sin(2.*pi*(i+1)*step/self.period) * 2./self.period
        self.fcos[i]     = cos(2.*pi*(i+1)*step/self.period) * 2./self.period
        self.sum_sin[i] += obs[step] * self.fsin[i]
        self.sum_cos[i] += obs[step] * self.fcos[i]
    for i in range(0, self.nfreqs):
      self.ampl[i]  = sqrt(self.sum_sin[i]**2 + self.sum_cos[i]**2)/(self.period/len(obs))
      self.phase[i] = atan2(self.sum_sin[i], self.sum_cos[i])*180./pi

xtemp = harmonics(4*365+12, 24*365)
xtemp.analyze(temp)
xtdew = harmonics(4*365+12, 24*365)
xtdew.analyze(tdew)

for i in range(0, xtemp.nfreqs):
  print(i+1,xtemp.ampl[i], xtemp.phase[i], xtdew.ampl[i], 24/(xtemp.omega[i]*(24/(2.*pi))), 365/(xtemp.omega[i]*8760/(2.*pi)) )
#print(xtemp.ampl)
#print(xtemp.phase)

ts = range(0,len(xtemp.ampl))
fig, ax = plt.subplots()
ax.plot(ts, xtemp.ampl)
ax.plot(ts, xtdew.ampl)
ax.grid()
plt.savefig("xtempdew.png")

fig2,ax2 = plt.subplots()
nx = 365*4
ax2.plot(ts[nx-12:min(nx+12,len(xtemp.ampl))], xtemp.ampl[nx-12:min(nx+12,len(xtemp.ampl)) ])
ax2.plot(ts[nx-12:min(nx+12,len(xtemp.ampl))], xtdew.ampl[nx-12:min(nx+12,len(xtemp.ampl)) ])
ax2.grid()
plt.savefig("xtemp_quartdiurnal.png")

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

