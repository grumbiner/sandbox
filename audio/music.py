import numpy as np
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
  def  __init__(self, duration = music.quarter_note, frequency = music.cref, volume = 0.5):
    #print("in init, note", flush=True)
    self.duration  = duration
    self.frequency = frequency
    self.volume    = volume
    ts = np.linspace(0, duration, int(duration*self.fs), False)
    self.note = np.sin(self.frequency*ts*2.*np.pi)
    self.note -= self.note.min()
    self.note *= self.volume
    #print("init freq",self.frequency, flush=True)

  def set(self, x):
    self.duration  = x.duration
    self.frequency = x.frequency
    self.volume    = x.volume
    self.note[:]   = x.note[:]

  #for harmonic additions
  def add_overtone(self, harmonic, proportion):
    freq = harmonic * self.frequency
    ts   = np.linspace(0, self.duration, int(self.duration*self.fs), False)
    tmp  = np.sin(freq*ts*2.*np.pi)
    tmp -= tmp.min()
    tmp *= self.volume*proportion
    self.note += tmp

  #sounds near original frequency
  def add_ratio(self, ratio, proportion):
    freq = self.frequency*ratio
    ts   = np.linspace(0, self.duration, int(self.duration*self.fs), False)
    tmp  = np.sin(freq*ts*2.*np.pi)
    tmp -= tmp.min()
    tmp *= self.volume*proportion
    self.note += tmp


  def shift(self, name, y, length = music.quarter_note):
    relength = length / self.duration
    #debug print("shift ",length, self.duration, relength, flush=True)
    y.duration  = length
    y.volume    = self.volume
    y.frequency = note.parse(name)
    y.note      = np.zeros((len(self.note)))
    ratio = y.frequency/self.frequency
    #print("y ",y.frequency, self.frequency, ratio, len(self.note), len(y.note), flush=True )

    for k in range(0,len(self.note*relength)):
      y.note[k] = self.note[ int(k*ratio+0.5) % len(self.note) ]
     

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
    #print(name, octave, x, music.cref, flush=True)
    freq = music.cref
    n = octave-4
    freq *= 2**(n)
    freq *= 2**(x/12.)
    return freq

  def from_csv(self, ampls, mag):
    #print("ampls range: ",ampls.max(), ampls.min(), flush=True )
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

# Spectral power for an amplitude and frequency:
  def power(a, omega = 1.):
  #a
  #a**2
  #a**2/omega
  #a**2/omega**2
    return(a**2/omega)

# Given amplitudes and base frequency, composite a note for this 'instrument'
  def from_harmonics(self, ampls, harms):
    self.ampls = ampls
    self.harms = harms
    volsum = 0.
    for i in range(len(harms)):
      volsum += note.power(self.ampls[i])
    print("harmonics ",self.ampls, len(self.ampls), volsum )
    #Assumes that the base tone is already in place
    for i in range(1,len(harms)):
      self.add_overtone(harms[i], note.power(ampls[i])/volsum)
    self.normalize(1.)
      

# Amplitudes and relative frequencies close to each other
  def from_piano(self, ampls, harms):
    self.ampls = ampls
    self.harms = harms
    harm_base = harms[0]
    volsum = 0.
    for i in range(len(harms)):
      volsum += note.power(self.ampls[i])
    print("piano ",self.ampls, len(self.ampls) )
    for i in range(1, len(harms)):
      self.add_ratio(harms[i]/harm_base, note.power(ampls[i])/volsum)
    self.normalize(1.)

#--------------------------------------------------------
