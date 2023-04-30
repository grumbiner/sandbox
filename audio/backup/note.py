from math import *
import numpy as np
#import simpleaudio as sa

#--------------------------------------------------------

class music:
  #fs = 48000
  fs = 44100
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

  def  __init__(self, duration, frequency, volume):
  # volume = range [0,1] real
    self.duration  = duration
    self.frequency = frequency
    self.volume    = volume
    ts = np.linspace(0, duration, int(duration*self.fs), False)
    self.note = np.sin(self.frequency*ts*2.*np.pi)
    self.note -= self.note.min()
    self.note *= self.volume

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

  def noise(self, mag):
  #add white noise
    tmp = np.zeros(len(self.note) )
    tmp = np.random.uniform(low=-1., high=+1., size=len(self.note))
    tmp *= mag
    self.note += tmp

    
#--------------------------------------------------------
