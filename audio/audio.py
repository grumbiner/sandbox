from math import *
import numpy as np
import simpleaudio as sa

#Various test computations and illustrations

#fs = 44100
fs = 48000
seconds = 3
# Time
t = np.linspace(0, seconds, seconds*fs, False)

bpm = 180
quarter_note = 60./bpm #seconds
#print(2**(1./12.), 9./8., 1.125**0.5)

#r = 1.5
#for k in (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ):
#  f = r**k
#  while (f > 2.0):
#    f /= 2.0
#  print(k, f, r**k)
#for k in range (0,64):
#  r *= 1.5
#  if (r > 2.0):
#    r /= 2.0
#  print(k,r,log(2)/log(r))

  
# approx minimum on computer speaker and bob's ear: freq  = 140
# approx maximum on computer speaker and bob's ear: freq  = 9250
freq = 440
note  = np.sin(freq*t*2*np.pi)

freq = 442
##note += np.sin(freq*t*2*np.pi) * 0.125
note += np.sin(freq*t*2*np.pi) * 0.0 
#
freq *= 438
##note += np.sin(freq*t*2*np.pi) * 0.125
note += np.sin(freq*t*2*np.pi) * 0.0

#freq -= 2
#note += np.sin(freq*t*2*np.pi) * 0.5

# _signed_ ints in 16 bits
audio = note * (2**15 - 1) / np.max(np.abs(note))
audio = audio.astype(np.int16)
#print(audio.max(), audio.min())

play_obj = sa.play_buffer(audio, 1, 2, fs)
play_obj.wait_done()
