from math import *
import numpy as np
import simplaudio as sa

import sounddevice as sd
from scipy.io.wavfile import write

# included by 'note', which extends the music class: from music import *
from note import *

# ----- --------------------------------------
# ----- Recording demo with possible write out
fs = 44100   #(may also come from music.fs)
seconds = 2

recording = sd.rec(int(seconds*fs), samplerate=fs, channels=1)
sd.wait()

print(len(recording))

for i in range(0,len(recording)):
  print(i, recording[i])

#write('output.wav', fs, recording)
# ----- --------------------------------------


#---------- Play -----------------------
#using simplaudio, play a vector x at volume 'music.max_volume'
# music is an RG class
audio = music.max_volume * x
audio = audio.astype(np.int16)
play_obj = sa.play_buffer(audio, 1, 2, music.fs)
play_obj.wait_done()
# ----- --------------------------------------
