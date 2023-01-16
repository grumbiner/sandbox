from math import *
import numpy as np
import simpleaudio as sa

#--------------------------------------------------------
from music import *

#--------------------------------------------------------
#Twinkle, Twinkle:
vol = 0.5

n = []
n.append( note(music.quarter_note, note.parse('C5'), vol) )
n.append( note(music.quarter_note, note.parse('C5'), vol) )
n.append( note(music.quarter_note, note.parse('G5'), vol) )
n.append( note(music.quarter_note, note.parse('G5'), vol) )
n.append( note(music.quarter_note, note.parse('A5'), vol) )
n.append( note(music.quarter_note, note.parse('A5'), vol) )
n.append( note(music.half_note, note.parse('G5'), vol) )

n.append( note(music.quarter_note, note.parse('F5'), vol) )
n.append( note(music.quarter_note, note.parse('F5'), vol) )
n.append( note(music.quarter_note, note.parse('E5'), vol) )
n.append( note(music.quarter_note, note.parse('E5'), vol) )
n.append( note(music.quarter_note, note.parse('D5'), vol) )
n.append( note(music.quarter_note, note.parse('D5'), vol) )
n.append( note(music.half_note, note.parse('C5'), vol) )

x = np.append(n[0].note,n[1].note)
for i in range (2,len(n)):
  x = np.append(x, n[i].note)

# Convert for play:
audio = x * music.max_volume
# _signed_ ints in 16 bits
audio = audio.astype(np.int16)
play_obj = sa.play_buffer(audio, 1, 2, music.fs)
play_obj.wait_done()
