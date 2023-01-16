from note import *
import simpleaudio as sa

# -----------------  For a piano-like principle -------------------------
harm_base = 365.
#ampls = [7., 7., 3., 2., 1.]
#harms = [365., 366., 367., 364., 363.]
#ampls = [7., 1.38, 0.4, 0.27, 0.]
ampls = [7., 0., 0., 0., 0.]
harms = [365., 730., 1095., 1460., 1825.]

volsum = 0.
for i in range(0, len(harms)):
  volsum += note.power(ampls[i])
base = note(music.quarter_note, note.parse('C4'), note.power(ampls[0]/volsum) )

base_pure = note(music.quarter_note, note.parse('C4'), note.power(ampls[0]/volsum) )

for k in range(1, len(harms)):
  base.add_ratio(harms[k]/harm_base, note.power(ampls[k]/volsum))

#debug: for i in range(0, len(base.note)):
#debug:   print(i, base.note[i], base.note[i]-base_pure.note[i])

#-------------------------------------------------------------------------
# Construct some music
vol = 1.
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
