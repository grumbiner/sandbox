import sounddevice as sd
from scipy.io.wavfile import write
fs = 44100
seconds = 2

recording = sd.rec(int(seconds*fs), samplerate=fs, channels=1)
sd.wait()

print(len(recording))

for i in range(0,len(recording)):
  print(i, recording[i])

#write('output.wav', fs, recording)
