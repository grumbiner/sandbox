from math import *
import numpy as np

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

