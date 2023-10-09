import sys

class filt:
    def __init__(self):
        self.type = 'none'
        self.chan = -1
        self.tcrit = -1.
        self.pice  = 0.
        self.pland = 0.
        self.pwater = 0.
        self.npts   = 0.
    def hold(self, words):
        self.type = words[1]
        self.chan = int(words[2])
        self.tcrit = float(words[3])
        self.pice  = float(words[4])
        self.pland = float(words[5])
        self.pwater = float(words[6])
        self.npts   = int(words[7])
    def show(self):
        print(self.type, self.chan, self.tcrit, self.pice, self.pland, self.pwater, self.npts)

fin = open(sys.argv[1], "r")
collect = []
most = 0
for line in fin:
  if ('ch' in line):
    words = line.split()
    tmp = filt()
    tmp.hold(words)
    if (tmp.npts > most):
      most = tmp.npts
    collect.append(tmp)
    del tmp

#debug: print("channel info: ",len(collect))
#scan by channel, by type, for most npts:
for ch in range (0, 12):
  best_hot = filt()
  best_cold = filt()
  for i in range(0, len(collect)):
    if (collect[i].chan == ch):
      if (collect[i].type == 'hot'):
        if (collect[i].npts > best_hot.npts):
          best_hot = collect[i]
      elif (collect[i].type == 'cold'):
        if (collect[i].npts > best_cold.npts):
          best_cold = collect[i]
      else:
        print("invalid type ",collect[i].type)

  #display if more than 1% as many points as the best filter
  if (best_cold.npts > most/100):
    best_cold.show()
  if (best_hot.npts > most/100):
    best_hot.show()
  del best_hot, best_cold
