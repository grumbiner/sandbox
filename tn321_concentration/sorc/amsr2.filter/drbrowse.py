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

        c1 = int(words[2].strip('[],'))
        c2 = int(words[3].strip('[],'))
        self.chan = [c1,c2]

        self.tcrit = float(words[4])
        self.pice  = float(words[5])
        self.pland = float(words[6])
        self.pwater = float(words[7])
        self.npts   = int(words[8])
    def show(self):
        print(self.type, self.chan, self.tcrit, self.pice, self.pland, self.pwater, self.npts)

#-------------- begin working code ---------------------
fin = open(sys.argv[1], "r")
collect = []
most = 0
w2=""
for line in fin:
  if ('dr' in line):
    words = line.split()
#    for w in words:
#        try:
#            i = int(w)
#        except:
#            w2 = w.strip('[],')
#            try:
#                i = int(w2)
#            except:
#                i = -10
#        print(w, w2, i)
#    exit(0)
    tmp = filt()
    tmp.hold(words)
    if (tmp.npts > most):
      most = tmp.npts
    collect.append(tmp)
    del tmp

#debug: 
print("channel info: ",len(collect), flush=True)

#scan by channel, by type, for most npts:
for ch1 in range (0, 12):
  for ch2 in range (ch1+1, 12):
    best_hot = filt()
    best_cold = filt()
    for i in range(0, len(collect)):
      if (collect[i].chan[0] == ch1 and collect[i].chan[1] == ch2 ):
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
