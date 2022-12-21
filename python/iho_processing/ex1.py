import bounders
from curves import *

class exception(curve) :

  #the line is a character string with the name of the area, the parameter, and its allowed bounds
  def __init__(self, line):
    words = line.split()
    self.name = words[0]
    fname = self.name +".curve"
    self.read_curve(fname)

    self.bounds = bounders.bounds(param=words[1], 
                  pmin = words[2], pmax = words[3], 
                  pmaxmin = words[4], pminmax = words[5])
    #self.bounds.show()

  def is_ok(self, pt, value):
    tmp = self.inside(pt)         # is the point inside the curve?
    t2  = self.bounds.ptinbounds(value)  # is it out in bounds?
    return(tmp and t2)

#------------------- demonstration -----------------
f = open("exceptions.file")
exceptions = []
for line in f:
  x = exception(line)
  exceptions.append(x)
  #x.bounds.show()

print(len(exceptions))

#in region, in bounds
pt = (-5.0, 75.0)
value = 1.8
print( exceptions[0].inside(pt) )
print( exceptions[0].is_ok(pt, value))

exit(0)

#in region, out of bounds
pt = (-5.0, 75.0)
value = 2.8
print( exceptions[0].is_ok(pt, value))

#not in region, in bounds if it were:
pt = (-5.0, 35.0)
value = 1.8
print( exceptions[0].is_ok(pt, value))
