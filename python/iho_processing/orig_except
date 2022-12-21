import bounders
import curves

class exception :

  #the line is a character string with the name of the area, the parameter, and its allowed bounds
  def __init__(self, line):
    words = line.split()
    self.name = words[0]
    fname = self.name +".curve"
    self.curve  = curves.curve()
    self.curve.read_curve(fname)

    self.bounds = bounders.bounds(param=words[1], 
                       pmin = words[2], pmax = words[3], 
                       pmaxmin = words[4], pminmax = words[5])
    #debug self.bounds.show()

  def is_ok(self, pt, value):
    tmp = self.curve.inside(pt) # is the point inside the curve?
    t2  = self.bounds.ptinbounds(value)  #is it out in bounds?
    return(tmp and t2)

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
print( exceptions[0].is_ok(pt, value))

#in region, out of bounds
pt = (-5.0, 75.0)
value = 2.8
print( exceptions[0].is_ok(pt, value))

#not in region, in bounds if it were:
pt = (-5.0, 35.0)
value = 1.8
print( exceptions[0].is_ok(pt, value))
