#class for dealing with bounding curves
import curves

x = curves.curve()

x.read_curve("sulu.curve")

#should be true
pt = (float(110.), float(10.0) )
print(x.inside(pt))

#should be false
pt = (110., 20.0)
print(x.inside(pt))
