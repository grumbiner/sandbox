print "in point file"

class Point:

  def __init__(self,i = 0, j = 0):
    print "hello",i,j
    self.i = i
    self.j = j
    print "hello",self.i,self.j

  def magnitude(self):
    return 42


