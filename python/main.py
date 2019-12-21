#print "in point file"
#
#class Point:
#  def __init__(self,i = 0, j = 0):
#    print "hello",i,j
#    self.i = i
#    self.j = j
#    print "hello",self.i,self.j
#
#  def magnitude(self):
#    return 42

import fred

#Need the doubled naming becasue first name refers to the file that the class is in,
#  second name is the class we're using from that file
#Alternately, from fred import Point

x = fred.Point()
print x.magnitude()

y = fred.Point(3,5)
print y.magnitude()

