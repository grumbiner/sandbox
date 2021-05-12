import numpy as np

class base :
  nx = int(0)
  ny = int(0)
  
  def __init__(self, x = 36, y = 36):
    self.nx = x
    self.ny = y
    #print("base nx ny ",self.nx, self.ny)

  def show(self):
    print("base show ", self.nx, self.ny)

class grid2(base) :
  npts = int(42)

  def __init__(self, x = 36, y = 36):
    base.__init__(self, x,y)
    self.npts = self.nx*self.ny
    #print("hello from grid2 init ", self.npts)

  def show(self):
    print("grid2 show ", self.nx, self.ny, self.npts)


#----------------------------------------

alpha = base(5,5)
alpha.show()

x = grid2(5,5)
x.show()

y = grid2()
y.show()
