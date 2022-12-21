import numpy as np

#RG utility -- is point inside bounding curve
# derived from C++ implementation
def isleft(p0, p1, p2):
  tmp = (p1[0]-p0[0]) * (p2[1]-p0[1]) - (p2[0] - p0[0])*(p1[1]-p0[1])
  if (tmp > 0):
    return 1
  elif (tmp < 0):
    return -1
  else:
    return 0

def inside(x, curve):
  npts = len(curve)

  unclosed = False
  if (curve[0][0] != curve[npts-1][0]  or
      curve[0][1] != curve[npts-1][1] ):
    print("did not close, add extra point")
    unclosed = True
    last = curve[0]
    exit(1)

  wn = 0
  for i in range(0,npts-1):
    if (curve[i][0] <= x[0]):
      if (curve[i+1][0] > x[0]):
        if (isleft(curve[i], curve[i+1], x) > 0):
          wn += 1
    else:
      if (curve[i+1][0] <= x[0]):
        if (isleft(curve[i], curve[i+1], x) < 0):
          wn -= 1
    #debug print(i,wn)

  if (unclosed):
    i = npts-1
    if (curve[i][0] <= x[0]):
      if (last[0] > x[0]):
        if (isleft(curve[i], last, x) > 0):
          wn += 1
    else:
      if (last[0] <= x[0]):
        if (isleft(curve[i], last, x) < 0):
          wn -= 1
  
  return wn
  


nx = 80
ny = 90
points = []
for lon in range (0,nx):
  m = (float(lon),0.)
  points.append(m)
for lat in range (0,ny):
  m = (float(nx), float(lat))
  points.append(m)
for lon in range (nx, 0, -1):
  m = (float(lon), float(ny))
  points.append(m)
for lat in range (ny, 0, -1):
  m = (0., float(lat))
  points.append(m)
m = points[0]
points.append(m)

print(len(points))
print(points[0], points[len(points)-1])

wn = np.zeros((ny, nx))

for nlat in range(0, ny):
  for nlon in range(0, nx):
    pt = (float(nlon*2.), float(nlat*2.))
    wn[nlat, nlon] = inside(pt, points)
    #print(nlon, nlat, wn[nlat, nlon])
print(wn.max(), wn.min())

import matplotlib
import matplotlib.pyplot as plt
from matplotlib.colors import BoundaryNorm
from matplotlib.ticker import MaxNLocator

yax,xax = np.mgrid[slice(0,ny+1,1), slice(0,nx+1,1) ]
levels = MaxNLocator(nbins=5).tick_values(wn.min(), wn.max())
cmap = plt.get_cmap('PiYG')
norm = BoundaryNorm(levels, ncolors=cmap.N,clip=True)
fig,ax = plt.subplots()
im = ax.pcolormesh(xax,yax,wn,cmap=cmap, norm=norm)
fig.colorbar(im, ax=ax)
ax.set_title('test')
plt.show()
