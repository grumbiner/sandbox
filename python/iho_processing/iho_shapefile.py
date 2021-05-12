import numpy as np
import shapefile

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

def inside(x, bbox):
  return (x[0] >= bbox[0] and x[0] <= bbox[2] and
          x[1] >= bbox[1] and x[1] <= bbox[3] )

#def inside(x, curve):
#  npts = len(curve)
#
#  unclosed = False
#  if (curve[0][0] != curve[npts-1][0]  or
#      curve[0][1] != curve[npts-1][1] ):
#    #debug print("did not close, add extra point")
#    unclosed = True
#    last = curve[0]
#
#  wn = 0
#  for i in range(0,npts-1):
#    if (curve[i][0] <= x[0]):
#      if (curve[i+1][0] > x[0]):
#        if (isleft(curve[i], curve[i+1], x) > 0):
#          wn += 1
#    else:
#      if (curve[i+1][0] <= x[0]):
#        if (isleft(curve[i], curve[i+1], x) < 0):
#          wn -= 1
#    #debug print(i,wn)
#
#  if (unclosed):
#    i = npts-1
#    if (curve[i][0] <= x[0]):
#      if (last[0] > x[0]):
#        if (isleft(curve[i], last, x) > 0):
#          wn += 1
#    else:
#      if (last[0] <= x[0]):
#        if (isleft(curve[i], last, x) < 0):
#          wn -= 1
#  
#  return wn
  

#defined: shapeType = 5, -> shapefile.POLYGON

sf = shapefile.Reader("World_Seas_IHO_v3.shp")
print(len(sf))
print(sf.bbox)

print(sf.fields)

print(sf.shapeTypeName, sf.shapeType)

y = sf.shapes()

nx = 80
ny = 90
wncount = np.zeros((ny, nx))
total_area = 0.0
 
for k in range(0, len(sf)):
  x = sf.shape(k) 
  total_area += sf.record(k)[8]
  print(k, len(y[k].points), sf.record(k)[8], sf.record(k)[0] )
  #n = len(x.points)
  #if (x.points[0] != x.points[n-1]):
  #  #print("open curve: ",k,x.points[0], x.points[n-1])
  #  print("open curve: ",k,x.points[0][0] - x.points[n-1][0], x.points[0][1] - x.points[n-1][1])

  #if (sf.record(k)[0] == "Sulu Sea"):
  #  print(x.bbox, len(x.bbox))
  #  print(x.bbox[0], x.bbox[1])
  #  print(x.bbox[2], x.bbox[3])
    
    #for nlat in range(0, ny):
    #  for nlon in range(0, nx):
    #    pt = (116.0+0.1*nlon, 5.0+0.1*nlat) #a point in the Sulu Sea
    #    wn = inside(pt, x.bbox)
    #    if (wn):
    #      wncount[nlat, nlon] = 1
    #      print(pt, wn)

    #pt = (140.0, 30.0) # a point outside
    #print(inside(pt, x.points))

    #for i in range(0,len(x.points)):
    #  print(i,x.points[i])

print("total area: ",total_area)

#print(wncount.max(), wncount.min())
#import matplotlib
#import matplotlib.pyplot as plt
#from matplotlib.colors import BoundaryNorm
#from matplotlib.ticker import MaxNLocator
#
#yax,xax = np.mgrid[slice(0,ny+1,1), slice(0,nx+1,1) ]
#levels = MaxNLocator(nbins=5).tick_values(wncount.min(), wncount.max())
#cmap = plt.get_cmap('PiYG')
#norm = BoundaryNorm(levels, ncolors=cmap.N,clip=True)
#fig,ax = plt.subplots()
#im = ax.pcolormesh(xax,yax,wncount,cmap=cmap, norm=norm)
#fig.colorbar(im, ax=ax)
#ax.set_title('test')
#plt.show()
