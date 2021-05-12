import numpy as np

import matplotlib
import matplotlib.pyplot as plt

nx = int(200)
ny = int(200)
#x = np.linspace(-2,2,nx)
#y = np.linspace(-2,2,ny)
x,y = np.meshgrid(np.linspace(-2,2,nx), np.linspace(-2,2,ny))
x,y = x-x.mean(), y-y.mean()
z = np.zeros((nx,ny))
z = x*y * np.exp(-x**2 - y**2)
z /= z.max()

fig,ax = plt.subplots()
ax.set(xlabel = "x label")
ax.set(ylabel = "y label")
beta =  plt.imshow(z)
c = matplotlib.cm.get_cmap(name="Greens")
c = matplotlib.cm.get_cmap(name="coolwarm")
plt.set_cmap(c)
#matplotlib.colorbar.ColorbarBase(ax)
cbar = fig.colorbar(beta, ax=ax, extend='both')
cbar.minorticks_on()
plt.show()




#matplotlib.colorbar.ColorbarBase(ax)
#plt.gray()
