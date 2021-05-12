import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.animation as animation


def init():
    print("hello from init")
    x,y = np.meshgrid(np.linspace(-2,2,nx), np.linspace(-2,2,ny))
    x,y = x-x.mean(), y-y.mean()
    z = x*y * np.exp(-x**2 - y**2)
    z /= z.max()
    del xdata[:]
    del ydata[:]
    line.set_data(xdata, ydata)
    return line,

nx = int(200)
ny = int(200)
z = np.zeros((nx,ny))
xdata, ydata = [], []

fig, ax = plt.subplots()
ax.set(xlabel = "X label", ylabel = "Y label")
c = matplotlib.cm.get_cmap(name="coolwarm")
plt.set_cmap(c)
#z = init()
line, = ax.plot([])
ax.pcolormesh(z)
ax.grid()

count = int(0)

def run(z):
    print("hello from run, count = ",count)
    count += 1
    z *= -1
#    line.set_data()
    return line,


ani = animation.FuncAnimation(fig, run, blit=False, frames = 15,
                              repeat=False, init_func=init)
plt.show()
