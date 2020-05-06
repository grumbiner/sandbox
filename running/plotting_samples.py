import matplotlib
import matplotlib.pyplot as plt


#Speed/distance:
fig, ax = plt.subplots()
ax.set(xlabel = 'Distance (km)', ylabel = 'Speed (m/s)')
ax.plot(dists/1000., dists/times)
ax.grid()
plt.show()
