import matplotlib.pyplot as plt
import cartopy.crs as ccrs

fig = plt.figure(figsize=(3, 3))
ax = plt.axes(projection=ccrs.Stereographic())
ax.coastlines(resolution='110m')
ax.gridlines()
print("made gridlines",flush=True)

plt.show()

plt.savefig("tmp.png")
