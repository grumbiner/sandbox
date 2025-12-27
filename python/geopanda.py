import geopandas as gpd

fp = "NWP.shp"
data = gpd.read_file(fp)
orig = data

print(data.crs, flush=True)

print("data\n",data,flush=True)

import matplotlib
import matplotlib.pyplot as plt
fig, (ax1, ax2) = plt.subplots(nrows=1, ncols=2, figsize=(12,8))

orig.plot(ax=ax1, facecolor='gray')
ax1.set_title("wgs84")

data.plot(ax=ax2, facecolor='blue')
ax2.set_title("second")

plt.tight_layout()

plt.savefig("try.png")
