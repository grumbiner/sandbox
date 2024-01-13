#!/usr/bin/env python3

import matplotlib
import matplotlib.pyplot as plt

import cartopy.crs as ccrs

matplotlib.use('agg') #non-interactive
#-------------------------------------------

projection = ccrs.Miller()
fig,ax = plt.subplots(figsize=(7.5,7.5), subplot_kw=dict(projection=projection))

print("about to call coastlines",flush=True)
#ax.coastlines(resolution = '110m')
ax.coastlines()
print("back from call coastlines",flush=True)
plt.savefig("hello2.png")
print("back from savefig",flush=True)
#plt.show()
print("done",flush=True)
