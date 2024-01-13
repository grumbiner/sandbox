#!/usr/bin/env python3

import matplotlib
import matplotlib.pyplot as plt

import cartopy.crs as ccrs
import cartopy.feature as cfeature

#matplotlib.use('agg') #non-interactive
#-------------------------------------------

projection = ccrs.PlateCarree()
fig,ax = plt.subplots(figsize=(7.5,7.5), subplot_kw=dict(projection=projection))

print("about to call coastlines",flush=True)
#ax.coastlines()

ax.add_feature(cfeature.GSHHSFeature(levels=[1], scale="c") )
ax.add_feature(cfeature.GSHHSFeature(levels=[2], scale="l") )
ax.add_feature(cfeature.GSHHSFeature(levels=[3,4], scale="f") )

print("back from call coastlines",flush=True)
plt.savefig("hello2.png")
print("back from savefig",flush=True)
