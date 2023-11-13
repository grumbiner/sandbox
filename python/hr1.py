#!/usr/bin/env python3
import argparse
import glob
import os
import csv

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import netCDF4 as nc

import cartopy.crs as ccrs
import cartopy.feature as cfeature

matplotlib.use('agg')


#-------------------------------------------------------------
def getedge(fin, edgelons, edgelats):
    for line in fin:
        words=line.split(",")
        edgelats.append(float(words[1]))
        edgelons.append(float(words[0]))

    print("found ",len(edgelats), len(edgelons), "pts in edge file", flush=True)


#PlateCaree
#LambertConformal
#LambertCylindrical
#NorthPolarStereo

def plot_world_map(lons, lats, data, edgelons, edgelats):
    vmin = np.nanmin(data)
    vmax = np.nanmax(data)

    proj = ccrs.LambertConformal(central_longitude=-170, central_latitude=65., cutoff=25.)
    #proj = ccrs.NorthPolarStereo(true_scale_latitude=60.)
    #proj = ccrs.Stereographic(central_longitude=+170, central_latitude=60. )

    ax  = plt.axes(projection = proj)
    fig = plt.figure(figsize=(12, 9))
    ax  = fig.add_subplot(1, 1, 1, projection = proj)

    #ax.set_extent((-220,-120, 40, 90), crs=ccrs.PlateCarree())

    #Bering/okhotsk/some Beaufort/Chukchi
    ax.set_extent((-220,-145, 50, 80), crs=ccrs.PlateCarree())
    ax.gridlines(crs=ccrs.PlateCarree(), 
                 xlocs=[140., 150., 160., 170., -180, -170, -160, -150], 
                 ylocs=[45, 50, 55, 60, 66.6, 70, 75] )

    #'natural earth' -- coast only -- 
    ax.coastlines(resolution='10m')
    #ax.add_feature(cfeature.GSHHSFeature(levels=[1,2,3,4], scale="i") )

    plttitle = 'Plot of variable %s' % ("hello2")
    plt.title(plttitle)

    #Establish the color bar
    #colors=matplotlib.cm.get_cmap('jet')
    #colors=matplotlib.cm.get_cmap('gray')
    colors=matplotlib.cm.get_cmap('terrain')

# For gridded fields of 'data'
    #print("vmin max = ",vmin, vmax, flush=True)
    #cs = ax.pcolormesh(lons, lats, data,vmin=vmin,vmax=vmax,cmap=colors, transform=ccrs.PlateCarree() )
    #cs = ax.pcolormesh(lons, lats, data,vmin=30.,vmax=vmax,cmap=colors, transform=ccrs.PlateCarree() )
    #cb = plt.colorbar(cs, extend='both', orientation='horizontal', shrink=0.5, pad=.04)
    #cbarlabel = '%s' % ("hello1")
    #cb.set_label(cbarlabel, fontsize=12)

# For a scatter plot of points:
    #plt.scatter(edgelons, edgelats, transform=ccrs.PlateCarree(), s = 0.5, alpha = 0.5 )
    cs = plt.scatter(lons, lats, c = data, transform=ccrs.PlateCarree(), s = 0.5, alpha = 0.5 )
    cb = plt.colorbar(cs, extend='both', orientation='horizontal', shrink=0.5, pad=.04)
    cbarlabel = '%s' % ("hello1")
    cb.set_label(cbarlabel, fontsize=12)

# General

    plt.savefig("aice.scatter.png")

    plt.close('all')

#----------------------------------------------------------------

# For UFS quarter degree, selected subset:
model = nc.Dataset("ice20200217.01.2020021600.subset.nc","r")
lons1 = model.variables["TLON"][:,:]
lats1 = model.variables["TLAT"][:,:]
aice1 = model.variables["aice_h"][0,:,:]

# For OSI-SAF ice, 10 km polar stereo grids:
obs  = nc.Dataset("ice_conc_nh_polstere-100_multi_202302171200.nc","r")
lons2 = obs.variables["lon"][:,:]
lats2 = obs.variables["lat"][:,:]
ic   = obs.variables["ice_conc"][0,:,:]
aice2 = ic.astype('float')
if (aice2.max() > 2.0): aice2 /= 100.


# For NIC edges:
edgelats = []
edgelons = []
#fin = open("n.2022148.beta","r")
#getedge(fin, edgelons, edgelats)

plot_world_map(lons2, lats2, aice2, edgelons, edgelats)

