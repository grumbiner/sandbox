#!/usr/bin/env python3
import argparse
import glob
import os

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import netCDF4 as nc

import cartopy.crs as ccrs
import cartopy.feature as cfeature

print("finished with imports",flush=True)

matplotlib.use('agg')
print("set batch environment", flush=True)

#PlateCaree
#LambertConformal
#LambertCylindrical
#NorthPolarStereo

def plot_world_map(lons, lats, data):
    print("entered plot world map",flush=True)
    vmin = np.nanmin(data)
    vmax = np.nanmax(data)
    print("max min = ",vmin, vmax, flush=True)

    proj = ccrs.LambertConformal(central_longitude=-170, central_latitude=60., cutoff=25.)
    #proj = ccrs.NorthPolarStereo(true_scale_latitude=60.)
    #proj = ccrs.Stereographic(central_longitude=+170, central_latitude=60. )
    print("proj = ",proj,flush=True)

    ax = plt.axes(projection = proj)
    print("established ax",flush=True)
    fig = plt.figure(figsize=(640/50,480/50))
    print("established fig",flush=True)
    #fig = plt.figure( )
    ax = fig.add_subplot(1, 1, 1, projection = proj)
    print("add subplot",flush=True)

    #ax.set_extent([-100, 100, 30, 90], crs = proj)
    ax.set_extent((-220,-120, 40, 90), crs=ccrs.PlateCarree())
    print("set_extent",flush=True)

    ax.gridlines(crs=ccrs.PlateCarree(), xlocs=[-180, -170, -150, -120], ylocs=[40,50,60,66.6,70,80] )
    print("set_gridlines",flush=True)

    #'natural earth' -- coast only -- ax.coastlines(resolution='10m')
    #print("add_feature -- coastlines",flush=True)

    ax.add_feature(cfeature.GSHHSFeature(levels=[1,2,3,4], scale="l") )
    print("add_feature -- gshhs",flush=True)

    plt.savefig("hello1b.png")

    cbarlabel = '%s' % ("hello1")
    plttitle = 'Plot of variable %s' % ("hello2")
    plt.title(plttitle)

    plt.savefig("hello2.png")

    #Establish the color bar
    #colors=matplotlib.cm.get_cmap('jet')
    colors=matplotlib.cm.get_cmap('gray')
    colors=matplotlib.cm.get_cmap('terrain')
    #print("colors = ",colors)

    #cs = ax.pcolormesh(lons, lats, data,vmin=vmin,vmax=vmax,cmap=colors, transform=ccrs.PlateCarree() )
    cs = ax.pcolormesh(lons, lats, data,vmin=30.,vmax=vmax,cmap=colors, transform=ccrs.PlateCarree() )
    cb = plt.colorbar(cs, extend='both', orientation='horizontal', shrink=0.5, pad=.04)
    cb.set_label(cbarlabel, fontsize=12)

    plt.savefig("hello3.png")

    plt.close('all')

#----------------------------------------------------------------
lons = range(-360,360)
lats = range(-90,90)

data = np.zeros((len(lats),len(lons)))
for i in range(-360,360):
  data[:,i] = lats[:] 
print("set lats, lons, data",flush=True)

plot_world_map(lons, lats, data)
print("back in main",flush=True)
