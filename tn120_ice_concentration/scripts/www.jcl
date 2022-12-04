#QSUB -o  /wd2/wd21/wd21rg/ssmi.getout
#QSUB -lT 150
#QSUB -lM   8.0Mw
#QSUB -me

#Script for handling the production and dissemination of all ssmi-derived
#  sea ice concentration files
#Bob Grumbine 1 March 1995.
#Modified for producing reanalysis files 28 November 1995.  Bob Grumbine

SDIR=/wd2/wd21/wd21rg/execs
ADIR=/dm/wd21rg/ssmi/fifteenth
CDIR=/wd2/wd21/wd21rg/control
RDIR=/marine/ice/ice.analy
RDIR2=/data/forecasts/ice.analy

export SDIR ADIR CDIR RDIR RDIR2

########################################
#Create the gif files for home page use
rcp nh.xpm polar:/data/WWW/seaice/analysis/nh.xpm
rcp sh.xpm polar:/data/WWW/seaice/analysis/sh.xpm

