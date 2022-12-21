Contents of the ftp directory: -- Updated 21 March 2007

  ICE CONCENTRATION GRAPHICS
cbar.gif : Index of the color to concentration relationship
nh.gif   : gif of most recent sea ice concentration field - in the north
nh12.gif :     as above, but at 12.7 km resolution instead of 25.4
ak.gif   : gif of most recent sea ice concentration field - in the Alaska Region
sh.gif   : gif of most recent sea ice concentration field - in the south
sh12.gif :     as above, but at 12.7 km resolution instead of 25.4
global.gif    : 0.5 degree lat-long grid sea ice for modellers
global5min.gif: 5 arc-minute lat-long grid sea ice for modellers 


  ICE CONCENTRATION DATA
northpsg.yyyymmdd : 25.4 km Grib file of northern hemisphere ice concentration.
southpsg.yyyymmdd : 25.4 km Grib file of southern hemisphere ice concentration. 
north12psg.yyyymmdd : 12.7 km Grib file of northern hemisphere ice concentration.
south12psg.yyyymmdd : 12.7 km Grib file of southern hemisphere ice concentration. 
../cdas/eng.yyyymmdd : 30 minute lat-long grib file of global ice for modellers
../cdas/eng5min.yyyymmdd : 5 minute lat-long grib file of global ice for modellers

  ICE DRIFT MODEL OUTPUT -- Is available from the operational server
ftpprd.ncep.noaa.gov.  
Please see http://polar.ncep.noaa.gov/mmab/translation.shtml for details
drift.out: Most recent ice drift model output.  
ak.out   : Most recent Alaska Region drift model output


../wgrib/: Wes Ebisuzaki's grib reader.  Most current version is at 
        wesley.ncep.noaa.gov.  pub/wgrib.

archive/  : Directory with a monthly archive of ice concentrations.
  These are the grib'bed daily fields (northpsg, southpsg) that
  have been concatenated and unix compressed.

nh/		: Northern hemisphere ice concentration figures.  
sh/		: Southern hemisphere ice concentration figures.
alaska/		: Alaska Region ice concentration figures.
ross/		: Ross Sea ice concentration figures.
amery/		: Amery Sea ice concentration figures.
weddell/	: Weddell Sea ice concentration figures.
All have subdirectories by year, 1998-previous year 


Land masks were updated 19 May 2004
support/  : Directory with various support files for users of the 
  ice concentration data fields.  This includes:
ssmi.corners   : Specifications of the sea ice concentration gridding.
seaice_nland.map      : Raster map, 385x465, of 25.4 km northern hemisphere land mask.
seaice_sland.map      : Raster map, 345x355, of 25.4 km southern hemisphere land mask. 
seaice_newland    : Raster map, 720x360, of global 0.5 degree land mask.
seaice_gland5min  : Raster map, 4320x2160 of global 5 minute land mask.
seaice_sland127.map   : Raster map of 12.7 km southern hemisphere land mask.
seaice_nland127.map   : Raster map of 12.7 km northern hemisphere land mask.
onedeg.map     : Raster map, 360x180, of global land mask -- for old (pre-10/31/1995)

publications/  : Technical notes regarding the drift model, ssmi ice concentration
   analysis, and sea ice model.

  Effective 5 June 1997 the northpsg and southpsg files went to a range
of 0 to 2.24 (formerly 0 to 224) to accord with GRIB standards and the
form of the eng files.  The archives will be brought in line with the
standard when time permits.
