import sys

import numpy as np
import numpy.ma as ma
import netCDF4 


#---------------------------------------------------------------------------
#Loop over input arg list (JRR-IceAge*)
#fname = "20220828/JRR-IceAge_v2r3_j01_s202208281036198_e202208281037426_c202208281059540.nc"

n = 0
nvalid = 0
totnp = 0
counts = np.zeros((301),dtype=int)

for fname in sys.argv:
  #debug: print(n, fname,flush=True)

  try:
    viirs = netCDF4.Dataset(fname, 'r')
  except:
    print("Could not open fname: ",fname,flush=True)
    continue

  n += 1
  np = viirs.variables['TotRetrPixs'][:]
  #debug: print("file ",n,"valid pixels ",np,flush=True)
  if (np == 0):
      #debug: print(fname," has no ice thickness information",flush=True)
      #exit(0)
      continue
  nvalid += 1
  totnp += np
  #This is a masked array, determined by fill value
  thick = viirs.variables['IceThickness'][:,:]
  print(n,np,"thick ",thick.max(), thick.min(),flush=True )

  #Geography:
  lats = viirs.variables['Latitude'][:,:]
  lons = viirs.variables['Longitude'][:,:]
  #debug: print("lats ",lats.max(), lats.min(), flush=True )
  #debug: print("lons ",lons.max(), lons.min(), flush=True )

  #QC:

  #Start Working:
  mask = ma.masked_array(thick)
  indices = mask.nonzero()
  #debug: print("len indices:",len(indices), len(indices[0]), flush=True)
  for k in range(0,len(indices[0])):
      i = indices[1][k]
      j = indices[0][k]
      #verbose: print(lons[j,i], lats[j,i], thick[j,i], " pt")
      #thickness histogram
      counts[int(thick[j,i]*100.+0.5)] += 1


print("number of files with valid ice thicknesses: ",nvalid)
print("total number of ice thicknesses: ",totnp)
for k in range(0,300):
    print(k,counts[k])

exit(0)

# Notes -------------------------------------------------------------

nx = len(viirs.dimensions['Columns'])
ny = len(viirs.dimensions['Rows'])
print("dimensions nx,ny: ",nx, ny)

#Accessing general info for file
print("resolution: ",viirs.resolution,flush=True)
#Accessing the fill value (a per-variable value):
print("fill?",viirs.variables['IceThickness']._FillValue)

age = viirs.variables['IceAge'][:,:]
print("age ",age.max(), age.min() )

exit(0)

#ncdump -h:

#netcdf JRR-IceAge_v2r3_j01_s202208281059048_e202208281100293_c202208281143240 {
#dimensions:
#	Columns = 3200 ;
#	Rows = 768 ;
#variables:
#	float Latitude(Rows, Columns) ;
#		Latitude:long_name = "Latitude" ;
#		Latitude:units = "degrees_north" ;
#		Latitude:comments = "Pixel latitude in field Latitude (degree)" ;
#		Latitude:_FillValue = -999.f ;
#		Latitude:valid_range = -90.f, 90.f ;
#	float Longitude(Rows, Columns) ;
#		Longitude:long_name = "Longitude" ;
#		Longitude:units = "degrees_east" ;
#		Longitude:comments = "Pixel longitude in field Longitude (degree)" ;
#		Longitude:_FillValue = -999.f ;
#		Longitude:valid_range = -180.f, 180.f ;
#	short IceAge(Rows, Columns) ;
#		IceAge:long_name = "Ice Age classification numbers(1-8)" ;
#		IceAge:coordinates = "Longitude Latitude" ;
#		IceAge:units = "1" ;
#		IceAge:_FillValue = -32768s ;
#		IceAge:valid_range = 0s, 8s ;
#	float IceThickness(Rows, Columns) ;
#		IceThickness:long_name = "Ice thickness" ;
#		IceThickness:coordinates = "Longitude Latitude" ;
#		IceThickness:units = "meter" ;
#		IceThickness:_FillValue = -999.f ;
#		IceThickness:valid_range = 0.f, 9999.f ;
#	short QCFlags(Rows, Columns) ;
#		QCFlags:long_name = "QC Flags" ;
#		QCFlags:coordinates = "Longitude Latitude" ;
#		QCFlags:units = "1" ;
#		QCFlags:_FillValue = -32768s ;
#	byte SummaryQC_Ice_Thickness_Age(Rows, Columns) ;
#		SummaryQC_Ice_Thickness_Age:long_name = "User-level summary QC: 0=Good, 1=Uncertain, 2=Bad, 3=Non-Retrievable" ;
#		SummaryQC_Ice_Thickness_Age:coordinates = "Longitude Latitude" ;
#		SummaryQC_Ice_Thickness_Age:units = "1" ;
#		SummaryQC_Ice_Thickness_Age:_FillValue = -128b ;
#		SummaryQC_Ice_Thickness_Age:valid_range = 0b, 3b ;
#	int ProdQualInfo(Rows, Columns) ;
#		ProdQualInfo:long_name = "Product qualiity information" ;
#		ProdQualInfo:coordinates = "Longitude Latitude" ;
#		ProdQualInfo:units = "1" ;
#		ProdQualInfo:_FillValue = -999 ;
#	int NumOfQACategories ;
#		NumOfQACategories:long_name = "Number of QA flag values" ;
#		NumOfQACategories:units = "1" ;
#	int Tot_QA_Normal ;
#		Tot_QA_Normal:long_name = "Cloud-free pixels with no glint and no cloud shadow" ;
#		Tot_QA_Normal:units = "1" ;
#	int Tot_QA_Uncertain ;
#		Tot_QA_Uncertain:long_name = "Probably cloud-free pixels with no glint and no cloud shadow" ;
#		Tot_QA_Uncertain:units = "1" ;
#	int Tot_QA_NonRetrievable ;
#		Tot_QA_NonRetrievable:long_name = "Not qualified to attempt retrieval" ;
#		Tot_QA_NonRetrievable:units = "1" ;
#	int Tot_QA_BadData ;
#		Tot_QA_BadData:long_name = "No real solution for thickness. Ice is too thick or too thin or missing" ;
#		Tot_QA_BadData:units = "1" ;
#	int TotWaterPixs ;
#		TotWaterPixs:long_name = "Total number of pixels w. water surface" ;
#		TotWaterPixs:units = "1" ;
#	int TotRetrPixs ;
#		TotRetrPixs:long_name = "Total number of valid ice thickness and age retrievals" ;
#		TotRetrPixs:units = "1" ;
#	float TermntPixPct ;
#		TermntPixPct:long_name = "% of terminated ice thickness and age retreivals of allprocessed pixels" ;
#		TermntPixPct:units = "%" ;
#		TermntPixPct:valid_range = 0.f, 100.f ;
#	int TotDaytimePixs ;
#		TotDaytimePixs:long_name = "Total number of daytime valid retreivals" ;
#		TotDaytimePixs:units = "1" ;
#	int TotNighttimePixs ;
#		TotNighttimePixs:long_name = "Total number of nighttime valid retreivals" ;
#		TotNighttimePixs:units = "1" ;
#	float MeanIceThk ;
#		MeanIceThk:long_name = "Mean ice thickness retrieval" ;
#		MeanIceThk:units = "meter" ;
#	float MaxIceThk ;
#		MaxIceThk:long_name = "Max ice thickness retrieval" ;
#		MaxIceThk:units = "meter" ;
#	float MinIceThk ;
#		MinIceThk:long_name = "Min ice thickness retrieval" ;
#		MinIceThk:units = "meter" ;
#	float STDIceThk ;
#		STDIceThk:long_name = "Standard deviation of ice thickness retrievals" ;
#		STDIceThk:units = "meter" ;
#
#// global attributes:
#		:Conventions = "CF-1.5" ;
#		:Metadata_Conventions = "CF-1.5, Unidata Dataset Discovery v1.0" ;
#		:standard_name_vocabulary = "CF Standard Name Table (version 17, 24 March 2011)" ;
#		:project = "JPSS Risk Reduction: Enterprise Ice Age/Thickness Products" ;
#		:institution = "DOC/NOAA/NESDIS/NDE > S-NPP Data Exploitation, NESDIS, NOAA, U.S. Department of Commerce" ;
#		:naming_authority = "gov.noaa.nesdis.nde" ;
#		:satellite_name = "NOAA-20" ;
#		:instrument_name = "VIIRS" ;
#		:title = "JRR-IceAge" ;
#		:summary = "Enterprise Ice Thickness/ Age Products" ;
#		:history = "JRR-IceAge v2r3" ;
#		:processing_level = "NOAA Level 2" ;
#		:references = "" ;
#		:id = "6459aec7-0e50-4dcf-bf80-3364100ad2d2" ;
#		:Metadata_Link = "JRR-IceAge_v2r3_j01_s202208281059048_e202208281100293_c202208281143240.nc" ;
#		:start_orbit_number = 24744 ;
#		:end_orbit_number = 24744 ;
#		:day_night_data_flag = "day" ;
#		:ascend_descend_data_flag = 0 ;
#		:time_coverage_start = "2022-08-28T10:59:04Z" ;
#		:time_coverage_end = "2022-08-28T11:00:29Z" ;
#		:date_created = "2022-08-28T11:43:24Z" ;
#		:channels_used = "none" ;
#		:channel_center_wavelength = "none" ;
#		:resolution = "750M" ;
#		:production_site = "NSOF" ;
#		:production_environment = "ITE" ;
#		:cdm_data_type = "swath" ;
#		:geospatial_first_scanline_first_fov_lat = -53.86364f ;
#		:geospatial_first_scanline_last_fov_lat = -61.74908f ;
#		:geospatial_last_scanline_first_fov_lat = -49.76255f ;
#		:geospatial_last_scanline_last_fov_lat = -56.7679f ;
#		:geospatial_first_scanline_first_fov_lon = 75.2131f ;
#		:geospatial_first_scanline_last_fov_lon = 23.18701f ;
#		:geospatial_last_scanline_first_fov_lon = 70.17741f ;
#		:geospatial_last_scanline_last_fov_lon = 24.00581f ;
#		:geospatial_lat_units = "degrees_north" ;
#		:geospatial_lon_units = "degrees_east" ;
#		:geospatial_bounds = "POLYGON((75.2131 -53.8636,23.187 -61.7491,24.0058 -56.7679,70.1774 -49.7626,75.2131 -53.8636))" ;
#}
