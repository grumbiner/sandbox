DSET  allest
options yref
#options sequential
UNDEF -999
TITLE mostly ignored, but include anyhow
# Lat long 1/2, 1/4, 1/12, respectively
XDEF  720 LINEAR  0.25  0.5
XDEF 1440 LINEAR  0.125 0.25
XDEF 4320 LINEAR  0.041666666666 0.083333333333333
YDEF  360 LINEAR -89.75  0.5
YDEF  720 LINEAR -89.875 0.25
YDEF 2160 LINEAR -89.958333333333 0.083333333333333
#2d fields don't need to worry
ZDEF  1 LINEAR   1 1
#Number of time steps and the interval
TDEF  34 LINEAR  21sep00 1dy

#Now for specifics about the data:
VARS 1
sst 0 254 255
ENDVARS
#others:
icec       0 -1,40,1  ice concentration (0 - 100)
