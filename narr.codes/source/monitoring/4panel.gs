*
'page'
'c'
'setup'
'set rbcols 48 47 46 45 44 43 42 41 81 21 22 23 24 25 26 27 28'
'q dim'

*get date
line=sublin(result,5)
date=subwrd(line,6)
say date

'map rr'

* single maps

'c'
rh700(date)
'printim rh700.1p.png x400 y300'

'c'
mslp2(date)
'printim mslp.1p.png x400 y300'

'c'
z500_rvort(date)
'printim z500_rvort.1p.png x400 y300'

'c'
z500_avort(date)
'printim z500_avort.1p.png x400 y300'

'c'
z300(date)
'printim z300.1p.png x400 y300'

'c'
z250(date)
'printim z250.1p.png x400 y300'

'c'
precip(date)
'printim prcp.1p.png x400 y300'

'c'
z700(date)
'printim z700.1p.png x400 y300'

'c'
sst(date)
'printim tmpsfc.1p.png x400 y300'

'c'
snow(date)
'printim snow_h2o.1p.png x400 y300'

'c'
snowdepth(date)
'printim snowd.1p.png x400 y300'

'c'
albedo(date)
'printim albedo.1p.png x400 y300'

'c'
tbot(date)
'printim tbot.1p.png x400 y300'

* 4 panel maps

'page'
'c'
'page q1'
mslp2(date)
'page q2'
z500_avort(date)
'page q3'
z300(date)
'page q4'
precip(date)
'printim page1.png x800 y600'

'page'
'c'
'page q1'
z700(date)
'page q2'
sst(date)
'page q3'
tbot(date)
'page q4'
* tcdc(date)
snowdepth(date)
'printim page2.png x800 y600'

'page'
'c'
'page q1'
lcdclcl(date)
'page q2'
mcdcmcl(date)
'page q3'
hcdchcl(date)
'page q4'
tcdc(date)
* tmpclt(date)
* ulwrftoa(date)
'printim clouds.4p.png x800 y600'

'page'
'c'
'page q1'
precip(date)
'page q2'
convect_precip(date)
'page q3'
ncon_precip(date)
'page q4'
ulwrftoa(date)
'printim precip.4p.png x800 y600'

'page'
'c'
'page q1'
cape(date)
'page q2'
lifted_index(date)
'page q3'
cin_sfc(date)
'page q4'
ht_pbl(date)
'printim instability.4p.png x800 y600'

'page'
'c'
'page q1'
wv_clm(date)
'page q2'
wv_700(date)
'page q3'
wc_clm(date)
'page q4'
wc_700(date)
'printim water_flux.4p.png x800 y600'

'page'
'c'
'page q1'
tmp2m(date)
'page q2'
tmp850(date)
'page q3'
thickness(date)
'page q4'
hgt0deg(date)
'printim freezing.4p.png x800 y600'

'page'
'c'
'page q1'
tsoil1(date)
'page q2'
tsoil2(date)
'page q3'
tsoil3(date)
'page q4'
tsoil4(date)
'printim tsoil.4p.png x800 y600'


'page'
'c'
'page q1'
soilw1(date)
'page q2'
soilw2(date)
'page q3'
soilw3(date)
'page q4'
soilw4(date)
'printim soilw.4p.png x800 y600'

'page'
'c'
'page q1'
fracsoill1(date)
'page q2'
fracsoill2(date)
'page q3'
fracsoill3(date)
'page q4'
fracsoill4(date)
'printim soill.4p.png x800 y600'

'page'
'c'
'page q1'
snow(date)
'page q2'
snowmelt(date)
'page q3'
runoff(date)
'page q4'
gndwater(date)
'printim runoff.4p.png x800 y600'

'page'
'c'
'page q1'
veg_frac(date)
'page q2'
evp(date)
'page q3'
soilm(date)
'page q4'
mstav(date)
'printim veg.4p.png x800 y600'

'page'
'c'
'page q1'
dswrfsfc(date)
'page q2'
dlwrfsfc(date)
'page q3'
netradflx(date)
'page q4'
ulwrfsfc(date)
'printim rad_flux.4p.png x800 y600'

'page'
'c'
'page q1'
shtflsfc(date)
'page q2'
lhtflsfc(date)
'page q3'
potevap(date)
'page q4'
gflux(date)
'printim sfc_flux.4p.png x800 y600'

'page'
'c'
'page q1'
tmp2m(date)
'page q2'
rh2m(date)
'page q3'
wind10m(date)
'page q4'
tsfc_t2m(date)
'printim sfc_cond.4p.png x800 y600'

'page'
'c'
'page q1'
csnow(date)
'page q2'
crain(date)
'page q3'
cfrzr(date)
'page q4'
cicep(date)
'printim precip_cat.4p.png x800 y600'

'page'
'c'
'page q1'
prestrp(date)
'page q2'
tmptrp(date)
'page q3'
windtrp(date)
'page q4'
vwshtrp(date)
'printim trop.4p.png x800 y600'

exit

function prestrp(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 30 270'
'd prestrp/100'
'cbar98'
'draw title Trop pressure [mb] ' date
return

function tmptrp(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 5 220'
'd tmptrp'
'cbar98'
'draw title Trop temperature [K] ' date
return

function windtrp(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 5 35'
'set arrlab off'
'd mag(ugrdtrp,vgrdtrp)'
'cbar98'
'set ccolor 98'
'd ugrdtrp;skip(vgrdtrp,10)'
'draw title Trop winds [m/s] ' date
return

function vwshtrp(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 0.002 0.014'
'd vwshtrp'
'cbar98'
'draw title Trop vert wind shear [1/s] ' date
return

function cfrzr(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  0.5'
'set ccols  99 45'
'd cfrzrsfc.2'
'draw title Freezing rain @ 3hr [0/1] ' date
return

function cicep(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  0.5'
'set ccols  99 45'
'd cicepsfc.2'
'draw title Ice pellets @ 3hr [0/1] ' date
return

function crain(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  0.5'
'set ccols  99 45'
'd crainsfc.2'
'draw title Rain  @ 3hr [0/1] ' date
return

function csnow(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  0.5'
'set ccols  99 45'
'd csnowsfc.2'
'draw title Snow @ 3hr [0/1] ' date
return

function wind10m(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 2.5 17.5'
'set arrlab off'
'd mag(ugrd10m,vgrd10m)'
'cbar98'
'set ccolor 98'
'd ugrd10m;skip(vgrd10m,10)'
'draw title 10m winds [m/s] ' date
return

function tsfc_t2m(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 2 0'
'd tmpsfc - tmp2m'
'cbar98'
'draw title TMPsfc - TMP2m [K] ' date
return

function rh2m(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs   10 20 30 40 50 60 70 80 90'
'set ccols   29 26 23 21 99 41 43 45 47 49'
'd rh2m'
'cbar98'
'draw title RH 2m [%] ' date
return

function mstav(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs   2 5 10 15 20 25 30 40 50 60 70 80 90'
'set ccols   84 27 25 23 21 99 33 36 38 41 43 45 47 49'
'd mstav0_100cm'
'cbar98'
'draw title soil moisture availability [% sat] ' date
return

function gflux(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 30 0'
'd gfluxsfc.2'
'cbar98'
'draw title ground flux  [W/m/m] ' date
return

function netflx(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 50 0'
'd uswrfsfc.2+ulwrfsfc.2-dswrfsfc.2-dlwrfsfc.2-lhtflsfc.2-shtflsfc.2'
'cbar98'
'draw title Net Sfc Flux [W/m/m] ' date
return

function netradflx(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
* 'defint 50 0'
'set clevs  -300 -250 -200 -150 -100 -50 50 100 150 200 250 300'
'set ccols  29 27 25 24 23 22 99 42 43 44 45 47 49'
'd uswrfsfc.2+ulwrfsfc.2-dswrfsfc.2-dlwrfsfc.2'
'cbar98'
'draw title Net Rad Flux [W/m/m] ' date
return

function potevap(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
* 'defint 100 500'
'set clevs  -100 -50 0 50 100 150 200 250 300 350 400 450 500'
'set ccols  76 74 72 41 43 45 47 49 99 22 24 25 27 29'
'd  pevapsfc.2/0.03456*8'
'cbar98'
'draw title pot evap [W/m/m] ' date
return

function lhtflsfc(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -100 -50 0 50 100 150 200 250 300 350 400 450 500'
'set ccols  76 74 72 41 43 45 47 49 99 22 24 25 27 29'
'd -lhtflsfc.2'
'cbar98'
'draw title LHTFL [W/m/m] ' date
return

function shtflsfc(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -100 -50 0 50 100 150 200 250 300 350 400 450 500'
'set ccols  76 74 72 41 43 45 47 49 99 22 24 25 27 29'
'd -shtflsfc.2'
'cbar98'
'draw title SHTFL [W/m/m] ' date
return


function ulwrftoa(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 50 350'
'd ulwrftoa.2'
'cbar98'
'draw title ULWRFtoa [W/m/m] ' date
return

function ulwrfsfc(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 50 350'
'd ulwrfsfc.2'
'cbar98'
'draw title ULWRFsfc [W/m/m] ' date
return

function uswrfsfc(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 50 350'
'd uswrfsfc.2'
'cbar98'
'draw title USWRFsfc [W/m/m] ' date
return

function dlwrfsfc(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 50 350'
'd dlwrfsfc.2'
'cbar98'
'draw title DLWRFsfc [W/m/m] ' date
return

function dswrfsfc(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 100 700'
'd dswrfsfc.2'
'cbar98'
'draw title DSWRFsfc [W/m/m] ' date
return

function evp(date)

'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'defint 0.1 .9'
'set clevs  0.01 0.02 0.05 0.1 0.15 0.2 0.3 0.4 0.5 0.8  1 1.5  '
'set clevs  0.01 0.02 0.05 0.1 0.2 0.5 1  2 5 10 '
'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'

'd evpsfc.2'
'cbar98'
'draw title Evp [mm] ' date
return

function soilm(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs   100 200 300 400 500 600 700 800 900'
'set ccols   49 46 43 41 99 21 23 25 27 29'
'd soilm0_200cm'
'cbar98'
'draw title soilm 0-200 cm [mm] ' date
return

function veg_frac(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs   10 20 30 40 50 60 70 80 90'
'set ccols   79 76 73 71 99 31 33 35 37 39'
'd vegsfc'
'cbar98'
'draw title veg fraction [%] ' date
return

function soilw0_1m(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs   5 10 15 20 25 30 35 40 45 50 '
'set ccols   49 47 45 43 41 99 21 23 25 27 29'
'd (0.1*SOILW0_10cm+0.3*SOILW10_40cm+0.6*SOILW40_100cm)*100'
'cbar98'
'draw title soilw 0-1m [%] ' date
return

function thickness(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout contour'
'set cint 100'
'd hgtprs(lev=500)-hgtprs(lev=1000)'
'set ccolor 2'
'set cint 5400'
'd hgtprs(lev=500)-hgtprs(lev=1000)'
'draw title Z500-Z1000 [m] ' date
return

function runoff(date)
'set grads off'
'set gxout shaded'
'set csmooth on'
'set clevs  0 0.5 1 1.5 2 2.5 3 4 6 8'
'set ccols  99 42 44 46 49 21 23 24 25 27 29 88'
'd ssrunsfc.2'
'cbar98'
'draw title surface runoff [mm] ' date
return

function gndwater(date)
'set grads off'
'set gxout shaded'
'set csmooth on'
'set clevs  0 0.5 1 1.5 2 2.5 3 4 6 8'
'set ccols  99 42 44 46 49 21 23 24 25 27 29 88'
'd bgrunsfc.2'
'cbar98'
'draw title subsurface runoff [mm] ' date
return


function tbot(date)
'set grads off'
' defint 5 275'
'set gxout shaded'
'set csmooth on'
'd tsoildpl'
'cbar98'
'draw title Tbot [K] ' date
return

function snowmelt(date)
'set grads off'
'set gxout shaded'
'set csmooth on'
'set clevs  0 0.5 1 1.5 2 2.5 3 4 6 8'
'set ccols  99 42 44 46 49 21 23 24 25 27 29 88'
'd SNOMsfc.2'
'cbar98'
'draw title Snow Melt [mm] ' date
return


function fracsoill1(date)
'set grads off'
'set gxout shaded'
'set csmooth on'
'set clevs   0 10 20 30 40 50 60 70 80 90'
'set ccols   99 22 24 25 27 29 41 43 45 47 49'
'd (soilw0_10cm - soill0_10cm)/soilw0_10cm*100'
'cbar98'
'draw title % frozen soilw 0-10 cm ' date
return

function fracsoill2(date)
'set grads off'
'set gxout shaded'
'set csmooth on'
'set clevs   0 10 20 30 40 50 60 70 80 90'
'set ccols   99 22 24 25 27 29 41 43 45 47 49'
'd (soilw10_40cm - soill10_40cm)/soilw10_40cm*100'
'cbar98'
'draw title % frozen soilw 10-40 cm ' date
return

function fracsoill3(date)
'set grads off'
'set gxout shaded'
'set csmooth on'
'set clevs   0 10 20 30 40 50 60 70 80 90'
'set ccols   99 22 24 25 27 29 41 43 45 47 49'
'd (soilw40_100cm - soill40_100cm)/soilw40_100cm*100'
'cbar98'
'draw title % frozen soilw 40-100 cm ' date
return

function fracsoill4(date)
'set grads off'
'set gxout shaded'
'set csmooth on'
'set clevs   0 10 20 30 40 50 60 70 80 90'
'set ccols   99 22 24 25 27 29 41 43 45 47 49'
'd (soilw100_200cm - soill100_200cm)/soilw100_200cm*100'
'cbar98'
'draw title % frozen soilw 100-200 cm ' date
return

function soill1(date)
'set grads off'
'set clevs  0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65'
'set ccols  29 27 25 24 23 22 99 42 43 44 45 47 49'
'set gxout shaded'
'set csmooth on'
'd soill0_10cm'
'cbar98'
'draw title SOILL (liq) 0-10cm [frac] ' date
return

function soill2(date)
'set grads off'
'set clevs  0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65'
'set ccols  29 27 25 24 23 22 99 42 43 44 45 47 49'
'set gxout shaded'
'set csmooth on'
'd soill10_40cm'
'cbar98'
'draw title SOILL (liq) 10-40cm [frac] ' date
return

function soill3(date)
'set grads off'
'set clevs  0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65'
'set ccols  29 27 25 24 23 22 99 42 43 44 45 47 49'
'set gxout shaded'
'set csmooth on'
'd soill40_100cm'
'cbar98'
'draw title SOILL (liq) 40-100cm [frac] ' date
return

function soill4(date)
'set grads off'
'set clevs  0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65'
'set ccols  29 27 25 24 23 22 99 42 43 44 45 47 49'
'set gxout shaded'
'set csmooth on'
'd soill100_200cm'
'cbar98'
'draw title SOILL (liq) 100-200cm [frac] ' date
return

function soilw1(date)
'set grads off'
'set clevs  0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65'
'set ccols  29 27 25 24 23 22 99 42 43 44 45 47 49'
'set gxout shaded'
'set csmooth on'
'd soilw0_10cm'
'cbar98'
'draw title SOILW 0-10cm [frac] ' date
return

function soilw2(date)
'set grads off'
'set clevs  0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65'
'set ccols  29 27 25 24 23 22 99 42 43 44 45 47 49'
'set gxout shaded'
'set csmooth on'
'd soilw10_40cm'
'cbar98'
'draw title SOILW 10-40cm [frac] ' date
return

function soilw3(date)
'set grads off'
'set clevs  0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65'
'set ccols  29 27 25 24 23 22 99 42 43 44 45 47 49'
'set gxout shaded'
'set csmooth on'
'd soilw40_100cm'
'cbar98'
'draw title SOILW 40-100cm [frac] ' date
return

function soilw4(date)
'set grads off'
'set clevs  0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65'
'set ccols  29 27 25 24 23 22 99 42 43 44 45 47 49'
'set gxout shaded'
'set csmooth on'
'd soilw100_200cm'
'cbar98'
'draw title SOILW 100-200cm [frac] ' date
return

function tsoil1(date)
'set grads off'
' defint 5 280'
'set gxout shaded'
'set csmooth on'
'd tsoil0_10cm'
'cbar98'
'draw title TSOIL 0-10cm [K] ' date
return

function tsoil2(date)
'set grads off'
' defint 5 280'
'set gxout shaded'
'set csmooth on'
'd tsoil10_40cm'
'cbar98'
'draw title TSOIL 10-40cm [K] ' date
return

function tsoil3(date)
'set grads off'
' defint 5 280'
'set gxout shaded'
'set csmooth on'
'd tsoil40_100cm'
'cbar98'
'draw title TSOIL 40-100cm [K] ' date
return

function tsoil4(date)
'set grads off'
' defint 5 280'
'set gxout shaded'
'set csmooth on'
'd tsoil100_200cm'
'cbar98'
'draw title TSOIL 100-200cm [K] ' date
return

function tmp2m(date)
'set grads off'
'set clevs  -25 -20 -15 -10 -5 0 5 10 15 20 25 30'
'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'
'set gxout shaded'
'set csmooth on'
'd tmp2m-273.15'
'cbar98'
'set gxout contour'
'set ccolor 2'
'set cint 100'
'd tmp2m-273.15'
'draw title TMP2m [C] ' date
return

function tmp850(date)
'set grads off'
'set clevs  -25 -20 -15 -10 -5 0 5 10 15 20 25 30'
'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'
'set gxout shaded'
'set csmooth on'
'd tmpprs(lev=850)-273.15'
'cbar98'
'set gxout contour'
'set ccolor 2'
'set cint 100'
'd tmpprs(lev=850)-273.15'
'draw title TMP850 [C] ' date
return

function hgt0deg(date)
'set grads off'
'defint 250 1750'
'set gxout shaded'
'set csmooth on'
'd hgt0deg'
'cbar98'
'draw title lowest 0C level above msl [m] ' date
return

function wv_clm(date)
'set grads off'
* 'defint 50000 0'
'set clevs -300000  -200000 -150000 -100000 -50000 -25000 25000 50000 100000 150000 200000 300000'
'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'

'set gxout shaded'
'set csmooth on'
'd WVCONVclm.2'
'cbar98'
'set ccolor 98'
'set arrlab off'
'd wvuflxclm.2;skip(wvvflxclm.2,9)'
'draw title WVCONVclm [kg/m^2/s] ' date
return

function wv_700(date)
'set grads off'
* 'defint 25000 0'
'set clevs -150000 -100000 -75000 -50000 -25000 -10000 10000 25000 50000 75000 100000 150000'
'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'

'set gxout shaded'
'set csmooth on'
'd WVCONVplr.2'
'cbar98'
'set ccolor 98'
'set arrlab off'
'd wvuflxplr.2;skip(wvvflxplr.2,9)'
'draw title WVCONV 0-700 hPa [kg/m^2/s] ' date
return

function wc_clm(date)
'set grads off'
* 'defint 5000 0'
'set clevs  -30000 -20000 -15000 -10000 -5000 -2500 2500 5000 10000 15000 20000 30000'
'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'

'set gxout shaded'
'set csmooth on'
'd WCCONVclm.2'
'cbar98'
'set ccolor 98'
'set arrlab off'
'd wcuflxclm.2;skip(wcvflxclm.2,9)'
'draw title WCCONVclm [kg/m^2/s] ' date
return

function wc_700(date)
'set grads off'
'set clevs  -15000 -10000 -7500 -5000 -2500 -1000 1000 2500 5000 7500 10000 15000'
'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'


'set gxout shaded'
'set csmooth on'
'd WCCONVplr.2'
'cbar98'
'set ccolor 98'
'set arrlab off'
'd wcuflxplr.2;skip(wcvflxplr.2,9)'
'draw title WCCONV 0-700 hPa [kg/m^2/s] ' date
return


function ht_pbl(date)
'set grads off'
'set clevs  300 600 900 1200 1500 1800 2100 2400 2700 3000 3300 3600 3900 4200'
'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'
'set gxout shaded'
'set csmooth on'
'd HPBLsfc'
'cbar98'
'draw title PBL depth [m] ' date
return

function cin_sfc(date)
'set grads off'
'set clevs -400 -350 -300 -250 -200 -150 -100 -50 -25'
'set ccols  83 29 27 24 22 99 42 44 47 49 '
'set gxout shaded'
'set csmooth on'
'd cinsfc'
'cbar98'
'draw title surface Convective inhibition [J/kg] ' date
return


function cape(date)
'set grads off'
'set clevs 50 500 1000 1500 2000 2500 3000 3500 4000 4500 5000'
'set ccols  49 47 45 44 43 42 99 22 23 24 25 27 29'
'set gxout shaded'
'set csmooth on'
'd capesfc'
'cbar98'
'draw title surface CAPE [J/kg] ' date
return

function lifted_index(date)
'set grads off'
* 'defint 3 -3'
'set clevs  -10 -8 -6 -4 -2 0 16'
'set ccols  88 29 26 24 21 99 42 45 47 49'
'set gxout shaded'
'set csmooth on'
'd lftx500_1000mb - 273.15'
'cbar98'
'draw title 1000-500 lifted index [K] ' date
return

function tcdc(date)
'set clevs 10 20 40 60 80 90'
'set ccols  49 47 45 44 43 42 99'
'set grads off'
'set gxout shaded'
'set csmooth on'
'd tcdcclm.2'
'cbar98'
'draw title total cloud cover ' date
return

function hcdchcl(date)
'set clevs 10 20 40 60 80 90'
'set ccols  49 47 45 44 43 42 99'
'set grads off'
'set gxout shaded'
'set csmooth on'
'd HCDChcl.2'
'cbar98'
'draw title high cloud cover ' date
return

function mcdcmcl(date)
'set clevs 10 20 40 60 80 90'
'set ccols  49 47 45 44 43 42 99'
'set grads off'
'set gxout shaded'
'set csmooth on'
'd mcdcmcl.2'
'cbar98'
'draw title middle cloud cover ' date
return

function lcdclcl(date)
'set clevs 10 20 40 60 80 90'
'set ccols  49 47 45 44 43 42 99'
'set grads off'
'set gxout shaded'
'set csmooth on'
'd lcdclcl.2'
'cbar98'
'draw title low cloud cover ' date
return

function tmpclt(date)
'set grads off'
'defint 5 285'
'set gxout shaded'
'set csmooth on'
'd tmpclt.2'
'cbar98'
'draw title cloud top temp ' date
return



function rh700(date)
'set ccolor 98'
'set clevs  -3 -2 -1.5 -1 -0.5 -0.25 0.25 0.5 1 1.5 2 3'
'set ccols  29 27 25 24 23 22 99 42 43 44 45 47 49'
'set grads off'
'set lev 700'
'set gxout shaded'
'set cterp off'
'set csmooth on'
spfh2rh(700,TMPprs,SPFHprs)
'd vvelprs'
'cbar98'
'set gxout contour'
'set cthick 5'
'set ccolor 98'
'set cint 20'
'd rh2'
'draw title RH and omega at 700 mb ' date
return


function precip(date)
'set grads off'
'set gxout shaded'
'set cterp off'
'set clevs 0 0.5 1 2 4 6 8 12 15 20'
'set ccols 99 42 44 46 48 21 23 25 27 88'
'd APCPsfc.2'
'cbar98'
'draw title PRECIP in 3 hours [mm] ' date
return

function convect_precip(date)
'set grads off'
'set gxout shaded'
'set cterp off'
'set clevs 0 0.5 1 2 4 6 8 12 15 20'
'set ccols 99 42 44 46 48 21 23 25 27 88'
'd ACPCPsfc.2'
'cbar98'
'draw title convective PRECIP in 3 hours [mm] ' date
return


function ncon_precip(date)
'set grads off'
'set gxout shaded'
'set cterp off'
'set clevs 0 0.5 1 2 4 6 8 12 15 20'
'set ccols 99 42 44 46 48 21 23 25 27 88'
'd APCPsfc.2-ACPCPsfc.2'
'cbar98'
'draw title non-convective PRECIP in 3 hours [mm] ' date
return

function convect_precip(date)

function mslp(date)
'set ccolor 98'
'set grads off'
'set lev 1000'
'set gxout contour'
'set csmooth on'
'set cthick 5'
'defint 4 1008'
'd MSLETmsl/100'
'set cthick 4'
'set ccolor 98'
'set cint 50'
'd HGTprs(lev=500)-HGTprs(lev=1000)'
'set cthick 8'
'set ccolor 59'
'set cint 5400'
'd HGTprs(lev=500)-HGTprs(lev=1000)'
'draw title MSLP and 1000-500 thickness ' date
say 'date=' date
return

function mslp2(date)
'set ccolor 98'
'set grads off'
'set lev 1000'
'set gxout shaded'
'set cterp off'
'defint 100 5500'
'd HGTprs(lev=500)-HGTprs(lev=1000)'
'cbar98'
'set gxout contour'
'set csmooth on'
'set cthick 5'
'set ccolor 98'
'set cint 5'
'd MSLETmsl/100'
'set cthick 8'
'set ccolor 59'
'set cint 5400'
'd HGTprs(lev=500)-HGTprs(lev=1000)'
'draw title MSLP and 1000-500 thickness ' date
return

function z500_rvort(date)
'set ccolor 98'
'set grads off'
'set lev 500'
'set gxout shaded'
'set cterp off'
'set csmooth on'
'defint 50 0'
'd hcurl(UGRDprs,VGRDprs)*1000000'
'cbar98'
'set gxout contour'
'set cthick 5'
'set ccolor 98'
'd HGTprs'
'draw title Height and Rel Vort 500 mb ' date
return

function z500_avort(date)
'set ccolor 98'
'set grads off'
'set lev 500'
'set gxout shaded'
'set cterp off'
'set csmooth on'
'defint 50 50'
'd (hcurl(UGRDprs,VGRDprs)+sin(lat*3.141592/180)/86400*4*3.141592)*1000000'
'cbar98'
'set gxout contour'
'set cthick 5'
'set ccolor 98'
'd HGTprs'
'draw title Height and Abs Vort 500 mb ' date
return

function spfh2rh(p,t,q)
*
* p in hP
* t in K
* q in kg/kg
*  conversion as used by eta model
'define rh2=10000*'p'*'q'/379.90516/exp(17.2693882*('t'-273.16)/('t'-35.86))'
* set all rh < 1 to 1
'define rh3=maskout(rh2,rh2-1)'
'define rh3=const(rh3,1,-u)'
* set all rh > 100 to 100
'define rh3=maskout(rh3,100-rh3)'
'define rh3=const(rh3,100,-u)'
* keep domain the same
'define rh2=rh3*rh2/rh2'

return

function z700(date)
'set ccolor 98'
'set clevs  10 20 30 40 50 60 70 80 90'
'set ccols   29 26 23 21 99 41 43 45 47 49'
'set grads off'
'set lev 700'
'set gxout shaded'
'set cterp off'
'set cint 0'
'set csmooth on'
spfh2rh(700,TMPprs,SPFHprs)
'd rh2'
'cbar98'
'set gxout contour'
'set cthick 5'
'set ccolor 98'
'd HGTprs'
'draw title Height and RH at 700 mb ' date
return

function sst(date)
'set grads off'
'set ccolor 98'
'set gxout shaded'
'set cterp off'
'defint 5 15'
'd tmpsfc-273.16'
'set gxout contour'
'set ccolor 98'
'set cint 100'
'd (TMPsfc-273.16)'
'cbar98'
'draw title surface temp [C] ' date
return

function snow(date)
'set ccolor 98'
'set grads off'
'set gxout shaded'
'set cterp off'
'set csmooth off'
'set ccols 99 78 77 76 75 74 73 72 21 81 31 32 33 34 35 36 37 38'
'set clevs 0 1 2 4 8 12 16 22 25 50 75 100 150 200 '
'd WEASDsfc.2'
'cbar98'
'draw title Snow Depth [mm H2O] ' date
return

function snowdepth(date)
'set ccolor 98'
'set grads off'
'set gxout shaded'
'set cterp off'
'set csmooth off'
'set ccols 99 78 77 76 75 74 73 72 21 31 32 33 34 35 36 37 38'
'set clevs 0 0.001 0.002 0.005 0.01 0.02  0.04  0.1  0.2  0.4  1.0 4 10 '
'd SNODsfc'
'cbar98'
'draw title Snow Depth [m] ' date
return

function z300(date)
'set ccolor 98'
'set grads off'
'set lev 300'
'set gxout shaded'
'set cterp off'
'defintplus 10'
'd mag(UGRDprs,VGRDprs)'
'cbar98'
'set gxout contour'
'set cthick 5'
'set ccolor 98'
'd HGTprs'
'set ccolor 98'
'set arrlab off'
'd UGRDprs;skip(VGRDprs,10)'
'draw title 300 mb height, isotachs [m/s] ' date
return

function z250(date)
'set ccolor 98'
'set grads off'
'set lev 250'
'set gxout shaded'
'set cterp off'
'defintplus 10'
'd mag(UGRDprs,VGRDprs)'
'cbar98'
'set gxout contour'
'set cthick 5'
'set ccolor 98'
'd HGTprs'
'set ccolor 98'
'set arrlab off'
'd UGRDprs;skip(VGRDprs,10)'
'draw title 250 mb height, isotachs [m/s] ' date
return

function albedo(date)
'set ccolor 98'
'set grads off'
'set gxout shaded'
'set cterp off'
'set clevs  10 20 30 40 50 60 70 80'
'set ccols  49 47 45 43 42 86 84 82 99'
'd ALBDOsfc'
'cbar98'
'draw title albedo ' date
return

function spfh2rh(p,t,q)
*
* p in hP
* t in K
* q in kg/kg
*  conversion as used by eta model
'define rh2=10000*'p'*'q'/379.90516/exp(17.2693882*('t'-273.16)/('t'-35.86))'
* set all rh < 1 to 1
'define rh3=maskout(rh2,rh2-1)'
'define rh3=const(rh3,1,-u)'
* set all rh > 100 to 100
'define rh3=maskout(rh3,100-rh3)'
'define rh3=const(rh3,100,-u)'
* keep domain the same
'define rh2=rh3*rh2/rh2'

return

function z700(date)
'set ccolor 98'
'set clevs  10 20 30 40 50 60 70 80 90'
'set ccols   29 26 23 21 99 41 43 45 47 49'
'set grads off'
'set lev 700'
'set gxout shaded'
'set cterp off'
'set cint 0'
'set csmooth on'
spfh2rh(700,TMPprs,SPFHprs)
'd rh2'
'cbar98'
'set gxout contour'
'set cthick 5'
'set ccolor 98'
'd HGTprs'
'draw title Height and RH at 700 mb ' date
return

function sst(date)
'set grads off'
'set ccolor 98'
'set gxout shaded'
'set cterp off'
'defint 5 15'
'd tmpsfc-273.16'
'set gxout contour'
'set ccolor 98'
'set cint 100'
'd (TMPsfc-273.16)'
'cbar98'
'draw title surface temp [C] ' date
return

function snow(date)
'set ccolor 98'
'set grads off'
'set gxout shaded'
'set cterp off'
'set csmooth off'
'set ccols 99 78 77 76 75 74 73 72 21 81 31 32 33 34 35 36 37 38'
'set clevs 0 1 2 4 8 12 16 22 25 50 75 100 150 200 '
'd WEASDsfc.2'
'cbar98'
'draw title Snow Depth [mm H2O] ' date
return

function snowdepth(date)
'set ccolor 98'
'set grads off'
'set gxout shaded'
'set cterp off'
'set csmooth off'
'set ccols 99 78 77 76 75 74 73 72 21 31 32 33 34 35 36 37 38'
'set clevs 0 0.001 0.002 0.005 0.01 0.02  0.04  0.1  0.2  0.4  1.0 4 10 '
'd SNODsfc'
'cbar98'
'draw title Snow Depth [m] ' date
return

function z300(date)
'set ccolor 98'
'set grads off'
'set lev 300'
'set gxout shaded'
'set cterp off'
'defintplus 10'
'd mag(UGRDprs,VGRDprs)'
'cbar98'
'set gxout contour'
'set cthick 5'
'set ccolor 98'
'd HGTprs'
'set ccolor 98'
'set arrlab off'
'd UGRDprs;skip(VGRDprs,10)'
'draw title 300 mb height, isotachs [m/s] ' date
return

function albedo(date)
'set ccolor 98'
'set grads off'
'set gxout shaded'
'set cterp off'
'set clevs  10 20 30 40 50 60 70 80'
'set ccols  49 47 45 43 42 86 84 82 99'
'd ALBDOsfc'
'cbar98'
'draw title albedo ' date
say 'date==' date
return

function tcdc(date)
'set ccolor 98'
'set grads off'
'set gxout shaded'
'set cterp off'
'defintplus 10'
'set ccols  99 42 43 44 45 47 49 22 26 29'
'set clevs  10 20 30 40 50 60 70 80 90'
'd TCDCclm.2'
'cbar98'
'draw title total cloud cover [%] ' date
return
