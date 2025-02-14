'open all.ctl '
'set gxout grfill '
'set mpdset hires '
#'set lat  50  90'
#'set lon 185 260'
#
step=0
while (step < 9) 
'clear '
'set grads off '
'set mproj nps'
'set lat 45 90'
'set lon -350 10'
'set display color white '
'set strsiz 0.2'
x=90-5*step
'draw string 3.00 7.875 NH Regions 0.'x' rcrit'
#'set clevs   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29'
'set clevs   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36'
#'set ccols 10 11 12 13 14 11 12 13 14 11 12 13 14 11 12 13 14 11 12 13 14 11 12 13 14 11 12 13 14 11 12' 
'set ccols 10 11 12 13 14 15 11 12 13 14 15 11 12 13 14 15 11 12 13 14 15 11 12 13 14 15 11 12 13 14 15 11 12 13 14 11 12' 
'set t 'step+1
'd maskout(sst,36-sst)'
'run cbarn'
'printim nhregions'x'.gif'
#
'clear '
'set grads off '
'set mproj sps'
'set lat -90 -50'
'set lon -240 120'
'set display color white '
'set strsiz 0.2'
x=90-5*step
'draw string 3.00 7.875 SH Regions 0.'x' rcrit'
'set clevs   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36'
'set ccols 10 11 12 13 14 15 11 12 13 14 15 11 12 13 14 15 11 12 13 14 15 11 12 13 14 15 11 12 13 14 15 11 12 13 14 15 11 12' 
'd maskout(sst,36-sst)'
'run cbarn'
'printim shregions'x'.gif'
#
step=step+1
endwhile
'quit '
