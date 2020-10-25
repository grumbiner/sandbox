'open second.ctl'
'set gxout grfill'
'clear'

'set grads off'
'set display color white'
'set mpdset hires'
'set lon 30 390' #cut through S. Africa, minimize ocean along seam

'set strsiz 0.2'
'draw string 3.00 7.95 arcminutes (latitude) per 0.5 K'

#Times, by weeks
'set clevs 21 28 35 42 49 56 63 70 77 84 98 112 126 140'
# Resolutions, arcmin:
'set clevs 2 3 4 5 6 7.5 10 12 15 20 30 60 '
# deciles of climatological SST
'set clevs 0.87 7.16 13.03 18.16 21.61 24.29 26.17 27.32 28.23'

#Color scheme management methods:
'set ccols 16 17 18 19 20 21 22 23 24 25 26 27 28 29 '
'run colorset.gs'
'run cbarn'

#####   Looping
step=1
while (step <= 35 )
  'set grads off'
  'set strsiz 0.2'
  'draw string 4.00 7.75 Autocorrelations'
  'set t 'step
  'set clevs -0.25 -0.1 0 0.1 0.25 0.5 0.6 0.7 0.8 0.9'
  'd  lag'
  'run cbarn'
  'printim acor'step-1'.gif'
  'clear'
  step = step + 1
endwhile

#####   grads functions:
#arithmetic:
'd sst/100'
'd sst-273.15'

#aave = area-weighted average
'define alpha = aave((skew),lon=-180,lon=180,lat=-90,lat=90)'
#displaying atan2
'd atan2(dy, dx) * 180 / 3.141592654'
#cdiff = gradient fn, -->   http://cola.gmu.edu/grads/gadoc/gradfunccdiff.html
'd sqrt(cdiff(sst,x)*cdiff(sst,x)+cdiff(sst,y)*cdiff(sst,y))'




'run cbarn'
'printim lapl.gif'



#
'quit'
