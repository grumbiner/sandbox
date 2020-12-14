'open expect.ctl'
'set gxout grfill'
'clear'
'set display color white'
'set mpdset hires'
#'set lon 30 390'
#'set lat  15  85'
'set mproj sps'
'set lat -90 -55'
'set lon -350 10'
#
#
'set grads off'
'set strsiz 0.2'
'draw string 4.00 7.75 anomaly vs. 1981-2010 reference'
'set clevs  -1 -0.75 -0.5 -0.25 -0.125 0.125 0.25 0.5 0.75 1 '
'd maskout(maskout((temp - climo),0.05-land),ice-0.1)'
'run cbarn'
'printim icecover.gif'
'clear'
#
'set grads off'
'set strsiz 0.2'
'draw string 4.00 7.75 temperature under ice pack'
'set clevs  -1.92 -1.87 -1.81 -1.75 -1.69 -1.64 -1.53 -1.36 -1.08 -0.81 -0.54 '
'd maskout(maskout((temp),0.05-land),ice-0.1)'
'run cbarn'
'printim tice.gif'
'clear'
'quit'
#
'set grads off'
'set strsiz 0.2'
'draw string 4.00 7.75 anomaly vs. 1961-1990 reference'
'set clevs -8 -4 -2 -1 -0.5 0.5 1 2 4 8'
'd maskout(maskout(oldanom,0.05-land),abs(oldanom)-0.5)'
'run cbarn'
'printim oldanom.gif'
'clear'
#
'set grads off'
'set strsiz 0.2'
'draw string 4.00 7.75 delta of anomalies '
'd maskout(maskout(temp - climo - oldanom, 0.05 - land ), abs(temp - climo - oldanom) - 0.5)'
'run cbarn'
'printim delanom.gif'
'clear'
#
'quit'
'set grads off'
'set strsiz 0.2'
'draw string 4.00 7.75 sst-climo'
#'set clevs -1 0 0.25 0.5 0.75 1 2 3 4 5 '
'd climo'
'run cbarn'
'printim climo.gif'
'clear'
#
'set grads off'
'set strsiz 0.2'
'draw string 4.00 7.75 sst-obs'
'd temp'
'run cbarn'
'printim temp.gif'
'clear'
#
'quit'