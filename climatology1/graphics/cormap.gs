'open cormap.ctl'
'set gxout grfill'
'clear'
#'enable print b.grads'
'set display color white'
'set mpdset hires'
#'set lon 30 390'
'set mproj nps'
'set lat 70 90'
'set lon -350 10'
#'set mproj sps'
#'set lat -90  -55'
#'set lon -240 120'
#
'set grads off'
'set strsiz 0.2'
'set t 1'
'draw string 4.00 7.75 Correlation '
'set clevs 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0'
'd correl'
'run cbarn'
'printim cormap1.gif'
'clear'
#
'set grads off'
'set strsiz 0.2'
'set t 1'
'draw string 4.00 7.75 Correlation '
'set clevs 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0'
'd correl'
'run cbarn'
'printim cormap1.gif'
'clear'
#
'set grads off'
'set strsiz 0.2'
'set t 2'
'draw string 4.00 7.75 Correlation '
'set clevs 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0'
'd correl'
'run cbarn'
'printim cormap2.gif'
'clear'
#
'set grads off'
'set strsiz 0.2'
'set t 3'
'draw string 4.00 7.75 Correlation '
'set clevs 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0'
'd correl'
'run cbarn'
'printim cormap3.gif'
'clear'
#
'set grads off'
'set strsiz 0.2'
'draw string 4.00 7.75 r^2 '
'set clevs 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0'
'd correl*correl'
'run cbarn'
'printim sh_r2.gif'
'clear'
#
'quit'
