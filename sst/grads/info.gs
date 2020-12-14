'open info.ctl '
'set gxout grfill '
'clear '
'set display color white '
'set mpdset hires '
#
'set grads off '
'set strsiz 0.2'
'draw string 3.00 7.75 Shannon Information 16 K bins '
'd info16'
'run cbarn '
'printim info16.gif'
'clear'
#
'set grads off '
'set strsiz 0.2'
'draw string 3.00 7.75 Shannon Information 8 K bins '
'd info8'
'run cbarn '
'printim info8.gif'
'clear'
#
'set grads off '
'set strsiz 0.2'
'draw string 3.00 7.75 Shannon Information 4 K bins '
'd info4'
'run cbarn '
'printim info4.gif'
'clear'
#
'set grads off '
'set strsiz 0.2'
'draw string 3.00 7.75 Shannon Information 0.5 K bins '
'd infop5'
'run cbarn '
'printim infop5.gif'
'clear'
#
'set grads off '
'set strsiz 0.2'
'draw string 3.00 7.75 Shannon Information 16 K bins vs. 4 K'
'd (info16-info4) - 2'
'run cbarn '
'printim info16m4.gif'
'clear'
#
'set grads off '
'set strsiz 0.2'
'draw string 3.00 7.75 Shannon Information 4 K bins vs. 0.5 K'
'd (infop5-info4) - 3'
'run cbarn '
'printim infop5m4.gif'
'clear'
'quit'
#
'set grads off '
'set strsiz 0.2'
#'draw string 3.00 7.75 Shannon Information 1 vs. range'
#'set clevs 0.0 0.25 0.375 0.5 0.6 0.65 0.7 0.75 0.8 0.85 0.9 '
'draw string 3.00 7.75 Shannon Information 10 K bins vs. range'
'set clevs 0.15 .18 0.20 .21 .22 .23 .24 0.25 0.275 0.30 0.35 '
'd info10/range'
'run cbarn '
'printim range10.gif'
'clear'
#
'quit '
#'set lat 30 90 '
#'set lon 130 240 '
#'run colorset.gs'
#'set clevs 0 2 4 5 5.5 5.75 6 6.25 6.5 7 7.5 8 9 '
#'set ccols 16 17 18 19 20 21 22 23 24 25 26 27 28 29 '
#'set clevs -0.5 -0.25 -0.10 -0.05 0.05 0.10 0.2 0.3 0.4 0.5 0.75'
#'d info1-(info2-1) '
#'set clevs 0.0 0.05 0.10 0.25 0.5 0.75 1.0 1.25'
#'d info1-(info3-2.32) '
#'set clevs 0.0 0.05 0.10 0.25 0.5 0.75 1.0 1.25'
#'d info1-(info4-3.32) '