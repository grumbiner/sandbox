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


* 4 panel maps
'set dfile 1'
'page'
'c'
'page q1'
z200(date)
'page q2'
z500(date)
'page q3'
z850(date)
'page q4'
t850(date)
'printim r2.1.' date '.png x800 y600'


'page'
'c'
'page q1'
mslp(date)
'page q2'
rh700(date)

* plot the gaussian grid files'
'set dfile 2'
'page q3'
* spfh2m(date)
rh2m(date)
'page q4'
tmp2m(date)
'printim r2.2.' date '.png x800 y600'

'page'
'c'
'page q1'
soilw(date)
'page q2'
wind10m(date)
'page q3'
lhtfl(date)
'page q4'
shtfl(date)
'printim r2.3.' date '.png x800 y600'

exit
function verf(file,date,var1,var2)
'map lola'
'define bias = aave(' var1 ' - ' var2 ',g)'
'd bias'
_bias=subwrd(result,4)

'define rms = aave((' var1 ' - ' var2 ')*( ' var1 ' - ' var2 '),g)'
'd sqrt(rms)'
_rms=subwrd(result,4)

a='/u/wx51we/home/rr/rr_r2_data/verf/' file '.dat'
b=file ' ' date ' ' _bias ' ' _rms
say write ' ' a ' ' b
err=write(a , b, append)
'map rr'

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

function z200(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -60 -50 -40 -30 -20 -10 10 20 30 40 50 60'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'set lev 200'
'd hgtprs.1-hgtprs.2'
'cbar98'
'set gxout contour'
'defint 100'
'set ccolor 98'
'd hgtprs.1'
verf(z200,date,hgtprs.1,hgtprs.2)
'draw title z200 rr-r2 [m] ' date ' diff=' _bias ' rms=' _rms
return

function z500(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -60 -50 -40 -30 -20 -10 10 20 30 40 50 60'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'set lev 500'
'd hgtprs.1-hgtprs.2'
'cbar98'
'set gxout contour'
'defint 100'
'set ccolor 98'
'd hgtprs.1'
verf(z500,date,hgtprs.1,hgtprs.2)
'draw title z500 rr-r2 [m] ' date ' diff=' _bias ' rms=' _rms
return

function z850(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -60 -50 -40 -30 -20 -10 10 20 30 40 50 60'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'set lev 850'
'd hgtprs.1-hgtprs.2'
'cbar98'
'set gxout contour'
'defint 50'
'set ccolor 98'
'd hgtprs.1'
verf(z850,date,hgtprs.1,hgtprs.2)
'draw title z850 rr-r2 [m] ' date ' diff=' _bias ' rms=' _rms
return

function t850(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -6 -5 -4 -3 -2 -1 1 2 3 4 5 6'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'set lev 850'
'd tmpprs.1-tmpprs.2'
'cbar98'
'set gxout contour'
'defint 5'
'set ccolor 98'
'd tmpprs.1'
verf(t850,date,tmpprs.1,tmpprs.2)
'draw title t850 rr-r2 [K] ' date ' diff=' _bias ' rms=' _rms
return

function rh700(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -90 -75 -60 -45 -30 -15 15 30 45 60 75 90'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'set lev 700'
spfh2rh(700,tmpprs.1,spfhprs.1)
'd rh2-rhprs.2'
'cbar98'
'set gxout contour'
'defint 15'
'set ccolor 98'
'd rh2'
'map lola'
spfh2rh(700,tmpprs.1,spfhprs.1)
verf(rh700,date,rh2,rhprs.2)
'draw title rh700 rr-r2 [K] ' date ' diff=' _bias ' rms=' _rms
return

function mslp(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -12 -10 -8 -6 -4 -2 2 4 6 8 10 10'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'set lev 850'
'd msletmsl.1-presmsl.2)/100'
'cbar98'
'set gxout contour'
'defint 5'
'set ccolor 98'
'd msletmsl.1/100'
'map lola'
'define a=msletmsl.1/100'
'define b=presmsl.2/100'
verf(mslp,date,a,b)
'draw title mslp rr-r2 [mb] ' date ' diff=' _bias ' rms=' _rms
return

function tmp2m(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -15 -10 -5 -3 -2 -1 1 2 3 5 10 15'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'd tmp2m.3-tmp2m.4'
'cbar98'
'set gxout contour'
'defint 5'
'set ccolor 98'
'd tmp2m.3'
verf(tmp2m,date,tmp2m.3,tmp2m.4)
'draw title tmp2m rr-r2 [K] ' date ' diff=' _bias ' rms=' _rms
return

function spfh2m(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -6 -5 -4 -3 -2 -1 1 2 3 4 5 6'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'd (spfh2m.3-spfh2m.4)*1000'
'cbar98'
'set gxout contour'
'defint 2'
'set ccolor 98'
'd spfh2m.3*1000'
'draw title spfh2m rr-r2 [g/kg] ' date
return

function rh2m(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -60 -50 -40 -30 -20 -10 10 20 30 40 50 60'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
* 'define prs=pres2m.4/100'
'define prs=pressfc.4/100 - 0.2'
spfh2rh(prs,tmp2m.4,spfh2m.4)
'd rh2m.3-rh2'
'cbar98'
'set gxout contour'
'defint 10'
'set ccolor 98'
'd rh2m.3'
'map lola'
'define prs=pressfc.4/100 - 0.2'
spfh2rh(prs,tmp2m.4,spfh2m.4)
verf(rh2m,date,rh2m.3,rh2)
'draw title rh2m rr-r2 [g/kg] ' date ' diff=' _bias ' rms=' _rms
return


function soilw(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'set clevs  -0.3 -0.25 -0.2 -0.15 -0.1 -0.05 0.05 0.1 0.15 0.2 0.25 0.3'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'map usa2'

'define s1=(soilw0_10cm.3+3*soilw10_40cm.3+6*soilw40_100cm.3+10*soilw100_200cm.3)/20'
'define s2=(soilwsoilt.4+19*soilwsoilm.4)/20'

'd s1-s2'
'cbar98'
* 'set gxout contour'
* 'defint 0.05'
* 'set ccolor 98'
* 'd s1'

'map lola'
'define s1=(soilw0_10cm.3+3*soilw10_40cm.3+6*soilw40_100cm.3+10*soilw100_200cm.3)/20'
'define s2=(soilwsoilt.4+19*soilwsoilm.4)/20'
verf(soilm,date,s1,s2)
'draw title soilm rr-r2 ' date ' diff=' _bias ' rms=' _rms

'map rr'

return

function wind10m(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'map rr'
'set gxout shaded'
* 'set clevs  -12 -10 -8 -6 -4 -2 2 4 6 8 10 12'
* 'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'set clevs  2 4 6 8 10 12'
'set clevs  1 2 4 6 10 15'
'set clevs  2 4 6 8 10 12'
'set ccols  99 22 24 25 27 29 88'
* 'd mag(ugrd10m.3,vgrd10m.3)-mag(ugrd10m.4,vgrd10m.4)'
'd mag(ugrd10m.3-ugrd10m.4,vgrd10m.3-vgrd10m.4))'
'cbar98'
'set ccolor 49'
'd ugrd10m.3-ugrd10m.4;skip(vgrd10m.3-vgrd10m.4,2)'
'cbar98'
'draw title (rr-r2) 10m wind speed [m/s] ' date 
return

function lhtfl(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'map rr'
'set gxout shaded'
'set clevs  -300 -250 -200 -150 -100 -50 50 100 150 200 250 300'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'define rrlhtfl=-(lhtflsfc.5(t=1)+lhtflsfc.6(t=1))/2'
'd rrlhtfl-lhtflsfc.7(t+1)'
'cbar98'
'set ccolor 98'
'set gxout contour'
'defint 100'
'd rrlhtfl'
'map lola'
'define rrlhtfl=-(lhtflsfc.5(t=1)+lhtflsfc.6(t=1))/2'
'define r2=lhtflsfc.7(t+1)'
verf(lhtfl,date,rrlhtfl,r2)
*'draw title lhtfl rr-r2 ' date ' diff=' _bias ' rms=' _rms

* land verfification
'map lola'
'define mask=maskout(1,landsfc.8(t=1)-0.5)'
'define rr=-(lhtflsfc.5(t=1)+lhtflsfc.6(t=1))/2*mask'
'define r2=lhtflsfc.7(t+1)'
verf(lhtflland,date,rr,r2)
'draw title lhtfl rr-r2 ' date ' ldiff=' _bias ' lrms=' _rms

* sea verification
'map lola'
'define mask=maskout(1,0.5-landsfc.8(t=1))'
'define rr=-(lhtflsfc.5(t=1)+lhtflsfc.6(t=1))/2*mask'
'define r2=lhtflsfc.7(t+1)'
verf(lhtflsea,date,rr,r2)
return

function shtfl(date)
'set grads off'
'set ccolor 98'
'set csmooth on'
'set gxout shaded'
'map rr'
'set gxout shaded'
'set clevs  -300 -250 -200 -150 -100 -50 50 100 150 200 250 300'
'set ccols  88 49 47 45 44 42 99 22 24 25 27 29 88'
'define rrshtfl=-(shtflsfc.5(t=1)+shtflsfc.6(t=1))/2'
'd rrshtfl-shtflsfc.7(t+1)'
'cbar98'
'set ccolor 98'
'set gxout contour'
'defint 100'
'd rrshtfl'
'map lola'
'define rr=-(shtflsfc.5(t=1)+shtflsfc.6(t=1))/2'
'define r2=shtflsfc.7(t+1)'
verf(shtfl,date,rr,r2)
*'draw title shtfl rr-r2 ' date ' diff=' _bias ' rms=' _rms
* land verfification
'map lola'
'define mask=maskout(1,landsfc.8(t=1)-0.5)'
'define rr=-(shtflsfc.5(t=1)+shtflsfc.6(t=1))/2*mask'
'define r2=shtflsfc.7(t+1)'
verf(shtflland,date,rr,r2)
'draw title shtfl rr-r2 ' date ' ldiff=' _bias ' lrms=' _rms
* sea verification
'map lola'
'define mask=maskout(1,0.5-landsfc.8(t=1))'
'define rr=-(shtflsfc.5(t=1)+shtflsfc.6(t=1))/2*mask'
'define r2=shtflsfc.7(t+1)'
verf(shtflsea,date,rr,r2)

return
