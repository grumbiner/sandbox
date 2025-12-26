'reinit'
'open flx.ctl'
'open awip3203cp_00-18.ctl'
'open awip3203cp_03-21.ctl'
**'open flx.ctl'

mask=10000000000
startime=1
while(startime<=2)

'set t 'startime''

****************LHTFL
write(lshtmp.dat, LHTFLsfc)
'set x 1 192'
'set y 1 94'

'define gr=lhtflsfc.1'

ifile=1
while(ifile<=2)

if(ifile=1)
  'set dfile 3'
  verftime=startime
* verifying time in awip3203cp_00-18 = startime in awip3200cp_03-21
  'set t 'verftime''
  'q time 1 1'
  say result
  time=subwrd(result,3)
  write(lshtmp.dat, time,append)
  'set dfile 1'
  'set t 'startime''
  'set x 1 192'
  'set y 1 94'm
  'define rr=(-1)*lhtflsfc.2(time+00hr)'
else
  'set dfile 2'
  verftime=startime+1
* verifying time in awip3203cp_03-21 = startime+06hr in awip3200cp_00-18
  'set t 'verftime''
  'q time 1 1'
  say result
  time=subwrd(result,3)
  write(lshtmp.dat, time,append)
  'set dfile 1'
  'set t 'startime''
  'set x 1 192'
  'set y 1 94'
  'define rr=(-1)*lhtflsfc.3(time+03hr)'
endif


'set lon 180 330'
'set lat 10 85'
'set gxout stat'
'd rr*gr'
stanum=sublin(result,7)
say stanum
valnum=subwrd(stanum,8)
*say valnum
write(lshtmp.dat, valnum,append)

'set x 1'
'set y 1'

'define meanf=aave(maskout(rr,'mask'-gr),lon=180,lon=330,lat=10,lat=85)'
'define meano=aave(maskout(gr,'mask'-rr),lon=180,lon=330,lat=10,lat=85)'
'define meanfo=aave(rr*gr,lon=180,lon=330,lat=10,lat=85)'
'define meanf2=aave(maskout(rr*rr,'mask'-gr),lon=180,lon=330,lat=10,lat=85)'
'define meano2=aave(maskout(gr*gr,'mask'-rr),lon=180,lon=330,lat=10,lat=85)'


'q defval meanf 1 1'
aa=subwrd(result,3)
write(lshtmp.dat, aa,append)

'q defval meano 1 1'
aa=subwrd(result,3)
write(lshtmp.dat, aa,append)

'q defval meanfo 1 1'
aa=subwrd(result,3)
write(lshtmp.dat, aa,append)

'q defval meanf2 1 1'
aa=subwrd(result,3)
write(lshtmp.dat, aa,append)

'q defval meano2 1 1'
aa=subwrd(result,3)
write(lshtmp.dat, aa,append)

'define a=sqrt(meanf2+meano2-2*meanfo)'
'q defval a 1 1'
say result

'define rms=sqrt(aave((rr-gr)*(rr-gr),lon=180,lon=330,lat=10,lat=85))'
'q defval rms 1 1'
say result

ifile=ifile+1
endwhile

****************SHTFL
write(lshtmp.dat, SHTFLsfc,append)
'set x 1 192'
'set y 1 94'

'define gr=shtflsfc.1'

ifile=1
while(ifile<=2)

if(ifile=1)
  'set dfile 3'
  verftime=startime
* verifying time in awip3203cp_00-18 = startime in awip3200cp_03-21
  'set t 'verftime''
  'q time 1 1'
  say result
  time=subwrd(result,3)
  write(lshtmp.dat, time,append)
  'set dfile 1'
  'set t 'startime''
  'set x 1 192'
  'set y 1 94'
  'define rr=(-1)*shtflsfc.2(time+00hr)'
else
  'set dfile 2'
  verftime=startime+1
* verifying time in awip3203cp_03-21 = startime+06hr in awip3200cp_00-18
  'set t 'verftime''
  'q time 1 1'
  say result
  time=subwrd(result,3)
  write(lshtmp.dat, time,append)
  'set dfile 1'
  'set t 'startime''
  'set x 1 192'
  'set y 1 94'
  'define rr=(-1)*shtflsfc.3(time+03hr)'
endif

'set lon 180 330'
'set lat 10 85'
'set gxout stat'
'd rr*gr'
stanum=sublin(result,7)
say stanum
valnum=subwrd(stanum,8)
*say valnum
write(lshtmp.dat, valnum,append)

'set x 1'
'set y 1'

'define meanf=aave(maskout(rr,'mask'-gr),lon=180,lon=330,lat=10,lat=85)'
'define meano=aave(maskout(gr,'mask'-rr),lon=180,lon=330,lat=10,lat=85)'
'define meanfo=aave(rr*gr,lon=180,lon=330,lat=10,lat=85)'
'define meanf2=aave(maskout(rr*rr,'mask'-gr),lon=180,lon=330,lat=10,lat=85)'
'define meano2=aave(maskout(gr*gr,'mask'-rr),lon=180,lon=330,lat=10,lat=85)'


'q defval meanf 1 1'
aa=subwrd(result,3)
write(lshtmp.dat, aa,append)

'q defval meano 1 1'
aa=subwrd(result,3)
write(lshtmp.dat, aa,append)

'q defval meanfo 1 1'
aa=subwrd(result,3)
write(lshtmp.dat, aa,append)

'q defval meanf2 1 1'
aa=subwrd(result,3)
write(lshtmp.dat, aa,append)

'q defval meano2 1 1'
aa=subwrd(result,3)
write(lshtmp.dat, aa,append)

'define a=sqrt(meanf2+meano2-2*meanfo)'
'q defval a 1 1'
say result

'define rms=sqrt(aave((rr-gr)*(rr-gr),lon=180,lon=330,lat=10,lat=85))'
'q defval rms 1 1'
say result

ifile=ifile+1
endwhile


startime=startime+1
endwhile

'reinit'
'quit'
