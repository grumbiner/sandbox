; program plot_standard.pro 
; this IDL program plots the degribbed COFS data on the 
; standard grid.  The data was processed with the fortran90
; program read_standard.f90  
; William P. O'Connor May 2002
;----------------------------------------------------------------------
; set dimensions 
im=332 
jm=210   
print,'im = ',im,format='(a15,5x,i5)'  
print,'jm = ',jm,format='(a15,5x,i5)'  
;----------------------------------------------------------------------
; declare arrays
ua=fltarr(im,jm)       ; u velocity 
va=fltarr(im,jm)       ; v velocity  
t =fltarr(im,jm)       ; temperature array 
s =fltarr(im,jm)       ; salinity   
wl=fltarr(im,jm)       ; water level 
speed=fltarr(im,jm)    ; current speed 
mask=intarr(im,jm)     ; land/water mask 
xlon=fltarr(im)        ; longitude array
ylat=fltarr(jm)        ; latitude array 
xbndry=fltarr(im+1)    ; x cell boundary array
ybndry=fltarr(jm+1)    ; y cell boundary array
yyyymmdd='     '       ; define as character string 
;------------------------------------------------------------------------
; initialize arrays 
for j=0,jm-1 do begin
for i=0,im-1 do begin
ua(i,j)=0.0
va(i,j)=0.0
 t(i,j)=0.0
 s(i,j)=0.0 
 wl(i,j)=0.0
 speed(i,j)=0.0 
 mask(i,j)=1
endfor
endfor 
;
for i=0,im-1 do begin
xlon(i)=-83.05 + 0.10*i  
endfor
;
for j=0,jm-1 do begin
ylat(j)=26.35 + 0.10*j 
endfor 
;
for i=0,im do begin
xbndry(i)=-83.00 + 0.10*i
endfor 
;
for j=0,jm do begin
ybndry(j)=26.30 + 0.10*j 
endfor 
;------------------------------------------------------------------------- 
;set plot type
ptype=' '
print,'enter plot type (x,ps);'
read,ptype
set_plot,ptype
;
xdist=11.1*float(im)*cos(37.0*3.1416/180.)  
ydist=11.1*float(jm) 
aspect=ydist/xdist  
print,'aspect = ',aspect,format='(a15,5x,f10.4)'

;----------------------------------------------------------
; Plotted on landscape on paper. Terminal does not take landscape.
;
isize=1024. 
jsize=isize*aspect 
print,'isize = ',isize,format='(a15,5x,f10.4)' 
print,'jsize = ',jsize,format='(a15,5x,f10.4)' 
if(ptype eq 'x') then window,0,xsize=isize,ysize=jsize
ys=5.0
xs=ys/aspect  
print,'landscape on paper',format='(a20)'  
print,'xsize = ',xs,format='(a15,5x,f10.4)'
print,'ysize = ',ys,format='(a15,5x,f10.4)'  
if(ptype eq 'ps') then device,ysize=ys,xsize=xs, $ 
          /inch,xoffs=0.5,yoffs=9.0,/landscape 

if (ptype eq 'ps') then device,filename='plot_standard.ps'
;----------------------------------------------------------------------------
; read date string from shell script 
read,yyyymmdd,format='(a8)' 
;
num_clean=1  ; define as integer
read,num_clean
print,'num_clean = ',num_clean,format='(a15,i5)'
num_nout=1   ; define as integer 
read,num_nout
print,'num_nout = ',num_nout,format='(a15,i5)'
;
clean_lat=fltarr(num_clean)
clean_lon=fltarr(num_clean) 
;
;---------------------------------------------------------
; open the Gulf stream position file 'clean.yyyymmdd' and read the lat/lon postions
openr,10,'clean.'+yyyymmdd 
;
for k=0,num_clean-1 do begin
readf,10,yhold,xhold,format='(f5.2,f6.2)'  
clean_lat(k)=yhold 
clean_lon(k)=-xhold
;print,k,clean_lat(k),clean_lon(k),format='(i5,1x,f5.2,f6.2)' 
endfor 
close,10 
;---------------------------------------------------
;  
;num_nout=161 
if (num_nout > 1) then begin
nout_lat=fltarr(num_nout)
nout_lon=fltarr(num_nout)  
endif
;
; open the Gulf stream position file 'nout.yyyymmdd' and read the lat/lon postions
openr,20,'nout.'+yyyymmdd  
;
if (num_nout > 1) then begin
for k=0,num_nout-1 do begin
readf,20,yhold,xhold,format='(f4.1,f5.1)'  
nout_lat(k)=yhold 
nout_lon(k)=-xhold 
;print,k,nout_lat(k),nout_lon(k),format='(i5,1x,f4.1,f5.1)' 
endfor  
endif
close,20 
;----------------------------------------------------------------------------
; open and read the data file 
openr,50,'uvstw.txt'
readu,50,ua,va,s,t,wl 
close,50 
;----------------------------------------------------------------------------
; process array data 
; set land values of -100.0 to special value to avoid plotting  
for j=0,jm-1 do begin
for i=0,im-1 do begin
if(t(i,j) eq -100.0) then mask(i,j)=0 
if(t(i,j) eq -100.0) then t(i,j)=!values.f_nan 
if(s(i,j) eq -100.0) then s(i,j)=!values.f_nan 
if(ua(i,j) eq -100.0) then ua(i,j)=!values.f_nan 
if(va(i,j) eq -100.0) then va(i,j)=!values.f_nan 
if(wl(i,j) eq -100.0) then wl(i,j)=!values.f_nan 
endfor
endfor
;---------------------------------------------------------------------------- 
; plot the salinity array 
contour,s,xlon,ylat,  $   
;  levels=[6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.,19.,20.],   $
  levels=[30.,32.,33.,34.,35.,36.,37.,38.],   $
 c_linestyle=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  $
 c_labels=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],   $      
 xstyle=1,ystyle=1, $ 
 xmargin=[0,0],ymargin=[0,0] ,$  
 xrange=[-83.00,-49.95],yrange=[26.30,47.30] 
;
; overplot the 'clean.yyyymmdd' file Gulf Stream 
for k=0,num_clean-1 do begin
plots,clean_lon(k),clean_lat(k),psym=1,symsize=1.0  
endfor  
;
; overplot the 'nout.yyyymmdd' file Gulf Stream 
if (num_nout > 1) then begin
for k=0,num_nout-1 do begin
plots,nout_lon(k),nout_lat(k),psym=5,symsize=1.0  
endfor  
endif
;
xyouts,0.05,0.95,'NOAA REGIONAL OCEAN FORECAST SYSTEM (ROFS)',size=1,/NORMAL
xyouts,0.05,0.90,'STANDARD GRID',size=1,/NORMAL  
xyouts,0.05,0.85,'SURFACE SALINITY (PSU)',size=1,/NORMAL 
xyouts,0.05,0.75,'VALID ',size=1,/NORMAL 
xyouts,0.15,0.75,yyyymmdd,size=1,/NORMAL   
xyouts,0.05,0.80,'GULF STREAM NORTH WALL +',size=1,/NORMAL 
;plots,-79.0,41.0,psym=1,symsize=1.0  
;xyouts,0.05,0.65,'nout',size=1,/NORMAL 
;plots,-79.0,40.0,psym=5,symsize=1.0
;
; plot grid outline
; plot vertical cell boundaries 
for i=0,im-2 do begin
 for j=0,jm-1 do begin
  if((mask(i,j) eq 0) and (mask(i+1,j) eq 1)) then begin
   plots,[xbndry(i+1),xbndry(i+1)],[ybndry(j),ybndry(j+1)],thick=2 
  endif
  if((mask(i,j) eq 1) and (mask(i+1,j) eq 0)) then begin
   plots,[xbndry(i+1),xbndry(i+1)],[ybndry(j),ybndry(j+1)],thick=2 
  endif  
 endfor
endfor
;
; plot horozontal cell boundaries   
for i=0,im-1 do begin
 for j=0,jm-2 do begin  
  if((mask(i,j) eq 0) and (mask(i,j+1) eq 1)) then begin
   plots,[xbndry(i),xbndry(i+1)],[ybndry(j+1),ybndry(j+1)],thick=2 
  endif
  if((mask(i,j) eq 1) and (mask(i,j+1) eq 0)) then begin
   plots,[xbndry(i),xbndry(i+1)],[ybndry(j+1),ybndry(j+1)],thick=2 
  endif
 endfor
endfor   
;
;------------------------------------------------------------------------------
; plot the temperature array 
contour,t,xlon,ylat,  $   
  levels=[6.,8.,10.,12.,14.,16.,18.,20.,22.,24.,26.,28.,30.,32.,34.,36.,38.,40.],   $
 c_linestyle=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  $
    c_labels=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],   $      
 xstyle=1,ystyle=1, $ 
 xmargin=[0,0],ymargin=[0,0] ,$  
 xrange=[-83.00,-49.95],yrange=[26.30,47.30] 
;
; overplot the 'clean.yyyymmdd' file Gulf Stream 
for k=0,num_clean-1 do begin
plots,clean_lon(k),clean_lat(k),psym=1,symsize=1.0  
endfor  
;
; overplot the 'nout.yyyymmdd' file Gulf Stream 
if (num_nout > 1) then begin
for k=0,num_nout-1 do begin
plots,nout_lon(k),nout_lat(k),psym=5,symsize=1.0  
endfor  
endif
;
xyouts,0.05,0.95,'NOAA REGIONAL OCEAN FORECAST SYSTEM (ROFS)',size=1,/NORMAL
xyouts,0.05,0.90,'STANDARD GRID',size=1,/NORMAL  
xyouts,0.05,0.85,'SURFACE TEMPERATURE (C)',size=1,/NORMAL 
xyouts,0.05,0.75,'VALID ',size=1,/NORMAL 
xyouts,0.15,0.75,yyyymmdd,size=1,/NORMAL   
xyouts,0.05,0.80,'GULF STREAM NORTH WALL +',size=1,/NORMAL 
;plots,-79.0,41.0,psym=1,symsize=1.0  
;xyouts,0.05,0.65,'nout',size=1,/NORMAL 
;plots,-79.0,40.0,psym=5,symsize=1.0
;
; plot grid outline
; plot vertical cell boundaries 
for i=0,im-2 do begin
 for j=0,jm-1 do begin
  if((mask(i,j) eq 0) and (mask(i+1,j) eq 1)) then begin
   plots,[xbndry(i+1),xbndry(i+1)],[ybndry(j),ybndry(j+1)],thick=2 
  endif
  if((mask(i,j) eq 1) and (mask(i+1,j) eq 0)) then begin
   plots,[xbndry(i+1),xbndry(i+1)],[ybndry(j),ybndry(j+1)],thick=2 
  endif  
 endfor
endfor
;
; plot horozontal cell boundaries   
for i=0,im-1 do begin
 for j=0,jm-2 do begin  
  if((mask(i,j) eq 0) and (mask(i,j+1) eq 1)) then begin
   plots,[xbndry(i),xbndry(i+1)],[ybndry(j+1),ybndry(j+1)],thick=2 
  endif
  if((mask(i,j) eq 1) and (mask(i,j+1) eq 0)) then begin
   plots,[xbndry(i),xbndry(i+1)],[ybndry(j+1),ybndry(j+1)],thick=2 
  endif
 endfor
endfor   
;
;-----------------------------------------------------------------------------
; find the maximum current vector
vmax=0.0
for j=0,jm-1 do begin
   for i=0,im-1 do begin
     if(mask(i,j) eq 0) then goto, skipspeed 
     speed(i,j)=sqrt(va(i,j)*va(i,j)+ua(i,j)*ua(i,j))
     if(speed(i,j) gt vmax) then vmax=speed(i,j) 
     skipspeed: 
   endfor
endfor 
print,'max speed (m/s) = ',vmax,format='(a20,f8.2)' 
;--------------------------------------------
;
; plot the current speed array 
contour,speed,xlon,ylat,  $   
  levels=[0.5,1.0,1.5,2.0,2.5],   $
 c_linestyle=[0,0,0,0,0],  $
    c_labels=[1,1,1,1,1],   $      
 xstyle=1,ystyle=1, $ 
 xmargin=[0,0],ymargin=[0,0] ,$  
 xrange=[-83.00,-49.95],yrange=[26.30,47.30] 
;
; overplot the 'clean.yyyymmdd' file Gulf Stream 
for k=0,num_clean-1 do begin
plots,clean_lon(k),clean_lat(k),psym=1,symsize=1.0  
endfor  
;
; overplot the 'nout.yyyymmdd' file Gulf Stream 
if (num_nout > 1) then begin
for k=0,num_nout-1 do begin
plots,nout_lon(k),nout_lat(k),psym=5,symsize=1.0  
endfor  
endif
; 
xyouts,0.05,0.95,'NOAA REGIONAL OCEAN FORECAST SYSTEM (ROFS)',size=1,/NORMAL
xyouts,0.05,0.90,'STANDARD GRID',size=1,/NORMAL  
xyouts,0.05,0.85,'SURFACE CURRENTS',size=1,/NORMAL 
xyouts,0.05,0.80,'MAX CURRENT (M/S)',size=1,/NORMAL
xyouts,0.20,0.80,vmax,size=1,/NORMAL 
xyouts,0.05,0.75,'VALID ',size=1,/NORMAL 
xyouts,0.15,0.75,yyyymmdd,size=1,/NORMAL   
xyouts,0.05,0.80,'GULF STREAM NORTH WALL +',size=1,/NORMAL 
;plots,-79.0,41.0,psym=1,symsize=1.0  
;xyouts,0.05,0.65,'nout',size=1,/NORMAL 
;plots,-79.0,40.0,psym=5,symsize=1.0
;
;-------------------------------------------------------------------------
; Set program so that a speed of 1 m/sec will be a vector of
; length 10 grid squares on the plot 
;ratio=10.0/1.0
;
; multiply the speeds at each point by this ratio and add a small
; constant to ua to avoid division by zero in the atan function  
;for j=0,jm-1 do begin
;for i=0,im-1 do begin
;ua(i,j)=ua(i,j)*ratio  + 0.0001 
;va(i,j)=va(i,j)*ratio
;endfor
;endfor
;---------------------------------------------------------------------------------
; convert current speeds to magnitudes in device coordinates 
; set ratio so that a maximum windspeed of 2 m/s will 
; be a vector of length 100. pixels on screen 
; or 2 cm on paper 
if(ptype eq 'x') then ratio=100.0/2.0
if(ptype eq 'ps') then ratio=2000./2.0 
for j=0,jm-1 do begin
   for i=0,im-1 do begin
      if(mask(i,j) eq 0) then goto, skipratio
      ua(i,j)=ua(i,j)*ratio
      va(i,j)=va(i,j)*ratio
      skipratio:
   endfor
endfor
;
;---------------------------------------------------------------
; make a vector plot of the winds in device coordinates   
;
pi=3.1415926
;
for j=0,jm-1,5 do begin
   for i=0,im-1,5 do begin
      if(mask(i,j) eq 0) then goto, skipvectors 
      length=sqrt(ua(i,j)*ua(i,j)+va(i,j)*va(i,j)) ; length in device coord
; draw shaft of arrow  
      x0=xlon(i)
      y0=ylat(j)
      d=convert_coord([x0,y0],/data,/to_device) 
      x0=d(0)
      y0=d(1) 
      x1=x0+ua(i,j) 
      y1=y0+va(i,j)  
      plots,[x0,x1],[y0,y1],/DEVICE    
; draw the arrowheads in device coordinates 
theta=atan(va(i,j),ua(i,j)+0.0001)   ; avoid division by 0 in atan function
; length of arrowhead will be 0.2 times length of vector
length=0.2*length 
; draw left side of arrowhead
angle=theta+(150.0*pi/180.0)   
x2=x1+length*cos(angle) 
y2=y1+length*sin(angle)  
plots,[x1,x2],[y1,y2],/DEVICE 
; draw right side of arrowhead
angle=theta+(210.0*pi/180.0) 
x3=x1+length*cos(angle)
y3=y1+length*sin(angle) 
plots,[x1,x3],[y1,y3],/DEVICE  
      skipvectors:   
   endfor
endfor 
;------------------------------------------------------------------------------------
; plot grid outline
; plot vertical cell boundaries 
for i=0,im-2 do begin
 for j=0,jm-1 do begin
  if((mask(i,j) eq 0) and (mask(i+1,j) eq 1)) then begin
   plots,[xbndry(i+1),xbndry(i+1)],[ybndry(j),ybndry(j+1)],thick=2 
  endif
  if((mask(i,j) eq 1) and (mask(i+1,j) eq 0)) then begin
   plots,[xbndry(i+1),xbndry(i+1)],[ybndry(j),ybndry(j+1)],thick=2 
  endif  
 endfor
endfor
;
; plot horozontal cell boundaries   
for i=0,im-1 do begin
 for j=0,jm-2 do begin  
  if((mask(i,j) eq 0) and (mask(i,j+1) eq 1)) then begin
   plots,[xbndry(i),xbndry(i+1)],[ybndry(j+1),ybndry(j+1)],thick=2 
  endif
  if((mask(i,j) eq 1) and (mask(i,j+1) eq 0)) then begin
   plots,[xbndry(i),xbndry(i+1)],[ybndry(j+1),ybndry(j+1)],thick=2 
  endif
 endfor
endfor   
;
;------------------------------------------------------------------------------
; plot the water level array 
contour,wl,xlon,ylat,  $   
  levels=[-0.5,-0.4,-0.3,-0.2,-0.1,0.0,0.1,0.2,0.3,0.4,0.5],   $
 c_linestyle=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  $
    c_labels=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],   $      
 xstyle=1,ystyle=1, $ 
 xmargin=[0,0],ymargin=[0,0] ,$  
 xrange=[-83.00,-49.95],yrange=[26.30,47.30] 
;
; overplot the 'clean.yyyymmdd' file Gulf Stream 
for k=0,num_clean-1 do begin
plots,clean_lon(k),clean_lat(k),psym=1,symsize=1.0  
endfor  
;
; overplot the 'nout.yyyymmdd' file Gulf Stream 
if (num_nout > 1) then begin
for k=0,num_nout-1 do begin
plots,nout_lon(k),nout_lat(k),psym=5,symsize=1.0  
endfor  
endif
;
xyouts,0.05,0.95,'NOAA REGIONAL OCEAN FORECAST SYSTEM (ROFS)',size=1,/NORMAL
xyouts,0.05,0.90,'STANDARD GRID',size=1,/NORMAL  
xyouts,0.05,0.85,'SURFACE HEIGHT (M)',size=1,/NORMAL 
xyouts,0.05,0.75,'VALID ',size=1,/NORMAL 
xyouts,0.15,0.75,yyyymmdd,size=1,/NORMAL   
xyouts,0.05,0.80,'GULF STREAM NORTH WALL +',size=1,/NORMAL 
;plots,-79.0,41.0,psym=1,symsize=1.0  
;xyouts,0.05,0.65,'nout',size=1,/NORMAL 
;plots,-79.0,40.0,psym=5,symsize=1.0
;
; plot grid outline
; plot vertical cell boundaries 
for i=0,im-2 do begin
 for j=0,jm-1 do begin
  if((mask(i,j) eq 0) and (mask(i+1,j) eq 1)) then begin
   plots,[xbndry(i+1),xbndry(i+1)],[ybndry(j),ybndry(j+1)],thick=2 
  endif
  if((mask(i,j) eq 1) and (mask(i+1,j) eq 0)) then begin
   plots,[xbndry(i+1),xbndry(i+1)],[ybndry(j),ybndry(j+1)],thick=2 
  endif  
 endfor
endfor
;
; plot horozontal cell boundaries   
for i=0,im-1 do begin
 for j=0,jm-2 do begin  
  if((mask(i,j) eq 0) and (mask(i,j+1) eq 1)) then begin
   plots,[xbndry(i),xbndry(i+1)],[ybndry(j+1),ybndry(j+1)],thick=2 
  endif
  if((mask(i,j) eq 1) and (mask(i,j+1) eq 0)) then begin
   plots,[xbndry(i),xbndry(i+1)],[ybndry(j+1),ybndry(j+1)],thick=2 
  endif
 endfor
endfor   
;
;-------------------------------------------------------------------------------
if(ptype eq 'ps') then device,/close 
;spawn,' lpr -Pbfps.109 idl.ps '   
end 
