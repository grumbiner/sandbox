pro box, x0, y0, x1, y1, color
        polyfill, [x0,x0,x1,x1], [y0,y1,y1,y0], col=color,/normal
end
;-------------------------------------------------------------------
set_plot,'ps'
device,landscape=1
device,filename='plot_gs_rings.ps'
;--------------------------------------------------------
; determine name of input data file
YYYYMMDD='        ' 
eddy_name='   '  
print,'Type in name eddy ring file, eg eddy_05.dat:'
read,eddy_name,format='(a)'
print,eddy_name,format='(a)' 
;----------------------------------------------------------
; bottitle1='Two days worth of observations  are used in the COFS data assimilation'
bottitle1='     ' 
bottitle3=' '
; create color table
;-------------------------------------------------------------------------
; modifide Nov. 11, 2001 by William O'Connor to put date of data on plot  
; ---------------------------------------------------------------------------------------------
;                                  Water Temperature  (deg C)
;             5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24   25
;             2   4   6   8  10  12  14  16  18  20  22  24  26  28  30  32  34  36
; ----------------------------------------------------------------------------------------------
;                                           Levels
;          |-Purp-------|-----Blue------|--Green-|-Yel------|-Bwn----|-Or|--Red ------|Mag |blk
;         0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20   21
; -----------------------------------------------------------------------------------------------------
;
rcolor=[255,175,175,120,  0,  0,  0, 30,  0,  0,115,255,255,255,255,198,255,255,255,170,255,  0]
gcolor=[255, 75, 10, 10,  0,150,195,255,160,240,255,255,200,140,122, 81, 85, 52,  0,  0,  0,  0]
bcolor=[255,255,255,255,255,255,255,255,  0,  0,  0,  0, 75,  0,  0,  0,  0,  0,  0,  0,190,  0]

;
;
tvlct, rcolor, gcolor, bcolor
;
device,/color
;
masking=''
header1=''
header2=''
header3=''
header4=''
header5=''
header6=''
header7=''
header8=''
;
;
map_set,/merc,lim=[20,-85,50,-45],title=title2,color=21, /cont
map_continents,/usa,color=21
;
xyouts,-87.0,50.7,title2,color=21,charsize=1.5
;
; US only (high resolution coastline)
;map_set,/merc,lim=[25,-83,50,-49],title='USA',/usa
;
; Plot symbol marking staTION location
  a=findgen(16)*(!PI*2/16.)
  usersym, cos(a),sin(a),/fill
;
; 
;
; ----------------------- ------------------------------------------
; PLot raw sstobs 
;
i=-1
iyr=0000
mon=00
ida=00
ihr=00
imin=00
ityp=000
itype=0
obid=' '

; ------------------------------------------------------------------
; Determine month
;  mon=10
;  ida1=14
;  ida2=15
;  iyr=1998
dateheader=' '
if(mon eq 1)then month='January'
if(mon eq 2)then month='February'
if(mon eq 3)then month='March'
if(mon eq 4)then month='April'
if(mon eq 5)then month='May'
if(mon eq 6)then month='June'
if(mon eq 7)then month='July'
if(mon eq 8)then month='August'
if(mon eq 9)then month='September'
if(mon eq 10)then month='October'
if(mon eq 11)then month='November'
if(mon eq 12)then month='December'
;
; --------------------- Header -------------------------------
map_grid,latlab=-45,lonlab=20.5,latdel=2.0, londel=2.0, color=21, /label
;
; white out for titles
box, 0.03, 0.855, 0.95, 0.95, 0
;
toptitle2='!6NAVY WARM/COLD CORE RINGS JAN - SEP 2003'
xyouts,-77.0, 49.3,toptitle2, charsize=1.4, color=5
xyouts, -68.0, 28.0,'Gulf Stream Ring Position',charsize=1.0, color=21

; botcredits='NOAA/NWS/NCEP/EMC/MMAB'
; xyouts,-53.5,21.5,botcredits,charsize=0.8,color=5
;
; Plot location of Bermuda
plots, -64.75, 32.41, psym=4, symsize=1.0, color=21
xyouts,-64.7, 32.41, 'Bermuda', charsize=0.6, color=21
;
; ----------------------- Mean Gulf Stream Position --------------
;D = {gulf, lat:0.0, lon:0.0}
;xx=fltarr(31)
;yy=fltarr(31)
;
;gulfpos = replicate(D,31)
;;
;openr,33,'/migr/data/cfspom/data/oceanclimo/meangulfpos.loc'
;readf,33,format='(f5.2,1x,f5.1)',gulfpos
;;
;;
;for i=0,31-1 do begin
;  xlon = gulfpos(i).lon * (-1.0)
;  xlat = gulfpos(i).lat
;;  print, i, xlat, xlon
;  xx(i)=xlon
;  yy(i)=xlat
;endfor
;
;; draw line
;  plots, xx, yy, color=4
;;
;
;
;gulftitle1='5-Yr Mean'
;gulftitle2='Gulf Stream'
;gulftitle3='Landward Sfc Edge'
;xyouts,-49.6, 40.2, gulftitle1,charsize=0.7, color=21
;xyouts,-49.6, 39.6, gulftitle2,charsize=0.7, color=21
;xyouts,-49.6, 39.1, gulftitle3,charsize=0.7, color=21
; -------------------------------------------------------------------
;
bottitle2='Temp. model - Temp. obs. (0.01 C)'
;xyouts,-68.0,27.2,bottitle2,charsize=.9, color=21
;
;------------------------------------------------------------
; open the file with eddy positions are read the data   
no_eddy_obs=1
read,no_eddy_obs 
print,'no_eddy_obs = ',no_eddy_obs,format='(a25,i5)'

eddy_label='    ' 
datek='    '   
elon=fltarr(no_eddy_obs)
elat=fltarr(no_eddy_obs)
ob_date=strarr(no_eddy_obs) 
; 
openr,12,eddy_name
for i=0,no_eddy_obs-1 do begin
readf,12,datek,eddy_label,elatk,elonk,format='(a8,8x,a12,9x,f5.1,1x,f4.1)'  
print,datek,eddy_label,elatk,elonk,format='(a8,8x,a12,9x,f5.1,1x,f4.1)'  
ob_date(i)=datek 
elat(i)=elatk
elon(i)=-elonk 
endfor  
close,12
;
; plot the eddy locations 
;for i=0,no_eddy_obs-1 do begin
 oplot,elon,elat,linestyle=0,color=21,psym=-7,symsize=0.3 
; plots,[elon(i),elat(i)],psym=7,symsize=0.3,color=21
;endfor  
;
; plot starting point with X
plots,[elon(0),elat(0)],psym=7,symsize=1,color=21
;
xyouts,-66.0,27.0,eddy_name,charsize=1,color=21
xyouts,-76.0,26.0,'first date',charsize=1,color=21
xyouts,-72.0,26.0,ob_date(0),charsize=1,color=21 
xyouts,-65.0,26.0,'first position',charsize=1,color=21
xyouts,-58.0,26.0,elat(0),charsize=1,color=21 
xyouts,-54.0,26.0,elon(0),charsize=1,color=21 
;
xyouts,-76.0,25.0,'last date',charsize=1,color=21
xyouts,-72.0,25.0,ob_date(no_eddy_obs-1),charsize=1,color=21 
xyouts,-65.0,25.0,'last position',charsize=1,color=21
xyouts,-58.0,25.0,elat(no_eddy_obs-1),charsize=1,color=21 
xyouts,-54.0,25.0,elon(no_eddy_obs-1),charsize=1,color=21 

xyouts,-68.0,23.0,'X = initial position',charsize=1,color=21 
;---------------------------------------------------------
close,/all
;
  DEVICE,/CLOSE
;
END
