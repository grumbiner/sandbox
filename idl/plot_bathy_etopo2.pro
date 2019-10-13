; include proceedures files 
@make_scaling.pro
@make_land_mask.pro
@find_max_mask.pro 
@plot_stairstep_bndry.pro
@find_lonlat_corners.pro 
;----------------------------------------------------------------
; program plot_bathy_etopo2.pro 
; this program plots bathymetry sectioned from ETOPO2 dataset 
;--------------------------------------------------------------
; set debugging switch
 debug=1         ; print debugging mesages  
;debug=0         ; no not print debugging messages 
cregion='   '   ; define as character   
clabel ='   '   ; define as character 
header ='   '   ; define as character 
;-----------------------------------------------------
; ask for input header of file
print,'Enter 10 character header name of file:'
read,header,format='(a10)' 
; open the reader file and read information
openr,5,header+'.txt'
readf,5,cregion,format='(a30)'
readf,5,clabel,format='(a10)'
readf,5,im,jm,wlon,elon,slat,nlat,format='(2i5,4f10.4)'  
close,5
;---------------------------------------------------------------
print,im,jm,wlon,elon,slat,nlat,format='(2i5,4f10.4)' 
print,'im = ',im,format='(a15,5x,i5)'  
print,'jm = ',jm,format='(a15,5x,i5)'  
;
;----------------------------------------------------------------------------------
; dimension the arrays  
imm1=im-1
jmm1=jm-1 
;
h =fltarr(im,jm)  
topog=fltarr(im,jm)  
alon=fltarr(im,jm)
alat=fltarr(im,jm)  
alonLL=fltarr(im,jm)
alatLL=fltarr(im,jm)  
fsm=intarr(im,jm)  
;
;---------------------------------------------------------------------------------
; open and read the data file  
openr,10,clabel+'.dat',/f77_unformatted 
readu,10,alon,alat,h 
close,10
;-----------------------------------------------------------------
; define land topography as negative of h
topog=-h 
;-------------------------------------------------------------------
if (debug eq 1) then begin
; print the lon/lat at center of four grid around domain 
print,'i = ',0,' j = ',0,' alon = ',alon(0,0),' alat = ',alat(0,0), $
      ' h = ',h(0,0),format='(a5,i5,a5,i5,a10,f7.2,a10,f7.2,a5,f10.0)' 
print,'i = ',0,' j = ',jmm1,' alon = ',alon(0,jmm1),' alat = ',alat(0,jmm1), $
      ' h = ',h(0,jmm1),format='(a5,i5,a5,i5,a10,f7.2,a10,f7.2,a5,f10.0)' 
print,'i = ',imm1,' j = ',0,' alon = ',alon(imm1,0),' alat = ',alat(imm1,0), $
      ' h = ',h(imm1,0),format='(a5,i5,a5,i5,a10,f7.2,a10,f7.2,a5,f10.0)' 
print,'i = ',imm1,' j = ',jmm1,' alon = ',alon(imm1,jmm1),' alat = ',alat(imm1,jmm1), $
      ' h = ',h(imm1,jmm1),format='(a5,i5,a5,i5,a10,f7.2,a10,f7.2,a5,f10.0)' 
endif 
;--------------------------------------------------------------------
; find the lon/lat at the lower left corner of each grid to be
; use for plotting stairstep boundaries
find_lonlat_corners,im,jm,debug,alon,alat,alonLL,alatLL  
;--------------------------------------------------------------------------------- 
; set plot type
ptype=' '
print,'enter plot type (x,ps,tek):'
read,ptype
set_plot,ptype
;
; call proceedure to set up plot scaling 
make_scaling,im,jm,alat,xs,ys,ptype,debug 
if(ptype eq 'ps') then device,ysize=ys,xsize=xs, $ 
           filename='plot_bathy_etopo2.ps',        $ 
          /inch,xoffs=1.0,yoffs=10.0,/landscape 
;
; call proceedure to make land mask
min_allowed_depth=1.0  ; minimum ocean depth allowed, else set to land
make_land_mask,h,fsm,im,jm,min_allowed_depth 
;  
; call proceedure to find maximum depth 
find_max_mask,h,fsm,im,jm 
;-----------------------------------------------------------
; open and read file with depth contours
openr,22,'idl_contours.in' 
readf,22,nocean      ; read number of ocean depth contours 
print, 'nocean = ',nocean,format='(a15,i5)'
depth_levels=fltarr(nocean)
; read the depth contours
for k=0,nocean-1 do begin
readf,22,aplace
depth_levels(k)=aplace 
endfor
;
readf,22,nland       ; read number of land contours 
print, 'nland = ',nland,format='(a15,i5)'
if(nland eq 0) then goto, skipland
land_levels=fltarr(nland) 
for k=0,nland-1 do begin
readf,22,aplace
land_levels(k)=aplace 
endfor
skipland:  
close,22 
;------------------------------------------------------------
;  contour the depth array htop 
contour,h,alon,alat,  $   
  levels=depth_levels, $ 
  c_linestyle=[1,1,1,1,1,1,1,1],  $
  c_labels=[1,1,1,1,1,1],   $      
  xstyle=1,ystyle=1, $ 
  xmargin=[0,0],ymargin=[0,0], $  
  xrange=[alon(0,jmm1),alon(imm1,0)],yrange=[alat(0,0),alat(imm1,jmm1)]
;
; contour the land array topog if specified 
if (nland eq 0) then goto,skipland2
contour,topog,alon,alat,  $   
   levels=land_levels, $ 
   c_linestyle=[1,1,1,1,1,1,1,1],  $
   c_labels=[1,1,1,1,1,1],   $      
   xstyle=1,ystyle=1, $ 
xmargin=[0,0],ymargin=[0,0], $  
xrange=[alon(0,jmm1),alon(imm1,0)],yrange=[alat(0,0),alat(imm1,jmm1)], $
/over
skipland2:
;---------------------------------------------------------------------------
; plot the stairstep land sea boundary 
plot_stairstep_bndry,im,jm,fsm,alatLL,alonLL
;-----------------------------------------------------------
xyouts,0.25,-0.05,cregion,size=1.0,/NORMAL 
;
if(ptype eq 'ps') then device,/close 
;spawn,' lpr -Pphaser4  plot_bathy_etopo2.ps '   
end 
