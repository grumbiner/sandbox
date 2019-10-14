;-------------------------------------------------------------
PRO make_scaling,im,jm,alat,xs,ys,ptype,debug
; set up plot scaling
; calculate proportions in east-west and north-south 
imm1=im-1
jmm1=jm-1 
avglat=0.5*(alat(0,0)+alat(0,jmm1))
if(debug eq 1) then begin
print,'avglat = ',avglat,format='(a15,f8.2)' 
endif 
circ=2.*3.14159*6371.*cos(avglat*3.14159/180.)  
deltax=(circ/360.)/30.
xside=imm1*deltax
deltay=111./30.
yside=jmm1*deltay 
if(debug eq 1) then begin
print,'xside = ',xside,format='(a10,f10.2)'
print,'yside = ',yside,format='(a10,f10.2)' 
endif 
aspect=  yside/xside  
if(debug eq 1) then begin
print,'aspect = ',aspect,format='(a15,5x,f10.4)'
endif
;
; slightly elongated east west case  
; will be plotted on landscape on paper
isize=1024. 
jsize=isize*aspect 
if(debug eq 1) then begin
print,'isize = ',isize,format='(a15,5x,f10.4)' 
print,'jsize = ',jsize,format='(a15,5x,f10.4)' 
endif
if(ptype eq 'x') then window,0,xsize=isize,ysize=jsize
xs=7.0/aspect  
ys=7.0
if(debug eq 1) then begin
print,'landscape on paper',format='(a20)'  
print,'xsize = ',xs,format='(a15,5x,f10.4)'
print,'ysize = ',ys,format='(a15,5x,f10.4)'  
endif 
;
return
end 
;---------------------------------------------------------------
