;--------------------------------------------------------------------
; file find_lonlat_corners.pro
PRO find_lonlat_corners,im,jm,debug,alon,alat,alonLL,alatLL 
; determine the lon and lat at the lower left of grid square
; general interior case 
imm1=im-1
jmm1=jm-1 
for j=1,jmm1 do begin
for i=1,imm1 do begin
alonLL(i,j)=0.25*(alon(i,j)+alon(i,j-1)+alon(i-1,j)+alon(i-1,j-1)) 
alatLL(i,j)=0.25*(alat(i,j)+alat(i,j-1)+alat(i-1,j)+alat(i-1,j-1)) 
endfor
endfor  
; special case south boundary    
for i=1,imm1 do begin
alonLL(i,0)=0.5*(alon(i,0)+alon(i-1,0)) 
alatLL(i,0)=alat(i,0)-0.5*(alat(i,1)-alat(i,0)) 
endfor 
;
; special case for west boundary
for j=1,jmm1 do begin
alatLL(0,j)=0.5*(alat(0,j)+alat(0,j-1)) 
alonLL(0,j)=alon(0,j)-0.5*(alon(0,j-1)-alon(0,j)) 
endfor  
;
; special case for SW corner
alatLL(0,0)=alat(0,0)-0.5*(alat(0,1)-alat(0,0)) 
alonLL(0,0)=alon(0,0)-0.5*(alon(1,0)-alon(0,0)) 
; 
if(debug eq 1) then begin 
; print the lon/lat at Lower Left of four grid around domain 
print,'i = ',0,' j = ',0,' alonLL = ',alonLL(0,0),' alatLL = ',alatLL(0,0), $
               format='(a5,i3,a5,i3,a10,f7.2,a10,f7.2)' 
print,'i = ',0,' j = ',jmm1,' alonLL = ',alonLL(0,jmm1),' alatLL = ',alatLL(0,jmm1), $
               format='(a5,i3,a5,i3,a10,f7.2,a10,f7.2)' 
print,'i = ',imm1,' j = ',0,' alonLL = ',alonLL(imm1,0),' alatLL = ',alatLL(imm1,0), $
               format='(a5,i3,a5,i3,a10,f7.2,a10,f7.2)' 
print,'i = ',imm1,' j = ',jmm1,' alonLL = ',alonLL(imm1,jmm1),' alatLL = ',alatLL(imm1,jmm1), $
               format='(a5,i3,a5,i3,a10,f7.2,a10,f7.2)' 
print,'i = ',1,' j = ',1,' alonLL = ',alonLL(1,1),' alatLL = ',alatLL(1,1), $
               format='(a5,i3,a5,i3,a10,f7.2,a10,f7.2)' 
endif 
;
return
end 
;--------------------------------------------------------------------------------- 
