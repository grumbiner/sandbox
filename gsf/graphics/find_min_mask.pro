;---------------------------------------------------------------
; file find_min_mask.pro 
; IDL proceedure finds minimum value of 2-D array with mask  
;---------------------------------------------------------------
PRO find_min_mask,h,fsm,im,jm,dmin,imin,jmin
;-----------------------------------
; h    =fltarr(im,jm)
; fsm  =intarr(im,jm)   mask array  
; im   = first array dimension
; jm   = second array dimension
; dmin = min value 
;-----------------------------------
imm1=im-1
jmm1=jm-1 
dmin=+1.e20   
for j=0,jmm1 do begin
for i=0,imm1 do begin
if (fsm(i,j) eq 0) then goto, skipcomp
if (h(i,j) lt dmin) then begin
   dmin=h(i,j)  
   imin=i
   jmin=j
endif    
skipcomp:   
endfor
endfor  
;print,'dmin = ',dmin,format='(a10,f8.0)' 
return
end 
;----------------------------------------------------------------
