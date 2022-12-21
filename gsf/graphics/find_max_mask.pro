;---------------------------------------------------------------
; file find_max_mask.pro 
; IDL proceedure finds maximum value of 2-D array with mask  
;---------------------------------------------------------------
PRO find_max_mask,h,fsm,im,jm,dmax,imax,jmax
;-----------------------------------
; h    =fltarr(im,jm)
; fsm  =intarr(im,jm)   mask array  
; im   = first array dimension
; jm   = second array dimension
; dmax = max value 
;-----------------------------------
imm1=im-1
jmm1=jm-1 
dmax=-1.e20
for j=0,jmm1 do begin
for i=0,imm1 do begin
if (fsm(i,j) eq 0) then goto, skipcomp
if (h(i,j) gt dmax) then begin  
  dmax=h(i,j)  
  imax=i
  jmax=j
endif 
skipcomp:   
endfor
endfor  
; print,'dmax = ',dmax,format='(a10,f8.0)' 
return
end 
;----------------------------------------------------------------
