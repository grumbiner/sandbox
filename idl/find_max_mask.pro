;---------------------------------------------------------------
PRO find_max_mask,h,fsm,im,jm
; find maximum value of 2-D array with mask  
imm1=im-1
jmm1=jm-1 
dmax=-9999. 
for j=0,jmm1 do begin
for i=0,imm1 do begin
if (fsm(i,j) eq 0) then goto, skipcomp
if (h(i,j) gt dmax) then dmax=h(i,j)  
skipcomp:   
endfor
endfor  
print,'dmax = ',dmax,format='(a10,f8.0)' 
return
end 
;----------------------------------------------------------------
