;---------------------------------------------------------------
; file make_land_mask.pro
PRO make_land_mask,h,fsm,im,jm,min_allowed_depth 
; define the land mask array 
for j=0,jm-1 do begin
for i=0,im-1 do begin
fsm(i,j)=1 
if(h(i,j) lt min_allowed_depth) then fsm(i,j)=0  
endfor
endfor
; 
; set land values of scalar to special value to avoid plotting
for j=0,jm-1 do begin
  for i=0,im-1 do begin
    if(fsm(i,j) eq 0 ) then h(i,j)=!values.f_nan 
  endfor
endfor
;  
return
end 
;---------------------------------------------------------------
