;---------------------------------------------------------------------------
PRO plot_stairstep_bndry,im,jm,fsm,alatLL,alonLL
; plot land sea stairstep grid outline
; plot north-south boundaries
for i=0,im-2 do begin
 for j=0,jm-2 do begin
  if((fsm(i,j) eq 0) and (fsm(i+1,j) eq 1 )) then begin
   plots,[alonLL(i+1,j),alonLL(i+1,j+1)],[alatLL(i+1,j),alatLL(i+1,j+1)],thick=2 
  endif
   if((fsm(i,j) eq 1) and (fsm(i+1,j) eq 0)) then begin
   plots,[alonLL(i+1,j),alonLL(i+1,j+1)],[alatLL(i+1,j),alatLL(i+1,j+1)],thick=2 
  endif  
 endfor
endfor
; plot east-west boundaries 
for i=0,im-2 do begin
 for j=0,jm-2 do begin  
  if((fsm(i,j) eq 0 ) and (fsm(i,j+1) eq 1)) then begin
   plots,[alonLL(i,j+1),alonLL(i+1,j+1)],[alatLL(i,j+1),alatLL(i+1,j+1)],thick=2 
  endif
  if((fsm(i,j) eq 1 ) and (fsm(i,j+1) eq 0 )) then begin
   plots,[alonLL(i,j+1),alonLL(i+1,j+1)],[alatLL(i,j+1),alatLL(i+1,j+1)],thick=2 
  endif
 endfor
endfor   
;
return
end 
;-----------------------------------------------------------
