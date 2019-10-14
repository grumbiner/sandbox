PRO tvim,a
;
; Program reads IMAGE FROM THE [SSIPS] DIRECTORY
;  in the file [IDL.USIM]IMAGE.SST
;  than it reads the file in IDL array A with dimension 512x512 
;          and displays the image
;
BEL =STRING([7B,7B,7B])
;print,' '
;print,'Enter "TVIMAGE, a" if you want to work with the image further!'    
;print,' '                                                             
;
;b = intarr(512,512)   ; define array b   
a = bytarr(512,512)   ; define array A   
;
;  READ  IMAGE  FILE
;
;spawn,'run [ssips]readim';  run fortran program to read the image
openr,1,'trbl.512'
readu,1,a
CLOSE,1 
;
HELP,A
;a = byte(b)
;HELP,A
;
;    READ IMAGE ID, latmax, latmin, lonmin, lonmax, ETC.
;
;openr,2,filepath('ID.sst',subdir='usim')
;readF,2,Id, latmax, latmin, lonmin, lonmax,mon,day,yr,hr,min,orb,sat,RES  
;CLOSE,2   
;
latmin = 34.5  & latmax = 39.875
lonmin = -77.563  &  lonmax = -70.805
;print,' Image has been read in array A !'    
;   
 
;print,'ID has been read ! It = ',I    
;
; transpose and rotate the image in 270 degree
;a = rotate(a,7)
;print,'Image has been rotated !'     
; if temperature image, make it "negative"
;QQ = BYTE(255)
;if (Id eq 2) then a = QQ - a 
;
;
;r1:
;latmin = 34.      &   latmax = 36.
;lonmin =-77.      &   lonmax =-74.
;STR = STRARR(4)
;
;STR(0) = 'OVERLAY THE MAP ?'
;STR(1) = '      YES        '
;STR(2) = '      NO         '
;PRINT,BEL 
;COL = WMENU(STR,TITLE=0,INIT=1)
;    
;IF COL EQ 2 THEN GOTO, LN1  

;
latc = (latmin + latmax) / 2.
lonc = (lonmin + lonmax) / 2.
ndir = 0.
;
;
window,0,xsize=512,ysize=512
;
map_set, latc,lonc,ndir,/merc,/usa,limit=[latmin-5.,lonmin-5., $
latmax+5.,lonmax+5.],XMargin=[0,0],YMargin=[0,0],/NOBORDER                                             
A = MAP_IMAGE(A,LATMIN=LATMIN,LATMAX=LATMAX,LONMIN=LONMIN,LONMAX=LONMAX) 
      
;
tvSCL,a                         ; display the image     
PRINT,BEL 
map_set, latc,lonc,ndir,/merc,/usa,limit=[latmin-5.,lonmin-5., $
latmax+5.,lonmax+5.],XMargin=[0,0],YMargin=[0,0],/NOBORDER          
tv,a
;
; 
print,' '  
;if(id eq 1) then print,' VISIBLE image has been displayed !'   
;if(id eq 2) then print,' TEMPERATURE image has been displayed !'   
;

;Map_set, latc,lonc,ndir,/merc,/usa,color=3,limit=[latmin,lonmin,$
;latmax,lonmax],XMargin=[0,0],YMargin=[0,0],/NOBORDER,/NOERASE, $
;latdel = 1,londel = 1                                               


goto,lno
print,' '  
print,'Image has been navigated!'    
print,' '  
;
LN1:
STR(0) = 'LABLE THE IMAGE ?'
STR(1) = '      YES        '
STR(2) = '      NO         '
COL = WMENU(STR,TITLE=0,INIT=1)
;    
IF COL EQ 2 THEN GOTO, LNO  

STR(0) = 'COLOR OF THE UPPER LABLE'
STR(1) = '        BLACK      '
STR(2) = '        WHITE      '
STR(3) = '        GRAY       '
;
;
SLAMA = STRTRIM(LATMAX,2)    &   SLOMI = STRTRIM(LONMIN,2)   
I = STRPOS(SLAMA,'.')        &   J = STRPOS(SLOMI,'.') 
IN = I - 2                   &   JN = J - 2
SLAMAN = STRMID(SLAMA,IN,4)  &   SLOMIN = STRMID(SLOMI,JN,4)
MON = FIX(MON) & DAY = FIX(DAY) & YR = FIX(YR)
SMON =STRTRIM(MON,2) & SDAY = STRTRIM(DAY,2) & SYR = STRTRIM(YR,2)
ST1 = SLAMAN+'N '+SLOMIN+'W      ' & ST2 ='DATE='+SMON+'/'+SDAY+'/'+SYR
HR = FIX(HR) & MIN = FIX(MIN)
SHR=STRTRIM(HR,2) & SMIN = STRTRIM(MIN,2)
ST3='   TIME='+SHR+':'+SMIN+'Z'
PRINT,BEL 
COL = WMENU(STR,TITLE=0,INIT=1)
;
IF COL EQ 1 THEN COLR = 0    
IF COL EQ 2 THEN COLR = 255    
IF COL EQ 3 THEN COLR = 130    
PRINT,BEL

XYOUTS,2,495,ST1,CHARSIZE=2,CHARTHICK=2,COLOR= COLR,/DEVICE
;
PRINT,BEL
COL = WMENU(STR,TITLE=0,INIT=1)
;
IF COL EQ 1 THEN COLR = 0    
IF COL EQ 2 THEN COLR = 255    
IF COL EQ 3 THEN COLR = 130    
PRINT,BEL

XYOUTS,160,495,ST2,CHARSIZE=2,CHARTHICK=2,COLOR= COLR,/DEVICE
;
PRINT,BEL
COL = WMENU(STR,TITLE=0,INIT=1)
;
IF COL EQ 1 THEN COLR = 0    
IF COL EQ 2 THEN COLR = 255    
IF COL EQ 3 THEN COLR = 130    
PRINT,BEL

XYOUTS,320,495,ST3,CHARSIZE=2,CHARTHICK=2,COLOR= COLR,/DEVICE
;
; 
STR(0) = 'COLOR OF THE LOWER LABLE'
STR(1) = '       BLACK            '
STR(2) = '       WHITE            '
STR(3) = '       GRAY             '
;
PRINT,BEL
COL = WMENU(STR,TITLE=0,INIT=1)
;
IF COL EQ 1 THEN COLR = 0    
IF COL EQ 2 THEN COLR = 255    
IF COL EQ 3 THEN COLR = 130    
PRINT,BEL
;
SLAMI = STRTRIM(LATMIN,2)    &   SLOMA = STRTRIM(LONMAX,2)   
I = STRPOS(SLAMI,'.')        &   J = STRPOS(SLOMA,'.') 
IN = I - 2                   &   JN = J - 2
SLAMIN = STRMID(SLAMI,IN,4)  &   SLOMAN = STRMID(SLOMA,JN,4)
ORB = FIX(ORB) 
SRES = STRTRIM(RES,2) & I = STRPOS(SRES,'.') & IN = I - 1 
SRES = STRMID(SRES,IN,4)
SORB =STRTRIM(ORB,2)
ST1 = 'ORBIT = '+SORB
ST2 = '    RES = '+SRES+'KM   '
ST3 = SLAMIN+'N '+SLOMAN+'W'
XYOUTS,2,3,ST1,CHARSIZE=2,CHARTHICK=2,COLOR= COLR,/DEVICE     
PRINT,BEL
COL = WMENU(STR,TITLE=0,INIT=1)
;
IF COL EQ 1 THEN COLR = 0    
IF COL EQ 2 THEN COLR = 255    
IF COL EQ 3 THEN COLR = 130    
PRINT,BEL
XYOUTS,152,3,ST2,CHARSIZE=2,CHARTHICK=2,COLOR= COLR,/DEVICE     
;
PRINT,BEL
COL = WMENU(STR,TITLE=0,INIT=1)
;
IF COL EQ 1 THEN COLR = 0    
IF COL EQ 2 THEN COLR = 255    
IF COL EQ 3 THEN COLR = 130    
PRINT,BEL
XYOUTS,375,3,ST3,CHARSIZE=2,CHARTHICK=2,COLOR= COLR,/DEVICE     
;
LNO:
;
;
a = tvrd()
;print, latc,lonc,latmin,lonmin,latmax,lonmax
END      
 
