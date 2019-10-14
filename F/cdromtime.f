      program opentim          
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c      
                       
      IMPLICIT none

      INTEGER nx, ny, lrec
      PARAMETER (nx = 304)
      PARAMETER (ny = 448)
      PARAMETER (lrec = 3040)
      CHARACTER*1 icecon(nx, ny)
      CHARACTER*60 infile                                     
      INTEGER day, ier, iunit, count
C================================================================              

      PRINT *,'Begin timing'
      DO 1001 count = 1, 80
      DO 1000 day = 1, 25
       IF (day .LT. 10) THEN
         WRITE (infile,9001) day
 9001    FORMAT ('c:\models\ssmi\time\91120',I1,'.tot')
       ELSE
         WRITE (infile,9002) day
 9002    FORMAT ('c:\models\ssmi\time\9112',I2,'.tot')
       ENDIF
       iunit = 21           
       open (unit=iunit,file=infile,status='old',      
     +      form='binary',recl=lrec,iostat=ier)       
             if (ier.ne.0) then                   
                 print *,infile, ' not accessible!'                   
                 stop                   
             endif                                
        READ (iunit) icecon
        CLOSE (21)
 1000 CONTINUE
 1001 CONTINUE
c                                   
      PRINT *,'Stop'
      STOP
      END
