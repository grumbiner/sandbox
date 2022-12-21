      SUBROUTINE readat(x, nz, nnx, nny, base, tag)
      IMPLICIT none

      INTEGER nx, ny, lrec
      INTEGER nnx, nny, nz
      PARAMETER (nx = 304)
      PARAMETER (ny = 448)
      PARAMETER (lrec = 3040)

      CHARACTER*1 line(nx), x(nz, nx, ny)

      CHARACTER*60 base, tag
      CHARACTER*122 infile
      INTEGER i, j, iunit, day, ier
C================================================================
      iunit = 31           
      DO 1000 day = 1, nz
       IF (day .LT. 10) THEN
         WRITE (infile,9001) base, day, tag
 9001    FORMAT (A60,'0',I1,A60)
       ELSE
         WRITE (infile,9002) base, day, tag
 9002    FORMAT (A60,I2,A60)
       ENDIF
       CALL blnkrm(infile)
       OPEN (unit=iunit,file=infile,status='old',      
     +      form='binary',recl=lrec,iostat=ier)       
             if (ier.ne.0) then                   
                 print *,infile, ' not accessible!'  
             endif                                
        DO 2000 j = 1, ny
          READ (iunit) line
          DO 2100 i = 1, nx
            x(day, i, j) = line(i)
 2100     CONTINUE
 2000   CONTINUE

        CLOSE (iunit)
 1000 CONTINUE

      RETURN
      END
