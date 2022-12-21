      PROGRAM reform
C     Program to read in pc formatted files and write out as unformatted.
C     Base version from acor3.for 12-08-93.
C     
C     Subroutines:
C       Readat - 12-09-93 Read in a month's worth of nsidc maps      
C       Writ   - 12-09-93 Write out the month of reanalyzed maps

      IMPLICIT none
      
      INTEGER nday, nx, ny
      PARAMETER (nday =   1)
      PARAMETER (nx   = 304)
      PARAMETER (ny   = 448)
      CHARACTER*1 x(nday, nx, ny)

      CHARACTER*60 base, tag

      PRINT *,'What is the base name of the input file?'
      READ (*,9009) base
      PRINT *,'What is the file tag?'
      READ (*,9009) tag
 9009 FORMAT (A60) 
      
      CALL readat(x, nday, nx, ny, base, tag)

      PRINT *,'What is the base name for the output files?'
      READ (*,9009) base
      PRINT *,'What is the tag for the output files?'
      READ (*,9009) tag
      CALL writ(x, nday, nx, ny, base, tag)


      STOP
      END

      SUBROUTINE readat(x, nz, nnx, nny, base, tag)
      IMPLICIT none

      INTEGER nx, ny, lrec
      INTEGER nnx, nny, nz
      PARAMETER (nx = 304)
      PARAMETER (ny = 448)
      PARAMETER (lrec = 3040)

      CHARACTER*1 icecon(nx, ny), x(nz, nx, ny)

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
          DO 2100 i = 1, nx
            READ (iunit) x(day, i, j)
 2100     CONTINUE
 2000   CONTINUE


        CLOSE (iunit)
 1000 CONTINUE

      RETURN
      END

      SUBROUTINE writ(x, nz, nnx, nny, base, tag)
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

      DO 1000 day = 1, nz
       iunit = 20+day
       IF (day .LT. 10) THEN
         WRITE (infile,9001) base, day, tag
 9001    FORMAT (A60,'0',I1,A60)
       ELSE
         WRITE (infile,9002) base, day, tag
 9002    FORMAT (A60,I2,A60)
       ENDIF
       CALL blnkrm(infile)
       OPEN (unit=iunit,file=infile,status='new',
     +      form='binary', iostat=ier)       
             if (ier.ne.0) then                   
                 print *,infile, ' not accessible!'  
             endif                                
        DO 1100 j = 1, ny
          DO 1200 i = 1, nx
            line(i) = x(day, i, j) 
 1200     CONTINUE
          WRITE (iunit) line
 1100   CONTINUE

        CLOSE (iunit, STATUS='KEEP')
 1000 CONTINUE

      RETURN
      END


      SUBROUTINE blnkrm(infile)
      INTEGER i, j
      CHARACTER*122 infile, outfile

      j = 0
      DO 1000 i = 1, 122
        IF (infile(i:i) .NE. ' ') THEN
          j = j + 1
          outfile(j:j) = infile(i:i)
        ENDIF
 1000 CONTINUE

      infile = outfile
      PRINT *,'in blnkrm, outfile = ',outfile

      RETURN
      END
