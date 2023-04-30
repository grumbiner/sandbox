      PROGRAM getfreqs
      IMPLICIT none
C Extract a set of frequencies from a data set.  Specify by way of Doodson-like
C   numbers

C Derived variant: Read in data and transpose so that indices vary by t, i, j; rather
C    than i, j, t of the basic grib input.

C Time series
      !68668 = Jan 1 1962 to Dec 31 2008, 6 hourly
      INTEGER nt
      INTEGER nx, ny
      PARAMETER (nt = 68668)
      PARAMETER (nx =   192)
      PARAMETER (ny =    94)
      REAL :: x(nx, 32, nt)
      REAL :: tmp(nx, 94)
      REAL :: z(nt)

C Misc
      INTEGER i, todo, iindex, jindex, tmpj, k
      CHARACTER*70 fname, fname2, fname3


C ------------- Start executable section ------------------------------
C -------------- Get data to analyze
      READ (*,*) fname
      PRINT *,"fname = ",fname
      !Read in data 
      OPEN (12, FILE=fname, FORM="UNFORMATTED", STATUS="OLD") 


C----------------------------------------------------------------------------
      ! Note that the splitting of 1-32, 33-64, 65-94 is a limit imposed 
      !      by compiler (2Gb max array size), not algorithmic
      DO jindex = 1, 94
        PRINT *,'jindex = ',jindex
        IF (jindex .EQ. 1) THEN
          DO i = 1, nt
            READ (12) tmp
            !DEBUG PRINT *,i,MAXVAL(tmp), MINVAL(tmp)
            x(:,:,i) = tmp(:,1:32)
          ENDDO
        ELSE IF (jindex .EQ. 33) THEN
          REWIND(12)
          DO i = 1, nt
            READ (12) tmp
            x(:,:,i) = tmp(:,33:64)
          ENDDO
        ELSE IF (jindex .EQ. 65) THEN
          REWIND(12)
          DO i = 1, nt
            READ (12) tmp
            x(:,1:30,i) = tmp(:,65:94)
          ENDDO
        ENDIF

        IF (jindex .LE. 32) THEN
          tmpj = jindex
        ELSE IF (jindex .LE. 64) THEN
          tmpj = jindex - 32
        ELSE
          tmpj = jindex - 64
        ENDIF
        !  End of annoying splitting of original data

        PRINT *,'tmpj = ',tmpj
        DO iindex = 1, nx
          WRITE(fname3,9003) iindex, jindex 
          OPEN (13,FILE=fname3, FORM="UNFORMATTED", STATUS="NEW")
          DO k = 1, nt
            z(k) = x(iindex, tmpj, k)
          ENDDO 
          WRITE (13) z
          CLOSE(13)
        ENDDO !iindex
        

      ENDDO !jindex

 9003 FORMAT("trans.",I3.3,".",I2.2)

      END
