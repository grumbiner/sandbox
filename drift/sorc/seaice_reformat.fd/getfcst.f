      SUBROUTINE getfcst(funit2, x0, y0, dir, dist, code, npts, ndays)
!     Read in both the skiles1 and skiles2 forecasts.
!     Robert Grumbine 21 April 1994.
!     Modified to work more easily as a subroutine 4 April 1995.
!     Updated for F90 and better behavior as sbr -- assume file is open,
!       Robert Grumbine 28 May 2014

      IMPLICIT none

!     Parameters for reading data in
      INTEGER npts, ndays, funit2, code

      INTEGER skpt2, count
      REAL dir(npts, ndays), dist(npts, ndays)
      REAL x0(npts), y0(npts)

      CHARACTER(80) header
      CHARACTER(1) trailer
      INTEGER i, fday

      code = 0

! Read in forecasts
 9020 FORMAT (A80)
      DO fday = 1, ndays
        count = 0
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        READ (funit2,9020, END=9200, ERR=9200) header 
        DO i = 1, 207
          READ (funit2, *, END=9200, ERR=9200) skpt2, dir(i,fday), 
     1           dist(i,fday)
          count = count + 1
        ENDDO
        i = 207
        READ (funit2, 9020, END=9200, ERR=9200) header

        !Loop through all the ice edge points, until seeing blank line
        !(err)
 1001   CONTINUE
        i = i + 1
        count = count + 1
!CD        PRINT *,'starting to loop in getfcst'
        READ (funit2,9020, END=9200, ERR=9200) header
        READ ( header, 9007, ERR=1002) skpt2, x0(i), y0(i), 
     1           dir(i,fday), dist(i,fday)
        !WRITE (*,*) skpt2, x0(i), y0(i), dir(i,fday), dist(i,fday)
        IF (skpt2 .NE. 0) GO TO 1001

 1002   CONTINUE
 
        !read in the second blank line
        READ (funit2,9020, END=9200, ERR=9200) trailer
        PRINT *,'day ',fday,' count = ',count 
      ENDDO

!==========================================
 9007   FORMAT (I5, 3x, 2F8.3, 2x, 2F6.1)

!     The 1 shift is because 1 is added to fday, count before the attempt to read
      code = fday - 1
      npts = count - 1
!      DO i = 1, npts
!        WRITE (*,*) 'getfcst ',i,dir(i,1), dist(i,1), dir(i,5),
!     1                      dist(i,5)
!      ENDDO

      RETURN

 9200 CONTINUE

      code = -fday 
      npts = count - 1
      RETURN

 9999 CONTINUE
      PRINT *,'Failed to open the skiles points file'
      STOP

      END
