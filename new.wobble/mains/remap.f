      PROGRAM remap
!read in output of an 'extract' on a T62 grid and product amplitude, phase files for each 
!  frequency

      IMPLICIT none

      INTEGER nx, ny, nf
      PARAMETER (nx = 192)
      PARAMETER (ny =  94)
      PARAMETER (nf =  20)
      DOUBLE PRECISION ampl(nx, ny, nf)
      DOUBLE PRECISION phase(nx, ny, nf)

      DOUBLE PRECISION freq(nf)
      REAL tmp(nx, ny)
      INTEGER i, j, k
      INTEGER ti, tj
      DOUBLE PRECISION f, a, dummy
      CHARACTER*80 fname
      
      OPEN (10, FILE="utrim", FORM="FORMATTED", STATUS="OLD")
      DO j = 1, ny
      DO i = 1, nx
      DO k = 1, nf
        READ(10,*) ti, tj, freq(k), ampl(i,j,k), phase(i,j,k), dummy
        IF (ti .NE. i .OR. tj .NE. j) THEN
          PRINT *,'reading order mismatch expect ',i,j,' but got ',
     1               ti, tj
        ENDIF
        !WRITE(*,*) ti, tj, freq(k), ampl(i,j,k), phase(i,j,k), dummy
        !STOP
      ENDDO
      ENDDO
      ENDDO

!Have read in the data, now see about writing it back out in amplitude and phase files
      DO k = 1, nf
        tmp(:,:) = ampl(:,:,k)
        WRITE(fname,9001) "ampl",k
        OPEN(11, FILE=fname, FORM="UNFORMATTED", STATUS="NEW");
        WRITE(11) tmp
        CLOSE(11)

        tmp(:,:) = phase(:,:,k)
        WRITE(fname,9001) "phas",k
        OPEN(11, FILE=fname, FORM="UNFORMATTED", STATUS="NEW");
        WRITE(11) tmp
        CLOSE(11)
 
      ENDDO
 9001 FORMAT(A4,".",I2)

      END
