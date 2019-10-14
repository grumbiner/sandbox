      PROGRAM fields
C     Read old ssmi mastermap data files of fairly arbitrary format.
C     Derived (loosely) from Ken Mitchell program ssmiplt.f
C     Move to include files the portions which depend on
C       resolution and satellite.
C     Bob Grumbine 21 April 1994

      IMPLICIT none

      INCLUDE "ssmigrid.inc"

      INTEGER*2 header(grid,nhead)
      INTEGER*2 line(grid)

      REAL  field(grid,grid), outgrd(grid,grid)
      INTEGER pfield, flags(grid, grid), rflags(nfield)
      INTEGER pflags
      INTEGER i, j, k, dummy, nget, get

      PRINT *,'How many fields would you like?'
      READ (*,9001) nget

 9001 FORMAT (I3)
      DO 9999 get = 1, nget
      PRINT *,'Which field would you like?'
      READ (*,9001) pfield
      
C     Dummy reads to space down to where the field and header begin.
      REWIND (20)
      IF (pfield .NE. 1) THEN
        DO 1000 i = 1, grid*(pfield-1)+1
          READ (20) dummy
 1000   CONTINUE
      ENDIF
C     Now pass control to the field reader routine
      CALL fielder(pfield, header, line, field, flags)
      
C     Have now read in the data and done basic rescaling.
      IF (type(pfield) .EQ. 3) THEN
        PRINT *,'How many flag types do you want to use'
        READ (*,9001) pflags
        DO 2000 i = 1, pflags
          PRINT *,'Enter flag # ',i
          READ (*,9001) rflags(i)
 2000   CONTINUE
        DO 2050 k = 1, pflags
        DO 2100 j = 1, grid
          DO 2200 i = 1, grid
            outgrd(i,j) = 0.
            IF (flags(i,j) .EQ. rflags(k)) outgrd(i,j) = field(i,j)
 2200     CONTINUE
 2100   CONTINUE
 2050   CONTINUE

       ELSE
        DO 2300 j = 1, grid
          DO 2300 i = 1, grid
            outgrd(i,j) = MAX(low(pfield) , field(i,j))
            outgrd(i,j) = MIN(high(pfield), outgrd(i,j))
 2300   CONTINUE
      ENDIF

      IF (flip) THEN
        DO 3000 j = grid, 1, -1
          DO 3100 i = 1, grid
            field(i,j) = outgrd(i,grid+1-j)
 3100     CONTINUE
 3000   CONTINUE
        WRITE (52) field
       ELSE
        WRITE (52) outgrd
      ENDIF

 9999 CONTINUE
  
      STOP
      END
