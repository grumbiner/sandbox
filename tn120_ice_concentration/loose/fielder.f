      SUBROUTINE fielder(pfield, header, line, field, flags)
C     Subroutine to read in data from the formerly operational
C       mastermap files.
C     Bob Grumbine 4 April 1994.
      INCLUDE "ssmigrid.inc"
      
      INTEGER pfield
      INTEGER*2 header(grid, nhead)
      INTEGER*2 line(grid)
      REAL field(grid, grid)
      INTEGER flags(grid, grid)
 
      INTEGER i, j

C     Read in the header
      DO 1000 j = 1, nhead
        READ (20) (header(i,j),i=1,grid)
 1000 CONTINUE

C     Read in the data field 
      DO 1001 j = 1, grid
      DO 1001 i = 1, grid
        flags(i,j) = 0
        field(i,j) = -1.
 1001 CONTINUE

      DO 2000 j = 1, grid
        READ (20) line

C       Errors are given as negative numbers.  For now, put error
C       checking elsewhere

        IF (type(pfield) .EQ. 1) THEN
C         No rescaling, just type conversion
          DO 2200 i = 1, grid
            field(i,j) = FLOAT(line(i))
 2200     CONTINUE
         ELSE IF (type(pfield) .EQ. 2) THEN
C         Multiplicative rescaling
          DO 2210 i = 1, grid
            field(i,j) = FLOAT(line(i)) / scale(pfield)
 2210     CONTINUE
         ELSE IF (type(pfield) .EQ. 3) THEN
C         ABCDE encoding
          DO 2220 i = 1, grid
            flags(i,j) = MOD(line(i), 10)
            field(i,j) = FLOAT( INT(line(i) / 10)) / scale(pfield)
 2220     CONTINUE
         ELSE
C         No such coding known
          STOP 'No known coding of this type'
        ENDIF

 2000 CONTINUE

      RETURN
      END 
