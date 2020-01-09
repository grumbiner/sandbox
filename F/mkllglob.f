      PROGRAM mk
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: ICE2GRIB     ENGRIB A 0.5 DEGREE LAT-LONG CHAR ARRAY 
C   PRGMMR: GRUMBINE         ORG: NP21        DATE: 1999-05-06
C
C ABSTRACT: READ IN 1 BYTE VALUES FROM A 0.5 DEGREE LAT-LONG GRID
C    AND WRITE THEM OUT AS A GRIB FIELD.
C
C PROGRAM HISTORY LOG:
C    97-06-24 ROBERT GRUMBINE
C    98-01-27 Robert Grumbine    Modify to write out a WMO file
C    98-07-21 Robert Grumbine    Y2K and F90 changes
C    98-07-21 Robert Grumbine    Fix grid number to match appendix K
C    98-11-18 Robert Grumbine    Drop W3FQ02 in favor of utcdat
C
C USAGE:
C  INPUT FILES:
C     FTNF06 - STANDARD INPUT - DATE INFORMATION
C     FTNF11 - CHARACTER ARRAY TO ENGRIB
C  OUTPUT FILES:
C     FTNF51 - OUTPUT GRIB FILE
C     FTNF52 - OUTPUT WMO Encoded Grib Bulletin
C 
C  SUBPROGRAMS CALLED:
C      GRIBIT, WMOOUT, MAKWMO, QUEDES, TRANST 
C    LIBRARY:
C      W3LIB: WRYTE, W3FI72, GTBITS, utcdat, W3FI92, W3AI19
C
C  EXIT STATES:
C    COND = 0  - SUCCESSFUL RUN
C
C  REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C   MACHINE: CRAY4
C
C$$$
C     Engrib character maps already on lat-long grids.  
C     Robert Grumbine 4 June 1997.

      IMPLICIT none
C     Includes for bacio
      INCLUDE "locale.inc"
      INCLUDE "clib.inc"
      INTEGER nactual, fdes, start, newpos, bacio
      CHARACTER*7 fname

      INTEGER nx, ny
      REAL dxlat, dylat
      PARAMETER (dxlat = 0.5)
      PARAMETER (dylat = 0.5)
      PARAMETER (nx = 360 / dxlat)
      PARAMETER (ny = 180 / dylat)

      INTEGER gridno
      PARAMETER (gridno = 235) !NCEP grid number of this grid
      REAL outmap(nx, ny)
      LOGICAL lbm(nx, ny)

      INTEGER griblen
      PARAMETER (griblen = (100 + 28 + nx * ny * (8+1)) /8 )

      CHARACTER cmap(nx, ny)
      CHARACTER grib( griblen )
      INTEGER lgrib, ierr

C     Definitions for the WMO section
      INTEGER byteint, wmolen, wmounit
      CHARACTER*6 BULHEAD
      CHARACTER*4 KW
      PARAMETER (wmolen = 1280)
      PARAMETER (byteint = 4)
      PARAMETER (wmounit = 52)
      PARAMETER (BULHEAD = "OEXA88")
      PARAMETER (KW      = "KWBM")
      INTEGER linelen, nlines
      PARAMETER (linelen = wmolen/byteint)
      PARAMETER (nlines  = (griblen + 80 + 21) / wmolen + 2)
      INTEGER lwork(linelen, nlines)


C     Local Utility variables      
      INTEGER i, j, cen, yy, mm, dd
      CHARACTER*8 tag

COLD      READ (11) cmap
      WRITE (fname,9009)
 9009 FORMAT ("fort.11")
      newpos = 0
      start  = 0
      i = nx*ny
      ierr = bacio(BAOPEN_RONLY + BAREAD + BACLOSE, start, newpos,
     1              SIZEOF_CHARACTER, i, nactual, fdes, fname, cmap)
      IF (ierr .NE. 0) THEN
        PRINT *,'bacio ierr = ',ierr
        STOP "error from bacio read "
      ENDIF

      READ (*, 9001) tag
 9001 FORMAT (A8)
      READ (tag, 9002) cen, yy, mm, dd
 9002 FORMAT (I2, I2, I2, I2)
      yy = yy + 100 * cen          ! Pass 4 digit year to gribit

      CALL W3TAGB('ICE2GRIB',1999,0126,0058,'NP21   ')

      DO 1000 j = 1, ny
        DO 1100 i = 1, nx
          outmap(i,j) = FLOAT(ICHAR(cmap(i,j))) / 100.
 1100   CONTINUE
 1000 CONTINUE

      CALL gribit(outmap, lbm, 0, nx, ny, 8, 0.0, 28, 1, 7, 120, 0, 91,
     1     102, 0, 0, yy, mm, dd, 0, 1,
     2     0, 0, 10, 0, 0, 2, 
C     Last argument is power in multiplying (data)*10**x prior to gribbing.
     3     90.-dylat/2.,         dxlat/2., 
     4     90.-dylat/2+(ny-1)*dylat, dxlat/2.+dxlat*(nx-1), 
     5     dxlat, dylat, 0, -10., gridno, grib, lgrib, ierr) 

      IF (ierr .EQ. 0) THEN
        WRITE (fname, 9010)
 9010   FORMAT("fort.51")
        ierr = bacio(BAOPEN_WONLY + BAWRITE + BACLOSE, start, newpos,
     1                SIZEOF_CHARACTER, lgrib, nactual, fdes, fname,
     2                grib)
COLD        CALL WRYTE(51, lgrib, grib)
CTEST        PRINT *,'Calling wmoout, linelen, nlines, byteint, lgrib = ',
CTEST     1                           linelen, nlines, byteint, lgrib
        OPEN(wmounit, ACCESS='DIRECT',RECL=wmolen)
        CALL wmoout(BULHEAD, KW, yy, mm, dd, 0, lwork, linelen, nlines,
     1                 grib, lgrib, wmounit)
       ELSE
        PRINT *,'Error ',ierr,' constructing grib message in mkllglob'
      ENDIF
     
      CALL W3TAGE('ICE2GRIB')
      STOP
      END
