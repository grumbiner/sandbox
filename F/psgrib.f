      PROGRAM psgrib
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: PSGNORTH     ENGRIB POLAR STEREOGRAPHIC DATA
C   PRGMMR: GRUMBINE         ORG: NP21        DATE: 1998-01-29
C
C ABSTRACT: READ IN 1 BYTE VALUES FROM A GRID SPECIFIED BY THE
C    ICEGRID.INC FILE AND WRITE THEM OUT AS A GRIB FIELD.
C
C PROGRAM HISTORY LOG:
C    97-06-24 ROBERT GRUMBINE
C    98-01-28 Robert Grumbine  -- Add generation of WMO bulletin
C    98-07-21 Robert Grumbine  -- Y2K and F90
C    98-11-18 Robert Grumbine    Drop W3FQ02 in favor of utcdat
C  1999-09-02 Robert Grumbine    IBM SP Conversion: change WRYTE to bacio
C
C USAGE:
C    INPUT FILES:
C     FTNF06 - STANDARD INPUT - DATE FLAG FOR GRIB
C     FTNF11 - CONCENTRATION GRID TO ENGRIB
C    OUTPUT FILES:
C     FTNF51 - GRIBBED CONCENTRATION FILE
C     FTNF52 - WMO-Encoded file
C
C  SUBPROGRAMS CALLED:
C    UNIQUE: GRIBIT, wmoout, MAKWMO, QUEDES, TRANST
C    LIBRARY: 
C      W3LIB - WRYTE, W3FI72, GTBITS, utcdat, W3FI92, W3AI19
C      BACIO
C
C  EXIT STATES:
C    COND = 0 - SUCCESSFUL RUN
C
C  REMARKS:
C
C  ATTRIBUTES:
C    LANGUAGE: CRAY FORTRAN
C    MACHINE: CRAY3
C
C$$$
C     Engrib character data from polar stereographic grids.  
C     Robert Grumbine 4 June 1997.  

      IMPLICIT none
C     Includes for bacio
      INCLUDE "locale.inc"
      INCLUDE "clib.inc"

C     Variables for Gribbing
      INCLUDE "icegrid.inc"
      REAL map(LP, MP)
      LOGICAL lbm(LP, MP)
      REAL pi, xlat1, xlon1, xlat2, xlon2
      INTEGER griblen
      PARAMETER (griblen = (100 + 28 + LP*MP*(8+1)) / 8 )
      CHARACTER conc(LP, MP), grib( griblen )
      INTEGER lgrib, ierr
      
C     Variables for WMO encoding
      INTEGER byteint, wmolen, wmounit
      CHARACTER*6 BULHEAD
      CHARACTER*4 KW
      PARAMETER (wmolen = 1280)
      PARAMETER (byteint =  4 )
      PARAMETER (wmounit = 52 )
      PARAMETER (KW      = "KWBM")
      INTEGER linelen, nlines
      PARAMETER (linelen = wmolen / byteint)
      PARAMETER (nlines  = (griblen + 80 + 21) / wmolen + 1)
      INTEGER lwork(linelen, nlines)
      PARAMETER (BULHEAD = "OENA88")

C     Local Utility variables
      INTEGER i, j
      INTEGER cen, yy, mmm, dd
      INTEGER fdes, newpos, nactual, start, bacio
      CHARACTER*7 fname

      CALL W3TAGB('PSGNORTH',1998,0029,0095,'NP21   ')

      READ (*,*) cen
      READ (*,*) yy
      READ (*,*) mmm
      READ (*,*) dd
      yy = yy + 100 * cen

      newpos = 0
      start = 0
      i = LP*MP
      WRITE (fname,9001) 
 9001 FORMAT ("fort.11")
      ierr = bacio(BAOPEN_RONLY + BAREAD + BACLOSE, start, newpos, 
     1              SIZEOF_CHARACTER, i, nactual, fdes, fname, conc) 
      IF (ierr .NE. 0) THEN
        PRINT *,'bacio ierr = ',ierr
        STOP "error from bacio read "
      ENDIF
COLD      READ (11) conc
      DO 1000 j = 1, MP
        DO 1100 i = 1, LP
CD          map(i,j) = FLOAT(ICHAR(conc(i,MP+1-j)))
          map(i,j) = FLOAT(ICHAR(conc(i,j))) / 100.
CD          PRINT *,i,j,map(i,j)
 1100   CONTINUE
 1000 CONTINUE

      CALL mapxy(xorig, yorig, xlat1, xlon1, 
     1            slat, slon, sgn, eccen2, rearth)
      CALL mapxy(xorig+L*dx, yorig+M*dy, xlat2, xlon2, 
     1            slat, slon, sgn, eccen2, rearth)

      pi = ABS(ACOS(-1.))
      xlat1 = xlat1*pi/180.
      xlon1 = xlon1*pi/180.
      xlat2 = xlat2*pi/180.
      xlon2 = xlon2*pi/180.

CD      PRINT *,'lp*mp = ',LP*MP
      CALL gribit(map, lbm, 5, LP, MP, 8, 0.0, 
     1            28, 1, 7, 120, 0, 91, 102, 0, 0, 
     2            yy, mmm, dd, 0, 1, 0, 0, 10, 0, 0, 2, 
     3     xlat1, xlon1, xlat2, xlon2, dx, dy, -90.-slon, sgn, gridno,
     4     grib, lgrib, ierr) 

CD      PRINT *,'ierr = ',ierr
CD      PRINT *,'lgrib = ',lgrib
  
      IF (ierr .EQ. 0) THEN
        WRITE (fname, 9002) 
 9002   FORMAT("fort.51")
        ierr = bacio(BAOPEN_WONLY + BAWRITE + BACLOSE, start, newpos, 
     1                SIZEOF_CHARACTER, lgrib, nactual, fdes, fname, 
     2                grib) 
COLD        CALL WRYTE(51, lgrib, grib)
        OPEN(wmounit, ACCESS='DIRECT',RECL=wmolen)
        CALL wmoout(BULHEAD, KW, yy, mm, dd, 0, lwork, linelen, nlines,
     1               grib, lgrib, wmounit)
       ELSE
        PRINT *,'Error ',ierr,' constructing grib message in psgrib'
      ENDIF
     
      CALL W3TAGE('PSGNORTH')

      STOP
      END
