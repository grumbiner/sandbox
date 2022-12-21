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
C    06-01-18 Boi    Vuong       REPLACED THE ROUTINE WMOOUT WITH MAKWMO
C                                AND MKFLDSEP TO ADD FIELD SEPARATOR FOR TOC
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
C      GRIBIT, MAKWMO , 
C    LIBRARY:
C      W3LIB: WRYTE, W3FI72, GTBITS, utcdat, W3FI92, W3AI19
C
C  EXIT STATES:
C    COND = 0  - SUCCESSFUL RUN
C
C  REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 77
C   MACHINE: ANY
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

      INCLUDE "icegrid.inc"

      INTEGER nx, ny, LENHEAD
      PARAMETER (nx = 360 / dxlat)
      PARAMETER (ny = 180 / dylat)
      PARAMETER (LENHEAD=21)

      REAL outmap(nx, ny)
      LOGICAL lbm(nx, ny)

      INTEGER griblen
      PARAMETER (griblen = (100 + 28 + nx * ny * (8+1)) /8 )

      CHARACTER cmap(nx, ny)
      CHARACTER grib( griblen )
      CHARACTER * 1  CSEP(80)

      INTEGER lgrib,ierr
      INTEGER IOPT,INSIZE,LENOUT

C     Definitions for the WMO section
      INTEGER wmounit, iret
      CHARACTER*6 BULHEAD
      CHARACTER*1 HEADER(21)

      CHARACTER*4 KW
      PARAMETER (WMOUNIT = 52)
      PARAMETER (BULHEAD = "OEXA88")
      PARAMETER (KW      = "KWBM")
      INTEGER MOVA2I

C     Local Utility variables      
      INTEGER i, j, cen, yy, mm, dd

      CHARACTER*8 tag

      WRITE (fname,9009)
 9009 FORMAT ("fort.11")
      IOPT   = 2
      INSIZE = 19
      IRET   = 0
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
          outmap(i,j) = FLOAT(MOVA2I(cmap(i,j))) / 100.
 1100   CONTINUE
 1000 CONTINUE

      CALL gribit(outmap, lbm, 0, nx, ny, 8, 0.0, 28, 1, 7, 120, 0, 81,
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

        IF (WMO) THEN
          WRITE (fname, 9020)
 9020     FORMAT("fort.52")
          CALL BAOPENW(WMOUNIT,FNAME,IRET)
          IF ( IRET .NE. 0 ) THEN
            WRITE(6,FMT='("ERROR OPENING OUTPUT GRIB FILE:",A8)') FNAME
            WRITE(6,FMT='(" BAOPENW ERROR = ",I5)') IRET
            STOP 20
          ENDIF
C
C       MAKE FLAG FIELD SEPARATOR BLOCK
C
          CALL MKFLDSEP(CSEP,IOPT,INSIZE,LGRIB+LENHEAD,LENOUT)
C
C         MAKE WMO HEADER
C
          CALL MAKWMO (BULHEAD,DD,0,KW,HEADER)
C
C       WRITE OUT SEPARATOR BLOCK, ABBREVIATED WMO HEADING,
C
          CALL WRYTE(WMOUNIT,LENOUT,CSEP)
          CALL WRYTE(WMOUNIT,LENHEAD,HEADER)
          CALL WRYTE(WMOUNIT,LGRIB,GRIB)

        ENDIF
       ELSE
        PRINT *,'Error ',ierr,' constructing grib message in mkllglob'
      ENDIF
     
      CALL W3TAGE('ICE2GRIB')
      STOP
      END
