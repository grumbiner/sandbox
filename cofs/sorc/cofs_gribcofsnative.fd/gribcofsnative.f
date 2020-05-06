C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM:    GRIBCOFSNATIVE      GRIB EDITION 1 
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 95-09-21
C
C ABSTRACT: PACKS CFS MODEL OUTPUT INTO GRIB
C
C PROGRAM HISTORY LOG:
C   95-09-21  C. PETERS   ORIGINALLY WRITTEN 
C
C   INPUT LIST:  
C     FT21F001 - yymmddhh.avg,yymmddhh.3D,yymmddhh.2D
C     FT22F001 - yymmddhh.grd
C
C   OUTPUT LIST: 
C     FT03F001 - diagi.txt,diags.txt,diaga.txt
C     FT52F001 - ecofsgrd.grb
C     FT53F001 - yymmddhh.igb,yymmddhh.agb,yymmddhh.sgb
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:  
C      GENPDS
C      GENGDS
C      GENBDS
C      WRYTE
C      DATAPK_AVG
C      DATAPK_INST
C      DATAPK_SFC
C      GTBITS
C      FNDBIT
C      GETLBL
C      GETARG
C     
C     LIBRARY:
C      W3FI72
C      W3FI63
C 
C REMARKS:  SINCE NO OFFICIAL GDS EXISTS YET FOR THE CFS MODEL, THE DATA
C ARE PACKED USING A "USER DEFINED" GRID FLAG {PDS(7)}.  DUE TO THE COMPLEX
C NATURE OF THE GRID (CURVILINEAR ORTHOGONAL), NO GRID DESCRIPTION IS IN-
C CLUDED.  HENCE THE DATA WILL BE UNPACKED WITH "NON-DEFINED GRID" AS NO
C GDS HAS BEEN DEVELOPED YET.  FOR FUTURE WORK, EITHER A GDS MUST BE DE-
C VELOPED OR THE DATA MUST BE INTERPOLATED TO A STANDARD NWS GRID AND THEN
C PACKED INTO GRIB.  
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NAS, CRAY C-90, IBM ASP
C
C$$$
C     MAIN PROGRAM
C
C------------------------------------------------------------------------
C LAST REVISION: DEC 22, LL - INPUT ROUTINE CHANGED
C  NOV 26, CP - IMJM PARAMETERIZATION
C  NOV 21, 95, LL - MODIFICATIONS TO FIT ECOFS 3.0 VERSION
C  JAN 26, 96, LL - OCEAN DEPTH, GRIDPOINTS LOCATION ENCODING ADDED
C                   LABELING OF THE SEA LEVEL CHANGED TO 82
C  FEB  7, 96, LL - TIME/DATE COMPONENTS TRUNCATION CHANGED TO ROUNDING
C  MAR  8, 96, LL - VERTICAL INTERPOLATION TO PRESCRIBED FIXED DEPTHS
C                   ADDED
C  APR  8, 96, LL - LL_960327e: Changes needed for daily min, max elevation 
C                   calculation 
C              LL - LL_960327l: Enhancements of output handling routines
C  MAY  1, 96, CP&LL - CP_960416a, LL_960501: Handling of averaged fields
C                   and maxmin elevations; hourly 2-D file
C  MAY  5, 96, LL - LL_960506 - further improvements in output handling
C
C   OUTPUT; PARAMETER PASSING; PACKING VERIFICATION


      PROGRAM GRIBCOFSNATIVE
C
      CHARACTER*80 FNAME, FNAM2, FNAM3, FNAM4, FNAM5, GBNAME, DIAGNAME
      CHARACTER*60 HDR
      CHARACTER*60 VERSTR
      CHARACTER*1 GTYPE
      LOGICAL LEND, MEND, EXIST, EOF
c irivin Aug 11, 1999
c      INTEGER GETARG
       CALL W3TAGB('GRIBCOFSN',2001,0067,0076,'NP20')
C
C.....PARSE COMMAND LINE PARAMETERS
C     FNAME GRIBNAME 
       WRITE(*,*)'USAGE: gribcofsnative.x'
     &   ,' gtype 3D-file/avg-file gridfile gribfile diagfile mxmnfile'
       if (IARGC().LT.5) then
         write(*,*) 
     &     ' gribcofsnative.x: not enough arguments for gribbing'
        CALL EXIT (1)
      ENDIF

cirivin Aug 11, 1999: no error handling anymore (need to DBGZ)
 
      call GETARG(1,gtype)
      call GETARG(2,FNAM2)
      call GETARG(3,FNAM3)
      call GETARG(4,GBNAME)
      IF (.NOT.EXIST(FNAM2)) GOTO 1041
      IF (.NOT.EXIST(FNAM3)) GOTO 1041
      call GETARG(5,DIAGNAME)
      if (gtype.eq.'A') then
        if (IARGC().LT.6) then
          write(*,*) 
     &      ' gribcofsnative.x: not enough arguments for gribbing'
     &      ,' of the averaged file'
          CALL EXIT (1)
        else 
          call GETARG(6,FNAM4)  
          IF (.NOT.EXIST(FNAM4)) GOTO 1041
        endif
      endif
cirivin Jan 5, 2001
c     CALL ASNRM(IER)

      OPEN (UNIT=21,FILE=FNAM2,FORM='UNFORMATTED')
      OPEN (UNIT=22,FILE=FNAM3,FORM='UNFORMATTED') 
      if (gtype.eq.'A') then
        OPEN (UNIT=23,FILE=FNAM4,FORM='UNFORMATTED') 
      endif
      OPEN (UNIT=71,FILE=DIAGNAME)


      READ (22,END=500) HDR
      CALL GETLBL(22,IHHREF,IDDREF,IMMREF,IYYREF,IFT,IDGRD,IM,JM,KB,
     &            EOF)
      BACKSPACE (22)
      if (gtype.eq.'I') then
        READ (21,END=500) HDR
      elseif ((gtype.eq.'A')) then
        READ (21,END=500) HDR
        READ (23,END=500) HDR
      endif
      write (*,*) 'header:',HDR
  
  
  
  500 LEND=.TRUE.
      IMJM=IM*JM
      KB1=KB - 1
      if (gtype.eq.'I') then
        CALL DATAPK_INST(IM,JM,KB,IMJM,KB1,GBNAME)
      elseif (gtype.eq.'S') then
        CALL DATAPK_SFC(IM,JM,KB,IMJM,KB1,GBNAME)
      elseif (gtype.eq.'A') then
        CALL DATAPK_AVG(IM,JM,KB,IMJM,KB1,GBNAME)
      endif
      CALL W3TAGE('GRIBCOFSN')
      STOP
 1039 WRITE (*,*) 'FIRST PARAMETER DOES NOT NAME AN EXISTING FILE'
      CALL EXIT(2)
 1040 WRITE (*,*) 'PARAMETER(S) NOT DECODED PROPERLY'
      CALL EXIT(3)
 1041 WRITE (*,*) '2ND OR 3RD PARAMETER DOES NOT NAME AN EXISTING FILE'
      CALL EXIT(2)
      END
