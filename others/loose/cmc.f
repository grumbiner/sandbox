C-----------------------------------------------------------------------
      SUBROUTINE PROCESSPT( ISTYPE, RLAT, RLON, MONTH, BADBRTEMP )
C-----------------------------------------------------------------------
*** S/R   PROCESSPT
*
* Version   1.0    A. Alfheim MARCH 17 / 93
*
* REVISION  001  - (NONE)
* REVISION  042  - A. Alfheim June 7 1994
*                  CALCSST called only for sea points with no precip
*                  Before: IF (.NOT. PRECIP ) CALL CALCSST
*                          CALL CALCSST
*                  inconsistencies with common ATMH2O fixed
*                  so TAU's  properly initialized and WIND37
*                  initialized to 0.0
*                  S/R landmet_old removed, no longer used
*
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - Process data at rlat and rlon.
*                  I.E. Assuming Brightness temperatures have been 
*                  derived, determine derived products, ice, wind, etc
*
*
* NOTE           - The Brightness temperatures are passed through a
*                  common block TBLR 
*
* ARGUMENTS
*    OUTPUT      - BADBRTEMP (INTEGER) SET TRUE IF ONE OF 
*                  THE BRIGHTNESS TEMPS HAS AN INVALID VALUE
*                  THIS POINT SHOULD THEN BE REJECTED. 
*                  SET FALSE OTHERWISE
*
* IMPLICITS
**
C-----------------------------------------------------------------------
      IMPLICIT NONE

      LOGICAL  ISOCEAN , PRECIP
      REAL     RLAT    , RLON     , SSTI
      INTEGER  BADBRTEMP, ISBADBRTEMP
      INTEGER  ISTYPE  ,  ICY, MONTH

      INTEGER         NTOT, NLMET, NWNOPRECIP, NWPRECIP, NICE, NREJ
      COMMON /QCFLAG/ NTOT, NLMET, NWNOPRECIP, NWPRECIP, NICE, NREJ
C-----------------------------------------------------------------------
C     Total number of points processed
      NTOT = NTOT+1
C     _________________________________________
C     Check brightness temp values for validity
      BADBRTEMP = ISBADBRTEMP()
      IF ( BADBRTEMP .EQ. 1 ) THEN
         NREJ = NREJ + 1
         RETURN
      ENDIF 
C     ____________________________________
C     Set atmospheric radiative properties
      CALL SETATM(RLAT, MONTH)

C     _________________________________
C     Initialization of input variables
      CALL INITPRODS
      CALL INITIA

      ISOCEAN = .FALSE.
      IF ( (ISTYPE .GE. 3) .AND. (ISTYPE .LE. 5) ) ISOCEAN = .TRUE.
      IF ( ISOCEAN ) THEN
         CALL SETICY(ISTYPE, RLAT, ICY)
C        _______________________________
C        Extract sea surface temperature
         CALL GETTM(RLON , RLAT , SSTI)
C        ___________________________
C        Execute the SSM/I ICE algorithm
         CALL ICEOCE(ICY)
C        ________________________________________________________________
C        If ICY = 1 its a ICE point, otherwise its ice free. 
         IF ( ICY .EQ. 1 ) THEN
            NICE = NICE + 1
         ELSE
            RETURN
         ENDIF
      ELSE
C        __________________________________
C        This is land point               
         RETURN
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      INTEGER FUNCTION ISBADBRTEMP
C-----------------------------------------------------------------------
*** FUNCTION ISBADBRTEMP
*
* Version   1.0    MARCH 17 / 93
*
* REVISION  001  - (NONE)
*           2.0    JULY 2 /96 , converted to function and no 1b stuff
*                  used to be s/r CKBRTEMP
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - DETERMINE WHETHER BRIGHTNESS TEMPERATURES
*                  FALL WITHIN VALID RANGES
*
* USAGE          -                                 
*
* ARGUMENTS
*    OUTPUT      - BADBRTEMP SET TO TRUE IF ONE OF 
*                  THE BRIGHTNESS TEMPS HAS AN INVALID VALUE
*                  THIS POINT SHOULD THEN BE REJECTED. 
*                  SET FALSE OTHERWISE
*
* IMPLICITS
*                /TBLR/    - INPUT BRIGHTNESS TEMPERATURES
**
C-----------------------------------------------------------------------
CBG      IMPLICIT NONE
      REAL    TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H
      COMMON /TBLR/ TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

      ISBADBRTEMP=0
C     ________________________________________
C     Check for invalid brightness temperature
C
      IF ( (TB19V .LE. 151.0)        .OR.
     .     (TB19H .LE. 92.0)         .OR.
     .     ((TB19V-TB19H) .GE. 84.0) .OR.
     .     (TB37V .LE. 171.0)        .OR.
     .     (TB37H .LE. 125.0)        .OR.
     .     ((TB37V-TB37H) .GE. 80.0) .OR.
     .     (TB19H .GE. TB19V)        .OR.
     .     (TB37H .GE. TB37V)            ) THEN
C        __________________________________________
C        Errors have been found in this observation
         ISBADBRTEMP = 1
      END IF

      RETURN
      END

C-----------------------------------------------------------------------
        SUBROUTINE SETATM(RLAT, IMONTH)
C-----------------------------------------------------------------------
*** S/P  SETATM
*
* version 2.0     DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
* REVISION  001  - (NONE)
* REVISION  060   A. ALFHEIM  JUL 04 1996
*                 RLON ARGUEMENTS REMOVED, NOT USED, IMONTH NOW ARG.
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - SET ATMOSPHERIC RADIATIVE PROPERTIES
*
* USAGE          - CALL SETATM
*
* ARGUMENTS      - NONE
*
* IMPLICITS
*                /ATMH20/ - ATMOSPHERIC RADIATIVE PROPERTIES
*                           Summer and winter case.
*                           TAU19 - ATMOSPHERIC OPTICAL OPACITY AT 19 GHZ.
*                           TA19  - ATMOSPHERIC OFFSET  AT 19 GHZ.
*                           TAU37 - ATMOSPHERIC OPTICAL OPACITY AT 37 GHZ.
*                           TA37  - ATMOSPHERIC OFFSET  AT 37 GHZ.
**
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    RLAT
      INTEGER IMONTH, IWINT
      REAL             TAU37, TA37, TAU19, TA19
      COMMON /ATMH20/  TAU37, TA37, TAU19, TA19
C-----------------------------------------------------------------------
        IWINT=0
        IF(RLAT.GE.40.0) THEN
           IF(IMONTH.GE.10.OR.IMONTH.LE.4) THEN
              IWINT=1
           ELSE
              IWINT=0
           END IF
        END IF
        IF(RLAT.LE.-40.0) THEN
           IF(IMONTH.GE.4 .AND. IMONTH.LE.10) THEN
              IWINT=1
           ELSE
              IWINT=0
           END IF
        END IF
C
        IF(IWINT.EQ.1) THEN
C          fall/winter
           TA19  = 6.8
           TAU19 = 0.0282
           TA37  = 14.0
           TAU37 = 0.057
        ELSE
C          spring/summer
           TA19  = 14.0
           TAU19 =0.0564
           TA37  = 28.0
           TAU37 = 0.114
        END IF
*
        RETURN
	END


C-----------------------------------------------------------------------
        SUBROUTINE SETICY(ISTYPE, RLAT, ICY)
C-----------------------------------------------------------------------
*** S/P   SETICY
*
* AUTHOR         - ARM CONSULTANTS  -  FEBRUARY 1989
*                  1404-35 FOUNTAINHEAD RD.,
*                  DOWNSVIEW,ONTARIO,M3J-2V7
*                  (416) 663-3938
*                  DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
*
* REVISION  001  - (NONE)
* REVISION  002  - a. alfheim March 93 V40
* REVISION  060   A. ALFHEIM  JUL 04 1996
*                 JDAY, RLON ARGUEMENTS REMOVED, NOT USED
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - SET ICY FLAG FOR THE CURRENT PIXEL
*
* USAGE          - CALL SETICY(ISTYPE, RLAT, ICY)
*
* ARGUMENTS
*            I   - ISTYPE - SURFACE TYPE
*            I   - RLAT   - LATITUDE
*            O   - ICY    - ICE CONDITION FLAG
*                           -1  - Indicates land point   
*                            0  - NO ICE
*                            1  - ICE OR POSSIBLE ICE
*
C NOTES -  the following surface types are provided in the SSM/I report
C    istype = 0  -  land
C             1  -  not used
C             2  -  not used
C           > 3  -  ice
C           > 4  -  possible ice
C           > 5  -  ocean
C             6  -  coast
C             7  -  flooded soil
C             8  -  heavy precip
C             9  -  arable soil
C            10  -  desert
C            11  -  frozen ground
C            12  -  glacial ground
C            13  -  snow over frozen ground
C            14  -  snow over soil
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  ISTYPE, ICY
      REAL     RLAT

C     Set ICY flag according to the surface type as
C     decoded from the SSM/I report
      ICY = -1

      IF(ISTYPE .EQ. 5) ICY = 0
      IF(ISTYPE .EQ. 3 .OR. ISTYPE .EQ. 4) ICY = 1
C     Verify ICY flag using latitude cutoff
      IF(ICY .EQ. 1) THEN
         IF(RLAT .GT. -40.0 .AND. RLAT .LT. 40.0) ICY = 0
      END IF
      RETURN
      END


C-----------------------------------------------------------------------
       SUBROUTINE INITIA
C-----------------------------------------------------------------------
*** S/R   INITIA
*
* AUTHOR         - ARM CONSULTANTS  -  FEBRUARY 1989
*                  1404-35 FOUNTAINHEAD RD.,
*                  DOWNSVIEW, ONTARIO, M3J-2V7
*                  (416) 663-3938
*                  DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
* REVISION  001  - (NONE)
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - SET INITIAL CONSTANTS
*
* USAGE          - CALL INITIA
*
* ARGUMENTS      - NONE
*
* IMPLICITS
*                /ICEFRA/ - ICE FRACTIONS
*                /FUNPAR/ - EMPIRICAL PARAMETERS
**
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL     FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      REAL     CFY,CMY,TB19,WV,WH,BV,BH,BDIFF,TBI37,
     .         TA1,TA2,A1,A2,B1,B2,DEN,TW37,TW19,
     .         EW19,EW37,TF37,TO37,TF19,TO19,EFY37,
     .         EMY37,EFY19,EMY19,TATM37,TATM19
      REAL     DIFF,TBCUT,WCUT,CA,CV,CH,WI1,WI2,X1H,X1V,
     .         CON1,CON2,CON3,CON4,CON5,CON6

        COMMON /ICEFRA/ FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
        COMMON /FUNPAR/ CFY,CMY,TB19,WV,WH,BV,BH,BDIFF,TBI37,
     .                  TA1,TA2,A1,A2,B1,B2,DEN,TW37,TW19,
     .                  EW19,EW37,TF37,TO37,TF19,TO19,EFY37,
     .                  EMY37,EFY19,EMY19,TATM37,TATM19
        COMMON /MISC/   DIFF,TBCUT,WCUT,CA,CV,CH,WI1,WI2,X1H,X1V,
     .                  CON1,CON2,CON3,CON4,CON5,CON6
C-----------------------------------------------------------------------
C       set experimental constants
C
C-BJ 01/08/90    DMIN1=-0.03
C       CHANGE IN DMIN1
        DMIN1=0.0
C       CHANGE IN DMIN2
        DMIN2=0.25
C       CHANGE IN DMAX
        DMAX=0.40

        TF37=250.0
        TO37=186.0
        TW37=200.0
        TF19=250.0
        TO19=216.0
        TW19=180.0
        EW37=0.76
        EFY37=0.923
        EW19=0.66
        EFY19=0.94
        EMY37=0.70
        EMY19=0.82

        TBCUT=215.0
        CON1=200.0
        CON2=130.0
        CON3=250.0
        CON4=230.0
        CON5=.9091
        CON6=0.0454

        RETURN
        END

C-----------------------------------------------------------------------
       SUBROUTINE INITPRODS
C-----------------------------------------------------------------------
*** S/R   INITIA
*
* AUTHOR         - 
*
* REVISION  001  - (NONE)
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - SET INITIAL CONSTANTS
*
* USAGE          - CALL INITPRODS
*
* ARGUMENTS      - NONE
*
* IMPLICITS
*                /METVR/  - METEOROLOGICAL PRODUCTS
*                /ICEFRA/ - ICE FRACTIONS
**
C-----------------------------------------------------------------------
      IMPLICIT NONE

      REAL   FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      COMMON /ICEFRA/ FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2

      FFY    = -99.9
      FMY    = -99.9
      FTI    = -99.9
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE ICEOCE (ICY)
C-----------------------------------------------------------------------
*** S/R   ICEOCE    (algorithm as of 01.03.89    CMC version # "1.03.89"
*
*                  version 2.0
*
* AUTHOR           - AES/ISTS MICROWAVE GROUP (I.G. RUBINSTEIN)
*
* CODED            - ARM CONSULTANTS  - FEBRUARY 1989
*                    DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
* REVISION  001  - (NONE)
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - MAIN DRIVER FOR THE SSM/I ICE/OCEAN ALGORITHM
*
*                  ANALYZE BRIGHTNESS TEMPERATURE TO EVALUATE THE
*                  PRESENCE OF ICE.
*                  IF ICE IS NOT PRESENT,SEVERAL METEOROLOGICAL
*                  VARIABLES ARE CALCULATED.
*
* USAGE          - ICEOCE (ICY)
*
* ARGUMENTS
*            I   - ICY  - FLAG INDICATING THE POSSIBLE PRESENCE OF ICE.
*                         IF =0,ICE IS NEVER PRESENT.
*                         IF =1,ICE MIGHT BE PRESENT.
*
* IMPLICITS
*                /TBLR/   - INPUT BRIGHTNESS TEMPERATURES
*                /MISC/    - OTHER PARAMETERS
*                /ICEFRA/  - ICE FRACTIONS AND PARAMETERS
*                                FFY   - FIRST YEAR ICE
*                                FMY   - MULTI YEAR ICE
*                                FTI   - THIN ICE
*                                FTOT  - TOTAL ICE
*                                FW    - OPEN WATER
*
* ALGORITHM      - IF ICE IS PRESENT,IT CALCULATES THE FRACTIONS
*                  OF OLD,NEW AND THIN ICE AND THE FRACTION OF OPEN
*                  OCEAN
*                  IF ICE IS NOT PRESENT,
*
* VARIABLES
*              TB19V  - VERTICALLY POLARIZED SIGNAL OF THE BRIGHTNESS
*                      TEMPERATURE AT 19 GHZ. (KELVIN DEGREE * 10)
*              TB19H  - HORIZONTALLY POLARIZED SIGNAL OF THE BRIGHTNESS
*                      TEMPERATURE AT 19 GHZ. (KELVIN DEGREE * 10)
*              TB37V  - VERTICALLY POLARIZED SIGNAL OF THE BRIGHTNESS
*                      TEMPERATURE AT 37 GHZ. (KELVIN DEGREE * 10)
*              TB37H  - HORIZONTALLY POLARIZED SIGNAL OF THE BRIGHTNESS
*                      TEMPERATURE AT 37 GHZ. (KELVIN DEGREE * 10)
*              FTOT  - TOTAL ICE COVER (%).
*              FW    - OPEN OCEAN FRACTION (%).
*
* MODULES CALLED
*              CONFIR  - TO CHECK IF ICE IS PRESENT WHEN INCONSISTENCIES
*                        WERE FOUND IN PREVIOUS TEST.
*              DTEST   - TO CHECK THE PRESENCE OF ICE AND TO 
*                        DISCRIMINATE BETWEEN NEW,OLD AND THIN ICE.
*              ERROR   - TO PREVENT DIVISIONS BY ZERO.
*              FINAL   - TO CALCULATE WATER PERCENTAGE AND TO DO A FINAL
*                        TESTING TO SEE IF ICE IS PRESENT.
*              FNICE   - TO CALCULATE VARIOUS FUNCTIONS,FIRST YEAR AND
*                        MULTIYEAR ICE.
**
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  ICY   , IRET  , IFF , IFW
      REAL     DIF37 , DIF19 , GHH

      REAL  FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      REAL  DIFF,TBCUT,WCUT,CA,CV,CH,WI1,WI2,X1H,X1V,
     &      CON1,CON2,CON3,CON4,CON5,CON6
      REAL    TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H


      COMMON /ICEFRA/ FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      COMMON /MISC/   DIFF,TBCUT,WCUT,CA,CV,CH,WI1,WI2,X1H,X1V,
     &                CON1,CON2,CON3,CON4,CON5,CON6
      COMMON /TBLR/ TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

C       To discriminate the cases when ice is never present.
        IF (ICY .EQ. 1) THEN
C       ___________________________________
C           Assume thin ice fraction to be null
C
            FTI = 0.0
            IF (TB37V .LE. 215.0) WCUT=6.0
            IF (TB37V .GT. 215.0) WCUT=8.5
C           ___________________
C           Calculate functions
            CALL FNICE
C
            D = CON5-CON6*(TB37V-TB19V)
C           ______________________________________________________
C           To discriminate between cases when the presence of ice
C           might be due to winds or other factors and cases when
C           the presence of ice is certain.
C
            CT    = FFY+FMY
            IRET  = 0
            IFF   = 0
            DIF37 = TB37V-TB37H
            GHH   = TB37H-TB19H
            DIF19 = TB19V-TB19H
            IF ( DIF19 .LT. 45 .OR.
     .          (DIF37 .LT. 50 .and. GHH .LT. 25.) .OR.
     .           TB37V .GT. 220                         ) IFF = 1
            IF ( CT .GT. 70.0 .OR.
     .           (IFF .EQ. 1 .and. (D .gt. -0.1)) ) THEN
               CALL CONFIR
            ELSE
C           _____________________________________________
C              Verify if divisions by zero will occur and if
C              concentrations of first year ice is valid
               CALL ERROR (IRET)
C              ____________________________________________________
C              Test for the presence of ice and calculate the 
C              percentage of ocean if the ice cover is only partial.
               IF (IRET.EQ.0) CALL DTEST
            END IF
            IF (IRET .EQ. 0 .OR. IFF .EQ. 1) CALL FINAL
            IFW = AINT(FW)
 	    IF (IFW .EQ. 100) ICY = 0
        ENDIF


        IF (ICY .EQ. 0 ) THEN
	    FFY  = 0.0
	    FMY  = 0.0
	    FTI  = 0.0
	    FTOT = 0.0
            FW   = 100.0
        ENDIF
        RETURN
        END

C-----------------------------------------------------------------------
        SUBROUTINE CONFIR
C-----------------------------------------------------------------------
*** S/P   CONFIR
*
* AUTHOR         - ARM CONSULTANTS  -  FEBRUARY 1989
*                  1404-35 FOUNTAINHEAD RD.,
*                  DOWNSVIEW, ONTARIO, M3J-2V7
*                  (416) 663-3938
*                  DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
* REVISION  001  - (NONE)
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - CHECK IF ICE IS PRESENT WHEN CONSISTENCY PROBLEMS
*                  OCCUR
*
* USAGE          - CALL CONFIR
*
* ARGUMENTS      - NONE
*
* IMPLICITS
*                /ATMH20/ - ATMOSPHERIC RADIATIVE PROPERTIES
*                /TBLR/   - INPUT BRIGHTNESS TEMPERATURES
*                /ICEFRA/ - ICE FRACTIONS
*                /FUNPAR/ - EMPIRICAL PARAMETERS
*
* MODULES CALLED
*                FNICE    - CALCULATE FIRST YEAR AND MULTI YEAR ICE
*                           COMPONENTS
**
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL  TBI19

      REAL  TAU37,TA37,TAU19,TA19
      REAL  FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      REAL  CFY,CMY,TB19,WV,WH,BV,BH,BDIFF,TBI37,
     &      TA1,TA2,A1,A2,B1,B2,DEN,TW37,TW19,
     &      EW19,EW37,TF37,TO37,TF19,TO19,EFY37,
     &      EMY37,EFY19,EMY19,TATM37,TATM19

      REAL    TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

      COMMON /ATMH20/  TAU37, TA37, TAU19, TA19
      COMMON /ICEFRA/ FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      COMMON /FUNPAR/ CFY,CMY,TB19,WV,WH,BV,BH,BDIFF,TBI37,
     &                TA1,TA2,A1,A2,B1,B2,DEN,TW37,TW19,
     &                EW19,EW37,TF37,TO37,TF19,TO19,EFY37,
     &                EMY37,EFY19,EMY19,TATM37,TATM19

      COMMON /TBLR/ TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

C-----------------------------------------------------------------------
C     _________________________________________________________________
C     If negative values of ice fraction have been calculated, do other
C     tests to get the right ice fractions.
      IF (FFY.LT.0.0.OR.FMY.LT.0.0.OR.CT.GT.100.0) THEN
C        ______________________________________________________________
C        The problem is not due to the total ice fraction. Therefore,
C        test first year and multiyear ice fractions.
         IF (CT.LE.100.0) THEN
            CT=1.242-0.016*(TB19V-TB19H)
            TBI37=(TB37V-TA37-(1.0-CT)*TW37)/CT
            tbi19=(TB19V -ta19-(1-ct)*180)/ct
            IF (FMY.GE.0.OR.TBI19.LE.240) THEN
C              ______________________________________________________
C              Problem due to first year part of the ice cover < 0 %.
               FFY=(TBI37-TO37)/(TF37-TO37)
               FMY=1.-FFY
               IF(FFY.GT.1.0)FFY=1.0
               FFY=CT*FFY
               FFY=FFY*100.0
               FMY=CT*FMY*100.0
            ELSE
C              ________________________________________
C              Problem due to multiyear ice cover < 0%.
               TA37=TBI37-TF37
               TAU37=TA37/250.0
               IF (TA37.LT.0) THEN
               TA37=0.0
               TAU37=0.0
               ENDIF
               TA19=0.5*TA37
               TAU19=0.5*TAU37
               CALL FNICE
               IF (FMY.LE.0.0) THEN
C                 ________________________
C                 Only first and thin ice.
                  IF(CT.GT.1.0) CT=1.0
                  FFY=0.05*(TBI19-235.)*CT*100.0
                  IF(FFY.GT.0) THEN
                  FTI=CT*(100.0-FFY)
                  ELSE
                  FFY=0.
                  FTI=FTOT
                  ENDIF
                  FMY=0.0
               END IF
            END IF
C        ________________________________
C        Problem due to total ice > 100%.
         ELSE IF (TB37V.GE.250.0) THEN
            TA37=TB37V-250.0
            TA19=TB19V-250.0
            FFY=100.0
            FMY=0.0
         ELSE
            TA37=0.0
            TA19=0.0
            TAU37=0.0
            TAU19=0.0
            CALL FNICE
            CT=FFY+FMY
            IF (CT.GT.100.0) FMY=100.0-FFY
         END IF
      END IF

      RETURN
      END

C-----------------------------------------------------------------------
        SUBROUTINE DTEST
C-----------------------------------------------------------------------
*** S/P   DTEST
*
* AUTHOR         - ARM CONSULTANTS  -  FEBRUARY 1989
*                  1404-35 FOUNTAINHEAD RD.,
*                  DOWNSVIEW, ONTARIO, M3J-2V7
*                  (416) 663-3938
*                  DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
* REVISION  001  - (NONE)
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - TEST FOR THE PRESENCE OF ICE.
*                  IF ICE IS PRESENT, DISCRIMINATE BETWEEN:
*                  OLD, FIRST YEAR AND THIN ICE
*
* USAGE          - CALL DTEST
*
* ARGUMENTS      - NONE
*
* IMPLICITS
*                /TBLR/   - INPUT BRIGHTNESS TEMPERATURES
*                /ICEFRA/ - ICE FRACTIONS
*                /MISC/   - EMPIRICAL CONSTANTS
*
* MODULES CALLED
*                 ICE     - CONFIRM DIAGNOSTIC THAT ICE IS PRESENT
*                 NOICET  - CONFIRM THAT ICE IS NOT PRESENT
**
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL  FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      REAL  DIFF,TBCUT,WCUT,CA,CV,CH,WI1,WI2,X1H,X1V,
     &      CON1,CON2,CON3,CON4,CON5,CON6
      REAL    TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

      COMMON /ICEFRA/ FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      COMMON /MISC/   DIFF,TBCUT,WCUT,CA,CV,CH,WI1,WI2,X1H,X1V,
     &                CON1,CON2,CON3,CON4,CON5,CON6
      COMMON /TBLR/ TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H
C-----------------------------------------------------------------------

      DIFF=CH-CV
      IF (D.LE.DMIN1) THEN
C        _____________________________________________________________
C        No ice seems to be present. Further tests are done to confirm
C        this finding.
         CALL NOICET
      ELSE IF (TB37V.GT.TBCUT) THEN
         IF (TB37V.LE.250.0) THEN
C           ___________________________________________________________
C           The wind could have altered the brightness temperature data
C           The next test tries to see if it is the case.
            IF (D.GE.DMAX.OR.DIFF.LT.WCUT) CALL ICE
         ELSE
            CA=(TB19V-180.0)/67.0*100.0
            IF (CV-CA.GT.35.0) THEN
C              ______________________________________________
C              Melting snow could change the brightness temperature
C              There is no ice. NOICET is used to confirm this
               CALL NOICET
            ELSE IF (D.GE.DMAX.OR.DIFF.LT.WCUT) THEN
C              _______________________________________________________
C              The wind could have altered the brightness temperature
C              data. The previous test tries to see if it is the case.
C              The next call tries to confirm.
               CALL ICE
            END IF
         END IF
      ELSE IF (D.GT.1.0) THEN
C        _________________________________________________
C        Ice concentration is > 90% and old ice is present
         FTOT=100.0
         CALL ICE
      ELSE IF (DIFF.LT.WCUT.AND.D.GE.DMIN2) THEN
         CALL ICE
      ELSE IF (DIFF.GE.WCUT.OR.D.LT.DMIN2) THEN
C        _____________________________________________________
C        The observation was done on the edge of the ice sheet
         WI1=0.0101*TB37V-(.0066*TB37H)-1.1695
         WI2=0.0085*TB19V-(.0043*TB37V)-0681
         IF (TB37V.LT.215.0) THEN
            IF (WI1.GE.0.15.AND.WI2.GE.0.05) THEN
               FTI=WI1*100.0
               ffy=0
               FMY=0.0
            ELSE
               CALL NOICET
            END IF
         ELSE IF (WI1.GE.0.2.AND.WI2.GT.0.1) THEN
            FTI=100*(WI1*235-(TB37H-(1-WI1)*130))/(WI1*15)
            IF (FTI.GE.0) THEN
               FFY=WI1*100-FTI
            ELSE
               FTI=0
               FFY=WI1*100
            ENDIF
            FMY=0.0
         ELSE
            CALL NOICET
         END IF
      END IF
      RETURN
      END

C-----------------------------------------------------------------------
        SUBROUTINE ERROR(IRET)
C-----------------------------------------------------------------------
*** S/P   ERROR
*
* AUTHOR         - ARM CONSULTANTS  -  FEBRUARY 1989
*                  1404-35 FOUNTAINHEAD RD.,
*                  DOWNSVIEW, ONTARIO, M3J-2V7
*                  (416) 663-3938
*                  DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
* REVISION  001  - (NONE)
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - CHECK IF THE CONCENTRATION OF FIRST YEAR ICE IS VALID.
*                  (PREVENT FOR DIVISIONS BY ZERO)
*
* USAGE          - CALL ERROR
*
* ARGUMENTS      - IRET - ERROR RETURN FLAG
*
* IMPLICITS
*                /INP/    - INPUT BRIGHTNESS TEMPERATURES
*                /ICEFRA/ - ICE FRACTIONS
*                /FUNPAR/ - EMPIRICAL PARAMETERS
*                /MISC/   - EMPIRICAL CONSTANTS
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IRET

      REAL  FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      REAL  DIFF,TBCUT,WCUT,CA,CV,CH,WI1,WI2,X1H,X1V,
     &      CON1,CON2,CON3,CON4,CON5,CON6
      REAL    TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

      COMMON /ICEFRA/ FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      COMMON /MISC/   DIFF,TBCUT,WCUT,CA,CV,CH,WI1,WI2,X1H,X1V,
     &                CON1,CON2,CON3,CON4,CON5,CON6
      COMMON /TBLR/ TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

C-----------------------------------------------------------------------
      X1V=CON3-CON1
      X1H=CON4-CON2
      IF (ABS(X1V).LE.(0.0001).OR.ABS(X1H).LE.(0.0001)) IRET=1
      IF (IRET.EQ.0) THEN
C         _____________________________________________________
C         Calculate vertical polarization of the first year ice
          CV=(TB37V-CON1)/X1V*100.0
C         _______________________________________________________
C         Calculate horizontal polarization of the first year ice
          CH=(TB37H-CON2)/X1H*100.0
C         _____________________________
C         Check for valid concentration
          IF (CV.LT.-100.0.OR.CV.GT.200.0.OR.CH.LT.-100.0.OR.
     &                                          CH.GT.200.0) IRET=1
      END IF
      IF (IRET.EQ.1) THEN
          FFY=-99.9
          FMY=-99.9
          FTI=-99.9
          FTOT=-99.9
          FW=-99.9
      END IF

      RETURN
      END

C-----------------------------------------------------------------------
        SUBROUTINE FINAL
C-----------------------------------------------------------------------
*** S/P   FINAL
*
* AUTHOR         - ARM CONSULTANTS  -  FEBRUARY 1989
*                  1404-35 FOUNTAINHEAD RD.,
*                  DOWNSVIEW, ONTARIO, M3J-2V7
*                  (416) 663-3938
*                  DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
* REVISION  001  - (NONE)
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - CALCULATE WATER PERCENTAGE AND DO A FINAL
*                  TESTING TO SEE IF ICE IS PRESENT.
*
* USAGE          - CALL FINAL
*
* ARGUMENTS      - NONE
*
* IMPLICITS
*                /TBLR/   - INPUT BRIGHTNESS TEMPERATURES
*                /ICEFRA/ - ICE FRACTIONS
*                /FUNPAR/ - EMPIRICAL PARAMETERS
**
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IFLAG
      REAL     TOL , CTI , HI37 , X200 , DF19 , Y180 , XXYY

      REAL  FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      REAL  CFY,CMY,TB19,WV,WH,BV,BH,BDIFF,TBI37,
     &      TA1,TA2,A1,A2,B1,B2,DEN,TW37,TW19,
     &      EW19,EW37,TF37,TO37,TF19,TO19,EFY37,
     &      EMY37,EFY19,EMY19,TATM37,TATM19
      REAL    TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

      COMMON /ICEFRA/ FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      COMMON /FUNPAR/ CFY,CMY,TB19,WV,WH,BV,BH,BDIFF,TBI37,
     &                TA1,TA2,A1,A2,B1,B2,DEN,TW37,TW19,
     &                EW19,EW37,TF37,TO37,TF19,TO19,EFY37,
     &                EMY37,EFY19,EMY19,TATM37,TATM19
      COMMON /TBLR/ TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

      DATA TOL/1.0E-6/
C-----------------------------------------------------------------------
C       ___________________________
C       Check open ocean percentage.
C
        FTOT=FFY+FMY+FTI
        FW=100.0-FTOT
C       ____________________________
C       Check on multiyeAr ice part.
C
        IF (FW.LT.100.0) THEN
CX BJ 11/05/89           IF (TB37V.LT.235.0.AND.FTOT.GE.95.0) THEN
C**********NEW LIMITS FOR TB37V******************
           IF (TB37V.LT.230.0.AND.FTOT.GE.95.0 .AND. TB19V.LT.240) THEN
C             ____________________
C             There is no thin ice.
C
              FFY=(0.0392*TB37V-0.0304*TB19V-1.151)*100.0
              IF (FFY.LT.0.0) FFY=0.0
              FMY=100.-FFY
              FTI=0.0
              FTOT=FFY+FMY
              FW=100.0-FTOT
           ELSE
              CFY=FFY*0.01
              CMY=FMY*0.01
              CTI=FTI*0.01
              CT=CFY+CMY+CTI
              TB19=TB19V
              IF(CT.GT.0.0.AND.CT.LE.1.0)THEN
               TB19=(TB19V-(1.0-CT)*180.0)/CT
C  ***********HI37 NEW FUNCTION*************
               HI37=(TB37H-(1.0-CT)*130.0)/CT
              ENDIF
              IF(CT.GT.0.0.AND.TB37H.GT.175.0.AND.TB37V.gt.220) THEN
C                ____________________________
C                There is only first year ice.
C
C    ***********NEW ICE DETECTION UNIT**********
                 IF(CT.GT.1.0) CT=1.0
                 FFY=CT*(HI37-210)*4
                 FTI=CT*100-FFY
                   FMY=0.0
                 IF(FFY.GT.100)THEN
                 FFY=99.9
                 FTI=0.0
                 ENDIF
                     IF(FFY.LT.0) THEN
                 FFY=0
                 FMY=0
                 FTI=FTOT
                     ENDIF
                   IF(D.GT..95.AND.TB19V.LT.245)THEN
                 FTI=0
                  FFY=CFY*100
                   FMY=CMY*100
                   ENDIF
              END IF
           END IF
C          ____________________________________________________________
C          Do a consistency check to be sure we do have ice when we say
C          there is ice.
C
           IFLAG=0
           IF (CT.LT.0.0.OR.D.GT.DMAX.OR.(D.GT.0.15.
     &                            AND.TB37V.GT.200.0)) IFLAG=1
           IF (IFLAG.NE.1.AND.CT.LT.1.0) THEN
              WV=(TB37V-(CFY*250.0)-(CMY*186.0))/(1.0-CT)
              WH=(TB37H-(CFY*235.0)-(CMY*176.0))/(1.0-CT)
              IF (D.GT.DMIN1.AND.CT.GE.1.0.OR.(D.GT.0.0.AND.
     &            CT.LT.1.0.AND.WV.GE.202.0.AND.WH.GE.130.0)
     &            .AND.CT.GT.0.0) THEN
                BV=(TB37V-(1.0-CT)*200.0)/CT
                BH=(TB37H-(1.0-CT)*130.0)/CT
                BDIFF=BV-BH
                X200=TB37V-200.0
                IF(ABS(X200).GT.TOL) THEN
C       ***** NEW LIMITS*********
                   DF19=TB19V-TB19H
                   y180=TB19V-180
                   XXYY=y180-((X200*.62)+1.9286)
                   IF (xxyy.LT.0.0.OR.BDIFF.LT.8.0.OR.BDIFF.GT.25.0
     &                       .OR.DF19.GT.70) THEN
C    **********______________________________________
C                     We were wrong there is only open ocean
C
                      FTI=0.0
                      FFY=0.0
                      FMY=0.0
                      FTOT=0.0
                      FW=100.0
                   END IF
                END IF
              END IF
           END IF
        END IF
C       ______________________________________________________________
C       Final correction for values of fraction of ice higher than 100
C       of lower than 0.0
C
        IF (FTOT.GT.100.0) FTOT=100.0
        IF (FTI.GT.100.0) FTI=100.0
        IF (FFY.GT.100.0) FFY=100.0
        IF (FMY.GT.100.0) FMY=100.0
        IF (FW.GT.100.0) FW=100.0
C
        IF (FTOT.LT.0.0.AND.FTOT.GT.-98.0) FTOT=0.0
        IF (FW.LT.0.0.AND.FW.GT.-98.0) FW=0.0
        IF (FTI.LT.0.0.AND.FTI.GT.-98.0) FTI=0.0
        IF (FFY.LT.0.0.AND.FFY.GT.-98.0) FFY=0.0
        IF (FMY.LT.0.0.AND.FMY.GT.-98.0) FMY=0.0

        RETURN
        END
C----------------------------------------------------------------------------
        SUBROUTINE FNICE
C----------------------------------------------------------------------------
*** S/P   FNICE
*
* AUTHOR         - ARM CONSULTANTS  -  FEBRUARY 1989
*                  1404-35 FOUNTAINHEAD RD.,
*                  DOWNSVIEW, ONTARIO, M3J-2V7
*                  (416) 663-3938
*                  DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
* REVISION  001  - (NONE)
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - CALCULATE FIRST YEAR AND MULTI-YEAR ICE COMPONENTS
*
* USAGE          - CALL FNICE
*
* ARGUMENTS      - NONE
*
* IMPLICITS
*                /TBLR/   - INPUT BRIGHTNESS TEMPERATURES
*                /ICEFRA/ - ICE FRACTIONS
*                /FUNPAR/ - EMPIRICAL PARAMETERS
*                /ATMH20/ - ATMOSPHERIC RADIATIVE PROPERTIES
C----------------------------------------------------------------------------
      IMPLICIT NONE
      REAL  TAU37,TA37,TAU19,TA19
      REAL  FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      REAL  CFY,CMY,TB19,WV,WH,BV,BH,BDIFF,TBI37,
     &      TA1,TA2,A1,A2,B1,B2,DEN,TW37,TW19,
     &      EW19,EW37,TF37,TO37,TF19,TO19,EFY37,
     &      EMY37,EFY19,EMY19,TATM37,TATM19
      REAL    TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

      COMMON /ATMH20/  TAU37, TA37, TAU19, TA19
      COMMON /ICEFRA/ FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      COMMON /FUNPAR/ CFY,CMY,TB19,WV,WH,BV,BH,BDIFF,TBI37,
     &                TA1,TA2,A1,A2,B1,B2,DEN,TW37,TW19,
     &                EW19,EW37,TF37,TO37,TF19,TO19,EFY37,
     &                EMY37,EFY19,EMY19,TATM37,TATM19
      COMMON /TBLR/ TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H
C----------------------------------------------------------------------------
C       _______________________________________________________________
C       Brightness temperature corrected for atmospheric extinction and
C       for the ocean contribution at 37 GHz (TA1) and at 19 GHz (TA2).
        TA1=(TB37V-TA37)*EXP(TAU37)-TW37-(1.-EW37)*TA37*EXP(-TAU37)
        TA2=(TB19V-TA19)*EXP(TAU19)-TW19-(1.-EW19)*TA19*EXP(-TAU19)
C       _______________________________________________________
C       First year ice/ Ocean at 37 GHz (A1) and at 19 GHz (A2)
C       Old ice/Ocean at 37 GHz (B1) and at 19 GHz (B2).
        A1=TF37-TW37+TA37*EXP(-TAU37)*(EW37-EFY37)
        B1=TO37-TW37+TA37*EXP(-TAU37)*(EW37-EMY37)
        A2=TF19-TW19+TA19*EXP(-TAU19)*(EW19-EFY19)
        B2=TO19-TW19+TA19*EXP(-TAU19)*(EW19-EMY19)
        DEN=(A1*B2)-(A2*B1)
C       ___________________________________________________________
C       Fraction of first year ice (FFY) and of multiyear ice (FMY)
        FFY=((TA1*B2)-(TA2*B1))/DEN
        FMY=((TA2*A1)-(TA1*A2))/DEN
        FFY=FFY*100.0
        FMY=FMY*100.0

        RETURN
        END
C----------------------------------------------------------------------------
        SUBROUTINE ICE
C----------------------------------------------------------------------------
*** S/P    ICE
*
* AUTHOR         - ARM CONSULTANTS  -  FEBRUARY 1989
*                  1404-35 FOUNTAINHEAD RD.,
*                  DOWNSVIEW, ONTARIO, M3J-2V7
*                  (416) 663-3938
*                  DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
* REVISION  001  - (NONE)
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - CONFIRM THE DIAGNOSIS THAT ICE IS PRESENT
*                  A SERIES OF TEST ARE PERFORMED.
*
* USAGE          - CALL ICE
*
* ARGUMENTS      - NONE
*
* IMPLICITS
*                /TBLR/   - INPUT BRIGHTNESS TEMPERATURES
*                /ICEFRA/ - ICE FRACTIONS
*                /MISC/   - EMPIRICAL CONSTANTS
**
C----------------------------------------------------------------------------
      IMPLICIT NONE
      REAL  CFF

      REAL  FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      REAL  DIFF,TBCUT,WCUT,CA,CV,CH,WI1,WI2,X1H,X1V,
     &      CON1,CON2,CON3,CON4,CON5,CON6
      REAL    TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

      COMMON /ICEFRA/ FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      COMMON /MISC/   DIFF,TBCUT,WCUT,CA,CV,CH,WI1,WI2,X1H,X1V,
     &                CON1,CON2,CON3,CON4,CON5,CON6
      COMMON /TBLR/ TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

C----------------------------------------------------------------------------
      FMY=FTOT-FFY
      IF (CT.LE.50.0) THEN
          FTOT=(CH+CV)/2.0
C         CHANGE MADE 25/03/90; NEW ICE DETECTION  ON
          CFF=FTOT/100
          FTI=(CFF*105-TB37H+130)*FTOT/20
          IF(FTI.GT.0) THEN
             FFY=FTOT-FTI
          ELSE
             FTOT=FTI
             FFY=0
          ENDIF
          FMY=0.0
       ELSE
C         ____________________________________________
C         Further check to confirm the presence of ice
          CALL CONFIR
       END IF

       RETURN
       END
C----------------------------------------------------------------------------
        SUBROUTINE NOICET
C----------------------------------------------------------------------------
*** S/P   NOICET
*
* AUTHOR         - ARM CONSULTANTS  -  FEBRUARY 1989
*                  1404-35 FOUNTAINHEAD RD.,
*                  DOWNSVIEW, ONTARIO, M3J-2V7
*                  (416) 663-3938
*                  DSS CONTRACT NUMBER: KM173-7-7014/02-AJ
*
* REVISION  001  - (NONE)
*
* STATUS         - DEVELOPMENT -
*
* LANGUAGE       - FORTRAN ANSI-77
*
* OBJECT         - PREVIOUS TESTS FOUND THAT NO ICE AT ALL IS PRESENT
*                  A FINAL VERIFICATION IS MADE BEFORE CONCLUDING THAT
*                  WIND ALTERED THE BRIGHTNESS TEMPERATURES.
*
* USAGE          - CALL NOICET
*
* ARGUMENTS      - NONE
*
* IMPLICITS
*                /TBLR/   - INPUT BRIGHTNESS TEMPERATURES
*                /ICEFRA/ - ICE FRACTIONS
*                /FUNPAR/ - EMPIRICAL PARAMETERS
**
C----------------------------------------------------------------------------
      IMPLICIT NONE
      REAL  WI37 , WI19 , GRD

      REAL  FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      REAL  CFY,CMY,TB19,WV,WH,BV,BH,BDIFF,TBI37,
     &      TA1,TA2,A1,A2,B1,B2,DEN,TW37,TW19,
     &      EW19,EW37,TF37,TO37,TF19,TO19,EFY37,
     &      EMY37,EFY19,EMY19,TATM37,TATM19
      REAL    TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

      COMMON /ICEFRA/ FFY,FMY,FTI,FTOT,FW,CT,DMIN1,DMAX,D,DMIN2
      COMMON /FUNPAR/ CFY,CMY,TB19,WV,WH,BV,BH,BDIFF,TBI37,
     &                TA1,TA2,A1,A2,B1,B2,DEN,TW37,TW19,
     &                EW19,EW37,TF37,TO37,TF19,TO19,EFY37,
     &                EMY37,EFY19,EMY19,TATM37,TATM19
      COMMON /TBLR/ TB19V, TB19H, TB22V, TB37V, TB37H, TB85V, TB85H

C----------------------------------------------------------------------------
C     ___________________________________________________________
C     To calculate the atmospheric offset at 37 GHz and at 19 GHz.
      WI37=.767*TB37H-1.1765*TB37V+135.87
      WI19=(.65*(TB19H-100)-(TB19V-180))/.5
      TATM37=TB37V-200.0
      TATM19=TB19V-180
C     ***  NEW CONDITION ***
      GRD=TATM19-(.62*TATM37+1.92)
      IF (GRD.GT.0.0.AND.D.GT.0.15.AND.TB37V.GE.220) THEN
C        ____________________________________________
C        There is ice. Previous tests are overwritten
         CALL CONFIR
      ELSE IF (CT.LT.0.0.OR.D.LT.DMIN1) THEN
C        ________________
C        There is no ice.
         FFY=0.0
         FMY=0.0
      END IF

      RETURN
      END
C
C-----------------------------------------------------------------------------
CJacques Halle,                          Tel     : (514) 421-4660
CSatellite Data Assimilation Division    Fax     : (514) 421-4657
CCanadian Meteorological Centre          Internet: Jacques.Halle@ec.gc.ca
C2121 Trans Canada Highway
CNorth Service Road
CDorval, Quebec, H9P 1J3
CCanada
C-----------------------------------------------------------------------------
