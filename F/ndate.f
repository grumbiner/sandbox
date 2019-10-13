      program ndate
c
c read from standard input change in hours and yyyymmddhh,
c get back new yyyymmddhh
c
      integer time(2)
c
      read (*,*) dt, itime
      dth = 3600. * dt
c
      time(1) = itime/100
      time(2) = mod(itime,100) * 10000
      call TICK21 ( time , dth ) 
c
      write (*,'(i8.8,i2.2)') time(1), time(2)/10000
c
      end
C/ ------------------------------------------------------------------- /
      SUBROUTINE TICK21 ( TIME, DTIME )
C/
C/                  +-----------------------------------+
C/                  | WAVEWATCH-III           NOAA/NCEP |
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         29-Mar-1993 |
C/                  +-----------------------------------+
C/                                Based on TICK of the GLA GCM.
C/
C  1. Purpose :
C
C     Updates time information, DTIME=0 converts to "legal" time.
C     Goes into the 21st century.
C
C  3. Parameters :
C
C     Parameter list
C     ----------------------------------------------------------------
C       TIME    I.A.  I/O  (1) Current date in YYYYMMDD format.
C                          (2) Current time in HHMMSS format.
C       DTIME   Real   I   Time step in seconds.
C     ----------------------------------------------------------------
C
C  4. Subroutines used :
C
C       IYMD21   Increment date in YYYYMMDD format.
C       STRACE   Service routine
C
C  5. Called by :
C
C     Any other routine.
C
C  8. Structure :
C
C     See source code.
C
C  9. Switches :
C
C     C/S  Enable subroutine tracing using STRACE.
C
C 10. Source code :
C/
C/ ------------------------------------------------------------------- /
C/ Parameter list
C/
      INTEGER         TIME(2)
      REAL            DTIME
C/
C/ ------------------------------------------------------------------- /
C/ Local parameters
C/
      INTEGER         NYMD, NHMS, NSEC
C/S      INTEGER         IENT
      INTEGER         IYMD21
      EXTERNAL        IYMD21
C/
C/ ------------------------------------------------------------------- /
C/
C/S      SAVE IENT
C/S      DATA IENT /0/
C/S      CALL STRACE (IENT, 'TICK21')
*
* Zero increment: get "legal" data
*
      NYMD   = TIME(1)
      NHMS   = TIME(2)
      IF (DTIME.EQ.0.) THEN
          NYMD = IYMD21 (NYMD,-1)
          NYMD = IYMD21 (NYMD, 1)
        ENDIF
*
* Convert and increment time :
*
      NSEC = NHMS/10000*3600 + MOD(NHMS,10000)/100* 60 +
     &       MOD(NHMS,100) + NINT(DTIME)
*
* Check change of date :
*
  100 CONTINUE
      IF (NSEC.GE.86400)  THEN
          NSEC = NSEC - 86400
          NYMD = IYMD21 (NYMD,1)
          GOTO 100
        ENDIF
*
  200 CONTINUE
      IF (NSEC.LT.00000)  THEN
          NSEC = 86400 + NSEC
          NYMD = IYMD21 (NYMD,-1)
          GOTO 200
        ENDIF
*
      NHMS = NSEC/3600*10000 + MOD(NSEC,3600)/60*100 + MOD(NSEC,60)
*
      TIME(1) = NYMD
      TIME(2) = NHMS
*
      RETURN
C/
C/ End of TICK21 ----------------------------------------------------- /
C/
      END
C/ ------------------------------------------------------------------- /
      INTEGER FUNCTION IYMD21 ( NYMD ,M )
C/
C/                  +-----------------------------------+
C/                  | WAVEWATCH-III           NOAA/NCEP |
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         18-Oct-1998 |
C/                  +-----------------------------------+
C/                                Based on INCYMD of the GLA GCM.
C/
C  1. Purpose :
C
C     Increment date in YYYYMMDD format by +/- 1 day.
C
C  3. Parameters :
C
C     Parameter list
C     ----------------------------------------------------------------
C       NYMD    Int.   I   Old date in YYMMDD format.
C       M       Int.   I   +/- 1 (Day adjustment)
C     ----------------------------------------------------------------
C
C  4. Subroutines used :
C
C       STRACE
C
C  5. Called by :
C
C     Any subroutine.
C
C  8. Structure :
C
C     See source code.
C
C  9. Switches :
C
C     C/S  Enable subroutine tracing using STRACE.
C
C 10. Source code :
C
C/
C/ ------------------------------------------------------------------- /
C/ Parameter list
C/
      INTEGER         NYMD, M
C/
C/ ------------------------------------------------------------------- /
C/ Local parameters
C/
      INTEGER         NDPM(12), NY, NM, ND
C/S      INTEGER         IENT
      LOGICAL         LEAP
C/
C/ ------------------------------------------------------------------- /
C/
C/S      SAVE     IENT
C/S      DATA     IENT /0/
      DATA     NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
C/S      CALL STRACE (IENT, 'IYMD21')
*
* "Unpack" and increment date :
*
      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      NM   = MIN ( 12 , MAX(1,NM) )
      ND   = MOD(NYMD,100) + M
      LEAP = MOD(NY,400).EQ.0 .OR.
     &        ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
*
* M = -1, change month if necessary :
*
      IF (ND.EQ.0) THEN
          NM   = NM - 1
          IF (NM.EQ.0) THEN
              NM   = 12
              NY   = NY - 1
            ENDIF
          ND   = NDPM(NM)
          IF (NM.EQ.2 .AND. LEAP)  ND = 29
        ENDIF
*
* M = 1, leap year
*
      IF (ND.EQ.29 .AND. NM.EQ.2 .AND. LEAP)  GO TO 20
*
*        next month
*
      IF (ND.GT.NDPM(NM)) THEN
          ND = 1
          NM = NM + 1
          IF (NM.GT.12) THEN
              NM = 1
              NY = NY + 1
          ENDIF
        ENDIF
*
   20 CONTINUE
      IYMD21 = NY*10000 + NM*100 + ND
*
      RETURN
C/
C/ End of IYMD21 ----------------------------------------------------- /
C/
      END
