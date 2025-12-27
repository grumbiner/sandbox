      SUBROUTINE SNOHIRES (SI,SM,GLAT,GLON,LIST,LUSAF,DTR,missno,idate)
C
C  INPUTS:  SM (ETA LAND/SEA MASK)
C            GLAT (LATITUDE ARRAY OF ETA GRID)
C            GLON (LONGITUDE ARRAY OF ETA GRID)
C            LIST (UNIT NUMBER OF PRINTOUT)
C            LUSAF (LOGICAL - TRUE:USE IMS SNOW AND USAF SNOW, 
C                            FALSE: USE IMS SNOW ONLY)
C            DTR   (DEGREES TO RADIANS CONVERSION FACTOR)
C
C  OUTPUTS: SI (SNOWDEPTH AND SEA-ICE ON ETA GRID, SEE CALLING
C                SUBROUTINE CNSTS FOR MORE DETAILS ON SI OUTPUT)
C
      IMPLICIT REAL (A-H, O-Z)
C
      INCLUDE "parmeta.res"
C
      LOGICAL*1  LUSAF,bitice(im,jm),missno
      INTEGER KPDS(25), KGDS(22), JPDS(25), JGDS(22)
C
      REAL         AFSNO   (512,512),snotmp(512,512)
      COMMON /AFMSK/ MSKAF   (512,512)
C     INTEGER      MSKAF   (512,512)
C
c     REAL         SCVH    (1024,1024)
c     COMMON /IMMSK/ MSKSCVH (1024,1024)
      integer idate(4)
C     INTEGER      MSKSCVH (1024,1024)
C
      REAL         SI(IM,JM), SM(IM,JM), GLAT(IM,JM), GLON(IM,JM)
C
C  INPUT UNITS (45-NESDIS DAILY SNOW/ICE, 41-USAF DAILY SNOW/ICE,
C                43-NESDIS 1/16-BEDIENT LAND/SEA MASK,
C                42-USAF   1/8-BEDIENT LAND/SEA MASK)
C 
      DATA   INSNOAF/41/, INMSKAF/42/
C
C
C**************************  BEGIN EXECUTION ***********************
C
      print *,' welcome to snohires'
C     SPECIFY THE UNIT NUMBER OF THE PRINTER.
      IOUTUPRT = LIST
C
C     SPECIFY PARAMETERS OF THE N.H. 1024X1024 IMS 1/16-MESH GRID
C
C     LOCATION OF THE POLE:
      XPNMC8 = 513.
      YPNMC8 = 512.
C     N -- THE NUMBER OF GRID INTERVALS FROM THE POLE TO THE EQUATOR:
      ENNMC8 = 16.0E0 * 31.2043316E0
C     THE LONGITUDINAL ROTATION ANGLE OF THE GRID
      ALNMC8 = 10.E0
C     THE ORIENTATION WEST LONGITUDE OF THE GRID:
      ORIENT8 = 80.E0
C
C  SPECIFY PARAMETERS OF THE N.H. 512 X 512 GRID TYPE 88 (USAF GRID).
C
C  LOCATION OF POLE:
      XPNMCAF = 257.E0
      YPNMCAF = 256.E0
C  GRID MESH LENGTH AT 60N IN KM (FYI, BUT NOT NEEDED)
C.... XMESHL  = 47.625E0
C  NUMBER OF GRID INTERVALS FROM POLE TO EQUATOR
      ENNMCAF = 8.0 * 31.2043316E0
C  THE LONGITUDE ROTATION ANGLE OF THE GRID
      ALNMCAF = 10.E0
C  THE ORIENTATION WEST LONGITUDE OF THE GRID
      ORIENTAF  = 80.E0
C
C  PRINT SPECIFIED USAF SNOW PROCESSING FLAG
C
      WRITE(IOUTUPRT,2211)   LUSAF
 2211 FORMAT(//1H ,5X,'SUBROUTINE SNOHIRES                    '/
     1     1H ,1X,'  WILL TRY TO PROCESS USAF SNOWCOVER: LUSAF = ',L2)
C
      IF (LUSAF) THEN
C
C  READ AIR FORCE 1/8-BEDIENT DAILY N.H. SNOW/ICE VIA SNO8GET
C
      CALL SNO8GET(AFSNO,IYEAR,IMONTH,IDAY,INSNOAF,LIST,LUSAF,idate,
     *   iret)
C
C  IF I/O ERROR ENCOUNTERED IN SNO8GET, THEN LUSAF IS RETURNED FALSE
C 
        IF (.NOT. LUSAF) THEN
          WRITE(IOUTUPRT,56)
   56     FORMAT(1H ,'ERROR: I/O ERR IN READ OF USAF SNOW ANAL')
c         print*,'GRDETA TERMINATING IN SNOHIRES'
c         stop 56
          if(iret.eq.99) then
           afsno=999.9
           missno=.true.
           print*,'SNOW IS MISSING'
          endif
        ENDIF
c
c The snow received from COLA currently has the rows flipped so that
c j=1 is at the top of the domain and j=512 is at the bottom.  Also,
c the columns are also flipped.  The following code flips the snow, first
c the columns, then the rows.
c PS - 10 December 2001
c
      do j=1,512
      do i=1,512
       snotmp(i,j)=afsno(i,513-j)
      enddo
      enddo
c
c Put the new values from the snotmp array into the snodep array.
c
      do j=1,512
      do i=1,512
       afsno(i,j)=snotmp(i,j)
      enddo
      enddo
c
c Now do the i-columns.
c
      do j=1,512
      do i=1,512
       snotmp(i,j)=afsno(513-i,j)
      enddo
      enddo
c
      do j=1,512
      do i=1,512
       afsno(i,j)=snotmp(i,j)
      enddo
      enddo

      ENDIF
Ctest
Ctest      write(60,7702) IYEAR, IMONTH, IDAY
Ctest  7702 format('USAF DATA: ', 3i2.2)
Ctest
C
C    NOTE:  UPON RETURN FROM CALL SNO8GET ABOVE, USAF SNOW/ICE FIELD
C            HAS FOLLOWING PHYSICAL RANGES:
C         - VALUES OVER SEA POINTS ARE 0.0 OR 11.0 (SEA-ICE FLAG)
C         - VALUES OVER LAND/COAST ARE 0.0 OR POS DEPTH IN METERS
C         - SNOWDEPTH OVER LAND IS ACTUAL, NOT WATER EQUIVALENT YET
C
C   READ THE USAF AFGWC LAND/COAST/SEA MASK
C                           (SEA=1,LAND=2,COASTAL-LAND=4,OFFWORLD=9)
CER   READ(INMSKAF)  MSKAF
C
C---------- I/O OF PRIMARY INPUT FIELDS IS COMPLETE -------------
C                        INIT RADIANS TO DEGREES
      RTD = 1./DTR
C
       IF ( LUSAF) THEN
         WRITE(IOUTUPRT,2321) LUSAF,IDEPTH
 2321    FORMAT(1H //' USAF SNODEP ANAL WILL BE USED, LUSAF=',L2/
     1    1H , 35X,'SNODEPTH THRESHOLD (TENTHS OF INCHES) =',I3)
         CALL PRINTAF (AFSNO,MSKAF)
      ELSE
         WRITE(IOUTUPRT,2322) LUSAF
 2322    FORMAT(1H //' USAF SNODEP ANALYSIS WILL BE IGNORED'/
     1          1H , 35X,'LOGICAL FLAG LUSAF=',L2)
      ENDIF
C
C----------INITIALIZE SNOW/ICE ARRAYS TO ZERO ON ETA GRID------------
C
      SI = 0.0
C--------------------------------------------------------------------
C
C ****** NOW BEGIN MAJOR LOOP OVER ALL ETA GRIDS AND POINTS *******
C
         DO J=1,JM
         DO I=1,IM
C
C--------------- DETERMINE LAT/LON OF ETA GRID POINT -------------
C                    (HERE LONG WILL BE EAST LONG)
C
      YYLAT = GLAT(I,J)*RTD
      XLONG = 360. - GLON(I,J)*RTD
c     print*,'i,j,glat,glon=',i,j,glat(i,j),glon(i,j)
c     print*,'yylat,xlong=',yylat,xlong
      print*,'i,j,sm(i,j)=',i,j,sm(i,j)
C
C  WHERE ETA DOMAIN SOUTH OF 22 N LAT (INCLUDING ANY S.H.),
C  WE KEEP DEFAULT ZERO SNOW/ICE 
C
      IF (YYLAT.LT.22.0E0) GO TO 4300
C 
      IF (.NOT. LUSAF) GO TO 4300
C
C
C-------------- BEGIN USAF SNOW/ICE INTERPOLATION------------------
c  If current USAF snow/ice anal was successfully read (lusaf=true), 
c  add the USAF information as follows:
C
C--------------------------------------IF ETA SEA POINT, SKIP USAF ANL
c
      IF ( SM(I,J) .GT. 0.9 ) GO TO 4300
C
C-------------------------------------------------------------------
C  THIS IS AN ETA LAND POINT, SO APPLY USAF SNOWDEPTH ANAL
C
C  DETERMINE LOCATION OF ETA POINT ON THE 512 X 512 USAF GRID
C
         RM= ENNMCAF*COS(YYLAT * DTR) / (1.E0 + SIN(YYLAT * DTR))
         RAD = (XLONG - ALNMCAF) * DTR
         X = XPNMCAF + RM * COS(RAD)
         Y = YPNMCAF + RM * SIN(RAD)
C
      IS  = INT(X)
      IP1 = IS + 1
      JS  = INT(Y)
      JP1 = JS + 1
C
C-----IF OUTSIDE OF USAF GRID DOMAIN (I.E. S.H.) WE KEEP ZERO DEFAULT--
C
c Next domain check not needed because we are already north of 22 N
c latitude. PS - 21 December 2001
c
c     IF ((IS .LT. 1) .OR. (IS .GT. 511) .OR. (JS .LT. 1)
c    1         .OR. (JS .GT. 511))  THEN
c       GO TO 4300
c     ENDIF
C--------------------------------------------------------------------
C
C  NOW USE ETA AND USAF LAND-SEA MASK TO ENSURE ONLY LAND POINTS ARE 
C  INTERPOLATED TO LAND POINTS (TO DETERMINE SNOW)
C   (USAF LAND MASK: SEA=1,LAND=2,COASTAL-LAND=4,OFFWORLD=9)
C   NOTE: IN REACHING THIS STAGE, WE HAVE ALREADY INSURED WE ARE ON 
C   ETA LAND POINT AND NORTH OF 22N LAT., I.E. NOT MSKAF=9 (I.E. NOT OFFWORLD)
C  
      ILAND = 2
C
      IPOINT = NINT(X)
      JPOINT = NINT(Y)
      IF ( MSKAF(IPOINT,JPOINT) .GE. ILAND) THEN
        SI(I,J) = AFSNO(IPOINT,JPOINT)
        GO TO 4300
      ENDIF
C
      KOUNT = 0
C
      XRATIO = X - REAL(IS)
      YRATIO = Y - REAL(JS)
C
      AREA11 = (1.0E0 - XRATIO) * (1.0E0 - YRATIO)
      AREA21 = XRATIO * (1.0E0 - YRATIO)
      AREA12 = (1.0E0 - XRATIO) * YRATIO
      AREA22 = XRATIO * YRATIO
C
      IF( MSKAF(IS, JS) .GE. ILAND) THEN
         KOUNT  = KOUNT + 1
         AREA   = AREA11
         IPOINT = IS
         JPOINT = JS
      END IF
C
      IF( MSKAF(IS, JP1) .GE. ILAND ) THEN
         KOUNT = KOUNT +1
         IF (KOUNT .EQ. 1) THEN
            IPOINT = IS
            JPOINT = JP1
         ELSEIF (AREA12 .GT. AREA) THEN
            AREA   = AREA12
            IPOINT = IS
            JPOINT = JP1
         END IF
      END IF
C
      IF( MSKAF(IP1, JS) .GE. ILAND ) THEN
         KOUNT = KOUNT + 1
         IF (KOUNT .EQ. 1) THEN
            AREA   = AREA21
            IPOINT = IP1
            JPOINT = JS
         ELSEIF (AREA21 .GT. AREA) THEN
            AREA   = AREA21
            IPOINT = IP1
            JPOINT = JS
         END IF
      END IF
C
      IF( MSKAF(IP1, JP1) .GE. ILAND ) THEN
         KOUNT = KOUNT + 1
         IF (KOUNT .EQ. 1) THEN
            AREA   = AREA22
            IPOINT = IP1
            JPOINT = JP1
         ELSEIF (AREA22 .GT. AREA) THEN
            AREA   = AREA22
            IPOINT = IP1
            JPOINT = JP1
         END IF
      END IF
C
C     DETERMINE SNOW USING THE CLOSEST LAND POINT SURROUNDING
C     ETA GRID POINT WITH THE SAME LAND-SEA MASK FLAG
C
      IF (KOUNT .GT. 0) THEN
          SI(I,J) = AFSNO(IPOINT,JPOINT)
C
      ELSE
C
C         NO IMMEDIATELY SURROUNDING POINTS IN THE 512 X 512 FIELD OF
C         SNOW HAVE THE SAME LAND MASK AS THE ETA POINT.
C         THE ETA POINT MAY BE AN ISLAND.
C         SO EXPAND SEARCH RADIUS AND TAKE FIRST MASK FLAG MATCH
C
          IPOINT = NINT(X)
          JPOINT = NINT(Y)
C
          DO 7346  LL=1,7
           JPE = MIN (512, JPOINT+LL)
           JPB = MAX (1 ,  JPOINT-LL)
           IPE = MIN (512, IPOINT+LL)
           IPB = MAX (1 ,  IPOINT-LL)
C
             DO 6346 MK=JPB,JPE
             DO 6346 NK=IPB,IPE
               IF (MSKAF(NK,MK) .GE. ILAND) THEN
               SI(I,J) = AFSNO(NK,MK)
               GO TO 4300
               ENDIF
 6346        CONTINUE
 7346     CONTINUE
C
C   NO LAND MASK MATCH FOUND, SO WE PRINT WARNING AND STOP.
c
          print*,'*** WARNING ***..NO USAF LAND MSK MATCH '
          print*,'AT ETA-I,J,ELAT,ELON,USAF-I,J:',
     *    I,J,YYLAT,XLONG,IS,JS,SI(I,J)
c         WRITE (IOUTUPRT, 7347) I,J,YYLAT,XLONG,IS,JS,SI(I,J)
c7347      FORMAT(1H ,'*** WARNING ***..NO USAF LAND MSK MATCH ',
c    1 ' AT ETA-I,J,ELAT,ELON,USAF-I,J:',2I6,2F7.2,2I3/)
          print*,'TERMINATING GRDETA IN SNOHIRES'
          stop7347
C
      ENDIF
C     
C
C******************** END MAJOR ETA GRID POINT LOOP ****************
C
 4300 CONTINUE
         ENDDO
         ENDDO
C
C  PRINT SAMPLE OF SNOW/ICE ON ETA GRID
C
      CALL PRINTETA (SI,SM)
Ctest CALL PRINTYL(SI,SM)

      RETURN

      END
