      SUBROUTINE SFCEDASC
C**********************************************************************
                             P A R A M E T E R
     & (D00=0.0,D5=5.E-1,D01=1.00E-2,H1=1.0,HM1=-1.0
     &, H90=90.0,H360=360.0,EPS=1.E-10)
C----------------------------------------------------------------------
      integer,parameter::real_32=selected_real_kind(6,30)
C-----------------------------------------------------------------------
C                            I N C L U D E S
C DEFINE TARGET ETA GRID THAT THIS GRDETA EXECUTE CREATES
      include "parmeta.res"
      include "parmsoil"
C DEFINE GRID OF INPUT ETA RESTART FILE
      include "parmeta.resin"
      include "griddef.in"
C----------------------------------------------------------------------
                             P A R A M E T E R
     & (LP1=LM+1)
C----------------------------------------------------------------------
                             C O M M O N  /PTETA/
     & IDAT(3),PT,DETA(LM),AETA(LM),ETA(LP1),DFL(LP1)
     &,RES   (IM,JM),FIS   (IM,JM),ALBEDO   (IM,JM)
     &,SNO   (IM,JM),SST   (IM,JM),SI    (IM,JM)
     &,SM    (IM,JM),LMH   (IM,JM),LMV   (IM,JM)
     &,SMC(IM,JM,NSOIL),STC(IM,JM,NSOIL),CMC(IM,JM),SH2O(IM,JM,NSOIL)
C-----------------------------------------------------------------------
                             C O M M O N /MASKS/
     & HBM2  (IM,JM),VBM2  (IM,JM),VBM3  (IM,JM),SICE (IM,JM)
     &,HTM   (IM,JM,LM),VTM   (IM,JM,LM)
C-----------------------------------------------------------------------
                             C O M M O N /LATLONS/
     & HLAT(IM,JM),HLON(IM,JM),VLAT(IM,JM),VLON(IM,JM)
C-----------------------------------------------------------------------
c                            C O M M O N  /SFCTYPES/
c    & IVGTPK(IM,JM),ISLTPK(IM,JM),ISPTPK(IM,JM)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & IDATE(4),IDATI(3)
C 
      DIMENSION ISLTPKI(IM,JM)
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::CMCI
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::SMI,SICEI
c     REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::IVGTPKI,ISLTPKI,ISPTPKI
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::SMCI,STCI,SH2OI
C-----------------------------------------------------------------------
C COMMENTED BLOCK BELOW IS FORMER NON-DYNAMIC MEMORY ALLOCATION
C                             D I M E N S I O N
C     & IDATE(4),IDATI(3)
C     &,SMCI(IMI,JMI,NSOIL),STCI(IMI,JMI,NSOIL),SH2OI(IMI,JMI,NSOIL)
C     &,CMCI(IMI,JMI)
C     &,SMI(IMI,JMI),SICEI(IMI,JMI)
C     &,IVGTPKI(IMI,JMI),ISLTPKI(IMI,JMI),ISPTPKI(IMI,JMI)
C-----------------------------------------------------------------------
                             D A T A
     & NHIBUI/34/, NRESTRTI/36/, LDATE/52/
C-----------------------------------------------------------------------

C READ DATE AND HOUR
      REWIND LDATE
      READ(LDATE) IDATE
      IHRST=IDATE(1)
      READ(NRESTRTI) DUM,IDATI,IHRSTI
      REWIND NRESTRTI
C CHECK THAT VAILD DATE/TIME OF INPUT RESTART FILE MATCHES EXPECTED DATE/TIME
      IF ( (IHRST .NE. IHRSTI) .OR. (IDAT(1) .NE. IDATI(1)) .OR.
     1     (IDAT(2) .NE. IDATI(2)) .OR. (IDAT(3) .NE. IDATI(3)) ) THEN
        PRINT *,'MIS-MATCH WITH INPUT EDAS DATE/TIME - STOP EXECUTION'
        PRINT *,IHRST,IHRSTI,IDAT(1),IDATI(1),IDAT(2),IDATI(2),
     1     IDAT(3),IDATI(3)
C       STOP 666
      ENDIF
C
C ALLOCATE DYNAMIC MEMORY
C
      ALLOCATE(CMCI(IMI,JMI))
      ALLOCATE(SMI(IMI,JMI))
      ALLOCATE(SICEI(IMI,JMI))
c     ALLOCATE(IVGTPKI(IMI,JMI))
c     ALLOCATE(ISLTPKI(IMI,JMI))
c     ALLOCATE(ISPTPKI(IMI,JMI))
      ALLOCATE(SMCI(IMI,JMI,NSOIL))
      ALLOCATE(STCI(IMI,JMI,NSOIL))
      ALLOCATE(SH2OI(IMI,JMI,NSOIL))
C
C INPUT EDAS LAND SURFACE STATES FROM NRESTRTI FILE
C
C SKIPPING:  2+LM+3+LM*9+14 RECORDS
C
      NSKIP=19+10*LMI
      DO ISKIP=1,NSKIP
        READ(NRESTRTI) DUM
      ENDDO
C
C READ TOTAL SOIL MOISTURE (SMCI),UNFROZEN SOIL MOISTURE (SMH2OI),
C CANOPY WATER (CMCI), SOIL TEMP (STCI)
C
      READ(NRESTRTI) SMCI
      READ(NRESTRTI) CMCI
      READ(NRESTRTI) STCI      
C
C INPUT EDAS LAND SURFACE CHARACTERISTICS FROM NHIBUI FILE
C
C SKIPPING:  6 RECORDS
C
      NSKIP=6
      DO ISKIP=1,NSKIP
        READ(NHIBUI) DUM
      ENDDO
C
C READ SEA ICE/LAND MASK FLAGS:  SMI, SICEI
C
      READ(NHIBUI) SMI
      READ(NHIBUI) SICEI
      print *,im,jm,imi,jmi
C
C SKIPPING:  LM+LM+25 RECORDS
C
      NSKIP=2*LM+25
      DO ISKIP=1,NSKIP
        READ(NHIBUI) DUM
      ENDDO
C
C READ VEG TYPE (IVGTPKI), SOIL TYPE (ISLTPKI), SLOPE TYPE (ISPTPKI)
C
      READ(NHIBUI) DUM
      READ(NHIBUI) ISLTPKI
      READ(NHIBUI) DUM
C
C ENTER PRIMARY TASK LOOP TO FIND NEAREST LAND NEIGHBOR AND
C ASSIGN NEAREST LAND NEIGHBOR STATES AND CHARACTERISTICS
C
      DO  200 J = 1, JM
        DO 100 I = 1, IM
C IF NON LAND-MASS POINT, SKIP TO END OF LOOP (100)
          IF ((SM(I,J) .GT. 0.5) .OR. (SICE(I,J) .GT. 0.5)) GOTO 100
C DETERMINE LAT/LON OF TARGET GRID POINT
          GLATD = HLAT(I,J)
          GLOND = H360 - HLON(I,J)
C DETERMINE NEAREST NEIGHBOR FROM INPUT GRID
          CALL LL2ETA(GLATD,GLOND,IMI,JMI,TPH0DI,TLM0DI,DPHDI,DLMDI,
     1      II,JI)
c          write(*,*)i,j,ii,ji
C
C CHECK TO SEE THAT II,JI WITHIN IMI,JMI RANGE, IF NOT IN RANGE, THEN
C THE TARGET GRID POINT I,J IS OUTSIDE DOMAIN OF INPUT EDAS GRID,
C THEREFORE JUMP TO NEXT I INDEX LOOP (100), THUS RETAIN GDAS DEFAULT.
C
           IF ( (II .LT. 1) .OR. (II .GT. IMI) ) GOTO 100
           IF ( (JI .LT. 1) .OR. (JI .GT. JMI) ) GOTO 100
c
c below are debugging lines
c           if ( (ii .lt. 1) .or. (ii .gt. imi) ) then
c	     print *,'target point outside input domain'
c	     print *,glatd,glond,im,jm,i,j,ii,ji,imi,jmi
c	     goto 100
c	   endif
c           if ( (ji .lt. 1) .or. (ji .gt. jmi) ) then
c	     print *,'target point outside input domain'
c	     print *,glatd,glond,im,jm,i,j,ii,ji,imi,jmi
c	     goto 100
c           endif
C
C CHECK TO SEE IF NEAREST NEIGHBOR IS LAND MASS
C
          IF ((SMI(II,JI).GT. 0.5).OR.(SICEI(II,JI) .GT. 0.5)) THEN
C
C SEARCH FOR NEAREST LAND MASS NEIGHBOR (TO A LIMIT OF KRAD=5)
	    DO 50 KRAD=1,5
	      DO 40 JY = JI-KRAD,JI+KRAD
                DO 30 IX = II-KRAD,II+KRAD
                  IF ((SMI(IX,JY)   .GT. 0.5) .OR.
     1                (SICEI(IX,JY) .GT. 0.5)) GOTO 30 
C WE HAVE FOUND A LAND POINT - REDEFINE II,JI
                  II=IX
		  JI=JY
		  GOTO 60
  30            CONTINUE
  40          CONTINUE
  50        CONTINUE
C
C END OF LOOP (50) REACHED WITH NO NEARBY LAND NEIGHBOR FOUND
C THEREFORE RETAIN GDAS DEFAULT
C
            PRINT *,'NO NEARBY LAND NEIGHBOR AT:',I,J,GLATD,GLOND
	    GOTO 100
          ENDIF
C TAKE NEAREST LAND NEIGHBOR STATES
  60      CONTINUE
c         IVGTPK(I,J) =IVGTPKI(II,JI)
c         ISLTPK(I,J) =ISLTPKI(II,JI)
c         ISPTPK(I,J) =ISPTPKI(II,JI)
          CMC(I,J)    =CMCI(II,JI)
          DO 70 K=1,NSOIL
            SMC(I,J,K) =SMCI(II,JI,K)
            STC(I,J,K) =STCI(II,JI,K)
C ----------------------------------------------------------------------
C cold start with first guess of liquid soil water content following
C explicit solution for Flerchinger Eqn from Koren et al, JGR, 1999,
C Eqn 17 (see NOAH LSM, FCT FRH2O).
            IF (STC(I,J,K) .LT. 273.149) THEN
              SH2O(I,J,K)=SH2OINIT(STC(I,J,K),SMC(I,J,K),ISLTPKI)
            ELSE
              SH2O(I,J,K)=SMC(I,J,K)
            ENDIF
  70      CONTINUE
 100    CONTINUE
 200  CONTINUE

C
C DEALLOCATE DYNAMIC MEMORY
C
      DEALLOCATE(CMCI)
      DEALLOCATE(SMI)
      DEALLOCATE(SICEI)
c     DEALLOCATE(IVGTPKI)
c     DEALLOCATE(ISLTPKI)
c     DEALLOCATE(ISPTPKI)
      DEALLOCATE(SMCI)
      DEALLOCATE(STCI)
      DEALLOCATE(SH2OI)

      RETURN
      END
