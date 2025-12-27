      SUBROUTINE SFCEDASC
C**********************************************************************
                             P A R A M E T E R
     & (D00=0.0,D5=5.E-1,D01=1.00E-2,H1=1.0,HM1=-1.0
     &, H90=90.0,H360=360.0,EPS=1.E-10)
C----------------------------------------------------------------------
C Parameter used in cold start SH2O initialization
      PARAMETER (NSOTYP=9)
C----------------------------------------------------------------------
      integer,parameter::real_32=selected_real_kind(6,30)
C-----------------------------------------------------------------------
C                            I N C L U D E S
C DEFINE TARGET ETA GRID THAT THIS GRDETA EXECUTE CREATES
      include "parmeta.res"
      include "parmsoil"
C DEFINE GRID OF INPUT ETA RESTART FILE
      include "parmeta80.resin"
      include "griddef80.in"
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
C Soil parameter arrays used in cold start SH2O initialization
      DIMENSION PSIS(NSOTYP),BETA(NSOTYP),SMCMAX(NSOTYP)
C-----------------------------------------------------------------------
                             D A T A
c    & NHIBUI/34/, NRESTRTI/36/, LDATE/52/
     $ NHIBUI/48/, NRESTRTI/49/, LDATE/52/
C-----------------------------------------------------------------------
C Constants used in cold start SH2O initialization
      DATA HLICE/3.335E5/,GRAV/9.81/,T0/273.15/
      DATA BLIM/5.5/
      DATA PSIS /0.04,0.62,0.47,0.14,0.10,0.26,0.14,0.36,0.04/
      DATA BETA /4.26,8.72,11.55,4.74,10.73,8.17,6.77,5.25,4.26/
      DATA SMCMAX /0.421,0.464,0.468,0.434,0.406,
     &             0.465,0.404,0.439,0.421/ 
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
C ----------------------------------------------------------------------
C IF NON LAND-MASS POINT, SKIP TO END OF LOOP (100), I.E. ACCEPT GDAS
C VALUES OVER OPEN OCEAN AND SEA-ICE
          IF ((SM(I,J) .GT. 0.5) .OR. (SICE(I,J) .GT. 0.5)) GOTO 100
C DETERMINE LAT/LON OF TARGET GRID POINT
          GLATD = HLAT(I,J)
          GLOND = H360 - HLON(I,J)
C DETERMINE NEAREST NEIGHBOR FROM INPUT GRID
          CALL LL2ETA(GLATD,GLOND,IMI,JMI,TPH0DI,TLM0DI,DPHDI,DLMDI,
     1      II,JI)
c          if(i.eq.203.and.j.eq.196) write(*,*)i,j,ii,ji
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
c	    endif
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
cer
                  if (i.eq.203 .and. j.eq.196) then
                    print *,krad,i,j,ii,ji,ix,jy,sm(ix,jy),
     1               sicei(ix,jy),smci(ix,jy,1),stci(ix,jy,1),
     2               sm(ix,jy),smc(ix,jy,1),stc(ix,jy,1)
                  endif
cer
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
C cold start:  determine liquid soil water content (SH2O)
C SH2O <= SMC for T < 273.149K (-0.001C)
            IF (STC(I,J,K) .LT. 273.149) THEN
C ----------------------------------------------------------------------
C first guess following explicit solution for Flerchinger Eqn from Koren
C et al, JGR, 1999, Eqn 17 (KCOUNT=0 in FUNCTION FRH2O).
              BX = BETA(ISLTPKI(I,J))
              IF ( BETA(ISLTPKI(I,J)) .GT. BLIM ) BX = BLIM
              FK = (((HLICE/(GRAV*(-PSIS(ISLTPKI(I,J)))))*
     1             ((STC(I,J,K)-T0)/STC(I,J,K)))**
     1             (-1/BX))*SMCMAX(ISLTPKI(I,J))
              IF (FK .LT. 0.02) FK = 0.02
              SH2O(I,J,K) = MIN ( FK, SMC(I,J,K) )
C ----------------------------------------------------------------------
C now use iterative solution for liquid soil water content using
C FUNCTION FRH2O (from the Eta "NOAH" land-surface model) with the
C initial guess for SH2O from above explicit first guess.
              SH2O(I,J,K)=FRH2O(STC(I,J,K),SMC(I,J,K),SH2O(I,J,K),
     .                    SMCMAX(ISLTPKI(I,J)),BETA(ISLTPKI(I,J)),
     .                    PSIS(ISLTPKI(I,J)))
            ELSE
C ----------------------------------------------------------------------
C SH2O = SMC for T => 273.149K (-0.001C)
              SH2O(I,J,K)=SMC(I,J,K)
c
            ENDIF
  70      CONTINUE
          if(mod(i,10).eq.0 .and .mod(j,10).eq.0) then
           write(96,12346)i,j,sh2o(i,j,1),sh2o(i,j,2),sh2o(i,j,3),
     &         sh2o(i,j,4)
           write(96,12346)i,j,smc(i,j,1),smc(i,j,2),smc(i,j,3),
     &         smc(i,j,4)
           write(96,12346)i,j,stc(i,j,1),stc(i,j,2),stc(i,j,3),
     &         stc(i,j,4)
           write(96,12348)i,j,ISLTPKI(I,J),smci(i,j,1),stci(i,j,1),
     &         sm(i,j),smi(i,j)
12346      format(1x,2i5,4(1x,e12.5))
12347      format(1x,3i5,2(1x,e12.5),/)
12348      format(1x,3i5,4(1x,e12.5),/)
          endif
          if(i.eq.203 .and. j.eq.196) then
           write(96,12346)i,j,sh2o(i,j,1),sh2o(i,j,2),sh2o(i,j,3),
     &         sh2o(i,j,4)
           write(96,12346)i,j,smc(i,j,1),smc(i,j,2),smc(i,j,3),
     &         smc(i,j,4)
           write(96,12346)i,j,stc(i,j,1),stc(i,j,2),stc(i,j,3),
     &         stc(i,j,4)
           write(96,12348)i,j,ISLTPKI(I,J),smci(i,j,1),stci(i,j,1),
     &         sm(i,j),smi(i,j)
          endif
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
