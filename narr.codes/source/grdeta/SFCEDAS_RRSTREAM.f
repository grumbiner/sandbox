      SUBROUTINE SFCEDAS_RRSTREAM
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
                             C O M M O N  /SFCTYPES/
     & IVGTPK(IM,JM),ISLTPK(IM,JM),ISPTPK(IM,JM)
C-----------------------------------------------------------------------
      COMMON /TGRND/ TG(IM,JM)
C-----------------------------------------------------------------------
                             D I M E N S I O N
     & IDATE(4),IDATI(3)
C 
      INTEGER,ALLOCATABLE,DIMENSION(:,:)::IVGTPKI,ISLTPKI,ISPTPKI
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::DUM1
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:)::SMI,SICEI
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::SMC1,STC1
      REAL(REAL_32),ALLOCATABLE,DIMENSION(:,:,:)::SMC2,STC2
C-----------------------------------------------------------------------
C COMMENTED BLOCK BELOW IS FORMER NON-DYNAMIC MEMORY ALLOCATION
C                             D I M E N S I O N
C     & IDATE(4),IDATI(3)
C     &,SMCI(IMI,JMI,NSOIL),STCI(IMI,JMI,NSOIL),SH2OI(IMI,JMI,NSOIL)
C     &,CMCI(IMI,JMI)
C     &,SMI(IMI,JMI),SICEI(IMI,JMI)
C     &,IVGTPKI(IMI,JMI),ISLTPKI(IMI,JMI),ISPTPKI(IMI,JMI)
C-----------------------------------------------------------------------
      LOGICAL RUN_D
      INTEGER IDAT_D(3)
      INTEGER IHRST_D, NTSD_D
      CHARACTER*32 LABEL_D
C-----------------------------------------------------------------------
                             D A T A
c    & NHIBUI/34/, NRESTRTI/36/, LDATE/52/
     & NHIBUI/48/, NRESTRTI/49/, LDATE/52/, NRESTRT2/47/, NRESTRTP/46/
C-----------------------------------------------------------------------
C
C ALLOCATE DYNAMIC MEMORY
C
      ALLOCATE(DUM1(IM,JM))

      ALLOCATE(SMI(IM,JM))
      ALLOCATE(SICEI(IM,JM))

      ALLOCATE(IVGTPKI(IM,JM))
      ALLOCATE(ISLTPKI(IM,JM))
      ALLOCATE(ISPTPKI(IM,JM))

      ALLOCATE(SMC1(IM,JM,NSOIL))
      ALLOCATE(STC1(IM,JM,NSOIL))
      ALLOCATE(SMC2(IM,JM,NSOIL))
      ALLOCATE(STC2(IM,JM,NSOIL))
C
C INPUT TG from parallel 32km run from IBM-snow
C
      REWIND NRESTRTP
C
C SKIPPING:  2+LM+3+LM*9 RECORDS  !!!!!!!!! LM=60 in 32km parallel
C
      NSKIP=2+60+3+60*9
      DO ISKIP=1,NSKIP
        READ(NRESTRTP) DUM
      ENDDO

      READ(NRESTRTP) RUN_D,IDAT_D,IHRST_D,NTSD_D,LABEL_D,
     &        DUM1,DUM1,TG,DUM1,DUM1,DUM1

C
C INPUT EDAS LAND SURFACE STATES FROM NRESTRTI FILE
C
      REWIND NRESTRTI
C
C SKIPPING:  2+LM+3+LM*9+14 RECORDS
C
      NSKIP=19+10*LM
      DO ISKIP=1,NSKIP
        READ(NRESTRTI) DUM
      ENDDO
      print*,'after 1st DUM reads'
C
C READ TOTAL SOIL MOISTURE (SMCI),UNFROZEN SOIL MOISTURE (SMH2OI),
C CANOPY WATER (CMCI), SOIL TEMP (STCI)
C
      READ(NRESTRTI) SMC1
      print*,'after SMC'
      READ(NRESTRTI) DUM
      print*,'after CMC1,DUM'
      READ(NRESTRTI) STC1
      print*,'after STC'

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
      READ(NHIBUI) IVGTPKI
      READ(NHIBUI) ISLTPKI
      READ(NHIBUI) ISPTPKI

C
C ENTER LOOP TO CHECK SOIL TYPE, VEG TYPE AND SLOPE TYPE
C
      DO  200 J = 1, JM
        DO 100 I = 1, IM
C IF NON LAND-MASS POINT, SKIP TO END OF LOOP (100)
          write(99,"(2I4,4F4.1,6I3)")
     &               I,J,SM(I,J),SMI(I,J),SICE(I,J),SICEI(I,J),
     &               IVGTPK(I,J),IVGTPKI(I,J),
     &               ISLTPK(I,J),ISLTPKI(I,J),
     &               ISPTPK(I,J),ISPTPKI(I,J)
          IF ((SM(I,J) .GT. 0.5) .OR. (SICE(I,J) .GT. 0.5)) GOTO 100

          IF ( IVGTPK(I,J) .NE. IVGTPKI(I,J) ) THEN
             WRITE(0,*) " IVGTPK(I,J) .NE. IVGTPKI(I,J) "
             WRITE(0,*) I,J,IVGTPK(I,J),IVGTPKI(I,J)
             STOP 200
             WRITE(99,*) " IVGTPK(I,J) .NE. IVGTPKI(I,J) "
          END IF

          IF ( ISLTPK(I,J) .NE. ISLTPKI(I,J) ) THEN
             WRITE(0,*) " ISLTPK(I,J) .NE. ISLTPKI(I,J) "
             WRITE(0,*) I,J,ISLTPK(I,J),ISLTPKI(I,J)
             STOP 210
             WRITE(99,*) " ISLTPK(I,J) .NE. ISLTPKI(I,J) "
          END IF

          IF ( ISPTPK(I,J) .NE. ISPTPKI(I,J) ) THEN
             WRITE(0,*) " ISPTPK(I,J) .NE. ISPTPKI(I,J) "
             WRITE(0,*) I,J,ISPTPK(I,J),ISPTPKI(I,J)
             STOP 220
             WRITE(99,*) " ISPTPK(I,J) .NE. ISPTPKI(I,J) "
          END IF

 100    CONTINUE
 200  CONTINUE
C
C
C-----------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------
C
C INPUT EDAS LAND SURFACE STATES FROM NRESTRT2 FILE !!! second restart file
C
C SKIPPING:  2+LM+3+LM*9+14 RECORDS
C
      REWIND NRESTRT2

      NSKIP=19+10*LM
      DO ISKIP=1,NSKIP
        READ(NRESTRT2) DUM
      ENDDO
      print*,'after 1st DUM reads'
C
C READ TOTAL SOIL MOISTURE (SMCI),UNFROZEN SOIL MOISTURE (SMH2OI),
C CANOPY WATER (CMCI), SOIL TEMP (STCI)
C
      READ(NRESTRT2) SMC2
      print*,'after SMC2'
      READ(NRESTRT2) DUM
      print*,'after CMC2,DUM'
      READ(NRESTRT2) STC2
      print*,'after STC2'

C
C ENTER PRIMARY TASK LOOP TO AVERAGE TWO RESTART FILES (Oct 87 and Oct 88)
C
      DO  400 J = 1, JM
        DO 300 I = 1, IM
C IF NON LAND-MASS POINT, SKIP TO END OF LOOP (300)
          IF ((SM(I,J) .GT. 0.5) .OR. (SICE(I,J) .GT. 0.5)) GOTO 300

      print *,"RRSTREAMS",I,J,SM(I,J),SICE(I,J),STC1(I,J,1),STC2(I,J,1)
          DO 71 K=1,NSOIL
            SMC(I,J,K) = 0.5*(SMC1(I,J,K)+SMC2(I,J,K))
            STC(I,J,K) = 0.5*(STC1(I,J,K)+STC2(I,J,K))
  71      CONTINUE

 300    CONTINUE
 400  CONTINUE

C-----------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------

C
C DEALLOCATE DYNAMIC MEMORY
C
      DEALLOCATE(IVGTPKI)
      DEALLOCATE(ISLTPKI)
      DEALLOCATE(ISPTPKI)
      DEALLOCATE(SMC1)
      DEALLOCATE(STC1)
      DEALLOCATE(SMC2)
      DEALLOCATE(STC2)

      RETURN
      END
