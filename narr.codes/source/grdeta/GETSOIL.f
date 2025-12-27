      SUBROUTINE GETSOIL
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
                             D I M E N S I O N
     & IDATE(4),IDATI(3)
C-----------------------------------------------------------------------
                             D A T A
     & NRESTRTI/36/, LDATE/52/
C-----------------------------------------------------------------------

C READ DATE AND HOUR
      REWIND LDATE
      READ(LDATE) IDATE
      IHRST=IDATE(1)
      READ(NRESTRTI) DUM,IDATI,IHRSTI
C CHECK THAT VAILD DATE/TIME OF INPUT RESTART FILE MATCHES EXPECTED DATE/TIME
      IF ( (IHRST .NE. IHRSTI) .OR. (IDAT(1) .NE. IDATI(1)) .OR.
     1     (IDAT(2) .NE. IDATI(2)) .OR. (IDAT(3) .NE. IDATI(3)) ) THEN
        PRINT *,'MIS-MATCH WITH INPUT EDAS DATE/TIME - STOP EXECUTION'
        PRINT *,IHRST,IHRSTI,IDAT(1),IDATI(1),IDAT(2),IDATI(2),
     1     IDAT(3),IDATI(3)
c       STOP 666
      ENDIF
C
C INPUT EDAS LAND SURFACE STATES FROM NRESTRTI FILE
C
C SKIPPING:  2+LM+3+LM*9+14 RECORDS
C
      NSKIP=19+10*LM
      DO ISKIP=1,NSKIP
        READ(NRESTRTI) DUM
      ENDDO
C
C READ TOTAL SOIL MOISTURE (SMC),UNFROZEN SOIL MOISTURE (SMH2O),
C CANOPY WATER (CMC), SOIL TEMP (STC)
C
      READ(NRESTRTI) SMC
      READ(NRESTRTI) CMC
      READ(NRESTRTI) STC
      READ(NRESTRTI) SH2O
C
      RETURN
      END
