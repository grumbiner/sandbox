      SUBROUTINE READRST(LRSTRT,RSTFIL
     1,                  PD,T,Q,U,V,Q2,CWM,USTAR
     2,                  IDAT,IHRST)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  READRST     GET PIECES OF RESTART FILE
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-05-21
C
C ABSTRACT:  THIS ROUTINE READS A RESTART FILE AND KEEPS 
C            THE PRIMARY PROGNOSTIC VARIABLES AND USTAR
C
C PROGRAM HISTORY LOG:
C   99-05-21  T BLACK - ORIGINATOR
C
C USAGE:  CALL NESTBC FROM PROGRAM POST0
C
C   INPUT ARGUMENT LIST:
C     LRSTRT - THE UNIT NUMBER OF THE RESTART FILE
C     RSTFIL - THE NAME OF RESTART FILE
C
C   OUTPUT ARGUMENT LIST:
C     PD     - THE SURFACE PRESSURE MINUS PT
C     T      - THE TEMPERATURE 
C     Q      - THE SPECIFIC HUMIDITY 
C     U      - THE U COMPONENT 
C     V      - THE V COMPONENT
C     Q2     - THE TURBULENT KINETIC ENERGY
C     CWM    - THE CLOUD WATER/ICE 
C     USTAR  - THE FRICTION VELOCITY 
C     IDAT   - THE DATE IN A 3-INTEGER ARRAY
C     IHRST  - THE STARTING HOUR OF THE FORECAST IN UTC
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C             NONE
C
C----------------------------------------------------------------------
      INCLUDE "parmeta"
C----------------------------------------------------------------------
                              R E A L
     & PD(IM,JM),USTAR(IM,JM)
     &,T(IM,JM,LM),Q(IM,JM,LM),U(IM,JM,LM),V(IM,JM,LM)
     &,Q2(IM,JM,LM),CWM(IM,JM,LM)
     &,DUM(IM,JM,4)
C----------------------------------------------------------------------
                              I N T E G E R
     & IDAT(3)
C----------------------------------------------------------------------
                              C H A R A C T E R
     & RSTFIL*50
C----------------------------------------------------------------------
C**********************************************************************
C----------------------------------------------------------------------
C***
C***  OPEN UNIT TO THE RESTART FILE AND READ WHAT IS NEEDED
C***
      print*,'rstfil=',rstfil
      CLOSE(LRSTRT)
      OPEN(UNIT=LRSTRT,FILE=RSTFIL,FORM='UNFORMATTED',IOSTAT=IER)
C***
C***  READ QUANTITIES NEEDED FROM THE RESTRT FILE
C***
      READ(LRSTRT)RUN,IDAT,IHRST,NTSD
      READ(LRSTRT)
C
      DO L=1,LM
        READ(LRSTRT)
      ENDDO
C
      READ(LRSTRT)
      READ(LRSTRT)PD
      READ(LRSTRT)
C
      DO L=1,LM
        READ(LRSTRT)((T(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT)((Q(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT)((U(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT)((V(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT)((Q2(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT)
        READ(LRSTRT)((CWM(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT)
        READ(LRSTRT)
      ENDDO
C
      READ(LRSTRT)
      READ(LRSTRT)
      READ(LRSTRT)DUM,USTAR
C
      CLOSE(LRSTRT)
C----------------------------------------------------------------------
      RETURN
      END
