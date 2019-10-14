C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  RFFTMLT    FFT.
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 92-04-16
C
C ABSTRACT: USED IN PLACE OF LIBRARY RFFTMLT FOR SPEED AND ODD VALUES.
C   COMPANION FFTFAX MUST BE USED WITH THIS ROUTINE!
C
C PROGRAM HISTORY LOG:
C   92-04-16  IREDELL
C
C USAGE:    CALL RFFTMLT(A,W,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
C
C   INPUT ARGUMENT LIST:
C     A        - INPUT (INC,JUMP,LOT) FIELD;
C                IF A IS GRID, ONLY (1,N,LOT) VALUES ARE USED,
C                IF A IS FOURIER, ONLY (1,N+2,LOT) VALUES ARE USED.
C     W        - WORK AREA OF LENGTH MIN(LOT,64)*N*2.
C     TRIGS    - OUTPUT FROM COMPANION FFTFAX.
C     IFAX     - OUTPUT FROM COMPANION FFTFAX.
C     INC      - INITIAL DIMENSION OF A.
C     JUMP     - SECOND DIMENSION OF A.
C     N        - NUMBER OF POINTS.
C     LOT      - NUMBER OF FFTS TO DO.
C     ISIGN    - ISIGN = -1 FOR GRID TO FOURIER SPACE,
C                ISIGN = 1 FOR FOURIER TO GRID SPACE.
C
C   OUTPUT ARGUMENT LIST:
C     A        - INPUT (INC,JUMP,LOT) FIELD;
C                IF A IS GRID, ONLY (1,N,LOT) VALUES ARE USED,
C                IF A IS FOURIER, ONLY (1,N+2,LOT) VALUES ARE USED.
C
C   REMARKS: FORTRAN 9X EXTENSIONS ARE USED.
C
C ATTRIBUTES:
C   CRAY YMP.
C
C$$$
      SUBROUTINE RFFTMLT(A,W,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
      PARAMETER(LDX=64)
      DIMENSION A(INC,JUMP,LOT),TRIGS(2*N),IFAX(20)
      DIMENSION W((LOT/LDX*LDX+LDX/LOT*LOT)/(LOT/LDX+LDX/LOT),N,2)
      IB=1
      IC=2
      LDM=MIN(LOT,LDX)
      NFAX=IFAX(1)
      IF(ISIGN.EQ.-1) THEN
        DO M=0,LOT-1,LDX
          LDO=MIN(LOT-M,LDX)
          LA=N
          IF(NFAX.EQ.1) THEN
            DO J=1,N
              DO L=1,LDO
                W(L,J,IB)=A(1,J,M+L)
              ENDDO
            ENDDO
          ELSE
            K=1+NFAX
            LA=LA/IFAX(1+NFAX)
            CALL QPASS(A(1,1,M+1),W(1,1,IB),TRIGS,
     &                 INC,LDM,JUMP,1,LDO,N,IFAX(1+NFAX),LA,IERR)
            DO K=NFAX,3,-1
              LA=LA/IFAX(K)
              CALL QPASS(W(1,1,IB),W(1,1,IC),TRIGS,
     &                   LDM,LDM,1,1,LDO,N,IFAX(K),LA,IERR)
              ID=IB
              IB=IC
              IC=ID
            ENDDO
          ENDIF
          LA=LA/IFAX(2)
          CALL QPASS(W(1,1,IB),A(1,1,M+1),TRIGS,
     &               LDM,INC,1,JUMP,LDO,N,IFAX(2),LA,IERR)
        ENDDO
      ELSE
        DO M=0,LOT-1,LDX
          LDO=MIN(LOT-M,LDX)
          LA=1
          CALL RPASS(A(1,1,M+1),W(1,1,IB),TRIGS,
     &               INC,LDM,JUMP,1,LDO,N,IFAX(2),LA,IERR)
          IF(NFAX.EQ.1) THEN
            DO J=1,N
              DO L=1,LDO
                A(1,J,M+L)=W(L,J,IB)
              ENDDO
            ENDDO
          ELSE
            LA=LA*IFAX(2)
            DO K=3,NFAX
              CALL RPASS(W(1,1,IB),W(1,1,IC),TRIGS,
     &                   LDM,LDM,1,1,LDO,N,IFAX(K),LA,IERR)
              ID=IB
              IB=IC
              IC=ID
              LA=LA*IFAX(K)
            ENDDO
            CALL RPASS(W(1,1,IB),A(1,1,M+1),TRIGS,
     &                 LDM,INC,1,JUMP,LDO,N,IFAX(NFAX+1),LA,IERR)
          ENDIF
          DO L=1,LDO
            A(1,N+1,M+L)=0.
            A(1,N+2,M+L)=0.
          ENDDO
        ENDDO
      ENDIF
      RETURN
      END
