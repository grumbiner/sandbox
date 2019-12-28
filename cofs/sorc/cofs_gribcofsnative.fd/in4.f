C
      SUBROUTINE IN4 (LUN,IFT,ELB,ULEV,VLEV,ZLEV,DUM1,DVM1,LEND,IM,JM)
C     ===========================================
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    IN4
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 00-11-01
C
C ABSTRACT:
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL IN4(LUN,IFT,ELB,ULEV,VLEV,ZLEV,DUM1,DVM1,LEND,IM,JM)
C
C   INPUT ARGUMENT LIST:
C     FT22F001 - yymmddhh.grd
C
C   OUTPUT ARGUMENT LIST:
C
C   SUBPROGRAMS CALLED
C     UNIQUE:
C      GETLBL
C
C ATTRIBUTES:
C   LANGUAGE: IBM 370 VS FORTRAN
C   MACHINE:  NAS, CRAY C-90, IBM SP
C
C$$$
C
      CHARACTER*64 HEADER               
      LOGICAL LEND                        
      DIMENSION  ELB(IM,JM),ULEV(IM,JM), VLEV(IM,JM)
      DIMENSION DUM1(IM,JM), DVM1(IM,JM)
      CALL GETLBL(LUN,IHHREF,IDDREF,IMMREF,IYYREF,IFT,IDGRD,IMI,JMI,
     &            KB,LEND)
      IF (LEND) RETURN
      READ (LUN,END=500) ZLEV,ELB,ULEV,VLEV,DUM1,DVM1
      
      OPEN (77,FILE='SMASK1.TXT')
      DO J=1,JM
        WRITE (77,'(1X,181I1)') (NINT(DUM1(I,J)),I=1,IM)
      ENDDO        

      DO J=1,JM
        WRITE (77,'(1X,181I1)') (NINT(DVM1(I,J)),I=1,IM)
      ENDDO        
      
      DO J=1,JM
        WRITE (77,'(1X,10(F5.2,1X))') (ELB(I,J),I=1,10)
      ENDDO        
      
      
      
      CLOSE (77)  
      
      RETURN
      LEND=.FALSE.
      RETURN      
  500 LEND=.TRUE.
      END
