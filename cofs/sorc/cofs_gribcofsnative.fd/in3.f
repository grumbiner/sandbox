C
      SUBROUTINE IN3 (LUN,IFT,
     &                ELMAX,ELMIN,IDGRD,IM,JM,KB,NAVAC,N_HRS,
     &                KEND)
C     ===========================================
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    IN3
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 00-11-01
C
C ABSTRACT:
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL (LUN,IFT, ELMAX,ELMIN,IDGRD,IM,JM,KB,
C                     NAVAC,N_HRS,KEND)
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
      LOGICAL KEND                                   
      REAL ELMAX(IM,JM),ELMIN(IM,JM)
      CALL GETLBL(LUN,IHHREF,IDDREF,IMMREF,IYYREF,IFT,IDGRD,IMI,JMI,
     &            KB,KEND)
      IF (KEND) RETURN                     
      READ (LUN,END=500) NAVAC,N_HRS
      READ (LUN,END=500) ELMAX,ELMIN
      WRITE (*,*) 'IN3 FINISHED'
      KEND=.FALSE.
      RETURN      
  500 KEND=.TRUE.
      RETURN    
      END
