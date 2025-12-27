      SUBROUTINE FIXED(IMOUT,JMOUT)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    FIXED       POSTS FIXED FIELDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-08-30
C     
C ABSTRACT:  THIS ROUTINE POSTS FIXED (IE, TIME INDEPENDENT)
C  ETA MODEL FIELDS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-08-30  RUSS TREADON
C   96-04-05  MIKE BALDWIN - CHANGED ALBEDO CALC
C   98-06-16  T BLACK      - CONVERSION FROM 1-D TO 2-D
C   98-07-17  MIKE BALDWIN - REMOVED LABL84
C   00-01-05  JIM TUCCILLO - MPI VERSION
C   03-01-27  CHUANG AND EK- ADD SNOW FREE AND MAX SNOW ALBEDO
C     
C USAGE:    CALL FIXED(IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
C
C   OUTPUT ARGUMENT LIST: 
C     NONE 
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - LOOPS
C                  MASKS
C                  LLGRDS
C                  RQSTFLD
C                  PHYS
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     INCLUDE/DECLARE PARAMETERS.
C     
      INCLUDE "parmeta"
      INCLUDE "parmout"
      INCLUDE "parm.tbl"
      INCLUDE "params"
      INCLUDE "parmsoil"
C
cek      PARAMETER (SNOALB=0.55)
C     
C     DECLARE VARIABLES
      REAL EGRID1(IM,JM),EGRID2(IM,JM)
      REAL GRID1(IMOUT,JMOUT),GRID2(IMOUT,JMOUT)
C
C     INCLUDE COMMON BLOCKS.
C     
      INCLUDE "LOOPS.comm"
      INCLUDE "LLGRDS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "RQSTFLD.comm"
      INCLUDE "PHYS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "CTLBLK.comm"
C     
C     
C********************************************************************
C
C     START FIXED HERE.
C
C     LATITUDE (OUTPUT GRID).
      IF (IGET(048).GT.0) THEN
         DO J = 1,JMOUT
            DO I = 1,IMOUT
               GRID1(I,J) = GDLAT(I,J)
            END DO
         END DO
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(048),LVLS(1,IGET(048)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     LONGITUDE (OUTPUT GRID). CONVERT TO EAST
      IF (IGET(049).GT.0) THEN
         DO J = 1,JMOUT
            DO I = 1,IMOUT
               GRID1(I,J) = 360. - GDLON(I,J)
            END DO
         END DO
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(049),LVLS(1,IGET(049)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     LAND/SEA MASK.
      IF (IGET(050).GT.0) THEN
         DO J = JSTA,JEND
         DO I = 1,IM
            EGRID1(I,J) = 1. - SM(I,J)
         ENDDO
         ENDDO
         CALL E2OUT(050,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(050),LVLS(1,IGET(050)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     SEA ICE MASK.
      IF (IGET(051).GT.0) THEN
         CALL E2OUT(051,000,SICE,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(051),LVLS(1,IGET(051)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     MASS POINT ETA SURFACE MASK.
      IF (IGET(052).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J) = FLOAT(LMH(I,J))
         ENDDO
         ENDDO
         CALL E2OUT(052,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(052),LVLS(1,IGET(052)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C     
C     VELOCITY POINT ETA SURFACE MASK.
      IF (IGET(053).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J) = FLOAT(LMV(I,J))
         ENDDO
         ENDDO
         CALL E2OUT(053,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(053),LVLS(1,IGET(053)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     SURFACE ALBEDO.
C
      IF (IGET(150).GT.0) THEN
         DO J=JSTA,JEND
         DO I=1,IM
           EGRID1(I,J)=ALBEDO(I,J)
         ENDDO
         ENDDO
         CALL E2OUT(150,000,EGRID1,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(150),LVLS(1,IGET(150)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
! ADD SNOW FREE ALBEDO
      IF (IGET(226).GT.0) THEN
         CALL E2OUT(226,000,ALBASE,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         ID(02)=130
         CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(226),LVLS(1,IGET(226)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
! ADD MAX SNOW ALBEDO
      IF (IGET(227).GT.0) THEN
       DO J=JSTA,JEND
       DO I=1,IM
c sea point, albedo=0.06 same as snow free albedo
        IF( (abs(SM(I,J)-1.) .lt. 1.0E-5) ) THEN
         RMXSNAL(I,J)=0.06
c sea-ice point, albedo=0.60, same as snow free albedo
        ELSEIF( (abs(SM(I,J)-0.)   .lt. 1.0E-5) .AND.
     &          (abs(SICE(I,J)-1.) .lt. 1.0E-5) ) THEN
         RMXSNAL(I,J)=0.60
        ENDIF
       ENDDO
       ENDDO
c
         CALL E2OUT(227,000,RMXSNAL,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         ID(02)=130
         CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
         CALL OUTPUT(IOUTYP,IGET(227),LVLS(1,IGET(227)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
!
C      IF (IGET(227).GT.0) THEN
!         icount=0
!         do j=jsta,jend
!          do i=1,im
!           print*,'SAMPLE MXSNAL',i,j,
!     +   RMXSNAL(i,j)
c           icount=icount+1
c           if(icount.gt.200)go to 51
!          end do
!         end do
c 51      continue
C         CALL E2OUT(227,000,RMXSNAL,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
C         ID(1:25) = 0
C         ID(02)=130
C         CALL SCLFLD(GRID1,100.,IMOUT,JMOUT)
C         CALL OUTPUT(IOUTYP,IGET(227),LVLS(1,IGET(227)),
C     X        GRID1,IMOUT,JMOUT)
C      ENDIF
C     
C     SEA SURFACE TEMPERAURE.
      IF (IGET(151).GT.0) THEN
         CALL E2OUT(151,000,SST,EGRID2,GRID1,GRID2,IMOUT,JMOUT)
         ID(1:25) = 0
         CALL OUTPUT(IOUTYP,IGET(151),LVLS(1,IGET(151)),
     X        GRID1,IMOUT,JMOUT)
      ENDIF
C
C     END OF ROUTINE.
C     
      RETURN
      END

