      SUBROUTINE READICE
C
C     THIS SUBROUTINE READS IN THE AIR FORCE SNOWDEPTH ON THE 32-KM ETA
C     GRID AND DISTRIBUTES TO ALL THE NODES TO COMPARE WITH THE EDAS
C     SNOW TO BE READ IN VIA READ_RESTRT2.
C     
C     IT WILL ALSO READ IN THE SEA-ICE, THE SSTs, THE ALBEDO, AND
C     THE GREENNESS FRACTION.
C
C     PERRY SHAFRAN - 27 NOVEMBER 2002
C
       INCLUDE "parmeta"
       INCLUDE "parm.tbl"
       INCLUDE "parmsoil"
       INCLUDE "mpp.h"

                           P A R A M E T E R
     & (LP1=LM+1,JAM=6+2*(JM-10))

      INCLUDE "PVRBLS.comm"
      INCLUDE "CTLBLK.comm"
      INCLUDE "PHYS.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "MASKS.comm"

      INTEGER*2 IYR
      LOGICAL*1 BIT(IM,JM)
      INTEGER JPDS(25), JGDS(22), KGDS(22), KPDS(25)

      IF (MYPE.EQ.0) THEN
      WRITE(*,*) " ********************************* "
      WRITE(*,*) " READ NEW SICE "
      WRITE(*,*) " READ NEW SICE "
      WRITE(*,*) " ********************************* "
      END IF
C
C
C READ IN THE SEA-ICE.
C
      IF(MYPE.EQ.0) THEN
        call baopenr(43,'fort.43',iret)
        write(0,*) 'baopenr on unit43', ' iret=', iret
        JPDS=-1
        IF(IDAT(3).GE.2000) THEN
          IYR=2000-IDAT(3)
        ELSE
          IYR=IDAT(3)-1900
        ENDIF
        JPDS(8)=IYR
        JPDS(9)=IDAT(1)
        JPDS(10)=IDAT(2)
        CALL GETGB(43,0,IM*JM,0,JPDS,JGDS,KF,K,KPDS,KGDS,BIT,TEMP1,IRET)
        WRITE(0,11) iret,kf,kpds(5),
     &         (kpds(21)*100+kpds(8))/100-1, mod(kpds(8),100),kpds(9),
     &          kpds(10)
11      FORMAT('iret=',i3,' kf=', i6,' fld=', i3,
     &     2x,'SEA ICE',1x, 4i2.2)
        IF (IRET.NE.0) THEN
           WRITE(0,*)"JPDS(8),JPDS(9),JPDS(10)",JPDS(8),JPDS(9),JPDS(10)
           CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
           STOP
        END IF
       ENDIF
C
C DISTRIBUTE SEA ICE TO ALL THE NODES
C
      CALL DSTRB(TEMP1,SICE,1,1,1)
C

      DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
      DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
         IF (SICE(I,J).GT.0.5) SM(I,J)=0.0
      END DO
      END DO

      DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
      DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
         IF (SM(I,J).GT.0.5) ALBEDO(I,J)=0.06
      END DO
      END DO

      RETURN
      END
