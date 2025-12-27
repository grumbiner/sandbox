      SUBROUTINE VWR
C--------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C--------------------------------------------------------
      INCLUDE "VRBLS.comm"
C--------------------------------------------------------
      INTEGER JSTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      REAL VWRITE(IM,JM)
C--------------------------------------------------------
      IOUT=80
      DO 500 L=1,LM
C
      DO J=JS_LOC_TABLE(MYPE),JE_LOC_TABLE(MYPE)
      DO I=IS_LOC_TABLE(MYPE),IE_LOC_TABLE(MYPE)
        VWRITE(I+MY_IS_GLB-1,J+MY_JS_GLB-1)=V(I,J,L)
      ENDDO
      ENDDO
C
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C
      IF(MYPE.EQ.0)THEN
        DO IPE=1,NPES-1
          DO J=JS_GLB_TABLE(IPE),JE_GLB_TABLE(IPE)
            CALL MPI_RECV(VWRITE(IS_GLB_TABLE(IPE),J),
     1                    ICHUNKTAB(IPE),MPI_REAL,IPE,99,
     2                    MPI_COMM_COMP,ISTAT,IRECV)
          ENDDO
        ENDDO
C
      ELSE
        DO J=MY_JS_GLB,MY_JE_GLB
          CALL MPI_SEND(VWRITE(MY_IS_GLB,J),ICHUNKTAB(MYPE),
     1                  MPI_REAL,0,99,
     2                  MPI_COMM_COMP,ISEND)
        ENDDO
      ENDIF
C
      IF(MYPE.EQ.0)THEN
        DO J=1,JM
          IENDX=IM
          IF(MOD(J,2).EQ.1)IENDX=IM-1
          WRITE(IOUT)(VWRITE(I,J),I=1,IENDX)
        ENDDO
      ENDIF
  500 CONTINUE
C
      STOP555
C
      RETURN
      END
