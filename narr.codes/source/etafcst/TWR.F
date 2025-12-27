      SUBROUTINE TWR
C--------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C--------------------------------------------------------
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
C--------------------------------------------------------
      INTEGER JSTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      REAL TWRITE(IM,JM)
      REAL ARRAY_LOC(IDIM1:IDIM2,JDIM1:JDIM2)
C--------------------------------------------------------
      IOUT=80
      REWIND IOUT
C
      DO 500 L=1,LM
C
      IF(MYPE.EQ.0)THEN
        DO J=MY_JS_LOC,MY_JE_LOC
        DO I=MY_IS_LOC,MY_IE_LOC
          TWRITE(I+MY_IS_GLB-1,J+MY_JS_GLB-1)=T(I,J,L)
c         TWRITE(I+MY_IS_GLB-1,J+MY_JS_GLB-1)=Q(I,J,L)
c         TWRITE(I+MY_IS_GLB-1,J+MY_JS_GLB-1)=Q2(I,J,L)
        ENDDO
        ENDDO
C
        DO IPE=1,NPES-1
          MAXVALS=(IDIM2-IDIM1+1)*(JDIM2-JDIM1+1)
C
          CALL MPI_RECV(ARRAY_LOC,MAXVALS,
     1                  MPI_REAL,IPE,IPE,
     2                  MPI_COMM_COMP,JSTAT,IRECV)
C
          JOFFSET=0
          DO JGLB=JS_GLB_TABLE(IPE),JE_GLB_TABLE(IPE)
            JLOC=JS_LOC_TABLE(IPE)+JOFFSET
            IOFFSET=0
            DO IGLB=IS_GLB_TABLE(IPE),IE_GLB_TABLE(IPE)
              TWRITE(IGLB,JGLB)=ARRAY_LOC(IS_LOC_TABLE(IPE)
     1                                    +IOFFSET,JLOC)
              IOFFSET=IOFFSET+1
            ENDDO
            JOFFSET=JOFFSET+1
          ENDDO
        ENDDO
C
      ELSE
        NUMVALS=(IDIM2-IDIM1+1)*(JDIM2-JDIM1+1)
C
c       CALL MPI_SEND(Q(IDIM1,JDIM1,L),NUMVALS,
c       CALL MPI_SEND(Q2(IDIM1,JDIM1,L),NUMVALS,
        CALL MPI_SEND(T(IDIM1,JDIM1,L),NUMVALS,
     1                MPI_REAL,0,MYPE,
     2                MPI_COMM_COMP,ISEND)
      ENDIF
C
      CALL MPI_BARRIER(MPI_COMM_COMP,IRTN)
C
      IF(MYPE.EQ.0)THEN
        DO J=1,JM
          IENDX=IM
          IF(MOD(J,2).EQ.0)IENDX=IM-1
          WRITE(IOUT)(TWRITE(I,J),I=1,IENDX)
        ENDDO
      ENDIF
  500 CONTINUE
C
c     CALL MPI_FINALIZE(IERR)
c     STOP555
C
      RETURN
      END
