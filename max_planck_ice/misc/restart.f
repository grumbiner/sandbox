      PROGRAM restart
C     Control restart file.  Check for insane values in the 
C       restart file, assimilate concentration, thickness, and
C       SST as needed and available.
C     Bob Grumbine 11 October 1995

      IMPLICIT none
      INCLUDE "icegrid.inc"

      LOGICAL debug
      PARAMETER (debug = .FALSE.)

      REAL conc(0:L, 0:M)
C     Following is for Reynolds sst files.  Points lie on half-integer spaces.
      INTEGER nxsst, nysst
      PARAMETER (nxsst = 360)
      PARAMETER (nysst = 180)
      REAL sst(nxsst, nysst), err(nxsst, nysst)

C     For reference temperature and salinity information 
      REAL tshal(0:L, 0:M), tdeep(0:L, 0:M)
      REAL sshal(0:L, 0:M), sdeep(0:L, 0:M)

C     Fields for the restart file 
      REAL TICM(0:L, 0:M, NLEVEL), QH(0:L, 0:M)
      REAL A(0:L, 0:M), H(0:L, 0:M), HSN(0:L, 0:M)
      REAL U(L, M, 3), V(L, M, 3)
      REAL QS(0:L, 0:M), QT(0:L, 0:M), VM(L, M)
      REAL QTB(0:L, 0:M), QSB(0:L, 0:M), QHB(0:L, 0:M)
      REAL QDT(0:L, 0:M), QDS(0:L, 0:M), TICE(0:L, 0:M)

C     PARAMETERS to read in
      REAL TIO, QHO, QTO, QSO, QDTO, QDSO
      REAL bathy(0:L, 0:M)

C     Working variables
      INCLUDE "clib.inc"
      INCLUDE "locale.inc"
      INTEGER banio, jret
      INTEGER mode, start, newpos, size, no, nactual, fdes
      LOGICAL flags, flagc, flagrestart
      REAL cweight, sweight, hset
      INTEGER errunit
      INTEGER i, j, k

      errunit = 20

      READ (*,*) flagc
      READ (*,*) flags
      READ (*,*) flagrestart
      PRINT *,'Logical flags are ',flagc, flags, flagrestart

      READ (*,*) cweight
      READ (*,*) sweight
      PRINT *,'Concentration and SST weights are ',cweight, sweight
      READ (*,*) hset
      PRINT *,'Thickness to set ice to when new ',hset

      IF ( flagc ) THEN
        mode  = BAOPEN_RONLY + BAREAD + BACLOSE + NOSEEK
        start = 0
        no    = LP*MP
        size  = SIZEOF_REAL
        jret  = banio(mode, start, newpos, size, no, nactual, fdes, 
     1             "fort.10", conc)
        IF (nactual .NE. no) THEN
          PRINT *, "didn't read requested concs! ",nactual," of ",no
          PRINT *, "size, newpos, fdes, jret = ",size, newpos,fdes,jret
          STOP
        ENDIF
        IF (jret .NE. 0) THEN
          PRINT *,'jret from concs banio = ',jret
        ENDIF

      ENDIF
      IF (flags) THEN
        mode  = BAOPEN_RONLY + BAREAD + BACLOSE + NOSEEK
        start = 0
        no    = nxsst*nysst
        size  = SIZEOF_REAL
        jret  = banio(mode, start, newpos, size, no, nactual, fdes, 
     1             "fort.11", sst)
        IF (nactual .NE. no) THEN
          PRINT *, "didn't read requested sst! ",nactual," of ",no
          PRINT *, "size, newpos, fdes, jret = ",size, newpos,fdes,jret
          STOP
        ENDIF
        IF (jret .NE. 0) THEN
          PRINT *,'jret from sst banio = ',jret
        ENDIF
        jret  = banio(mode, start, newpos, size, no, nactual, fdes, 
     1             "fort.11", err)
        IF (nactual .NE. no) THEN
          PRINT *, "didn't read requested err! ",nactual," of ",no
          PRINT *, "size, newpos, fdes, jret = ",size, newpos,fdes,jret
          STOP
        ENDIF
        IF (jret .NE. 0) THEN
          PRINT *,'jret from err banio = ',jret
        ENDIF
      ENDIF

      READ (12) tshal
      READ (12) sshal
      READ (13) tdeep
      READ (13) sdeep

      IF (flagrestart) THEN
        READ (14) H
        READ (14) A
        READ (14) HSN
        READ (14) TICE
        READ (14) QT
        READ (14) QS
        READ (14) QH
        READ (14) QTB
        READ (14) QSB
        READ (14) QHB
        READ (14) QDT
        READ (14) QDS
        READ (14) TICM
        READ (14) U
        READ (14) V
      ELSE
        READ (16, *) TIO, QHO, QTO, QSO, QDTO, QDSO 
        READ (17) bathy
        DO 1000 j = 0, M
        DO 1100 i = 0, L 
          A(i,j)   = 0.0
          H(i,j)   = 0.0
          HSN(i,j) = 0.0
          QH(i,j)  = QHO
          QT(i,j)  = QTO
          QS(i,j)  = QSO
          QHB(i,j) = bathy(i,j)
          QDT(i,j) = MIN(QDTO, bathy(i,j) ) - 0.1
          QDS(i,j) = MIN(QDSO, bathy(i,j) ) - 0.1
          TICE(i,j) =   TIO
          DO 1200 k = 1, NLEVEL
            TICM(i,j,k) = TIO 
 1200     CONTINUE
 1100   CONTINUE
 1000 CONTINUE
      DO 1300 j = 1, M
      DO 1400 i = 1, L
      DO 1500 k = 1, 3
        u(i,j,k) = 0.0
        v(i,j,k) = 0.0
 1500 CONTINUE
 1400 CONTINUE
 1300 CONTINUE
      ENDIF


C     Now have all the data.  Start cleaning up.
      CALL filtsalt(sshal, sdeep, QS, QSB, LP, MP, errunit)

      IF (flags) THEN
        CALL filtsst(sst, err, nxsst, nysst, 
     1               tshal, tdeep, sshal, sdeep, 
     2               QT, QS, QTB, QSB, LP, MP, sweight, errunit)
      ENDIF

      IF (flagc .AND. flags) THEN
C        last arg is the maximum mixed layer temperature that will permit ice
        CALL filtconc(conc, LP, MP, sst, err, nxsst, nysst, tshal, 5.0)
      ENDIF

      IF (flagc) THEN
        CALL updatec(conc, A, H, QT, tshal, sshal, LP, MP, hset,
     1                 cweight, errunit)
      ENDIF

      CALL sanity(QT, QS, H, A, tshal, sshal, tdeep, sdeep, 
     1   QTB, QSB, QHB, QDT, QDS,
     2   LP, MP)

      WRITE (15) H
      WRITE (15) A
      WRITE (15) HSN
      WRITE (15) TICE
      WRITE (15) QT
      WRITE (15) QS
      WRITE (15) QH
      WRITE (15) QTB
      WRITE (15) QSB
      WRITE (15) QHB
      WRITE (15) QDT
      WRITE (15) QDS
      WRITE (15) TICM
      WRITE (15) U
      WRITE (15) V

      IF (debug) THEN
        PRINT *,'Ice thickness'
        WRITE (*,9001) ((H(i,j),i=0,L),j=M,0,-1)
        PRINT *,'Concentration'
        WRITE (*,9001) ((A(i,j),i=0,L),j=M,0,-1)
        PRINT *,'Snow thickness'
        WRITE (*,9001) ((HSN(i,j),i=0,L),j=M,0,-1)
        PRINT *,'Ice Temperature '
        WRITE (*,9001) ((TICE(i,j),i=0,L),j=M,0,-1)
        PRINT *,'OML Temperature'
        WRITE (*,9001) ((QT(i,j),i=0,L),j=M,0,-1)
        PRINT *,'OML Salinity'
        WRITE (*,9001) ((QS(i,j),i=0,L),j=M,0,-1)
        PRINT *,'OML Thickness'
        WRITE (*,9001) ((QH(i,j),i=0,L),j=M,0,-1)
      ENDIF
 9001 FORMAT (32F5.1) 

      STOP
      END
