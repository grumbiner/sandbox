C***********************************************************----------!!
      SUBROUTINE timav(uc, vc, ut, vt, ss, sd, h, scrit, tav, tstep,
     1                     delx, dely)
C     Program to compute averaged quantities from model output
C     Version modified to work 'on the fly'.  Original
C       assumed that the files were already written and merely
C       needed averaging.  Now work with data as it is being generated.
C       BG 9-29-89
      IMPLICIT none
      INCLUDE "grid.inc"

      REAL uc(nx, ny), vc(nx, ny), ss(nx, ny), sd(nx, ny)
      REAL ut(nx, ny), vt(nx, ny), h(nx, ny)
      REAL scrit, delx, dely

      INTEGER nscrit
      PARAMETER (nscrit = 1)
      REAL delscrit
      PARAMETER (delscrit = 0.005)
      REAL ftav
      INTEGER i, j, k, l
      INTEGER tstep, tav
      REAL r1(nx, ny), r2(nx, ny), r3(nx, ny), r4(nx, ny)
      REAL r5(nx, ny), r6(nx, ny)
      REAL f1(ny,nscrit), f2(ny,nscrit)
      REAL f1t(ny,nscrit), f2t(ny,nscrit)
      SAVE r1, r2, r3, r4, r5, r6, f1, f2, f1t, f2t

C***********************************************************----------!!

C     Number of steps to average over is tav.
      ftav = FLOAT(tav)
      IF (tstep .EQ. 1) THEN
        DO 1040 k = 1, ny
          DO 1050 l = 1, nx
            r1(l,k) = 0.0
            r2(l,k) = 0.0
            r3(l,k) = 0.0
            r4(l,k) = 0.0
            r5(l,k) = 0.0
            r6(l,k) = 0.0
 1050     CONTINUE
 1040   CONTINUE
         DO 1060 k = 1, ny
          DO 1060 l = 1, nscrit
            f1(k,l) = 0.0
            f2(k,l) = 0.0
 1070     CONTINUE
 1060   CONTINUE

       ELSE

        DO 1900 i = 1, nscrit
          CALL reflux(vt, vc, ss, sd, h, f1t(1,i), f2t(1,i),
     1                scrit+(i-1)*delscrit, nx, ny, delx, dely)
          DO 1910 k = 1, ny
            f1(k,i) = f1(k,i)+f1t(k,i)
            f2(k,i) = f2(k,i)+f2t(k,i)
 1910     CONTINUE
 1900   CONTINUE

        DO 2000 k = 1, ny
          DO 2010 l = 1, nx
            r1(l,k) = r1(l,k) + uc(l,k)
            r2(l,k) = r2(l,k) + vc(l,k)
            r3(l,k) = r3(l,k) + ut(l,k)
            r4(l,k) = r4(l,k) + vt(l,k)
            r5(l,k) = r5(l,k) + ss(l,k)
            r6(l,k) = r6(l,k) + sd(l,k)
 2010     CONTINUE
 2000   CONTINUE

        IF ( MOD(tstep,tav) .EQ. 0) THEN
          DO 2020 k = 1, ny
            DO 2030 l = 1, nx
              r1(l,k) = r1(l,k) / ftav
              r2(l,k) = r2(l,k) / ftav
              r3(l,k) = r3(l,k) / ftav
              r4(l,k) = r4(l,k) / ftav
              r5(l,k) = r5(l,k) / ftav
              r6(l,k) = r6(l,k) / ftav
 2030       CONTINUE
            DO 2040 i = 1, nscrit
              f1(k,i) = f1(k,i) / ftav / 1.E5
              f2(k,i) = f2(k,i) / ftav / 1.E5
 2040       CONTINUE
 2020     CONTINUE
          WRITE (50) r1
          WRITE (51) r2
          WRITE (52) r3
          WRITE (53) r4
          WRITE (54) r5
          WRITE (55) r6
          DO 2100 j = 1, nscrit
           WRITE (56,9009) (f1(i,j),i=1,ny)
           WRITE (57,9009) (f2(i,j),i=1,ny)
 2100          CONTINUE
           WRITE (56,9010)
           WRITE (57,9010)
          DO 2060 k = 1, ny
            DO 2050 l = 1, nx
              r1(l,k) = 0.0
              r2(l,k) = 0.0
              r3(l,k) = 0.0
              r4(l,k) = 0.0
              r5(l,k) = 0.0
              r6(l,k) = 0.0
 2050       CONTINUE
             DO 2070 i = 1, nscrit
              f1(k,i) = 0.0
              f2(k,i) = 0.0
 2070       CONTINUE
 2060     CONTINUE
        ENDIF

      ENDIF

 9001 FORMAT (BN, I5)

 9002 FORMAT (A60)

 9009 FORMAT (36F6.2)

 9010 FORMAT (' end of step')

      RETURN
      END
