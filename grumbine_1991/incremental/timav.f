C***********************************************************----------!!
      SUBROUTINE timav(uc, vc, ut, vt, ss, sd, h, scrit, tav, tstep,
     1                     nx, ny, delx, dely)
C     Program to compute averaged quantities from model output
C     Version modified to work 'on the fly'.  Original
C       assumed that the files were already written and merely
C       needed averaging.  Now work with data as it is being generated.
C       BG 9-29-89
      INTEGER nx, ny, nnx, nny
      PARAMETER (nnx = 36, nny = 36)

      REAL uc(nx, ny), vc(nx, ny), ss(nx, ny), sd(nx, ny)
      REAL ut(nx, ny), vt(nx, ny), h(nx, ny)
      REAL scrit, delx, dely

      REAL ftav
      INTEGER i, j, k, l
      INTEGER tstep, tav
      CHARACTER*60 fname
      REAL r1(nnx, nny), r2(nnx, nny), r3(nnx, nny), r4(nnx, nny)
      REAL r5(nnx, nny), r6(nnx, nny)
      REAL f1(nny), f2(nny)
      REAL f1t(nny), f2t(nny)
      SAVE r1, r2, r3, r4, r5, r6, f1, f2, f1t, f2t

C***********************************************************----------!!

C     Number of steps to average over is tav.
      IF (tstep .EQ. 1) THEN
        ftav = FLOAT(tav)
        DO 1040 k = 1, ny
          DO 1050 l = 1, nx
            r1(l,k) = 0.0
            r2(l,k) = 0.0
            r3(l,k) = 0.0
            r4(l,k) = 0.0
            r5(l,k) = 0.0
            r6(l,k) = 0.0
 1050     CONTINUE
          f1(k) = 0.0
          f2(k) = 0.0
 1040   CONTINUE

       ELSE

        CALL reflux(vt, vc, ss, sd, h, f1t, f2t,
     1              scrit, nx, ny, delx, dely)
        DO 2000 k = 1, ny
          DO 2010 l = 1, nx
            r1(l,k) = r1(l,k) + uc(l,k)
            r2(l,k) = r2(l,k) + vc(l,k)
            r3(l,k) = r3(l,k) + ut(l,k)
            r4(l,k) = r4(l,k) + vt(l,k)
            r5(l,k) = r5(l,k) + ss(l,k)
            r6(l,k) = r6(l,k) + sd(l,k)
 2010     CONTINUE
          f1(k) = f1(k) + f1t(k)
          f2(k) = f2(k) + f2t(k)
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
            f1(k) = f1(k) / ftav / 1.E5
            f2(k) = f2(k) / ftav / 1.E5
 2020     CONTINUE
          WRITE (50) r1
          WRITE (51) r2
          WRITE (52) r3
          WRITE (53) r4
          WRITE (54) r5
          WRITE (55) r6
          WRITE (56,9009) f1
          WRITE (57,9009) f2
          DO 2040 k = 1, ny
            DO 2050 l = 1, nx
              r1(l,k) = 0.0
              r2(l,k) = 0.0
              r3(l,k) = 0.0
              r4(l,k) = 0.0
              r5(l,k) = 0.0
              r6(l,k) = 0.0
 2050       CONTINUE
            f1(k) = 0.0
            f2(k) = 0.0
 2040     CONTINUE
        ENDIF

      ENDIF

 9001 FORMAT (BN, I5)

 9002 FORMAT (A60)

 9009 FORMAT (36F5.2)

      RETURN
      END
