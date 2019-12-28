C*************************************************----------++++++++++!!
      SUBROUTINE ssink(pchem, chemu, deltaz, deltat,
     1                     x, w, q, fna, faa, fiw, aamix,
     2                     pref, temper, wind, region, rsalt)

C     Compute the source/sink strengths.
C     Surface gas transfer by 8/4/91 BG.
C     Bottom water source/sink by 8/4/91 BG.
C     Biological source/sink (Phosphate only) 8/4/91 BG.

      IMPLICIT none
      
      INTEGER chemu, nlayer, region, nr
      PARAMETER (nlayer = 76)
      PARAMETER (nr     = 3 )

      INCLUDE "chmptr.inc"
      
      DOUBLE PRECISION deltaz, deltat, aamix
      DOUBLE PRECISION x(nlayer, chemu, region)
      DOUBLE PRECISION q(nlayer, chemu, region)
      DOUBLE PRECISION w(nlayer, region)
      DOUBLE PRECISION fna(nlayer, region), faa(nlayer, region)
      DOUBLE PRECISION fiw(nlayer, region)
      INTEGER pchem(chemu)
      DOUBLE PRECISION pref(nchem), temper(region), wind(region)
      REAL rsalt

      INTEGER i, k, m, n
      DOUBLE PRECISION temp1, dummy(nlayer)
      DOUBLE PRECISION produc, gasses, talk, cco2
      DOUBLE PRECISION sumbio, sumbw

      DOUBLE PRECISION lowpo4(nr)
      SAVE lowpo4

      DATA lowpo4(1) /0.000D-6/ !Tuned
      DATA lowpo4(2) /1.000D-6/ !Mongrel
      DATA lowpo4(3) /1.650D-6/ !Geosecs 100 meter average


C     Ensure that the forcing is zero prior to starting.
      DO 120 m = 1, region
        DO 110 k = 1, chemu
          DO 100 i = 1, nlayer
            q(i, k, m) = 0.D0
  100     CONTINUE
  110   CONTINUE
  120 CONTINUE

C     Compute gas transfer at the surface:
      DO 210 m = 1, region
        DO 200 k = 1, chemu
          temp1 = x(2, k, m)
          IF (pchem(k) .EQ. tco2) THEN
            talk = 0.0
            DO 220 n = 1, chemu
              IF (pchem(n) .EQ. alk) THEN
                talk = x(2,n,m)
               ENDIF
  220       CONTINUE
            temp1 = cco2(x(2,k,m), talk, temper(m), rsalt, 0.D0)
CD          PRINT *,'co2 stuff: x(2,k,m), temp1, talk, temper(m)',
CD     1                 x(2,k,m), temp1, talk, temper(m)
          ENDIF
          q(2, k, m) = gasses(pchem(k), temp1,
     1                   pref(pchem(k)), temper(m), rsalt, wind(m) )
CD          PRINT *,'gaseous source',q(2,k,m), pchem(k)
  200   CONTINUE
  210 CONTINUE

C     Compute the source strengths due to bottom water formation
      DO 320 m = 1, region
        DO 300 k = 1, chemu
          sumbw = 0.D0
          CALL bwsrc(fna, faa, fiw, aamix, w, dummy, x,
     1                               nlayer, deltaz, pchem(k), k,
     2                                        chemu, region, m)
          DO 310 i = 1, nlayer
            q(i,k,m) = q(i,k,m) + dummy(i)
CD            sumbw = sumbw + dummy(i)
CD            WRITE (*,9001) i, k, m, q(i,k,m)
  310     CONTINUE
CD          WRITE (*,9001) k, k, m, sumbw
  300   CONTINUE
  320 CONTINUE
 9001 FORMAT (3I3,E13.5)
 
C     Biological Utilization
C     Detritus Production
C     Oxidation
      DO 420 m = 1, region
        produc = DMAX1(0.0D0,x(2,1,m)-lowpo4(m))*deltaz/deltat/2.D0
        x(1,1, m) = DMIN1(lowpo4(m),x(1,1,m))
        x(2,1, m) = DMIN1(lowpo4(m),x(2,1,m))
        DO 400 k = 1, chemu
CD          sumbio = 0.D0
          CALL biosrc(pchem(k), x(1,k,m), dummy, nlayer,
     1                         deltaz, deltat, produc)
          DO 410 i = 1, nlayer
            q(i,k,m) = q(i,k,m) + dummy(i)
CD            sumbio = sumbio + dummy(i)
  410     CONTINUE
CD          WRITE (*,9001) k, k, m, sumbio
  400   CONTINUE
        q(1,1,m) = 0.0
        q(2,1,m) = 0.0
  420 CONTINUE
      
C     Carbonate Dissolution
C     Radio-decay

      RETURN
      END
