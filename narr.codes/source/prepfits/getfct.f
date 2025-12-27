C-----------------------------------------------------------------------
C  GETFCT - INTERPOLATE BACKGROUND PROFILES to the irep report
C           VERTICALLY AND IN TIME
C-----------------------------------------------------------------------
      SUBROUTINE getfct(irep,ibak,fhour,onlysf)

      INCLUDE 'parm.inc'

      LOGICAL onlysf
      LOGICAL latlong, lambert, polarstereo
      COMMON  /counts/ kntgsf
      COMMON /gridef/ imax, jmax, kmax, alat1, elon1, dxx, dyy, elonv, 
     +            alatan, latlong, lambert, polarstereo

      COMMON /guesfc/ psi(mxr,mxb), zsi(mxr,mxb), tsi(mxr,mxb), 
     +            usi(mxr,mxb), vsi(mxr,mxb), qsi(mxr,mxb), 
     +            pmi(mxr,mxb), cpi(mxr,mxb), cni(mxr,mxb),
     +            pxi(mxr,mxb)
      COMMON /guess/ pi(mxl,mxr,mxb), zi(mxl,mxr,mxb), ti(mxl,mxr,mxb),
     +            ui(mxl,mxr,mxb), vi(mxl,mxr,mxb), qi(mxl,mxr,mxb), 
     +            ai(mxl,mxr,mxb)
      real*8 psi,zsi,tsi,usi,vsi,qsi,pmi,cpi,cni,pxi
      real*8 pi,zi,ti,ui,vi,qi,ai

      real*8 bak(10,255)
      REAL*8 hdr(10), cat(255), obs(10,255), qms(10,255)
      REAL*8 adate,vdata,vdate,bdate

      COMMON /observ/ hdr, cat, obs, qms, nlev
      COMMON /vrtfac/ maxprs, vrterp(1200)
      COMMON /vdates/ vdata, vdate(mxb), fhr(mxb), nofo(mxb)
      COMMON /backgv/ bak, nbak

      real*8 p(mxl), z(mxl), t(mxl), u(mxl), v(mxl), q(mxl), a(mxl)

      real*8 f100
      DATA bmiss /10E10/
      DATA tzero /273.15/
      DATA betap /.0552/
      DATA beta /.00650/
      DATA rog /29.261/
      DATA g /9.81/
      DATA r /287.05/
      data f100 /100.0/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  COMPUTE THE TIME INTERPOLATION WEIGHT FOR THIS REPORT
C  -----------------------------------------------------

      IF (ibak.lt.nbak.and.nofo(ibak).ne.1) THEN
        dvhr = mod(vdata,f100)
        fvhr = mod(vdate(ibak),f100)
        IF (fvhr.gt.dvhr) fvhr = dvhr - fvhr + 24.
        timewt = abs((dvhr-fvhr)/(fhr(ibak+1)-fhr(ibak)))
        fhour = fhr(ibak) + abs(dvhr-fvhr)
        in = 1
      ELSE
        timewt = 0.
        fhour = fhr(ibak)
        in = 0
      END IF

      bak = 0

C     TIME INTERPOLATE VALUES FROM ADJACENT FORECAST PROFILES
C     -------------------------------------------------------

      DO it = 0, in
        jbak = ibak + it
        timw = 1 - it - timewt

C       GET GUESS PROFILE AT OB LOCATION
C       --------------------------------

        ps = psi(irep,jbak)
        pm = pmi(irep,jbak)
        zs = zsi(irep,jbak)
        ts = tsi(irep,jbak)
        us = usi(irep,jbak)
        vs = vsi(irep,jbak)
        qs = qsi(irep,jbak)
        cp = cpi(irep,jbak)
        cn = cni(irep,jbak)
        px = pxi(irep,jbak)

        DO k = 1, kmax
          p(k) = pi(k,irep,jbak)
          z(k) = zi(k,irep,jbak)
          t(k) = ti(k,irep,jbak)
          u(k) = ui(k,irep,jbak)
          v(k) = vi(k,irep,jbak)
          q(k) = qi(k,irep,jbak)
c         if(irep.eq.13) then
c          print*,'k,jbak,ai(k,irep,jbak)=',k,jbak,ai(k,irep,jbak)
c         endif
          a(k) = ai(k,irep,jbak)
        END DO

C       INTERPOLATE GUESS PROFILES TO OB PRESSURES
C       ------------------------------------------

        IF ( onlysf ) THEN
	  lstop = 1
          pfc = psi(irep,jbak)
          pmo = pmi(irep,jbak)
          zob = zsi(irep,jbak)
          tob = tsi(irep,jbak)
          uob = usi(irep,jbak)
          vob = vsi(irep,jbak)
          qob = qsi(irep,jbak)
          IF (pfc.lt.bmiss) THEN
		bak(1,2) = bak(1,2) + pfc * timw
	  ELSE
		bak(1,2) = bmiss
	  END IF
          IF (qob.lt.bmiss) THEN
		bak(2,2) = bak(2,2) + qob * timw * 1.E6
	  ELSE
		bak(2,2) = bmiss
	  END IF
          IF (tob.lt.bmiss) THEN
		bak(3,2) = bak(3,2) + tob * timw - tzero
	  ELSE
		bak(3,2) = bmiss
	  END IF
          IF (zob.lt.bmiss) THEN
		bak(4,2) = bak(4,2) + zob * timw
	  ELSE
		bak(4,2) = bmiss
	  END IF
          IF (uob.lt.bmiss) THEN
		bak(5,2) = bak(5,2) + uob * timw
	  ELSE
		bak(5,2) = bmiss
	  END IF
          IF (vob.lt.bmiss) THEN
		bak(6,2) = bak(6,2) + vob * timw
	  ELSE
		bak(6,2) = bmiss
	  END IF
          IF (pmo.lt.bmiss) THEN
		bak(7,2) = bak(7,2) + pmo * timw
	  ELSE
		bak(7,2) = bmiss
	  END IF
	  IF ( tob.lt.bmiss ) kntgsf = kntgsf + 1
	ELSE
	    lstop = nlev
	END IF	
C*
        DO l = 1, lstop

          pob = obs(1,l)
c         if(irep.eq.13) print*,'l,obs(2,l)=',l,obs(2,l)
          qob = obs(2,l)
          tob = obs(3,l)
          zob = obs(4,l)
          uob = obs(5,l)
          vob = obs(6,l)
          pmo = obs(7,l)

C         SEA-LEVEL PRESSURE
C         ------------------

          IF ((cat(l).eq.0.or.cat(l).eq.6).and.pm.lt.bmiss) THEN
            pmo = pm
            bak(7,l) = bak(7,l) + pmo * timw
          ELSE
            pmo = bmiss
            bak(7,l) = bmiss
          END IF

C         SURFACE PRESSURE
C         ----------------
          IF (cat(l).eq.0.or.(cat(l).eq.6.and.pm.lt.bmiss)) THEN
            cat(l) = 0
            IF (pob.lt.bmiss) THEN
              pfc = pob
            ELSE IF (zob.lt.bmiss.and.ts.lt.bmiss.and.ps.lt.bmiss.and.zs
     +                  .lt.bmiss) THEN
              dz = zob - zs
              tm = ts - dz * beta * .5
              pfc = ps * exp(-dz/(tm*rog))
            ELSE
              pfc = bmiss
            END IF
          ELSE
            pfc = bmiss
          END IF

C         PSIG IS THE SURFACE (HIGHEST) GUESS PRESSURE
          psig = p(1)

c         if(irep.eq.13) print*,'pob=',pob
          IF (pob.gt.0..and.pob.lt.bmiss) THEN
            ip = pob
            ip = min(ip,maxprs)
            ip = max(ip,1)

C           SPECIFIC HUMIDITY
C           -----------------

c           if(irep.eq.13) print*,'qob=',qob
            IF (qob.lt.bmiss) THEN
              lb = vrterp(ip)
              wt = vrterp(ip) - lb
              la = min(lb+1,kmax)
c             if(irep.eq.13) then
c             print*,'la,wt,lb=',la,wt,lb
c             print*,'a(la),a(lb)=',a(la),a(lb)
c             endif
              IF (a(la).lt.bmiss.and.a(lb).lt.bmiss) THEN
                aob = a(lb) + (a(la)-a(lb)) * wt
                qob = exp(aob) * 1.E6
              ELSE
                qob = bmiss
              END IF
            END IF

C           TEMPERATURE
C           -----------

            IF (tob.lt.bmiss) THEN
              IF (pob.gt.psig) THEN
                tob = t(1) + (pob-psig) * betap - tzero
                IF (t(1).ge.bmiss) tob = bmiss
              ELSE
                lb = vrterp(ip)
                wt = vrterp(ip) - lb
                la = min(lb+1,kmax)
                IF (t(la).lt.bmiss.and.t(lb).lt.bmiss) THEN
                  tob = t(lb) + (t(la)-t(lb)) * wt - tzero
                ELSE
                  tob = bmiss
                END IF
              END IF
            END IF

C           HEIGHT
C           ------

            IF (zob.lt.bmiss) THEN
              IF (pob.gt.psig) THEN
                IF (t(1).lt.bmiss.and.p(1).lt.bmiss) THEN
                  tm = t(1) + (.5*(pob-psig)) * betap
                  zob = z(1) - rog * tm * log(pob/p(1))
                ELSE
                  zob = bmiss
                END IF
              ELSE
                lb = vrterp(ip)
                wt = vrterp(ip) - lb
                la = max(min(lb+1,kmax),1)
                IF (t(la).lt.bmiss.and.t(lb).lt.bmiss.and.p(lb).lt.bmiss
     +                      ) THEN
                  tm = t(lb) + (t(la)-t(lb)) * wt
                  zob = z(lb) - rog * tm * log(pob/p(lb))
                ELSE
                  zob = bmiss
                END IF
              END IF
            END IF

C           U AND V COMPONENTS
C           ------------------

            IF (uob.lt.bmiss.or.vob.lt.bmiss) THEN
              lb = vrterp(ip)
              wt = vrterp(ip) - lb
              la = min(lb+1,kmax)
              IF (u(la).lt.bmiss.and.u(lb).lt.bmiss.and.v(la).lt.bmiss
     +                    .and.v(lb).lt.bmiss) THEN
                uob = u(lb) + (u(la)-u(lb)) * wt
                vob = v(lb) + (v(la)-v(lb)) * wt
              ELSE
                uob = bmiss
                vob = bmiss
              END IF
            END IF
          ELSE
            pfc = bmiss
            qob = bmiss
            tob = bmiss
            zob = bmiss
            uob = bmiss
            vob = bmiss
          END IF

C         FILL THE MISSING VALUES WITH A LARGE NUMBER
C         -------------------------------------------

c         if(irep.eq.13) print*,'qob=',qob
          IF (pfc.ge.bmiss) bak(1,l) = bmiss
          IF (qob.ge.bmiss) bak(2,l) = bmiss
          IF (tob.ge.bmiss) bak(3,l) = bmiss
          IF (zob.ge.bmiss) bak(4,l) = bmiss
          IF (uob.ge.bmiss) bak(5,l) = bmiss
          IF (vob.ge.bmiss) bak(6,l) = bmiss

C         SCATTER BACK THE RELEVANT FORECAST VALUES
C         -----------------------------------------

          IF (pfc.lt.bmiss) bak(1,l) = bak(1,l) + pfc * timw
          IF (qob.lt.bmiss) bak(2,l) = bak(2,l) + qob * timw
          IF (tob.lt.bmiss) bak(3,l) = bak(3,l) + tob * timw
          IF (zob.lt.bmiss) bak(4,l) = bak(4,l) + zob * timw
          IF (uob.lt.bmiss) bak(5,l) = bak(5,l) + uob * timw
          IF (vob.lt.bmiss) bak(6,l) = bak(6,l) + vob * timw
c         IF (cape.lt.bmiss) bak(8,l)= bak(8,l) + cape* timw
c         IF (cin.lt.bmiss)  bak(9,l)= bak(9,l) + cin * timw
c         IF (pli.lt.bmiss) bak(10,l)=bak(10,l) + pli * timw

          if(l.eq.1) then
            bak(8,l)=cp
            bak(9,l)=cn
            bak(10,l)=px
          else
            bak(8,l)=bmiss
            bak(9,l)=bmiss
            bak(10,l)=bmiss
          endif

        END DO
      END DO

      RETURN
      END
