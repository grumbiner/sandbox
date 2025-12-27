      SUBROUTINE VWEIGHTS
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    VWEIGHTS    COMPUTE INTERP WEIGHTS FOR WINDS
C   PRGRMMR: ROGERS          ORG: W/NP22     DATE: 99-09-24
C
C ABSTRACT:  COMPUTE WEIGHTS FOR INTERPOLATION OF WINDS ON
C            THE ETA GRID 104 ANALYSIS (WHICH ARE AT H-POINTS)
C            TO THE LOCATION OF THE U AND V WIND POINTS IN
C            THE NGM
C
C PROGRAM HISTORY LOG:
C   99-09-24  ERIC ROGERS
C
C USAGE:    CALL VWEIGHTS
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST:
C     NONE : INTERPOLATION WEIGHTS PASSED TO MAIN CODE VIA
C            COMMON BLOCK VWGTS
C
C   OUTPUT FILES:
C     NONE
C
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       W3LIB   - W3FB10
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN-90
C     MACHINE : CRAY C-90
C$$$
C
      INCLUDE "parmg104"
      PARAMETER(IGOUT=ILIM,JGOUT=JLIM+1)
      REAL GLAT(IGOUT,JGOUT,3),GLON(IGOUT,JGOUT,3)
      REAL POLEII(3),POLEJJ(3)
      REAL D(6)
      REAL LAM1,LAM2
C
      COMMON /VWGTS/ WU(IGOUT,JGOUT,6),WV(IGOUT,JGOUT,6)
C
      COMMON /IPOINT/ IPT,JPT
C
      DATA POLEII/ 75.5, 75.5, 75.0/
      DATA POLEJJ/109.5,109.0,109.5/
      DATA EARTHR / 6371.2 /
C
C*********************************************************************
C
C     SET CONSTANTS.
      XMESHL = 90.75464
      ALONVT = 105.000000
      RE     = (EARTHR * 1.86603) / XMESHL
      PI     = ACOS(-1.)
      HALFPI = PI/2.
      TWOPI  = 2.*PI
      DTR    = PI/180.
      R2D    = 1./DTR
      GI2    = RE * RE
C
      DO K = 1, 3
        CALL GENLL(POLEII(K),POLEJJ(K),ALONVT,XMESHL,
     1    GLAT(1,1,K),GLON(1,1,K))
c       IOUT=50+K
        do j = 1,jgout
         do i = 1,igout
           elon = 360.0 - GLON(I,J,K)
           if(i.eq.ipt.and.j.eq.jpt) then
           WRITE(6,10000) i,j,GLAT(I,J,K),GLON(I,J,K),elon
10000      FORMAT(2x,2i8,2x,f12.5,1x,f12.5,1x,f12.5) 
           endif
         end do
        end do
      ENDDO
c 
C do internal u-points
c
      do jt = 1, jgout-1
      do it = 2, igout-1
       sumdel=0.0
       xmax=-99999.0
       xmin=99999.0
       tlat=glat(it,jt,2)
       tlon=glon(it,jt,2)
       if(mod(it,10).eq.0 .and. mod(jt,10).eq.0) then
         write(6,10)it,jt,tlat,tlon
10       format(1x,2i4,2(1x,e12.5))
       endif
       do kupt = 1, 6
        if(kupt.eq.1) then
          inc=-1
          jnc=0
        elseif(kupt.eq.2) then
          inc=0
          jnc=0
        elseif(kupt.eq.3) then
          inc=1
          jnc=0
        elseif(kupt.eq.4) then
          inc=-1
          jnc=1
        elseif(kupt.eq.5) then
          inc=0
          jnc=1
        elseif(kupt.eq.6) then
          inc=1
          jnc=1
        endif
        slat=glat(it+inc,jt+jnc,1)
        slon=glon(it+inc,jt+jnc,1)
        call w3fb10(tlat,tlon,slat,slon,beard,d(kupt))
        if(mod(it,10).eq.0 .and. mod(jt,10).eq.0) then
         write(6,2000)kupt,it+inc,jt+jnc,slat,slon,
     &    d(kupt) 
        endif
        sumdel = sumdel + d(kupt)
        xmax=amax1(d(kupt),xmax)
        xmin=amin1(d(kupt),xmin)
      enddo
      bwgt=(xmax-xmin)/(xmax*2.)
      ydiff=1.-(bwgt*2.)
      swgt=ydiff*0.25 
      sum = 2.*bwgt+4.*swgt
      do kupt = 1, 6
        if(kupt.eq.2 .or. kupt.eq.5) then
          wu(it,jt,kupt) = bwgt
        else
          wu(it,jt,kupt) = swgt
        endif
      enddo
       if(mod(it,10).eq.0 .and. mod(jt,10).eq.0) then
        write(6,2100)it,jt,(d(k),k=1,6),
     &         (wu(it,jt,k),k=1,6),sum
2000    format(1x,3i6,3(e12.5,1x))
2100    format(1x,2i4,6(1x,e12.5),/,9x,7(1x,E12.5))
       endif
      enddo
      enddo
C
C done with internal u-points, do u-points at it=1
c
      it=1
      do jt = 1, jgout
       if(jt.lt.jgout) then
       wu(it,jt,1)=0.0
       wu(it,jt,4)=0.0
       sumdel=0.0
       xmax=-99999.0
       xmin=99999.0
       tlat=glat(it,jt,2)
       tlon=glon(it,jt,2)
       if(mod(jt,10).eq.0) then
         write(6,10)it,jt,tlat,tlon
       endif
       do kupt = 2, 6
        if(kupt.eq.2) then
          inc=0
          jnc=0
        elseif(kupt.eq.3) then
          inc=1
          jnc=0
        elseif(kupt.eq.5) then
          inc=0
          jnc=1
        elseif(kupt.eq.6) then
          inc=1
          jnc=1
        endif
        if(kupt.ne.4) then
         slat=glat(it+inc,jt+jnc,1)
         slon=glon(it+inc,jt+jnc,1)
         call w3fb10(tlat,tlon,slat,slon,beard,d(kupt))
         if(mod(jt,10).eq.0) then
          write(6,2000)kupt,it+inc,jt+jnc,slat,slon,
     &     d(kupt) 
         endif
         sumdel = sumdel + d(kupt)
         xmax=amax1(d(kupt),xmax)
         xmin=amin1(d(kupt),xmin)
        endif
       enddo
      bwgt=(2.*(xmax-xmin))/(xmax*3.)
      ydiff=1.-(bwgt*2.)
      swgt=ydiff*0.5 
      do kupt = 2, 6
        if(kupt.eq.2 .or. kupt.eq.5) then
          wu(it,jt,kupt) = bwgt
        elseif(kupt.eq.3.or.kupt.eq.6) then
          wu(it,jt,kupt) = swgt
        endif
      enddo
       if(mod(jt,10).eq.0) then
        write(6,2100)it,jt,(d(k),k=1,6),
     &         (wu(it,jt,k),k=1,6),sum
       endif
      else
       wu(it,jt,1) = 0.0
       wu(it,jt,2) = 1.0
       wu(it,jt,3) = 0.0
       wu(it,jt,4) = 0.0
       wu(it,jt,5) = 0.0
       wu(it,jt,6) = 0.0
      endif
      enddo
c
C done with u-points at it=1, do u-points at it=igout
c
      it=igout
      do jt = 1, jgout
       if(jt.lt.jgout) then
       wu(it,jt,3)=0.0
       wu(it,jt,6)=0.0
       sumdel=0.0
       xmax=-99999.0
       xmin=99999.0
       tlat=glat(it,jt,2)
       tlon=glon(it,jt,2)
       if(mod(jt,10).eq.0) then
         write(6,10)it,jt,tlat,tlon
       endif
       do kupt = 1, 5
        if(kupt.eq.1) then
          inc=-1
          jnc=0
        elseif(kupt.eq.2) then
          inc=0
          jnc=0
        elseif(kupt.eq.4) then
          inc=-1
          jnc=1
        elseif(kupt.eq.5) then
          inc=0
          jnc=1
        endif
        if(kupt.ne.3) then
         slat=glat(it+inc,jt+jnc,1)
         slon=glon(it+inc,jt+jnc,1)
         call w3fb10(tlat,tlon,slat,slon,beard,d(kupt))
         if(mod(jt,10).eq.0) then
          write(6,2000)kupt,it+inc,jt+jnc,slat,slon,
     &     d(kupt) 
         endif
         sumdel = sumdel + d(kupt)
         xmax=amax1(d(kupt),xmax)
         xmin=amin1(d(kupt),xmin)
        endif
       enddo
      bwgt=(2.*(xmax-xmin))/(xmax*3.)
      ydiff=1.-(bwgt*2.)
      swgt=ydiff*0.5 
      do kupt = 1, 5
        if(kupt.eq.2 .or. kupt.eq.5) then
          wu(it,jt,kupt) = bwgt
        elseif(kupt.eq.1.or.kupt.eq.4) then
          wu(it,jt,kupt) = swgt
        endif
      enddo
       if(mod(jt,10).eq.0) then
        write(6,2100)it,jt,(d(k),k=1,6),
     &         (wu(it,jt,k),k=1,6),sum
       endif
      else
       wu(it,jt,1) = 0.0
       wu(it,jt,2) = 1.0
       wu(it,jt,3) = 0.0
       wu(it,jt,4) = 0.0
       wu(it,jt,5) = 0.0
       wu(it,jt,6) = 0.0
      endif
      enddo
C
c  end of u-points at it=igout, do u-points at jt=jgout
C
      jt=jgout
      do it = 1, igout
        wu(it,jt,1) = 0.0
        wu(it,jt,2) = 1.0
        wu(it,jt,1) = 0.0
        wu(it,jt,1) = 0.0
        wu(it,jt,1) = 0.0
        wu(it,jt,1) = 0.0
      enddo
C
C  end of u-points at jt=jgout, do internal v-points
C
      do jt = 2, jgout-1
      do it = 1, igout-1
       sumdel=0.0
       xmax=-99999.0
       xmin=99999.0
       tlat=glat(it,jt,3)
       tlon=glon(it,jt,3)
       if(mod(it,10).eq.0 .and. mod(jt,10).eq.0) then
         write(6,10)it,jt,tlat,tlon
       endif
       do kvpt = 1, 6
        if(kvpt.eq.1) then
          inc=0
          jnc=-1
        elseif(kvpt.eq.2) then
          inc=1
          jnc=-1
        elseif(kvpt.eq.3) then
          inc=0
          jnc=0
        elseif(kvpt.eq.4) then
          inc=1
          jnc=0
        elseif(kvpt.eq.5) then
          inc=0
          jnc=1
        elseif(kvpt.eq.6) then
          inc=1
          jnc=1
        endif
        slat=glat(it+inc,jt+jnc,1)
        slon=glon(it+inc,jt+jnc,1)
        call w3fb10(tlat,tlon,slat,slon,beard,d(kvpt))
        if(mod(it,10).eq.0 .and. mod(jt,10).eq.0) then
         write(6,2000)kvpt,it+inc,jt+jnc,slat,slon,
     &    d(kvpt)
        endif
        sumdel = sumdel + d(kvpt)
        xmax=amax1(d(kvpt),xmax)
        xmin=amin1(d(kvpt),xmin)
      enddo
      bwgt=(xmax-xmin)/(xmax*2.)
      ydiff=1.-(bwgt*2.)
      swgt=ydiff*0.25
      sum = 2.*bwgt+4.*swgt
      do kvpt = 1, 6
        if(kvpt.eq.3 .or. kvpt.eq.4) then
          wv(it,jt,kvpt) = bwgt
        else
          wv(it,jt,kvpt) = swgt
        endif
      enddo
       if(mod(it,10).eq.0 .and. mod(jt,10).eq.0) then
        write(6,2100)it,jt,(d(k),k=1,6),
     &         (wv(it,jt,k),k=1,6),sum
       endif
      enddo
      enddo
C
C  Done with internal v-points, do v-points at jt=1
C
      jt = 1
      do it = 1, igout
       if(it.lt.igout) then
        wv(it,jt,1) = 0.0
        wv(it,jt,2) = 0.0
        xmax=-99999.0
        xmin=99999.0
        sumdel=0.0
        tlat=glat(it,jt,3)
        tlon=glon(it,jt,3)
        if(mod(it,10).eq.0) then
         write(6,10)it,jt,tlat,tlon
        endif
        do kvpt = 3, 6
         if(kvpt.eq.3) then
          inc=0
          jnc=0
         elseif(kvpt.eq.4) then
          inc=1
          jnc=0
         elseif(kvpt.eq.5) then
          inc=0
          jnc=1
         elseif(kvpt.eq.6) then
          inc=1
          jnc=1
         endif
         slat=glat(it+inc,jt+jnc,1)
         slon=glon(it+inc,jt+jnc,1)
         call w3fb10(tlat,tlon,slat,slon,beard,d(kvpt))
         if(mod(it,10).eq.0) then
           write(6,2000)kvpt,it+inc,jt+jnc,slat,slon,
     &        d(kvpt)
         endif
         sumdel = sumdel + d(kvpt)
         xmax=amax1(d(kvpt),xmax)
         xmin=amin1(d(kvpt),xmin)
        enddo
        bwgt=(2.*(xmax-xmin))/(3.*xmax)
        ydiff=1.-(bwgt*2.)
        swgt=ydiff*.5
        do kvpt = 3, 6
          if(kvpt.eq.3 .or. kvpt.eq.4) then
            wv(it,jt,kvpt) = bwgt
          else
            wv(it,jt,kvpt) = swgt
          endif
        enddo
        if(mod(it,10).eq.0) then
         write(6,2100)it,jt,(d(k),k=1,6),
     &         (wv(it,jt,k),k=1,6),sum
        endif
       else
        wv(it,jt,1)=0.0
        wv(it,jt,2)=0.0
        wv(it,jt,3)=1.0
        wv(it,jt,4)=0.0
        wv(it,jt,5)=0.0
        wv(it,jt,6)=0.0
       endif
      enddo
C
C  Done with v-points at jt=1, do v-points at jt=jgout
C
      jt = jgout
      do it = 1, igout
       if(it.lt.igout) then
        wv(it,jt,5) = 0.0
        wv(it,jt,6) = 0.0
        xmax=-99999.0
        xmin=99999.0
        sumdel=0.0
        tlat=glat(it,jt,3)
        tlon=glon(it,jt,3)
        if(mod(it,10).eq.0) then
         write(6,10)it,jt,tlat,tlon
        endif
        do kvpt = 1, 4
         if(kvpt.eq.1) then
          inc=0
          jnc=-1
         elseif(kvpt.eq.2) then
          inc=1
          jnc=-1
         elseif(kvpt.eq.3) then
          inc=0
          jnc=0
         elseif(kvpt.eq.4) then
          inc=1
          jnc=0
         endif
         slat=glat(it+inc,jt+jnc,1)
         slon=glon(it+inc,jt+jnc,1)
         call w3fb10(tlat,tlon,slat,slon,beard,d(kvpt))
         if(mod(it,10).eq.0) then
           write(6,2000)kvpt,it+inc,jt+jnc,slat,slon,
     &        d(kvpt)
         endif
         sumdel = sumdel + d(kvpt)
         xmax=amax1(d(kvpt),xmax)
         xmin=amin1(d(kvpt),xmin)
        enddo
        bwgt=(2.*(xmax-xmin))/(3.*xmax)
        ydiff=1.-(bwgt*2.)
        swgt=ydiff*.5
        do kvpt = 1, 4
          if(kvpt.eq.3 .or. kvpt.eq.4) then
            wv(it,jt,kvpt) = bwgt
          else
            wv(it,jt,kvpt) = swgt
          endif
        enddo
        if(mod(it,10).eq.0) then
         write(6,2100)it,jt,(d(k),k=1,6),
     &         (wv(it,jt,k),k=1,6),sum
        endif
       else
        wv(it,jt,1)=0.0
        wv(it,jt,2)=0.0
        wv(it,jt,3)=1.0
        wv(it,jt,4)=0.0
        wv(it,jt,5)=0.0
        wv(it,jt,6)=0.0
       endif
      enddo
C
C Done with v-points at jt=jgout, do v-points at i=igout
C
      it=igout
      do jt=1,jgout
        wv(it,jt,1) = 0.0
        wv(it,jt,2) = 0.0
        wv(it,jt,3) = 1.0
        wv(it,jt,4) = 0.0
        wv(it,jt,5) = 0.0
        wv(it,jt,6) = 0.0
      enddo
C
C Finally finished
C
      RETURN
      END
