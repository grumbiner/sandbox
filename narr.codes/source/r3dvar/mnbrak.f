      SUBROUTINE mnbrak(ax,bx,cx,fa,fb,fc,func, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)

      include 'mpif.h'
      include "my_comm.h"

      REAL      GOLD,GLIMIT,TINY
      real(4)   ax,bx,cx,fa,fb,fc,func,u,fu

      real(4)   xq(nxc*nyc*lmetaex)
      real(4)   xbarq(max(1,mqdata))
      real(4)   pq(nxc*nyc*lmetaex)
      real(4)   pbarq(max(1,mqdata))
      real(4)   yoq(max(1,mqdata))
      real(4)   xbarbq(max(1,mqdata))
      logical qfw(max(1,mqdata))

  real(4) xbarpw(max(1,mpwdata))
  real(4) pbarpw(max(1,mpwdata))
  real(4) yopw(max(1,mpwdata))
  real(4) xbarbpw(max(1,mpwdata))
  logical pwfw(max(1,mpwdata))

      real(4)   xw(nxc*nyc*lmetaex,2)
      real(4)   xbarw(max(1,mwdata))
      real(4)   pw(nxc*nyc*lmetaex,2)
      real(4)   pbarw(max(1,mwdata))
      real(4)   yow(max(1,mwdata))
      real(4)   xbarbw(max(1,mwdata))
      logical wfw(max(1,mwdata))

      real(4)   xbarwr(lmetaex,max(1,mwrdata))
      real(4)   pbarwr(lmetaex,max(1,mwrdata))
      real(4)   yowr(max(1,mwrdata))
      real(4)   xbarbwr(lmetaex,max(1,mwrdata))
      integer(4) ibighwr(lbig2ges+2,max(1,mwrdata))

      real(4) xp(nxc*nyc)
      real(4) xbarp(max(1,mpdata))
      real(4) pp(nxc*nyc)
      real(4) pbarp(max(1,mpdata))
      real(4) yop(max(1,mpdata))
      real(4) xbarbp(max(1,mpdata))
      logical psfw(max(1,mpdata))

      real(4)   xt(nxc*nyc*lmetaex)
      real(4)   xbart(max(1,mtdata))
      real(4)   pt(nxc*nyc*lmetaex)
      real(4)   pbart(max(1,mtdata))
      real(4)   yot(max(1,mtdata))
      real(4)   xbarbt(max(1,mtdata))
      logical tfw(max(1,mtdata))

  real(4) xpred(jpch,npred)
  real(4) xbarrad(max(1,mraddata))
  real(4) ppred(jpch,npred)
  real(4) pbarrad(max(1,mraddata))
  real(4) yorad(max(1,mraddata))
  real(4) xbarbrad(max(1,mraddata))
  logical userad
  logical doqc
  logical radfw(max(1,mraddata))


      EXTERNAL  func
      PARAMETER (GOLD=1.618034, GLIMIT=100., TINY=1.e-20)

!
!  Given a function func, and given distinct initial points ax and bx,
!  this routine searches in the downhill direction (devined by the
!  function as evaluated at the initial points) and returns new points
!  ax, bx, cx that bracket a minimum of the function.  Also returned
!  are the function values at the three points fa, fb and fc.
!  Parameters:  GOLD is the default ratio by which successive intervals
!  are magnified; GLIMIT is the maximum magnification allowed for a
!  parabolic-fit step.
!
      REAL dum,q,r,ulim

      call mpi_comm_rank(my_comm,mype,ierr)

      fa = func(ax, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!     if(mype.eq.0) write(0,*)' mnbrak--ax,fa: ',ax,fa
      fb = func(bx, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!     if(mype.eq.0) write(0,*)' mnbrak--bx,fb: ',bx,fb
      if(fb.gt.fa) then
        dum = ax
        ax = bx
        bx = dum
        dum = fb
        fb = fa
        fa = dum
      endif
      cx = bx+GOLD*(bx-ax)
      fc = func(cx, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!     if(mype.eq.0) write(0,*)' mnbrak--cx,fc: ',cx,fc
    1 if(fb.ge.fc) then
        r = (bx-ax)*(fb-fc)
        q = (bx-cx)*(fb-fa)
        u = bx-((bx-cx)*q-(bx-ax)*r)/(2.*sign(max(abs(q-r),TINY),q-r))
!       if(mype.eq.0) write(0,*)' mnbrak--ax,bx,cx,fa,fb,fc,r,q,u: ',ax,bx,cx,fa,fb,fc,r,q,u
        ulim = bx+GLIMIT*(cx-bx)
        if((bx-u)*(u-cx).gt.0.) then
          fu = func(u, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!         if(mype.eq.0) write(0,*)' mnbrak--u,fu: ',u,fu
          if(fu.lt.fc) then
            ax = bx
            fa = fb
            bx = u
            fb = fu
!           if(mype.eq.0) write(0,*)' mnbrak(end)--ax,bx,cx,fa,fb,fc: ',ax,bx,cx,fa,fb,fc
            return
          elseif(fu.gt.fb) then
            cx = u
            fc = fu
!           if(mype.eq.0) write(0,*)' mnbrak(end)--ax,bx,cx,fa,fb,fc: ',ax,bx,cx,fa,fb,fc
            return
          endif
          u = cx+GOLD*(cx-bx)
          fu = func(u, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!         if(mype.eq.0) write(0,*)' mnbrak--u,fu: ',u,fu
        elseif((cx-u)*(u-ulim).gt.0.) then
          fu = func(u, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!         if(mype.eq.0) write(0,*)' mnbrak--u,fu: ',u,fu
          if(fu.lt.fc) then
            bx = cx
            cx = u
            u = cx+GOLD*(cx-bx)
            fb = fc
            fc = fu
            fu = func(u, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!           if(mype.eq.0) write(0,*)' mnbrak--u,fu: ',u,fu
          endif
        elseif((u-ulim)*(ulim-cx).ge.0.) then
          u = ulim
          fu = func(u, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!         if(mype.eq.0) write(0,*)' mnbrak--u,fu: ',u,fu
        else
          u = cx+GOLD*(cx-bx)
          fu = func(u, &
              xq,xbarq,pq,pbarq,yoq,xbarbq,mqdata,qfw, &
                          xbarpw,pbarpw,yopw,xbarbpw,mpwdata,pwfw, &
              xw,xbarw,pw,pbarw,yow,xbarbw,mwdata,wfw, &
                          xbarwr,pbarwr,yowr,xbarbwr,mwrdata,ibighwr,lbig2ges, &
              xp,xbarp,pp,pbarp,yop,xbarbp,mpdata,psfw, &
              xt,xbart,pt,pbart,yot,xbarbt,mtdata,tfw, &
              xpred,xbarrad,ppred,pbarrad,yorad,xbarbrad,mraddata,jpch,npred,userad,radfw, &
              nxc,nyc,lmetaex,myxsc,myxec,myysc,myyec,doqc)
!         if(mype.eq.0) write(0,*)' mnbrak--u,fu: ',u,fu
        endif
        ax = bx
        bx = cx
        cx = u
        fa = fb
        fb = fc
        fc = fu
        goto 1
      endif
!     if(mype.eq.0) write(0,*)' mnbrak(end)--ax,bx,cx,fa,fb,fc: ',ax,bx,cx,fa,fb,fc
      return
      END
