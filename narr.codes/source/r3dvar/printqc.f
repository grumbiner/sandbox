      subroutine printqc(psdata,pstaid,ip,wtp,mpdata,eyop0,pwdata,pwstaid,ipw,wtpw,mpwdata,eyopw0, &
                qdata,qstaid,iq,wtq,mqdata,eyoq0,tdata,tstaid,it,wtt,mtdata,eyot0, &
                wdata,wstaid,iw,wtw,mwdata,eyow0,iuniterr)

      include "mpif.h"
      include "my_comm.h"

      real(4) psdata(mpdata,6),pwdata(mpwdata,6)
      real(4) qdata(mqdata,6),tdata(mtdata,6)
      real(4) wdata(mwdata,6)
      character(8) pstaid(mpdata),pwstaid(mpwdata),qstaid(mqdata),tstaid(mtdata),wstaid(mwdata)
      integer ip(max(1,mpdata)),ipw(max(1,mpwdata)),iq(max(1,mqdata))
      integer it(max(1,mtdata)),iw(max(1,mwdata))
      real(4) wtp(4,max(1,mpdata)),wtpw(4,max(1,mpwdata)),wtq(4,max(1,mqdata))
      real(4) wtt(4,max(1,mtdata)),wtw(4,max(1,mwdata))
      real(4) eyop0(max(1,mpdata)),eyopw0(max(1,mpwdata)),eyoq0(max(1,mqdata))
      real(4) eyot0(max(1,mtdata)),eyow0(max(1,mwdata))
      character(8) estaid(2000)
      character(16) filename
      real(4) edata(2000,6)

      call mpi_comm_rank(my_comm,mype,ierr)
      iunit = iuniterr + mype
      write(6,*)' printqc--writing to unit ',iunit,' for mype=',mype
      iunitp = iuniterr + mype + 2*npes + 2
      write(filename,'(''error'',i3.3)')mype
      open(iunitp,file=filename,form='formatted')

!  Print some information where ix(i) = 1

!  Height data

      ie = 0
      write(6,500)
  500 format(' Height data with large errors:')
      do i=1,mpdata
        if(ip(i).ne.0) then
          if(wtp(2,i).ne.0.) then
            rat = psdata(i,2)/wtp(2,i)
          else
            rat = 0.
          endif
          write(6,508) pstaid(i),'Z',exp(psdata(i,1)),psdata(i,2),psdata(i,3), &
            eyop0(i)*(wtp(3,i)+wtp(4,i)),ip(i),wtp(1,i),psdata(i,6),psdata(i,5)
          write(iunitp,508) pstaid(i),'Z',exp(psdata(i,1)),psdata(i,2),psdata(i,3), &
            eyop0(i)*(wtp(3,i)+wtp(4,i)),ip(i),wtp(1,i),psdata(i,6),psdata(i,5)
        endif
        if(ip(i).eq.1 .and. psdata(i,6).ne.0.) then   ! Check for SDM keep
          ie = ie+1
          if(ie.gt.2000) then
            ie = ie-1
            goto 10
          endif
          estaid(ie) = pstaid(i)
          do j=1,6
            edata(ie,j) = psdata(i,j)
          enddo
   10     continue
        endif
      enddo
      ivar = 2
      irc = 2
      if(ie.gt.0) then
        write(iunit,550) ie,ivar,irc
        write(iunit,551) (estaid(i),min(1.e7,exp(edata(i,1))),edata(i,4),edata(i,5),i=1,ie)
      endif
  550 format(1x,3i8)
  551 format(1x,a10,f10.1,f10.2,f10.0)
      write(0,*) ' printqc--iunit,ie,ivar,irc,mype',iunit,ie,ivar,irc,mype
      write(6,*) ' printqc--iunit,ie,ivar,irc,mype',iunit,ie,ivar,irc,mype
      write(6,511) (mype,estaid(i),exp(edata(i,1)),edata(i,4),edata(i,5),i=1,ie)
  511 format(i5,a10,f10.1,f10.2,f8.0)

!  Precipitable water data

      ie = 0
      write(6,502)
  502 format(' Precipitable water data with large errors:')
      do i=1,mpwdata
        if(ipw(i).ne.0) then
          if(wtpw(2,i).ne.0.) then
            rat = pwdata(i,2)/wtpw(2,i)
          else
            rat = 0.
          endif
          write(6,509) pwstaid(i),'PW',exp(pwdata(i,1)),pwdata(i,2),pwdata(i,3), &
            eyopw0(i)*(wtpw(3,i)+wtpw(4,i)),ipw(i),wtpw(1,i),pwdata(i,6),pwdata(i,5)
          write(iunitp,509) pwstaid(i),'PW',exp(pwdata(i,1)),pwdata(i,2),pwdata(i,3), &
            eyopw0(i)*(wtpw(3,i)+wtpw(4,i)),ipw(i),wtpw(1,i),pwdata(i,6),pwdata(i,5)
        endif
        if(ipw(i).eq.1 .and. pwdata(i,6).ne.0.) then
          ie = ie + 1
          if(ie.gt.2000) then
            ie = ie - 1
            goto 20
          endif
          estaid(ie) = pwstaid(i)
          do j=1,6
            edata(ie,j) = pwdata(i,j)
          enddo
   20     continue
        endif
      enddo
      ivar = 3
      irc = 2
      if(ie.gt.0) then
        write(iunit,550) ie,ivar,irc
        write(iunit,551) (estaid(i),min(1.e7,exp(edata(i,1))),edata(i,4),edata(i,5),i=1,ie)
      endif
      write(0,*) ' printqc--iunit,ie,ivar,irc,mype',iunit,ie,ivar,irc,mype
      write(6,*) ' printqc--iunit,ie,ivar,irc,mype',iunit,ie,ivar,irc,mype
      write(6,511) (mype,estaid(i),exp(edata(i,1)),edata(i,4),edata(i,5),i=1,ie)

!  Moisture data

      ie = 0
      write(6,503)
  503 format(' Moisture data with large errors:')
      do i=1,mqdata
        if(iq(i).ne.0) then
          if(wtq(2,i).ne.0.) then
            rat = qdata(i,2)/wtq(2,i)
          else
            rat = 0.
          endif
          write(6,507) qstaid(i),'Q',exp(qdata(i,1)),qdata(i,2),qdata(i,3), &
            eyoq0(i)*(wtq(3,i)+wtq(4,i)),iq(i),wtq(1,i),qdata(i,6),qdata(i,5)
          write(iunitp,507) qstaid(i),'Q',exp(qdata(i,1)),qdata(i,2),qdata(i,3), &
            eyoq0(i)*(wtq(3,i)+wtq(4,i)),iq(i),wtq(1,i),qdata(i,6),qdata(i,5)
        endif
        if(iq(i).eq.1 .and. qdata(i,6).ne.0.) then
          ie = ie + 1
          if(ie.gt.2000) then
            ie = ie - 1
            goto 30
          endif
          estaid(ie) = qstaid(i)
          do j=1,6
            edata(ie,j) = qdata(i,j)
          enddo
   30     continue
        endif
      enddo
      ivar = 4
      irc = 2
      if(ie.gt.0) then
        write(iunit,550) ie,ivar,irc
        write(iunit,551) (estaid(i),min(1.e7,exp(edata(i,1))),edata(i,4),edata(i,5),i=1,ie)
      endif
      write(0,*) ' printqc--iunit,ie,ivar,irc,mype',iunit,ie,ivar,irc,mype
      write(6,*) ' printqc--iunit,ie,ivar,irc,mype',iunit,ie,ivar,irc,mype
      write(6,511) (mype,estaid(i),exp(edata(i,1)),edata(i,4),edata(i,5),i=1,ie)

!  Temperature data

      ie = 0
      T0 = 273.15
      write(6,504)
  504 format(' Temperature data with large errors:')
      print *,' mtdata = ',mtdata
      do i=1,mtdata
        if(it(i).ne.0) then
          if(wtt(2,i).ne.0.) then
            rat = tdata(i,2)/wtt(2,i)
          else
            rat = 0.
          endif
          write(6,506) tstaid(i),'T',exp(tdata(i,1)),tdata(i,2)-T0,tdata(i,3)-T0, &
            eyot0(i)*(wtt(3,i)+wtt(4,i))-T0,it(i),wtt(1,i),tdata(i,6),tdata(i,5)
          write(iunitp,506) tstaid(i),'T',exp(tdata(i,1)),tdata(i,2)-T0,tdata(i,3)-T0, &
            eyot0(i)*(wtt(3,i)+wtt(4,i))-T0,it(i),wtt(1,i),tdata(i,6),tdata(i,5)
        endif
        if(it(i).eq.1 .and. tdata(i,6).ne.0.) then
          ie = ie + 1
          if(ie.gt.2000) then
            ie = ie - 1
            goto 40
          endif
          estaid(ie) = tstaid(i)
          do j=1,6
            edata(ie,j) = tdata(i,j)
          enddo
   40     continue
        endif
      enddo
  506 format(1x,a8,1x,a2,'  press,ob,guess,anal: ',4(1x,f9.1),'  it: ',i1,'  anl-wt: ',f8.4,2(1x,f6.0))
  507 format(1x,a8,1x,a2,'  press,ob,guess,anal: ',1x,f9.1,3(1x,e9.3),'  iq: ',i1,'  anl-wt: ',f8.4,2(1x,f6.0))
  508 format(1x,a8,1x,a2,'  press,ob,guess,anal: ',4(1x,f9.1),'  ip: ',i1,'  anl-wt: ',f8.4,2(1x,f6.0))
  509 format(1x,a8,1x,a2,'  press,ob,guess,anal: ',4(1x,f9.1),'  ipw: ',i1,'  anl-wt: ',f8.4,2(1x,f6.0))
  510 format(1x,a8,1x,a2,'  press,ob,guess,anal: ',4(1x,f9.1),'  iw: ',i1,'  anl-wt: ',f8.4,2(1x,f6.0))
      ivar = 5
      irc = 2
      if(ie.gt.0) then
        write(iunit,550) ie,ivar,irc
        write(iunit,551) (estaid(i),min(1.e7,exp(edata(i,1))),edata(i,4),edata(i,5),i=1,ie)
      endif
      write(0,*) ' printqc--iunit,ie,ivar,irc,mype',iunit,ie,ivar,irc,mype
      write(6,*) ' printqc--iunit,ie,ivar,irc,mype',iunit,ie,ivar,irc,mype
      write(6,511) (mype,estaid(i),exp(edata(i,1)),edata(i,4),edata(i,5),i=1,ie)

!  Wind data

      ie = 0
      write(6,505)
  505 format(' Wind data with large errors:')
      do i=1,mwdata
        if(iw(i).ne.0) then
          if(wtw(2,i).ne.0.) then
            rat = wdata(i,2)/wtw(2,i)
          else
            rat = 0.
          endif
          write(6,510) wstaid(i),'V',exp(wdata(i,1)),wdata(i,2),wdata(i,3), &
            eyow0(i)*(wtw(3,i)+wtw(4,i)),iw(i),wtw(1,i),wdata(i,6),wdata(i,5)
          write(iunitp,510) wstaid(i),'V',exp(wdata(i,1)),wdata(i,2),wdata(i,3), &
            eyow0(i)*(wtw(3,i)+wtw(4,i)),iw(i),wtw(1,i),wdata(i,6),wdata(i,5)
        endif
        if(iw(i).eq.1 .and. wdata(i,6).ne.0.) then
          ie = ie + 1
          if(ie.gt.2000) then
            ie = ie - 1
            goto 50
          endif
          estaid(ie) = wstaid(i)
          do j=1,6
            edata(ie,j) = wdata(i,j)
          enddo
   50     continue
        endif
      enddo
      ivar = 6
      irc = 2
      if(ie.gt.0) then
        write(iunit,550) ie,ivar,irc
        write(iunit,551) (estaid(i),min(1.e7,exp(edata(i,1))),edata(i,4),edata(i,5),i=1,ie)
      endif
      write(0,*) ' printqc--iunit,ie,ivar,irc,mype',iunit,ie,ivar,irc,mype
      write(6,*) ' printqc--iunit,ie,ivar,irc,mype',iunit,ie,ivar,irc,mype
      write(6,511) (mype,estaid(i),exp(edata(i,1)),edata(i,4),edata(i,5),i=1,ie)

      close(iunit)
      close(iunitp)

      return
      end
