      subroutine setprint(iq,it,iw,ip,ipw,mqdata,mtdata,mwdata,mpdata,mpwdata, &
                qstaid,tstaid,wstaid,pstaid,pwstaid)
      character(8) qstaid(max(1,mqdata)),tstaid(max(1,mtdata)),wstaid(max(1,mwdata))
      character(8) pstaid(max(1,mpdata)),pwstaid(max(1,mpwdata)),stnid
      integer(4)   iq(max(1,mqdata)),it(max(1,mtdata)),iw(max(1,mwdata)),ip(max(1,mpdata)),ipw(max(1,mpwdata))
      data iprint /77/

!  Initialize

      iq = 0
      it = 0
      iw = 0
      ip = 0
      ipw = 0

!  Read station id's for stations to print info.

   10 read(iprint,500,end=100,err=100) stnid
      write(6,501) stnid
  500 format(1x,a8)
  501 format(' setprint--station to print: ',a8)

!  Search through list for match.

      do i=1,mqdata
        if(qstaid(i).eq.stnid) then
          iq(i) = 2
          goto 20
        endif
   20   continue
      enddo
      do i=1,mtdata
        if(tstaid(i).eq.stnid) then
          it(i) = 2
          goto 30
        endif
   30   continue
      enddo
      do i=1,mwdata
        if(wstaid(i).eq.stnid) then
          iw(i) = 2
          goto 40
        endif
   40   continue
      enddo
      do i=1,mpdata
        if(pstaid(i).eq.stnid) then
          ip(i) = 2
          goto 50
        endif
   50   continue
      enddo
      do i=1,mpwdata
        if(pwstaid(i).eq.stnid) then
          ipw(i) = 2
          goto 60
        endif
   60   continue
      enddo
      goto 10
  100 continue
      write(6,502)
  502 format(' setprint--station list complete')
      close(iprint)

      return
      end
