subroutine read_nonbufsat(nbufrad,mbufrad, &
        erlon0,erlat0,wbglb,sbglb,dlon0,dlat0,imetaglb,jmetaglb,nsat,mype,npes,delhour,gsstm, &
        satfile,sattype,isatthin,nelesat,isatid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nonbufsat  read 1b sat data which is not stored
!                               in bufr format.
!                                This is adaptation of parts of 
!                                 subroutine read_obs from global ssi
!   prgmmr: d. parrish                         10/13/2001
!
! abstract: read 1b sat data which is not stored in bufr format
!
! program history log:
!   10-13-2001  parrish
!
!   input argument list:
!     allrad   - array which may already contain some satellite radiance data (from bufr file)
!     nbufrad  - max number of satellite radiance data
!     mbufrad  - current number of satellite radiance data on this pe
!     erlon0,erlat0 - earth coordinates of center of complete rotated eta-grid
!     wbglb,sbglb - coordinates of sw corner of complete eta-grid
!     dlon0,dlat0 - longitude and latitude grid increments of eta-grid
!     imetaglb,jmetaglb - horizontal dimensions of complete eta-grid
!
!   output argument list:
!     allrad - augmented array of radiance data on this pe
!     mbufrad - new total number of satellite radiance data on this pe
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  include 'mpif.h'
         include "my_comm.h"

  parameter (nmax=75)

  integer(4) idate5(5)
  real(4),pointer::allrad(:,:)
  common/radbuf/allrad

  real(4) data1b(nmax,40)
  real(4) data1b4(nmax)                    !  after  1/04/1998
  real(4) data1b8(nmax)                    !  before 1/04/1998
  real(4) rlong(nmax),rlatg(nmax)
  integer(4) iflagg(nmax)
  integer(8) labelrad
  real(4) rlabelrad(2)
  equivalence(rlabelrad(1),labelrad)

  character(10) satfile(50),sattype(50)
  integer(4) isatthin(50),nelesat(50),isatid(50)

  logical airs,hirs2,hirs3,hsb,msu,amsua,amsub,eos_amsua,cosfilter
  logical after_1_4_1998

! Initialize variables

  iordcheck=1
  lbig2check=4
  lhalfcheck=1
  istaghglb=0
  dg2rad=atan(1.)/45.
  cerlat0=cos(erlat0*dg2rad)
  serlat0=sin(erlat0*dg2rad)
!-------------------------------following limits guarantee that points kept can be interpolated
!------------------------------- to with 4-point interpolation in horizontal
! rlongmin=wbglb+1.0001*dlon0
! rlongmax=wbglb+2.*dlon0*(imetaglb-1.)-1.0001*dlon0
! rlatgmin=sbglb+1.0001*dlat0
! rlatgmax=sbglb+dlat0*(jmetaglb-1.)-1.0001*dlat0
  rlongmin=wbglb+3.1*dlon0
  rlongmax=wbglb+2.*dlon0*(imetaglb-1.)-3.1*dlon0
  rlatgmin=sbglb+3.1*dlat0
  rlatgmax=sbglb+dlat0*(jmetaglb-1.)-3.1*dlat0
  if(mype.eq.0) print *,' in read_nonbufsat, rlongmin,max=',rlongmin,rlongmax
  if(mype.eq.0) print *,' in read_nonbufsat, rlatgmin,max=',rlatgmin,rlatgmax


  lunin  = 10
!
! Loop over satellite radiance data

  labelrad=-1_8
  if(mype.eq.0) then
   do ii=1,nsat
     print*,'ii,satfile(ii)=',ii,satfile(ii)
   enddo
  endif
  do ii=1,nsat

! to turn off cosine filter for polar orbiting data set ithin to -ithin.

   cosfilter=.true.
   if(isatthin(ii) .le. 0.) then
     isatthin(ii) = abs(isatthin(ii))
     cosfilter=.false.
   end if
   isatthin(ii) = max(1,isatthin(ii))
                      if(mype.eq.0) print *,' ii,isatthin,cosfilter=',ii,isatthin(ii),cosfilter

   airs=.false.
   hirs2=.false.
   hirs3=.false.
   hsb=.false.
   msu=.false.
   amsua=.false.
   amsub=.false.
   eos_amsua=.false.
   if(sattype(ii) .eq. 'hirs/2')hirs2=.true.
   if(sattype(ii) .eq. 'hirs/3')hirs3=.true.
   if(sattype(ii) .eq. 'msu')msu=.true.
   if(sattype(ii) .eq. 'amsua')amsua=.true.
   if(sattype(ii) .eq. 'amsub')amsub=.true.
        step = 0.
        start=0.
     if (hirs2) then
        step   = 1.80
        start  = -49.5
     elseif (msu) then
        step   = 9.474
        start  = -47.37
     elseif (hirs3) then
        step   = 1.80
        start  = -49.5
     elseif (amsua) then
        step   = 3. + 1./3.
        start  = -48.33
     elseif (amsub) then
        step   = 1.1
        start  = -48.95
     elseif (airs) then
        step   = 1.1
        start = -48.95
     elseif (hsb) then
        step   = 1.1
        start  = -48.95
     elseif (eos_amsua) then
        step   = 1.1
        start  = -48.95
!       step   = 3. + 1./3.
!       start  = -48.33
      end if
                      if(mype.eq.0) print *,' step,start=',step,start
!
!      Process TOVS 1b data
   if (hirs2 .or. hirs3 .or. msu .or. amsua .or. amsub) then
!
!      Open units to satellite data input files
    open(lunin,file=satfile(ii),form='unformatted')

!    interrogate file to see if old or new format.  if new format, then position after header record

    call interrogate_1b(lunin,data1b8,after_1_4_1998,ils,inadir,ilat,ilon,ilza,isza,ihgt,n1data,nreal,nchanl)
     if(mype.eq.0) print *,' interrogate 1b file ',satfile(ii),'  after_1_4_1998=',after_1_4_1998
!
!        Initialize variables

    nread = 0
    nthin = 0
!
!        Set indices for lon,lat location in data array

    ich8=10+8
    ich10=10+10
    ich2=10+2
    ich15=10+15
!
    nelesat(ii)=n1data
    if(mype.eq.0) write(6,*)'read_nonbufsat:  read 1B header record for ',satfile(ii),lunin, &
              ' with ',nreal,nchanl,n1data
    if(n1data.eq.0) go to 120
    if (abs(n1data)>nmax) then
     if(mype.eq.0) then
      write(6,*)'read_nonbufsat:  *** WARNING:  BAD 1B FILE***'
      write(6,*)' iscr,satfile=',lunin,satfile(ii)
      write(6,*)' SKIP PROCESSING OF THIS 1B FILE'
     end if
     goto 130
    endif
!              if(mype.eq.0) write(0,*)' at 1 in read_nonbufsat'
!
!        Read and optionally thin data.
         aix=0.
         if(hirs2 .or. msu)then
           rato=1.1363987
         end if
    do nrec=1,1000000
            ith=0
            do while (aix .lt. float(isatthin(ii)))
              if(after_1_4_1998) then
               read(lunin,end=180,err=120) (data1b4(k),k=1,n1data)
              else
               read(lunin,end=180,err=120) (data1b8(k),k=1,n1data)
               data1b4(1:n1data)=data1b8(1:n1data)
              end if
!             if(data1b4(9).gt.0.0) then
!             if(data1b4(10).lt.-30.0.and.data1b4(10).gt.-160.0.and.data1b4(9).gt.0.0) then
!             if(ii.eq.10) then
!               print*,'hirs2,msu,satfile(ii)=',hirs2,msu,satfile(ii)
!               do i=1,5
!                print*,'i,data1b(i)=',i,data1b4(i)
!               enddo
!             endif
              ith=ith+1
                     ithmax=max(ith,ithmax)
              ainc=1.
              alat=abs(data1b4(ilat))
              if(alat .gt. 30. .and. cosfilter)ainc=cos(dg2rad*(alat-30.))
              aix=aix+ainc
              if(after_1_4_1998) then
               idate5(1) = data1b4(3) !year                          ! after  1/04/1998
               idate5(2) = data1b4(4) !month                         ! after  1/04/1998
               idate5(3) = data1b4(5) !day                           ! after  1/04/1998
               idate5(4) = data1b4(6)/3600.  !hour                   ! after  1/04/1998
               idate5(5) = (data1b4(6)-idate5(4)*3600)/60 !minute    ! after  1/04/1998
               isc = data1b4(6)-idate5(4)*3600-idate5(5)*60  !second ! after  1/04/1998
              else
               idate5(1) = data1b4(1) !year                          ! before 1/04/1998
               idate5(2) = data1b4(2) !month                         ! before 1/04/1998
               idate5(3) = data1b4(3) !day                           ! before 1/04/1998
               idate5(4) = data1b4(4)        !hour                   ! before 1/04/1998
               idate5(5) = data1b4(5)                     !minute    ! before 1/04/1998
               isc = data1b4(6)                              !second ! before 1/04/1998
              end if
!             if(mype.eq.4) print*,'idate5 in read_nonbufsat=',idate5
!             if(ii.eq.10) print*,'idate5 in read_nonbufsat=',idate5
              call w3fs21(idate5,nmind)
              sstime=float(nmind) + float(isc)/60.
              tdiff=sstime-gsstm
              rlat=data1b4(ilat)
              rlon=data1b4(ilon)
              if(rlon<0.)rlon=rlon+360.
              ifov          = nint(data1b4(inadir))
              panglr=(start+(ifov-1)*step)*dg2rad
              data1b(1,ith) = data1b4(1)             ! satellite ID
              data1b(2,ith) = tdiff                  ! time
              data1b(3,ith) = rlat                   ! lat
              data1b(4,ith) = rlon                   ! long
!             if(mype.eq.4) then
!             print*,'ii,ith,satfile(ii)=',ii,ith,satfile(ii)
!             print*,'rlat=',data1b(3,ith)
!             print*,'rlon=',data1b(4,ith)
!             endif

              if(hirs2 .or. msu)then
               data1b(5,ith) = asin(rato*sin(panglr))
              else
               data1b(5,ith) = real(data1b4(ilza))*dg2rad   ! local zenith angle
               if(amsua .and. ifov .le. 15) data1b(5,ith)=-data1b(5,ith)
               if(amsub .and. ifov .le. 45) data1b(5,ith)=-data1b(5,ith)
               if(hirs3 .and. ifov .le. 28) data1b(5,ith)=-data1b(5,ith)
              end if
              data1b(6,ith) = panglr                 ! look angle
              data1b(7,ith) = data1b4(inadir)+.001   ! scan position
              data1b(8,ith) = data1b4(isza)          ! solar zenith angle
              if(after_1_4_1998) then
               data1b(9,ith) = data1b4(ihgt)          ! station height
               data1b(10,ith)= data1b4(ils)+.001      ! land sea mask
              else
               data1b(9,ith) = 0.                     !  height not available, so set to zero, and
                                                      !   substitute model surface height later.
               data1b(10,ith)=0                       !   land sea mask not available--so assume sea (=0)
              end if
              do nn=1,nchanl
               data1b(nn+10,ith) = data1b4(nn+nreal)
              end do
            end do
            aix=aix-float(isatthin(ii))
 180        continue
            nread  = nread + ith
!           if(mype.eq.4) then
!           if(ii.eq.10) then
!            print*,'satfile(ii)=',satfile(ii)
!            print*,'ith=',ith
!            print*,'nrec,npes,mod(nrec,npes)=',nrec,npes,mod(nrec,npes)
!           endif
            if(ith .eq. 0)go to 120
!      from here on only process each rec once, spinning off data equally to all processors

     if(mod(nrec,npes).eq.mype) then

!         do time and space window check
       icountth=0
       ith0=0
       do i=1,ith
        iflagg(i)=0
        tdiff=data1b(2,ith)
!       if(mype.eq.4) then
!       if(ii.eq.10) then
!         print*,'satfile(ii)=',satfile(ii)
!         print*,'idate5 in read_nonbufsat=',idate5
!         print*,'tdiff,delhour,delhour*60=',tdiff,delhour,delhour*60
!       endif
        if(tdiff.ge.-delhour*60..and.tdiff.le.delhour*60.) then
         rlon=data1b(4,i) ; rlat=data1b(3,i)
         call tllv(rlon,rlat,erlon0,dg2rad,cerlat0,serlat0,rlong(i),rlatg(i),1)
!           space window
!        if(mype.eq.4) then
!         print*,'satfile(ii)=',satfile(ii)
!         print*,'rlat,rlon=',rlat,rlon
!         print*,'rlong(i),rlatg(i)=',rlong(i),rlatg(i)
!         print*,'rlongmin,rlongmax=',rlongmin,rlongmax
!         print*,'rlatgmin,rlatgmax=',rlatgmin,rlatgmax
!        endif
         if(rlong(i).gt.rlongmin.and.rlong(i).lt.rlongmax.and. &
                  rlatg(i).gt.rlatgmin.and.rlatg(i).lt.rlatgmax) then
          ith0=i
          iflagg(i)=1
         end if
        end if
        icountth=icountth+iflagg(i)
       end do
       if(icountth.gt.0.and.ith0.ne.0) then

        ithout=ith0

!           Set scan position for MSU data
            iscanpos = 0
            sch8flg=0.
            if (msu) then
               iscanpos = nint(data1b(7,ith0))
            else
              if(hirs2 .or. hirs3)then
               sch8flg=999999.
               ch8max=-100.
               ithx=ith0
               ithx2=ith0
               do i=1,ith
                if(iflagg(i).eq.1) then
                 call get_sstx_alns(sstx,alns,rlatg(i),rlong(i),iordcheck,lbig2check,lhalfcheck, &
                               imetaglb,jmetaglb,wbglb,dlon0,sbglb,dlat0,istaghglb)
                 ch8flg = data1b(ich8,i)-sstx
                 ch8ch10 = data1b(ich8,i)-data1b(ich10,i)
                 if(isatid(ii) .eq. 11)ch8flg=ch8flg-2.19955+1.28578*ch8ch10
                 if(isatid(ii) .eq. 14)ch8flg=ch8flg-2.61089+1.32519*ch8ch10
                 if(isatid(ii) .eq. 15)ch8flg=ch8flg-1.85483+1.30573*ch8ch10
                 if(isatid(ii) .eq. 16)ch8flg=ch8flg-1.85483+1.30573*ch8ch10
                 crit=abs(ch8flg)+abs(data1b(9,i))+20.*alns
                 if(crit .lt. sch8flg )then
                  ithx=i
                  sch8flg=crit
                 end if
      !            write(0,*)' mype,isatid,crit,sstx,alns,ch8,ch10=',mype,isatid(ii), &
      !                                                crit,sstx,alns,data1b(ich8,i),data1b(ich10,i)
                 if(data1b(ich8,i) .gt. ch8max)then
                   ithx2=i
                   ch8max=data1b(ich8,i)
                 end if
                end if
               end do
               if(sch8flg .lt. 20.)then
                ithout=ithx
               else
                ithout=ithx2
               end if
              else
                if(amsua .or. amsub)then
                  ithx=ith0
!                 za=sin(dg2rad*data1b(ilza,ithx))
!                 fact=sqrt(1.-za*za)
!                 ch15ch2s=fact*(data1b(ich15,ithx)-data1b(ich2,ithx))
!                 do i=1,ith
!                  za=sin(dg2rad*data1b(ilza,i))
!                  fact=sqrt(1.-za*za)
!                  ch15ch2=fact*(data1b(ich15,i)-data1b(ich2,i))
!                  if(ch15ch2 .gt. ch15ch2s + float(mod(i+ithin(ii)-ithx+ithin(ii)/2,ithin(ii))))then
!                    ithx=i
!                    ch15ch2s=ch15ch2
!                  end if
!                 end do
                  ithout=ithx
                end if
              end if
            end if
!
!           Remove MSU scan positions 1 and 11.
!           These are the outermost MSU scan positions.
        if ( iscanpos/=1 .and. iscanpos/=11) then


!
!
!              Increment data counter.  Write out data.
         if(mbufrad+1.gt.nbufrad) call increase_allrad(mbufrad,nbufrad)
         mbufrad=mbufrad+1
  !         write(0,*)' mype,ith0,ithout,iscanpos,mbufrad,nbufrad=',mype,ith0,ithout,iscanpos,mbufrad,nbufrad
         labelrad=labelrad-1_8
         if(mbufrad.le.nbufrad) then
          allrad(:,mbufrad)=0.
          allrad(:n1data,mbufrad)=data1b(:n1data,ithout)
          allrad(43,mbufrad)=ii
!         if(mype.eq.4) then
!         print*,'ii,rlat,rlon=',ii,rlat,rlon
!         endif
          allrad(44,mbufrad)=rlatg(ithout)
          allrad(45,mbufrad)=rlong(ithout)
          allrad(46:47,mbufrad)=rlabelrad(1:2)
         end if
        end if        !    end block over scan positions

       end if         !    end block over space-time window
     end if           !    end block for each processor
    end do            !  end loop over all records for this satellite

!         Jump here when read EOF

    120 continue
     if(mype.eq.0) print *,' eof reading file in read_nonbufsat, file,nread=',satfile(ii),nread
     go to 140

!      Jump here when a problem is detected reading 1b header record.

    130 continue
      if(mype.eq.0) print *,' error or bad 1b file in read_nonbufsat, file,nread=',satfile(ii),nread
    140 continue
    close (lunin)

   end if          !     end test of whether or not we have TOVS 1b file

  end do          !   end loop over different satellites

           if(mype.eq.0) print *,' delhour=',delhour

return
end subroutine read_nonbufsat
subroutine interrogate_1b(lunin,data1b8,after_1_4_1998,ils,inadir,ilat,ilon,ilza,isza,ihgt,n1data,nreal,nchanl)

  real(4) data1b8(*)
  logical after_1_4_1998

!  attempt to read first record of 1b file.  If get error or eof then must be format from after 1/04/1998

  ilat = 9
  ilon = 10
  isza = 12
  n1data=35
  rewind lunin
  read(lunin,end=120,err=120)(data1b8(k),k=1,n1data)
     after_1_4_1998=.false.
     nreal=15
     nchanl=19
     ils = 11              ! before 1/04/1998
     inadir = 15           ! before 1/04/1998
     ilza = 13             ! before 1/04/1998
     ihgt = 14             ! before 1/04/1998
     rewind lunin
     return
  
120 continue
  rewind lunin
  n1data=0
     after_1_4_1998=.true.
     ils=7                 ! after  1/04/1998
     inadir = 8            ! after  1/04/1998
     ilza = 11             ! after  1/04/1998
     ihgt = 13             ! after  1/04/1998
  read(lunin,end=130,err=130)nreal,nchanl
  n1data=nreal+nchanl
130 continue
  return

end subroutine interrogate_1b
