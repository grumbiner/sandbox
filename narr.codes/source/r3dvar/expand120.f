subroutine expand120(nbufdat,mbufdat,mbufdat_out,delpmin_fill,mype)

!  fill in type 120 obs between sig-levels.
!    this is justified because the observed quantities are linear in log(p) between
!       sig levels (this is how sig levels are defined)
!    give new data type 119, to avoid confusion with bill's qc bookkeeping

  include 'types.h'

  type(general_obs),pointer::alldata(:)
  integer(2),pointer::lev_val(:),lev_max(:)
  common/databuf/lev_val,lev_max,alldata

  integer(1),allocatable::mark(:)
  character(8) cstaid,cstaid0

  type(general_obs),allocatable::thisdata(:)
  integer(2),allocatable::levs_this(:)

  mbufdat_out=mbufdat
  allocate(mark(max(1,mbufdat)))
  if(mbufdat.gt.1) then
   allocate(thisdata(1000))
   allocate(levs_this(1000))

   levsmax=0
   numintmax=0
!  do igroup=1004,1006,2   ! 1004 is t, 1006 is q
   do igroup=1006,1006,2   ! 1004 is t, 1006 is q     !  do q only

    nbad=0
    mark=0
    do i=1,mbufdat-1
     if(mark(i).eq.0) then
     igroup0=nint(alldata(i)%group)
     if(igroup0.eq.igroup) then
      itype0=nint(alldata(i)%type)
      if(itype0.eq.120) then
       cstaid0=alldata(i)%staid
       time0=alldata(i)%time
       levs=0
       do j=i,mbufdat
        if(mark(j).eq.0) then
         jgroup=nint(alldata(j)%group)
         if(jgroup.eq.igroup0) then
          itype=nint(alldata(j)%type)
          if(itype.eq.itype0) then
           cstaid=alldata(j)%staid
           time=alldata(j)%time
           if(cstaid.eq.cstaid0.and.time.eq.time0) then
            mark(j)=1
            levs=levs+1
            thisdata(levs)=alldata(j)
            levs_this(levs)=lev_val(j)
            if(lev_val(j).ge.lev_max(j)) exit
           end if
          end if
         end if
        end if
       end do
       if(levs.gt.1) then
        levsmax=max(levs,levsmax)
        do j=1,levs-1
         pthis=exp(thisdata(j)%pressure)
         pnext=exp(thisdata(j+1)%pressure)
         if(pnext.ge.pthis) nbad=nbad+1
         if(pnext.lt.pthis-1.3*delpmin_fill.and.levs_this(j+1)-levs_this(j).eq.1.and. &
               pthis-pnext.lt.99.) then
          numint=(pthis-pnext)/delpmin_fill
          numintmax=max(numintmax,numint)
          wgt1=0.
          dwgt=1./(numint+1.)
          do k=1,numint
           wgt1=wgt1+dwgt
           if(mbufdat_out+1.gt.nbufdat) call increase_alldata(mbufdat_out,nbufdat)
           mbufdat_out=mbufdat_out+1
           if(mbufdat_out.le.nbufdat) then
            wgt0=1.-wgt1
            alldata(mbufdat_out)=thisdata(j)
            alldata(mbufdat_out)%type=119
            alldata(mbufdat_out)%lon=wgt0*thisdata(j)%lon+wgt1*thisdata(j+1)%lon
            alldata(mbufdat_out)%lat=wgt0*thisdata(j)%lat+wgt1*thisdata(j+1)%lat
            alldata(mbufdat_out)%long=wgt0*thisdata(j)%long+wgt1*thisdata(j+1)%long
            alldata(mbufdat_out)%latg=wgt0*thisdata(j)%latg+wgt1*thisdata(j+1)%latg
            alldata(mbufdat_out)%pressure=wgt0*thisdata(j)%pressure+wgt1*thisdata(j+1)%pressure
            alldata(mbufdat_out)%tobs=wgt0*thisdata(j)%tobs+wgt1*thisdata(j+1)%tobs
            if(igroup.eq.1006) &
             alldata(mbufdat_out)%qobs=wgt0*thisdata(j)%qobs+wgt1*thisdata(j+1)%qobs
            alldata(mbufdat_out)%label=-alldata(mbufdat_out)%label ! distinguish added obs
                                                                   ! from original obs
           end if
          end do
         end if
        end do
       end if
      end if
     end if
     end if
    end do
          if(mype.eq.0) print *,' in expand120, mype,igroup,nbad=',mype,igroup,nbad
   end do
     if(mype.eq.0) then
      print *,' in expand120, mype,nbufdat,mbufdat,mbufdat_out=',mype,nbufdat,mbufdat,mbufdat_out
      print *,' in expand120, mype,levsmax,numintmax,delpmin_fill=',mype,levsmax,numintmax,delpmin_fill
     end if

   deallocate(thisdata)
   deallocate(levs_this)
  end if

  deallocate(mark)


return
end subroutine expand120
