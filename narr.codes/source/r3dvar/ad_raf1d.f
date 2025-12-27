SUBROUTINE ad_raf1d(g,filter,ixyz, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!  2nd half of recursive anisotropic self-adjoint filter (full-strings version)

  IMPLICIT NONE

  include 'mpif.h'
      include "my_comm.h"
  INCLUDE 'filtertype.h'

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices

  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  REAL(4), DIMENSION( ims:ime, jms:jme, kms:kme ), INTENT(INOUT) :: &
            g                      !  input--field to be filtered, output--filtered field

  TYPE(filter_cons) filter(14)           ! structure defining recursive filter

  integer(4) ixyz                         !   =1, then filter in x direction only
                                          !   =2, then filter in y direction only
                                          !   =3, then filter in z direction only

  real(4) work(min(ims,jms,kms):max(ime,jme,kme))

  integer(4) i,icolor,icolor2,ierr,im,ip,ipass,ipep1,ipsm1,ismooth,j,jm
  integer(4) jp,jpass,jpep1,jpsm1,k,km,kp,kpep1,kpsm1
  integer(4) im3,ip3,ipep3,ipsm3,jm3,jp3,jpep3,jpsm3,km3,kp3,kpep3,kpsm3


  if(filter(1)%npass.gt.0) then
   icolor=0
   if(ixyz.eq.1) icolor=filter(1)%icolorx
   if(ixyz.eq.2) icolor=filter(1)%icolory
   if(ixyz.eq.3) icolor=filter(1)%icolorz
   icolor2=icolor+7
   if(icolor.ne.0) then
    do ipass=filter(1)%npass,1,-1
     jpass=min(ipass,filter(1)%mpass)

     icolor=icolor2
     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color_loc(g,filter(icolor),jpass,filter(1)%no_interp,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
     icolor=icolor2-7
     if(filter(icolor)%npointsmaxall.gt.0) &
         call one_color(g,filter(icolor),jpass,filter(1)%no_interp,filter(1)%ifilt_ord, &
             filter(icolor)%nstrings,filter(icolor)%istart, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!      following barrier is required because there is no communication for icolor>=8--the call
!          to one_color_loc, and all work must end for a color, before moving to the next one

                                               !!!! DO NOT REMOVE THIS BARRIER !!!!!
     call mpi_barrier(my_comm,ierr)     !!!! DO NOT REMOVE THIS BARRIER !!!!!
                                               !!!! DO NOT REMOVE THIS BARRIER !!!!!
    end do

   end if
  end if

!      apply 1-2-1 smoother in each direction

  if(filter(1)%nsmooth.gt.0.and.ixyz.eq.1) then
   ipsm1=max(ids,ims,ips-1) ; ipep1=min(ide,ime,ipe+1)
   do ismooth=1,filter(1)%nsmooth
    call refresh_halo3x(g,1, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info
    do k=kps,kpe
     do j=jps,jpe
      work(ipsm1:ipep1)=g(ipsm1:ipep1,j,k)
      do i=ips,ipe
       ip=min(i+1,ipep1) ; im=max(ipsm1,i-1)
       g(i,j,k)=.25*(work(ip)+work(im))+.5*work(i)
      end do
     end do
    end do
   end do
  end if
  if(filter(1)%nsmooth.gt.0.and.ixyz.eq.2) then
   jpsm1=max(jds,jms,jps-1) ; jpep1=min(jde,jme,jpe+1)
   do ismooth=1,filter(1)%nsmooth
    call refresh_halo3y(g,1, &
               ids, ide, jds, jde, kds, kde, &         ! domain indices
               ips, ipe, jps, jpe, kps, kpe,  &         ! patch indices
               ims, ime, jms, jme, kms, kme, &         ! memory indices
               inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j )     ! processor info
    do k=kps,kpe
     do i=ips,ipe
      work(jpsm1:jpep1)=g(i,jpsm1:jpep1,k)
      do j=jps,jpe
       jp=min(j+1,jpep1) ; jm=max(jpsm1,j-1)
       g(i,j,k)=.25*(work(jp)+work(jm))+.5*work(j)
      end do
     end do
    end do
   end do
  end if
  if(filter(1)%nsmooth.gt.0.and.ixyz.eq.3) then
   kpsm1=max(kds,kms,kps-1) ; kpep1=min(kde,kme,kpe+1)
   do ismooth=1,filter(1)%nsmooth
    do j=jps,jpe
     do i=ips,ipe
      work(kpsm1:kpep1)=g(i,j,kpsm1:kpep1)
      do k=kps,kpe
       kp=min(k+1,kpep1) ; km=max(kpsm1,k-1)
       g(i,j,k)=.25*(work(kp)+work(km))+.5*work(k)
      end do
     end do
    end do

   end do
  end if

return
end subroutine ad_raf1d
