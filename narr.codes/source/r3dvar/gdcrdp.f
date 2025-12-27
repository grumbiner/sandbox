      subroutine gdcrdp(d,nd,x,nx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gdcrdp      get grid coords for monotonic increasing
!   prgmmr: parrish          org: w/nmc22    date: 90-10-11
!
! abstract: get grid coords for monotonic increasing points.
!
! program history log:
!   90-10-11  parrish
!   97-03-26  parrish   add test for equal spaced ref grid points to speed up
!
!   input argument list:
!     d,nd     - input points, number of input points.
!     x,nx     - values, number of reference grid points.
!
!   output argument list:
!     d        - converted to grid units.
!
! attributes:
!   language: cft77
!   machine:  cray ymp
!
!$$$


  real(4) d(nd),x(nx)

  integer(4) i,ix,id,isrchfge
  real(4) deldximax,deldxi,dt,reldel
  real(4),allocatable,dimension(:)::dxi
  real(4) zero4,one4
  zero4=0._8 ; one4=1._8

!-------- test to see if x points are equally spaced

         deldximax=zero4
  allocate(dxi(nx-1))
         dxi(1)=one4/(x(2)-x(1))
         do i=2,nx-1
          dxi(i)=one4/(x(i+1)-x(i))
          deldxi=abs(dxi(i)-dxi(1))
          deldximax=max(deldxi,deldximax)
         end do
         reldel=deldximax/dxi(1)
         if(reldel.le.1.e-6_8) then
!         print *,' gdcrdp found equally spaced ref points'

!---------we have equally spaced points, so can just divide by grid interval

          d=one4+(d-x(1))*dxi(1)
         else
!         print *,' gdcrdp working with non-uniform ref points'


!--------- we have general grid, so must test each point

      do 400 id=1,nd
        dt=d(id)
        if(dt .le. x(1))then
          d(id)=one4+(dt-x(1))/(x(2)-x(1))
        else
          ix=isrchfge(nx-1,x,1,dt)-1
          d(id)=float(ix)+(dt-x(ix))/(x(ix+1)-x(ix))
        end if
400   continue
         end if
      return
      end subroutine gdcrdp
