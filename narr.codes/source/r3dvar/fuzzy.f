!						*****************
!						*   fuzzy.f90	*
!						*  PURSER 1999	*
!						*****************
!   Routines concerned with generation of independent Gaussian random numbers.
module isetidum
  integer iset,idum,iff
end module isetidum
 
subroutine greed(iarg)
use isetidum
iset=0; idum=100; iff=0
!  grab seed to initialize random number generator
! print'('' input integer random number seed'')'
! read(*,*)iran
  do i=0,10*(iarg+1)
   call random_number(x)
  end do
  iran=nint(1000000.*x)
  call speed(iran)
end subroutine greed

subroutine speed(iran)
integer, parameter :: nn=2
integer,dimension(nn):: jseed
jran=mod(iran,32778)
jseed(1)=jran+1
jseed(2)=jran**2-jran+1
call random_seed(put=jseed(1:nn))
do i=1,10                ! discard 	 
   call random_number(x) ! the first ten 	  
enddo		         ! "random" numbers
end subroutine speed

function gauss()
use isetidum
save
  if (iset==0) then
1    v1=2.*ran0()-1.
     v2=2.*ran0()-1.
     r=v1**2+v2**2
     if(r >= 1. .or. r==0.)go to 1
     fac=sqrt(-2.*log(r)/r)
     gset=v1*fac
     gauss=v2*fac
     iset=1
  else
     gauss=gset
     iset=0
  endif
end function gauss

function ran0()
  use isetidum
  save
  dimension v(97)
  if(idum < 0 .or. iff == 0)then
     iff=1
     idum=1
     do j=1,97
        call random_number(dum)
     enddo
     do j=1,97
        call random_number(v(j))
     enddo
     call random_number(y)
  endif
  j=1+int(97.*y)
  if(j > 97.or.j < 1)pause
  y=v(j)
  ran0=y
  call random_number(v(j))
end function ran0

