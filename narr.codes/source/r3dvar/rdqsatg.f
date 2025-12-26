subroutine rdqsatg(qsatges,imeta,jmeta,lmetaex)

!       transfer array qsatges from common
!          (replaces reading from disk)

  include 'types.h'
  include "r3dv_data.comm"

  real(4) qsatges(imeta,jmeta,lmetaex)


! if(npcom.ne.lmetaex) then
!  print *,' problem in rdqsatg, lmetaex,npcom=',lmetaex,npcom
!  stop
! end if

  do k=1,lmetaex
   do j=1,jmeta
    do i=1,imeta
     qsatges(i,j,k)=qsatgescom(i,j,k)
    end do
   end do
  end do

return
end subroutine rdqsatg

