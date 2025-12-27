subroutine wrqsatg(qsatges,imeta,jmeta,lmetaex)

!       transfer array qsatges to common for later use
!          (replaces writing to disk)

  include 'types.h'
  include "r3dv_data.comm"

  real(4) qsatges(imeta,jmeta,lmetaex)

  allocate(qsatgescom(imeta,jmeta,lmetaex))
  npcom=lmetaex
  do k=1,lmetaex
   do j=1,jmeta
    do i=1,imeta
     qsatgescom(i,j,k)=qsatges(i,j,k)
    end do
   end do
  end do

return
end subroutine wrqsatg

