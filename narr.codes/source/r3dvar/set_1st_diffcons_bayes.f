subroutine set_1st_diffcons_bayes(iddaq,iddapsi,iddachi,iddap,iddat,dda,dlambda,nlambda)

!    initialize differencing indices and constants

  integer(4) iddaq(3,2,nlambda),iddapsi(3,2,nlambda),iddachi(3,2,nlambda)
  integer(4) iddap(2,2,nlambda),iddat(3,2,nlambda)
  real(4) dda(2,nlambda),dlambda(nlambda)

!

  iddaq=1
  iddaq(2,1,3)=2
  iddaq(2,2,3)=1
  iddaq(3,1,7)=2
  iddaq(3,2,7)=1 
  iddaq(1,1,12)=2
  iddaq(1,2,12)=1 

  iddapsi=1
  iddapsi(2,1,4)=2
  iddapsi(2,2,4)=1 
  iddapsi(3,1,8)=2
  iddapsi(3,2,8)=1 
  iddapsi(1,1,13)=2
  iddapsi(1,2,13)=1 

  iddachi=1
  iddachi(2,1,5)=2
  iddachi(2,2,5)=1 
  iddachi(3,1,9)=2
  iddachi(3,2,9)=1 
  iddachi(1,1,14)=2
  iddachi(1,2,14)=1 

  iddap=1
  iddap(2,1,1)=2
  iddap(2,2,1)=1 
  iddap(1,1,10)=2
  iddap(1,2,10)=1 

  iddat=1
  iddat(2,1,2)=2
  iddat(2,2,2)=1 
  iddat(3,1,6)=2
  iddat(3,2,6)=1 
  iddat(1,1,11)=2
  iddat(1,2,11)=1 

  do i=1,nlambda
   dda(1,i)=1./dlambda(i)
   dda(2,i)=-dda(1,i)
  end do

return
end subroutine set_1st_diffcons_bayes
