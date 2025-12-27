subroutine get_ijk(ijk,i,j,k,istart,iend,jstart,jend,kstart)

!  given absolute address ijk = i-istart+1 + 
!           (iend-istart+1)*( (j-jstart) + (jend-jstart+1)*(k-kstart) )

!  then compute i,j,k

  ijkm=ijk-1
  nx=iend-istart+1
  ny=jend-jstart+1
  nxy=nx*ny
  kadd=max(0,-ijkm/nxy)
  ijkm=ijkm+kadd*nxy
  km=ijkm/nxy
  k=km+kstart
  ijm=ijkm-nxy*km
  jm=ijm/nx
  j=jm+jstart
  im=ijm-jm*nx
  i=im+istart
  k=k-kadd

return
end
