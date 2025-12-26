subroutine smooth_restore(f,nz,nx,ny,rnorm,iter_restore,iter_smooth)

!   smooth_restore f to spread f out into extrapolation areas (defined by rnorm)

  real(4) f(nx,ny,nz),rnorm(nx,ny)

  real(4) work(max(nx,ny,nz))
  integer(1) mark(nx,ny),norm(nx,ny)


  do k=1,nz
   mark=0
   norm=nint(rnorm)
   do j=1,ny
    do i=2,nx
     im=i-1
     if(rnorm(i,j).lt..5.and.norm(im,j).ne.0.and.mark(i,j).eq.0) then
      f(i,j,k)=f(im,j,k)
      norm(i,j)=norm(im,j)
      mark(i,j)=1
     end if
    end do
    do i=nx-1,1,-1
     ip=i+1
     if(rnorm(i,j).lt..5.and.norm(ip,j).ne.0.and.mark(i,j).eq.0) then
      f(i,j,k)=f(ip,j,k)
      norm(i,j)=norm(ip,j)
      mark(i,j)=1
     end if
    end do
   end do
   do j=2,ny
    jm=j-1
    do i=1,nx
     if(rnorm(i,j).lt..5.and.norm(i,jm).ne.0.and.mark(i,j).eq.0) then
      f(i,j,k)=f(i,jm,k)
      norm(i,j)=norm(i,jm)
      mark(i,j)=1
     end if
    end do
   end do
   do j=ny-1,1,-1
    jp=j+1
    do i=1,nx
     if(rnorm(i,j).lt..5.and.norm(i,jp).ne.0.and.mark(i,j).eq.0) then
      f(i,j,k)=f(i,jp,k)
      norm(i,j)=norm(i,jp)
      mark(i,j)=1
     end if
    end do
   end do
  end do
  do k=1,nz
   do loop=1,iter_restore
    do i=1,nx
     work(1:ny)=f(i,1:ny,k)
     do j=1,ny
      jp=min(j+1,ny)
      jm=max(1,j-1)
      if(rnorm(i,j).lt..5) then
       f(i,j,k)=.5*work(j)+.25*(work(jp)+work(jm))
      end if
     end do
    end do
    do j=1,ny
     work(1:nx)=f(1:nx,j,k)
     do i=1,nx
      ip=min(i+1,nx)
      im=max(1,i-1)
      if(rnorm(i,j).lt..5) then
       f(i,j,k)=.5*work(i)+.25*(work(ip)+work(im))
      end if
     end do
    end do
   end do
  end do
  do loop=1,iter_smooth
   do k=1,nz
    do i=1,nx
     work(1:ny)=f(i,1:ny,k)
     do j=1,ny
      jp=min(j+1,ny)
      jm=max(1,j-1)
      f(i,j,k)=.5*work(j)+.25*(work(jp)+work(jm))
     end do
    end do
    do j=1,ny
     work(1:nx)=f(1:nx,j,k)
     do i=1,nx
      ip=min(i+1,nx)
      im=max(1,i-1)
      f(i,j,k)=.5*work(i)+.25*(work(ip)+work(im))
     end do
    end do
   end do
   do j=1,ny
    do i=1,nx
     work(1:nz)=f(i,j,1:nz)
     do k=1,nz
      kp=min(k+1,nz)
      km=max(1,k-1)
      f(i,j,k)=.5*work(k)+.25*(work(kp)+work(km))
     end do
    end do
   end do
  end do
  call refresh_grid(f,nx,ny,nz,2)

return
end subroutine smooth_restore
