program imask2nc

  use netcdf

  implicit none

  integer, parameter :: nlon = 4320, nlat = 2160
  integer, parameter :: ndim1 = 1, ndim2 = 2, ndim3 = 3, ndim4 = 4

  integer, dimension(ndim2) :: dim2, corner2, edge2
  integer, dimension(ndim1) :: dim1, corner1, edge1

  real(kind=4) :: delxy = 1.00/12.00

     real(kind=4), dimension(nlon)      :: glon
     real(kind=4), dimension(nlat)      :: glat
  integer(kind=4), dimension(nlon,nlat) :: wet

  character(len=100) :: cdffile
  character(len= 64) :: varname, varunit, varlong

     real :: rval
  integer :: i,j,ij,ii,jj,xtdim,ytdim,xtid,ytid
  integer :: rc, ncid, datid,mval

  !----------------------------------------------------------------------

  do j = 1,nlat
   glat(j) = (90.00 - 1.0/24.0) - float(j-1)*delxy
  enddo
  do i = 1,nlon
   glon(i) = ( 0.00 + 1.0/24.0) + float(i-1)*delxy
  enddo
  print *,glon(1),glon(nlon)
  print *,glat(1),glat(nlat)

  !----------------------------------------------------------------------

  mval = 0
   wet = 1
  open(20,file="iceland.text")
  do ij = 1,nlon*nlat
   read(20,*)ii,jj,rval
   !i = (nlon-ii)+1
   !j = (nlat-jj)+1
   if(rval .gt. 0.0)wet(ii+1,jj+1) = 0
  enddo
  close(20)

  !----------------------------------------------------------------------
  cdffile = 'imask_5min.nc'

   rc = nf90_create(trim(cdffile), nf90_clobber, ncid)
   print *,'setting up ',trim(cdffile)

   rc = nf90_def_dim(ncid,    'lon_0',         nlon,     xtdim)
   rc = nf90_def_dim(ncid,    'lat_0',         nlat,     ytdim)

    dim1(1) = xtdim
    rc = nf90_def_var(ncid, 'lon_0',  nf90_float,          dim1,  xtid)
    rc = nf90_put_att(ncid, xtid,        'units',       'degrees_east')
    rc = nf90_put_att(ncid, xtid,    'long_name',          'Longitude')

    dim1(1) = ytdim
    rc = nf90_def_var(ncid, 'lat_0',  nf90_float,          dim1, ytid)
    rc = nf90_put_att(ncid, ytid,        'units',     'degrees_north')
    rc = nf90_put_att(ncid, ytid,    'long_name',          'Latitude')

    dim2(2) = ytdim
    dim2(1) = xtdim
    varname = 'icemask'
    varunit = '  '
    rc = nf90_def_var(ncid, trim(varname), nf90_int, dim2, datid)
    rc = nf90_put_att(ncid, datid,     'units', trim(varunit))
    rc = nf90_put_att(ncid, datid, 'missing_value', mval)
    rc = nf90_put_att(ncid, datid,    '_FillValue', mval)
    rc = nf90_enddef(ncid)

    rc = nf90_put_var(ncid,  xtid,  glon)
    rc = nf90_put_var(ncid,  ytid,  glat)

    rc = nf90_inq_varid(ncid,     'icemask',  datid)
    rc = nf90_put_var(ncid,           datid,    wet)
    print *,trim(nf90_strerror(rc))

    rc = nf90_close(ncid)

end program imask2nc

