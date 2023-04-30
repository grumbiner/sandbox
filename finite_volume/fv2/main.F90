PROGRAM finite_volume
  USE types
  IMPLICIT none

  REAL(SELECTED_REAL_KIND(7,30)) :: tlat, tlon
  REAL(SELECTED_REAL_KIND(7,30)) :: dlat, dlon, firstlat, firstlon
  PARAMETER (dlat = 1.0)
  PARAMETER (dlon = 1.0)
  PARAMETER (firstlat = -90.0 + dlat / 2.)
  PARAMETER (firstlon = dlon / 2.)
  INTEGER nx, ny
  PARAMETER (nx = 360)
  PARAMETER (ny = 180)

  TYPE(cell)  :: field(nx*ny)

  INTEGER i, j, index

  index = 1
  DO j = 1, ny
    tlat = firstlat + (j - 1)*dlat
    DO i = 1, nx
      tlon = firstlon + (i-1)*dlon
      field(index)%h = 4000.
      field(index)%u =    0.
      field(index)%v =    0.
      field(index)%eta =  0.
      field(index)%center%lat = tlat
      field(index)%center%lon = tlon
      field(index)%f          = 2. * omega * sin(tlat*degperrd)

      field(index)%me         = index
      field(index)%left_len   = r_earth * pi/2 * (dlat/90.)
      field(index)%right_len  = r_earth * pi/2 * (dlat/90.)
      field(index)%bottom_len = r_earth * (2.*pi)/360. *dlon * cos((tlat-dlat/2.)*degperrd)
      field(index)%top_len    = r_earth * (2.*pi)/360. *dlon * cos((tlat+dlat/2.)*degperrd)
      CALL alloc(field(index),4) !neighbors 
! 2 along top and bottom, ultimately 6 or more for convergence of meridians
      index = index + 1
    ENDDO
  ENDDO
! revindex to take latpt and return index
! fn(latpt q, allcenters(indexmax))

  DO i = 1, nx*ny
    PRINT *,i,lenmin(field(i)),field(i)%left_len, field(i)%right_len, field(i)%bottom_len, field(i)%top_len
  ENDDO


END PROGRAM finite_volume
