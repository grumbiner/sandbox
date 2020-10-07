!==========================================================================
PROGRAM alpha
  USE things
  IMPLICIT none

  INTEGER nx, ny, nz
  PARAMETER (nx = 32)
  PARAMETER (ny = 32)
  PARAMETER (nz = 32)

! arguments to iterate:
  INTEGER npts
  PARAMETER (npts = nx*ny*nz)
  TYPE(thing) bodies(npts)
  REAL(crystal_kind) :: dx, dy, dz, dt, m
  INTEGER ref, nsteps, outfreq
  TYPE(triplet) initial_loc

! arguments for scan
  REAL(crystal_kind) :: dt_suggest, fraction, minpot
  INTEGER active(npts), del_index

! working
  INTEGER i, j, k, count

! cubic crystal (Angstroms)
  dx = 3.7e-10
  dy = 3.7e-10
  dz = 3.7e-10
  count = 1
  DO k = 1, nz
  DO j = 1, ny
  DO i = 1, nx
    CALL initialize_thing(bodies(count))
    bodies(count)%x%x = dx*MOD((i-1),nx)
    bodies(count)%x%y = dy*MOD((j-1),ny)
    bodies(count)%x%z = dz*MOD((k-1),nz)
    count = count + 1
  ENDDO
  ENDDO
  ENDDO

!  ref = nx/2 + (ny/2*nx) + npts / 2     ! middle of chip
  ref = nx/2 + (ny/2*nx) +   0          ! center of face
!  ref = nx/2 + (   0*nx) +   0          ! middle of edge
!  ref = 1 + (0*ny) +    0               ! corner

  bodies(ref)%x%x =  bodies(ref)%x%x + 0.00*dx
  bodies(ref)%x%y =  bodies(ref)%x%y + 0.00*dy
  bodies(ref)%x%z =  bodies(ref)%x%z + 0.00*dz

  m        = 1.66e-27*12.   ! in kg, 1 amu, ~1GeV
  fraction = 3.16e-4
  DO ref = 1, npts
    CALL scan(bodies, npts, ref, fraction, minpot, active, count, &
        dx, m, dt_suggest)
    PRINT *,'ref, dt_suggest, count ',ref, dt_suggest, count
  ENDDO

  STOP

  dt  = 1.e-14
  outfreq = MAX(1, INT(0.5 + 1.0e-13 / dt))
  nsteps  = 1.5e-09 / dt
  PRINT *,'dt, outfreq, nsteps, millions steps = ',dt, outfreq, nsteps, &
     nsteps/1e6

  initial_loc = bodies(ref)%x
  CALL iterate(outfreq, nsteps, m, dt, dx, dy, dz, ref, bodies, npts) 

END PROGRAM alpha 
!====================================================================
