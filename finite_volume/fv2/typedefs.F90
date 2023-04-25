MODULE types
  
  TYPE latpt
    REAL(SELECTED_REAL_KIND(7,30)) :: lat, lon
  END TYPE latpt

  TYPE cell
    REAL(SELECTED_REAL_KIND(7,30)) ::  u, v, eta, h, f
    REAL(SELECTED_REAL_KIND(7,30)) ::  left_len, right_len, bottom_len, top_len
    INTEGER, POINTER :: neighbors(:)
    TYPE(latpt) :: center
    INTEGER             :: count_neigh
    INTEGER             :: me
  END TYPE cell
  
  REAL(SELECTED_REAL_KIND(14,30)) :: pi 
  PARAMETER(pi = 3.1415926535898d0)
  REAL(SELECTED_REAL_KIND(14,30)) :: degperrd = pi/180.d0
  REAL(SELECTED_REAL_KIND(14,30)) :: rdperdeg = 180.d0/pi

  REAL(SELECTED_REAL_KIND(7,30)) :: omega = 7.292E-5
  REAL(SELECTED_REAL_KIND(4,30)) :: g_earth = 9.81
  REAL(SELECTED_REAL_KIND(6,30)) :: r_earth = 6371.2e3 ! meters

CONTAINS

  REAL FUNCTION lenmin(x)
  IMPLICIT none
  TYPE(cell) x
  REAL y
  y = 1.e10
  y = MIN(y, x%left_len)
  y = MIN(y, x%right_len)
  y = MIN(y, x%bottom_len)
  y = MIN(y, x%top_len)

  lenmin = y
  RETURN
  END FUNCTION lenmin

  SUBROUTINE alloc(x, n)
    IMPLICIT none
    TYPE(cell) :: x
    INTEGER count_neigh, n
    count_neigh  = n
    ALLOCATE(x%neighbors(count_neigh))
    RETURN
  END SUBROUTINE alloc

  SUBROUTINE dealloc(x)
    IMPLICIT none
    TYPE(cell) :: x
    DEALLOCATE(x%neighbors)
  END SUBROUTINE dealloc

  SUBROUTINE re_alloc(x, n)
    IMPLICIT none
    TYPE(cell) :: x
    INTEGER    :: tneighbors(n)
! create copy of current
! create neighbors of size n
! copy old ones in to the first slots of n
    INTEGER n

    ALLOCATE(x%neighbors(n))
    RETURN
  END SUBROUTINE re_alloc


END MODULE types
