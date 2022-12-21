program alpha
  REAL*4 x
  REAL*8 y, z
  REAL(selected_real_kind(6,35)) :: q
  

  x = 1./3.
  q = 1./3.
  r = 1.d0/3.d0
  z = 1./3.
  y = 1.d0/3.d0
  IF (x .eq. z) THEN
    print *,'z equal'
  else
    print *,'z not equal', z - x
  ENDIF
  IF (x .eq. y) THEN
    print *,'y equal'
  else
    print *,'y not equal', y - x
  ENDIF

  PRINT *,'r, q = ',r, q
  WRITE (*,9001) r, q
9001 FORMAT('r,q = ',2F14.11)

END 
