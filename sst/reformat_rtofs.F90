PROGRAM alpha
  INTEGER nx, ny
  PARAMETER (nx = 4500)
  PARAMETER (ny = 3298)
  REAL lats(nx, ny), lons(nx, ny), salts(nx, ny)
  
  INTEGER i,j
  
  OPEN(10, FILE="textout", FORM="FORMATTED", STATUS="OLD")
  DO j = 1, ny
    DO i = 1, nx
      READ (10,*) lons(i,j), lats(i,j), salts(i,j)
    ENDDO
    WRITE(*,*) j, MAXVAL(lons(:,j)), MINVAL(lons(:,j)), &
    MAXVAL(lats(:,j)), MINVAL(lats(:,j)), MAXVAL(salts(:,j)), &
    MINVAL(salts(:,j)) 
  ENDDO

END
