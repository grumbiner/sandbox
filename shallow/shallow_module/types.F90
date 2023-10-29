MODULE types

  REAL(SELECTED_REAL_KIND(7,20)) :: g_earth, r_earth
  PARAMETER (g_earth = 9.81)
  PARAMETER (r_earth = 6371.2e3) 

  TYPE latpt 
    REAL lat, lon
  END TYPE latpt

  TYPE cell
    REAL u,v,h,f,eta
    REAL dx, dy ! dx_left, dx_right, dy_down, dy_up
    TYPE(latpt) :: center
  ENDTYPE

END MODULE types
