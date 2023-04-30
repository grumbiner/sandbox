PROGRAM alpha

  USE astronomy
  USE time_series
  USE harmonics

  TYPE (series) :: radius
  TYPE (harmonic_set) :: harmonic

  INTEGER i

!---- Begin executable -----------------------------------------
  PRINT *,'anomal ',omega_e_anomalistic

!---- set up dummy time series ---------------------------------
  radius%nx     = 3655
  radius%dt     = 1.0
  ALLOCATE(radius%x(radius%nx    ))

  DO i = 1, radius%nx    
    radius%x(i) = i
  ENDDO
  radius%x = radius%x - radius%nx/2.0 - 0.5
  PRINT *,'max min radius ',maxval(radius%x), minval(radius%x)

!---- set up simple harmonic set ------------------------------
  harmonic%nfreqs = 600
  ALLOCATE(harmonic%freqs(harmonic%nfreqs), harmonic%acos(harmonic%nfreqs), &
           harmonic%asin(harmonic%nfreqs)    )
  DO i = 1, harmonic%nfreqs
    harmonic%freqs(i) = i * 2.*PI/DBLE(radius%nx)
    harmonic%acos(i)  =   1.0   / DBLE(i)
    harmonic%asin(i)  = -1163.4 / DBLE(i) 
  ENDDO

!---- start doing some standardish analysis -------------------
  CALL demod(radius, harmonic)
  DO i = 1, radius%nx
    PRINT *,i, radius%x(i)
  ENDDO

END
