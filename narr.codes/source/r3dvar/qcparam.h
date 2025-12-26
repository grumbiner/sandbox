      real(4) b,pgq,pgpw,pgw,pgp,pgt,pgrad
      parameter(b=10.)        ! normalized distance for gross errors
      parameter(pgq=.002)     ! probability of gross error for specific humidity
      parameter(pgpw=.002)    ! probability of gross error for precipitable water
      parameter(pgp=.002)     ! probability of gross error for pressure (ht)
      parameter(pgw=.005)     ! probability of gross error for wind
      parameter(pgt=.007)     ! probability of gross error for temperature
      parameter(pgrad=.002)   ! probability of gross error for radiances
