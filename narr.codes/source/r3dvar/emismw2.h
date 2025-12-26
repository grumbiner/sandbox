      real(8) emc(59)
      data emc/&
       .175350E+02, -.617670E+00,  .894800E-02,  .318420E+01,&
       .191890E-01, -.108730E-01,  .258180E-03,  .683960E+02,&
      -.406430E+00,  .228320E-01, -.530610E-03,  .476290E+01,&
       .154100E+00, -.337170E-01,  .844280E-03,  .782870E+02,&
      -.434630E-02,  .531250E+01, -.114770E-01,  .314159E+01,&
      -.100000E+01,  .195000E-04,  .255000E+01, -.637182E+01,&
       .253918E-01,  .357569E-04,  .942928E+01, -.332839E-01,&
      -.647724E-04, -.329282E+01,  .965450E-02,  .281588E-04,&
       .252676E+00,  .343867E-02, -.156362E-04, -.156669E-03,&
       .139485E-04, -.407633E-07, -.141316E+00, -.356556E-02,&
       .142869E-04, -.240701E+01, -.563888E-01,  .325227E-03,&
       .296005E+01,  .704675E-01, -.426440E-03, -.751252E+00,&
      -.191934E-01,  .125937E-03, -.288253E+00, -.102655E-02,&
       .226701E-05, -.119072E-02, -.263165E-04,  .114597E-06,&
       .406300E+00,  .200031E-02, -.781635E-05/
! changed coefficients 19 Nov.
!     & 17.535    ,    -0.61767 ,   0.008948   , 3.1842,
!     & 0.019189  ,    -0.010873,   0.00025818 , 68.396,
!     & -0.40643  ,    0.022832 ,   -0.00053061, 4.7629, 
!     & 0.1541    ,    -0.033717,   0.00084428 , 78.287,
!     & -0.0043463,    5.3125   ,   -0.011477  , 3.141592654,
!     & -1.0      ,    1.95E-5  ,    2.55     ,
!     & -2.35188  ,    -0.132072,    0.000658883,
!     & 3.44125   ,  0.187539   , -0.000931400,
!     & -1.14758  , -0.0626640  ,  0.000308404,
!     & -0.232072 ,  0.0119520  , -4.01439e-05,
!     & 0.00549094,  1.51716e-06, -9.59848e-09,
!     & 0.238199  , -0.00881164 ,  2.71126e-05,
!     & 11.6962   , -0.346877   ,  0.00121357,
!     & -15.9742  ,  0.443442   , -0.00156718,
!     & 5.11736   , -0.127518   ,  0.000457518,
!     & -0.773612 ,  0.000775719,  1.80312e-06,
!     & 0.0103726 , -0.000133524,  3.65630e-07,
!     & 0.625144  ,  0.00625598 , -2.56224e-05/
!
!Coefficients post-April 24 change
!     & -2.35188  ,    -0.132072,    0.000658883,
!     & 3.44125   ,  0.187539   , -0.000931400,
!     & -1.14758  , -0.0626640  ,  0.000308404,
!     & -0.232072 ,  0.0119520  , -4.01439e-05,
!     & 0.00549094,  1.51716e-06, -9.59848e-09,
!     & 0.238199  , -0.00881164 ,  2.71126e-05,
!     & 11.6962   , -0.346877   ,  0.00121357,
!     & -15.9742  ,  0.443442   , -0.00156718,
!     & 5.11736   , -0.127518   ,  0.000457518,
!     & -0.773612 ,  0.000775719,  1.80312e-06,
!     & 0.0103726 , -0.000133524,  3.65630e-07,
!     & 0.625144  ,  0.00625598 , -2.56224e-05/

! Coefficients pre-April 24 change
!                                                   -.271499E+01,
!     & -.285110E-01,   .178220E-03,   .394354E+01,  .433341E-01,
!     & -.262158E-03,  -.126083E+01,  -.160313E-01,   .910478E-04,
!     & -.609792E-01,   .368813E-02,  -.128188E-04,   .545718E-02,
!     & -.138377E-04,  -.192805E-09,   .980869E-01,  -.325372E-02,
!     & .103577E-04,   .445209E+01,  -.804326E-03,   .493464E-04,
!     & -.669273E+01,  -.798760E-02,  -.327148E-04,   .253287E+01,
!     & .700837E-02,  -.837905E-05,  -.519445E+00,  -.304075E-02,
!     & .140881E-04,   .690632E-02,  -.116684E-04,  -.123170E-07,
!     & .504731E+00,   .337427E-02,  -.160390E-04/
!
! Explanation:
! FreqGHz: Observation frequency in GHz
! Angdeg: local zenith angle
! Ci: cosine of local zenith angle
! CiCi: cosine squared of local zenith angle
! SiSi: sine squared of local zenith angle
! emc(38): Emissivity model data
! Permittivity model data (Lamkaouchi model)
!   [1-3]: Temperature polynomial coefficients for Tau1 - Lamkaouchi (1996)
!   [4-7]: Temperature polynomial coefficients for Tau2 - Lamkaouchi (1996)
!  [8-11]: Temperature polynomial coefficients for Del1 - Lamkaouchi (1996)
! [12-15]: Temperature polynomial coefficients for Del2 - Lamkaouchi (1996)
! [16-17]: Temperature polynomial coefficients for static permittivity - Lamkaouchi (1996)
! [18-19]: Temperature polynomial coefficients for infinite freq. permittivity - Lamkaouchi (1996)
! Pi is stored for good measure
!    [20]: Stored value of Pi  - temporary, use RTTOV pi when available.
! Large scale correction model version 1: This does *NOT* correct for
! hemispherical scattering and is *NO LONGER USED*
!    [21]: Angle coefficient for large scale correction - see English (1997)
!    [22]: Windspeed coefficient for large scale correction - see English (1997)
!    [23]: Constant for large scale correction - see English (1997)
!    [24]: Reference frequency for large scale correction - see English (1997)
!    [25]: Normalisation frequency for large scale correction - see English (1997)
!    [26]: Scaling factor for large scale correction - see English (1997)
! Bragg scattering correction coefficients
!    [27]: Scaling factor for small scale correction - see English (1997)
! Foam model coefficients for Monahan model
!    [28]: First coefficient in Monahan foam model (neutral stability)  - see English (1997)
!    [29]: Second coefficient in Monahan foam model (neutral stability) - see English (1997)
! Alternative permittivity model (Liebe)
!    [30]: a1 in Liebe's dielectric model - see Liebe (1989)
!    [31]: b1 in Liebe's dielectric model - see Liebe (1989)
!    [32]: c1 in Liebe's dielectric model - see Liebe (1989)
!    [33]: c2 in Liebe's dielectric model - see Liebe (1989)
!    [34]: d1 in Liebe's dielectric model - see Liebe (1989)
!    [35]: d2 in Liebe's dielectric model - see Liebe (1989)
!    [36]: d3 in Liebe's dielectric model - see Liebe (1989)
!    [37]: e1 in Liebe's dielectric model - see Liebe (1989)
!    [38]: e2 in Liebe's dielectric model - see Liebe (1989)
! Version 2 of large scale correction which *DOES»* take account of
! hemispherical scattering.
! 1.) Mixed polarisation mode (nominal V at nadir)
!    [39]: Term a00 in mixed pol of large scale correction model
!    [40]: Term a01 in mixed pol mode of large scale correction model
!    [41]: Term a02 in mixed pol mode of large scale correction model
!    [42]: Term a10 in mixed pol mode of large scale correction model
!    [43]: Term a11 in mixed pol mode of large scale correction model
!    [44]: Term a12 in mixed pol mode of large scale correction model
!    [45]: Term a20 in mixed pol mode of large scale correction model
!    [46]: Term a21 in mixed pol mode of large scale correction model
!    [47]: Term a22 in mixed pol mode of large scale correction model
!    [48]: Term a30 in mixed pol mode of large scale correction model
!    [49]: Term a31 in mixed pol mode of large scale correction model
!    [50]: Term a32 in mixed pol mode of large scale correction model
!    [51]: Term a40 in mixed pol mode of large scale correction model
!    [52]: Term a41 in mixed pol mode of large scale correction model
!    [53]: Term a42 in mixed pol mode of large scale correction model
! 2.) Vertical polarisation mode
!    [54]: Term a00 in vertical pol mode of large scale correction model
!    [55]: Term a01 in vertical pol mode of large scale correction model
!    [56]: Term a02 in vertical pol mode of large scale correction model
!    [57]: Term a10 in vertical pol mode of large scale correction model
!    [58]: Term a11 in vertical pol mode of large scale correction model
!    [59]: Term a12 in vertical pol mode of large scale correction model
!    [60]: Term a20 in vertical pol mode of large scale correction model
!    [61]: Term a21 in vertical pol mode of large scale correction model
!    [62]: Term a22 in vertical pol mode of large scale correction model
!    [63]: Term a30 in vertical pol mode of large scale correction model
!    [64]: Term a31 in vertical pol mode of large scale correction model
!    [65]: Term a32 in vertical pol mode of large scale correction model
!    [66]: Term a40 in vertical pol mode of large scale correction model
!    [67]: Term a41 in vertical pol mode of large scale correction model
!    [68]: Term a42 in vertical pol mode of large scale correction model 
! 3. ) Horizontal polarisation mode
!    [69]: Term a00 in horizontal pol mode of large scale correction model
!    [70]: Term a01 in horizontal pol mode of large scale correction model
!    [71]: Term a02 in horizontal pol mode of large scale correction model
!    [72]: Term a10 in horizontal pol mode of large scale correction model
!    [73]: Term a11 in horizontal pol mode of large scale correction model
!    [74]: Term a12 in horizontal pol mode of large scale correction model
!    [75]: Term a20 in horizontal pol mode of large scale correction model
!    [76]: Term a21 in horizontal pol mode of large scale correction model
!    [77]: Term a22 in horizontal pol mode of large scale correction model
!    [78]: Term a30 in horizontal pol mode of large scale correction model
!    [79]: Term a31 in horizontal pol mode of large scale correction model
!    [80]: Term a32 in horizontal pol mode of large scale correction model
!    [81]: Term a40 in horizontal pol mode of large scale correction model
!    [82]: Term a41 in horizontal pol mode of large scale correction model
!    [83]: Term a42 in horizontal pol mode of large scale correction model
!    [84]: Windspeed coefficient in mixed polarisation high U, theta correction
!    [85]: View angle coefficient in mixed polarisation high U, theta correction
!    [86]: Constant coefficient in mixed polarisation high U, theta correction
!    [87]: Windspeed coefficient in vertical polarisation high U, theta correction
!    [88]: View angle coefficient in vertical polarisation high U, theta correction
!    [89]: Constant coefficient in vertical polarisation high U, theta correction
!    [90]: Windspeed coefficient in horizontal polarisation high U, theta correction
!    [91]: View angle coefficient in horizontal polarisation high U, theta correction
!    [92]: Constant coefficient in horizontal polarisation high U, theta correction
