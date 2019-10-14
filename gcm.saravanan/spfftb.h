C Common module for f.f.t. table used by sptransforms
      INTEGER NFFTAB
      REAL FFTAB
      PARAMETER (NFFTAB=256)
      COMMON/SPFFTB/ FFTAB(4,2,NFFTAB)
