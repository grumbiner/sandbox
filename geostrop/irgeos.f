      SUBROUTINE irgeos(pswv, temp, height, ug, vg)
C     Bob Grumbine Adaptation of Mark Iredell program unit
C       in Maplauncher for computing geostrophic winds on a 
C       sigma surface.  5/14/92
C.......................................................................
C  COMPUTATION OF GEOSTROPHIC WIND FIELDS ON SIGMA SURFACES
C
C  STREAMFUNCTION AND POTENTIAL ARE SCALED TO BE IN UNITS OF HEIGHT
C  AND VORTICITY AND DIVERGENCE ARE SCALED TO BE IN UNITS OF 1/M. THUS
C
C    F/G*(UG,VG) = K*GRAD(SIG) + GRAD(CHG)
C    GRAD.(F/G*(UG,VG)) = GRAD2(CHG) = DVG
C    GRAD*(F/G*(UG,VG)) = GRAD2(SIG) = VTG
C
C  IT IS RECOMMENDED THAT ONLY EXTRATROPICAL VALUES BE USED
C  FOR ALL THE OTHER FIELDS.
C
C  REQUIRED INPUT IN WAVESPACE
C     PSWV    LN(PS)
C     WAVE1   TEMPERATURE (K)
C     WAVE2   HEIGHT (M)
C
C  POSSIBLE OUTPUT IN GRIDSPACE
C   1 UG      U COMPONENT OF GEOSTROPHIC WIND (M/S)
C   2 VG      V COMPONENT OF GEOSTROPHIC WIND (M/S)
C   3 UDG     U COMPONENT OF DIVERGENT PART OF GEOSTROPHIC WIND (M/S)
C   4 VDG     V COMPONENT OF DIVERGENT PART OF GEOSTROPHIC WIND (M/S)
C   5 URG     U COMPONENT OF ROTATIONAL PART OF GEOSTROPHIC WIND (M/S)
C   6 VRG     V COMPONENT OF ROTATIONAL PART OF GEOSTROPHIC WIND (M/S)
C   7 DVG     DIVERGENCE OF GEOSTROPHIC WIND (1/M)
C   8 VTG     VORTICITY OF GEOSTROPHIC WIND (1/M)
C   9 SIG     STREAMFUNCTION OF GEOSTROPHIC WIND (M)
C  10 CHG     POTENTIAL OF GEOSTROPHIC WIND (M)
C  11 $G      GEOSTROPHIC WIND VECTORS (M/S)
C  12 $DG     DIVERGENT GEOSTROPHIC WIND VECTORS (M/S)
C  13 $RG     ROTATIONAL GEOSTROPHIC WIND VECTORS (M/S)
C  14 SPG     GEOSTROPHIC WIND SPEED (M/S)

C  BG: Declarations
      INTEGER IDIM, JDIM, KDIM, IDIML, JDIML, JDIMX, IJDIM, IJMAXL
C     T126 is 384*190, T62 is 192*94
      PARAMETER (IDIM = 384)
      PARAMETER (JDIM = 190)
      PARAMETER (KDIM =  18)
      PARAMETER (IDIML = IDIM)
      PARAMETER (JDIML = JDIM)
      PARAMETER (JDIMX = ( (JDIM/JDIML)*JDIM +
     1                     (JDIML/JDIM)*JDIML ) /
     2                     (JDIM/JDIML + JDIML/JDIM )  )
      PARAMETER (IJDIM = IDIML*JDIML)
      PARAMETER (IJMAXL = IDIML*JDIML)

      INTEGER MWVD2, MWVD, MWAVE, IROMB, NVARN, NVARN2
      PARAMETER (IROMB = 0)
      PARAMETER (MWAVE = 126)
      PARAMETER (MWVD  = (MWAVE+1)*(MWAVE+2)/2*(1-IROMB)
     1                 + (MWAVE+1)*(MWAVE+1)*IROMB        )
      PARAMETER (MWVD2 = MWVD*2 )
      PARAMETER (NVARN = 36) ! This number is almost certainly wrong. 
      PARAMETER (NVARN2 = 2*NVARN)

      LOGICAL LREGU
      PARAMETER (LREGU = .TRUE.)
      INTEGER MNGDIM
      PARAMETER (MNGDIM = 14)

      REAL COSL(JDIM), WG1(MWVD2), WG2(MWVD2)      
      REAL WORK(IJDIM, 3)
      REAL FOUT(IJMAXL, NVARN2), GRID(IJMAXL)
      REAL WAVE1(MWVD2, KDIM), WAVE2(MWVD2, KDIM)
      REAL WG0(MWVD2), SINCLT(JDIMX), GOF(JDIMX), COR(JDIMX)

C     Arguments:
      REAL PSWV(MWVD2), TEMP(MWVD2), HEIGHT(MWVD2)
      REAL UG(IDIM, JDIM), VG(IDIM, JDIM)

C     Local variables:
      REAL RAD, PSIGM, CHIGM, FAC
      INTEGER IGAU, JGAU, IJGAU, IDIR, IMAX, JMAX
      INTEGER MAXWV
      INTEGER I, J, K
      INTEGER JGEOS, NV, IY, IM, ID, IH, IFT
      INTEGER LABEL, NCHLB, IJ, IJMAX
      INTEGER IGEOS(KDIM)
      INTEGER INAMEG(MNGDIM, KDIM)
      INTEGER NUMCHM(NVARN2)
      CHARACTER*6 NNAMEG(MNGDIM), LELEM
      CHARACTER*80 MTITLE(NVARN2)
      REAL GSURF, OMEGA
      PARAMETER (GSURF = 9.8062)
      PARAMETER (OMEGA = 7.292E-5)
C  End of declarations
C--------=---------=---------=---------=---------=---------=*********+!!

C     Conduct computations that were previously in MAPLAUNCHER but
C        are still needed here.
      RAD = 4.*ATAN(1.)/180.
      MAXWV = MWAVE
      IF (LREGU) THEN
        IDIR = -1
        IMAX = IDIML
        JMAX = JDIML
       ELSE
        IDIR = -101
        IMAX = IDIM
        JMAX = JDIM
      ENDIF
      IJMAX = IMAX*JMAX
      JGEOS = 1
      IGEOS(1) = 1
      INAMEG(1,1) = 1
      INAMEG(2,1) = 1
      DO 9999 I = 3, 14
        INAMEG(I,1) = 0
 9999 CONTINUE
      K = 1

CD      PRINT *,'pswv, temp, height',pswv, temp, height

C     Start original section 
        IGAU=IDIM
        JGAU=JDIM
        IJGAU=IGAU*JGAU
        CALL GAULAT(COSL,JGAU)
        DO 120 J=1,JGAU
          COSL(J)=SIN(COSL(J)*RAD)
  120   CONTINUE
CBG     Included, computation to establish GOF multiplier
        DO 121 J = 1, JGAU
          COR(J) = 2.*OMEGA*COSL(J)
  121   CONTINUE
        DO 122 J = 1, JGAU
          IF (ABS(COR(J)) .GT. 1.E-5) THEN
            GOF(J) = GSURF/COR(J)
           ELSE
            GOF(J) = GSURF/SIGN(1.E-5,COR(J))
          ENDIF
  122   CONTINUE
CBG     End of inclusion
 
CD          PRINT *,'level = ',K
C  COMPUTE PSIG AND CHIG
          PSIGM=0.
          IF (K.EQ.1) PSIGM=106.
          CHIGM=0.
          CALL GEOSTR
CD     1    (PSWV,WAVE1(1,K),WAVE2(1,K),PSIGM,CHIGM,
     1    (PSWV,TEMP,HEIGHT,PSIGM,CHIGM,
     2     WG1,WG2,WORK,COSL,
     3     IGAU,JGAU,MAXWV,IROMB)
CD           PRINT *,'psi, chi ',WG1, WG2
C  COMPUTE AND SAVE DIVG
          CALL LAPLAC(1,WG2,WG2,MAXWV,IROMB)
          WG2(1) = 0.
C  COMPUTE AND SAVE VORG
          CALL LAPLAC(1,WG1,WG1,MAXWV,IROMB)
          WG1(1) = 0.
C  COMPUTE TOTAL GEOSTROPHIC WIND
          CALL SPHERV (IDIR,WORK(1,1),WORK(1,2),WG2,WG1,
     1                 WORK(1,3),SINCLT,IMAX,JMAX,MAXWV,IROMB)
CD          PRINT *,'Called spherv for total geostrophic wind'
C  COMPUTE UG
CD          PRINT *,'Computing UG'
          DO 1611 IJ=1,IJMAX
          WORK(IJ,1)=WORK(IJ,1)*GOF((IJ-1)/IMAX+1)
CD          PRINT *,'UG(i,J) = ',WORK(IJ,1),IJ
 1611     CONTINUE
          IF(.NOT.LREGU) CALL GAU2LX(WORK(1,1),IDIM,JDIM,IDIML,JDIML)
C  COMPUTE VG
CD          PRINT *,'Computing VG'
          DO 1612 IJ=1,IJMAX
          WORK(IJ,2)=WORK(IJ,2)*GOF((IJ-1)/IMAX+1)
 1612     CONTINUE
          IF(.NOT.LREGU) CALL GAU2LX(WORK(1,2),IDIM,JDIM,IDIML,JDIML)
C  SAVE UG
          NV=INAMEG(1,K)
          LELEM=NNAMEG(1)
CBG Changeover to argument in ug, vg
          IJ = 0
          DO 161 J = 1, JDIML
            DO 162 I = 1, IDIML
              IJ = IJ + 1
              UG(I,J) = WORK(IJ, 1)
  162       CONTINUE
  161     CONTINUE

C  SAVE VG
          NV=INAMEG(2,K)
          LELEM=NNAMEG(2)
          IJ = 0
          DO 9162 J = 1, JDIML
            DO 9163 I = 1, IDIML
              IJ = IJ + 1
              VG(I,J) = WORK(IJ,2)
 9163       CONTINUE
 9162     CONTINUE

C

CD      PRINT *,'ug, vg, in irgeos ', UG, VG
      RETURN
      END
