C   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
       SUBROUTINE PUTVEG(TLM0D,TPH0D,DLMD,DPHD,HLAT,HLON,
     &                   FVEG0,SM,SICE,FVEG1)
C   **************************************************************
C   * Interpolate NESDIS vegetation fraction product (five-year  *
C   * climatology with 0.144 degree resolution from 89.928S,180W * 
C   * to 89.928N, 180E)                                          *
C   * F. Chen 07/96                                              *
C   **************************************************************

      INCLUDE "parmeta.res"
C
      PARAMETER  (L0=2500*1250)
C
      DIMENSION FVEG0(2500,1250), FVEG1(IM,JM)  
      DIMENSION HLAT(IM,JM),HLON(IM,JM)
      DIMENSION SM(IM,JM),SICE(IM,JM)
C      INTEGER IPOPT(20), JPDS(25), JGDS(22), KGDS0(22), KGDS1(22)
C     +      , KPDS0(25), KPDS1(25), BITFLG1(1)
C      CHARACTER GDS1(42)
C      LOGICAL BIT0(2500,1250), BIT1(IM,JM)
CFEI      DATA IG/96/

C   This subroutine AVERAGES the value stored in FVEG0 TO the FVEG1 array.
C   Special effort is made to ensure that islands are taken care of when
C   not resolved on the 0.144 x 0.144 deg. grid.
C  *** Define the search limit for isolated island point 
        NLIM=30*(1+AINT(0.15/DPHD))
C  *** Define the Eta averaging box size, NBOX=0 gives nearest neighbor
        NBOX1=NINT(MAX(DLMD,DPHD)/0.144)
        NBOX=NBOX1
C        print*,' im,jm=',im,jm,'NBOX=',NBOX,'DPHD',DPHD,'NLIM',NLIM,
C     &         'DLMD',DLMD

        DO J = 1,JM
        DO I = 1,IM

          IF((SM(I,J).GT.0.5).OR.(SICE(I,J).EQ.1.0)) THEN
            FVEG1(I,J) = 0.0
          ELSE
            DX=(HLON(I,J) - 179.928)/0.144
            INDX = NINT(DX)
C  ***  Here, 179.928 is the starting longitude (180.072) + one grid cell 
C       width (0.144) so that the first index is one NOT 0.
            IF(INDX.LT.1) THEN 
              DX=(HLON(I,J) + 180.072)/0.144
              INDX = NINT(DX)
            ENDIF
            DY=(HLAT(I,J) + 90.072)/0.144
            INDY = NINT(DY)
C  *** Get area-average value of FVEG1 from input grid FVEG0, for 
C      finer Eta grid, this becomes nearest-neighbour 
            ICON1=0
  100       CONTINUE
            IF(MOD(NBOX,2).EQ.0) THEN
              IF(DX.GT.REAL(INDX)) THEN
                NLO=MAX(1,INDX-NBOX/2+1)
                NHI=MIN(2500,INDX+NBOX/2)
              ELSE
                NLO=MAX(1,INDX-NBOX/2)
                NHI=MIN(2500,INDX+NBOX/2-1)
              ENDIF
              IF(DY.GT.REAL(INDY)) THEN
                MLO=MAX(1,INDY-NBOX/2+1)
                MHI=MIN(1250,INDY+NBOX/2) 
              ELSE
                MLO=MAX(1,INDY-NBOX/2)
                MHI=MIN(1250,INDY+NBOX/2-1)
              ENDIF
            ELSE
              NLO=MAX(1,INDX-NBOX/2)
              NHI=MIN(2500,INDX+NBOX/2)
              MLO=MAX(1,INDY-NBOX/2)
              MHI=MIN(1250,INDY+NBOX/2)
            ENDIF
C            NLO=MAX(1,INDX-NBOX)
C            NHI=MIN(2500,INDX+NBOX)
C            MLO=MAX(1,INDY-NBOX)
C            MHI=MIN(1250,INDY+NBOX)
            ICON=0
            VEGSUM=0.0
            DO N=NLO, NHI
              DO M=MLO, MHI
                IF(FVEG0(N,M).NE.0.0) THEN
                 ICON=ICON+1 
                 VEGSUM=VEGSUM+FVEG0(N,M)
                ENDIF
              ENDDO
            ENDDO
            IF(ICON.NE.0) THEN
              FVEG1(I,J)=VEGSUM/ICON
              NBOX=NBOX1
            ELSE
C *** Search for an isolated point 
              ICON1=ICON1+1
              NBOX=NBOX+1 
              IF(ICON1.LE.NLIM) THEN
                GO TO 100
              ELSE
                FVEG1(I,J)=0.55
                NBOX=NBOX1
                print *,"OH OH ",HLAT(I,J),HLON(I,J)
              ENDIF
            ENDIF
C ** End of land case
C          if(NBOX.NE.NBOX1) print*,' NBOX is NOT NBOX1,=',NBOX
          END IF  
          IF(FVEG1(I,J).LT.0.0.OR.FVEG1(I,J).GT.1.0) THEN 
           print*,' FVEG1 out of range, FVEG=',FVEG1(I,J)
          ENDIF
C ** End of IM,JM loops
        END DO  
        END DO  
        RETURN
        END
