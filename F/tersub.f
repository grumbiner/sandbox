      SUBROUTINE TERSUB(IM,JM,NM,NR,NW)
      PARAMETER(LREC=7216,IPR=30,JPR=30,IMN=2160,JMN=1080)
      PARAMETER(IHB=7,IHO=68,IKB=9,IKO=30,IRB=64)
      PARAMETER(PI=3.14159265358979)
      REAL COSCLT(JM),WGTCLT(JM),RCLT(JM)
      INTEGER IB2(0:IM+1),JB2(0:JM)
      REAL WGT(IM,JM),SLM(IM,JM),ORO(IM,JM),VAR(IM,JM),ORS(NW)
      CHARACTER REC(LREC)
      INTEGER IHT(IPR,JPR),KHT(IPR,JPR)
      INTEGER IHTQ(IPR,JPR),KHTQ(IPR,JPR)
      DATA IHTMAX/100/,KHTMAX/400/
C  SET CONSTANTS AND ZERO FIELDS
      IB2(0)=0
      IB2(IM+1)=IMN
      DO I=1,IM
        IB2(I)=NINT((I-0.5)*IMN/IM)
      ENDDO
      CALL GAUSSLAT(JM,COSCLT,WGTCLT)
      DO J=1,JM/2
        RCLT(J)=ACOS(COSCLT(J))
      ENDDO
      JB2(0)=JMN
      JB2(JM/2)=JMN/2
      JB2(JM)=0
      R2PJMN=JMN/(2.*PI)
CDIR$ IVDEP
      DO J=1,JM/2-1
        J2=NINT((RCLT(J)+RCLT(J+1))*R2PJMN)
        JB2(J)=JMN-J2
        JB2(JM-J)=J2
      ENDDO
      DO J=1,JM
        DO I=1,IM
          WGT(I,J)=0.
          SLM(I,J)=0.
          ORO(I,J)=0.
          VAR(I,J)=0.
        ENDDO
      ENDDO
C  LOOP OVER ALL NAVY RECORDS
      JG1=JM
      DO JB=1,JMN/JPR
        JS=(JB-1)*JPR
        JG2=JG1
        DO WHILE(JB2(JG1-1).LT.JS+JPR)
          JG1=JG1-1
        ENDDO
        DO IB=1,IMN/IPR
          IS=(IB-1)*IPR
          IG1=NINT((IS+0.5)*IM/IMN)+1
          IG2=NINT((IS+IPR-0.5)*IM/IMN)+1
C  READ AND UNPACK
          READ(11) REC
          CALL GBYTES(REC,IHT,IHO,IHB,IRB-IHB,IPR*JPR)
          CALL GBYTES(REC,KHT,IKO,IKB,IRB-IKB,IPR*JPR)
C  DO QUALITY CONTROL (ON 19 INVALID POINTS)
          NQBAD=1
          DO WHILE(NQBAD.GT.0)
            NQBAD=0
            DO J=1,JPR
              DO I=1,IPR
                IF(IHT(I,J).GT.IHTMAX.OR.KHT(I,J).GT.KHTMAX) THEN
                  SIHT=0.
                  SKHT=0.
                  WBHT=0.
                  IF(J-1.GE.1.AND.IHT(I,J-1).LE.IHTMAX.AND.
     &                            KHT(I,J-1).LE.KHTMAX) THEN
                    WBHT=WBHT+1.
                    SIHT=SIHT+IHT(I,J-1)
                    SKHT=SKHT+KHT(I,J-1)
                  ENDIF
                  IF(I-1.GE.1.AND.IHT(I-1,J).LE.IHTMAX.AND.
     &                            KHT(I-1,J).LE.KHTMAX) THEN
                    WBHT=WBHT+1.
                    SIHT=SIHT+IHT(I-1,J)
                    SKHT=SKHT+KHT(I-1,J)
                  ENDIF
                  IF(I+1.LE.IPR.AND.IHT(I+1,J).LE.IHTMAX.AND.
     &                              KHT(I+1,J).LE.KHTMAX) THEN
                    WBHT=WBHT+1.
                    SIHT=SIHT+IHT(I+1,J)
                    SKHT=SKHT+KHT(I+1,J)
                  ENDIF
                  IF(J+1.LE.JPR.AND.IHT(I,J+1).LE.IHTMAX.AND.
     &                              KHT(I,J+1).LE.KHTMAX) THEN
                    WBHT=WBHT+1.
                    SIHT=SIHT+IHT(I,J+1)
                    SKHT=SKHT+KHT(I,J+1)
                  ENDIF
                  IF(WBHT.GT.0.) THEN
                    IHTQ(I,J)=NINT(SIHT/WBHT)
                    KHTQ(I,J)=NINT(SKHT/WBHT)
C                   PRINT '("QC ON ",6I8)',I+IS,J+JS,
C    &               IHT(I,J),KHT(I,J),IHTQ(I,J),KHTQ(I,J)
                  ELSE
                    NQBAD=NQBAD+1
                    IHTQ(I,J)=IHT(I,J)
                    KHTQ(I,J)=KHT(I,J)
                  ENDIF
                ELSE
                  IHTQ(I,J)=IHT(I,J)
                  KHTQ(I,J)=KHT(I,J)
                ENDIF
              ENDDO
            ENDDO
            DO J=1,JPR
              DO I=1,IPR
                IHT(I,J)=IHTQ(I,J)
                KHT(I,J)=KHTQ(I,J)
              ENDDO
            ENDDO
          ENDDO
C  SUM FIELDS
          DO JG=JG1,JG2
            J1=MAX(JB2(JG)-JS+1,1)
            J2=MIN(JB2(JG-1)-JS,JPR)
            DO IGN=IG1,IG2
              I1=MAX(IB2(IGN-1)-IS+1,1)
              I2=MIN(IB2(IGN)-IS,IPR)
              IG=MOD(IGN-1,IM)+1
              DO IJ=1,(I2-I1+1)*(J2-J1+1)
                I=MOD(IJ-1,I2-I1+1)+I1
                J=(IJ-1)/(I2-I1+1)+J1
                SHT=-0.01*(IHT(I,J)-100)
                ZHT=30.48*(KHT(I,J)-100)
                WGT(IG,JG)=WGT(IG,JG)+1.
                SLM(IG,JG)=SLM(IG,JG)+SHT
                ORO(IG,JG)=ORO(IG,JG)+ZHT
                VAR(IG,JG)=VAR(IG,JG)+ZHT**2
              ENDDO
            ENDDO
          ENDDO
C  FINISHED WITH THIS NAVY RECORD
        ENDDO
      ENDDO
C  NORMALIZE FIELDS
      DO J=1,JM
        DO I=1,IM
          IF(WGT(I,J).GT.0.) THEN
            SLM(I,J)=NINT(SLM(I,J)/WGT(I,J))
            ORO(I,J)=ORO(I,J)/WGT(I,J)
            VAR(I,J)=SQRT(MAX(VAR(I,J)/WGT(I,J)-ORO(I,J)**2,0.))
          ENDIF
        ENDDO
      ENDDO
C  SPECTRALLY TRUNCATE OROGRAPHY
      CALL SPHERT(1,ORO,ORS,0,0.,IM,JM,NM,NR)
      CALL SPHERT(-1,ORO,ORS,0,0.,IM,JM,NM,NR)
C  OUTPUT FIELDS
      WRITE(51) SLM
      WRITE(52) ORO
      WRITE(53) VAR
      WRITE(54) ORS
      RETURN
      END
