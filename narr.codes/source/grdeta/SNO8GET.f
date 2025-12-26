      SUBROUTINE SNO8GET(SNODEP,IYR,IMN,IDY,INSNOAF,LIST,LUSAF,
     *    idate,iret2)
C
C USAGE: Program to read in USAF gribbed snow/ice data on 1/8th bedient
C polar stereographic grid. 
C
      INTEGER KPDS(25), KGDS(22), JPDS(25), JGDS(22)
      REAL SNODEP(512,512), snotmp(512,512)  
      LOGICAL*1 BITMAP(512,512), LUSAF
      dimension idate(4)
C
C     PROCESS USAF N.H. SNOW ANAL (FROM COLA), GRIBBED
C     USAF SNOW ANAL IS DAILY AND HIGH RES (45 KM)             
C                                                                  
C  SNOW IS IN METERS.
c  
c  For Regional Reanalysis, there is no ice, so just read the snow.
C                                            
      print*,'In sno8get,insnoaf=',insnoaf
      call baopenr(41,'fort.41',ireto)
      print*,'ireto=',ireto
      call baopenr(37,'fort.37',ireto)
      print*,'ireto=',ireto
      JPDS = -1
      icent = (idate(4) - 1) / 100 + 1
      jpds(8) = idate(4) - (icent-1)*100
      jpds(9) = idate(2)
      jpds(10)= idate(3)
c     jpds(21)=icent
      print*,'jpds=',jpds
      CALL GETGB(INSNOAF,0,512*512,0,JPDS,JGDS,KF,KNUM,KPDS,KGDS,
     &     BITMAP,SNODEP,IRET2)
      print*,'kpds=',kpds
      print*,'kgds=',kgds
      WRITE(6,*) 'AFTER GETGB FOR AF SNOW, IRET=', IRET2
C
      IYR2D = KPDS(8)
      IMN = KPDS(9)
      IDY = KPDS(10)
      ICENT = KPDS(21)
      IF(IYR2D.LT.100) THEN
       IYR = (ICENT - 1) * 100 + IYR2D
      ELSE
       IYR = ICENT * 100
      ENDIF
C
      LUSAF =  IRET2.EQ.0
      WRITE(6,*) 'LUSAF=', LUSAF
      IF (.NOT.LUSAF) RETURN
      
C
c     DO 20 I = 1, 512
c       DO 10 J = 1, 512
c         IF (ABS(ICE(I,J)-1.) .LT. 0.0001) SNODEP(I,J) = 11.
c10     CONTINUE
c20   CONTINUE
      RETURN
      END
