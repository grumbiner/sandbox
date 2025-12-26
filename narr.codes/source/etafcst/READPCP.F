      SUBROUTINE READPCP
C     ****************************************************************
C     *                                                              *
C     *   PRECIPITATION ASSIMILATION INITIALIZATION.                 *
C     *    READ IN PRECIP ANALYSIS AND DATA MASK AND SET UP ALL      *
C     *    APPROPRIATE VARIABLES.                                    *
C     *                   MIKE BALDWIN, MARCH 1994                   *
C     *                   Adapted to 2-D code, Ying Lin, Mar 1996    *
C     *                                                              *
C     ****************************************************************
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
C-----------------------------------------------------------------------
      INCLUDE "PPTASM.comm"
      INCLUDE "mpp.h"
      LOGICAL*1 BITMPCP(IM,JM)
      INTEGER JPDS(25), JGDS(22), KGDS(22), KPDS(25)

C-----------------------------------------------------------------------
C
C READ ANALYSES AND DATA MASK
C
      IF(MYPE.EQ.0) THEN
        call baopenr(41,'fort.41',iret)
        write(0,*) 'baopenr on unit41', ' iret=', iret
        JPDS=-1
      ENDIF
C
      DO IHR=1,3
        IF(MYPE.EQ.0) THEN
        print*,'ihr=',ihr
          CALL GETGB(41,0,IM*JM,IHR-1,JPDS,JGDS,KF,K,KPDS,KGDS,BITMPCP,
     &                     temp1,IRET)
          write(0,10) ihr, iret, kf, kpds(5),
     &         (kpds(21)*100+kpds(8))/100-1, mod(kpds(8),100),kpds(9),
     &          kpds(10), kpds(11)
   10      format('ihr=',i1,' iret=',i3,' kf=', i6,' fld=', i3, 
     &     2x,'1h accum from ', 5i2.2)
C
          DO J=1,JM
            DO I=1,IM
c             if(temp1(i,j).gt.1.0) print*,'i,j,pcp=',i,j,temp1(i,j)
c             print*,'i,j,pcp=',i,j,temp1(i,j)
              IF (BITMPCP(I,J)) THEN
C Convert data from mm to m:
		if (temp1(i,j).lt.0.0) then
       write(0,*) " precip < 0. will be set to 999.", I,J, temp1(i,j)
		    temp1(i,j) = 999.0
		else if (temp1(i,j).gt.24.0) then
       write(0,*) " precip >24. will be set to 999.", I,J, temp1(i,j)
		    temp1(i,j) = 999.0
		else
                    temp1(I,J)=temp1(I,J)*0.001            
		endif
c               if(temp1(i,j).gt.0.04) then
c               if(temp1(i,j).gt.0.001) then
c               if(i.ge. 99.and.i.le.105.and.(j.ge.172.and.j.le.174).or
c    *            .(j.ge.180.and.j.le.184)) then
c                 temp1(i,j)=999.
c                 bitmpcp(i,j)=.f.
c               endif
c               if(i.ge. 99.and.i.le.105.and.j.ge.113.and.j.le.119) then
c                 print*,'i,j,pcp=',i,j,temp1(i,j)
c                 temp1(i,j)=999.
c                 bitmpcp(i,j)=.f.
c               endif
              ELSE
                TEMP1(I,J)=999.
              ENDIF
c
c Mask out outer 2 rows and outer 2 colums - 19 March 2004 PS and DJ
c
       if(i.le.1.or.i.ge.im-1+mod(j,2) .or. j.le.2.or.j.ge.jm-1) then
          temp1(i,j)=999.
          bitmpcp(i,j)=.FALSE.
       endif
            ENDDO
          ENDDO
          WRITE(0,*) '  FINISHED READING PRECIP ANALYSES AND DATA MASK' 
        ENDIF
c
c special test
c
C
        CALL DSTRB(TEMP1,PPTDAT,1,3,IHR)
C
      ENDDO
C
      RETURN
      END

