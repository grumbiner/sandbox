      subroutine fftfax(n,ifax,trigs)
      parameter(pi=3.14159265358979)
      dimension trigs(2*n),ifax(20),jfax(20),lfax(6)
      data lfax/8,5,4,3,2,1/
      del=2.*pi/n
      nhl=(n/2)-1
      trigs(1)=1.
      trigs(2)=0.
cdir$ ivdep
      do k=1,nhl
        angle=k*del
        trigs(2*k+1)=cos(angle)
        trigs(2*k+2)=sin(angle)
        trigs(2*k+1+n)=trigs(2*k+1)/trigs(2*k+2)
        trigs(2*k+2+n)=trigs(2*k+2)
      enddo
      nu=n
      k=0
      l=1
      ifac=lfax(l)
      ifax(1)=0
   20 continue
        nw=nu/ifac
        if(nw*ifac.eq.nu.and.(ifac.ne.8.or.k.eq.0)) then
          k=k+1
          jfax(k)=ifac
          nu=nw
        else
          l=l+1
          ifac=lfax(l)
          if(ifac.eq.1) then
            write(6,*) n,' contains illegal factors'
            return
          endif
        endif
      if(nu.gt.1) go to 20
      ifax(1)=k
      do i=2,k+1
        ifax(i)=jfax(k+2-i)
      enddo
      return
      end

C      TRIGS(2*K+1+N)=TRIGS(2*K+1)/TRIGS(2*K+2)
C        TRIGS(2*K+2+N)=TRIGS(2*K+2)
C      ENDDO
C      NU=N
C      K=0
C      L=1
C      IFAC=LFAX(L)
C      IFAX(1)=0
C      DO WHILE(NU.GT.1)
C        NW=NU/IFAC
C        IF(NW*IFAC.EQ.NU.AND.(IFAC.NE.8.OR.K.EQ.0)) THEN
C          K=K+1
C          JFAX(K)=IFAC
C          NU=NW
C        ELSE
C          L=L+1
C          IFAC=LFAX(L)
C          IF(IFAC.EQ.1) THEN
C            WRITE(6,*) N,' CONTAINS ILLEGAL FACTORS'
C            RETURN
C          ENDIF
C        ENDIF
C      ENDDO
C      IFAX(1)=K
C      DO I=2,K+1
C        IFAX(I)=JFAX(K+2-I)
C      ENDDO
C      RETURN
C      END
