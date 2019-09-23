      REAL FUNCTION SASUM(N,SX,INCX)
C
C     TAKES THE SUM OF THE ABSOLUTE VALUES.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      real SX(1)
      REAL STEMP
      INTEGER I,INCX,N,mp1,NINCX
C
      SASUM = 0.0E0
      STEMP = 0.0E0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        STEMP = STEMP + ABS(SX(I)) 
   10 CONTINUE
      SASUM = STEMP
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C        CLEAN UP LOOP 
   20 m=mod(n,6)
      if (m.eq.0) go to 40
      do 30 i=1,m
	stemp=stemp+abs(sx(i))
   30 continue
      if (n.lt.6) go to 60
   40 mp1=m+1
      do 50 i=mp1,n,6
      stemp=stemp+abs(sx(i))+abs(sx(i+1))+abs(sx(i+2))
     *+abs(sx(i+3))+abs(sx(i+4))+abs(sx(i+5))
   50 continue
   60 sasum=stemp
      RETURN
      END
      SUBROUTINE saxpy (n,sa,sx,incx,sy,incy)
      real sx(1),sy(1),sa
      integer i,incx,incy,ix,iy,m,mp1,n
      if (n.le.0) return
      if (sa.eq.0.0) return
      if (incx.eq.1.and.incy.eq.1) go to 20
      ix=1
      iy=1
      if (incx.lt.0) ix=(-n+1)*incx+1
      if (incy.lt.0) iy=(-n+1)*incy+1
      do 10 i=1,n
      sy(iy)=sy(iy)+sa*sx(ix)
      ix=ix+incx
      iy=iy+incy
   10 continue
      return
   20 m=mod(n,4)
      if (m.eq.0) go to 40
      do 30 i=1,m
      sy(i)=sy(i)+sa*sx(i)
   30 continue
      if (n.lt.4) return
   40 mp1=m+1
      do 50 i=mp1,n,4
      sy(i)=sy(i)+sa*sx(i)
      sy(i+1)=sy(i+1)+sa*sx(i+1)
      sy(i+2)=sy(i+2)+sa*sx(i+2)
      sy(i+3)=sy(i+3)+sa*sx(i+3)
   50 continue
      return
      END
      REAL FUNCTION sdot (n,sx,incx,sy,incy)
      real sx(1),sy(1),stemp
      integer i,incx,incy,ix,iy,m,mp1,n
      stemp=0.0e0
      sdot=0.0e0
      if (n.le.0) return
      if (incx.eq.1.and.incy.eq.1) go to 20
      ix=1
      iy=1
      if (incx.lt.0) ix=(-n+1)*incx+1
      if (incy.lt.0) iy=(-n+1)*incy+1
      do 10 i=1,n
      stemp=stemp+sx(ix)*sy(iy)
      ix=ix+incx
      iy=iy+incy
   10 continue
      sdot=stemp
      return
   20 continue
      m=mod(n,5)
      if (m.eq.0) go to 40
      do 30 i=1,m
      stemp=stemp+sx(i)*sy(i)
   30 continue
      if (n.lt.5) go to 60
   40 mp1=m+1
      do 50 i=mp1,n,5
      stemp=stemp+sx(i)*sy(i)+sx(i+1)*sy(i+1)+
     *sx(i+2)*sy(i+2)+sx(i+3)*sy(i+3)+sx(i+4)*sy(i+4)
   50 continue
   60 sdot=stemp
      return
      END
      SUBROUTINE sscal (n,sa,sx,incx)
      real sa,sx(1)
      integer i,incx,m,mp1,n,nincx
      if (n.le.0) return
      if (incx.eq.1) go to 20
      nincx=n*incx
      do 10 i=1,nincx,incx
      sx(i)=sa*sx(i)
   10 continue
      return
   20 m=mod(n,5)
      if (m.eq.0) go to 40
      do 30 i=1,m
      sx(i)=sa*sx(i)
   30 continue
      if (n.lt.5) return
   40 mp1=m+1
      do 50 i=mp1,n,5
      sx(i)=sa*sx(i)
      sx(i+1)=sa*sx(i+1)
      sx(i+2)=sa*sx(i+2)
      sx(i+3)=sa*sx(i+3)
      sx(i+4)=sa*sx(i+4)
   50 continue
      return
      END
      INTEGER FUNCTION isamax (n,sx,incx)
      real sx(1),smax
      integer i,incx,ix,n
      isamax=0 
      if (n.lt.1) return
      isamax=1
      if (n.eq.1) return
      if (incx.eq.1) go to 20
      ix=1
      smax=abs(sx(1))
      ix=ix+incx
      do 10 i=2,n
      if (abs(sx(ix)).le.smax) go to 5
      isamax=i
      smax=abs(sx(ix))
    5 ix=ix+incx
   10 continue
      return
   20 smax=abs(sx(1))
      do 30 i=2,n
      if (abs(sx(i)).le.smax) go to 30
      isamax=i
      smax=abs(sx(i))
   30 continue
      return
      END
