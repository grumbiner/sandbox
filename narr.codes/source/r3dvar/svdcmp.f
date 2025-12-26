subroutine svdcmp(a,m,n,mp,np,w,v,ierror)

   implicit none
   integer ierror,m,mp,n,np,NMAX
   real(8) a(mp,np),v(np,np),w(np)
   PARAMETER (NMAX=500)                 !  Maximum anticipated value of n.
!   USES pythag
!       Given a matrix a(1:m,1:n), with physical dimensions mp by np, this routine computes its
!       singular value decomposition, A = U*W*Vt.  The matrix U replaces a on output.  The
!       diagonal matrix of singular values W is output as a vector w(1:n).  The matrix V (not the
!       transpose Vt) is output as V((1:n,1:n).
   integer i,its,j,jj,k,l,nm
   real(8) anorm,c,f,g,h,s,scale,x,y,z,rv1(NMAX),pythag

   ierror=0                            !    error flag:  return 0 means normal completion
                                       !                 return -1, then no convergence after 30 iterations

   g=0._8                              !  Housholder reduction to bidiagonal form.
   scale=0._8
   anorm=0._8
   do 25 i=1,n
       l=i+1
       rv1(i)=scale*g
       g=0._8
       s=0._8
       scale=0._8
       if(i.le.m) then
           do 11 k=i,m
                scale=scale+abs(a(k,i))
11         end do
           if(scale.ne.0._8) then
               do 12 k=i,m
                    a(k,i)=a(k,i)/scale
                    s=s+a(k,i)*a(k,i)
12             end do
               f=a(i,i)
               g=-sign(sqrt(s),f)
               h=f*g-s
               a(i,i)=f-g
               do 15 j=l,n
                    s=0._8
                    do 13 k=i,m
                         s=s+a(k,i)*a(k,j)
13                  end do
                    f=s/h
                    do 14 k=i,m
                         a(k,j)=a(k,j)+f*a(k,i)
14                  end do
15             end do
               do 16 k=i,m
                    a(k,i)=scale*a(k,i)
16             end do
           end if
       end if
       w(i)=scale*g
       g=0._8
       s=0._8
       scale=0._8
       if((i.le.m).and.(i.ne.n)) then
           do 17 k=l,n
                scale=scale+abs(a(i,k))
17         end do
           if(scale.ne.0._8) then
                do 18 k=l,n
                     a(i,k)=a(i,k)/scale
                     s=s+a(i,k)*a(i,k)
18              end do
                f=a(i,l)
                g=-sign(sqrt(s),f)
                h=f*g-s
                a(i,l)=f-g
                do 19 k=l,n
                     rv1(k)=a(i,k)/h
19              end do
                do 23 j=l,m
                     s=0._8
                     do 21 k=l,n
                          s=s+a(j,k)*a(i,k)
21                   end do
                     do 22 k=l,n
                          a(j,k)=a(j,k)+s*rv1(k)
22                   end do
23              end do
                do 24 k=l,n
                     a(i,k)=scale*a(i,k)
24              end do
           end if
       end if
       anorm=max(anorm,(abs(w(i))+abs(rv1(i))))
25 end do
   do 32 i=n,1,-1                !    Accumulation of right-hand transformations.
       if(i.lt.n) then
           if(g.ne.0._8) then
               do 26 j=l,n       !    Double division to avoid possible underflow.
                    v(j,i)=(a(i,j)/a(i,l))/g
26             end do 
               do 29 j=l,n
                    s=0._8
                    do 27 k=l,n
                         s=s+a(i,k)*v(k,j)
27                  end do
                    do 28 k=l,n
                         v(k,j)=v(k,j)+s*v(k,i)
28                  end do
29             end do
           end if
           do 31 j=l,n
               v(i,j)=0._8
               v(j,i)=0._8
31         end do
       end if
       v(i,i)=1._8
       g=rv1(i)
       l=i
32 end do
   do 39 i=min(m,n),1,-1          !    Accumulation of left-hand transformations.
       l=i+1
       g=w(i)
       do 33 j=l,n
           a(i,j)=0._8
33     end do
       if(g.ne.0._8) then
           g=1._8/g
           do 36 j=l,n
               s=0._8
               do 34 k=l,m
                   s=s+a(k,i)*a(k,j)
34             end do
               f=(s/a(i,i))*g
               do 35 k=i,m
                   a(k,j)=a(k,j)+f*a(k,i)
35             end do
36         end do
           do 37 j=i,m
               a(j,i)=a(j,i)*g
37         end do
       else
           do 38 j=i,m
               a(j,i)=0._8
38         end do
       end if
       a(i,i)=a(i,i)+1._8
39 end do
   do 49 k=n,1,-1                   !    Diagonalization of the bidiagonal form:  Loop over
       do 48 its=1,30               !       singular values, and over allowed iterations.
           do 41 l=k,1,-1           !    Test for splitting.
               nm=l-1               !    Note that rv1(1) is always zero.
               if((abs(rv1(l))+anorm).eq.anorm) go to 2
               if((abs(w(nm))+anorm).eq.anorm) go to 1
41         end do
1          c=0._8                   !    Cancellation of rv1(l), if l > 1.
           s=1._8
           do 43 i=l,k
               f=s*rv1(i)
               rv1(i)=c*rv1(i)
               if((abs(f)+anorm).eq.anorm) go to 2
               g=w(i)
               h=pythag(f,g)
               w(i)=h
               h=1._8/h
               c= (g*h)
               s=-(f*h)
               do 42 j=1,m
                   y=a(j,nm)
                   z=a(j,i)
                   a(j,nm)=(y*c)+(z*s)
                   a(j,i)=-(y*s)+(z*c)
42             end do
43         end do
2          z=w(k)
           if(l.eq.k) then          !    Convergence.
               if(z.lt.0._8) then   !    Singular value is made nonnegative.
                   w(k)=-z
                   do 44 j=1,n
                       v(j,k)=-v(j,k)
44                 end do
               end if
               go to 3
           end if
           if(its.eq.30) then
             ierror=-1
             print *,' no convergence in svdcmp after 30 iterations'
             return
           end if
           x=w(l)                   !     Shift from bottom 2-by-2 minor.
           nm=k-1
           y=w(nm)
           g=rv1(nm)
           h=rv1(k)
           f=((y-z)*(y+z)+(g-h)*(g+h))/(2._8*h*y)
           g=pythag(f,1._8)
           f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
           c=1._8                  !      Next QR transformation
           s=1._8
           do 47 j=l,nm
               i=j+1
               g=rv1(i)
               y=w(i)
               h=s*g
               g=c*g
               z=pythag(f,h)
               rv1(j)=z
               c=f/z
               s=h/z
               f= (x*c)+(g*s)
               g=-(x*s)+(g*c)
               h=y*s
               y=y*c
               do 45 jj=1,n
                   x=v(jj,j)
                   z=v(jj,i)
                   v(jj,j)= (x*c)+(z*s)
                   v(jj,i)=-(x*s)+(z*c)
45             end do
               z=pythag(f,h)
               w(j)=z            !      Rotation can be arbitrary if z = 0.
               if(z.ne.0._8) then
                   z=1._8/z
                   c=f*z
                   s=h*z
               end if
               f= (c*g)+(s*y)
               x=-(s*g)+(c*y)
               do 46 jj=1,m
                   y=a(jj,j)
                   z=a(jj,i)
                   a(jj,j)= (y*c)+(z*s)
                   a(jj,i)=-(y*s)+(z*c)
46             end do
47         end do
           rv1(l)=0._8
           rv1(k)=f
           w(k)=x
48     end do
3      continue
49 end do
return
end

function pythag(a,b)

  implicit none
  real(8) a,b,pythag
        !     Computes sqrt(a**2 + b**2) without destructive underflow or overflow.
  real(8) absa,absb
  absa=abs(a)
  absb=abs(b)
  if(absa.gt.absb) then
      pythag=absa*sqrt(1._8+(absb/absa)**2)
  else
      if(absb.eq.0._8) then
          pythag=0._8
      else
          pythag=absb*sqrt(1._8+(absa/absb)**2)
      end if
  end if
return
end
