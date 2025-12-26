subroutine get_aspect(iplot,jplot,kplot,aspect,amp,instats_x,instats_f,scale_stats,tg,ug,vg,qg,pg,ksfc, &
                 etam,ptop,dlonc,dlatc,nxc,nyc,lmeta,corlmin,corlmax, &
                 ids, ide, jds, jde, kds, kde, &         ! domain indices
                 ips, ipe, jps, jpe, kps, kpe, &         ! patch indices
                 ims, ime, jms, jme, kms, kme, &                     ! memory indices
      inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

  IMPLICIT NONE
  include 'mpif.h'
      include "my_comm.h"

  INTEGER(4), INTENT(IN) :: ids, ide, jds, jde, kds, kde, &   ! domain indices
                            ips, ipe, jps, jpe, kps, kpe, &   ! patch indices
                            ims, ime, jms, jme, kms, kme      ! memory indices
  INTEGER(4), INTENT(IN) :: &
     inpes, jnpes, mype, npes, pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

  integer(4) iplot(20,20),jplot(20,20),kplot(20,20)
  real(4) aspect(7,ips:ipe,jps:jpe,kps:kpe)
  real(4) amp(ips:ipe,jps:jpe,kps:kpe)

  integer(4) lmeta,nxc,nyc

  integer(4) instats_x,instats_f    !    file numbers where stats for this variable are contained
                                    !       (same routine used for psi, chi, t, q, peta)

  real(4) scale_stats               !    grid scale of model used to create stats

  real(4) tg(ims:ime,jms:jme,kms:kme),ug(ims:ime,jms:jme,kms:kme)   ! smoothed output fields on
  real(4) vg(ims:ime,jms:jme,kms:kme),qg(ims:ime,jms:jme,kms:kme)   !  coarse analysis grid
  real(4) pg(ims:ime,jms:jme)
  integer(4) ksfc(ips:ipe,jps:jpe)

  real(4)   etam(lmeta)                 ! eta coordinate
  real(4)   dlonc,dlatc                 !  coarse grid increments in degrees
  real(4) ptop                          ! top pressure

  real(4) corlmin,corlmax               !  min, max allowable correlation lengths (in grid units)

  integer(4) i,ierr,im,ip,iv3,j,jm,jp,k,km,kp
  real(4) a1,a2,a3,a4,a5,a6,biga1,biga2,biga3,biga4,biga5,biga6
  real(4) conmc,delxi,delyi,delzi,deta,dqnorm,dqref,dqx,dqx2,dqy,dqy2,dqz,dqz2,h,h0,href
  real(4) rldqnorm,rscale_delta,qlf,qlfs,qlx,qlxs
  real(4) sig_thetas,stdf,stdx,tgmax,tgmaxall,tgmin,tgminall,unorm
  real(4) inflate_aspect_inv
       real(4) atest(6),rtest(3,3),rlen1,rlen2,rlen3,aspectmax,aspectmin,rlenmax,rlenmin
       real(4) rlenmaxall,rlenminall,aspectmaxall,aspectminall
               real(4) aspectthis,rlenmin_as,rlenmax_as
               integer(4) iasmax,jasmax,kasmax
                   real(4) anglemax
                   integer(4) imax
          real(4) err1,err2,err3,err4,err5,err6,errmax,errmaxall
  integer(4) iplotall(20,20),jplotall(20,20),kplotall(20,20)
  real(4) aspectplot(20,20)
  real(4) aspectplotall(20,20)
  integer(4) iipl,jjpl
  
!    compute inflation factor for aspect tensor 

! inflate_aspect_inv=(.5*(dlonc+dlatc)/scale_stats)**2
  inflate_aspect_inv=1.
  if(mype.eq.0) print *,' in get_aspect, inflate_aspect_inv,scale_stats,dlonc,dlatc=', &
                             inflate_aspect_inv,scale_stats,dlonc,dlatc

!    initialize retrieve_stats

             if(ime-ims+1.ne.nxc.or.jme-jms+1.ne.nyc) write(0,*)' mype,ims,ime,nxc,jms,jme,nyc=', &
                                            mype,ims,ime,nxc,jms,jme,nyc
        if(mype.eq.0) write(0,*)' at 1 in get_aspect'
  call retrieve_xstats0(instats_x)
  call retrieve_fstats0(instats_f)
        if(mype.eq.0) write(0,*)' at 2 in get_aspect'

  h0=.01
      tgmax=-huge(tgmax)
      tgmin=huge(tgmin)
      errmax=0.
                           if(mype.eq.0) print *,' ids,ide,jds,jde,kds,kde=',ids,ide,jds,jde,kds,kde
  iplot=0
  jplot=0
  kplot=0
  aspectplot=0.
  do k=kps,kpe
      rlenmax=-huge(rlenmax)
      rlenmin=huge(rlenmin)
      aspectmax=-huge(aspectmax)
      aspectmin=huge(aspectmin)
   km=min(k+1,kde)
   kp=max(k-1,kds)
   delzi=1./(kp-km)
   do j=jps,jpe
    jm=min(j+1,jde)
    jp=max(j-1,jds)
    delyi=1./(jp-jm)
    do i=ips,ipe
     im=min(i+1,ide)
     ip=max(i-1,ids)
     delxi=1./(ip-im)
     dqx=delxi*(qg(ip,j,k)-qg(im,j,k))
     dqy=delyi*(qg(i,jp,k)-qg(i,jm,k))
     dqz=delzi*(qg(i,j,kp)-qg(i,j,km))
     dqx2=dqx**2
     dqy2=dqy**2
     dqz2=dqz**2
     dqnorm=sqrt(dqx2+dqy2+dqz2)
     rldqnorm=log10(max(dqnorm,.0078476))
     iv3=nint((rldqnorm+2.)*19.*.25+1.)
     iv3=min(20,max(1,iv3))
     rscale_delta=-2.+4.*(iv3-1.)/19.
     dqref=10.**rscale_delta
     sig_thetas=abs((tg(i,j,k)-tg(i,j,ksfc(i,j)))/tg(i,j,ksfc(i,j)))
         tgmax=max(tg(i,j,k),tgmax)
         tgmin=min(tg(i,j,k),tgmin)
     unorm=sqrt(ug(i,j,k)**2+vg(i,j,k)**2)
     call retrieve_xstats(sig_thetas,unorm,rldqnorm,stdx,qlxs)
     h=h0*sqrt(1.+(dqnorm/h0)**2)
     href=h0*sqrt(1.+(dqref/h0)**2)
     qlx=href*qlxs
     call retrieve_fstats(sig_thetas,unorm,rldqnorm,stdf,qlfs)
     qlf=dqref*qlfs
!    a1=.5*( (h/qlx)**2 + dqx*dqx/qlf**2 )*inflate_aspect_inv
!    a2=.5*( (h/qlx)**2 + dqy*dqy/qlf**2 )*inflate_aspect_inv
!    a3=.5*( (h/qlx)**2 + dqz*dqz/qlf**2 )*inflate_aspect_inv
!    a4=.5*(              dqy*dqz/qlf**2 )*inflate_aspect_inv
!    a5=.5*(              dqx*dqz/qlf**2 )*inflate_aspect_inv
!    a6=.5*(              dqx*dqy/qlf**2 )*inflate_aspect_inv
     a1=.5*( inflate_aspect_inv*(h/qlx)**2 + dqx*dqx/qlf**2 )
     a2=.5*( inflate_aspect_inv*(h/qlx)**2 + dqy*dqy/qlf**2 )
     a3=.5*( inflate_aspect_inv*(h/qlx)**2 + dqz*dqz/qlf**2 )
     a4=.5*(                                 dqy*dqz/qlf**2 )
     a5=.5*(                                 dqx*dqz/qlf**2 )
     a6=.5*(                                 dqx*dqy/qlf**2 )
     biga1=a2*a3-a4*a4
     biga2=a1*a3-a5*a5
     biga3=a1*a2-a6*a6
     biga4=a5*a6-a1*a4
     biga5=a4*a6-a2*a5
     biga6=a4*a5-a3*a6
     deta=a1*biga1+a6*biga6+a5*biga5
     aspect(1,i,j,k)=biga1/deta
     aspect(2,i,j,k)=biga2/deta
     aspect(3,i,j,k)=biga3/deta
     aspect(4,i,j,k)=biga4/deta
     aspect(5,i,j,k)=biga5/deta
     aspect(6,i,j,k)=biga6/deta
     aspect(7,i,j,k)=0.

!            check that inverse is correct

        err1=abs(a1*aspect(1,i,j,k)+a6*aspect(6,i,j,k)+a5*aspect(5,i,j,k)-1.)
        err2=abs(a1*aspect(6,i,j,k)+a6*aspect(2,i,j,k)+a5*aspect(4,i,j,k)   )
        err3=abs(a1*aspect(5,i,j,k)+a6*aspect(4,i,j,k)+a5*aspect(3,i,j,k)   )
        err4=abs(a6*aspect(6,i,j,k)+a2*aspect(2,i,j,k)+a4*aspect(4,i,j,k)-1.)
        err5=abs(a6*aspect(5,i,j,k)+a2*aspect(4,i,j,k)+a4*aspect(3,i,j,k)   )
        err6=abs(a5*aspect(5,i,j,k)+a4*aspect(4,i,j,k)+a3*aspect(3,i,j,k)-1.)
        errmax=max(err1,err2,err3,err4,err5,err6,errmax)
           atest(1)=aspect(1,i,j,k)
           atest(3)=aspect(2,i,j,k)
           atest(6)=aspect(3,i,j,k)
           atest(5)=aspect(4,i,j,k)
           atest(4)=aspect(5,i,j,k)
           atest(2)=aspect(6,i,j,k)
           call eigen(atest,rtest,3,0)
           rlen1=sqrt(atest(1))
           rlen2=sqrt(atest(3))
           rlen3=sqrt(atest(6))
             if(rlen1.ge.max(rlen2,rlen3)) imax=1
             if(rlen2.ge.max(rlen1,rlen3)) imax=2
             if(rlen3.ge.max(rlen1,rlen2)) imax=3
                
           aspectthis=max(rlen1,rlen2,rlen3)/min(rlen1,rlen2,rlen3)
               if(mod(i,50).eq.0.and.mod(j,50).eq.0.and.k.ge.12.and.k.le.27) then
                   iipl=1+i/50
                   jjpl=1+j/50
                   if(aspectthis.gt.aspectplot(iipl,jjpl)) then
                    aspectplot(iipl,jjpl)=aspectthis
                    iplot(iipl,jjpl)=i
                    jplot(iipl,jjpl)=j
                    kplot(iipl,jjpl)=k
                   end if
               end if
           if(aspectthis.gt.aspectmax) then
              anglemax=acosd(abs(rtest(3,imax))/sqrt(rtest(1,imax)**2+rtest(2,imax)**2+rtest(3,imax)**2))
              iasmax=i
              jasmax=j
              kasmax=k
              aspectmax=aspectthis
              rlenmin_as=min(rlen1,rlen2,rlen3)
              rlenmax_as=max(rlen1,rlen2,rlen3)
           end if
           aspectmin=min(aspectthis,aspectmin)
           rlenmax=max(max(rlen1,rlen2,rlen3),rlenmax)
           rlenmin=min(min(rlen1,rlen2,rlen3),rlenmin)
     amp(i,j,k)=.01*sqrt(.5*(stdx**2+stdf**2))
    end do
   end do
        call mpi_reduce(rlenmax,rlenmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
        call mpi_reduce(rlenmin,rlenminall,1,mpi_real4,mpi_min,0,my_comm,ierr)
        call mpi_reduce(aspectmax,aspectmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
        call mpi_reduce(aspectmin,aspectminall,1,mpi_real4,mpi_min,0,my_comm,ierr)
!        if(mype.eq.0) then
!         print '('' k,rlenmin,max='',i3,2f13.5)',k,rlenminall,rlenmaxall
!         print '('' k,aspectmin,max='',i3,2f13.5)',k,aspectminall,aspectmaxall
!        end if
!            if(k.eq.20) &
!              print *,' k,mype,i,j,rlenmin,max,aspect,anglemax=', &
!                  k,mype,iasmax,jasmax,rlenmin_as,rlenmax_as,aspectmax,anglemax
  end do
        call mpi_allreduce(aspectplot,aspectplotall,400,mpi_real4,mpi_max,my_comm,ierr)
        call mpi_allreduce(iplot,iplotall,400,mpi_integer4,mpi_max,my_comm,ierr)
        call mpi_allreduce(jplot,jplotall,400,mpi_integer4,mpi_max,my_comm,ierr)
        call mpi_allreduce(kplot,kplotall,400,mpi_integer4,mpi_max,my_comm,ierr)
        aspectplot=aspectplotall
        iplot=iplotall
        jplot=jplotall
        kplot=kplotall
          if(mype.eq.0) then
           do j=1,20
            do i=1,20
             if(iplot(i,j).ne.0) print *,' i,j,k,aspect=',iplot(i,j),jplot(i,j),kplot(i,j),aspectplot(i,j)
            end do
           end do
          end if
     call stats_aspect(aspect,ips,ipe,jps,jpe,kps,kpe,mype)
        call mpi_reduce(tgmax,tgmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
        call mpi_reduce(errmax,errmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
        call mpi_reduce(tgmin,tgminall,1,mpi_real4,mpi_min,0,my_comm,ierr)
       if(mype.eq.0) then
          print '('' tgmin,max='',2f13.5)',tgminall,tgmaxall
          print *,' max inverse error=',errmaxall
       end if

return
end subroutine get_aspect

subroutine stats_aspect(aspect,ips,ipe,jps,jpe,kps,kpe,mype)

  include 'mpif.h'
      include "my_comm.h"

  real(4) aspect(7,ips:ipe,jps:jpe,kps:kpe)

  do k=kps,kpe
   xmax=0.
   xmin=huge(xmin)
   delxmax=0.
   delxbar=0.
   count=0.
   do j=jps,jpe
    do i=ips,ipe-1
     ip=i+1
     anorm=0.
     dela=0.
     xthis=sqrt(aspect(1,i,j,k))
     xmax=max(xthis,xmax)
     xmin=min(xthis,xmin)
     do m=1,6
      anorm=anorm+abs(aspect(m,i,j,k))
      dela=dela+abs(aspect(m,ip,j,k)-aspect(m,i,j,k))
     end do
     dela=dela/anorm
     delxmax=max(dela,delxmax)
     delxbar=delxbar+dela
     count=count+1.
    end do
   end do
   call mpi_reduce(count,countall,1,mpi_real4,mpi_sum,0,my_comm,ierr)
   call mpi_reduce(delxbar,delxbarall,1,mpi_real4,mpi_sum,0,my_comm,ierr)
   call mpi_reduce(delxmax,delxmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
   call mpi_reduce(xmax,xmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
   call mpi_reduce(xmin,xminall,1,mpi_real4,mpi_min,0,my_comm,ierr)
   if(mype.eq.0) then
    delxbarall=delxbarall/countall
    print *,' for k=',k,', delxbar,delxmax,xmin,xmax=',delxbarall,delxmaxall,xminall,xmaxall
   end if
  end do
  do k=kps,kpe
   ymax=0.
   ymin=huge(ymin)
   zmax=0.
   zmin=huge(zmin)
   do j=jps,jpe
    do i=ips,ipe
     ythis=sqrt(aspect(2,i,j,k))
     ymax=max(ythis,ymax)
     ymin=min(ythis,ymin)
     zthis=sqrt(aspect(3,i,j,k))
     zmax=max(zthis,zmax)
     zmin=min(zthis,zmin)
    end do
   end do
   call mpi_reduce(ymax,ymaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
   call mpi_reduce(ymin,yminall,1,mpi_real4,mpi_min,0,my_comm,ierr)
   call mpi_reduce(zmax,zmaxall,1,mpi_real4,mpi_max,0,my_comm,ierr)
   call mpi_reduce(zmin,zminall,1,mpi_real4,mpi_min,0,my_comm,ierr)
   if(mype.eq.0) then
    print *,' for k=',k,', ymin,ymax=',yminall,ymaxall
    print *,' for k=',k,', zmin,zmax=',zminall,zmaxall
   end if
  end do

return
end subroutine stats_aspect

!
!     ..................................................................
!
!        SUBROUTINE EIGEN
!
!        PURPOSE
!           COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC
!           MATRIX
!
!        USAGE
!           CALL EIGEN(A,R,N,MV)
!
!        DESCRIPTION OF PARAMETERS
!           A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
!               RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
!               MATRIX A IN DESCENDING ORDER.
!           R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
!               IN SAME SEQUENCE AS EIGENVALUES)
!           N - ORDER OF MATRICES A AND R
!           MV- INPUT CODE
!                   0   COMPUTE EIGENVALUES AND EIGENVECTORS
!                   1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE
!                       DIMENSIONED BUT MUST STILL APPEAR IN CALLING
!                       SEQUENCE)
!
!        REMARKS
!           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)
!           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           NONE
!
!        METHOD
!           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED
!           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICAL
!           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND
!           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7
!
!     ..................................................................
!
      SUBROUTINE EIGEN(A,R,N,MV)
      DIMENSION A(1),R(1)
!
!        ...............................................................
!
!        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
!        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
!        STATEMENT WHICH FOLLOWS.
!
!     DOUBLE PRECISION A,R,ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX,
!    1                 COSX2,SINCS,RANGE
!
!        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
!        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
!        ROUTINE.
!
!        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
!        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTS
!        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT
!        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD
!        BE CHANGED TO 1.0D-12.
!
!        ...............................................................
!
!        GENERATE IDENTITY MATRIX
!
    5 continue
      RANGE=1.0E-12
      if(mv.eq.1) go to 25
      IQ=-N
      DO J=1,N
       IQ=IQ+N
       DO I=1,N
        IJ=IQ+I
        R(IJ)=0.0
        if(i.ne.j) go to 20
        R(IJ)=1.0
   20   CONTINUE
       end do
      end do
!
!        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)
!
   25 continue
      ANORM=0.0
      DO I=1,N
       DO J=I,N
        if(i.eq.j) go to 35
        IA=I+(J*J-J)/2
        ANORM=ANORM+A(IA)*A(IA)
   35   CONTINUE
       end do
      end do
      if(anorm.le.0.) go to 165
      ANORM=1.414*SQRT(ANORM)
      ANRMX=ANORM*RANGE/FLOAT(N)
!
!        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR
!
      IND=0
      THR=ANORM
   45 continue
      THR=THR/FLOAT(N)
   50 continue
      L=1
   55 continue
      M=L+1
!
!        COMPUTE SIN AND COS
!
   60 continue
      MQ=(M*M-M)/2
      LQ=(L*L-L)/2
      LM=L+MQ
   62 continue
      if(abs(a(lm))-thr.lt.0.) go to 130
      IND=1
      LL=L+LQ
      MM=M+MQ
      X=0.5*(A(LL)-A(MM))
   68 continue
      Y=-A(LM)/ SQRT(A(LM)*A(LM)+X*X)
      if(x.ge.0.) go to 75
      Y=-Y
!DP75 SINX=Y/ SQRT(2.0*(1.0+( SQRT(1.0-Y*Y))))
   75 continue
      SINX=Y/ SQRT(2.0*(1.0+( SQRT(MAX(0.,1.0-Y*Y)))))
      ONEMYY=1.0-Y*Y
      IF(1.0-Y*Y.LT.0.) write(0,*)' IN EIGEN, 1-Y*Y=',ONEMYY
      SINX2=SINX*SINX
!DP78 COSX= SQRT(1.0-SINX2)
   78 continue
      COSX= SQRT(MAX(0.,1.0-SINX2))
      COSX2=COSX*COSX
      SINCS =SINX*COSX
!
!        ROTATE L AND M COLUMNS
!
      ILQ=N*(L-1)
      IMQ=N*(M-1)
      DO 125 I=1,N
       IQ=(I*I-I)/2
       if(i.eq.l) go to 115
       if(i.eq.m) go to 115
       if(i.gt.m) go to 90
       IM=I+MQ
       GO TO 95
    90 continue
       IM=M+IQ
    95 continue
       if(i.ge.l) go to 105
       IL=I+LQ
       GO TO 110
   105 continue
       IL=L+IQ
   110 continue
       X=A(IL)*COSX-A(IM)*SINX
       A(IM)=A(IL)*SINX+A(IM)*COSX
       A(IL)=X
   115 continue
       if(mv.eq.1) go to 125
       ILR=ILQ+I
       IMR=IMQ+I
       X=R(ILR)*COSX-R(IMR)*SINX
       R(IMR)=R(ILR)*SINX+R(IMR)*COSX
       R(ILR)=X
  125 CONTINUE
      X=2.0*A(LM)*SINCS
      Y=A(LL)*COSX2+A(MM)*SINX2-X
      X=A(LL)*SINX2+A(MM)*COSX2+X
      A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
      A(LL)=Y
      A(MM)=X
!
!        TESTS FOR COMPLETION
!
!        TEST FOR M = LAST COLUMN
!
  130 continue
      if(m.eq.n) go to 140
      M=M+1
      GO TO 60
!
!        TEST FOR L = SECOND FROM LAST COLUMN
!
  140 continue
      if(l.eq.n-1) go to 150
      L=L+1
      GO TO 55
  150 continue
      if(ind.ne.1) go to 160
      IND=0
      GO TO 50
!
!        COMPARE THRESHOLD WITH FINAL NORM
!
  160 continue
      if(thr.gt.anrmx) go to 45
!
!        SORT EIGENVALUES AND EIGENVECTORS
!
  165 continue
      IQ=-N
      DO I=1,N
       IQ=IQ+N
       LL=I+(I*I-I)/2
       JQ=N*(I-2)
       DO J=I,N
        JQ=JQ+N
        MM=J+(J*J-J)/2
        if(a(ll).ge.a(mm)) go to 1970
        X=A(LL)
        A(LL)=A(MM)
        A(MM)=X
        if(mv.eq.1) go to 1970
        DO K=1,N
         ILR=IQ+K
         IMR=JQ+K
         X=R(ILR)
         R(ILR)=R(IMR)
         R(IMR)=X
        end do
1970    continue
       end do
      end do
      RETURN
      END
