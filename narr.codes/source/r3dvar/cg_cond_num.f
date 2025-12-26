      subroutine cg_cond_num(K,Z0,ALPHAT,BETAT)

  !------------------------------------------------------------------------------
  ! Find the Kth (ie., the last) zero, z0, of the orthogonal
  ! polynomial of degree K implied by the series, ALPHAT, BETAT, from CG method.
  ! This is slightly less than the maximum eigenvalue of the problem
  ! being solved, and thus an estimate of its condition number.
  !------------------------------------------------------------------------------

  integer(4) k
  real(8) z0,alphat,betat
  real(8) zir(200),dzir(200)

      call proots(k,alphat,betat,zir,dzir,ierror)
      if(ierror.eq.0) z0=zir(k)
      if(ierror.ne.0) z0=-1000._8
      return
      end subroutine cg_cond_num

!------------------------------------------------------------------------------
!      SUBROUTINE PROOTS
! Search for all the roots of the Mth-degree orthogonal polynomial implied by
! the alpha and beta series output from a conjugate gradient method.
!
! --> M:             Degree
! --> ALPHAT, BETAT: Alpha and beta series output from a CG scheme.
! <-- ZIR:           Roots
! <-- DZIR:          Derivative of the polynomial at each root.
!------------------------------------------------------------------------------
      subroutine proots(m,alphat,betat,zir,dzir,ierror)

  integer,parameter::nit=20,nl=20
  integer(4) m
  real(8) alphat(0:*),betat(0:*),zir(*),dzir(*)

  integer(4) l,j,k
  real(8) t,azret,dzret,drre,rre,zre,drle,rle,zle,s,r,onem6

     ierror=0
  onem6=(10._8)**(-6)
      if(m.le.0)return
      zir(1)=1./alphat(0)
      call orthop(zir(1),r,dzir(1),1,alphat,betat)
      if(m.eq.1)return
      do k=2,m
       s=1.
       zle=0._8
       call orthop(zle,rle,drle,k,alphat,betat)
       if(s*rle.le.0._8)then
        write(6,802)k
        ierror=1
        return
       endif
       do j=1,k-1
        s=-s        ! s=(-1)**j  = 1 for even, = -1 for odd j.
        zre=zir(j)
        call orthop(zre,rre,drre,k,alphat,betat)
        if(s*rre.lt.0._8)then
         dzret=-rre/drre
         write(6,803)k,j,rre,dzret
         azret=abs(dzret)
         if(azret.lt.onem6)dzret=onem6*dzret/azret
       ! if(azret.lt.1.d-6)dzret=(1.d-6)*dzret/azret
         zre=zre+2*dzret
         call orthop(zre,rre,drre,k,alphat,betat)
         if(s*rre.lt.0._8)then
          dzret=-rre/drre
          write(6,803)k,j,rre,dzret
          zre=zre+2*dzret
          call orthop(zre,rre,drre,k,alphat,betat)
          print *,' problem still present even after perturbation'
          ierror=1
          return
         endif
        endif
        call hunt(zle,zre,zir(j),dzir(j),k,alphat,betat,ierror)
        if(ierror.ne.0) return
        zle=zre
        rle=rre
       enddo

! Set tentative right-margin to 1.:
       t=1._8
! keep doubling the width, t, of the margin at right until it contains a zero:
       do l=1,nl
        zre=zle+t
        call orthop(zre,rre,drre,k,alphat,betat)
        if(s*rre.lt.0._8)goto 300
        t=t*2._8
       enddo
       write(6,801)nl,k
       ierror=1
       return
300    call hunt(zle,zre,zir(k),dzir(k),k,alphat,betat,ierror)
        if(ierror.ne.0) return
      enddo
      return
 
801   format(' even after',i5,' doublings of right-margin width,'/ &
      ' the last zero of the orthogonal polynomial of degree,',i5/ &
      ' is not found within this margin')
802   format(' in PROOTS, the orthogonal polynomial of degree',i5/ &
      ' is found to have erroneous sign at z=0')
803   format(' in PROOTS, the orthogonal polynomial of degree',i5/ &
      ' is found to have erroneous sign at root,',i5,' of the'/ &
      ' preceding polynomial. The value of the polynomial here is'/ &
      1x,e12.6/'  the estimated distance to the nearest zero is '/ &
      1x,e12.6)
      end subroutine proots

!------------------------------------------------------------------------------
!      SUBROUTINE HUNT
! Use both binary-search and (when close enough) Newton's method to locate
! the zero of the polynomial of degree K implied by series ALPHAT, BETAT,
! when this zero is known to reside within the interval (zle,zre).
!
! --> ZLE,ZRE: Points at left and right of the interval known to contain a zero
! <-- Z, DRZ:  The zero and the derivative of the polynomial there.
! --> K:       The degree of the orthogonal polynomial
! --> ALPHAT, BETA: The coefficents from the CG iterations which imply the
!                   sequence of orthogonal polynomials.
!------------------------------------------------------------------------------
      subroutine hunt(zle,zre,z,drz,k,alphat,betat,ierror)

  integer,parameter::nit=25
  integer(4) k
  real(8) zle,zre,z,drz
  real(8) alphat(0:*),betat(0:*)

  integer(4) i
  real(8) eps,dz,rz,zm,drr,rr,drl,rl,zcrit,zr,zl

      ierror=0
      eps=epsilon(eps)**.7_8
!     eps=epsilon(1.0)**.7
!     eps=1.e-12
      zl=zle
      zr=zre
      if(zr-zl.le.0._8) then
       print *, 'improper initial interval sent to HUNT'
       ierror=1
       return
      end if
      zcrit=(zr-zl)*eps
      call orthop(zl,rl,drl,k,alphat,betat)
      call orthop(zr,rr,drr,k,alphat,betat)
      if(rl*rr.ge.0._8)then
       print'('' initial bounds in HUNT do not bracket single zero'')'
       ierror=1
       return
      endif
      do i=1,nit
       zm=(zl+zr)/2._8
400     call orthop(zm,rz,drz,k,alphat,betat)
       if(drz.ne.0._8)then
        dz=-rz/drz
        z=zm+dz
        if(abs(dz).le.zcrit)return
        if(z.gt.zl.and.z.lt.zr)then
         if(dz.gt.0._8)then
          zl=zm
         else
          zr=zm
         endif
         zm=z
         goto 400
        endif
       endif
       if(rz*rl.le.0._8)then
        zr=zm
        rr=rz
       else
        zl=zm
        rl=rz
       endif
      enddo
      print'('' all'',i3,'' binary search iterations in HUNT used'')', &
                 nit
      z=zm
      return
      end subroutine hunt

!------------------------------------------------------------------------------
!     SUBROUTINE ORTHOP
! Evaluate the IORth-degree orthogonal polynomial, RZT, and its derivative,
! DRZT, at abscissa, ZT, implied by the series of alphas and betas,
! ALPHAT, BETAT, output from the conjugate-gradient algorithm.
!------------------------------------------------------------------------------
      subroutine orthop(zt,rzt,drzt,ior,alphat,betat)

  integer(4) ior
  real(8) zt,rzt,drzt
  real(8) alphat(0:*),betat(0:*)

  integer(4) k
  real(8) alpha,beta,dpzt,pzt
      pzt=0._8
      dpzt=0._8
      rzt=1._8
      drzt=0._8
      beta=0._8
      do k=0,ior-1
       dpzt=-drzt+dpzt*beta
       pzt=-rzt+pzt*beta
       beta=betat(k)
       alpha=alphat(k)
       drzt=drzt+(pzt+zt*dpzt)*alpha
       rzt=rzt+zt*pzt*alpha
      enddo
      return
      end subroutine orthop
