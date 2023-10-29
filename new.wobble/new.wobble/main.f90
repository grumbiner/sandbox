      PROGRAM wobble_main

      INCLUDE "astronomy.inc"

      INTEGER n, nfreq, nomega
!      PARAMETER (n = 16377)
      PARAMETER (n = 75972) !Reanalysis to dec 31 1999, 6 hourly
      DOUBLE PRECISION jd(n), x(n), y(n), utc(n), lod(n), r(n), rdot(n)
      PARAMETER (nfreq = 2000)
      DOUBLE PRECISION omega(0:nfreq-1), a(0:nfreq-1), b(0:nfreq-1)

      REAL stepsize
!      PARAMETER (stepsize = 0.25) ! 6 hourly info
      PARAMETER (stepsize = 1.0)

      INTEGER i
   
      CALL read_data("gamma", n, jd, x, y, utc, lod, r, rdot) 
      CALL set_freqs(omega, nfreq, nomega)

      CALL harmrm(x, n, omega, a, b, nomega)

      PRINT *,'harmonic spectrum'
      DO i = 0, nomega-1
        WRITE (*,9001) stepsize*2.*PI/omega(i), omega(i), a(i), b(i), sqrt(a(i)*a(i)+b(i)*b(i)), atan2(b(i),a(i))
      ENDDO
 9001 FORMAT(F9.3,E18.8,2x,3E18.8,E24.16)

      ! Extract the harmonics analyzed
      CALL extract(x, n, omega, a, b, nomega)

      ! Print residual:
      DO i = 1, n
        PRINT *,i, x(i)
      ENDDO

      END


      SUBROUTINE set_freqs(omega, nfreq, nomega)
      INCLUDE "astronomy.inc"
      INTEGER nomega, nfreq
      DOUBLE PRECISION omega(0:nfreq-1)

      omega(0) = 0.0
      omega(1) = omega_e_anom
      omega(2) = 2*omega_e_anom
      omega(3) = 3*omega_e_anom
      omega(4) = 4.*omega(1)
      omega(5) = omega_day + omega_e_sid
      omega(6) = (omega_day - omega_moon)*2. ! semi-diurnal lunar
      omega(7) = omega_day
      omega(8) = omega_day*2
      nomega = 9

!! Alternate:
!      omega(0) = 0.0;
!      omega(1) = omega_e_sid
!      omega(2) = 2.*omega(1)
!      omega(3) = omega_e_sid - omega_j
!      omega(4) = 2.*(omega_v - omega_e_sid)
!      omega(5) = omega_e_sid - omega_s
!      omega(6) = omega_e_sid - omega_u
!      omega(7) = omega_e_sid - omega_n
!      omega(8) = omega(3) * 2  ! jupiter half period
!      omega(9) = omega(4) / 2. ! venus double
!      omega(10) = omega_j
!!      omega(11) = omega_e_sid - omega_j - omega_s
!!      omega(12) = omega(11)*2.
!!      omega(13) = omega_e_sid - 2.*omega_s
!!      omega(14) = omega(13)*2.
!!      omega(15) = omega_e_sid - omega_j + omega_s
!!      omega(16) = omega(15)*2.
!!      omega(17) = omega_e_sid - 2.*omega_j
!!      omega(18) = omega(17)*2.
!!1  1   583.9
!!2  2   292.0
!!2  3  1456 (much better than leap year cycle!)
!!3  3   194.6
!!3  4   416.7
!!6  8   208.3
!!8 11   182.25
!!8 12   363.74
!      omega(11) = omega_e_sid * 3.
!      omega(12) = 2*omega_v - 3*omega_e_sid
!      omega(13) = 3.*(omega_v - omega_e_sid)
!      omega(14) = 3*omega_v - 4.*omega_e_sid
!      omega(15) = 2.*omega(14)
!      omega(16) = 4.*omega(12)
!      omega(17) = 8*omega_v - 11*omega_e_sid
!      nomega = 18

! Alternate:
!      omega(0) = 0.0;
!      !omega(1) = omega_e_anom
!      omega(1) = omega_e_sid
!      !omega(2) = 8*omega_v - 13*omega(1)
!      omega(2) = 6*omega_v - 9*omega(1)
!      omega(3) = 3*omega_v - 4*omega(1)
!      omega(4) = 2*omega_v - 3*omega(1)
!      omega(5) = 7*omega_v - 11*omega(1)
!      omega(6) = 5*omega_v - 8*omega(1)
!      omega(7) =   omega_v -   omega(1)
!      omega(8) = omega(1) - omega_j
!      omega(9) = omega(1) - 2*omega_j
!      omega(10) = omega(1) - 3*omega_j
!      omega(11) = 2*omega(8)
!      omega(12) = 2*omega(1)
!      omega(13) = 2*omega(7)
!      omega(14) = 3*omega(7)
!      omega(15) = 4*omega(7)
!      omega(16) = omega(1) - omega_s
!      omega(17) = omega(1) - 2*omega_s
!      omega(18) = omega(1) - 3*omega_s
!      omega(19) = 2*omega(16)
!      omega(20) = 3*omega(16)
!      omega(21) = omega_j
!      omega(22) = 2*omega_j
!      omega(23) = omega(1) - omega_j - omega_s
!      omega(24) = omega_moon - omega_perigee
!      omega(25) = 2.*omega_moon
!      omega(26) = 3.*omega(24)
!      omega(27) = 4.*omega(24)
!      omega(28) = 5.*omega(24)
!
!      m = 29

!! Alternate:
!      omega(0) = 0.0;
!      !omega(1) = omega_e_anom
!      omega(1) = omega_e_sid
!      omega(2) = 2.*omega(1)
!      omega(3) = 3.*omega(1)
!      omega(4) = omega_e_sid - omega_j
!      omega(5) = 2.*(omega_v - omega_e_sid)
!      omega(6) = omega(4) * 2  ! jupiter synodic half period
!      omega(7) = omega(5) / 2. ! venus double
!      omega(8) = 2*omega_v - 3*omega_e_sid
!      omega(9) = 3*omega_v - 4*omega_e_sid
!      omega(10) = omega_e_sid - 2.*omega_j
!      omega(11) = omega_e_sid - omega_mars
!      omega(12) = 2.*omega(11)
!      omega(13) = 2.*PI*3./81.5 !a dummy, bogus, period
!!      omega(8) = omega_j
!!      omega(11) = omega_e_sid - omega_j - omega_s
!!      omega(13) = omega_e_sid - 2.*omega_s
!!      omega(15) = omega_e_sid - omega_j + omega_s
!!      omega(17) = omega_e_sid - 2.*omega_j
!!      omega(11) = omega_e_sid * 3.
!!      omega(13) = 3.*(omega_v - omega_e_sid)
!!      omega(17) = 8*omega_v - 11*omega_e_sid
!      nomega = 14

! Alternate:
!      omega(0) = 0.0;
!      omega(1) = omega_e_sid
!      omega(2) = 2.*omega(1)
!      omega(3) = omega_e_sid - omega_j
!      omega(4) = 2.*omega(3)
!      omega(5) = 2.*(omega_v - omega_e_sid)
!      omega(6) = 0.5*omega(5)
!      omega(7) = omega_e_sid - omega_s
!      omega(8) = 2.*omega(7)
!
!      nomega = 9

! Alternate -- doodson-like numbers
!      OPEN (20,FILE="doodson", FORM="FORMATTED")
!      READ (20,*) todo
!      DO i = 1, todo
!        READ (20,*) day(i), month(i), year(i), venus(i), jupiter(i)
!        omega(i) = day(i)     * omega_day +     &
!                   month(i)   * omega_moon +     &
!                   year(i)    * omega_e_anom +     &
!                   venus(i)   * omega_v +     &
!                   jupiter(i) * omega_j
!      ENDDO
!      CLOSE(20)
!
!      nomega = todo


      RETURN
      END


      SUBROUTINE read_data(fname, n, jd, x, y, utc, lod, r, rdot) 
      IMPLICIT none
      CHARACTER(*), intent(in) :: fname
      INTEGER n
      DOUBLE PRECISION jd(n), x(n), y(n), utc(n), lod(n), r(n), rdot(n)

      INTEGER i

      OPEN (10,FILE="gamma", FORM="FORMATTED")
      DO i = 1, n
        READ (10,*) jd(i), x(i), y(i), utc(i), lod(i), r(i), rdot(i)
      ENDDO

      RETURN
      END

