PROGRAM summer
  IMPLICIT none
  INTEGER i, n
  PARAMETER(n = 1024*1024)
  REAL x(n), sum
  INTEGER tid, nthreads, OMP_GET_THREAD_NUM

  DO i = 1, n
    x(i) = i
  ENDDO

!$OMP PARALLEL PRIVATE(nthreads, tid)
  !PRINT *,'hello from thread ', OMP_GET_THREAD_NUM()
  sum = 0.
  DO i = 1, n
    sum = sum + x(i)
  ENDDO
!$OMP END PARALLEL

  PRINT *,'sum = ',sum

END
