!---------------------------------------------------------------
LOGICAL FUNCTION hfok(obs)
  IMPLICIT none
  REAL(8) obs(5,2) ! i = parameters, 5 = tmbr; j = channel
  LOGICAL tmp
  tmp = .TRUE.
  IF (obs(5,1) > 285 .OR. obs(5,2) > 285. ) tmp = .FALSE.
  IF (obs(5,1) > obs(5,2) ) tmp = .FALSE.
  hfok = tmp
  RETURN 
  END

LOGICAL FUNCTION lfok(obs)
  IMPLICIT none
  REAL(8) obs(5,12) ! i = parameters, 5 = tmbr; j = channel
  LOGICAL tmp
  INTEGER i
  tmp = .TRUE.

  DO i = 1, 6
    IF (obs(5,2*i-1) > 285 .OR. obs(5,2*i) > 285 ) tmp = .FALSE.
    IF (obs(5,2*i-1) > obs(5,2*i) ) tmp = .FALSE.
  ENDDO

  lfok = tmp
  RETURN 
  END

LOGICAL FUNCTION weatherok(obs, lat) 
! Weather filter.  Should only apply after regressions to AMSRE have
! been performed.
  IMPLICIT none
  REAL AMSR_GR37LIM, AMSR_GR24LIM
  PARAMETER (AMSR_GR37LIM = 0.046)
  PARAMETER (AMSR_GR24LIM = 0.045)
  REAL(8) obs(5,12), amsre(5,12)
  REAL(8) lat
  LOGICAL tmp
  REAL gr3719, gr2419

  amsre = obs
  CALL regress(amsre, lat)
  tmp = .FALSE.
  gr3719 = (amsre(5,12) - amsre(5,8) ) / (amsre(5,12)+amsre(5,8))
  gr2419 = (amsre(5,10) - amsre(5,8) ) / (amsre(5,10)+amsre(5,8))

  IF ((gr3719 < AMSR_GR37LIM) .AND. (gr2419 < AMSR_GR24LIM) ) THEN
    tmp = .TRUE.
  ENDIF
  weatherok = tmp
  RETURN
  END

SUBROUTINE regress(amsre, lat) 
  IMPLICIT none
  REAL(8) amsre(5,12)
  REAL(8) lat
!Perform limited regression -- just those channels that are used in
!weather filter
  IF (lat > 0) THEN
    amsre(5, 8) = amsre(5, 8)*1.031 - 9.710
    amsre(5,10) = amsre(5,10)*0.999 - 1.706
    amsre(5,12) = amsre(5,12)*0.997 - 2.610
  ELSE
    amsre(5, 8) = amsre(5, 8)*1.032 -10.013
    amsre(5,10) = amsre(5,10)*0.993 - 0.987
    amsre(5,12) = amsre(5,12)*0.995 - 2.400
  ENDIF

  RETURN
  END
