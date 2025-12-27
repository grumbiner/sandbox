      SUBROUTINE CVDIAG(AVEN,AVES,CV,CVT,CVB)
      DIMENSION CV( 256 ),CVT( 256 ),CVB( 256 )
      DIMENSION AVEN(3),AVES(3)
      NPTNOR=0
      NPTSOU=0
      DO 10 K=1,3
        AVEN(K)=0. E 0
        AVES(K)=0. E 0
   10 CONTINUE
      DO 2 I=1, 128
        AVEN(1)=AVEN(1)+CV(I)
        AVES(1)=AVES(1)+CV(I+ 128 )
        IF(CV(I).LE.0. E 0) GO TO 1
        AVEN(2)=AVEN(2)+CVT(I)
        AVEN(3)=AVEN(3)+CVB(I)
        NPTNOR = NPTNOR + 1
    1   IF(CV(I+ 128 ).LE.0. E 0) GO TO 2
        AVES(2)=AVES(2)+CVT(I+ 128 )
        AVES(3)=AVES(3)+CVB(I+ 128 )
        NPTSOU = NPTSOU + 1
    2 CONTINUE
      AVEN(1)=AVEN(1)/ 128
      AVES(1)=AVES(1)/ 128
      IF(NPTNOR.GT.0) THEN
        AVEN(2)=AVEN(2)/NPTNOR
        AVEN(3)=AVEN(3)/NPTNOR
      ENDIF
      IF(NPTSOU.GT.0) THEN
        AVES(2)=AVES(2)/NPTSOU
        AVES(3)=AVES(3)/NPTSOU
      ENDIF
      RETURN
      END
