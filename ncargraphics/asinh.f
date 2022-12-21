      FUNCTION ASINH(X)                                                 00000010
      DATA SM,BIG/4.47E-8,2047.5/,VBG/2011./                            00000020
      IF(X.GE.VBG) GO TO 16                                             00000030
      IF(X.LE.-VBG) GO TO 17                                            00000040
      XX = X*X                                                          00000050
      IF(XX.GT.SM) GO TO 10                                             00000060
      ASINH = X                                                         00000070
      RETURN                                                            00000080
   10 AA = 1.                                                           00000090
      IF(X.LT.0.0) AA =-1.                                              00000100
   11 IF(XX-0.5625) 12,13,14                                            00000110
   12 XX = 4.0*XX*(1. + XX)                                             00000120
      AA = 0.5*AA                                                       00000130
      GO TO 11                                                          00000140
   13 ASINH = 0.6931471806*AA                                           00000150
      RETURN                                                            00000160
   14 IF(XX.GE.BIG) GO TO 15                                            00000170
      ASINH = ALOG(SQRT(XX) + SQRT(XX+1.))*AA                           00000180
      RETURN                                                            00000190
   15 ASINH = ALOG(2.*SQRT(XX + 0.5))*AA                                00000200
      RETURN                                                            00000210
   16 ASINH = 0.6931471806+ALOG(X)                                      00000220
      RETURN                                                            00000230
   17 ASINH =-0.6931471806 - ALOG(-X)                                   00000240
      RETURN                                                            00000250
      END                                                               00000260
