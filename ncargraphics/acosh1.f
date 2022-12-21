      FUNCTION ACOSH1(X)                                                00000010
C     ACOSH1(X) = AR COSH (X+1)                                         00000020
      DATA SM,BIG/8.94E-8,2010./                                        00000030
      IF(X) 10,11,12                                                    00000040
   10 ACOSH1 = -1000.                                                   00000050
      RETURN                                                            00000060
   11 ACOSH1 = 0.0                                                      00000070
      RETURN                                                            00000080
   12 IF(X.GT.SM) GO TO 13                                              00000090
      ACOSH1= SQRT(2.*X)                                                00000100
      RETURN                                                            00000110
   13 XX = X                                                            00000120
      AA = 1.                                                           00000130
   14 IF(XX-0.25) 15,16,17                                              00000140
   15 XX = 2.*XX*(2. + XX)                                              00000150
      AA = 0.5*AA                                                       00000160
      GO TO 14                                                          00000170
   16 ACOSH1 = 0.6931471806*AA                                          00000180
      RETURN                                                            00000190
   17 IF(XX.GT.BIG) GO TO 18                                            00000200
      ACOSH1 = ALOG(XX + 1. + SQRT(XX*(XX+2.)))*AA                      00000210
      RETURN                                                            00000220
   18 ACOSH1 = 0.6931471806+ALOG(XX+1.)                                 00000230
      RETURN                                                            00000240
      END                                                               00000250
