      FUNCTION PSI(P)                                                   00000010
      IF(P) 5,6,40                                                      00000020
    5 S = SHR(P)                                                        00000030
      SA = 1. - S                                                       00000040
      SB = 1. + S                                                       00000050
      PSI = SA - 2.*ATAN(SA/SB)+ALOG(SB*SB*(1.+S*S)/(2.*S)**3)          00000060
      RETURN                                                            00000070
    6 PSI = 0.0                                                         00000080
      RETURN                                                            00000090
   40 PSI = -5.*ASINH(1.4*P)                                            00000100
      RETURN                                                            00000110
      END                                                               00000120
