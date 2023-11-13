                EVALUATE DATE-9                                         00000024
                   WHEN ZERO                                            00000025
                      MOVE ZERO TO Y2K-WK-DATE2-9                       00000026
                   WHEN 999999                                          00000027
                      MOVE 99999999 TO Y2K-WK-DATE2-9                   00000028
                   WHEN OTHER                                           00000029
                      MOVE DATE-9 TO Y2K-WK-DATE2-9-YYMMDD              00000030
                      IF Y2K-WK-DATE2R-9-YY LESS THAN Y2K-WK-CUTOFF-YR-900000031
                         MOVE 20 TO Y2K-WK-DATE2-9-CC                   00000032
                      ELSE                                              00000033
                         MOVE 19 TO Y2K-WK-DATE2-9-CC                   00000034
                      END-IF                                            00000035
                END-EVALUATE                                            00000036
