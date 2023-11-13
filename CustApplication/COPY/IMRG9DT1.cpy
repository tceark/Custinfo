      *****************************************************             00000010
      * 09/1998 - CHANGES MADE FOR YEAR 2000 COMPLIANCE - IMR   *       00000020
      *                                                         *       00000030
      *IMRG9DT1 - CODE TO WINDOW A GREGORIAN NUMERIC DATE       *       00000040
      *            USING IMR DATE FIELD Y2K-WK-DATE1-9          *       00000050
      ***********************************************************       00000060
                EVALUATE DATE-9                                         00000070
                   WHEN ZERO                                            00000080
                      MOVE ZERO TO Y2K-WK-DATE1-9                       00000090
                   WHEN 999999                                          00000100
                      MOVE 99999999 TO Y2K-WK-DATE1-9                   00000110
                   WHEN OTHER                                           00000120
                      MOVE DATE-9 TO Y2K-WK-DATE1-9-YYMMDD              00000130
                      IF Y2K-WK-DATE1R-9-YY LESS THAN Y2K-WK-CUTOFF-YR-900000140
                         MOVE 20 TO Y2K-WK-DATE1-9-CC                   00000150
                      ELSE                                              00000160
                         MOVE 19 TO Y2K-WK-DATE1-9-CC                   00000170
                      END-IF                                            00000180
                END-EVALUATE                                            00000190
