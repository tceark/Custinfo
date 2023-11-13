      ***************************************************************** NA300M2
      * SDF: NA300M2 NA300M2                                            NA300M2
      ***************************************************************** NA300M2
       01   NA300M2I.                                                   NA300M2
           02 FILLER    PIC X(12).                                      NA300M2
      * UR00051                                                         NA300M2
           02 MAP-SYSTEM-CD2L     COMP PIC S9(4).                       NA300M2
           02 MAP-SYSTEM-CD2F     PIC X.                                NA300M2
           02 MAP-SYSTEM-CD2I     PIC X(2).                             NA300M2
      * UR00052                                                         NA300M2
           02 MAP-ACTION-CD2L     COMP PIC S9(4).                       NA300M2
           02 MAP-ACTION-CD2F     PIC X.                                NA300M2
           02 MAP-ACTION-CD2I     PIC X(5).                             NA300M2
      * UR00053                                                         NA300M2
           02 MAP-CUST-INFO2L     COMP PIC S9(4).                       NA300M2
           02 MAP-CUST-INFO2F     PIC X.                                NA300M2
           02 MAP-CUST-INFO2I     PIC X(40).                            NA300M2
      * MU00011                                                         NA300M2
           02 MAP-CODED OCCURS   30 TIMES .                             NA300M2
             03 MAP-CODEL    COMP PIC S9(4).                            NA300M2
             03 MAP-CODEF    PIC X.                                     NA300M2
             03 MAP-CODEI    PIC X(5).                                  NA300M2
      * MU00012                                                         NA300M2
           02 MAP-DESCD OCCURS   30 TIMES .                             NA300M2
             03 MAP-DESCL    COMP PIC S9(4).                            NA300M2
             03 MAP-DESCF    PIC X.                                     NA300M2
             03 MAP-DESCI    PIC X(30).                                 NA300M2
      * MU00013                                                         NA300M2
           02 MAP-PF-BYTEL   COMP PIC S9(4).                            NA300M2
           02 MAP-PF-BYTEF   PIC X.                                     NA300M2
           02 MAP-PF-BYTEI   PIC X.                                     NA300M2
      * UR00001                                                         NA300M2
           02 MAP-ERROR-MSGD OCCURS   4 TIMES .                         NA300M2
             03 MAP-ERROR-MSGL    COMP PIC S9(4).                       NA300M2
             03 MAP-ERROR-MSGF    PIC X.                                NA300M2
             03 MAP-ERROR-MSGI    PIC X(37).                            NA300M2
      ***************************************************************** NA300M2
      * SDF: NA300M2 NA300M2                                            NA300M2
      ***************************************************************** NA300M2
       01   NA300M2O REDEFINES NA300M2I.                                NA300M2
           02 FILLER    PIC X(12).                                      NA300M2
      * UR00051                                                         NA300M2
           02 FILLER    PIC X(2).                                       NA300M2
           02 MAP-SYSTEM-CD2A     PIC X.                                NA300M2
           02 MAP-SYSTEM-CD2O     PIC X(2).                             NA300M2
      * UR00052                                                         NA300M2
           02 FILLER    PIC X(2).                                       NA300M2
           02 MAP-ACTION-CD2A     PIC X.                                NA300M2
           02 MAP-ACTION-CD2O     PIC X(5).                             NA300M2
      * UR00053                                                         NA300M2
           02 FILLER    PIC X(2).                                       NA300M2
           02 MAP-CUST-INFO2A     PIC X.                                NA300M2
           02 MAP-CUST-INFO2O     PIC X(40).                            NA300M2
      * MU00011                                                         NA300M2
           02 DFHMS1 OCCURS   30 TIMES .                                NA300M2
             03 FILLER       PIC X(2).                                  NA300M2
             03 MAP-CODEA    PIC X.                                     NA300M2
             03 MAP-CODEO    PIC X(5).                                  NA300M2
      * MU00012                                                         NA300M2
           02 DFHMS2 OCCURS   30 TIMES .                                NA300M2
             03 FILLER       PIC X(2).                                  NA300M2
             03 MAP-DESCA    PIC X.                                     NA300M2
             03 MAP-DESCO    PIC X(30).                                 NA300M2
      * MU00013                                                         NA300M2
           02 FILLER    PIC X(2).                                       NA300M2
           02 MAP-PF-BYTEA   PIC X.                                     NA300M2
           02 MAP-PF-BYTEO   PIC X.                                     NA300M2
      * UR00001                                                         NA300M2
           02 DFHMS3 OCCURS   4 TIMES .                                 NA300M2
             03 FILLER       PIC X(2).                                  NA300M2
             03 MAP-ERROR-MSGA    PIC X.                                NA300M2
             03 MAP-ERROR-MSGO    PIC X(37).                            NA300M2
