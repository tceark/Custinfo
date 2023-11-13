      ***************************************************************** NA300M1
      * SDF: NA300M1 NA300M1                                            NA300M1
      ***************************************************************** NA300M1
       01   NA300M1I.                                                   NA300M1
           02 FILLER    PIC X(12).                                      NA300M1
      * UR00051                                                         NA300M1
           02 MAP-SYSTEM-CDL      COMP PIC S9(4).                       NA300M1
           02 MAP-SYSTEM-CDF      PIC X.                                NA300M1
           02 MAP-SYSTEM-CDI      PIC X(2).                             NA300M1
      * UR00052                                                         NA300M1
           02 MAP-ACTION-CDL      COMP PIC S9(4).                       NA300M1
           02 MAP-ACTION-CDF      PIC X.                                NA300M1
           02 MAP-ACTION-CDI      PIC X(5).                             NA300M1
      * UR00053                                                         NA300M1
           02 MAP-CUST-INFOL      COMP PIC S9(4).                       NA300M1
           02 MAP-CUST-INFOF      PIC X.                                NA300M1
           02 MAP-CUST-INFOI      PIC X(40).                            NA300M1
      * MU00041                                                         NA300M1
           02 MAP-CODED OCCURS   45 TIMES .                             NA300M1
             03 MAP-CODEL    COMP PIC S9(4).                            NA300M1
             03 MAP-CODEF    PIC X.                                     NA300M1
             03 MAP-CODEI    PIC XX.                                    NA300M1
      * MU00042                                                         NA300M1
           02 MAP-DESCD OCCURS   45 TIMES .                             NA300M1
             03 MAP-DESCL    COMP PIC S9(4).                            NA300M1
             03 MAP-DESCF    PIC X.                                     NA300M1
             03 MAP-DESCI    PIC X(20).                                 NA300M1
      * MU00013                                                         NA300M1
           02 MAP-PF-BYTEL   COMP PIC S9(4).                            NA300M1
           02 MAP-PF-BYTEF   PIC X.                                     NA300M1
           02 MAP-PF-BYTEI   PIC X.                                     NA300M1
      * UR00001                                                         NA300M1
           02 MAP-ERROR-MSGD OCCURS   4 TIMES .                         NA300M1
             03 MAP-ERROR-MSGL    COMP PIC S9(4).                       NA300M1
             03 MAP-ERROR-MSGF    PIC X.                                NA300M1
             03 MAP-ERROR-MSGI    PIC X(37).                            NA300M1
      ***************************************************************** NA300M1
      * SDF: NA300M1 NA300M1                                            NA300M1
      ***************************************************************** NA300M1
       01   NA300M1O REDEFINES NA300M1I.                                NA300M1
           02 FILLER    PIC X(12).                                      NA300M1
      * UR00051                                                         NA300M1
           02 FILLER    PIC X(2).                                       NA300M1
           02 MAP-SYSTEM-CDA      PIC X.                                NA300M1
           02 MAP-SYSTEM-CDO      PIC X(2).                             NA300M1
      * UR00052                                                         NA300M1
           02 FILLER    PIC X(2).                                       NA300M1
           02 MAP-ACTION-CDA      PIC X.                                NA300M1
           02 MAP-ACTION-CDO      PIC X(5).                             NA300M1
      * UR00053                                                         NA300M1
           02 FILLER    PIC X(2).                                       NA300M1
           02 MAP-CUST-INFOA      PIC X.                                NA300M1
           02 MAP-CUST-INFOO      PIC X(40).                            NA300M1
      * MU00041                                                         NA300M1
           02 DFHMS1 OCCURS   45 TIMES .                                NA300M1
             03 FILLER       PIC X(2).                                  NA300M1
             03 MAP-CODEA    PIC X.                                     NA300M1
             03 MAP-CODEO    PIC XX.                                    NA300M1
      * MU00042                                                         NA300M1
           02 DFHMS2 OCCURS   45 TIMES .                                NA300M1
             03 FILLER       PIC X(2).                                  NA300M1
             03 MAP-DESCA    PIC X.                                     NA300M1
             03 MAP-DESCO    PIC X(20).                                 NA300M1
      * MU00013                                                         NA300M1
           02 FILLER    PIC X(2).                                       NA300M1
           02 MAP-PF-BYTEA   PIC X.                                     NA300M1
           02 MAP-PF-BYTEO   PIC X.                                     NA300M1
      * UR00001                                                         NA300M1
           02 DFHMS3 OCCURS   4 TIMES .                                 NA300M1
             03 FILLER       PIC X(2).                                  NA300M1
             03 MAP-ERROR-MSGA    PIC X.                                NA300M1
             03 MAP-ERROR-MSGO    PIC X(37).                            NA300M1
