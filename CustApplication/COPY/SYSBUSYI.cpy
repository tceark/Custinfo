      ***************************************************************** SYSBUSY
      * SDF: SYSBUSY SYSBUSY                                            SYSBUSY
      ***************************************************************** SYSBUSY
       01   SYSBUSYI.                                                   SYSBUSY
           02 FILLER    PIC X(12).                                      SYSBUSY
      * UR00051                                                         SYSBUSY
           02 MAP-SYSTEM-CDL      COMP PIC S9(4).                       SYSBUSY
           02 MAP-SYSTEM-CDF      PIC X.                                SYSBUSY
           02 MAP-SYSTEM-CDI      PIC X(2).                             SYSBUSY
      * UR00052                                                         SYSBUSY
           02 MAP-ACTION-CDL      COMP PIC S9(4).                       SYSBUSY
           02 MAP-ACTION-CDF      PIC X.                                SYSBUSY
           02 MAP-ACTION-CDI      PIC X(5).                             SYSBUSY
      * UR00053                                                         SYSBUSY
           02 MAP-CUST-INFOL      COMP PIC S9(4).                       SYSBUSY
           02 MAP-CUST-INFOF      PIC X.                                SYSBUSY
           02 MAP-CUST-INFOI      PIC X(40).                            SYSBUSY
      * MU00041                                                         SYSBUSY
           02 MAP-CODED OCCURS   45 TIMES .                             SYSBUSY
             03 MAP-CODEL    COMP PIC S9(4).                            SYSBUSY
             03 MAP-CODEF    PIC X.                                     SYSBUSY
             03 MAP-CODEI    PIC XX.                                    SYSBUSY
      * MU00042                                                         SYSBUSY
           02 MAP-DESCD OCCURS   45 TIMES .                             SYSBUSY
             03 MAP-DESCL    COMP PIC S9(4).                            SYSBUSY
             03 MAP-DESCF    PIC X.                                     SYSBUSY
             03 MAP-DESCI    PIC X(20).                                 SYSBUSY
      * MU00013                                                         SYSBUSY
           02 MAP-PF-BYTEL   COMP PIC S9(4).                            SYSBUSY
           02 MAP-PF-BYTEF   PIC X.                                     SYSBUSY
           02 MAP-PF-BYTEI   PIC X.                                     SYSBUSY
      * UR00001                                                         SYSBUSY
           02 MAP-ERROR-MSGD OCCURS   4 TIMES .                         SYSBUSY
             03 MAP-ERROR-MSGL    COMP PIC S9(4).                       SYSBUSY
             03 MAP-ERROR-MSGF    PIC X.                                SYSBUSY
             03 MAP-ERROR-MSGI    PIC X(37).                            SYSBUSY
      ***************************************************************** SYSBUSY
      * SDF: SYSBUSY SYSBUSY                                            SYSBUSY
      ***************************************************************** SYSBUSY
       01   SYSBUSYO REDEFINES SYSBUSYI.                                SYSBUSY
           02 FILLER    PIC X(12).                                      SYSBUSY
      * UR00051                                                         SYSBUSY
           02 FILLER    PIC X(2).                                       SYSBUSY
           02 MAP-SYSTEM-CDA      PIC X.                                SYSBUSY
           02 MAP-SYSTEM-CDO      PIC X(2).                             SYSBUSY
      * UR00052                                                         SYSBUSY
           02 FILLER    PIC X(2).                                       SYSBUSY
           02 MAP-ACTION-CDA      PIC X.                                SYSBUSY
           02 MAP-ACTION-CDO      PIC X(5).                             SYSBUSY
      * UR00053                                                         SYSBUSY
           02 FILLER    PIC X(2).                                       SYSBUSY
           02 MAP-CUST-INFOA      PIC X.                                SYSBUSY
           02 MAP-CUST-INFOO      PIC X(40).                            SYSBUSY
      * MU00041                                                         SYSBUSY
           02 DFHMS1 OCCURS   45 TIMES .                                SYSBUSY
             03 FILLER       PIC X(2).                                  SYSBUSY
             03 MAP-CODEA    PIC X.                                     SYSBUSY
             03 MAP-CODEO    PIC XX.                                    SYSBUSY
      * MU00042                                                         SYSBUSY
           02 DFHMS2 OCCURS   45 TIMES .                                SYSBUSY
             03 FILLER       PIC X(2).                                  SYSBUSY
             03 MAP-DESCA    PIC X.                                     SYSBUSY
             03 MAP-DESCO    PIC X(20).                                 SYSBUSY
      * MU00013                                                         SYSBUSY
           02 FILLER    PIC X(2).                                       SYSBUSY
           02 MAP-PF-BYTEA   PIC X.                                     SYSBUSY
           02 MAP-PF-BYTEO   PIC X.                                     SYSBUSY
      * UR00001                                                         SYSBUSY
           02 DFHMS3 OCCURS   4 TIMES .                                 SYSBUSY
             03 FILLER       PIC X(2).                                  SYSBUSY
             03 MAP-ERROR-MSGA    PIC X.                                SYSBUSY
             03 MAP-ERROR-MSGO    PIC X(37).                            SYSBUSY
