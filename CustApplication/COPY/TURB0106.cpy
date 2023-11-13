       01  WT-CNTL0106.                                                 TURB0106
      *----------------------------------------------------------------*TURB0106
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      TURB0106
           05  WT-C0106-CONTROL         PIC X(8) VALUE SPACE.           TURB0106
           05  WT-C0106-NAME            PIC X(8) VALUE 'TBV10106'.      TURB0106
           05  WT-C0106-REQUEST         PIC X    VALUE 'S'.             TURB0106
           05  FILLER                   PIC X    VALUE SPACE.           TURB0106
           05  WT-C0106-RETURN          PIC S9(4) COMP VALUE +0.        TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR* IMR CHANGE BEGIN                                                TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR*    05  WT-C0106-KEY-LENGTH      PIC S9(4) COMP VALUE +11.       TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR     05  WT-C0106-KEY-LENGTH      PIC S9(4) COMP VALUE +13.       TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR* IMR CHANGE END                                                  TURB0106
Y2KIMR*                                                                 TURB0106
           05  WT-C0106-ENTRY-LENGTH    PIC S9(4) COMP VALUE +33.       TURB0106
           05  WT-C0106-PARM-LENGTH     PIC S9(4) COMP VALUE +80.       TURB0106
           05  WT-C0106-KEY.                                            TURB0106
               10 WT-C0106-GENERIC-KEY.                                 TURB0106
                   15 WT-C0106-CARRIER      PIC X(2).                   TURB0106
                   15 WT-C0106-STATE        PIC X(2).                   TURB0106
                   15 WT-C0106-PRODUCT-TYPE PIC X(1)  VALUE SPACES.     TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR* IMR CHANGE BEGIN                                                TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR*        10 WT-C0106-RATE-EFF-DATE.                               TURB0106
Y2KIMR*           15 WT-C0106-EFF-DATE-YY PIC 9(2) VALUE ZEROS.         TURB0106
Y2KIMR*           15 WT-C0106-EFF-DATE-MM PIC 9(2) VALUE ZEROS.         TURB0106
Y2KIMR*           15 WT-C0106-EFF-DATE-DD PIC 9(2) VALUE ZEROS.         TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR         10 WT-C0106-RATE-EFF-DATE.                               TURB0106
Y2KIMR            15 WT-C0106-EFF-DATE-CC PIC 9(2) VALUE ZEROS.         TURB0106
Y2KIMR            15 WT-C0106-EFF-DATE-YY PIC 9(2) VALUE ZEROS.         TURB0106
Y2KIMR            15 WT-C0106-EFF-DATE-MM PIC 9(2) VALUE ZEROS.         TURB0106
Y2KIMR            15 WT-C0106-EFF-DATE-DD PIC 9(2) VALUE ZEROS.         TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR* IMR CHANGE END                                                  TURB0106
Y2KIMR*                                                                 TURB0106
           05  WT-C0106-DATA.                                           TURB0106
               10  WT-C0106-ALT-STATE          PIC X(2).                TURB0106
               10  WT-C0106-STATE-FACTOR       PIC 9V99.                TURB0106
031002         10  WT-C0106-STATE-FACTOR-X REDEFINES                    TURB0106
031002             WT-C0106-STATE-FACTOR       PIC X(3).                TURB0106
               10  WT-C0106-DEFAULT-RATE-LEVEL PIC X(2).                TURB0106
               10  WT-C0106-REENTRY-IND        PIC X.                   TURB0106
               10  WT-C0106-ONE-LIFE-IND       PIC X.                   TURB0106
               10  WT-C0106-PAAC-CMM-FACTOR    PIC 9V99.                TURB0106
               10  WT-C0106-PAAC-WRAP-FACTOR   PIC 9V99.                TURB0106
               10  WT-C0106-3-MNTH-RENEW-IND   PIC X.                   TURB0106
               10  WT-C0106-UNISEX-IND         PIC X.                   TURB0106
               10  WT-C0106-NEW-BUS-IND        PIC X.                   TURB0106
               10  WT-C0106-AGT-RENEW-IND      PIC X.                   TURB0106
                   88 NOAGENT-LETTER           VALUE 'N'.               TURB0106
               10  WT-C0106-CSE-RENEW-IND      PIC X.                   TURB0106
                   88 NOCASE-LETTER            VALUE 'N'.               TURB0106
               10  WT-C0106-CASE-CONSERV-IND   PIC X.                   TURB0106
               10  WT-C0106-DISCLOSURE-IND     PIC X.                   TURB0106
               10  WT-C0106-ALLOW-EMP-CHANGE-IND PIC X.                 TURB0106
980806             88 ALLOW-EMP-LEVEL-BENEFITS-106 VALUE 'Y'.           TURB0106
               10  WT-C0106-ELITE-VERSION        PIC X.                 TURB0106
                   88 ORIGINAL-ELITE-VERSION        VALUE SPACE.        TURB0106
               10  WT-C0106-VERSION-EFF-DATE   PIC 9(6).                TURB0106
               10  WT-C0106-COUNTY-RATING-IND  PIC X(1).                TURB0106
980192         10  WT-C0106-STATE-LEVEL        PIC X(2).                TURB0106
           05  WT-C0106-PARM.                                           TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR* IMR CHANGE BEGIN                                                TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR*        10  FILLER                PIC X(51) VALUE                TURB0106
Y2KIMR*        ' CONTROL DD=IS600V02,M=0106,K=(5,11),D=(41,33),S=B,'.   TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR*        10  FILLER                PIC X(51) VALUE                TURB0106
Y2KIMR*        ' CONTROL DD=IS600V02,M=0106,K=(5,13),D=(41,33),S=B,'.   TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR         10  FILLER                PIC X(51) VALUE                TURB0106
Y2KIMR         ' CONTROL DD=IS600V02,M=0106,K=(5,13),D=(51,33),S=B,'.   TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR* IMR CHANGE END                                                  TURB0106
Y2KIMR*                                                                 TURB0106
               10  FILLER                PIC X(29) VALUE                TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR* IMR CHANGE BEGIN                                                TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR*        'E=(31,1, )'.                                            TURB0106
Y2KIMR         'E=(41,1, )'.                                            TURB0106
Y2KIMR*                                                                 TURB0106
Y2KIMR* IMR CHANGE END                                                  TURB0106
Y2KIMR*                                                                 TURB0106
           05  FILLER                  PIC X(8) VALUE 'GSFRB.10'.       TURB0106
