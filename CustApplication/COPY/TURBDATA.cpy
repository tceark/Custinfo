       01  CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 CARR-CONTROL-KEY.                                         TURBDATA
               10 CARR-KEY-GENERIC.                                     TURBDATA
                  15 CARR-RECORD-TYPE  PIC 9(4).                        TURBDATA
                  15 CARR-CARRIER-CODE PIC XX.                          TURBDATA
                  15 FILLER            PIC X(14).                       TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *        10 FILLER               PIC X(4).                        TURBDATA
      *        10 CARR-EFF-DATE        PIC 9(6).                        TURBDATA
               10 FILLER               PIC X(12).                       TURBDATA
               10 CARR-EFF-DATE        PIC 9(8).                        TURBDATA
      *                                                                 TURBDATA
      * IMR CHANGES END                                                 TURBDATA
           05 CARR-RECORD-DATA.                                         TURBDATA
               10 CARR-CURRENT-IND     PIC X.                           TURBDATA
               10 CARR-ALT-KEY.                                         TURBDATA
                   15 CARR-OFU-IND         PIC X.                       TURBDATA
                       88 RECORD-IS-OUT-FOR-UPDATE VALUE 'Y' 'A' 'C'.   TURBDATA
                   15 CARR-USER-LID        PIC X(8).                    TURBDATA
               10 CARR-RECORD-DATA-OLD.                                 TURBDATA
                  15 FILLER            PIC X(1700).                     TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * THE RATE KEY RECORD SHOWS HOW TO BUILD THE KEY TO THE RATE      TURBDATA
      * FILE FOR EACH COVERAGE BY CARRIER                               TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  CARRIER-CONTROL-RECORD-RATE REDEFINES                        TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 CARR-RATE-KEY.                                            TURBDATA
               10 CARR-RATE-RECORD-TYPE         PIC 9(4).               TURBDATA
               10 CARR-RATE-CARRIER-CODE        PIC XX.                 TURBDATA
               10 CARR-RATE-COVERAGE-TYPE       PIC XX.                 TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *                                                                 TURBDATA
      *        10 FILLER                        PIC X(16).              TURBDATA
      *        10 CARR-RATE-EFF-DATE            PIC 9(6).               TURBDATA
               10 FILLER                        PIC X(24).              TURBDATA
               10 CARR-RATE-EFF-DATE            PIC 9(8).               TURBDATA
      *                                                                 TURBDATA
      * IMR CHANGES END                                                 TURBDATA
           05 CARR-RATE-DATA.                                           TURBDATA
               10 CARR-RATE-RESERVED-CONTROL    PIC X(10).              TURBDATA
               10 CARR-RATE-AREA                    PIC 99.             TURBDATA
               10 CARR-RATE-POLICY                  PIC 99.             TURBDATA
               10 CARR-RATE-OPTION                  PIC 99.             TURBDATA
               10 CARR-RATE-SPECIAL                 PIC 99.             TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * THE MAGIC PLAN DESCRIPTIONS                                     TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  MAGIC-PLAN-DESC-RECORD REDEFINES                             TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 MAGIC-PLAN-KEY.                                           TURBDATA
               10 MAGIC-PLAN-RECORD-TYPE        PIC 9(4).               TURBDATA
               10 MAGIC-PLAN-SYSTEM             PIC X(2).               TURBDATA
               10 MAGIC-PLAN-CARRIER            PIC X(2).               TURBDATA
               10 MAGIC-PLAN-TYPE               PIC X(2).               TURBDATA
               10 MAGIC-PLAN-PLAN-CODE          PIC X(2).               TURBDATA
               10 MAGIC-PLAN-LANGUAGE           PIC X.                  TURBDATA
               10 MAGIC-PLAN-LINE-NUMBER        PIC 9(5).               TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *                                                                 TURBDATA
      *        10 FILLER                        PIC X(06).              TURBDATA
      *        10 MAGIC-PLAN-EFF-DATE           PIC 9(06).              TURBDATA
               10 FILLER                        PIC X(14).              TURBDATA
               10 MAGIC-PLAN-EFF-DATE           PIC 9(08).              TURBDATA
      *                                                                 TURBDATA
      * IMR CHANGES END                                                 TURBDATA
           05 MAGIC-PLAN-DATA.                                          TURBDATA
              10 MAGIC-PLAN-RESERVED-CONTROL   PIC 9(10).               TURBDATA
              10 MAGIC-PLAN-TEXT                PIC X(80).              TURBDATA
000654        10 MAGIC-LINE-ADV                 PIC X(02).              TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * THE MAGIC PRINT POSITIONS.                                      TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  MAGIC-PRINT-POSITION-RECORD REDEFINES                        TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 MAGIC-PRINT-POSITION-KEY.                                 TURBDATA
              10 MAGIC-PP-RECORD-TYPE          PIC 9(4).                TURBDATA
              10 MAGIC-PP-CARRIER              PIC X(2).                TURBDATA
              10 MAGIC-PP-LANGUAGE             PIC X.                   TURBDATA
              10 MAGIC-PP-TYPE                 PIC X.                   TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *                                                                 TURBDATA
      *       10 MAGIC-PP-REST-OF-KEY          PIC X(16).               TURBDATA
      *       10 MAGIC-PP-EFF-DATE             PIC 9(6).                TURBDATA
              10 MAGIC-PP-REST-OF-KEY          PIC X(24).               TURBDATA
              10 MAGIC-PP-EFF-DATE             PIC 9(8).                TURBDATA
      *                                                                 TURBDATA
      * IMR CHANGES END                                                 TURBDATA
      *                                                                 TURBDATA
           05 MAGIC-PLAN-DATA.                                          TURBDATA
              10 MAGIC-PP-RESERVED-CONTROL     PIC X(10).               TURBDATA
              10 MAGIC-PP-START-DATE           PIC 9(6).                TURBDATA
              10 MAGIC-PP-END-DATE             PIC 9(6).                TURBDATA
              10 MAGIC-PP-PRNT-POS-1A          PIC X(40).               TURBDATA
              10 MAGIC-PP-PRNT-POS-1B          PIC X(40).               TURBDATA
              10 MAGIC-PP-PRNT-POS-2           PIC X(35).               TURBDATA
              10 MAGIC-PP-PRNT-POS-3           PIC X(35).               TURBDATA
              10 MAGIC-PP-PRNT-POS-4           PIC X(35).               TURBDATA
              10 MAGIC-PP-PRNT-POS-5           PIC X(35).               TURBDATA
              10 MAGIC-PP-PRNT-POS-6           PIC X(35).               TURBDATA
              10 MAGIC-PP-PRNT-POS-7           PIC X(35).               TURBDATA
              10 MAGIC-PP-PRNT-POS-8           PIC X(35).               TURBDATA
              10 MAGIC-PP-PRNT-POS-9           PIC X(35).               TURBDATA
              10 MAGIC-PP-PRNT-POS-10A         PIC X(40).               TURBDATA
              10 MAGIC-PP-PRNT-POS-10B         PIC X(21).               TURBDATA
              10 MAGIC-PP-PRNT-POS-11A         PIC X(40).               TURBDATA
              10 MAGIC-PP-PRNT-POS-11B         PIC X(26).               TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * THE TUTORIAL SCREEN/ FIELD CROSS REFERENCE                      TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  TUTOR-SCREEN-FIELD-RECORD REDEFINES                          TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 TUTOR-SF-KEY.                                             TURBDATA
               10 TUTOR-SF-RECORD-TYPE          PIC 9(4).               TURBDATA
               10 TUTOR-SF-SCREEN-NAME          PIC X(8).               TURBDATA
               10 TUTOR-SF-SCREEN-POSITION.                             TURBDATA
                   15 TUTOR-SF-SCREEN-ROW       PIC 9(2).               TURBDATA
                   15 TUTOR-SF-SCREEN-COLUMN    PIC 9(2).               TURBDATA
      * IMR Y2K CHANGES START                                           TURBDATA
      *                                                                 TURBDATA
      *        10 FILLER                        PIC X(08).              TURBDATA
      *        10 TUTOR-SF-EFF-DATE             PIC 9(6).               TURBDATA
               10 FILLER                        PIC X(16).              TURBDATA
               10 TUTOR-SF-EFF-DATE             PIC 9(8).               TURBDATA
      *                                                                 TURBDATA
      * IMR Y2K CHANGES END                                             TURBDATA
           05 TUTOR-SF-DATA.                                            TURBDATA
               10 TUTOR-SF-RESERVED-CONTROL     PIC 9(10).              TURBDATA
               10 TUTOR-SF-FIELD-FILE               PIC XX.             TURBDATA
               10 TUTOR-SF-FIELD-NUMBER             PIC 9(5).           TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * THE TUTORIAL FIELD DESCRIPTION                                  TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  TUTOR-FIELD-DESC-RECORD REDEFINES                            TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 TUTOR-FD-KEY.                                             TURBDATA
               10 TUTOR-KEY-GENERIC.                                    TURBDATA
                  15 TUTOR-FD-RECORD-TYPE       PIC 9(4).               TURBDATA
                  15 TUTOR-FD-FIELD-FILE        PIC X(2).               TURBDATA
                  15 TUTOR-FD-FIELD-NUMBER      PIC 9(5).               TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *                                                                 TURBDATA
      *           15 FILLER                     PIC X(13).              TURBDATA
      *        10 TUTOR-FD-EFF-DATE             PIC 9(06).              TURBDATA
                  15 FILLER                     PIC X(21).              TURBDATA
               10 TUTOR-FD-EFF-DATE             PIC 9(08).              TURBDATA
      *                                                                 TURBDATA
      * IMR CHANGES END                                                 TURBDATA
           05 TUTOR-FD-DATA.                                            TURBDATA
               10 TUTOR-FD-RESERVED-CONTROL     PIC X(10).              TURBDATA
               10 TUTOR-FD-FIELD-NAME               PIC X(30).          TURBDATA
               10 TUTOR-FD-FIELD-ALIAS              PIC X(07).          TURBDATA
               10 TUTOR-FD-COBOL-NAME               PIC X(30).          TURBDATA
               10 TUTOR-FD-POSITION-FROM            PIC 9(4).           TURBDATA
               10 TUTOR-FD-POSITION-TO              PIC 9(4).           TURBDATA
               10 TUTOR-FD-FIELD-TYPE               PIC X.              TURBDATA
               10 TUTOR-FD-FIELD-LENGTH             PIC 9(3).           TURBDATA
               10 TUTOR-FD-COBOL-PICTURE            PIC X(10).          TURBDATA
               10 TUTOR-FD-FIELD-AREA.                                  TURBDATA
                   15 TUTOR-FD-FIELD-DESC1              PIC X(80).      TURBDATA
                   15 TUTOR-FD-FIELD-DESC2              PIC X(80).      TURBDATA
                   15 TUTOR-FD-FIELD-DESC3              PIC X(80).      TURBDATA
                   15 TUTOR-FD-FIELD-DESC4              PIC X(80).      TURBDATA
                   15 TUTOR-FD-FIELD-DESC5              PIC X(80).      TURBDATA
                   15 TUTOR-FD-FIELD-DESC6              PIC X(80).      TURBDATA
                   15 TUTOR-FD-FIELD-DESC7              PIC X(80).      TURBDATA
                   15 TUTOR-FD-FIELD-DESC8              PIC X(80).      TURBDATA
                   15 TUTOR-FD-FIELD-DESC9              PIC X(80).      TURBDATA
                   15 TUTOR-FD-FIELD-DESC10             PIC X(80).      TURBDATA
               10 TUTOR-FD-FIELD-AREA-RDF REDEFINES TUTOR-FD-FIELD-AREA.TURBDATA
                   15 TUTOR-FD-FIELD-DESC PIC X(80) OCCURS 10    .      TURBDATA
               10 TUTOR-FD-TABLE                    PIC 9(4).           TURBDATA
               10 TUTOR-FD-TABLE-DISPLAY1           PIC 9(2).           TURBDATA
               10 TUTOR-FD-TABLE-DISPLAY2           PIC 9(2).           TURBDATA
               10 TUTOR-FD-TABLE-DISPLAY3           PIC 9(2).           TURBDATA
               10 TUTOR-FD-TABLE-DISPLAY4           PIC 9(2).           TURBDATA
               10 TUTOR-FD-TABLE-DISPLAY5           PIC 9(2).           TURBDATA
               10 TUTOR-FD-TABLE-STICKY             PIC 9(2).           TURBDATA
               10 TUTOR-FD-COL-HEAD1                PIC X(6).           TURBDATA
               10 TUTOR-FD-COL-HEAD2                PIC X(6).           TURBDATA
               10 TUTOR-FD-COL-HEAD3                PIC X(6).           TURBDATA
               10 TUTOR-FD-DISPLAY-TYPE             PIC X.              TURBDATA
               10 TUTOR-FD-RPT-USE-IND              PIC X.              TURBDATA
               10 TUTOR-FD-SCHEMA-NAME              PIC X(15).          TURBDATA
               10 TUTOR-FD-DISP-FMT                 PIC X(15).          TURBDATA
               10 TUTOR-FD-INT-FMT                  PIC X(05).          TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * THE TUTORIAL SCREEN DESCRIPTION                                 TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  TUTOR-SCREEN-DESC-RECORD REDEFINES                           TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 TUTOR-SD-KEY.                                             TURBDATA
               10 TUTOR-SD-RECORD-TYPE          PIC 9(4).               TURBDATA
               10 TUTOR-SD-SCREEN-NAME          PIC X(8).               TURBDATA
               10 TUTOR-SD-PAGE-NUMBER          PIC 9(2).               TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *                                                                 TURBDATA
      *        10 FILLER                        PIC X(10).              TURBDATA
      *        10 TUTOR-SD-EFF-DATE             PIC 9(6).               TURBDATA
               10 FILLER                        PIC X(18).              TURBDATA
               10 TUTOR-SD-EFF-DATE             PIC 9(8).               TURBDATA
      * IMR CHANGES END                                                 TURBDATA
      *                                                                 TURBDATA
           05 TUTOR-SD-DATA.                                            TURBDATA
               10 TUTOR-SD-RESERVED-CONTROL     PIC X(10).              TURBDATA
               10 TUTOR-SD-SCREEN-TITLE             PIC X(80).          TURBDATA
               10 TUTOR-SD-SCREEN-DESC-TABLE.                           TURBDATA
                   15 TUTOR-SD-SCREEN-DESC1         PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC2         PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC3         PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC4         PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC5         PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC6         PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC7         PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC8         PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC9         PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC10        PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC11        PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC12        PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC13        PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC14        PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC15        PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC16        PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC17        PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC18        PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC19        PIC X(80).          TURBDATA
                   15 TUTOR-SD-SCREEN-DESC20        PIC X(80).          TURBDATA
               10 TUTOR-SD-SCREEN-DESC-RDF REDEFINES                    TURBDATA
                  TUTOR-SD-SCREEN-DESC-TABLE.                           TURBDATA
                   15 TUTOR-SD-SCREEN-DESC PIC X(80) OCCURS 20.         TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * THE ERROR MESSAGE TABLE CONTAINS ERROR MESSAGES                 TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  ERROR-MESSAGE-RECORD REDEFINES                               TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 ERROR-KEY.                                                TURBDATA
               10 ERROR-RECORD-TYPE             PIC 9(4).               TURBDATA
               10 ERROR-SYSTEM-CODE             PIC XX.                 TURBDATA
               10 ERROR-MESSAGE-NUMBER          PIC 9(3).               TURBDATA
               10 FILLER                        PIC X(23).              TURBDATA
               10 ERROR-EFF-DATE                PIC 9(08).              TURBDATA
           05 ERROR-DATA.                                               TURBDATA
               10 ERROR-RESERVED-CONTROL        PIC X(10).              TURBDATA
               10 ERROR-SEVERITY-CODE               PIC X.              TURBDATA
               10 ERROR-MESSAGE-TEXT                PIC X(35).          TURBDATA
               10 ERROR-MESSAGE-AREA.                                   TURBDATA
                   15 ERROR-MESSAGE-DESC1               PIC X(79).      TURBDATA
                   15 ERROR-MESSAGE-DESC2               PIC X(79).      TURBDATA
                   15 ERROR-MESSAGE-DESC3               PIC X(79).      TURBDATA
                   15 ERROR-MESSAGE-DESC4               PIC X(79).      TURBDATA
                   15 ERROR-MESSAGE-DESC5               PIC X(79).      TURBDATA
                   15 ERROR-MESSAGE-DESC6               PIC X(79).      TURBDATA
                   15 ERROR-MESSAGE-DESC7               PIC X(79).      TURBDATA
                   15 ERROR-MESSAGE-DESC8               PIC X(79).      TURBDATA
                   15 ERROR-MESSAGE-DESC9               PIC X(79).      TURBDATA
                   15 ERROR-MESSAGE-DESC10              PIC X(79).      TURBDATA
               10 ERROR-MESSAGE-AREA-RDF REDEFINES ERROR-MESSAGE-AREA.  TURBDATA
                   15 ERROR-MESSAGE-DESC PIC X(79) OCCURS 10     .      TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * THE BILLING MESSAGE RECORD CONTAINS BILLING MESSAGES TEXT       TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  BILLING-MESSAGE-RECORD REDEFINES                             TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05  BILLING-MESSAGE-KEY.                                     TURBDATA
               10 BILLING-MESSAGE-RECORD-TYPE   PIC 9(4).               TURBDATA
               10 BILLING-MESSAGE-SYSTEM-CODE   PIC X(2).               TURBDATA
               10 BILLING-MESSAGE-NUMBER        PIC X(2).               TURBDATA
               10 BILLING-MESSAGE-VERSION       PIC X.                  TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *                                                                 TURBDATA
      *        10 BILLING-MESSAGE-KEY-FILLER    PIC X(15).              TURBDATA
      *        10 BILLING-MESSAGE-EFF-DATE      PIC 9(06).              TURBDATA
               10 BILLING-MESSAGE-KEY-FILLER    PIC X(23).              TURBDATA
               10 BILLING-MESSAGE-EFF-DATE      PIC 9(08).              TURBDATA
      *  IMR CHANGES END                                                TURBDATA
      *                                                                 TURBDATA
           05  BILLING-MESSAGE.                                         TURBDATA
               10  BILLING-MESSAGE-RESERVED-CNTL PIC X(10).             TURBDATA
               10  FILLER                       PIC X.                  TURBDATA
               10  BILLING-MESSAGE-SIZE         PIC 9(2).               TURBDATA
               10  BILLING-MESSAGE-CALC-NUMBER  PIC 9(5).               TURBDATA
               10  FILLER                       PIC X(28).              TURBDATA
      *        10  BILLING-MESSAGE-LINES        OCCURS 10 TIMES         TURBDATA
               10  BILLING-MESSAGE-LINES        PIC X(79)               TURBDATA
                                                OCCURS 10 TIMES.        TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * THE USERVIEW TABLE                                              TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  USERVIEW-RECORD REDEFINES                                    TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 USERVIEW-KEY.                                             TURBDATA
               10 USERVIEW-RECORD-TYPE          PIC 9(4).               TURBDATA
               10 USERVIEW-NAME                 PIC X(8).               TURBDATA
               10 USERVIEW-FIELD-NUMBER         PIC X(7).               TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *                                                                 TURBDATA
      *        10 FILLER                        PIC X(5).               TURBDATA
      *        10 USERVIEW-EFF-DATE             PIC 9(6).               TURBDATA
               10 FILLER                        PIC X(13).              TURBDATA
               10 USERVIEW-EFF-DATE             PIC 9(8).               TURBDATA
      * IMR CHANGES END                                                 TURBDATA
           05 USERVIEW-DATA.                                            TURBDATA
               10 USERVIEW-RESERVED-CONTROL     PIC X(10).              TURBDATA
               10 USERVIEW-KEY                  PIC X.                  TURBDATA
                                                                        TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * TESTABLE TRANSID TABLE                                          TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  TRANID-RECORD REDEFINES                                      TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 TRANID-KEY.                                               TURBDATA
               10 TRANID-RECORD-TYPE            PIC 9(4).               TURBDATA
               10 TRANID-TRANID                 PIC X(4).               TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *                                                                 TURBDATA
      *        10 FILLER                        PIC X(16).              TURBDATA
      *        10 TRANID-EFF-DATE               PIC 9(6).               TURBDATA
               10 FILLER                        PIC X(24).              TURBDATA
               10 TRANID-EFF-DATE               PIC 9(8).               TURBDATA
      * IMR CHANGES END                                                 TURBDATA
      *                                                                 TURBDATA
           05 TRANID-DATA.                                              TURBDATA
               10 TRANID-RESERVED-CONTROL     PIC X(10).                TURBDATA
                                                                        TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * TEST TABLE TABLE                                                TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  TESTABLE-RECORD REDEFINES                                    TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 TESTABLE-KEY.                                             TURBDATA
               10 TESTABLE-RECORD-TYPE          PIC 9(4).               TURBDATA
               10 TESTABLE-LID                  PIC X(8).               TURBDATA
               10 TESTABLE-TABLE                PIC X(4).               TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *        10 FILLER                        PIC X(8).               TURBDATA
      *        10 TESTABLE-EFF-DATE             PIC 9(6).               TURBDATA
               10 FILLER                        PIC X(16).              TURBDATA
               10 TESTABLE-EFF-DATE             PIC 9(8).               TURBDATA
      *                                                                 TURBDATA
      * IMR CHANGES END                                                 TURBDATA
      *                                                                 TURBDATA
           05 TESTABLE-DATA.                                            TURBDATA
               10 TESTABLE-RESERVED-CONTROL     PIC X(10).              TURBDATA
                                                                        TURBDATA
      ******************************************************************TURBDATA
      *                                                                 TURBDATA
      * PRODUCTION REPORTS DYNAMIC SQL TEXT                             TURBDATA
      *                                                                 TURBDATA
      ******************************************************************TURBDATA
       01  DYNAMIC-RECORD REDEFINES                                     TURBDATA
           CARRIER-CONTROL-RECORD.                                      TURBDATA
           05 DYNAMIC-KEY.                                              TURBDATA
               10 DYNAMIC-RECORD-TYPE           PIC 9(4).               TURBDATA
               10 DYNAMIC-PRE-QUAL              PIC X(2).               TURBDATA
               10 DYNAMIC-PROGRAM-ID            PIC X(10).              TURBDATA
               10 DYNAMIC-POST-QUAL             PIC X(2).               TURBDATA
      * IMR CHANGES START                                               TURBDATA
      *                                                                 TURBDATA
      *        10 FILLER                        PIC X(6).               TURBDATA
      *        10 DYNAMIC-EFF-DATE              PIC 9(6).               TURBDATA
               10 FILLER                        PIC X(14).              TURBDATA
               10 DYNAMIC-EFF-DATE              PIC 9(8).               TURBDATA
      *                                                                 TURBDATA
      * IMR CHANGES END                                                 TURBDATA
      *                                                                 TURBDATA
           05 DYNAMIC-DATA.                                             TURBDATA
               10 DYNAMIC-RESERVED-CONTROL      PIC X(10).              TURBDATA
               10 DYNAMIC-TEXT-AREA             PIC X(320).             TURBDATA
               10 DYNAMIC-TEXT REDEFINES                                TURBDATA
                  DYNAMIC-TEXT-AREA.                                    TURBDATA
                   15 DYNAMIC-TEXT PIC X(40) OCCURS 8 TIMES.            TURBDATA
                                                                        TURBDATA
