   CBL DATA(24)                                                         00000010
       IDENTIFICATION DIVISION.                                         00000020
       PROGRAM-ID. NA330A.                                              00000030
       DATE-WRITTEN. 03/22/88.                                          00000360
      *REMARKS. NA330 IS THE CUSTOMER INFORMATION ADDITION PROGRAM. THIS00000370
      *         PROGRAM ADDS NAME AND ADDRESS RECORDS TO THE AGNTNAME   00000380
      *         TABLE.                                                  00000390
      *------------------------PROGRAM PURPOSE-------------------------*00000400
      *                                                                *00000410
      *  PROGRAM TITLE: NA330                                          *00000420
      *  PROGRAM TEXT:                                                 *00000430
      *--------------------COMPILATION OPTIONS-------------------------*00000450
      *  COBOL II DB2 CICS                                             *00000440
      *----------------------------------------------------------------*00000850
      * -------------------------------------------------------------- *00001160
                                                                        00001680
       ENVIRONMENT DIVISION.                                            00001690
       DATA DIVISION.                                                   00001700
       WORKING-STORAGE SECTION.                                         00001710
       77  WS-ISEIB-ROUTINE                 PIC X(8) VALUE 'ISEIB   '.  00001720
       77  WS-PSIGSFC                       PIC X(08) VALUE 'PSIGSFC '. 00001730
                                                                        00001740
       01  WS-ADDRESS1.                                                 00001750
           05  WS-ADDRESS1-22          PIC X(22).                       00001760
           05  WS-ADDRESS1-REMAINDER   PIC X(08).                       00001770
       01  WS-ADDRESS2.                                                 00001780
           05  WS-ADDRESS2-22          PIC X(22).                       00001790
           05  WS-ADDRESS2-REMAINDER   PIC X(08).                       00001800
                                                                        00001810
      *                                                                 00001820
      * IMR CHANGE BEGIN                                                00001830
      *                                                                 00001840
       01  Y2K-WK-CUTOFF-YR-9               PIC 9(2) VALUE 50.          00001850
      *                                                                 00001860
       01  Y2K-WK-DATE1-9.                                              00001870
           05  Y2K-WK-DATE1-9-CC            PIC 9(2).                   00001880
           05  Y2K-WK-DATE1-9-YYMMDD        PIC 9(6).                   00001890
           05  Y2K-WK-DATE1R-9-YYMMDD REDEFINES Y2K-WK-DATE1-9-YYMMDD.  00001900
               10  Y2K-WK-DATE1R-9-YY       PIC 9(2).                   00001910
               10  Y2K-WK-DATE1R-9-MM       PIC 9(2).                   00001920
               10  Y2K-WK-DATE1R-9-DD       PIC 9(2).                   00001930
      *                                                                 00001940
       01  Y2K-WK-DATE1RR-9 REDEFINES Y2K-WK-DATE1-9.                   00001950
           05  Y2K-WK-DATE1RR-9-CCYY        PIC 9(4).                   00001960
           05  Y2K-WK-DATE1RR-9-MMDD        PIC 9(4).                   00001970
      *                                                                 00001980
       01  Y2K-WK-DATE1RRR-9 REDEFINES Y2K-WK-DATE1-9 PIC 9(8).         00001990
      *                                                                 00002000
      *                                                                 00002010
       01  Y2K-WK-DATE2-9.                                              00002020
           05  Y2K-WK-DATE2-9-CC            PIC 9(2).                   00002030
           05  Y2K-WK-DATE2-9-YYMMDD        PIC 9(6).                   00002040
           05  Y2K-WK-DATE2R-9-YYMMDD REDEFINES Y2K-WK-DATE2-9-YYMMDD.  00002050
               10  Y2K-WK-DATE2R-9-YY       PIC 9(2).                   00002060
               10  Y2K-WK-DATE2R-9-MM       PIC 9(2).                   00002070
               10  Y2K-WK-DATE2R-9-DD       PIC 9(2).                   00002080
      *                                                                 00002090
       01  Y2K-WK-DATE2RR-9 REDEFINES Y2K-WK-DATE2-9.                   00002100
           05  Y2K-WK-DATE2RR-9-CCYY        PIC 9(4).                   00002110
           05  Y2K-WK-DATE2RR-9-MMDD        PIC 9(4).                   00002120
      *                                                                 00002130
       01  Y2K-WK-DATE2RRR-9 REDEFINES Y2K-WK-DATE2-9 PIC 9(8).         00002140
      *                                                                 00002150
       01  Y2K-WK-CUTOFF-YR-X               PIC X(2) VALUE '50'.        00002160
      *                                                                 00002170
       01  Y2K-WK-DATE1-X.                                              00002180
           05  Y2K-WK-DATE1-X-CC            PIC X(2).                   00002190
           05  Y2K-WK-DATE1-X-YYMMDD        PIC X(6).                   00002200
           05  Y2K-WK-DATE1R-X-YYMMDD REDEFINES Y2K-WK-DATE1-X-YYMMDD.  00002210
               10  Y2K-WK-DATE1R-X-YY       PIC X(2).                   00002220
               10  Y2K-WK-DATE1R-X-MM       PIC X(2).                   00002230
               10  Y2K-WK-DATE1R-X-DD       PIC X(2).                   00002240
      *                                                                 00002250
       01  Y2K-WK-DATE1RR-X REDEFINES Y2K-WK-DATE1-X.                   00002260
           05  Y2K-WK-DATE1RR-X-CCYY        PIC X(4).                   00002270
           05  Y2K-WK-DATE1RR-X-MMDD        PIC X(4).                   00002280
      *                                                                 00002290
       01  Y2K-WK-DATE1RRR-X REDEFINES Y2K-WK-DATE1-X PIC X(8).         00002300
      *                                                                 00002310
       01 Y2K-WK-VARIABLE1.                                             00002320
          05 FILLER                         PIC X(02).                  00002330
          05 Y2K-WK-VARIABLE2.                                          00002340
             10 Y2K-WK-VARIABLE2-MM         PIC 9(02).                  00002350
             10 Y2K-WK-VARIABLE2-DD         PIC 9(02).                  00002360
             10 Y2K-WK-VARIABLE2-CC         PIC 9(02).                  00002370
             10 Y2K-WK-VARIABLE2-YY         PIC 9(02).                  00002380
          05 FILLER REDEFINES Y2K-WK-VARIABLE2.                         00002390
             10 Y2K-WK-VARIABLE2-N          PIC 9(08).                  00002400
      *                                                                 00002410
       01 Y2K-WK-VARIABLE3.                                             00002420
          05 Y2K-WK-VARIABLE3-AA            PIC 9(02).                  00002430
          05 Y2K-WK-VARIABLE3-BB.                                       00002440
             10 Y2K-WK-VARIABLE3-BB-MM      PIC 9(02).                  00002450
             10 Y2K-WK-VARIABLE3-BB-DD      PIC 9(02).                  00002460
             10 Y2K-WK-VARIABLE3-BB-YY      PIC 9(02).                  00002470
      *                                                                 00002480
           05  Y2K-WK-DATE-TEN.                                         00002490
               10  Y2K-WK-MONTH                 PIC 99     VALUE ZERO.  00002500
               10  Y2K-WK-SLASH-1               PIC X.                  00002510
               10  Y2K-WK-DAY                   PIC 99     VALUE ZERO.  00002520
               10  Y2K-WK-SLASH-2               PIC X.                  00002530
               10  Y2K-WK-CENT                  PIC 99     VALUE ZERO.  00002540
               10  Y2K-WK-YEAR                  PIC 99     VALUE ZERO.  00002550
      *                                                                 00002560
      * IMR CHANGE END                                                  00002570
      *                                                                 00002580
       77  WS-LINK-LENGTH                   PIC S9(8) VALUE +0    COMP. 00002590
       77  WS-ZERO-LENGTH                   PIC S9(4) VALUE +0    COMP. 00002600
       77  WS-COMM-LENGTH                   PIC S9(4) VALUE +600  COMP. 00002610
       77  WS-COMM-DB2-LENGTH               PIC S9(4) VALUE +0    COMP. 00002620
       77  WS-AUD-LENGTH                    PIC S9(4) VALUE +3800 COMP. 00002630
       77  WS-FIN-LENGTH                    PIC S9(4) VALUE +1000 COMP. 00002640
       77  WS-TUTOR-COMM-LENGTH             PIC S9(4) VALUE +906  COMP. 00002650
       77  WS-BROKER-LENGTH                 PIC S9(4) VALUE +723  COMP. 00002660
       77  WS-AG-KEY-LENGTH                 PIC S9(4) VALUE +8    COMP. 00002670
                                                                        00002680
           COPY AUDCICS.                                                00002690
           COPY CAWSINC.                                                00002700
           COPY DEMOCOMM.                                               00002710
           COPY TURB9999.                                               00002720
           COPY TURB8991.                                               00002730
           COPY TURB8997.                                               00002740
           COPY TUB8997A.                                               00002750
           COPY TURB2006.                                               00002760
           COPY TURB0705.                                               00002770
           COPY TURB0153.                                               00002780
           COPY TURB0043.                                               00002790
           COPY TURB0161.                                               00002800
           COPY TURB0525.                                               00002810
           COPY TURB0091.                                               00002820
           COPY EDITCODE.                                               00002820
      *---------------------------------------------------------------* 00002830
      *    COPY TUTORCOM.                                             * 00002840
      *---------------------------------------------------------------* 00002850
           COPY TU003COM.                                               00002860
           COPY CICSWS.                                                 00002870
           COPY TURBINC.                                                00002880
           COPY TURBDATA.                                               00002890
                                                                        00002900
      *                                                                 00002910
      * IMR CHANGE DATE ROUTINE BEGIN                                   00002920
      *                                                                 00002930
      *01  WS-DATEPROG-PARAMETERS.                                      00002940
      *    COPY DATEPARM.                                               00002950
       01  WS-DATEPROG-PARAMETERS.                                      00002960
           COPY DTEPARM2.                                               00002970
      *                                                                 00002980
      * IMR CHANGE DATE ROUTINE END                                     00002990
      *                                                                 00003000
       01  AUDIT-COMM-AREA.                                             00003010
           COPY AUDCOMM.                                                00003020
           COPY STATECON.                                               00003020
                                                                        00003030
       01  WS-WORK-AREA.                                                00003040
      *                                                                 00003050
      * IMR CHANGE DATE ROUTINE BEGIN                                   00003060
      *                                                                 00003070
      *    05  COB2-DATEPRG2                PIC X(8)  VALUE 'DATEPRG2'. 00003080
      *                                                                 00003090
           05  WS-NA330B                    PIC X(6)  VALUE 'NA330B'.   00003100
           05  COB2-DATEPRG2                PIC X(8)  VALUE 'DTEPROG2'. 00003100
      *                                                                 00003110
      * IMR CHANGE DATE ROUTINE END                                     00003120
      *                                                                 00003130
           05  WS-DISPLAY-SQLCODE-N         PIC S9(9).                  00003140
           05  WS-DISPLAY-SQLCODE-A  REDEFINES WS-DISPLAY-SQLCODE-N     00003150
                                            PIC X(9).                   00003160
           05  WS-GASET                     PIC S9(9) COMP.             00003170
           05  WS-GALENGTH                  PIC S9(4) COMP.             00003180
           05  WS-POINTER                   PIC S9(5) VALUE +1 COMP-3.  00003190
           05  REAL-NAME                    PIC X(30) VALUE SPACES.     00003200
           05  REAL-NAME-REDEF REDEFINES REAL-NAME.                     00003210
               10  REAL-BYTE                PIC X  OCCURS 30 TIMES.     00003220
           05  WS-CUSTOMER-STATUS           PIC X     VALUE SPACES.     00003230
               88  INVALID-CUSTOMER-STATUS            VALUE 'D' 'C'.    00003240
           05  WS-CICS-RESP                 PIC S9(4) COMP.             00003250
           05  WS-CICS-RESP2                PIC S9(4) COMP.             00003250
           05  WS-TS-ITEM                   PIC S9(4) VALUE +1 COMP.    00003260
           05  WS-TS-QUEUE-NAME.                                        00003270
               10  WS-TS-QUEUE-TRANID       PIC X(4).                   00003280
               10  WS-TS-QUEUE-TERMID       PIC X(4).                   00003290
           05  WS-REQID-NAME.                                           00003300
               10  WS-R-TERMID                   PIC X(4).              00003310
               10  FILLER                        PIC X(4)  VALUE 'BRSE'.00003320
           05  WS-HEX80                     PIC S9(4) VALUE +128 COMP.  00003330
           05  WS-HEX80-REDEF REDEFINES WS-HEX80.                       00003340
               10  FILLER                   PIC X.                      00003350
               10  HEX80                    PIC X.                      00003360
           05  WS-NULL-FIELD                PIC X(72).                  00003370
           05  WS-ERROR-FIELDS.                                         00003380
               10  WS-C9999-ERROR-CODE      PIC X(5).                   00003390
               10  WS-C9999-SEVERITY-CODE   PIC X.                      00003400
      **       10  FILLER                   PIC X.                      00003410
               10  WS-C9999-FILLER          PIC X.                      00003420
               10  WS-C9999-ERROR-MESSAGE   PIC X(30).                  00003430
               10  WS-C9999-DB2-ERROR       PIC X(11).                  00003440
           05  WS-SUBCRIPT COMP.                                        00003450
               10  ACTION-SUB               PIC S99.                    00003460
           05  WS-HOLD-MESSAGE              PIC X(5)   VALUE SPACE.     00003470
MANJU5     05  WS-DB2I-MESSAGE              PIC ZZZZ9.
      *    05  WS-DB2I-MESSAGE-N            PIC S9(9).                  00003140
      *    05  WS-DB2I-MESSAGE-A  REDEFINES WS-DB2I-MESSAGE-N PIC X(9). 00003150
           05  LOWER-CASE                   PIC X(26)                   00003520
                                   VALUE 'abcdefghijklmnopqrstuvwxyz'.  00003530
           05  UPPER-CASE                   PIC X(26)                   00003540
                                   VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.  00003550
           05  WS-SYSTEM-CODE.                                          00003560
               10  WS-SYSTEM-CODE-2BYTES    PIC X(2).                   00003570
               10  FILLER                   PIC X(2).                   00003580
           05  WS-ACTION-CODE.                                          00003590
               10  WS-ACTION-CODE-5BYTES    PIC X(5).                   00003600
               10  FILLER                   PIC X.                      00003610
           05  WS-DATE-R                    PIC 9(6)   VALUE ZERO.      00003620
           05  WS-DATE.                                                 00003630
               10  WS-MM                    PIC 99     VALUE ZERO.      00003640
               10  WS-DD                    PIC 99     VALUE ZERO.      00003650
               10  WS-YY                    PIC 99     VALUE ZERO.      00003660
           05  WS-DATE-A REDEFINES WS-DATE.                             00003670
               10  WS-MM-A                  PIC XX.                     00003680
               10  WS-DD-A                  PIC XX.                     00003690
               10  WS-YY-A                  PIC XX.                     00003700
           05  WS-BIRTH-DATE.                                           00003710
               10  BR-CC                    PIC XX.                     00003720
               10  BR-YY                    PIC XX.                     00003730
               10  FILLER                   PIC X      VALUE '-'.       00003740
               10  BR-MM                    PIC XX.                     00003750
               10  FILLER                   PIC X      VALUE '-'.       00003760
               10  BR-DD                    PIC XX.                     00003770
           05  WS-EFFECTIVE-DATE.                                       00003780
               10  EF-CC                    PIC XX.                     00003790
               10  EF-YY                    PIC XX.                     00003800
               10  FILLER                   PIC X      VALUE '-'.       00003810
               10  EF-MM                    PIC XX.                     00003820
               10  FILLER                   PIC X      VALUE '-'.       00003830
               10  EF-DD                    PIC XX.                     00003840
           05  WS-DATE-EIGHT.                                           00003850
               10  WS-MONTH                 PIC 99     VALUE ZERO.      00003860
               10  SLASH-1                  PIC X.                      00003870
               10  WS-DAY                   PIC 99     VALUE ZERO.      00003880
               10  SLASH-2                  PIC X.                      00003890
               10  WS-YEAR                  PIC 99     VALUE ZERO.      00003900
           05  WS-COMPARE-DATE.                                         00003910
               10  WS-COMPARE-YY            PIC 99     VALUE ZERO.      00003920
               10  WS-COMPARE-MM            PIC 99     VALUE ZERO.      00003930
               10  WS-COMPARE-DD            PIC 99     VALUE ZERO.      00003940
           05  HOLD-NEXT-ID                 PIC 9(8) VALUE ZERO.        00003950
           05  HOLD-NEXT-ID-R  REDEFINES  HOLD-NEXT-ID                  00003960
                                            PIC X(8).                   00003970
           05  PHONE-CHANGED-SW             PIC X.                      00003980
           05  DISPLAY-CHANGED-SW           PIC X.                      00003990
           05  WS-PHONE.                                                00004000
               10  WS-PHONE3                PIC X(3).                   00004010
               10  WS-PHONE4                PIC X(4).                   00004020
           05  WS-MAP-DATE.                                             00004030
               10  FILLER                   PIC 99.                     00004040
               10  WS-MAP-BIF               PIC 9(6).                   00004050
           05  WS-ID-PHONE.                                             00004060
               10  WS-ID-PHONE-AREA        PIC X(3).                    00004070
               10  WS-DASH1                 PIC X.                      00004080
               10  WS-ID-PHONE-EXCH        PIC X(3).                    00004090
               10  WS-DASH2                 PIC X.                      00004100
               10  WS-ID-PHONE-NUMB        PIC X(4).                    00004110
           05  WS-TIME-FIELD                PIC S9(8) COMP.             00004120
           05  WC-TODAYS-DATE.                                          00004130
               10  WC-TODAYS-MM             PIC XX.                     00004140
               10  FILLER                   PIC X.                      00004150
               10  WC-TODAYS-DD             PIC XX.                     00004160
               10  FILLER                   PIC X.                      00004170
               10  WC-TODAYS-YY             PIC XX.                     00004180
           05  WS-FINALST-REAS-CODE.                                    00004190
               10  WS-FINALST-BYTE1         PIC X.                      00004200
               10  FILLER                   PIC XX.                     00004210
           05  WS-IDNTITY-NUM                PIC X(8).                  00004220
           05  WS-APPLID                    PIC X(8).                   00004230
           05  WS-DUPLICATE-SW              PIC X.                      00004250
           05  OPER-ID                      PIC X(3).                   00004260
           05  WS-NUMERIC-CHECK.                                        00004270
               10 WS-NUMERIC-CHECK-BYTE OCCURS 20 TIMES                 00004280
                                            PIC X.                      00004290
           05  WS-IDNTITY-CHECK.                                        00004300
               10 WS-IDNTITY-CHECK-BYTE OCCURS 8 TIMES                  00004310
                                            PIC X.                      00004320
           05  WS-OFF-PRODUCERID            PIC X(8).                   00004330
           05  POSTAL-CODE                  PIC X(9).                   00004340
           05  COUNTRY-CODE                 PIC X(3).                   00004350
           05  IND-POSTAL-CODE              PIC S9(4) COMP.             00004360
           05  IND-COUNTRY-CODE             PIC S9(4) COMP.             00004370
           05  WS-IDNTITY-SW                PIC X.                      00004380
               88  WS-IDNTITY-GOOD-SW             VALUE 'N'.            00004390
               88  WS-IDNTITY-BAD-SW              VALUE 'Y'.            00004400
           05  WS-NUMERIC-SW                PIC X.                      00004410
           05  WS-BROKER-COMMAREA.                                      00004420
               10  WS-AGENT-COMMAREA        PIC X(600).                 00004430
               10  WS-BROKER-FIELDS         PIC X(121).                 00004440
               10  WS-SYSTEM-ADD-TYPE       PIC XX.                     00004450
           05  WS-STATE-ZIP.                                            00004460
               10  WS-STATE                 PIC X(2).                   00004470
               10  FILLER                   PIC X VALUE SPACE.          00004480
               10  WS-ZIP                   PIC X(5).                   00004490
           05  WS-NUMERIC-TEST8  PIC X(8).                              00004500
           05  WS-CMDLINE.                                              00004510
               10  WS-CMDLINE-1             PIC X(08)    VALUE SPACES.  00004520
               10  FILLER                   PIC X(01)    VALUE '\'.     00004530
               10  WS-CMDLINE-2             PIC X(03)    VALUE SPACES.  00004540
               10  FILLER                   PIC X(28)    VALUE ALL '_'. 00004550
           05  WS-TABLE-RETRY-CNT           PIC 9(2) COMP-3 VALUE ZERO. 00004560
           05  WS-SPACES-1                  PIC X(1)   VALUE SPACES.    00003510
           05  WS-SPACES-2                  PIC X(2)   VALUE SPACES.    00003510
           05  WS-SPACES-4                  PIC X(4)   VALUE SPACES.    00003510
           05  WS-SPACES-5                  PIC X(5)   VALUE SPACES.    00003510

       01  TCTUAL                      PIC S9(4) COMP.                  00004580
           88  INVALID-TCTUAL                    VALUE 0 THRU 135.      00004590

       01  WS-SCZ-COUNTY-CODE               PIC X(5).
       01  WS-SCZ-COUNTY-CODE-N REDEFINES
           WS-SCZ-COUNTY-CODE               PIC 9(5).

       01  WS-SCZ-ZIP                       PIC 9(5).
       01  WS-SCZ-ZIP-C         REDEFINES
           WS-SCZ-ZIP                       PIC X(5).
                                                                        00004600
       01  WSQ-COMMAREA.                                                00004610
           02  WSQ-COMM-FIELDS.                                         00004620
               05  WSQ-CICS-COMMAREA-LENGTH PIC S9(4) COMP VALUE +600.  00004630
               05  WSQ-SYSTEM-CODE          PIC X(02).                  00004640
               05  WSQ-ACTION-CODE          PIC X(05).                  00004650
               05  WSQ-CUSTOMER-INFO        PIC X(40).                  00004660
               05  WSQ-CUST-TOKEN-COUNT     PIC S9.                     00004670
               05  WSQ-CUSTOMER-INFO1       PIC X(40).                  00004680
               05  WSQ-CUSTOMER-INFO2       PIC X(30).                  00004690
               05  WSQ-CUSTOMER-INFO3       PIC X(30).                  00004700
               05  WSQ-CUSTOMER-INFO4       PIC X(30).                  00004710
               05  WSQ-CUSTOMER-INFO5       PIC X(30).                  00004720
               05  WSQ-CUSTOMER-INFO6       PIC X(30).                  00004730
               05  FILLER                   PIC X(46).                  00004740
               05  WSQ-PREVIOUS-TRANID      PIC X(4).                   00004750
               05  WSQ-CMDLINE-CHANGED      PIC X(1).                   00004760
               05  WSQ-KEY-CHANGED          PIC X(1).                   00004770
               05  WSQ-IDNTITY-NUMBER       PIC X(8).                   00004780
               05  WSQ-MAIL-LINE-COUNT      PIC 9.                      00004790
               05  WSQ-MAIL-LINE            PIC X(30) OCCURS 5 TIMES.   00004800
               05  WSQ-LAST-ITEM-NUM-BROWSED PIC S9(3) COMP-3.          00004810
               05  WSQ-CUST-TYPE            PIC X(2).                   00004820
               05  WSQ-CUST-STATUS          PIC X.                      00004830
               05  WSQ-CUST-PHONE.                                      00004840
                   10  WSQ-CUST-PHONE-AREA  PIC X(3).                   00004850
                   10  WSQ-CUST-PHONE-EXCH  PIC X(3).                   00004860
                   10  WSQ-CUST-PHONE-NUMB  PIC X(4).                   00004870
               05  FILLER                   PIC X(25).                  00004880
               05  WSQ-MSG-COUNT            PIC 9.                      00004890
               05  WSQ-MSG-ID               PIC X(5) OCCURS 4 TIMES.    00004900
               05  WSQ-MSG-MAX-SEVERITY     PIC X(1).                   00004910
               05  WSQ-NEXT-FUNCTION        PIC X(2).                   00004920
               05  WSQ-CURSOR-POSN          PIC S9(4) COMP.             00004930
               05  FILLER                   PIC X(83).                  00004940
           02  WSQ-AGNTNAME.                                            00004950
               05 WSQ-IDNTITY               PIC X(8).                   00004960
               05 WSQ-LAST-NAME             PIC X(20).                  00004970
               05 WSQ-FIRST-NAME            PIC X(15).                  00004980
               05 WSQ-MIDDLE-NAME           PIC X(15).                  00004990
               05 WSQ-PREFIX                PIC X(4).                   00005000
               05 WSQ-SUFFIX1               PIC X(4).                   00005010
               05 WSQ-SUFFIX2               PIC X(4).                   00005020
               05 WSQ-COMPANY-IND           PIC X(1).                   00005030
               05 WSQ-COMPANY-IN-ADDRESS    PIC X(1).                   00005040
               05 WSQ-COMPANY-NAME          PIC X(30).                  00005050
               05 WSQ-DISPLAY-NAME          PIC X(30).                  00005060
               05 WSQ-NICKNAME              PIC X(10).                  00005070
               05 WSQ-ADDRESS1              PIC X(30).                  00005080
               05 WSQ-ADDRESS2              PIC X(30).                  00005090
               05 WSQ-CITY                  PIC X(30).                  00005100
               05 WSQ-STATE                 PIC X(2).                   00005110
               05 WSQ-ZIP                   PIC X(5).                   00005120
               05 WSQ-ZIP-PLUS4             PIC X(4).                   00005130
               05 WSQ-COUNTY-CODE           PIC X(5).                   00005140
               05 WSQ-AREA-CODE             PIC X(3).                   00005150
               05 WSQ-PHONE                 PIC X(7).                   00005160
               05 WSQ-PHONE-EXTENSION       PIC X(4).                   00005170
               05 WSQ-SSN                   PIC X(9).                   00005180
               05 WSQ-SEX                   PIC X(1).                   00005190
      *****    05 WSQ-BIRTH-DATE            PIC S9(6)    COMP-3.        00005200
               05 WSQ-BIRTH-DATE            PIC S9(8)    COMP-3.        00005210
               05 WSQ-FINALST-REAS-CODE     PIC X(3).                   00005220
               05 WSQ-FINALST-OVRD-IND      PIC X(1).                   00005230
               05 WSQ-DUP-ADDR-OVRD-IND     PIC X(1).                   00005240
               05 WSQ-EFFECTIVE-DATE        PIC S9(6)    COMP-3.        00005250
               05 WSQ-CHANGE-DATE           PIC S9(6)    COMP-3.        00005260
               05 WSQ-CHANGE-LOGON          PIC X(8).                   00005270
               05 WSQ-ENTITY-TYPE           PIC X(2).                   00005280
               05 WSQ-RECORD-STATUS         PIC X(1).                   00005290
               05 WSQ-ALT-ADDRESS-IND       PIC X(1).                   00005300
               05 WSQ-FUTURE-ADDRESS-IND    PIC X(1).                   00005310
               05 WSQ-RECORD-ORIGIN         PIC X(1).                   00005320
               05 WSQ-COMBINED-STATUS       PIC X(2).                   00005330
               05 WSQ-SITE-CODE             PIC X(2).                   00005340
               05 WSQ-NAME-KEY1             PIC X(8).                   00005350
               05 WSQ-NAME-KEY2             PIC X(8).                   00005360
               05 WSQ-NAME-KEY3             PIC X(2).                   00005370
               05 WSQ-ADDRESS-KEY1          PIC X(10).                  00005380
               05 WSQ-ASSOCIATION1          PIC X(5).                   00005390
               05 WSQ-ASSOCIATION2          PIC X(5).                   00005400
               05 WSQ-ASSOCIATION3          PIC X(5).                   00005410
               05 WSQ-ENTITY-LITERAL        PIC X(10).                  00005420
               05 WSQ-CASE-SW               PIC X.                      00005430
               05 WSQ-FINALIST-SW           PIC X.                      00005440
               05 WSQ-FAX-AREA-CODE         PIC X(3).                   00005450
               05 WSQ-FAX-PHONE             PIC X(7).                   00005460
               05 WSQ-EMAIL1                PIC X(50).                  00005470
                                                                        00005480
       01  NA-COMMAREA.                                                 NA200C01
               05  NA-CICS-COMMAREA-LENGTH COMP PIC S9(4) VALUE +600.       NA20
               05  COMM-SYSTEM-CODE        PIC X(02).                       NA20
               05  COMM-ACTION-CODE        PIC X(05).                       NA20
               05  COMM-CUSTOMER-INFO      PIC X(40).                       NA20
               05  COMM-CUST-TOKEN-COUNT   PIC S9.                          NA20
               05  COMM-CUSTOMER-INFO1     PIC X(40).                       NA20
               05  COMM-CUSTOMER-INFO2     PIC X(30).                       NA20
               05  COMM-CUSTOMER-INFO3     PIC X(30).                       NA20
               05  COMM-CUSTOMER-INFO4     PIC X(30).                       NA20
               05  COMM-CUSTOMER-INFO5     PIC X(30).                       NA20
               05  COMM-CUSTOMER-INFO6     PIC X(30).                       NA20
               05  FILLER                  PIC X(38).                       NA20
               05  COMM-PREV-IDNTITY       PIC X(8).                        NA20
               05  COMM-PREVIOUS-TRANID    PIC X(4).                        NA20
               05  COMM-CMDLINE-CHANGED    PIC X(1).                        NA20
               05  COMM-KEY-CHANGED        PIC X(1).                        NA20
               05  COMM-IDNTITY            PIC X(8).                        NA20
               05  COMM-MAIL-LINE-COUNT    PIC 9.                           NA20
               05  COMM-MAIL-LINE          PIC X(30) OCCURS 5 TIMES.        NA20
               05  COMM-LAST-ITEM-NUM-BROWSED PIC S9(3) COMP-3.             NA20
               05  COMM-CUST-TYPE          PIC X(02).                       NA20
               05  COMM-CUST-STATUS        PIC X(01).                       NA20
               05  COMM-CUST-PHONE.                                         NA20
                   10  COMM-CUST-PHONE-AREA  PIC X(03).                     NA20
                   10  COMM-CUST-PHONE-EXCH  PIC X(03).                     NA20
                   10  COMM-CUST-PHONE-NUMB  PIC X(04).                     NA20
               05  COMM-COMBINE-ID1        PIC X(8).                        NA20
               05  COMM-COMBINE-ID2        PIC X(8).                        NA20
               05  COMM-EXTERNAL-CUST      PIC X.                           NA20
                                                                            NA20
               05  COMM-WEB-LOGON-ID       PIC X(8).                        NA20
                                                                            NA20
               05  COMM-MSG-COUNT          PIC 9.                           NA20
               05  COMM-MSG-ID             PIC X(5) OCCURS 4 TIMES.         NA20
               05  COMM-MSG-MAX-SEVERITY   PIC X(1).                        NA20
               05  COMM-NEXT-FUNCTION      PIC X(2).                        NA20
               05  COMM-CURSOR-POSN        PIC S9(4) COMP.                  NA20
                                                                            NA20
               05  COMM-WEB-DEBUG-IND      PIC X(01).                       NA20
               05  COMM-CONVERSION-IND     PIC X(1).                        NA20
               05  NA200C01-COMM-UNIQUE-NUM  PIC X(06).                     NA20
               05  NA200C01-COMM-EFFECTIVE-DATE     PIC X(10).              NA20
               05  COMM-ON-EXCH-TYPE       PIC X(1).                        NA20
                   88  COMM-ON-EXCH-NEW             VALUE 'E'.              NA20
                   88  COMM-ON-EXCH-CHG             VALUE 'C'.              NA20
                   88  COMM-ON-EXCH-RENEW           VALUE 'R'.              NA20
               05  FILLER                  PIC X(64).                       NA20
                                                                        NA200C01
       01  WS-ADD-COMMAREA.
           COPY ADDCOMA.
                                                                        00006070
       01  LINK-COMMAREA-LENGTH PIC S9(4) COMP VALUE +666.
                                                                        00006070
      *    COPY AGNTNCW3.                                               00006080
           COPY AGNTNV05.                                               00006090
      *    COPX AGNTNAME is not the base copybook, AGNTNV05 is.         00006100
           COPY CASNVW3.                                                00006110
           COPY NEXTCIM.                                                00006120
                                                                        00006130
           EXEC SQL                                                     00006140
              INCLUDE SQLCA                                             00006150
           END-EXEC.                                                    00006160
           05  WS-SQL-ERROR-MESSAGE         PIC X(78).                  00006170
                                                                        00006180
       01  WS-NAME-ADDRESS-DETAIL.                                      00007210
           COPY NA200C02.                                               00007220
       01  SECURITY-AREA.                                               00007230
           COPY SECCOMMC.                                               00007240
           COPY SYSBUSY.                                                00007250
           COPY EI100C01.                                               00007260
           COPY NA330M2.                                                00007270
           COPY DFHAID.                                                 00007280
           COPY ATTRB.                                                  00007290
           COPY DFHBMSCA.                                               00007300
           02  UNPROT-NORM-FSET  PIC X(1)  VALUE 'E'.                   00007310
                                                                        00007320
                                                                        00007330
       LINKAGE SECTION.                                                 00007340

       01  DFHCOMMAREA              PIC X.                              00007350
                                                                        00007360
      ******************************************************************00007500
                                                                        00007510
       PROCEDURE DIVISION.                                              00007520
                                                                        00007530
       0010-BEGIN-PROGRAM.                                              00007540
           DISPLAY 'NA330A 0010-BEGIN-PROGRAM'.

           INITIALIZE  NA-COMM-DISPLAY-NAME                             00007550
                       NA-COMM-TITLE                                    00007560
                       NA-COMM-RECORD-NUMBER                            00007570
                       NA-COMM-SEARCH-FIELD                             00007580
                       NA-COMM-PERSONAL-NAME                            00007590
                       NA-COMM-COMPANY-NAME                             00007600
                       NA-COMM-COMPANY-MAIL                             00007610
                       NA-COMM-BIRTH-MONTH                              00007620
                       NA-COMM-SSN                                      00007630
                       NA-COMM-ADDRESS-1                                00007640
                       NA-COMM-ADDRESS-2                                00007650
                       NA-COMM-ADDRESS-3                                00007660
                       NA-COMM-CITY                                     00007670
                       NA-COMM-STATE                                    00007680
                       NA-COMM-ZIP                                      00007690
                       NA-COMM-ZIP-PLUS4                                00007700
                       NA-COMM-PHONE-NUMBER                             00007710
                       NA-COMM-POSTAL-CODE                              00007720
                       NA-COMM-COUNTY-CODE                              00007730
                       NA-COMM-ASSOC-CODE                               00007740
                       NA-COMM-SITE-CODE                                00007750
                       NA-COMM-SEX                                      00007760
                       NA-COMM-CREATION-DATE                            00007770
                       NA-COMM-EFFECTIVE-DATE                           00007780
                       NA-COMM-LAST-CHG-DATE                            00007790
                       NA-COMM-STATUS                                   00007800
                       NA-COMM-MAIL-LINE1                               00007810
                       NA-COMM-MAIL-LINE2                               00007820
                       NA-COMM-MAIL-LINE3                               00007830
                       NA-COMM-MAIL-LINE4                               00007840
                       NA-COMM-MAIL-LINE5                               00007850
                       NA-COMM-MAIL-LINE6                               00007860
                       NA-COMM-LINE(1)                                  00007870
                       NA-COMM-LINE(2)                                  00007880
                       NA-COMM-LINE(3)                                  00007890
                       NA-COMM-LINE(4)                                  00007900
                       NA-COMM-LINE(5)                                  00007910
                       NA-COMM-LINE(6)                                  00007920
                       WS-ERROR-FIELDS                                  00007930
                       COM-AGNTNAME.

           MOVE 'NA'      TO   COMM-SYSTEM-CODE.
           MOVE 'A'       TO   COMM-ACTION-CODE.
                                                                        00007940
           EXEC CICS HANDLE CONDITION ERROR(9999-CICS-ERROR)            00007950
                                      END-EXEC.                         00007960
                                                                        00007970
           MOVE LOW-VALUES TO NA330M2O SYSBUSYO.                        00007980
                                                                        00007990
       0060-PROCESS-MENU-SCREEN.                                        00009060
                                                                        00009070
           DISPLAY 'EIBCALEN:' EIBCALEN.

           IF EIBCALEN = ZERO                                           80
               DISPLAY 'INITIAL SCREEN:'
               GO TO 0180-SEND-NA330M2-FIRST-TIME                       90
           ELSE
            IF EIBAID = DFHENTER                                        80
               DISPLAY 'INSIDE ENTER'
               GO TO 0070-EDIT-COMMAND-LINE                             90
            ELSE
             IF EIBAID = DFHCLEAR                                       80
               DISPLAY 'INSIDE CLEAR'
               GO TO 0330-PROCESS-SCREEN-CLEAR                          90
             ELSE
              IF EIBAID = DFHPF3                                        80
                DISPLAY 'INSIDE PF3'
                GO TO 0100-RETURN-TO-BROWSE                             90
              ELSE
                  DISPLAY 'WRONG KET HIT:'
                  GO TO 0350-WRONG-KEY-HIT                              00009410
              END-IF
             END-IF
            END-IF
           END-IF.
                                                                        00009420
       0070-EDIT-COMMAND-LINE.                                          00009430
           DISPLAY '0070-EDIT-COMMAND-LINE'                             00009440
           MOVE LOW-VALUES TO NA330M2I.                                 00009450
                                                                        00009460
           EXEC CICS RECEIVE MAP    ('NA330M2')                         00009470
                             RESP   (WS-CICS-RESP)                      00009480
                             END-EXEC.                                  00009490
                                                                        00009500
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00009510
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00009520
                GO TO 9999-CICS-ERROR.                                  00009530
                                                                        00009540
           PERFORM 0120-UPDATE-ID-MAP-FIELDS                            00009760
           DISPLAY '0130 PARA'
           PERFORM 0130-EDIT-NA330M2-MAP                                00009770
           DISPLAY '0190 PARA'
           GO TO 0190-SEND-NA330M2-MAP.                                 00009780
                                                                        00009790
       0100-RETURN-TO-BROWSE.

           EXEC CICS START TRANSID('NAM1')
                           TERMID (EIBTRMID)
                           RESP   (WS-CICS-RESP)
                           END-EXEC
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           EXEC CICS RETURN
                     END-EXEC.
                                                                        00010340
       0120-UPDATE-ID-MAP-FIELDS.                                       00010350
           DISPLAY '0120-UPDATE-ID-MAP-FIELDS'                          00010360
           IF MAP-EMAIL1F = HEX80                                       00010370
               MOVE SPACES TO COM-EMAIL1                                00010380
           ELSE                                                         00010390
               IF MAP-EMAIL1L > ZERO                                    00010400
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00010410
                  MOVE MAP-EMAIL1I TO COM-EMAIL1.                       00010420
                                                                        00010430
           IF MAP-FIRST-NAMEF = HEX80                                   00010440
               DISPLAY 'FIRST-NAME'
               MOVE SPACES TO COM-FIRST-NAME                            00010450
           ELSE                                                         00010460
               DISPLAY 'FIRST-NAME1'
               IF MAP-FIRST-NAMEL > ZERO                                00010470
               DISPLAY 'FIRST-NAME2'
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00010480
                  MOVE MAP-FIRST-NAMEI TO COM-FIRST-NAME.               00010490
                                                                        00010500
           IF MAP-MIDDLE-NAMEF = HEX80                                  00010510
               MOVE SPACES TO COM-MIDDLE-NAME                           00010520
           ELSE                                                         00010530
               IF MAP-MIDDLE-NAMEL > ZERO                               00010540
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00010550
                  MOVE MAP-MIDDLE-NAMEI TO COM-MIDDLE-NAME.             00010560
                                                                        00010570
           IF MAP-LAST-NAMEF = HEX80                                    00010580
               MOVE SPACES TO COM-LAST-NAME                             00010590
           ELSE                                                         00010600
               IF MAP-LAST-NAMEL > ZERO                                 00010610
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00010620
                  MOVE MAP-LAST-NAMEI TO COM-LAST-NAME.                 00010630
                                                                        00010640
           IF (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'STA')    00010650
              OR (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'CD')  00010660
              MOVE SPACES TO REAL-NAME                                  00010670
              MOVE +1 TO WS-POINTER                                     00010680
              STRING COM-FIRST-NAME DELIMITED BY SPACES                 00010690
                 INTO REAL-NAME WITH POINTER WS-POINTER                 00010700
              MOVE SPACE TO REAL-BYTE(WS-POINTER)                       00010710
              ADD 1 TO WS-POINTER                                       00010720
              IF COM-MIDDLE-NAME GREATER THAN SPACES                    00010730
                 STRING COM-MIDDLE-NAME DELIMITED BY SPACES             00010740
                 INTO REAL-NAME WITH POINTER WS-POINTER                 00010750
                 MOVE SPACE TO REAL-BYTE(WS-POINTER)                    00010760
                 ADD 1 TO WS-POINTER                                    00010770
                 STRING COM-LAST-NAME DELIMITED BY SPACES               00010780
                 INTO REAL-NAME WITH POINTER WS-POINTER                 00010790
                 MOVE REAL-NAME TO COM-COMPANY-NAME                     00010800
              ELSE                                                      00010810
                 STRING COM-LAST-NAME DELIMITED BY SPACES               00010820
                 INTO REAL-NAME WITH POINTER WS-POINTER                 00010830
                 MOVE REAL-NAME TO COM-COMPANY-NAME.                    00010840
                                                                        00010850
                                                                        00010860
           IF MAP-SUFFIX1F = HEX80                                      00010870
               MOVE SPACES TO COM-SUFFIX1                               00010880
           ELSE                                                         00010890
               IF MAP-SUFFIX1L > ZERO                                   00010900
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00010910
                  MOVE MAP-SUFFIX1I TO COM-SUFFIX1.                     00010920
                                                                        00010930
           IF MAP-SUFFIX2F = HEX80                                      00010940
               MOVE SPACES TO COM-SUFFIX2                               00010950
           ELSE                                                         00010960
               IF MAP-SUFFIX2L > ZERO                                   00010970
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00010980
                  MOVE MAP-SUFFIX2I TO COM-SUFFIX2.                     00010990
                                                                        00011000
           IF MAP-NICKNAMEF = HEX80                                     00011010
               MOVE SPACES TO COM-NICKNAME                              00011020
           ELSE                                                         00011030
               IF MAP-NICKNAMEL > ZERO                                  00011040
                  MOVE MAP-NICKNAMEI TO COM-NICKNAME.                   00011050
                                                                        00011060
           IF MAP-COMP-NAMEF = HEX80                                    00011070
               MOVE SPACES TO COM-COMPANY-NAME                          00011080
           ELSE                                                         00011090
               IF MAP-COMP-NAMEL > ZERO                                 00011100
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00011110
                  MOVE MAP-COMP-NAMEI TO COM-COMPANY-NAME.              00011120
                                                                        00011130
      *---------------------------------------------------------------* 00011140
      *    CHANGED SEX CODE CAN ALTER DEFAULT PREFIX ON DISPLAY NAME  * 00011150
      *---------------------------------------------------------------* 00011160
           IF MAP-SEXF = HEX80                                          00011170
               MOVE SPACES TO COM-SEX                                   00011180
           ELSE                                                         00011190
               IF MAP-SEXL > ZERO                                       00011200
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00011210
                  MOVE MAP-SEXI TO COM-SEX.                             00011220
                                                                        00011230
           IF MAP-PREFIXF = HEX80                                       00011240
               MOVE SPACES TO COM-PREFIX                                00011250
           ELSE                                                         00011260
               IF MAP-PREFIXL > ZERO                                    00011270
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00011280
                  MOVE MAP-PREFIXI TO COM-PREFIX.                       00011290
                                                                        00011300
           IF DISPLAY-CHANGED-SW = 'Y'                                  00011310
               PERFORM 0290-FORMAT-DISPLAY-NAME.                        00011320
                                                                        00011330
           IF MAP-ADDRESS1F = HEX80                                     00011340
               MOVE SPACES TO COM-ADDRESS1                              00011350
           ELSE                                                         00011360
               IF MAP-ADDRESS1L > ZERO                                  00011370
                  MOVE MAP-ADDRESS1I TO COM-ADDRESS1.                   00011380
                                                                        00011390
           IF MAP-ADDRESS2F = HEX80                                     00011400
               MOVE SPACES TO COM-ADDRESS2                              00011410
           ELSE                                                         00011420
               IF MAP-ADDRESS2L > ZERO                                  00011430
                  MOVE MAP-ADDRESS2I TO COM-ADDRESS2.                   00011440
                                                                        00011450
           IF MAP-BIRTH-DATEF = HEX80                                   00011460
               MOVE ZEROS TO COM-BIRTH-DATE                             00011470
      *                                                                 00011480
      * IMR CHANGE IMR6 BEGIN                                           00011490
      *                                                                 00011500
               MOVE ZEROS TO Y2K-WK-VARIABLE1                           00011510
      *                                                                 00011520
      * IMR CHANGE IMR6 END                                             00011530
      *                                                                 00011540
           ELSE                                                         00011550
               IF MAP-BIRTH-DATEL > ZERO                                00011560
                  INSPECT MAP-BIRTH-DATEI REPLACING ALL '-' BY 'A'      00011570
      *                                                                 00011580
      * IMR DATE ROUTINE CHANGE BEGIN                                   00011590
      *                                                                 00011600
      *           EXEC CICS BIF DEEDIT FIELD(MAP-BIRTH-DATEI)           00011610
      *                                LENGTH(8)                        00011620
      *                                END-EXEC                         00011630
      *           MOVE MAP-BIRTH-DATEI TO WS-MAP-DATE                   00011640
      *           MOVE WS-MAP-BIF      TO COM-BIRTH-DATE.               00011650
      *                                                                 00011660
                  EXEC CICS BIF DEEDIT FIELD(MAP-BIRTH-DATEI)           00011670
                                       LENGTH(10)                       00011680
                                       END-EXEC                         00011690
                  MOVE MAP-BIRTH-DATEI TO Y2K-WK-VARIABLE1              00011700
      *           MOVE Y2K-WK-VARIABLE2-MM TO Y2K-WK-VARIABLE3-BB-MM    00011710
      *           MOVE Y2K-WK-VARIABLE2-DD TO Y2K-WK-VARIABLE3-BB-DD    00011720
      *           MOVE Y2K-WK-VARIABLE2-YY TO Y2K-WK-VARIABLE3-BB-YY    00011730
      *           MOVE Y2K-WK-VARIABLE3 TO WS-MAP-DATE                  00011740
      *           MOVE WS-MAP-BIF TO COM-BIRTH-DATE.                    00011750
                  MOVE Y2K-WK-VARIABLE2-N  TO COM-BIRTH-DATE.           00011760
      *                                                                 00011770
      * IMR DATE ROUTINE CHANGE END                                     00011780
      *                                                                 00011790
                                                                        00011800
           IF MAP-SSNF = HEX80                                          00011810
               MOVE SPACES TO COM-SSN                                   00011820
           ELSE                                                         00011830
               IF MAP-SSNL > ZERO                                       00011840
                  MOVE MAP-SSNI TO COM-SSN.                             00011850
                                                                        00011860
           IF MAP-CITYF = HEX80                                         00011870
               MOVE SPACES TO COM-CITY                                  00011880
           ELSE                                                         00011890
               IF MAP-CITYL > ZERO                                      00011900
                  MOVE MAP-CITYI TO COM-CITY.                           00011910
                                                                        00011920
           DISPLAY 'COM-COMPANY-IN-ADDRESS1:' COM-COMPANY-IN-ADDRESS
           IF MAP-COMP-IN-ADDF = HEX80                                  00011930
            IF MAP-COMP-IN-ADDI = SPACES                                00011940
               MOVE 'Y' TO COM-COMPANY-IN-ADDRESS                       00011950
            END-IF
           ELSE                                                         00011960
               IF MAP-COMP-IN-ADDL > ZERO                               00011970
                  MOVE MAP-COMP-IN-ADDI TO COM-COMPANY-IN-ADDRESS       00011980
               ELSE
                  MOVE 'Y'     TO  COM-COMPANY-IN-ADDRESS.
           DISPLAY 'COM-COMPANY-IN-ADDRESS2:' COM-COMPANY-IN-ADDRESS
                                                                        00011990
           IF MAP-STATEF = HEX80                                        00012000
               MOVE SPACES TO COM-STATE                                 00012010
           ELSE                                                         00012020
               IF MAP-STATEL > ZERO                                     00012030
                  MOVE MAP-STATEI TO COM-STATE.                         00012040
                                                                        00012050
           IF MAP-ZIPF = HEX80                                          00012060
               MOVE SPACES TO COM-ZIP                                   00012070
           ELSE                                                         00012080
               IF MAP-ZIPL > ZERO                                       00012090
                  MOVE MAP-ZIPI TO COM-ZIP.                             00012100
                                                                        00012110
           IF MAP-ZIP-PLUS4F = HEX80                                    00012120
               MOVE SPACES TO COM-ZIP-PLUS4                             00012130
           ELSE                                                         00012140
               IF MAP-ZIP-PLUS4L > ZERO                                 00012150
                  MOVE MAP-ZIP-PLUS4I TO COM-ZIP-PLUS4.                 00012160
                                                                        00012170
           IF MAP-ADDR-OVRIDEF = HEX80                                  00012180
               MOVE SPACES TO COM-FINALST-OVRD-IND                      00012190
           ELSE                                                         00012200
               IF MAP-ADDR-OVRIDEL > ZERO                               00012210
                  MOVE MAP-ADDR-OVRIDEI TO COM-FINALST-OVRD-IND         00012220
               ELSE
                  MOVE 'N'              TO COM-FINALST-OVRD-IND.

                                                                        00012230
           MOVE 'N'  TO COM-DUP-ADDR-OVRD-IND.                          00012240
                                                                        00012250
           IF MAP-AREA-CODEF = HEX80                                    00012260
               MOVE SPACES TO COM-AREA-CODE                             00012270
           ELSE                                                         00012280
               IF MAP-AREA-CODEL > ZERO                                 00012290
                  MOVE MAP-AREA-CODEI TO COM-AREA-CODE.                 00012300
                                                                        00012310
           MOVE 'N' TO PHONE-CHANGED-SW.                                00012320
           MOVE COM-PHONE TO WS-PHONE.                                  00012330
                                                                        00012340
           IF MAP-PHONE3F = HEX80                                       00012350
               MOVE 'Y' TO PHONE-CHANGED-SW                             00012360
               MOVE SPACES TO WS-PHONE3                                 00012370
           ELSE                                                         00012380
               IF MAP-PHONE3L > ZERO                                    00012390
                  MOVE 'Y' TO PHONE-CHANGED-SW                          00012400
                  MOVE MAP-PHONE3I TO WS-PHONE3.                        00012410
                                                                        00012420
           IF MAP-PHONE4F = HEX80                                       00012430
               MOVE 'Y' TO PHONE-CHANGED-SW                             00012440
               MOVE SPACES TO WS-PHONE4                                 00012450
           ELSE                                                         00012460
               IF MAP-PHONE4L > ZERO                                    00012470
                  MOVE 'Y' TO PHONE-CHANGED-SW                          00012480
                  MOVE MAP-PHONE4I TO WS-PHONE4.                        00012490
                                                                        00012500
           IF PHONE-CHANGED-SW = 'Y'                                    00012510
               MOVE WS-PHONE TO COM-PHONE.                              00012520
                                                                        00012530
           IF MAP-PHONE-EXTF = HEX80                                    00012540
               MOVE SPACES TO COM-PHONE-EXTENSION                       00012550
           ELSE                                                         00012560
               IF MAP-PHONE-EXTL > ZERO                                 00012570
                  MOVE MAP-PHONE-EXTI TO COM-PHONE-EXTENSION.           00012580
                                                                        00012590
           IF MAP-FAX-AREA-CDF = HEX80                                  00012600
               MOVE SPACES TO COM-FAX-AREA-CODE                         00012610
           ELSE                                                         00012620
               IF MAP-FAX-AREA-CDL > ZERO                               00012630
                  MOVE MAP-FAX-AREA-CDI TO COM-FAX-AREA-CODE.           00012640
                                                                        00012650
           MOVE 'N' TO PHONE-CHANGED-SW.                                00012660
           MOVE COM-FAX-PHONE TO WS-PHONE.                              00012670
                                                                        00012680
           IF MAP-FAX-PHONE3F = HEX80                                   00012690
               MOVE 'Y' TO PHONE-CHANGED-SW                             00012700
               MOVE SPACES TO WS-PHONE3                                 00012710
           ELSE                                                         00012720
               IF MAP-FAX-PHONE3L > ZERO                                00012730
                  MOVE 'Y' TO PHONE-CHANGED-SW                          00012740
                  MOVE MAP-FAX-PHONE3I TO WS-PHONE3.                    00012750
                                                                        00012760
           IF MAP-FAX-PHONE4F = HEX80                                   00012770
               MOVE 'Y' TO PHONE-CHANGED-SW                             00012780
               MOVE SPACES TO WS-PHONE4                                 00012790
           ELSE                                                         00012800
               IF MAP-FAX-PHONE4L > ZERO                                00012810
                  MOVE 'Y' TO PHONE-CHANGED-SW                          00012820
                  MOVE MAP-FAX-PHONE4I TO WS-PHONE4.                    00012830
                                                                        00012840
           IF PHONE-CHANGED-SW = 'Y'                                    00012850
               MOVE WS-PHONE TO COM-FAX-PHONE.                          00012860
                                                                        00012870
           IF MAP-ALT-ADDRF = HEX80                                     00012880
               MOVE SPACES TO COM-ALT-ADDRESS-IND                       00012890
           ELSE                                                         00012900
               IF MAP-ALT-ADDRL  > ZERO                                 00012910
                  MOVE MAP-ALT-ADDRI  TO COM-ALT-ADDRESS-IND.           00012920
                                                                        00012930
           IF MAP-ENTITY-TYPEF = HEX80                                  00012940
               MOVE SPACES TO COM-ENTITY-TYPE COM-ENTITY-LITERAL        00012950
           ELSE                                                         00012960
               IF MAP-ENTITY-TYPEL  > ZERO                              00012970
                  MOVE MAP-ENTITY-TYPEI  TO COM-ENTITY-TYPE             00012980
                                            COM-ENTITY-LITERAL.         00012990
                                                                        00013000
           IF MAP-FUTURE-ADDRF = HEX80                                  00013010
               MOVE SPACES TO COM-FUTURE-ADDRESS-IND                    00013020
           ELSE                                                         00013030
               IF MAP-FUTURE-ADDRL  > ZERO                              00013040
                  MOVE MAP-FUTURE-ADDRI  TO COM-FUTURE-ADDRESS-IND.     00013050
                                                                        00013060
           IF MAP-CUST-STATUSF = HEX80                                  00013070
               MOVE SPACES TO COM-RECORD-STATUS                         00013080
           ELSE                                                         00013090
               IF MAP-CUST-STATUSL  > ZERO                              00013100
                  MOVE MAP-CUST-STATUSI  TO COM-RECORD-STATUS.          00013110
                                                                        00013120
           DISPLAY 'MAP-EFF-DATEI:' MAP-EFF-DATEI
           DISPLAY 'WS-MAP-BIF:' WS-MAP-BIF
           IF MAP-EFF-DATEF = HEX80                                     00013130
               MOVE ZEROS TO COM-EFFECTIVE-DATE                         00013140
           ELSE                                                         00013150
               IF MAP-EFF-DATEL  > ZERO                                 00013160
                  INSPECT MAP-EFF-DATEI REPLACING ALL '-' BY 'A'        00013170
      *           EXEC CICS BIF DEEDIT FIELD(MAP-EFF-DATEI)             00013180
      *                                LENGTH(8)                        00013190
      *                                END-EXEC                         00013200
                  MOVE MAP-EFF-DATEI TO WS-MAP-DATE                     00013210
                  MOVE WS-MAP-BIF    TO COM-EFFECTIVE-DATE.             00013220
           DISPLAY 'EFF DATE:' COM-EFFECTIVE-DATE
                                                                        00013230
                                                                        00013240
           IF MAP-ASSOC1F = HEX80                                       00013250
               MOVE SPACES TO COM-ASSOCIATION1                          00013260
           ELSE                                                         00013270
               IF MAP-ASSOC1L  > ZERO                                   00013280
                  MOVE MAP-ASSOC1I  TO COM-ASSOCIATION1.                00013290
                                                                        00013300
           IF MAP-ASSOC2F = HEX80                                       00013310
               MOVE SPACES TO COM-ASSOCIATION2                          00013320
           ELSE                                                         00013330
               IF MAP-ASSOC2L  > ZERO                                   00013340
                  MOVE MAP-ASSOC2I  TO COM-ASSOCIATION2.                00013350
                                                                        00013360
           IF MAP-ASSOC3F = HEX80                                       00013370
               MOVE SPACES TO COM-ASSOCIATION3                          00013380
           ELSE                                                         00013390
               IF MAP-ASSOC3L  > ZERO                                   00013400
                  MOVE MAP-ASSOC3I  TO COM-ASSOCIATION3.                00013410
                                                                        00013420
           IF MAP-POSTAL-CDF = HEX80                                    00013430
               MOVE SPACES TO COM-POSTAL-CODE                           00013440
           ELSE                                                         00013450
               IF MAP-POSTAL-CDL > ZERO                                 00013460
                  MOVE MAP-POSTAL-CDI TO COM-POSTAL-CODE.               00013470
                                                                        00013480
           IF MAP-COUNTRY-CDF = HEX80                                   00013490
               MOVE SPACES TO COM-COUNTRY-CODE                          00013500
           ELSE                                                         00013510
               IF MAP-COUNTRY-CDL > ZERO                                00013520
                  MOVE MAP-COUNTRY-CDI TO COM-COUNTRY-CODE.             00013530
                                                                        00013540
      *    EXEC CICS ASSIGN TCTUALENG(TCTUAL) END-EXEC.                 00013550
      *    EXEC CICS ADDRESS TCTUA(ADDRESS OF TCTUAR) END-EXEC.         00013560
                                                                        00013580
MANJU1     MOVE 'WIPRO123' TO  WS-DEMO-LOGONID
MANJU1     MOVE '01'   TO  WS-DEMO-SITE-CODE
           MOVE WS-DEMO-LOGONID TO COM-CHANGE-LOGON.                    00013590
           MOVE WS-DEMO-SITE-CODE    TO COM-SITE-CODE.                  00013600
                                                                        00013610
           EXEC CICS ASKTIME ABSTIME (WS-TIME-FIELD)                    00013620
                             RESP    (WS-CICS-RESP)                     00013630
                             END-EXEC.                                  00013640
                                                                        00013650
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00013660
               GO TO 9999-CICS-ERROR.                                   00013670
                                                                        00013680
           EXEC CICS FORMATTIME ABSTIME (WS-TIME-FIELD)                 00013690
                                MMDDYY  (WS-MAP-BIF)                    00013700
                                RESP    (WS-CICS-RESP)                  00013710
                                END-EXEC.                               00013720
                                                                        00013730
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00013740
               GO TO 9999-CICS-ERROR.                                   00013750
                                                                        00013760
           MOVE WS-MAP-BIF       TO COM-CHANGE-DATE                     00013770
                                    COM-EFFECTIVE-DATE.
                                                                        00013780
                                                                        00013790
       0130-EDIT-NA330M2-MAP.                                           00013800
           DISPLAY '0130-EDIT-NA330M2-MAP'                              00013810
           MOVE 'N' TO WS-EDIT-SW                                       00013820
                       WS-SQL-ERROR.                                    00013820
                                                                        00013830
           EXEC CICS LINK PROGRAM('NA330B')
                          COMMAREA(WS-ADD-COMMAREA)
                          DATALENGTH(LINK-COMMAREA-LENGTH)
                          RESP(WS-CICS-RESP)
                          RESP2(WS-CICS-RESP2)
           END-EXEC

           IF WS-EDIT-SW = 'Y'                                          00014430
              EVALUATE TRUE
                WHEN WS-ERR-FIELD1 = 'MAP-ENTITY-TYPE'
                WHEN WS-ERR-FIELD2 = 'MAP-ENTITY-TYPE'
                WHEN WS-ERR-FIELD3 = 'MAP-ENTITY-TYPE'
                WHEN WS-ERR-FIELD4 = 'MAP-ENTITY-TYPE'
                   MOVE -1       TO MAP-ENTITY-TYPEL                    00014490
                   MOVE DFHBMBRY TO MAP-ENTITY-TYPEA                    00014500
                WHEN WS-ERR-FIELD1 = 'MAP-CUST-STATUS'
                WHEN WS-ERR-FIELD2 = 'MAP-CUST-STATUS'
                WHEN WS-ERR-FIELD3 = 'MAP-CUST-STATUS'
                WHEN WS-ERR-FIELD4 = 'MAP-CUST-STATUS'
                   MOVE -1       TO MAP-CUST-STATUSL
                   MOVE DFHBMBRY TO MAP-CUST-STATUSA
                WHEN WS-ERR-FIELD1 = 'MAP-SSN'
                WHEN WS-ERR-FIELD2 = 'MAP-SSN'
                WHEN WS-ERR-FIELD3 = 'MAP-SSN'
                WHEN WS-ERR-FIELD4 = 'MAP-SSN'
                   MOVE -1       TO MAP-SSNL
                   MOVE DFHBMBRY TO MAP-SSNA
                WHEN WS-ERR-FIELD1 = 'MAP-AREA-CODE'
                WHEN WS-ERR-FIELD2 = 'MAP-AREA-CODE'
                WHEN WS-ERR-FIELD3 = 'MAP-AREA-CODE'
                WHEN WS-ERR-FIELD4 = 'MAP-AREA-CODE'
                   MOVE -1       TO MAP-AREA-CODEL
                   MOVE DFHBMBRY TO MAP-AREA-CODEA
                WHEN WS-ERR-FIELD1 = 'MAP-PHONE3'
                WHEN WS-ERR-FIELD2 = 'MAP-PHONE3'
                WHEN WS-ERR-FIELD3 = 'MAP-PHONE3'
                WHEN WS-ERR-FIELD4 = 'MAP-PHONE3'
                   MOVE -1       TO MAP-PHONE3L
                   MOVE DFHBMBRY TO MAP-PHONE3A
                   MOVE DFHBMBRY TO MAP-PHONE4A
                WHEN WS-ERR-FIELD1 = 'MAP-PHONE-EXT'
                WHEN WS-ERR-FIELD2 = 'MAP-PHONE-EXT'
                WHEN WS-ERR-FIELD3 = 'MAP-PHONE-EXT'
                WHEN WS-ERR-FIELD4 = 'MAP-PHONE-EXT'
                   MOVE -1       TO MAP-PHONE-EXTL
                   MOVE DFHBMBRY TO MAP-PHONE-EXTA
                WHEN WS-ERR-FIELD1 = 'MAP-FAX-AREA-CD'
                WHEN WS-ERR-FIELD2 = 'MAP-FAX-AREA-CD'
                WHEN WS-ERR-FIELD3 = 'MAP-FAX-AREA-CD'
                WHEN WS-ERR-FIELD4 = 'MAP-FAX-AREA-CD'
                   MOVE -1       TO MAP-FAX-AREA-CDL
                   MOVE DFHBMBRY TO MAP-FAX-AREA-CDA
                WHEN WS-ERR-FIELD1 = 'MAP-COMP-IN-ADD'
                WHEN WS-ERR-FIELD2 = 'MAP-COMP-IN-ADD'
                WHEN WS-ERR-FIELD3 = 'MAP-COMP-IN-ADD'
                WHEN WS-ERR-FIELD4 = 'MAP-COMP-IN-ADD'
                   MOVE -1       TO MAP-COMP-IN-ADDL
                   MOVE DFHBMBRY TO MAP-COMP-IN-ADDA
                WHEN WS-ERR-FIELD1 = 'MAP-ADDR-OVRIDE'
                WHEN WS-ERR-FIELD2 = 'MAP-ADDR-OVRIDE'
                WHEN WS-ERR-FIELD3 = 'MAP-ADDR-OVRIDE'
                WHEN WS-ERR-FIELD4 = 'MAP-ADDR-OVRIDE'
                   MOVE -1       TO MAP-ADDR-OVRIDEL
                   MOVE DFHBMBRY TO MAP-ADDR-OVRIDEA
                WHEN WS-ERR-FIELD1 = 'MAP-ZIP'
                WHEN WS-ERR-FIELD2 = 'MAP-ZIP'
                WHEN WS-ERR-FIELD3 = 'MAP-ZIP'
                WHEN WS-ERR-FIELD4 = 'MAP-ZIP'
                   MOVE -1       TO MAP-ZIPL
                   MOVE DFHBMBRY TO MAP-ZIPA
                WHEN WS-ERR-FIELD1 = 'MAP-ADDRESS1'
                WHEN WS-ERR-FIELD2 = 'MAP-ADDRESS1'
                WHEN WS-ERR-FIELD3 = 'MAP-ADDRESS1'
                WHEN WS-ERR-FIELD4 = 'MAP-ADDRESS1'
                   MOVE -1       TO MAP-ADDRESS1L
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A
                WHEN WS-ERR-FIELD1 = 'MAP-CITY'
                WHEN WS-ERR-FIELD2 = 'MAP-CITY'
                WHEN WS-ERR-FIELD3 = 'MAP-CITY'
                WHEN WS-ERR-FIELD4 = 'MAP-CITY'
                   MOVE -1       TO MAP-CITYL
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-CITYA
                WHEN WS-ERR-FIELD1 = 'MAP-STATE'
                WHEN WS-ERR-FIELD2 = 'MAP-STATE'
                WHEN WS-ERR-FIELD3 = 'MAP-STATE'
                WHEN WS-ERR-FIELD4 = 'MAP-STATE'
                   MOVE -1       TO MAP-STATEL
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-STATEA
                WHEN WS-ERR-FIELD1 = 'MAP-FIRST-NAME'
                WHEN WS-ERR-FIELD2 = 'MAP-FIRST-NAME'
                WHEN WS-ERR-FIELD3 = 'MAP-FIRST-NAME'
                WHEN WS-ERR-FIELD4 = 'MAP-FIRST-NAME'
                   MOVE -1  TO MAP-FIRST-NAMEL
                   MOVE DFHBMBRY TO MAP-FIRST-NAMEA
                WHEN WS-ERR-FIELD1 = 'MAP-LAST-NAME'
                WHEN WS-ERR-FIELD2 = 'MAP-LAST-NAME'
                WHEN WS-ERR-FIELD3 = 'MAP-LAST-NAME'
                WHEN WS-ERR-FIELD4 = 'MAP-LAST-NAME'
                   MOVE -1       TO MAP-LAST-NAMEL
                   MOVE DFHBMBRY TO MAP-LAST-NAMEA
                WHEN WS-ERR-FIELD1 = 'MAP-SEX'
                WHEN WS-ERR-FIELD2 = 'MAP-SEX'
                WHEN WS-ERR-FIELD3 = 'MAP-SEX'
                WHEN WS-ERR-FIELD4 = 'MAP-SEX'
                   MOVE -1       TO MAP-SEXL
                   MOVE DFHBMBRY TO MAP-SEXA
                WHEN WS-ERR-FIELD1 = 'MAP-MIDDLE-NAME'
                WHEN WS-ERR-FIELD2 = 'MAP-MIDDLE-NAME'
                WHEN WS-ERR-FIELD3 = 'MAP-MIDDLE-NAME'
                WHEN WS-ERR-FIELD4 = 'MAP-MIDDLE-NAME'
                   MOVE -1       TO MAP-MIDDLE-NAMEL
                   MOVE DFHBMBRY TO MAP-MIDDLE-NAMEA
                WHEN WS-ERR-FIELD1 = 'MAP-COUNTRY-CD'
                WHEN WS-ERR-FIELD2 = 'MAP-COUNTRY-CD'
                WHEN WS-ERR-FIELD3 = 'MAP-COUNTRY-CD'
                WHEN WS-ERR-FIELD4 = 'MAP-COUNTRY-CD'
                   MOVE -1       TO MAP-COUNTRY-CDL
                   MOVE DFHBMBRY TO MAP-COUNTRY-CDA
                WHEN WS-ERR-FIELD1 = 'MAP-POSTAL-CD'
                WHEN WS-ERR-FIELD2 = 'MAP-POSTAL-CD'
                WHEN WS-ERR-FIELD3 = 'MAP-POSTAL-CD'
                WHEN WS-ERR-FIELD4 = 'MAP-POSTAL-CD'
                   MOVE -1       TO MAP-POSTAL-CDL
                   MOVE DFHBMBRY TO MAP-POSTAL-CDA
              END-EVALUATE                                              00014530
           ELSE
              IF WS-SQL-ERROR = 'Y'
                 EXEC CICS SYNCPOINT ROLLBACK
                                  END-EXEC

                 MOVE WS-SQLCODE  TO SQLCODE
                 PERFORM  0410-DB2-ERROR THRU 0410-EXIT
              ELSE
                 DISPLAY 'RECORD ADDED SUCCESSFULLY:' COM-IDNTITY
              END-IF
           END-IF.
                                                                        00022920
       0180-SEND-NA330M2-FIRST-TIME.                                    00023040
                                                                        00023050
           MOVE 'NA'    TO MAP-SYSTEM-CDO.                              00023240

           MOVE 'A'     TO MAP-ACTION-CDO.                              00023290

           MOVE ALL '_' TO MAP-CUST-INFOO                               00023340
                                                                        00023380
           PERFORM 0270-MOVE-SPACES-TO-MAP.                             00023390
                                                                        00023400
           EXEC CICS SEND MAP    ('NA330M2')                            00023460
                          CURSOR                                        00023470
                          ERASE                                         00023480
                          RESP (WS-CICS-RESP)                           00023490
                          END-EXEC.                                     00023500
                                                                        00023510
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00023520
                GO TO 9999-CICS-ERROR.                                  00023530
                                                                        00023720
           DISPLAY 'HERE'

           EXEC CICS RETURN TRANSID ('NAA1')                            00023730
                COMMAREA (NA330M2O)
                LENGTH    (32)
             END-EXEC.                                                  00023740
                                                                        00023750
       0190-SEND-NA330M2-MAP.                                           00023760
                                                                        00023770
           MOVE LOW-VALUES TO NA330M2O.                                 00023780
                                                                        00023790
           MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1).                   00023800
           MOVE WS-MESSAGE-NUMBER2 TO COMM-MSG-ID(2).                   00023810
           MOVE WS-MESSAGE-NUMBER3 TO COMM-MSG-ID(3).                   00023820
           MOVE WS-MESSAGE-NUMBER4 TO COMM-MSG-ID(4).                   00023830
                                                                        00023840
           PERFORM 0380-CHECK-ERROR-MESSAGES                            00023850
               VARYING ACTION-SUB FROM 1 BY 1                           00023860
                   UNTIL ACTION-SUB > 4.                                00023870
                                                                        00023880
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00023890
               MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                    00023900
                                                                        00023910
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00023920
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00023930
               MOVE SPACES  TO COMM-SYSTEM-CODE                         00023940
           ELSE                                                         00023950
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00023960
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00023970
               MOVE ALL '_' TO MAP-ACTION-CDO                           00023980
               MOVE SPACES  TO COMM-ACTION-CODE                         00023990
           ELSE                                                         00024000
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00024010
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00024020
               MOVE ALL '_' TO MAP-CUST-INFOO                           00024030
               MOVE SPACES  TO COMM-CUSTOMER-INFO                       00024040
           ELSE                                                         00024050
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00024060
                                                                        00024070
           PERFORM 0280-MOVE-OUT-MAP-FIELDS.                            00024080
                                                                        00024090
           IF WS-EDIT-SW = 'Y'                                          00024100
               EXEC CICS SEND CONTROL ALARM END-EXEC                    00024110
           ELSE                                                         00024120
               MOVE -1 TO MAP-SYSTEM-CDL.                               00024130
                                                                        00024140
           EXEC CICS SEND MAP    ('NA330M2')                            00024150
                          CURSOR                                        00024160
                          ERASE                                         00024170
                          RESP (WS-CICS-RESP)                           00024180
                          END-EXEC.                                     00024190
                                                                        00024200
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00024210
                GO TO 9999-CICS-ERROR.                                  00024220
                                                                        00024230
                                                                        00024240
      *    MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00024250
      *    MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00024260
                                                                        00024270
      *    MOVE '01'               TO COMM-NEXT-FUNCTION.               00024280
      *    MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                  00024290
      *    MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                     00024300
      *    MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.           00024310
      *    EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00024320
      *                        FROM   (WSQ-COMMAREA)                    00024330
      *                        LENGTH (WS-COMM-DB2-LENGTH)              00024340
      *                        ITEM   (WS-TS-ITEM)                      00024350
      *                        RESP   (WS-CICS-RESP)                    00024360
      *                        REWRITE                                  00024370
      *                        MAIN                                     00024380
      *                        END-EXEC.                                00024390
      *                                                                 00024400
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00024410
      *         GO TO 9999-CICS-ERROR.                                  00024420
                                                                        00024430
           EXEC CICS RETURN TRANSID ('NAA1')                            00024440
                          COMMAREA (NA330M2O)
                          LENGTH    (32)
                            END-EXEC.                                   00024450
                                                                        00033280
       0270-MOVE-SPACES-TO-MAP.                                         00033290
                                                                        00033300
           MOVE SPACES TO MAP-FIRST-NAMEO MAP-MIDDLE-NAMEO              00033310
               MAP-LAST-NAMEO MAP-SUFFIX1O MAP-SUFFIX2O                 00033320
               MAP-NICKNAMEO MAP-COMP-NAMEO MAP-DIS-NAMEO               00033330
               MAP-ADDRESS1O MAP-BIRTH-DATEO MAP-SEXO                   00033340
               MAP-ADDRESS2O MAP-SSNO                                   00033350
               MAP-PREFIXO                                              00033360
               MAP-CITYO MAP-COMP-IN-ADDO                               00033370
               MAP-STATEO MAP-ZIPO MAP-ZIP-PLUS4O MAP-ADDR-OVRIDEO      00033380
               MAP-AREA-CODEO MAP-PHONE3O MAP-PHONE4O                   00033390
               MAP-FAX-AREA-CDO MAP-FAX-PHONE3O MAP-FAX-PHONE4O         00033400
               MAP-PHONE-EXTO MAP-ALT-ADDRO MAP-ENTITY-TYPEO            00033410
               MAP-FUTURE-ADDRO MAP-CUST-STATUSO                        00033420
               MAP-SITE-CODEO MAP-SITE-DESCO MAP-CHANGE-DATEO           00033430
               MAP-LOGONO MAP-ASSOC1O MAP-ASSOC2O MAP-ASSOC3O           00033440
               MAP-FINALST-CDO MAP-CIMO                                 00033450
               MAP-EMAIL1O.                                             00033460
                                                                        00033470
           MOVE 'N'     TO  MAP-ADDR-OVRIDEO.                           00033610
           MOVE 'Y'     TO  MAP-COMP-IN-ADDO.                           00033620
MANJU1     MOVE 'WIPRO123' TO  MAP-LOGONO.
MANJU1     MOVE '01'       TO  MAP-SITE-CODEO.
           MOVE SPACES     TO MAP-SITE-DESCO                            00033740
                                                                        00033780
           EXEC CICS ASKTIME ABSTIME (WS-TIME-FIELD)                    00033790
                             RESP    (WS-CICS-RESP)                     00033800
                             END-EXEC.                                  00033810
                                                                        00033820
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00033830
               GO TO 9999-CICS-ERROR.                                   00033840
                                                                        00033850
           EXEC CICS FORMATTIME ABSTIME (WS-TIME-FIELD)                 00033860
                                MMDDYY  (WS-MAP-BIF)                    00033870
                                RESP    (WS-CICS-RESP)                  00033880
                                END-EXEC.                               00033890
                                                                        00033900
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00033910
               GO TO 9999-CICS-ERROR.                                   00033920
                                                                        00033930
           MOVE WS-MAP-BIF      TO WSQ-CHANGE-DATE WSQ-EFFECTIVE-DATE.  00033940
           MOVE WSQ-CHANGE-DATE TO WS-DATE-R                            00033950
           MOVE WS-DATE-R       TO WS-DATE.                             00033960
           MOVE WS-MM           TO WS-MONTH.                            00033970
           MOVE WS-DD           TO WS-DAY.                              00033980
           MOVE WS-YY           TO WS-YEAR.                             00033990
           MOVE '/'             TO SLASH-1 SLASH-2.                     00034000
           MOVE WS-DATE-EIGHT   TO MAP-CHANGE-DATEO MAP-EFF-DATEO.      00034010
                                                                        00034020
       0280-MOVE-OUT-MAP-FIELDS.                                        00035350
MANJU1     MOVE 'WIPRO123'             TO WS-DEMO-USER-NAME.            00035360
           MOVE COM-FIRST-NAME         TO MAP-FIRST-NAMEO.              00035370
           MOVE COM-MIDDLE-NAME        TO MAP-MIDDLE-NAMEO.             00035380
           MOVE COM-LAST-NAME          TO MAP-LAST-NAMEO.               00035390
           MOVE COM-SUFFIX1            TO MAP-SUFFIX1O.                 00035400
           MOVE COM-SUFFIX2            TO MAP-SUFFIX2O.                 00035410
           MOVE COM-NICKNAME           TO MAP-NICKNAMEO.                00035420
           MOVE COM-COMPANY-NAME       TO MAP-COMP-NAMEO.               00035430
           MOVE COM-DISPLAY-NAME       TO MAP-DIS-NAMEO.                00035440
           MOVE COM-ADDRESS1           TO MAP-ADDRESS1O.                00035450
           MOVE COM-SEX                TO MAP-SEXO.                     00035460
           MOVE COM-PREFIX             TO MAP-PREFIXO.                  00035470
           MOVE COM-ADDRESS2           TO MAP-ADDRESS2O.                00035480
           MOVE COM-CITY               TO MAP-CITYO.                    00035490
           MOVE COM-COMPANY-IN-ADDRESS TO MAP-COMP-IN-ADDO.             00035500
           MOVE COM-STATE              TO MAP-STATEO.                   00035510
           MOVE COM-ZIP                TO MAP-ZIPO.                     00035520
           MOVE COM-ZIP-PLUS4          TO MAP-ZIP-PLUS4O.               00035530
           MOVE COM-FINALST-OVRD-IND   TO MAP-ADDR-OVRIDEO.             00035540
           MOVE COM-AREA-CODE          TO MAP-AREA-CODEO.               00035550
           MOVE COM-PHONE              TO WS-PHONE.                     00035560
           MOVE WS-PHONE3              TO MAP-PHONE3O.                  00035570
           MOVE WS-PHONE4              TO MAP-PHONE4O.                  00035580
           MOVE COM-SSN                TO MAP-SSNO.                     00035590
           MOVE COM-PHONE-EXTENSION    TO MAP-PHONE-EXTO.               00035600
           MOVE COM-FAX-AREA-CODE      TO MAP-FAX-AREA-CDO.             00035610
           MOVE COM-FAX-PHONE          TO WS-PHONE.                     00035620
           MOVE WS-PHONE3              TO MAP-FAX-PHONE3O.              00035630
           MOVE WS-PHONE4              TO MAP-FAX-PHONE4O.              00035640
           MOVE COM-EMAIL1             TO MAP-EMAIL1O.                  00035650
           MOVE COM-ALT-ADDRESS-IND    TO MAP-ALT-ADDRO.                00035660
           MOVE COM-ENTITY-TYPE        TO MAP-ENTITY-TYPEO.             00035670
           MOVE COM-FUTURE-ADDRESS-IND TO MAP-FUTURE-ADDRO.             00035680
           MOVE COM-RECORD-STATUS      TO MAP-CUST-STATUSO.             00035690
           MOVE COM-SITE-CODE          TO MAP-SITE-CODEO.               00035700
           MOVE COM-ASSOCIATION1       TO MAP-ASSOC1O.                  00035710
           MOVE COM-ASSOCIATION2       TO MAP-ASSOC2O.                  00035720
           MOVE COM-ASSOCIATION3       TO MAP-ASSOC3O.                  00035730
           MOVE COM-IDNTITY            TO MAP-CIMO.                     00035740
           MOVE COM-FINALST-REAS-CODE  TO MAP-FINALST-CDO.              00035750
           MOVE WS-DEMO-USER-NAME      TO MAP-LOGONO.                   00035760
           MOVE COM-POSTAL-CODE        TO MAP-POSTAL-CDO.               00035770
           MOVE COM-COUNTRY-CODE       TO MAP-COUNTRY-CDO.              00035780
           MOVE COM-RECORD-STATUS      TO MAP-CUST-STATUSO.             00035910
           MOVE SPACES                 TO MAP-SITE-DESCO.               00036000
           MOVE COM-ENTITY-TYPE        TO MAP-ENTITY-TYPEO.             00036090
                                                                        00036100
           PERFORM 0290-FORMAT-DISPLAY-NAME.                            00036110
                                                                        00036120
           MOVE COM-BIRTH-DATE TO Y2K-WK-VARIABLE2-N.                   00036240
           MOVE Y2K-WK-VARIABLE2-MM TO Y2K-WK-MONTH.                    00036250
           MOVE Y2K-WK-VARIABLE2-DD TO Y2K-WK-DAY.                      00036260
           MOVE Y2K-WK-VARIABLE2-CC TO Y2K-WK-CENT.                     00036270
           MOVE Y2K-WK-VARIABLE2-YY TO Y2K-WK-YEAR.                     00036280
           MOVE '/'                 TO Y2K-WK-SLASH-1 Y2K-WK-SLASH-2.   00036290
           MOVE Y2K-WK-DATE-TEN TO MAP-BIRTH-DATEO.                     00036300
      *                                                                 00036310
      * IMR CHANGE IMR7 END                                             00036320
      *                                                                 00036330
                                                                        00036340
           MOVE COM-CHANGE-DATE TO WS-DATE-R                            00036350
           MOVE WS-DATE-R       TO WS-DATE.                             00036360
           MOVE WS-MM           TO WS-MONTH.                            00036370
           MOVE WS-DD           TO WS-DAY.                              00036380
           MOVE WS-YY           TO WS-YEAR.                             00036390
           MOVE '/'             TO SLASH-1 SLASH-2.                     00036400
           MOVE WS-DATE-EIGHT   TO MAP-CHANGE-DATEO.                    00036410
                                                                        00036420
           MOVE COM-EFFECTIVE-DATE TO WS-DATE-R.                        00036430
           MOVE WS-DATE-R          TO WS-DATE.                          00036440
           MOVE WS-MM              TO WS-MONTH.                         00036450
           MOVE WS-DD              TO WS-DAY.                           00036460
           MOVE WS-YY              TO WS-YEAR.                          00036470
           MOVE '/'                TO SLASH-1 SLASH-2.                  00036480
           MOVE WS-DATE-EIGHT      TO MAP-EFF-DATEO.                    00036490
                                                                        00036500
           INSPECT COMM-ACTION-CODE REPLACING ALL '_' BY SPACE.         00036510
                                                                        00036520
           MOVE ATTRB-PROT-ASKIP       TO MAP-SYSTEM-CDA.               00035370
           MOVE ATTRB-PROT-ASKIP       TO MAP-ACTION-CDA.               00035370
           MOVE ATTRB-PROT-ASKIP       TO MAP-CUST-INFOA.               00035370
           MOVE ATTRB-PROT-ASKIP       TO MAP-FIRST-NAMEA.              00035370
           MOVE ATTRB-PROT-ASKIP       TO MAP-MIDDLE-NAMEA.             00035380
           MOVE ATTRB-PROT-ASKIP       TO MAP-LAST-NAMEA.               00035390
           MOVE ATTRB-PROT-ASKIP       TO MAP-SUFFIX1A.                 00035400
           MOVE ATTRB-PROT-ASKIP       TO MAP-SUFFIX2A.                 00035410
           MOVE ATTRB-PROT-ASKIP       TO MAP-NICKNAMEA.                00035420
           MOVE ATTRB-PROT-ASKIP       TO MAP-COMP-NAMEA.               00035430
           MOVE ATTRB-PROT-ASKIP       TO MAP-DIS-NAMEA.                00035440
           MOVE ATTRB-PROT-ASKIP       TO MAP-ADDRESS1A.                00035450
           MOVE ATTRB-PROT-ASKIP       TO MAP-SEXA.                     00035460
           MOVE ATTRB-PROT-ASKIP       TO MAP-PREFIXA.                  00035470
           MOVE ATTRB-PROT-ASKIP       TO MAP-ADDRESS2A.                00035480
           MOVE ATTRB-PROT-ASKIP       TO MAP-CITYA.                    00035490
           MOVE ATTRB-PROT-ASKIP       TO MAP-COMP-IN-ADDA.             00035500
           MOVE ATTRB-PROT-ASKIP       TO MAP-STATEA.                   00035510
           MOVE ATTRB-PROT-ASKIP       TO MAP-ZIPA.                     00035520
           MOVE ATTRB-PROT-ASKIP       TO MAP-ZIP-PLUS4A.               00035530
           MOVE ATTRB-PROT-ASKIP       TO MAP-ADDR-OVRIDEA.             00035540
           MOVE ATTRB-PROT-ASKIP       TO MAP-AREA-CODEA.               00035550
           MOVE ATTRB-PROT-ASKIP       TO MAP-PHONE3A.                  00035570
           MOVE ATTRB-PROT-ASKIP       TO MAP-PHONE4A.                  00035580
           MOVE ATTRB-PROT-ASKIP       TO MAP-SSNA.                     00035590
           MOVE ATTRB-PROT-ASKIP       TO MAP-PHONE-EXTA.               00035600
           MOVE ATTRB-PROT-ASKIP       TO MAP-FAX-AREA-CDA.             00035610
           MOVE ATTRB-PROT-ASKIP       TO MAP-FAX-PHONE3A.              00035630
           MOVE ATTRB-PROT-ASKIP       TO MAP-FAX-PHONE4A.              00035640
           MOVE ATTRB-PROT-ASKIP       TO MAP-EMAIL1A.                  00035650
           MOVE ATTRB-PROT-ASKIP       TO MAP-EMAIL2A.                  00035650
           MOVE ATTRB-PROT-ASKIP       TO MAP-ALT-ADDRA.                00035660
           MOVE ATTRB-PROT-ASKIP       TO MAP-ENTITY-TYPEA.             00035670
           MOVE ATTRB-PROT-ASKIP       TO MAP-FUTURE-ADDRA.             00035680
           MOVE ATTRB-PROT-ASKIP       TO MAP-CUST-STATUSA.             00035690
           MOVE ATTRB-PROT-ASKIP       TO MAP-SITE-CODEA.               00035700
           MOVE ATTRB-PROT-ASKIP       TO MAP-ASSOC1A.                  00035710
           MOVE ATTRB-PROT-ASKIP       TO MAP-ASSOC2A.                  00035720
           MOVE ATTRB-PROT-ASKIP       TO MAP-ASSOC3A.                  00035730
           MOVE ATTRB-PROT-ASKIP       TO MAP-CIMA.                     00035740
           MOVE ATTRB-PROT-ASKIP       TO MAP-FINALST-CDA.              00035750
           MOVE ATTRB-PROT-ASKIP       TO MAP-LOGONA.                   00035760
           MOVE ATTRB-PROT-ASKIP       TO MAP-POSTAL-CDA.               00035770
           MOVE ATTRB-PROT-ASKIP       TO MAP-COUNTRY-CDA.              00035780
           MOVE ATTRB-PROT-ASKIP       TO MAP-SITE-DESCA.               00035780
           MOVE ATTRB-PROT-ASKIP       TO MAP-BIRTH-DATEA.              00035780
           MOVE ATTRB-PROT-ASKIP       TO MAP-CHANGE-DATEA.             00035780
           MOVE ATTRB-PROT-ASKIP       TO MAP-EFF-DATEA.                00035780
           MOVE ATTRB-PROT-ASKIP       TO MAP-PASSWORDA.                00035780
                                                                        00037510
       0290-FORMAT-DISPLAY-NAME.                                        00037520
                                                                        00037530
      *---------------------------------------------------------------* 00037540
      *    GET FORMATED VERSION OF NAME AND ADDRESS                   * 00037550
      *---------------------------------------------------------------* 00037560
           MOVE ZEROS                 TO NA-COMM-RECORD-NUMBER.         00037570
           MOVE COM-FIRST-NAME        TO NA-COMM-FIRST-NAME.            00037580
           MOVE COM-MIDDLE-NAME       TO NA-COMM-MIDDLE-NAME.           00037590
           MOVE COM-LAST-NAME         TO NA-COMM-LAST-NAME.             00037600
           MOVE COM-PREFIX            TO NA-COMM-TITLE.                 00037610
           MOVE COM-SUFFIX1           TO NA-COMM-SUFFIX1.               00037620
           MOVE COM-SUFFIX2           TO NA-COMM-SUFFIX2.               00037630
           MOVE COM-COMPANY-NAME      TO NA-COMM-COMPANY-NAME.          00037640
           MOVE COM-COMPANY-IND       TO NA-COMM-COMPANY-MAIL.          00037650
           MOVE COM-COMPANY-NAME      TO NA-COMM-COMPANY-NAME.          00037660
           MOVE COM-ADDRESS1          TO NA-COMM-ADDRESS-1.             00037670
           MOVE COM-ADDRESS2          TO NA-COMM-ADDRESS-2.             00037680
           MOVE COM-CITY              TO NA-COMM-CITY.                  00037690
           MOVE COM-STATE             TO NA-COMM-STATE.                 00037700
           MOVE COM-ZIP               TO NA-COMM-ZIP.                   00037710
           MOVE COM-SEX               TO NA-COMM-SEX.                   00037720
           MOVE COM-ZIP-PLUS4         TO NA-COMM-ZIP-PLUS4.             00037730
           MOVE COM-ENTITY-TYPE       TO NA-COMM-SYSTEM-TYPE.           00037740
           MOVE COM-RECORD-STATUS     TO NA-COMM-RECORD-STATUS.         00037750
                                                                        00037760
           IF COM-AREA-CODE NUMERIC                                     00037770
               MOVE COM-AREA-CODE TO NA-COMM-PHONE-AREA                 00037780
           ELSE                                                         00037790
               MOVE ZEROES TO NA-COMM-PHONE-AREA.                       00037800
                                                                        00037810
           MOVE COM-PHONE TO WS-PHONE.                                  00037820
           IF WS-PHONE3 NUMERIC                                         00037830
               MOVE WS-PHONE3 TO NA-COMM-PHONE-3                        00037840
           ELSE                                                         00037850
               MOVE ZEROES TO NA-COMM-PHONE-3.                          00037860
                                                                        00037870
           IF WS-PHONE4 NUMERIC                                         00037880
               MOVE WS-PHONE4 TO NA-COMM-PHONE-4                        00037890
           ELSE                                                         00037900
               MOVE ZEROES TO NA-COMM-PHONE-4.                          00037910
                                                                        00037920
           MOVE 'Y'  TO NA-COMM-FORMAT-SWITCH.                          00037930
           MOVE 'Y'  TO NA-COMM-CASE-SWITCH.                            00037940
           MOVE 'FM' TO NA-COMM-REQUEST-CODE.                           00037950
           MOVE ZERO TO NA-COMM-RETURN-CODE.                            00037960
                                                                        00037970
      *    EXEC CICS LINK PROGRAM ('NA203')                             00037980
      *                   COMMAREA(NAME-AND-ADDRESS-COMMAREA)           00037990
      *                   LENGTH  (NA-COMMAREA-LENGTH)                  00038000
      *                   LENGTH  (LENGTH OF NAME-AND-ADDRESS-COMMAREA) 00038010
      *                   RESP    (WS-CICS-RESP)                        00038020
      *                   END-EXEC.                                     00038030
                                                                        00038040
      *    IF  WS-CICS-RESP NOT = DFHRESP(NORMAL)                       00038050
      *        GO TO 9999-CICS-ERROR.                                   00038060
      *---------------------------------------------------------------* 00038070
      *    IF THERE WAS AN SQL ERROR OR MISC CICS ERROR IN NA203      * 00038080
      *    IT HAS ALREADY PLACED AN ERROR DIAGNOSTIC MAP ON THE       * 00038090
      *    TERMINAL, WE WANT TO STOP THIS PROGRAM HERE SO THAT        * 00038100
      *    THE DIAGNOSTIC IS NOT OVERLAYED.                           * 00038110
      *---------------------------------------------------------------* 00038120
           IF NA-SQL-FAILED OR NA-MISC-CICS-ERROR                       00038130
               EXEC CICS SEND CONTROL ALARM END-EXEC                    00038140
               EXEC CICS RETURN END-EXEC.                               00038150
                                                                        00038160
           IF NA-SUCCESSFUL                                             00038170
               MOVE NA-COMM-MAIL-LINE1 TO COM-DISPLAY-NAME              00038180
               MOVE COM-DISPLAY-NAME   TO MAP-DIS-NAMEO.                00038190
                                                                        00040280
       0330-PROCESS-SCREEN-CLEAR.                                       00040290
                                                                        00040300
           MOVE SPACES TO WS-NULL-FIELD.                                00040540
           EXEC CICS SEND FROM (WS-NULL-FIELD)                          00040550
                          RESP (WS-CICS-RESP)                           00040560
                          ERASE                                         00040570
                          LENGTH(72)                                    00040580
                          END-EXEC.                                     00040590
                                                                        00040600
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00040610
                GO TO 9999-CICS-ERROR.                                  00040620
                                                                        00040630
           EXEC CICS RETURN                                             00040640
                     END-EXEC.                                          00040650
                                                                        00041030
       0350-WRONG-KEY-HIT.                                              00041040
                                                                        00041050
           MOVE LOW-VALUES TO NA330M2I.                                 00041060
                                                                        00041070
           EXEC CICS RECEIVE MAP    ('NA330M2')                         00041080
                             RESP   (WS-CICS-RESP)                      00041090
                             END-EXEC.                                  00041100
                                                                        00041110
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00041120
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00041130
                GO TO 9999-CICS-ERROR.                                  00041140
                                                                        00041150
           PERFORM 0120-UPDATE-ID-MAP-FIELDS.                           00041170
                                                                        00041360
           MOVE 'NA030'           TO WS-MESSAGE-NUMBER1.                00041370
           PERFORM 0370-BUMP-ERROR-MESSAGES.                            00041380
                                                                        00041660
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00041670
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00041680
           ELSE                                                         00041690
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00041700
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00041710
               MOVE ALL '_' TO MAP-ACTION-CDO                           00041720
           ELSE                                                         00041730
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00041740
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00041750
               MOVE ALL '_' TO MAP-CUST-INFOO                           00041760
           ELSE                                                         00041770
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00041780
                                                                        00041800
           PERFORM 0280-MOVE-OUT-MAP-FIELDS.                            00041810
                                                                        00041820
           EXEC CICS SEND MAP    ('NA330M2')                            00041830
                          CURSOR (COMM-CURSOR-POSN)                     00041840
                          RESP   (WS-CICS-RESP)                         00041850
                          ERASE                                         00041860
                          END-EXEC.                                     00041870
                                                                        00041880
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00041890
                GO TO 9999-CICS-ERROR.                                  00041900
                                                                        00041910
           EXEC CICS RETURN TRANSID ('NAA1')                            00041920
                          COMMAREA (NA330M2O)
                          LENGTH    (32)
                            END-EXEC.                                   00041930
                                                                        00041940
       0370-BUMP-ERROR-MESSAGES.                                        00042380
                                                                        00042390
           MOVE COMM-MSG-ID(1)    TO WSQ-MSG-ID(1).                     00042400
           MOVE COMM-MSG-ID(2)    TO WSQ-MSG-ID(2).                     00042410
           MOVE COMM-MSG-ID(3)    TO WSQ-MSG-ID(3).                     00042420
           MOVE COMM-MSG-ID(4)    TO WSQ-MSG-ID(4).                     00042430
           MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1).                   00042440
           MOVE WSQ-MSG-ID(1)      TO COMM-MSG-ID(2).                   00042450
           MOVE WSQ-MSG-ID(2)      TO COMM-MSG-ID(3).                   00042460
           MOVE WSQ-MSG-ID(3)      TO COMM-MSG-ID(4).                   00042470
           PERFORM 0380-CHECK-ERROR-MESSAGES                            00042480
               VARYING ACTION-SUB FROM 1 BY 1                           00042490
                   UNTIL ACTION-SUB > 4.                                00042500
                                                                        00042510
       0380-CHECK-ERROR-MESSAGES.                                       00042520
                                                                        00042530
           IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND                 00042540
               COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES                00042550
               MOVE COMM-MSG-ID (ACTION-SUB) TO WT-C9999-ERROR-CODE     00042560
               DISPLAY 'WT-C9999-ERROR-CODE' WT-C9999-ERROR-CODE
               PERFORM 0400-GET-ERROR-MESSAGE                           00042570
               PERFORM 0390-CHECK-RETURN-CODE.                          00042580
                                                                        00042590
       0390-CHECK-RETURN-CODE.                                          00042600
                DISPLAY 'SQLCODE EDIT ERROR MESSAGE' SQLCODE
               IF SQLCODE = ZERO                                        00042610
                   MOVE WT-C9999-ERROR-CODE TO WS-C9999-ERROR-CODE      00042620
      *            MOVE WT-C9999-SEVERITY-CODE TO WS-C9999-SEVERITY-CODE00042630
                   MOVE EDIT-DESC TO WS-C9999-ERROR-MESSAGE             00042640
                   IF WT-C9999-ERROR-CODE = 'DB03A' OR 'DB03B'          00042650
                      MOVE WS-DISPLAY-SQLCODE-A TO WS-C9999-DB2-ERROR   00042660
                   ELSE                                                 00042670
                      MOVE SPACES TO WS-C9999-DB2-ERROR                 00042680
                   END-IF                                               00042690
                   MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (ACTION-SUB)  00042700
               ELSE                                                     00042710
                  IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND          00042720
                      COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES         00042730
                      MOVE '** INVALID ERROR MESSAGE ** ' TO            00042740
                           MAP-ERROR-MSGO (ACTION-SUB).                 00042750
                                                                        00042760
       0400-GET-ERROR-MESSAGE.                                          00042770
           EXEC SQL                                                     00042780
           SELECT EDIT_DESC
              INTO :EDIT-DESC
             FROM EDITCODE                                              00042790
            WHERE EDIT_CD = :WT-C9999-ERROR-CODE                        00042800
           END-EXEC.                                                    00042820
                                                                        00043090
       0410-DB2-ERROR.                                                  00043100
           DISPLAY '0410-DB2-ERROR'.
MANJU5     MOVE WS-SQLCODE TO WS-DB2I-MESSAGE
MANJU5     MOVE WS-DB2I-MESSAGE TO WS-C9999-ERROR-CODE.
MANJU5     DISPLAY 'SQL CODE:' WS-DB2I-MESSAGE.
           MOVE WS-SQL-ERROR-MSG TO WS-C9999-ERROR-MESSAGE.
           MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (1).
           PERFORM 0280-MOVE-OUT-MAP-FIELDS.
           EXEC CICS SEND MAP    ('NA330M2')
                          CURSOR
                          ERASE
                          RESP (WS-CICS-RESP)
                          END-EXEC.
           EXEC CICS RETURN TRANSID ('NAA1')
                          COMMAREA (NA330M2O)
                          LENGTH    (32)
                     END-EXEC.
           EXEC CICS RETURN
                     END-EXEC.
       0410-EXIT.                                                       00043100
            EXIT.

      *---------------------------------------------------------------* 00043230
      * THIS ROUTINE TRANSFERS CONTROL TO AN ONLINE ERROR PROGRAM     * 00043240
      * WHICH DISPLAYS THE ERRORS ON THE SCREEN SUPPLYING INFORMATION * 00043250
      * FOR THE HELP-DESK  TO AID PROGRAMMERS IN DEBUGGING            * 00043260
      *---------------------------------------------------------------* 00043270
                                                                        00043280
           COPY CICSERR.                                                00043290
                                                                        00043300
