   CBL DATA(24)                                                         00000010
       IDENTIFICATION DIVISION.                                         00000020
       PROGRAM-ID. NA330.                                               00000030
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
           05  WS-MESSAGE-NUMBER1           PIC X(5)   VALUE SPACE.     00003480
           05  WS-MESSAGE-NUMBER2           PIC X(5)   VALUE SPACE.     00003490
           05  WS-MESSAGE-NUMBER3           PIC X(5)   VALUE SPACE.     00003500
           05  WS-MESSAGE-NUMBER4           PIC X(5)   VALUE SPACE.     00003510
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
           05  WS-EDIT-SW                   PIC X.                      00004240
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
                                                                        00004570
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
       01  WS-TS-QUEUE.                                                 00005490
           COPY NA200C01.                                               00005500
           02  COM-AGNTNAME.                                            00005510
               05 COM-IDNTITY               PIC X(8).                   00005520
               05 COM-LAST-NAME             PIC X(20).                  00005530
               05 COM-FIRST-NAME            PIC X(15).                  00005540
               05 COM-MIDDLE-NAME           PIC X(15).                  00005550
               05 COM-PREFIX                PIC X(4).                   00005560
               05 COM-SUFFIX1               PIC X(4).                   00005570
               05 COM-SUFFIX2               PIC X(4).                   00005580
               05 COM-COMPANY-IND           PIC X(1).                   00005590
               05 COM-COMPANY-IN-ADDRESS    PIC X(1).                   00005600
               05 COM-COMPANY-NAME          PIC X(30).                  00005610
               05 COM-DISPLAY-NAME          PIC X(30).                  00005620
               05 COM-NICKNAME              PIC X(10).                  00005630
               05 COM-ADDRESS1              PIC X(30).                  00005640
               05 COM-ADDRESS2              PIC X(30).                  00005650
               05 COM-CITY                  PIC X(30).                  00005660
               05 COM-STATE                 PIC X(2).                   00005670
               05 COM-ZIP                   PIC X(5).                   00005680
               05 COM-ZIP-PLUS4             PIC X(4).                   00005690
               05 COM-COUNTY-CODE           PIC X(5).                   00005700
               05 COM-AREA-CODE             PIC X(3).                   00005710
               05 COM-PHONE                 PIC X(7).                   00005720
               05 COM-PHONE-EXTENSION       PIC X(4).                   00005730
               05 COM-SSN                   PIC X(9).                   00005740
               05 COM-SEX                   PIC X(1).                   00005750
      ******   05 COM-BIRTH-DATE            PIC S9(6)    COMP-3.        00005760
               05 COM-BIRTH-DATE            PIC S9(8)    COMP-3.        00005770
               05 COM-FINALST-REAS-CODE     PIC X(3).                   00005780
               05 COM-FINALST-OVRD-IND      PIC X(1).                   00005790
               05 COM-DUP-ADDR-OVRD-IND     PIC X(1).                   00005800
               05 COM-EFFECTIVE-DATE        PIC S9(6)    COMP-3.        00005810
               05 COM-CHANGE-DATE           PIC S9(6)    COMP-3.        00005820
               05 COM-CHANGE-LOGON          PIC X(8).                   00005830
               05 COM-ENTITY-TYPE           PIC X(2).                   00005840
               05 COM-RECORD-STATUS         PIC X(1).                   00005850
               05 COM-ALT-ADDRESS-IND       PIC X(1).                   00005860
               05 COM-FUTURE-ADDRESS-IND    PIC X(1).                   00005870
               05 COM-RECORD-ORIGIN         PIC X(1).                   00005880
               05 COM-COMBINED-STATUS       PIC X(2).                   00005890
               05 COM-SITE-CODE             PIC X(2).                   00005900
               05 COM-NAME-KEY1             PIC X(8).                   00005910
               05 COM-NAME-KEY2             PIC X(8).                   00005920
               05 COM-NAME-KEY3             PIC X(2).                   00005930
               05 COM-ADDRESS-KEY1          PIC X(10).                  00005940
               05 COM-ASSOCIATION1          PIC X(5).                   00005950
               05 COM-ASSOCIATION2          PIC X(5).                   00005960
               05 COM-ASSOCIATION3          PIC X(5).                   00005970
               05 COM-ENTITY-LITERAL        PIC X(10).                  00005980
               05 COM-CASE-SW               PIC X.                      00005990
               05 COM-FINALIST-SW           PIC X.                      00006000
               05 COM-FAX-AREA-CODE         PIC X(3).                   00006010
               05 COM-FAX-PHONE             PIC X(7).                   00006020
               05 COM-EMAIL1                PIC X(50).                  00006030
               05 COM-POSTAL-CODE           PIC X(09).                  00006040
               05 COM-COUNTRY-CODE          PIC X(03).                  00006050
                                                                        00006060
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
      *  ***************************************************************00006190
      *  * REPLACED GENERIC AGNTNAME SELECT WITH SPECIFIC COLUMN NAMES* 00006200
      *  ***************************************************************00006210
      *                                                                 00006220
           EXEC SQL DECLARE AGNTCUR CURSOR FOR                          00006230
               SELECT IDNTITY                                           00006240
                    , LAST_NAME                                         00006250
                    , FIRST_NAME                                        00006260
                    , MIDDLE_NAME                                       00006270
                    , PREFIX                                            00006280
                    , SUFFIX1                                           00006290
                    , SUFFIX2                                           00006300
                    , COMPANY_IND                                       00006310
                    , COMPANY_IN_ADDRESS                                00006320
                    , COMPANY_NAME                                      00006330
                    , DISPLAY_NAME                                      00006340
                    , NICKNAME                                          00006350
                    , ADDRESS1                                          00006360
                    , ADDRESS2                                          00006370
                    , CITY                                              00006380
                    , STATE                                             00006390
                    , ZIP                                               00006400
                    , ZIP_PLUS4                                         00006410
                    , COUNTY_CODE                                       00006420
                    , AREA_CODE                                         00006430
                    , PHONE                                             00006440
                    , PHONE_EXTENSION                                   00006450
                    , SSN                                               00006460
                    , SEX                                               00006470
                    , BIRTH_DATE                                        00006480
                    , FINALST_REAS_CODE                                 00006490
                    , FINALST_OVRD_IND                                  00006500
                    , DUP_ADDR_OVRD_IND                                 00006510
                    , EFFECTIVE_DATE                                    00006520
                    , CHANGE_DATE                                       00006530
                    , CHANGE_LOGON                                      00006540
                    , ENTITY_TYPE                                       00006550
                    , RECORD_STATUS                                     00006560
                    , ALT_ADDRESS_IND                                   00006570
                    , FUTURE_ADDRESS_IND                                00006580
                    , RECORD_ORIGIN                                     00006590
                    , COMBINED_STATUS                                   00006600
                    , SITE_CODE                                         00006610
                    , NAME_KEY1                                         00006620
                    , NAME_KEY2                                         00006630
                    , NAME_KEY3                                         00006640
                    , ADDRESS_KEY1                                      00006650
                    , ASSOCIATION1                                      00006660
                    , ASSOCIATION2                                      00006670
                    , ASSOCIATION3                                      00006680
                    , FAX_AREA_CODE                                     00006690
                    , FAX_PHONE                                         00006700
                    , OFF_PRODUCERID                                    00006710
                    , EXTN_AGENCY_NUMBER                                00006720
                    , PRIMRY_AFFL_CODE                                  00006730
                    , SERVFEE_AGREE_IND                                 00006740
                    , SERVFEE_AGREE_DATE                                00006750
                    , EMAIL1                                            00006760
                    , EMAIL2                                            00006770
                    , PASSWORD                                          00006780
                    , CELLULAR_AREA_CODE                                00006790
                    , CELLULAR_PHONE                                    00006800
                    , INTEREST_CD                                       00006810
                    , PRIMRY_BUSINESS_CD                                00006820
                    , PRIMRY_BUS_REVS_DT                                00006830
                    , SELLING_SM_GRP_YR                                 00006840
                    , IN_INS_SINCE_YEAR                                 00006850
                    , COUNTRY                                           00006860
                    , FOREIGN_FLG                                       00006870
                    , RANKING_CD                                        00006880
                    , DO_NO_UPDATE                                      00006890
                    , TIN                                               00006900
                    , DOING_BUS_AS_NAME                                 00006910
                    , CERTIFIED_IND                                     00006920
                    , PAYEE_IND                                         00006960
               FROM AGNTNAME                                            00006970
               WHERE IDNTITY = :WS-IDNTITY-NUM                          00006980
           END-EXEC.                                                    00006990
                                                                        00007000
           EXEC SQL DECLARE CASECUR CURSOR FOR                          00007010
               SELECT                                                   00007020
                 IDNTITY, LAST_NAME, FIRST_NAME, MIDDLE_NAME,           00007030
                 PREFIX, SUFFIX1, SUFFIX2,                              00007040
                 COMPANY_IND, COMPANY_IN_ADDRESS,                       00007050
                 COMPANY_NAME, DISPLAY_NAME, NICKNAME,                  00007060
                 ADDRESS1, ADDRESS2,                                    00007070
                 CITY, STATE, ZIP, ZIP_PLUS4, COUNTY_CODE, AREA_CODE,   00007080
                 PHONE,  PHONE_EXTENSION,  SSN, SEX, BIRTH_DATE,        00007090
                 FINALST_REAS_CODE, FINALST_OVRD_IND, DUP_ADDR_OVRD_IND,00007100
                 EFFECTIVE_DATE, CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,00007110
                 RECORD_STATUS, ALT_ADDRESS_IND, FUTURE_ADDRESS_IND,    00007120
                 RECORD_ORIGIN, COMBINED_STATUS, SITE_CODE, NAME_KEY1,  00007130
                 NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,                    00007140
                 ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,              00007150
                 FAX_AREA_CODE, FAX_PHONE, EMAIL, PASSWORD              00007160
               FROM CASENAME                                            00007170
               WHERE IDNTITY = :WS-IDNTITY-NUM                          00007180
           END-EXEC.                                                    00007190
                                                                        00007200
       01  WS-NAME-ADDRESS-DETAIL.                                      00007210
           COPY NA200C02.                                               00007220
       01  SECURITY-AREA.                                               00007230
           COPY SECCOMMC.                                               00007240
           COPY SYSBUSY.                                                00007250
           COPY EI100C01.                                               00007260
           COPY NA330M1.                                                00007270
           COPY DFHAID.                                                 00007280
           COPY ATTRB.                                                  00007290
           COPY DFHBMSCA.                                               00007300
           02  UNPROT-NORM-FSET  PIC X(1)  VALUE 'E'.                   00007310
                                                                        00007320
                                                                        00007330
       LINKAGE SECTION.                                                 00007340
       01  DFHCOMMAREA                      PIC X.                      00007350
                                                                        00007360
       01  WS-LINK-STORAGE.                                             00007370
           02  WS-LINK-SIX-HUNDRED          PIC X(600).                 00007380
           02  WS-LINK-STUFF      OCCURS 0 TO 3496 TIMES                00007390
                                  DEPENDING ON WS-LINK-LENGTH           00007400
                                            PIC X.                      00007410
                                                                        00007420
       01  TCTUAR.                                                      00007430
           05  FILLER                  PIC X(36).                       00007440
           05  TCTUA-DEMO-DATA.                                         00007450
               10  FILLER              PIC S9(4) COMP.                  00007460
                   88  INVALID-DEMO-DATA               VALUE 0.         00007470
               10  FILLER              PIC X(98).                       00007480
                                                                        00007490
      ******************************************************************00007500
                                                                        00007510
       PROCEDURE DIVISION.                                              00007520
                                                                        00007530
       0010-BEGIN-PROGRAM.                                              00007540
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
                       WS-ERROR-FIELDS.                                 00007930
                                                                        00007940
           EXEC CICS HANDLE CONDITION ERROR(9999-CICS-ERROR)            00007950
                                      END-EXEC.                         00007960
                                                                        00007970
           MOVE LOW-VALUES TO NA330M1O SYSBUSYO.                        00007980
                                                                        00007990
MANJU *0020-DB2-SQL-PREPARATION.                                        00008000
      *---------------------------------------------------------------* 00008010
      *    CHECK TO SEE IF THE DB2 ATTACH FACILITY IS ACTIVE.         * 00008020
      *---------------------------------------------------------------* 00008030
      *    EXEC CICS EXTRACT EXIT                                       00008040
      *        PROGRAM ('DSNCEXT1')                                     00008050
      *        ENTRYNAME ('DSNCSQL')                                    00008060
      *        GASET     (WS-GASET)                                     00008070
      *        GALENGTH  (WS-GALENGTH)                                  00008080
      *        RESP      (WS-CICS-RESP)                                 00008090
      *    END-EXEC.                                                    00008100
                                                                        00008110
      *    IF WS-CICS-RESP = DFHRESP(INVEXITREQ)                        00008120
      *        MOVE 'PGM: NA330, PARA 0005- DATABASE NOT AVAILABLE'     00008130
      *            TO WS-SQL-ERROR-MESSAGE                              00008140
      *        MOVE +0000 TO SQLCODE                                    00008150
      *        GO TO 0410-DB2-ERROR.                                    00008160
                                                                        00008170
       0030-WRITE-COST-JOURNAL.                                         00008180
                                                                        00008230
      *    MOVE SPACES TO WS-COST-CARR-CODE.                            00008240
           COPY CAPRCINC.                                               00008250
                                                                        00008260
       0040-READ-NAQ1-TS-QUEUE.                                         00008270
                                                                        00008280
MANJU      DISPLAY 'INSIDE NA330 READ QUEUE'                            00008350
           MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.           00008360
           MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.           00008370
           EXEC CICS READQ TS QUEUE  (WS-TS-QUEUE-NAME)                 00008380
                              SET    (ADDRESS OF WS-LINK-STORAGE)       00008390
                              LENGTH (WS-ZERO-LENGTH)                   00008400
                              ITEM   (WS-TS-ITEM)                       00008410
                              RESP   (WS-CICS-RESP)                     00008420
                              END-EXEC.                                 00008430
                                                                        00008440
                                                                        00008450
           IF WS-CICS-RESP = DFHRESP(QIDERR)                            00008460
               PERFORM 0340-WRITE-NAQ1-QUEUE                            00008470
           ELSE                                                         00008480
              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                     00008490
                  GO TO 9999-CICS-ERROR.                                00008500
                                                                        00008510
           MOVE WS-ZERO-LENGTH TO WS-ZERO-LENGTH.                       00008520
           COMPUTE WS-LINK-LENGTH = (WS-ZERO-LENGTH - +600).            00008530
           MOVE WS-LINK-STORAGE TO WS-TS-QUEUE.                         00008540
           MOVE COMM-IDNTITY TO WS-IDNTITY-NUM.                         00008550
                                                                        00008560
      *---------------------------------------------------------------* 00008570
      *0050-CHECK-SECURITY-CLEARANCE.                                 * 00008580
      *                                                               * 00008590
      *************************************************               * 00008600
      *   LINK TO SECURITY MODULE - FOR RESOURCE CHECK                * 00008610
      *************************************************               * 00008620
      *    MOVE 'NAMENU  '         TO SEC-RESOURCE-NAME.              * 00008630
      *    MOVE 'Y'                TO SEC-CHK-RESOURCE.               * 00008640
      *    MOVE SPACE              TO SEC-RETURN-CODE.                * 00008650
      *    MOVE 'A'                TO SEC-FUNCTION-CODE.              * 00008660
      *                                                               * 00008670
      *    EXEC CICS LINK                                             * 00008680
      *              PROGRAM('SECCHECK')                              * 00008690
      *              COMMAREA(SECURITY-COMM-AREA)                     * 00008700
      *              LENGTH(31)                                       * 00008710
      *    END-EXEC.                                                  * 00008720
      *                                                               * 00008730
      *    IF  SEC-RETURN-CODE = 'B'                                  * 00008740
      *        MOVE 100 TO SQLCODE                                    * 00008750
      *        MOVE 'SC002' TO WS-MESSAGE-NUMBER1                     * 00008760
      *        GO TO 0300-INITIATE-ROUTER                             * 00008770
      *    ELSE                                                       * 00008780
      *    IF  SEC-RETURN-CODE = 'C'                                  * 00008790
      *        MOVE 100 TO SQLCODE                                    * 00008800
      *        MOVE 'SC003' TO WS-MESSAGE-NUMBER1                     * 00008810
      *        GO TO 0300-INITIATE-ROUTER                             * 00008820
      *    ELSE                                                       * 00008830
      *    IF  SEC-RETURN-CODE = 'D'                                  * 00008840
      *        MOVE 100 TO SQLCODE                                    * 00008850
      *        MOVE 'SC004' TO WS-MESSAGE-NUMBER1                     * 00008860
      *        GO TO 0300-INITIATE-ROUTER                             * 00008870
      *    ELSE                                                       * 00008880
      *    IF  SEC-RETURN-CODE = 'E'                                  * 00008890
      *        MOVE 100 TO SQLCODE                                    * 00008900
      *        MOVE 'SC005' TO WS-MESSAGE-NUMBER1                     * 00008910
      *        GO TO 0300-INITIATE-ROUTER                             * 00008920
      *    ELSE                                                       * 00008930
      *    IF  SEC-RETURN-CODE = 'F' OR 'G' OR 'H' OR 'I'             * 00008940
      *        MOVE 100 TO SQLCODE                                    * 00008950
      *        MOVE 'SC006' TO WS-MESSAGE-NUMBER1                     * 00008960
      *        GO TO 0300-INITIATE-ROUTER                             * 00008970
      *    ELSE                                                       * 00008980
      *    IF  SEC-RETURN-CODE NOT = 'A'                              * 00008990
      *        MOVE 100 TO SQLCODE                                    * 00009000
      *        MOVE 'SC007' TO WS-MESSAGE-NUMBER1                     * 00009010
      *        GO TO 0300-INITIATE-ROUTER.                            * 00009020
      *                                                               * 00009030
      *---------------------------------------------------------------* 00009040
                                                                        00009050
       0060-PROCESS-MENU-SCREEN.                                        00009060
                                                                        00009070
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00009080
               MOVE COMM-MSG-ID(1) TO WS-MESSAGE-NUMBER1                00009090
               MOVE COMM-MSG-ID(2) TO WS-MESSAGE-NUMBER2                00009100
               MOVE COMM-MSG-ID(3) TO WS-MESSAGE-NUMBER3                00009110
               MOVE COMM-MSG-ID(4) TO WS-MESSAGE-NUMBER4                00009120
               GO TO 0190-SEND-NA330M1-MAP                              00009130
           ELSE                                                         00009140
              IF COMM-NEXT-FUNCTION = '00'                              00009150
                  GO TO 0080-RETURN-FROM-ROUTER                         00009160
              ELSE                                                      00009170
                 IF EIBAID = DFHENTER                                   00009180
                     GO TO 0070-EDIT-COMMAND-LINE                       00009190
                 ELSE                                                   00009200
                    IF EIBAID = DFHCLEAR                                00009210
                        GO TO 0330-PROCESS-SCREEN-CLEAR                 00009220
      *             ELSE                                                00009230
      *                IF EIBAID = DFHPF18 OR DFHPF6                    00009240
      *                    GO TO 0310-PROCESS-TUTORIAL-REQUEST          00009250
                       ELSE                                             00009260
      *---------------------------------------------------------------* 00009270
      *                   IF EIBTRNID = 'TU01'                        * 00009280
      *                       GO TO 0320-RETURN-FROM-TUTORIAL         * 00009290
      *                   ELSE                                        * 00009300
      *---------------------------------------------------------------* 00009310
                             IF EIBAID = DFHPA1 OR DFHPA2               00009320
                                 GO TO 0360-RETURN-TO-MT-BILLION        00009330
                             ELSE                                       00009340
                                IF EIBAID = DFHPF14 OR DFHPF2           00009350
                                    GO TO 0090-EXECUTE-STACK-FUNCTION   00009360
                                ELSE                                    00009370
                                   IF EIBAID = DFHPF3                   00009380
                                       GO TO 0100-RETURN-TO-BROWSE      00009390
                                   ELSE                                 00009400
                                      GO TO 0350-WRONG-KEY-HIT.         00009410
                                                                        00009420
       0070-EDIT-COMMAND-LINE.                                          00009430
           DISPLAY '0070-EDIT-COMMAND-LINE'                             00009440
           MOVE LOW-VALUES TO NA330M1I.                                 00009450
                                                                        00009460
           EXEC CICS RECEIVE MAP    ('NA330M1')                         00009470
                             RESP   (WS-CICS-RESP)                      00009480
                             END-EXEC.                                  00009490
                                                                        00009500
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00009510
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00009520
                GO TO 9999-CICS-ERROR.                                  00009530
                                                                        00009540
           IF COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'RP'       00009550
              MOVE MAP-CIMI   TO  WS-IDNTITY-CHECK                      00009560
           END-IF.                                                      00009570
                                                                        00009580
           PERFORM 0110-UPDATE-COMMAND-LINE.                            00009590
                                                                        00009600
      *---------------------------------------------------------------* 00009610
      * IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE = 'Y' THEN SOMETHING* 00009620
      * ON THE COMMAND LINE CHANGED AND THE ROUTER SHOULD BE STARTED. * 00009630
      *---------------------------------------------------------------* 00009640
                                                                        00009650
MANJU      IF COMM-NEXT-FUNCTION = '01'                                 00009750
                   PERFORM 0120-UPDATE-ID-MAP-FIELDS                    00009760
                   PERFORM 0130-EDIT-NA330M1-MAP                        00009770
                   GO TO 0190-SEND-NA330M1-MAP.                         00009780
                                                                        00009660
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED  = 'Y'     00009670
               GO TO 0300-INITIATE-ROUTER.                              00009680
                                                                        00009690
      *---------------------------------------------------------------* 00009700
      * IF COMM-NEXT-FUNCTION = '01' THEN THE CUSTOMER IS RE-ENTERING * 00009710
      * THE PROGRAM AND MUST CHECK FOR FOR ERRORS THEN ADD THE RECORD * 00009720
      *---------------------------------------------------------------* 00009730
                                                                        00009740
                                                                        00009790
       0080-RETURN-FROM-ROUTER.                                         00009800
      *---------------------------------------------------------------* 00009810
      * IF COMM-NEXT-FUNCTION = '00' SEND OUT THE SYSTEM SELECTION    * 00009820
      *   MAP                                                         * 00009830
      *---------------------------------------------------------------* 00009840
                                                                        00009850
           GO TO 0180-SEND-NA330M1-FIRST-TIME.                          00009860
                                                                        00009870
       0090-EXECUTE-STACK-FUNCTION.                                     00009880
                                                                        00009890
      *---------------------------------------------------------------* 00009900
      * THIS FUNCTION TELLS THE ROUTER THAT THE CUSTOMER WISHES TO    * 00009910
      * EXECUTE A SERIES OF COMMAND LINE CHANGES THAT ARE STORED      * 00009920
      * IN THE STACK COMMAND DATASET (THIS IS A FUTURE FUNCTION)      * 00009930
      *---------------------------------------------------------------* 00009940
                                                                        00009950
           MOVE 'GO'   TO COMM-SYSTEM-CODE.                             00009960
           MOVE SPACES TO COMM-ACTION-CODE.                             00009970
           MOVE 'Y'    TO COMM-CMDLINE-CHANGED.                         00009980
           GO TO 0300-INITIATE-ROUTER.                                  00009990
                                                                        00010000
       0100-RETURN-TO-BROWSE.

           MOVE SPACES   TO COMM-SYSTEM-CODE.
           MOVE SPACES   TO COMM-ACTION-CODE.
           MOVE SPACES   TO COMM-CUSTOMER-INFO.

           MOVE 'NAA0'            TO COMM-PREVIOUS-TRANID.
           MOVE '00'              TO COMM-NEXT-FUNCTION.
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)
                          COMM-MSG-ID (3) COMM-MSG-ID (4).

           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME.
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)
                               FROM   (WS-TS-QUEUE)
                               LENGTH (WS-ZERO-LENGTH)
                               ITEM   (WS-TS-ITEM)
                               RESP   (WS-CICS-RESP)
                               REWRITE
                               MAIN
                               END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           EXEC CICS START TRANSID('NAM1')
                           TERMID (EIBTRMID)
                           RESP   (WS-CICS-RESP)
                           END-EXEC
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           EXEC CICS RETURN
                     END-EXEC.
                                                                        00010070
       0110-UPDATE-COMMAND-LINE.                                        00010080
                                                                        00010090
           IF MAP-SYSTEM-CDF = HEX80                                    00010100
               MOVE SPACES TO COMM-SYSTEM-CODE                          00010110
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED                      00010120
           ELSE                                                         00010130
               IF MAP-SYSTEM-CDL > ZERO                                 00010140
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED           00010150
                  MOVE MAP-SYSTEM-CDI TO COMM-SYSTEM-CODE.              00010160
                                                                        00010170
           IF MAP-ACTION-CDF = HEX80                                    00010180
               MOVE SPACES TO COMM-ACTION-CODE                          00010190
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED                      00010200
           ELSE                                                         00010210
               IF MAP-ACTION-CDL > ZERO                                 00010220
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED           00010230
                  MOVE MAP-ACTION-CDI TO COMM-ACTION-CODE.              00010240
                                                                        00010250
           IF MAP-CUST-INFOF = HEX80                                    00010260
               MOVE SPACES TO COMM-CUSTOMER-INFO                        00010270
               MOVE 'Y'            TO COMM-KEY-CHANGED                  00010280
           ELSE                                                         00010290
               IF MAP-CUST-INFOL > ZERO                                 00010300
                  MOVE 'Y'            TO COMM-KEY-CHANGED               00010310
                  MOVE MAP-CUST-INFOI TO COMM-CUSTOMER-INFO.            00010320
                                                                        00010330
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
               MOVE SPACES TO COM-FIRST-NAME                            00010450
           ELSE                                                         00010460
               IF MAP-FIRST-NAMEL > ZERO                                00010470
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
               DISPLAY 'FIRST-NAME'
               MOVE SPACES TO COM-LAST-NAME                             00010590
           ELSE                                                         00010600
               DISPLAY 'FIRST-NAME1'
               IF MAP-LAST-NAMEL > ZERO                                 00010610
               DISPLAY 'FIRST-NAME2'
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00010620
                  MOVE MAP-LAST-NAMEI TO COM-LAST-NAME.                 00010630

           DISPLAY 'COM-LAST-NAME:' COM-LAST-NAME
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
           IF MAP-COMP-IN-ADDF = HEX80                                  00011930
           IF MAP-COMP-IN-ADDI = SPACES                                 00011940
               MOVE 'Y' TO COM-COMPANY-IN-ADDRESS                       00011950
           ELSE                                                         00011960
               IF MAP-COMP-IN-ADDL > ZERO                               00011970
                  MOVE MAP-COMP-IN-ADDI TO COM-COMPANY-IN-ADDRESS.      00011980
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
           DISPLAY 'COM-FINALST-OVRD-IND:' COM-FINALST-OVRD-IND         00012170
           IF MAP-ADDR-OVRIDEF = HEX80                                  00012180
               MOVE SPACES TO COM-FINALST-OVRD-IND                      00012190
               DISPLAY 'FINALST_OVRD_IND1'
           ELSE                                                         00012200
               DISPLAY 'FINALST_OVRD_IND2'
               IF MAP-ADDR-OVRIDEL > ZERO                               00012210
               DISPLAY 'FINALST_OVRD_IND3'
                  MOVE MAP-ADDR-OVRIDEI TO COM-FINALST-OVRD-IND.        00012220
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
           IF MAP-EFF-DATEF = HEX80                                     00013130
               MOVE ZEROS TO COM-EFFECTIVE-DATE                         00013140
           ELSE                                                         00013150
               IF MAP-EFF-DATEL  > ZERO                                 00013160
                  DISPLAY 'MAP-EFF-DATEI:' MAP-EFF-DATEI
                  INSPECT MAP-EFF-DATEI REPLACING ALL '-' BY 'A'        00013170
                  DISPLAY 'MAP-EFF-DATEI:' MAP-EFF-DATEI
                  EXEC CICS BIF DEEDIT FIELD(MAP-EFF-DATEI)             00013180
                                       LENGTH(8)                        00013190
                                       END-EXEC                         00013200
                  MOVE MAP-EFF-DATEI TO WS-MAP-DATE                     00013210
                  DISPLAY 'WS-MAP-DATE:' WS-MAP-DATE
                  MOVE WS-MAP-BIF    TO COM-EFFECTIVE-DATE              00013220
                  DISPLAY 'COM-EFFECTIVE-DATE:' COM-EFFECTIVE-DATE.
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
           MOVE TCTUA-DEMO-DATA TO WS-DEMOGRAPHIC-COMM-AREA.            00013570
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
           MOVE WS-MAP-BIF       TO COM-CHANGE-DATE.                    00013770
                                                                        00013780
                                                                        00013790
       0130-EDIT-NA330M1-MAP.                                           00013800
           DISPLAY '0130-EDIT-NA330M1-MAP'                              00013810
           MOVE 'N' TO WS-EDIT-SW.                                      00013820
                                                                        00013830
      *---------------------------------------------------------------* 00013840
      * EDIT CUSTOMER TYPE FOR VALIDITY                               * 00013850
      *---------------------------------------------------------------* 00013860
           DISPLAY 'COM-ENTITY-LITERAL' COM-ENTITY-LITERAL              00013870
           INSPECT COM-ENTITY-LITERAL                                   00013880
               REPLACING ALL 'a' BY 'A'                                 00013890
                             'b' BY 'B'                                 00013900
                             'c' BY 'C'                                 00013910
                             'd' BY 'D'                                 00013920
                             'e' BY 'E'                                 00013930
                             'f' BY 'F'                                 00013940
                             'g' BY 'G'                                 00013950
                             'h' BY 'H'                                 00013960
                             'i' BY 'I'                                 00013970
                             'j' BY 'J'                                 00013980
                             'k' BY 'K'                                 00013990
                             'l' BY 'L'                                 00014000
                             'm' BY 'M'                                 00014010
                             'n' BY 'N'                                 00014020
                             'o' BY 'O'.                                00014030
                                                                        00014040
           INSPECT COM-ENTITY-LITERAL                                   00014050
               REPLACING ALL 'p' BY 'P'                                 00014060
                             'q' BY 'Q'                                 00014070
                             'r' BY 'R'                                 00014080
                             's' BY 'S'                                 00014090
                             't' BY 'T'                                 00014100
                             'u' BY 'U'                                 00014110
                             'v' BY 'V'                                 00014120
                             'w' BY 'W'                                 00014130
                             'x' BY 'X'                                 00014140
                             'y' BY 'Y'                                 00014150
                             'z' BY 'Z'.                                00014160
           MOVE COM-ENTITY-LITERAL TO WT-C8997A-KEY.                    00014170
           MOVE WT-CNTL8997A       TO TURBO-CNTL-AREA.                  00014180
           PERFORM 10000-CALL-GSF.                                      00014190
           MOVE TURBO-CNTL-AREA    TO WT-CNTL8997A.                     00014200
MANJU      MOVE ZERO TO WT-C8997A-RETURN.
      *    IF  WT-C8997A-RETURN = ZERO                                  00014210
               MOVE ZERO TO WT-C8997-RETURN                             00014220
      *        MOVE WT-C8997A-SYSTEM-CD TO COM-ENTITY-TYPE.             00014230
                                                                        00014240
      *---------------------------------------------------------------* 00014250
      *   NOTE MAKE SURE WT-C8997A-SYSTEM-DESC IS IN UPPER CASE ON    * 00014260
      *   THE TURBO SYSTEM.                                           * 00014270
      *---------------------------------------------------------------* 00014280
                                                                        00014290
           IF  WT-C8997A-RETURN NOT = ZERO                              00014300
              MOVE COM-ENTITY-TYPE    TO WT-C8997-SYSTEM-CD             00014310
              MOVE WT-CNTL8997        TO TURBO-CNTL-AREA                00014320
              PERFORM 10000-CALL-GSF                                    00014330
              MOVE TURBO-CNTL-AREA    TO WT-CNTL8997.                   00014340
                                                                        00014350
           IF  WT-C8997-RETURN NOT = ZERO                               00014360
                   MOVE 'Y'      TO WS-EDIT-SW                          00014370
                   MOVE -1       TO MAP-ENTITY-TYPEL                    00014380
                   MOVE DFHBMBRY TO MAP-ENTITY-TYPEA                    00014390
                   MOVE 'NA004'  TO WS-HOLD-MESSAGE                     00014400
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00014410
                                                                        00014420
           IF  COM-ENTITY-LITERAL NOT = 'CAM' AND 'AM' AND 'ML' AND     00014430
               'MAIL' AND 'BR' AND 'BROKER' AND 'LB' AND 'LIST BILL'    00014440
               AND 'BA' AND 'BILL ADDR'                                 00014450
               AND 'LD' AND 'LEAD'                                      00014460
               AND 'RP' AND 'NOI'                                       00014470
                   MOVE 'Y'      TO WS-EDIT-SW                          00014480
                   MOVE -1       TO MAP-ENTITY-TYPEL                    00014490
                   MOVE DFHBMBRY TO MAP-ENTITY-TYPEA                    00014500
                   MOVE 'NA004'  TO WS-HOLD-MESSAGE                     00014510
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00014520
                                                                        00014530
      *---------------------------------------------------------------* 00014540
      * EDIT CUSTOMER STATUS FOR VALIDITY                             * 00014550
      *---------------------------------------------------------------* 00014560
           MOVE COM-RECORD-STATUS  TO WT-C2006-STATUS-CD.               00014570
           MOVE WT-CNTL2006        TO TURBO-CNTL-AREA.                  00014580
           PERFORM 10000-CALL-GSF.                                      00014590
           MOVE TURBO-CNTL-AREA    TO WT-CNTL2006.                      00014600
MANJU      MOVE ZERO TO WT-C2006-RETURN
           IF  WT-C2006-RETURN NOT = ZERO                               00014610
                   MOVE 'Y'      TO WS-EDIT-SW                          00014620
                   MOVE -1       TO MAP-CUST-STATUSL                    00014630
                   MOVE DFHBMBRY TO MAP-CUST-STATUSA                    00014640
                   MOVE 'NA005'  TO WS-HOLD-MESSAGE                     00014650
                   PERFORM 0160-SLIDE-ERROR-MESSAGES                    00014660
           ELSE                                                         00014670
              IF COM-ENTITY-TYPE = 'BR' OR 'ML'                         00014680
                 MOVE COM-RECORD-STATUS  TO WS-CUSTOMER-STATUS          00014690
                 IF  INVALID-CUSTOMER-STATUS                            00014700
                     MOVE 'Y'      TO WS-EDIT-SW                        00014710
                     MOVE -1       TO MAP-CUST-STATUSL                  00014720
                     MOVE DFHBMBRY TO MAP-CUST-STATUSA                  00014730
                     MOVE 'NA005'  TO WS-HOLD-MESSAGE                   00014740
                     PERFORM 0160-SLIDE-ERROR-MESSAGES                  00014750
                 END-IF                                                 00014760
              END-IF                                                    00014770
           END-IF.                                                      00014780
                                                                        00014790
      *---------------------------------------------------------------* 00014800
      * EDIT CUSTOMER SSN STATUS FOR VALIDITY                         * 00014810
      *---------------------------------------------------------------* 00014820
           IF COM-SSN NOT = SPACES AND COM-SSN NOT = LOW-VALUES         00014830
               IF COM-SSN NOT NUMERIC                                   00014840
                   MOVE 'Y'      TO WS-EDIT-SW                          00014850
                   MOVE -1       TO MAP-SSNL                            00014860
                   MOVE DFHBMBRY TO MAP-SSNA                            00014870
                   MOVE 'NA031'  TO WS-HOLD-MESSAGE                     00014880
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00014890
                                                                        00014900
      *---------------------------------------------------------------* 00014910
      * EDIT CUSTOMER PHONE NUMBER FOR VALIDITY                       * 00014920
      *---------------------------------------------------------------* 00014930
           IF COM-AREA-CODE NOT = SPACES AND                            00014940
               COM-AREA-CODE NOT = LOW-VALUES                           00014950
               IF COM-AREA-CODE NOT NUMERIC                             00014960
                   MOVE 'Y'      TO WS-EDIT-SW                          00014970
                   MOVE -1       TO MAP-AREA-CODEL                      00014980
                   MOVE DFHBMBRY TO MAP-AREA-CODEA                      00014990
                   MOVE 'NA032'  TO WS-HOLD-MESSAGE                     00015000
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00015010
                                                                        00015020
           IF COM-PHONE NOT = SPACES AND                                00015030
               COM-PHONE NOT = LOW-VALUES                               00015040
               IF COM-PHONE NOT NUMERIC                                 00015050
                   MOVE 'Y'      TO WS-EDIT-SW                          00015060
                   MOVE -1       TO MAP-PHONE3L                         00015070
                   MOVE DFHBMBRY TO MAP-PHONE3A                         00015080
                   MOVE DFHBMBRY TO MAP-PHONE4A                         00015090
                   MOVE 'NA033'  TO WS-HOLD-MESSAGE                     00015100
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00015110
                                                                        00015120
           IF COM-PHONE-EXTENSION NOT = SPACES AND                      00015130
               COM-PHONE-EXTENSION NOT = LOW-VALUES                     00015140
               IF COM-PHONE-EXTENSION NOT NUMERIC                       00015150
                   MOVE 'Y'      TO WS-EDIT-SW                          00015160
                   MOVE -1       TO MAP-PHONE-EXTL                      00015170
                   MOVE DFHBMBRY TO MAP-PHONE-EXTA                      00015180
                   MOVE 'NA034'  TO WS-HOLD-MESSAGE                     00015190
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00015200
                                                                        00015210
      *---------------------------------------------------------------* 00015220
      * EDIT CUSTOMER FAX PHONE NUMBER FOR VALIDITY                   * 00015230
      *---------------------------------------------------------------* 00015240
           IF COM-FAX-AREA-CODE NOT = SPACES AND                        00015250
               COM-FAX-AREA-CODE NOT = LOW-VALUES                       00015260
               IF COM-FAX-AREA-CODE NOT NUMERIC                         00015270
                   MOVE 'Y'      TO WS-EDIT-SW                          00015280
                   MOVE -1       TO MAP-FAX-AREA-CDL                    00015290
                   MOVE DFHBMBRY TO MAP-FAX-AREA-CDA                    00015300
                   MOVE 'NA186'  TO WS-HOLD-MESSAGE                     00015310
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00015320
                                                                        00015330
           IF COM-FAX-PHONE NOT = SPACES AND                            00015340
               COM-FAX-PHONE NOT = LOW-VALUES                           00015350
               IF COM-FAX-PHONE NOT NUMERIC                             00015360
                   MOVE 'Y'      TO WS-EDIT-SW                          00015370
                   MOVE -1       TO MAP-FAX-PHONE3L                     00015380
                   MOVE DFHBMBRY TO MAP-FAX-PHONE3A                     00015390
                                    MAP-FAX-PHONE4A                     00015400
                   MOVE 'NA187'  TO WS-HOLD-MESSAGE                     00015410
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00015420
                                                                        00015430
      *---------------------------------------------------------------* 00015440
      * EDIT COMPANY IN ADDRESS FIELD FOR VALIDITY                    * 00015450
      *---------------------------------------------------------------* 00015460
      *    IF COM-COMPANY-NAME = SPACES OR COM-COMPANY-NAME = LOW-VALUES00015470
      *    MOVE 'Y' TO COM-COMPANY-IN-ADDRESS.                          00015480
      *    IF COM-COMPANY-IN-ADDRESS  NOT = SPACES AND                  00015490
      *        COM-COMPANY-IN-ADDRESS  NOT = LOW-VALUES                 00015500
           IF  COM-COMPANY-IN-ADDRESS NOT = 'Y' AND                     00015510
               COM-COMPANY-IN-ADDRESS NOT = 'N'                         00015520
               MOVE 'Y'      TO WS-EDIT-SW                              00015530
               MOVE -1       TO MAP-COMP-IN-ADDL                        00015540
               MOVE DFHBMBRY TO MAP-COMP-IN-ADDA                        00015550
               MOVE 'NA035'  TO WS-HOLD-MESSAGE                         00015560
               PERFORM 0160-SLIDE-ERROR-MESSAGES.                       00015570
                                                                        00015580
      *---------------------------------------------------------------* 00015590
      * EDIT ADDRESS OVERRIDE INDICATOR FOR VALIDITY                  * 00015600
      *---------------------------------------------------------------* 00015610
           IF COM-FINALST-OVRD-IND NOT = 'N' AND                        00015620
              COM-FINALST-OVRD-IND NOT = 'Y'                            00015630
                   MOVE 'Y'      TO WS-EDIT-SW                          00015640
                   MOVE -1       TO MAP-ADDR-OVRIDEL                    00015650
                   MOVE DFHBMBRY TO MAP-ADDR-OVRIDEA                    00015660
                   MOVE 'NA036'  TO WS-HOLD-MESSAGE                     00015670
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00015680
                                                                        00015690
      *---------------------------------------------------------------* 00015700
      * EDIT ADDRESS FIELDS WITH FINALST FOR VALIDITY                 * 00015710
      *---------------------------------------------------------------* 00015720
                                                                        00015730
           IF COM-ZIP NOT = SPACES  AND                                 00015740
              COM-ZIP NOT NUMERIC                                       00015750
              MOVE 'NA194'  TO WS-HOLD-MESSAGE                          00015760
              MOVE 'N' TO COM-FINALIST-SW                               00015770
              PERFORM 0160-SLIDE-ERROR-MESSAGES                         00015780
           END-IF                                                       00015790
           IF COM-ZIP EQUAL SPACES OR ZEROS                             00015800
              AND COM-FINALST-OVRD-IND = 'Y'                            00015810
              MOVE 'NA194'  TO WS-HOLD-MESSAGE                          00015820
              MOVE 'N' TO COM-FINALIST-SW                               00015830
              PERFORM 0160-SLIDE-ERROR-MESSAGES                         00015840
           END-IF                                                       00015850
                                                                        00015860
           IF COM-FINALST-OVRD-IND = 'N' AND                            00015870
              COM-ZIP NOT EQUAL '00100'                                 00015880
               PERFORM 0140-FINALST-ADDRESS-CHECK                       00015890
           ELSE                                                         00015900
             IF COM-FINALST-OVRD-IND = 'Y'                              00015910
                 IF COM-FINALIST-SW NOT = 'Y' AND                       00015920
                   COM-ZIP NOT EQUAL '00100'                            00015930
                   MOVE 'Y' TO  COM-FINALIST-SW                         00015940
                   MOVE 'N' TO COM-FINALST-OVRD-IND                     00015950
                   PERFORM 0140-FINALST-ADDRESS-CHECK.                  00015960
                                                                        00015970
           IF WS-FINALST-BYTE1 = '9' AND COM-FINALST-OVRD-IND = 'N'     00015980
                   MOVE 'Y'      TO WS-EDIT-SW                          00015990
                   MOVE -1       TO MAP-ADDRESS1L                       00016000
                   MOVE DFHBMBRY TO MAP-ADDRESS1A                       00016010
                   MOVE DFHBMBRY TO MAP-ADDRESS2A                       00016020
                   MOVE DFHBMBRY TO MAP-CITYA                           00016030
                   MOVE DFHBMBRY TO MAP-STATEA                          00016040
                   MOVE DFHBMBRY TO MAP-ZIPA                            00016050
                   MOVE DFHBMBRY TO MAP-ZIP-PLUS4A                      00016060
                   MOVE 'NA037'  TO WS-HOLD-MESSAGE                     00016070
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00016080
                                                                        00016090
                                                                        00016100
      *---------------------------------------------------------------* 00016110
      * EDIT ADDRESS FIELDS FOR SPACES                                * 00016120
      *---------------------------------------------------------------* 00016130
           IF COM-ADDRESS1 = SPACES AND COM-ADDRESS2 = SPACES           00016140
                   MOVE 'Y'      TO WS-EDIT-SW                          00016150
                   MOVE -1       TO MAP-ADDRESS1L                       00016160
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A           00016170
                   MOVE 'NA182'  TO WS-HOLD-MESSAGE                     00016180
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00016190
                                                                        00016200
      *---------------------------------------------------------------* 00016210
      * EDIT CITY FIELD FOR SPACES                                    * 00016220
      *---------------------------------------------------------------* 00016230
           IF COM-CITY = SPACES                                         00016240
                   MOVE 'Y'      TO WS-EDIT-SW                          00016250
                   MOVE -1       TO MAP-CITYL                           00016260
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-CITYA               00016270
                   MOVE 'NA183'  TO WS-HOLD-MESSAGE                     00016280
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00016290
                                                                        00016300
      *---------------------------------------------------------------* 00016310
      * EDIT STATE FIELD FOR SPACES                                   * 00016320
      *---------------------------------------------------------------* 00016330
           IF COM-STATE = SPACES                                        00016340
                   MOVE 'Y'      TO WS-EDIT-SW                          00016350
                   MOVE -1       TO MAP-STATEL                          00016360
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-STATEA              00016370
                   MOVE 'NA184'  TO WS-HOLD-MESSAGE                     00016380
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00016390
                                                                        00016400
      *---------------------------------------------------------------* 00016410
      * EDIT EFFECTIVE DATE FOR VALIDITY                              * 00016420
      *---------------------------------------------------------------* 00016430
                                                                        00016440
           MOVE COM-EFFECTIVE-DATE TO WS-DATE-R.                        00016450
           MOVE WS-DATE-R          TO WS-DATE.                          00016460
           MOVE WS-MM              TO INPUT-GREGORIAN-MM.               00016470
           MOVE WS-DD              TO INPUT-GREGORIAN-DD.               00016480
           MOVE WS-YY              TO INPUT-GREGORIAN-YY.               00016490
           MOVE 'VG'               TO OPERATION-TO-PERFORM.             00016500
           MOVE 'G'                TO INPUT-DATE-FORMAT-CODE.           00016510
      *    CALL COB2-DATEPRG2 USING WS-DATEPROG-PARAMETERS.             00016520
           MOVE ZEROS              TO WS-DATE WS-DATE-R.                00016530
           IF ERRORS                                                    00016540
                   MOVE 'Y'      TO WS-EDIT-SW                          00016550
                   MOVE -1       TO MAP-EFF-DATEL                       00016560
                   MOVE DFHBMBRY TO MAP-EFF-DATEA                       00016570
                   MOVE 'NA006'  TO WS-HOLD-MESSAGE                     00016580
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00016590
                                                                        00016600
      *---------------------------------------------------------------* 00016610
      * EDIT BIRTH DATE FOR VALIDITY                                  * 00016620
      *---------------------------------------------------------------* 00016630
                                                                        00016640
      *                                                                 00016650
      * IMRGLOBAL CHANGE DATE ROUTINE BEGIN                             00016660
      *                                                                 00016670
      **   IF BIRTH-DATE  EQUAL '1901-01-01' OR  '0001-01-01'           00016680
      **                  OR    '1900-01-01'                            00016690
      **                  OR    '1111-11-11'                            00016700
           IF COM-ENTITY-TYPE NOT = 'BR' AND 'ML'                       00016710
              NEXT SENTENCE                                             00016720
           ELSE                                                         00016730
           IF COM-BIRTH-DATE NOT = ZEROES                               00016740
           AND COM-BIRTH-DATE IS NUMERIC                                00016750
      *            MOVE COM-BIRTH-DATE  TO WS-DATE-R                    00016760
      *            MOVE WS-DATE-R       TO WS-DATE                      00016770
      *            MOVE WS-MM           TO INPUT-GREGORIAN-MM           00016780
      *            MOVE WS-DD           TO INPUT-GREGORIAN-DD           00016790
      *            MOVE WS-YY           TO INPUT-GREGORIAN-YY           00016800
      *            MOVE Y2K-WK-VARIABLE2 TO INPUT-4-GREG-DATE           00016810
      *            MOVE 'VG'            TO OPERATION-TO-PERFORM         00016820
      *                                                                 00016830
                   MOVE COM-BIRTH-DATE TO Y2K-WK-VARIABLE2-N            00016840
                   MOVE Y2K-WK-VARIABLE2 TO INPUT-4-GREG-DATE           00016850
      *            MOVE COM-BIRTH-DATE   TO INPUT-4-GREG-DATE           00016860
                   MOVE 'FG'            TO OPERATION-TO-PERFORM         00016870
                   MOVE 'G'             TO INPUT-DATE-FORMAT-CODE       00016880
      *            CALL COB2-DATEPRG2 USING WS-DATEPROG-PARAMETERS      00016890
                   MOVE ZEROS           TO WS-DATE WS-DATE-R.           00016900
      *                                                                 00016910
      * IMRGLOBAL CHANGE DATE ROUTINE END                               00016920
      *                                                                 00016930
                                                                        00016940
      **   IF BIRTH-DATE  EQUAL '1901-01-01' OR  '0001-01-01'           00016950
      **                  OR    '1900-01-01'                            00016960
      **                  OR    '1111-11-11'                            00016970
           IF COM-ENTITY-TYPE NOT = 'BR' AND 'ML'                       00016980
              NEXT SENTENCE                                             00016990
           ELSE                                                         00017000
           IF COM-BIRTH-DATE NOT = ZEROES                               00017010
           AND COM-BIRTH-DATE IS NUMERIC                                00017020
                   IF ERRORS                                            00017030
                       MOVE 'Y'      TO WS-EDIT-SW                      00017040
                       MOVE -1       TO MAP-BIRTH-DATEL                 00017050
                       MOVE DFHBMBRY TO MAP-BIRTH-DATEA                 00017060
                       MOVE 'NA038'  TO WS-HOLD-MESSAGE                 00017070
                       PERFORM 0160-SLIDE-ERROR-MESSAGES.               00017080
                                                                        00017090
      *---------------------------------------------------------------* 00017100
      * CHECK TO SEE IT DISPLAY NAME IS EMPTY                         * 00017110
      *---------------------------------------------------------------* 00017120
                                                                        00017130
MANJU *    IF COM-DISPLAY-NAME = SPACES                                 00017140
      *        PERFORM 0290-FORMAT-DISPLAY-NAME.                        00017150
                                                                        00017160
      *---------------------------------------------------------------* 00017170
      * EDIT FIRST, MIDDLE, LAST, AND COMPANY NAME FOR ENTRY          * 00017180
      *---------------------------------------------------------------* 00017190
                                                                        00017200
           IF COM-LAST-NAME = SPACES AND COM-FIRST-NAME = SPACES AND    00017210
              COM-MIDDLE-NAME = SPACES AND COM-COMPANY-NAME = SPACES    00017220
                 MOVE 'Y'     TO WS-EDIT-SW                             00017230
                 INSPECT COMM-ACTION-CODE REPLACING ALL '_' BY SPACE    00017240
                 IF (COMM-SYSTEM-CODE = 'AM' AND                        00017250
                 (COMM-ACTION-CODE = 'AA' OR COMM-ACTION-CODE = 'LB' OR 00017260
                  COMM-ACTION-CODE = 'BR'))                             00017270
                 OR (COMM-SYSTEM-CODE = 'NA' AND                        00017280
                 COMM-ACTION-CODE = 'BA')                               00017290
                 OR (COMM-SYSTEM-CODE = 'NA' AND                        00017300
                 COMM-ACTION-CODE = 'LD')                               00017310
                     MOVE -1  TO MAP-COMP-NAMEL                         00017320
                 ELSE                                                   00017330
                     MOVE -1  TO MAP-FIRST-NAMEL                        00017340
                 END-IF                                                 00017350
                 MOVE 'NA027' TO WS-HOLD-MESSAGE                        00017360
                 PERFORM 0160-SLIDE-ERROR-MESSAGES.                     00017370
                                                                        00017380
      *---------------------------------------------------------------* 00017390
      * EDIT LAST NAME SPACES WHEN FIRST AND MIDDLE ENTERED           * 00017400
      *---------------------------------------------------------------* 00017410
                                                                        00017420
           IF COM-FIRST-NAME NOT = SPACES AND                           00017430
               COM-MIDDLE-NAME NOT = SPACES AND COM-LAST-NAME = SPACES  00017440
               OR COM-FIRST-NAME NOT = SPACES AND COM-LAST-NAME = SPACES00017450
               OR COM-MIDDLE-NAME NOT = SPACES AND COM-LAST-NAME = SPACE00017460
                   MOVE 'Y'      TO WS-EDIT-SW                          00017470
                   MOVE -1       TO MAP-LAST-NAMEL                      00017480
                   MOVE DFHBMBRY TO MAP-LAST-NAMEA                      00017490
                   MOVE 'NA161'  TO WS-HOLD-MESSAGE                     00017500
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00017510
                                                                        00017520
      *---------------------------------------------------------------* 00017530
      * EDIT FIRST NAME WHEN LAST NAME ENTERED                        * 00017540
      *---------------------------------------------------------------* 00017550
                                                                        00017560
           IF COM-LAST-NAME NOT = SPACES AND COM-FIRST-NAME = SPACE     00017570
                   MOVE 'Y'      TO WS-EDIT-SW                          00017580
                   MOVE -1       TO MAP-FIRST-NAMEL                     00017590
                   MOVE DFHBMBRY TO MAP-FIRST-NAMEA                     00017600
                   MOVE 'NA161'  TO WS-HOLD-MESSAGE                     00017610
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00017620
                                                                        00017630
      *---------------------------------------------------------------* 00017640
      * DECIDE IF RECORD BELONGS TO A COMPANY OR AGENT                * 00017650
      *---------------------------------------------------------------* 00017660
           DISPLAY 'COMM-SYSTEM-CODE' COMM-SYSTEM-CODE.                 00017670
           DISPLAY 'COMM-ACTION-CODE' COMM-ACTION-CODE.                 00017670
           IF (COM-LAST-NAME = SPACES AND COM-FIRST-NAME = SPACES AND   00017680
               COM-MIDDLE-NAME = SPACES) OR                             00017690
              (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'STA') OR 00017700
              (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'CD')  OR 00017710
MANJU        ((COMM-SYSTEM-CODE = 'NA' AND                              00017710
MANJU          COMM-ACTION-CODE(1:1) = 'A') AND                         00017710
MANJU3         COM-COMPANY-NAME NOT = SPACES)
                   MOVE 'Y' TO COM-COMPANY-IND                          00017720
            DISPLAY 'COM-COMPANY-IND' COM-COMPANY-IND
                   UNSTRING COM-COMPANY-NAME                            00017730
                       DELIMITED BY ALL ' ' OR ','                      00017740
                           INTO COM-NAME-KEY1                           00017750
                                COM-NAME-KEY2                           00017760
                                COM-NAME-KEY3                           00017770
           ELSE                                                         00017780
              MOVE 'N' TO COM-COMPANY-IND                               00017790
              MOVE COM-LAST-NAME   TO COM-NAME-KEY1                     00017800
              MOVE COM-FIRST-NAME  TO COM-NAME-KEY2                     00017810
              MOVE COM-MIDDLE-NAME TO COM-NAME-KEY3.                    00017820
                                                                        00017830
MANJU *    IF NOT (COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'RP') 00017840
MANJU      IF (COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'RP')     00017840
                IF COM-COMPANY-IND NOT = 'Y'                            00017850
                   MOVE 'Y'      TO WS-EDIT-SW                          00017860
                   MOVE -1       TO MAP-COMP-NAMEL                      00017870
                   MOVE DFHBMBRY TO MAP-COMP-NAMEA                      00017880
                   MOVE 'NA188'  TO WS-HOLD-MESSAGE                     00017890
                   PERFORM 0160-SLIDE-ERROR-MESSAGES                    00017900
                END-IF                                                  00017910
           END-IF.                                                      00017920
                                                                        00017930
      *---------------------------------------------------------------* 00017940
      * EDIT CUSTOMER SEX STATUS FOR VALIDITY                         * 00017950
      *---------------------------------------------------------------* 00017960
           DISPLAY 'COM-COMPANY-IND '   COM-COMPANY-IND                 00017970
           IF COM-COMPANY-IND NOT = 'Y'                                 00017980
               IF COM-SEX NOT = 'M' AND COM-SEX NOT = 'F'               00017990
                   MOVE 'Y'      TO WS-EDIT-SW                          00018000
                   MOVE -1       TO MAP-SEXL                            00018010
                   MOVE DFHBMBRY TO MAP-SEXA                            00018020
                   MOVE 'NA154'  TO WS-HOLD-MESSAGE                     00018030
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00018040
                                                                        00018050
           IF COM-COMPANY-IND = 'Y'                                     00018060
               MOVE SPACE TO COM-SEX.                                   00018070
                                                                        00018080
      *---------------------------------------------------------------* 00018090
      * RECORD ORIGIN IS AN ONLINE ADD = 'O'                          * 00018100
      *---------------------------------------------------------------* 00018110
                                                                        00018120
           MOVE 'O' TO COM-RECORD-ORIGIN.                               00018130
                                                                        00018140
      *---------------------------------------------------------------* 00018150
      * EDIT LAST-NAME FOR NUMERICS                                   * 00018160
      *---------------------------------------------------------------* 00018170
                                                                        00018180
           MOVE COM-LAST-NAME TO WS-NUMERIC-CHECK.                      00018190
           PERFORM 0150-NUMERIC-CHECK                                   00018200
               VARYING ACTION-SUB FROM 1 BY 1                           00018210
                   UNTIL ACTION-SUB > 20.                               00018220
                                                                        00018230
           IF WS-NUMERIC-SW = 'Y'                                       00018240
                   MOVE 'Y'      TO WS-EDIT-SW                          00018250
                   MOVE -1       TO MAP-LAST-NAMEL                      00018260
                   MOVE DFHBMBRY TO MAP-LAST-NAMEA                      00018270
                   MOVE 'NA022'  TO WS-HOLD-MESSAGE                     00018280
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00018290
                                                                        00018300
      *---------------------------------------------------------------* 00018310
      * EDIT FIRST NAME FOR NUMERICS                                  * 00018320
      *---------------------------------------------------------------* 00018330
                                                                        00018340
           MOVE 'N'            TO WS-NUMERIC-SW.                        00018350
           MOVE COM-FIRST-NAME TO WS-NUMERIC-CHECK.                     00018360
           PERFORM 0150-NUMERIC-CHECK                                   00018370
               VARYING ACTION-SUB FROM 1 BY 1                           00018380
                   UNTIL ACTION-SUB > 15.                               00018390
                                                                        00018400
           IF WS-NUMERIC-SW = 'Y'                                       00018410
                   MOVE 'Y'      TO WS-EDIT-SW                          00018420
                   MOVE -1       TO MAP-FIRST-NAMEL                     00018430
                   MOVE DFHBMBRY TO MAP-FIRST-NAMEA                     00018440
                   MOVE 'NA040'  TO WS-HOLD-MESSAGE                     00018450
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00018460
                                                                        00018470
      *---------------------------------------------------------------* 00018480
      * EDIT MIDDLE NAME FOR NUMERICS                                 * 00018490
      *---------------------------------------------------------------* 00018500
                                                                        00018510
           MOVE 'N'             TO WS-NUMERIC-SW.                       00018520
           MOVE COM-MIDDLE-NAME TO WS-NUMERIC-CHECK.                    00018530
           PERFORM 0150-NUMERIC-CHECK                                   00018540
               VARYING ACTION-SUB FROM 1 BY 1                           00018550
                   UNTIL ACTION-SUB > 10.                               00018560
                                                                        00018570
           IF WS-NUMERIC-SW = 'Y'                                       00018580
                   MOVE 'Y'      TO WS-EDIT-SW                          00018590
                   MOVE -1       TO MAP-MIDDLE-NAMEL                    00018600
                   MOVE DFHBMBRY TO MAP-MIDDLE-NAMEA                    00018610
                   MOVE 'NA041'  TO WS-HOLD-MESSAGE                     00018620
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00018630
                                                                        00018640
                                                                        00018650
      *---------------------------------------------------------------* 00018660
      * EDIT IDNTITY FOR VALIDITY ON ENTITY TYPE 'RP'                 * 00018670
      *---------------------------------------------------------------* 00018680
                                                                        00018690
           IF COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'RP'       00018700
              SET  WS-IDNTITY-GOOD-SW  TO TRUE                          00018710
              PERFORM 0170-IDNTITY-CHECK                                00018720
                        VARYING ACTION-SUB FROM 1 BY 1                  00018730
                          UNTIL ACTION-SUB > 8                          00018740
              IF WS-IDNTITY-BAD-SW                                      00018750
                   MOVE 'Y'          TO  WS-EDIT-SW                     00018760
                   MOVE -1           TO  MAP-CIML                       00018770
                   MOVE DFHBMBRY     TO  MAP-CIMA                       00018780
                   MOVE 'NA191'      TO  WS-HOLD-MESSAGE                00018790
                   PERFORM 0160-SLIDE-ERROR-MESSAGES                    00018800
              END-IF                                                    00018810
           END-IF.                                                      00018820
                                                                        00018830
      *---------------------------------------------------------------* 00018840
      * EDIT COUNTRY CODE FOR VALIDITY                                * 00018850
      *---------------------------------------------------------------* 00018860
           IF COM-ZIP = '00100'                                         00018870
              EVALUATE TRUE                                             00018880
                 WHEN COM-COUNTRY-CODE = SPACES                         00018890
                   MOVE 'Y'      TO WS-EDIT-SW                          00018900
                   MOVE -1       TO MAP-COUNTRY-CDL                     00018910
                   MOVE DFHBMBRY TO MAP-COUNTRY-CDA                     00018920
                   MOVE 'CM427'  TO WS-HOLD-MESSAGE                     00018930
                   PERFORM 0160-SLIDE-ERROR-MESSAGES                    00018940
                 WHEN COM-COUNTRY-CODE = 'US'                           00018950
                   MOVE 'Y'      TO WS-EDIT-SW                          00018960
                   MOVE -1       TO MAP-COUNTRY-CDL                     00018970
                   MOVE DFHBMBRY TO MAP-COUNTRY-CDA                     00018980
                   MOVE 'CM428'  TO WS-HOLD-MESSAGE                     00018990
                   PERFORM 0160-SLIDE-ERROR-MESSAGES                    00019000
                 WHEN OTHER                                             00019010
                   MOVE COM-COUNTRY-CODE     TO WT-C0525-COUNTRY        00019020
                   MOVE WT-CNTL0525 TO TURBO-CNTL-AREA                  00019030
                   PERFORM 10000-CALL-GSF                               00019040
                   MOVE TURBO-CNTL-AREA TO WT-CNTL0525                  00019050
MANJU              MOVE ZERO TO WT-C0525-RETURN
                   IF  WT-C0525-RETURN = 0                              00019060
                       CONTINUE                                         00019070
                   ELSE                                                 00019080
                       IF WT-C0525-RETURN =  4                          00019090
                           MOVE 'Y'      TO WS-EDIT-SW                  00019100
                           MOVE -1       TO MAP-COUNTRY-CDL             00019110
                           MOVE DFHBMBRY TO MAP-COUNTRY-CDA             00019120
                           MOVE 'CM428'  TO WS-HOLD-MESSAGE             00019130
                           PERFORM 0160-SLIDE-ERROR-MESSAGES            00019140
                       END-IF                                           00019150
                   END-IF                                               00019160
              END-EVALUATE                                              00019170
           ELSE                                                         00019180
              IF COM-ZIP NOT EQUAL '00100'                              00019190
                 EVALUATE TRUE                                          00019200
                    WHEN COM-COUNTRY-CODE = SPACES                      00019210
                       CONTINUE                                         00019220
                    WHEN COM-COUNTRY-CODE NOT EQUAL SPACES              00019230
                       MOVE 'Y'      TO WS-EDIT-SW                      00019240
                       MOVE -1       TO MAP-COUNTRY-CDL                 00019250
                       MOVE DFHBMBRY TO MAP-COUNTRY-CDA                 00019260
                       MOVE 'CM428'  TO WS-HOLD-MESSAGE                 00019270
                       PERFORM 0160-SLIDE-ERROR-MESSAGES                00019280
                    END-EVALUATE                                        00019290
              END-IF                                                    00019300
           END-IF.                                                      00019310
                                                                        00019320
                                                                        00019330
      *---------------------------------------------------------------* 00019340
      * EDIT POSTAL CODE FOR VALIDITY                                 * 00019350
      *---------------------------------------------------------------* 00019360
           IF COM-ZIP = '00100'                                         00019370
             IF COM-COUNTRY-CODE = 'CA'                                 00019380
               EVALUATE TRUE ALSO TRUE                                  00019390
                 WHEN COM-STATE = 'AB' ALSO COM-POSTAL-CODE (1:1) = 'T' 00019400
                   CONTINUE                                             00019410
                 WHEN COM-STATE = 'BC' ALSO COM-POSTAL-CODE (1:1) = 'V' 00019420
                   CONTINUE                                             00019430
                 WHEN COM-STATE = 'LB' ALSO COM-POSTAL-CODE (1:1) = 'A' 00019440
                   CONTINUE                                             00019450
                 WHEN COM-STATE = 'MB' ALSO COM-POSTAL-CODE (1:1) = 'R' 00019460
                   CONTINUE                                             00019470
                 WHEN COM-STATE = 'NB' ALSO COM-POSTAL-CODE (1:1) = 'E' 00019480
                   CONTINUE                                             00019490
                 WHEN COM-STATE = 'NF' ALSO COM-POSTAL-CODE (1:1) = 'A' 00019500
                   CONTINUE                                             00019510
                 WHEN COM-STATE = 'NA' ALSO COM-POSTAL-CODE (1:1) = 'X' 00019520
                   CONTINUE                                             00019530
                 WHEN COM-STATE = 'NS' ALSO COM-POSTAL-CODE (1:1) = 'B' 00019540
                   CONTINUE                                             00019550
                 WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'L' 00019560
                 WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'N' 00019570
                 WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'K' 00019580
                 WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'M' 00019590
                 WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'P' 00019600
                   CONTINUE                                             00019610
                 WHEN COM-STATE = 'PE' ALSO COM-POSTAL-CODE (1:1) = 'C' 00019620
                   CONTINUE                                             00019630
                 WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'H' 00019640
                 WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'J' 00019650
                 WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'G' 00019660
                   CONTINUE                                             00019670
                 WHEN COM-STATE = 'SK' ALSO COM-POSTAL-CODE (1:1) = 'S' 00019680
                   CONTINUE                                             00019690
                 WHEN COM-STATE = 'YT' ALSO COM-POSTAL-CODE (1:1) = 'Y' 00019700
                   CONTINUE                                             00019710
                 WHEN OTHER                                             00019720
                     MOVE 'Y'      TO WS-EDIT-SW                        00019730
                     MOVE -1       TO MAP-POSTAL-CDL                    00019740
                     MOVE DFHBMBRY TO MAP-POSTAL-CDA                    00019750
                     MOVE 'CM120'  TO WS-HOLD-MESSAGE                   00019760
                     PERFORM 0160-SLIDE-ERROR-MESSAGES                  00019770
                 END-EVALUATE                                           00019780
             END-IF                                                     00019790
           ELSE                                                         00019800
             IF COM-POSTAL-CODE = SPACES                                00019810
                CONTINUE                                                00019820
             ELSE                                                       00019830
                MOVE 'Y'      TO WS-EDIT-SW                             00019840
                MOVE -1       TO MAP-POSTAL-CDL                         00019850
                MOVE DFHBMBRY TO MAP-POSTAL-CDA                         00019860
                MOVE 'CM120'  TO WS-HOLD-MESSAGE                        00019870
                PERFORM 0160-SLIDE-ERROR-MESSAGES                       00019880
             END-IF                                                     00019890
           END-IF.                                                      00019900
                                                                        00019910
      *---------------------------------------------------------------* 00019920
      * EDIT STATE CODE FOR VALIDITY                                  * 00019930
      *---------------------------------------------------------------* 00019940
           IF COM-STATE = SPACES OR                                     00019950
              COM-STATE = 'XX'                                          00019960
              EVALUATE TRUE                                             00019970
                 WHEN COM-COUNTRY-CODE = SPACES                         00019980
                 WHEN COM-COUNTRY-CODE = LOW-VALUES                     00019990
                 WHEN COM-COUNTRY-CODE = 'CA'                           00020000
                   MOVE 'Y'      TO WS-EDIT-SW                          00020010
                   MOVE -1       TO MAP-STATEL                          00020020
                   MOVE DFHBMBRY TO MAP-STATEA                          00020030
                   MOVE 'CM016'  TO WS-HOLD-MESSAGE                     00020040
                   PERFORM 0160-SLIDE-ERROR-MESSAGES                    00020050
              END-EVALUATE                                              00020060
           ELSE                                                         00020070
              MOVE COM-STATE   TO WT-C0091-STATE                        00020080
              MOVE WT-CNTL0091 TO TURBO-CNTL-AREA                       00020090
              PERFORM 10000-CALL-GSF                                    00020100
              MOVE TURBO-CNTL-AREA TO WT-CNTL0091                       00020110
           END-IF.                                                      00020120
                                                                        00020130
           EVALUATE WT-C0091-RETURN                                     00020140
               WHEN   +0                                                00020150
                   CONTINUE                                             00020160
               WHEN   +4                                                00020170
                   MOVE 'CM016'  TO WS-HOLD-MESSAGE                     00020180
                   PERFORM 0160-SLIDE-ERROR-MESSAGES                    00020190
           END-EVALUATE.                                                00020200
      *---------------------------------------------------------------* 00020210
      * EDIT ASSOCIATION CODE1 FOR VALIDITY                           * 00020220
      *---------------------------------------------------------------* 00020230
                                                                        00020240
           IF COM-ASSOCIATION1 NOT = SPACES                             00020250
               MOVE COM-ASSOCIATION1   TO WT-C0043-ASSOC-CODE-KEY       00020260
               MOVE WT-CNTL0043        TO TURBO-CNTL-AREA               00020270
               PERFORM 10000-CALL-GSF                                   00020280
               MOVE TURBO-CNTL-AREA    TO WT-CNTL0043                   00020290
MANJU          MOVE ZERO TO  WT-C0043-RETURN
               IF  WT-C0043-RETURN NOT = ZERO                           00020300
                   MOVE 'Y'      TO WS-EDIT-SW                          00020310
                   MOVE -1       TO MAP-ASSOC1L                         00020320
                   MOVE DFHBMBRY TO MAP-ASSOC1A                         00020330
                   MOVE 'NA155'  TO WS-HOLD-MESSAGE                     00020340
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00020350
                                                                        00020360
           IF COM-ASSOCIATION1 NOT = SPACES                             00020370
               MOVE COM-CHANGE-DATE TO WS-DATE-R                        00020380
               MOVE WS-DATE-R       TO WS-DATE                          00020390
               MOVE WS-YY           TO WS-COMPARE-YY                    00020400
               MOVE WS-MM           TO WS-COMPARE-MM                    00020410
               MOVE WS-DD           TO WS-COMPARE-DD.                   00020420
                                                                        00020430
      *                                                                 00020440
      * IMR CHANGE IMR1 BEGIN                                           00020450
      *                                                                 00020460
      *    IF COM-ASSOCIATION1 NOT = SPACES                             00020470
      *        IF  WT-C0043-RETURN  = ZERO AND                          00020480
      *             WS-COMPARE-DATE > WT-C0043-ASSOC-TERM-DATE          00020490
                                                                        00020500
                   COPY IMRG9DT1                                        00020510
                   REPLACING DATE-9 BY WS-COMPARE-DATE.                 00020520
                                                                        00020530
                   COPY IMRG9DT2                                        00020540
                   REPLACING DATE-9 BY WT-C0043-ASSOC-TERM-DATE.        00020550
                                                                        00020560
           IF COM-ASSOCIATION1 NOT = SPACES                             00020570
               IF  WT-C0043-RETURN  = ZERO AND                          00020580
                    Y2K-WK-DATE1-9 > Y2K-WK-DATE2-9                     00020590
      *                                                                 00020600
      * IMR CHANGE IMR1 END                                             00020610
      *                                                                 00020620
                    MOVE 'Y'      TO WS-EDIT-SW                         00020630
                    MOVE -1       TO MAP-ASSOC1L                        00020640
                    MOVE DFHBMBRY TO MAP-ASSOC1A                        00020650
                    MOVE 'NA158'  TO WS-HOLD-MESSAGE                    00020660
                    PERFORM 0160-SLIDE-ERROR-MESSAGES.                  00020670
                                                                        00020680
      *---------------------------------------------------------------* 00020690
      * EDIT ASSOCIATION CODE2 FOR VALIDITY                           * 00020700
      *---------------------------------------------------------------* 00020710
           IF COM-ASSOCIATION2 NOT = SPACES                             00020720
               MOVE COM-ASSOCIATION2   TO WT-C0043-ASSOC-CODE-KEY       00020730
               MOVE WT-CNTL0043        TO TURBO-CNTL-AREA               00020740
               PERFORM 10000-CALL-GSF                                   00020750
               MOVE TURBO-CNTL-AREA    TO WT-CNTL0043                   00020760
               IF  WT-C0043-RETURN NOT = ZERO                           00020770
                   MOVE 'Y'      TO WS-EDIT-SW                          00020780
                   MOVE -1       TO MAP-ASSOC2L                         00020790
                   MOVE DFHBMBRY TO MAP-ASSOC2A                         00020800
                   MOVE 'NA156'  TO WS-HOLD-MESSAGE                     00020810
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00020820
                                                                        00020830
           IF COM-ASSOCIATION2 NOT = SPACES                             00020840
               MOVE COM-CHANGE-DATE TO WS-DATE-R                        00020850
               MOVE WS-DATE-R       TO WS-DATE                          00020860
               MOVE WS-YY           TO WS-COMPARE-YY                    00020870
               MOVE WS-MM           TO WS-COMPARE-MM                    00020880
               MOVE WS-DD           TO WS-COMPARE-DD.                   00020890
                                                                        00020900
      *                                                                 00020910
      * IMR CHANGE IMR2 BEGIN                                           00020920
      *                                                                 00020930
      *    IF COM-ASSOCIATION2 NOT = SPACES                             00020940
      *        IF  WT-C0043-RETURN  = ZERO AND                          00020950
      *            WS-COMPARE-DATE > WT-C0043-ASSOC-TERM-DATE           00020960
                                                                        00020970
                   COPY IMRG9DT1                                        00020980
                   REPLACING DATE-9 BY WS-COMPARE-DATE.                 00020990
                                                                        00021000
                   COPY IMRG9DT2                                        00021010
                   REPLACING DATE-9 BY WT-C0043-ASSOC-TERM-DATE.        00021020
                                                                        00021030
           IF COM-ASSOCIATION1 NOT = SPACES                             00021040
               IF  WT-C0043-RETURN  = ZERO AND                          00021050
                    Y2K-WK-DATE1-9 > Y2K-WK-DATE2-9                     00021060
      *                                                                 00021070
      * IMR CHANGE IMR2 END                                             00021080
      *                                                                 00021090
                   MOVE 'Y'      TO WS-EDIT-SW                          00021100
                   MOVE -1       TO MAP-ASSOC2L                         00021110
                   MOVE DFHBMBRY TO MAP-ASSOC2A                         00021120
                   MOVE 'NA158'  TO WS-HOLD-MESSAGE                     00021130
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00021140
                                                                        00021150
      *---------------------------------------------------------------* 00021160
      * EDIT ASSOCIATION CODE3 FOR VALIDITY                           * 00021170
      *---------------------------------------------------------------* 00021180
           IF COM-ASSOCIATION3 NOT = SPACES                             00021190
               MOVE COM-ASSOCIATION3   TO WT-C0043-ASSOC-CODE-KEY       00021200
               MOVE WT-CNTL0043        TO TURBO-CNTL-AREA               00021210
               PERFORM 10000-CALL-GSF                                   00021220
               MOVE TURBO-CNTL-AREA    TO WT-CNTL0043                   00021230
               IF  WT-C0043-RETURN NOT = ZERO                           00021240
                   MOVE 'Y'      TO WS-EDIT-SW                          00021250
                   MOVE -1       TO MAP-ASSOC3L                         00021260
                   MOVE DFHBMBRY TO MAP-ASSOC3A                         00021270
                   MOVE 'NA157'  TO WS-HOLD-MESSAGE                     00021280
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00021290
                                                                        00021300
           IF COM-ASSOCIATION3 NOT = SPACES                             00021310
               MOVE COM-CHANGE-DATE TO WS-DATE-R                        00021320
               MOVE WS-DATE-R       TO WS-DATE                          00021330
               MOVE WS-YY           TO WS-COMPARE-YY                    00021340
               MOVE WS-MM           TO WS-COMPARE-MM                    00021350
               MOVE WS-DD           TO WS-COMPARE-DD.                   00021360
                                                                        00021370
      *                                                                 00021380
      * IMR CHANGE IMR3 BEGIN                                           00021390
      *                                                                 00021400
      *    IF COM-ASSOCIATION3 NOT = SPACES                             00021410
      *        IF  WT-C0043-RETURN  = ZERO AND                          00021420
      *            WS-COMPARE-DATE > WT-C0043-ASSOC-TERM-DATE           00021430
                                                                        00021440
                   COPY IMRG9DT1                                        00021450
                   REPLACING DATE-9 BY WS-COMPARE-DATE.                 00021460
                                                                        00021470
                   COPY IMRG9DT2                                        00021480
                   REPLACING DATE-9 BY WT-C0043-ASSOC-TERM-DATE.        00021490
                                                                        00021500
           IF COM-ASSOCIATION1 NOT = SPACES                             00021510
               IF  WT-C0043-RETURN  = ZERO AND                          00021520
                    Y2K-WK-DATE1-9 > Y2K-WK-DATE2-9                     00021530
      *                                                                 00021540
      * IMR CHANGE IMR3 END                                             00021550
      *                                                                 00021560
                   MOVE 'Y'      TO WS-EDIT-SW                          00021570
                   MOVE -1       TO MAP-ASSOC3L                         00021580
                   MOVE DFHBMBRY TO MAP-ASSOC3A                         00021590
                   MOVE 'NA158'  TO WS-HOLD-MESSAGE                     00021600
                   PERFORM 0160-SLIDE-ERROR-MESSAGES.                   00021610
                                                                        00021620
                                                                        00021630
           IF WS-EDIT-SW NOT = 'Y'                                      00021640
              IF COM-ENTITY-LITERAL = 'AM' OR 'CAM'                     00021650
                  OR 'LB' OR 'LIST BILL'                                00021660
                  OR 'BA' OR 'BILL ADDR'                                00021670
                  OR 'LD' OR 'LEAD'                                     00021680
                  PERFORM 0210-ADD-CASENAME-ROW                         00021690
              ELSE                                                      00021700
              IF COM-ENTITY-LITERAL = 'BR' OR 'ML' OR 'RP' OR           00021710
                                      'BROKER' OR 'MAIL' OR 'NOI'       00021720
                  PERFORM 0200-ADD-AGNTNAME-ROW.                        00021730
                                                                        00021740
                                                                        00021750
       0140-FINALST-ADDRESS-CHECK.                                      00021760
                                                                        00021770
           MOVE COM-ADDRESS1 TO NA-COMM-ADDRESS-1.                      00021780
           MOVE COM-ADDRESS2 TO NA-COMM-ADDRESS-2.                      00021790
           MOVE COM-CITY     TO NA-COMM-CITY.                           00021800
           MOVE COM-STATE    TO NA-COMM-STATE.                          00021810
           MOVE COM-ZIP      TO NA-COMM-ZIP-CODE.                       00021820
                                                                        00021830
      *    EXEC CICS ASSIGN APPLID(WS-APPLID) END-EXEC.                 00021840
                                                                        00021850
      *    IF WS-APPLID = 'CICSHADT' OR 'CICSHADP' OR 'CICSHADU'        00021860
      *                   OR 'CICSHADF'                                 00021870
      *                   OR 'CICSHADL' OR 'CICSADHD' OR 'CICSHUAE'     00021870
      *                   OR 'CICSADHQ' OR 'CICSADHS' OR 'CICSADHU'     00021870
      *                   OR 'CICSADHT' OR 'CICSADHB' OR 'CICSHADY'     00021870
      *        EXEC CICS LINK                                           00021880
      *            PROGRAM  ('CADDR')                                   00021890
      *            COMMAREA (NAME-AND-ADDRESS-COMMAREA)                 00021900
      *            LENGTH   (LENGTH OF NAME-AND-ADDRESS-COMMAREA)       00021910
      *            RESP     (WS-CICS-RESP)                              00021920
      *        END-EXEC                                                 00021930
      *    ELSE                                                         00021940
      *        EXEC CICS LINK                                           00021950
      *            PROGRAM  ('NA205')                                   00021960
      *            COMMAREA (NAME-AND-ADDRESS-COMMAREA)                 00021970
      *            LENGTH   (WS-FIN-LENGTH)                             00021980
      *            LENGTH   (LENGTH OF NAME-AND-ADDRESS-COMMAREA)       00021990
      *            RESP     (WS-CICS-RESP)                              00022000
      *        END-EXEC.                                                00022010
                                                                        00022020
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00022030
      *         GO TO 9999-CICS-ERROR.                                  00022040
                                                                        00022050
           MOVE NA-COMM-FINALIST-REASON TO COM-FINALST-REAS-CODE.       00022060
           MOVE NA-COMM-FINALIST-REASON TO WS-FINALST-REAS-CODE.        00022070
                                                                        00022080
      *    IF WS-FINALST-BYTE1 NOT = '9'                                00022090
      *       IF NA-COMM-ADDRESS-1 = COM-ADDRESS1 AND                   00022100
      *          NA-COMM-ADDRESS-2 = COM-ADDRESS2                       00022110
      *            NEXT SENTENCE                                        00022120
      *       ELSE                                                      00022130
      *         MOVE NA-COMM-ADDRESS-1 TO WS-ADDRESS1                   00022140
      *         MOVE NA-COMM-ADDRESS-2 TO WS-ADDRESS2                   00022150
      *         IF WS-ADDRESS1-REMAINDER > SPACES OR                    00022160
      *            WS-ADDRESS2-REMAINDER > SPACES                       00022170
      *** THE SOFTWARE HAS DEFAULTED/REFORMATTED AN ADDRESS LINE AND    00022180
      *** IT EXCEEDS 22 BYTES SO WARN THE USER                          00022190
      *              MOVE 'Y'      TO WS-EDIT-SW                        00022200
      *                               COM-FINALIST-SW                   00022210
      *              MOVE -1       TO MAP-ADDRESS1L                     00022220
      *              MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A         00022230
      *              MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS2A         00022240
      *              MOVE 'NA047'  TO WS-HOLD-MESSAGE                   00022250
      *              PERFORM 0160-SLIDE-ERROR-MESSAGES                  00022260
      *              MOVE 'NA048'  TO WS-HOLD-MESSAGE                   00022270
      *              PERFORM 0160-SLIDE-ERROR-MESSAGES.                 00022280
                                                                        00022290
           IF WS-FINALST-BYTE1 = '9'                                    00272840
              MOVE NA-COMM-ZIP        TO WS-SCZ-ZIP                     00272841
              PERFORM 0145-STATE-COUNTY-ZIP THRU 0145-EXIT              00272850
           END-IF.                                                      00272870
           IF WS-FINALST-BYTE1 NOT = '9'                                00022300
              IF (NA-COMM-ADDRESS-1 = COM-ADDRESS1 AND                  00022310
                 NA-COMM-ADDRESS-2 = COM-ADDRESS2)                      00022320
               MOVE NA-COMM-ADDRESS-1 TO COM-ADDRESS1                   00022330
               MOVE NA-COMM-ADDRESS-2 TO COM-ADDRESS2                   00022340
               MOVE NA-COMM-CITY      TO COM-CITY                       00022350
               MOVE NA-COMM-STATE     TO COM-STATE                      00022360
               MOVE NA-COMM-ZIP-CODE  TO COM-ZIP                        00022370
               MOVE NA-COMM-ZIP-PLUS4 TO COM-ZIP-PLUS4                  00022380
               MOVE NA-COMM-COUNTY-CODE TO COM-COUNTY-CODE              00022390
              ELSE                                                      00022400
                MOVE NA-COMM-ADDRESS-1 TO WS-ADDRESS1                   00022410
                MOVE NA-COMM-ADDRESS-2 TO WS-ADDRESS2                   00022420
                IF WS-ADDRESS1-REMAINDER > SPACES OR                    00022430
                   WS-ADDRESS2-REMAINDER > SPACES                       00022440
                   MOVE COM-ADDRESS1  TO NA-COMM-ADDRESS-1              00022450
                   MOVE COM-ADDRESS2  TO NA-COMM-ADDRESS-2              00022460
      *** RETURNED ADDRESS FROM THE DATABASE THAT WAS TRUNCATED         00022470
      *** THE SOFTWARE HAS DEFAULTED/REFORMATTED AN ADDRESS LINE AND    00022480
      *** IT EXCEEDS 22 BYTES SO WARN THE USER                          00022490
                     MOVE 'Y'      TO WS-EDIT-SW                        00022500
                                      COM-FINALIST-SW                   00022510
                     MOVE -1       TO MAP-ADDRESS1L                     00022520
                     MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A         00022530
                     MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS2A         00022540
                     MOVE 'NA047'  TO WS-HOLD-MESSAGE                   00022550
                     PERFORM 0160-SLIDE-ERROR-MESSAGES                  00022560
                     MOVE 'NA048'  TO WS-HOLD-MESSAGE                   00022570
                     PERFORM 0160-SLIDE-ERROR-MESSAGES                  00022580
               ELSE                                                     00022590
      **** NO TRUNCATION                                                00022600
                     MOVE NA-COMM-ADDRESS-1 TO COM-ADDRESS1             00022610
                     MOVE NA-COMM-ADDRESS-2 TO COM-ADDRESS2.            00022620
                                                                        00022630
           IF WS-FINALST-BYTE1 NOT = '9'                                00022640
      *        MOVE NA-COMM-ADDRESS-1 TO COM-ADDRESS1                   00022650
      *        MOVE NA-COMM-ADDRESS-2 TO COM-ADDRESS2                   00022660
               MOVE NA-COMM-CITY      TO COM-CITY                       00022670
               MOVE NA-COMM-STATE     TO COM-STATE                      00022680
               MOVE NA-COMM-ZIP-CODE  TO COM-ZIP                        00022690
               MOVE NA-COMM-ZIP-PLUS4 TO COM-ZIP-PLUS4                  00022700
               MOVE NA-COMM-COUNTY-CODE TO COM-COUNTY-CODE.             00022710

      ****************************************************
      * START OF COUNTY CODE UPDATE FROM STATE_COUNTY_ZIP*
      ****************************************************
       0145-STATE-COUNTY-ZIP.

           EXEC SQL
              SELECT STATE_NUMBER||COUNTY_NUMBER AS COUNTY_CODE
                INTO :WS-SCZ-COUNTY-CODE
                FROM STATE_COUNTY_ZIP
               WHERE STATE_CD    = :NA-COMM-STATE
               AND ZIP_CODE      = :WS-SCZ-ZIP-C
               FETCH FIRST ROW ONLY
           END-EXEC.

           EVALUATE SQLCODE
              WHEN  0
                 MOVE WS-SCZ-COUNTY-CODE-N TO NA-COMM-COUNTY-CODE       00284492
                 MOVE SPACES TO NA-COMM-FINALIST-REASON                 00284493
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 GO TO 0410-DB2-ERROR                                   00284497
           END-EVALUATE.
                 MOVE NA-COMM-FINALIST-REASON TO WS-FINALST-REAS-CODE.  00284500
                                                                        00284501
       0145-EXIT.                                                       00284502
            EXIT.                                                       00284503

       0150-NUMERIC-CHECK.                                              00022730
                                                                        00022740
           IF WS-NUMERIC-CHECK-BYTE (ACTION-SUB) NUMERIC                00022750
               MOVE 'Y' TO WS-NUMERIC-SW.                               00022760
                                                                        00022770
       0160-SLIDE-ERROR-MESSAGES.                                       00022780
                                                                        00022790
           IF WS-MESSAGE-NUMBER1 = SPACES OR LOW-VALUES                 00022800
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER1               00022810
           ELSE                                                         00022820
           IF WS-MESSAGE-NUMBER2 = SPACES OR LOW-VALUES                 00022830
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER2               00022840
           ELSE                                                         00022850
           IF WS-MESSAGE-NUMBER3 = SPACES OR LOW-VALUES                 00022860
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER3               00022870
           ELSE                                                         00022880
           IF WS-MESSAGE-NUMBER4 = SPACES OR LOW-VALUES                 00022890
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER4.              00022900
                                                                        00022910
                                                                        00022920
       0170-IDNTITY-CHECK.                                              00022930
                                                                        00022940
           IF WS-IDNTITY-CHECK-BYTE (ACTION-SUB) = SPACE                00022950
              OR (WS-IDNTITY-CHECK-BYTE (7) < 'A' AND                   00022960
                  WS-IDNTITY-CHECK-BYTE (8) < 'A')                      00022970
              OR (WS-IDNTITY-CHECK-BYTE (7) > '9' AND                   00022980
                  WS-IDNTITY-CHECK-BYTE (8) > '9')                      00022990
               SET WS-IDNTITY-BAD-SW   TO  TRUE                         00023000
           END-IF.                                                      00023010
                                                                        00023020
                                                                        00023030
       0180-SEND-NA330M1-FIRST-TIME.                                    00023040
                                                                        00023050
           INSPECT COMM-ACTION-CODE REPLACING ALL '_' BY SPACE.         00023060
      *    IF COMM-ACTION-CODE = 'STA  '                                00023070
      *       PERFORM 0405-CHK-VALID-CUSTOMER THRU 0405-EXIT.           00023080
                                                                        00023090
           PERFORM 0380-CHECK-ERROR-MESSAGES                            00023100
               VARYING ACTION-SUB FROM 1 BY 1                           00023110
                   UNTIL ACTION-SUB > 4.                                00023120
                                                                        00023130
           MOVE SPACES       TO COMM-MSG-MAX-SEVERITY.                  00023140
           IF COMM-ACTION-CODE = 'STA  '                                00023150
              MOVE SPACES       TO MAP-FIRST-NAMEO                      00023160
              MOVE SPACES       TO MAP-MIDDLE-NAMEO                     00023170
              MOVE SPACES       TO MAP-LAST-NAMEO                       00023180
              MOVE -1           TO MAP-FIRST-NAMEL                      00023190
           ELSE                                                         00023200
              MOVE -1           TO MAP-SYSTEM-CDL.                      00023210
                                                                        00023220
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00023230
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00023240
               MOVE SPACES  TO COMM-SYSTEM-CODE                         00023250
           ELSE                                                         00023260
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00023270
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00023280
               MOVE ALL '_' TO MAP-ACTION-CDO                           00023290
               MOVE SPACES  TO COMM-ACTION-CODE                         00023300
           ELSE                                                         00023310
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00023320
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00023330
               MOVE ALL '_' TO MAP-CUST-INFOO                           00023340
               MOVE SPACES  TO COMM-CUSTOMER-INFO                       00023350
           ELSE                                                         00023360
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00023370
                                                                        00023380
           PERFORM 0270-MOVE-SPACES-TO-MAP.                             00023390
                                                                        00023400
           IF COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'RP'       00023410
              MOVE ATTRB-UNPROT TO MAP-CIMA                             00023420
              MOVE 'REP'        TO MAP-CIMO                             00023430
           END-IF.                                                      00023440
                                                                        00023450
           EXEC CICS SEND MAP    ('NA330M1')                            00023460
                          CURSOR                                        00023470
                          ERASE                                         00023480
                          RESP (WS-CICS-RESP)                           00023490
                          END-EXEC.                                     00023500
                                                                        00023510
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00023520
                GO TO 9999-CICS-ERROR.                                  00023530
                                                                        00023540
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00023550
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00023560
                                                                        00023570
           MOVE '01'               TO COMM-NEXT-FUNCTION.               00023580
           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                  00023590
           MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.           00023600
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00023610
                               FROM   (WSQ-COMMAREA)                    00023620
                               LENGTH (WS-COMM-DB2-LENGTH)              00023630
                               ITEM   (WS-TS-ITEM)                      00023640
                               RESP   (WS-CICS-RESP)                    00023650
                               REWRITE                                  00023660
                               MAIN                                     00023670
                               END-EXEC.                                00023680
                                                                        00023690
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00023700
                GO TO 9999-CICS-ERROR.                                  00023710
                                                                        00023720
           EXEC CICS RETURN TRANSID ('NAA0')                            00023730
                            END-EXEC.                                   00023740
                                                                        00023750
       0190-SEND-NA330M1-MAP.                                           00023760
                                                                        00023770
           MOVE LOW-VALUES TO NA330M1O.                                 00023780
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
           EXEC CICS SEND MAP    ('NA330M1')                            00024150
                          CURSOR                                        00024160
                          ERASE                                         00024170
                          RESP (WS-CICS-RESP)                           00024180
                          END-EXEC.                                     00024190
                                                                        00024200
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00024210
                GO TO 9999-CICS-ERROR.                                  00024220
                                                                        00024230
                                                                        00024240
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00024250
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00024260
                                                                        00024270
           MOVE '01'               TO COMM-NEXT-FUNCTION.               00024280
           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                  00024290
           MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                     00024300
           MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.           00024310
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00024320
                               FROM   (WSQ-COMMAREA)                    00024330
                               LENGTH (WS-COMM-DB2-LENGTH)              00024340
                               ITEM   (WS-TS-ITEM)                      00024350
                               RESP   (WS-CICS-RESP)                    00024360
                               REWRITE                                  00024370
                               MAIN                                     00024380
                               END-EXEC.                                00024390
                                                                        00024400
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00024410
                GO TO 9999-CICS-ERROR.                                  00024420
                                                                        00024430
           EXEC CICS RETURN TRANSID ('NAA0')                            00024440
                            END-EXEC.                                   00024450
                                                                        00024460
                                                                        00024470
       0200-ADD-AGNTNAME-ROW.                                           00024480

           DISPLAY '0200-ADD-AGNTNAME-ROW'
                                                                        00024490
           IF NOT (COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'RP') 00024500
               MOVE ZEROS TO WS-TABLE-RETRY-CNT                         00024510
               EXEC SQL                                                 00024520
                   LOCK TABLE NEXT_IDNTITY IN EXCLUSIVE MODE            00024530
               END-EXEC                                                 00024540
               IF SQLCODE = (-904 OR -911 OR -913) AND                  00024550
                   WS-TABLE-RETRY-CNT < 10                              00024560
                  ADD 1 TO WS-TABLE-RETRY-CNT                           00024570
                  GO TO 0200-ADD-AGNTNAME-ROW                           00024580
               END-IF                                                   00024590
               DISPLAY 'SQLCODE' SQLCODE
               IF SQLCODE NOT = ZERO                                    00024600
                  MOVE 'DB007'       TO WS-MESSAGE-NUMBER1              00024610
                  MOVE DFHBMASB TO MAP-CIMA                             00024620
                  MOVE SQLCODE TO WS-DISPLAY-SQLCODE-N                  00024630
                  EXEC CICS SYNCPOINT ROLLBACK                          00024640
                                END-EXEC                                00024650
                  GO TO 0190-SEND-NA330M1-MAP                           00024660
               END-IF                                                   00024670
               EXEC SQL                                                 00024680
                   SELECT NEXT_IDNTITY_NUM                              00024690
                     INTO :HOLD-NEXT-ID-R                               00024700
                   FROM NEXT_IDNTITY                                    00024710
               END-EXEC                                                 00024720
               DISPLAY 'SQLCODE' SQLCODE
               ADD 1 TO HOLD-NEXT-ID                                    00024730
               MOVE HOLD-NEXT-ID-R    TO WS-IDNTITY-NUM                 00024740
           ELSE                                                         00024750
               MOVE MAP-CIMI           TO WS-IDNTITY-NUM                00024760
                                          WS-OFF-PRODUCERID             00024770
           END-IF.                                                      00024780

           DISPLAY 'WS-IDNTITY-NUM:' WS-IDNTITY-NUM
                                                                        00024790
           EXEC SQL                                                     00024800
               OPEN AGNTCUR                                             00024810
           END-EXEC.                                                    00024820
                                                                        00024830
               DISPLAY 'SQLCODE' SQLCODE
           IF SQLCODE NOT = ZERO                                        00024840
              EXEC CICS SYNCPOINT ROLLBACK                              00024850
                            END-EXEC                                    00024860
              GO TO 0190-SEND-NA330M1-MAP                               00024870
           END-IF.                                                      00024880
                                                                        00024890
      *  *************************************************************  00024900
      *  * REPLACED GENERIC AGNTNAME FETCH WITH SPECIFIC COLUMN NAMES*  00024910
      *  *************************************************************  00024920
      *                                                                 00024930
           EXEC SQL FETCH AGNTCUR                                       00024940
      *        INTO :DCLAGNTNAME:AGNTNAME-INDICATORS                    00024950
               INTO :IDNTITY           :IND-IDNTITY                     00024960
                  , :LAST-NAME                                          00024970
                  , :FIRST-NAME                                         00024980
                  , :MIDDLE-NAME                                        00024990
                  , :PREFIX                                             00025000
                  , :SUFFIX1                                            00025010
                  , :SUFFIX2                                            00025020
                  , :COMPANY-IND       :IND-COMPANY-IND                 00025030
                  , :COMPANY-IN-ADDRESS:IND-COMPANY-IN-ADDRESS          00025040
                  , :COMPANY-NAME                                       00025050
                  , :DISPLAY-NAME                                       00025060
                  , :NICKNAME                                           00025070
                  , :ADDRESS1          :IND-ADDRESS1                    00025080
                  , :ADDRESS2                                           00025090
                  , :CITY              :IND-CITY                        00025100
                  , :STATE             :IND-STATE                       00025110
                  , :ZIP               :IND-ZIP                         00025120
                  , :ZIP-PLUS4         :IND-ZIP-PLUS4                   00025130
                  , :COUNTY-CODE       :IND-COUNTY-CODE                 00025140
                  , :AREA-CODE         :IND-AREA-CODE                   00025150
                  , :PHONE             :IND-PHONE                       00025160
                  , :PHONE-EXTENSION   :IND-PHONE-EXTENSION             00025170
                  , :SSN-NUM           :IND-SSN                         00025180
                  , :SEX               :IND-SEX                         00025190
                  , :BIRTH-DATE                                         00025200
                  , :FINALST-REAS-CODE :IND-FINALST-REAS-CODE           00025210
                  , :FINALST-OVRD-IND  :IND-FINALST-OVRD-IND            00025220
                  , :DUP-ADDR-OVRD-IND :IND-DUP-ADDR-OVRD-IND           00025230
                  , :EFFECTIVE-DATE    :IND-EFFECTIVE-DATE              00025240
                  , :CHANGE-DATE       :IND-CHANGE-DATE                 00025250
                  , :CHANGE-LOGON      :IND-CHANGE-LOGON                00025260
                  , :ENTITY-TYPE       :IND-ENTITY-TYPE                 00025270
                  , :RECORD-STATUS     :IND-RECORD-STATUS               00025280
                  , :ALT-ADDRESS-IND   :IND-ALT-ADDRESS-IND             00025290
                  , :FUTURE-ADDRESS-IND:IND-FUTURE-ADDRESS-IND          00025300
                  , :RECORD-ORIGIN     :IND-RECORD-ORIGIN               00025310
                  , :COMBINED-STATUS   :IND-COMBINED-STATUS             00025320
                  , :SITE-CODE         :IND-SITE-CODE                   00025330
                  , :NAME-KEY1         :IND-NAME-KEY1                   00025340
                  , :NAME-KEY2         :IND-NAME-KEY2                   00025350
                  , :NAME-KEY3         :IND-NAME-KEY3                   00025360
                  , :ADDRESS-KEY1      :IND-ADDRESS-KEY1                00025370
                  , :ASSOCIATION1                                       00025380
                  , :ASSOCIATION2                                       00025390
                  , :ASSOCIATION3                                       00025400
                  , :FAX-AREA-CODE     :IND-FAX-AREA-CODE               00025410
                  , :FAX-PHONE         :IND-FAX-PHONE                   00025420
                  , :OFF-PRODUCERID    :IND-OFF-PRODUCERID              00025430
                  , :EXTN-AGENCY-NUMBER                                 00025440
                  , :PRIMRY-AFFL-CODE                                   00025450
                  , :SERVFEE-AGREE-IND                                  00025460
                  , :SERVFEE-AGREE-DATE                                 00025470
                  , :EMAIL1                                             00025480
                  , :EMAIL2                                             00025490
                  , :PASS-WORD                                          00025500
                  , :CELLULAR-AREA-CODE                                 00025510
                  , :CELLULAR-PHONE                                     00025520
                  , :INTEREST-CD                                        00025530
                  , :PRIMRY-BUSINESS-CD                                 00025540
                  , :PRIMRY-BUS-REVS-DT                                 00025550
                  , :SELLING-SM-GRP-YR                                  00025560
                  , :IN-INS-SINCE-YEAR                                  00025570
                  , :COUNTRY                                            00025580
                  , :FOREIGN-FLG                                        00025590
                  , :RANKING-CD                                         00025600
                  , :DO-NO-UPDATE                                       00025610
                  , :TIN                                                00025620
                  , :DOING-BUS-AS-NAME                                  00025630
                  , :CERTIFIED-IND                                      00025640
                  , :PAYEE-IND                                          00025680
           END-EXEC.                                                    00025690
               DISPLAY 'SQLCODE' SQLCODE
           IF SQLCODE = ZERO                                            00025710
               MOVE 'NA152'           TO WS-MESSAGE-NUMBER1             00025720
               MOVE 'Y' TO WS-DUPLICATE-SW                              00025730
               MOVE DFHBMASB TO MAP-CIMA                                00025740
               EXEC CICS SYNCPOINT ROLLBACK                             00025750
                                END-EXEC                                00025760
               EXEC SQL                                                 00025770
                   CLOSE AGNTCUR                                        00025780
               END-EXEC                                                 00025790
               GO TO 0190-SEND-NA330M1-MAP                              00025800
           ELSE                                                         00025810
              IF SQLCODE NOT = ZERO AND SQLCODE NOT = 100               00025820
                 EXEC CICS SYNCPOINT ROLLBACK                           00025830
                                  END-EXEC                              00025840
                 GO TO 0410-DB2-ERROR.                                  00025850
                                                                        00025860
           IF IND-LAST-NAME IS NEGATIVE                                 00025870
               MOVE SPACES TO LAST-NAME.                                00025880
           IF IND-FIRST-NAME IS NEGATIVE                                00025890
               MOVE SPACES TO FIRST-NAME.                               00025900
           IF IND-MIDDLE-NAME IS NEGATIVE                               00025910
               MOVE SPACES TO MIDDLE-NAME.                              00025920
           IF IND-PREFIX IS NEGATIVE                                    00025930
               MOVE SPACES TO PREFIX.                                   00025940
           IF IND-SUFFIX1 IS NEGATIVE                                   00025950
               MOVE SPACES TO SUFFIX1.                                  00025960
           IF IND-SUFFIX2 IS NEGATIVE                                   00025970
               MOVE SPACES TO SUFFIX2.                                  00025980
           IF IND-COMPANY-NAME IS NEGATIVE                              00025990
               MOVE SPACES TO COMPANY-NAME.                             00026000
           IF IND-BIRTH-DATE IS NEGATIVE                                00026010
               MOVE SPACES TO BIRTH-DATE.                               00026020
           IF IND-DISPLAY-NAME IS NEGATIVE                              00026030
               MOVE SPACES TO DISPLAY-NAME.                             00026040
           IF IND-NICKNAME IS NEGATIVE                                  00026050
               MOVE SPACES TO NICKNAME.                                 00026060
           IF IND-ADDRESS2 IS NEGATIVE                                  00026070
               MOVE SPACES TO ADDRESS2.                                 00026080
           IF IND-ASSOCIATION1 IS NEGATIVE                              00026090
               MOVE SPACES TO ASSOCIATION1.                             00026100
           IF IND-ASSOCIATION2 IS NEGATIVE                              00026110
               MOVE SPACES TO ASSOCIATION2.                             00026120
           IF IND-ASSOCIATION3 IS NEGATIVE                              00026130
               MOVE SPACES TO ASSOCIATION3.                             00026140
                                                                        00026150
           PERFORM  0220-MOVE-COMMAREA-TO-TABLE.                        00026160
                                                                        00026170
      *---------------------------------------------------------------* 00026180
      *    NOTE YOU MAY HAVE TO ADD OTHER ENTITY TYPES TO THE FOLLOWIN* 00026190
      *    IF STATEMENT. THIS ALLOWS THE PROGRAM TO TRANFER TO THE    * 00026200
      *    BROKER ADD PROGRAM AND IF THE BROKER ADD IS CANCELLED THE  * 00026210
      *    TYPE REMAINS AS A MAIL AND IS NOT UPDATED IN THE SYSTEM.   * 00026220
      *    FUTURE TYPES THAT MAY NEED TO BE ADDED ARE CAMS, CASES AND * 00026230
      *    MAGICS.                                                    * 00026240
      *---------------------------------------------------------------* 00026250
                                                                        00026260
           IF COM-ENTITY-TYPE = 'BR'                                    00026270
               MOVE 'ML'  TO ENTITY-TYPE.                               00026280
           IF COM-ENTITY-TYPE = 'RP'                                    00026290
               MOVE 'RP'  TO ENTITY-TYPE.                               00026300
           MOVE WS-IDNTITY-NUM TO COM-IDNTITY.                          00026310
           MOVE WS-IDNTITY-NUM TO IDNTITY.                              00026320
           MOVE IDNTITY           TO MAP-CIMO,                          00026330
                                 PASS-WORD.                             00026340
           DISPLAY 'EFF DATE:' EFFECTIVE-DATE
                                                                        00026350
      *---------------------------------------------------------------* 00026360
      *                                                               * 00026370
      *    WHEN EXECUTING THE ADD FUNCTION CHANGE THE ENTITY TYPE     * 00026380
      *    TO MAIL. THEN WHEN THE SYSTEM SUCH AS BROKER COMPLETES     * 00026390
      *    ITS PORTION OF THE ADD THE 2BYTES ENTITY TYPE WILL BE      * 00026400
      *    UPDATED TO REFLECT THE PARTICULAR ADD THAT WAS EXECUTED    * 00026410
      *    (BROKER, CASE, MAGIC). IF THE CUSTOMER DOES NOT COMPLETE   * 00026420
      *    THE ADD THE MAILING TYPE REMAINS.                          * 00026430
      *---------------------------------------------------------------* 00026440
                                                                        00026450
           EXEC SQL                                                     00026460
              INSERT INTO  AGNTNAME                                     00026470
                (IDNTITY, LAST_NAME, FIRST_NAME, MIDDLE_NAME,           00026480
                 PREFIX, SUFFIX1, SUFFIX2,                              00026490
                 COMPANY_IND, COMPANY_IN_ADDRESS,                       00026500
                 COMPANY_NAME, DISPLAY_NAME, NICKNAME,                  00026510
                 ADDRESS1, ADDRESS2,                                    00026520
                 CITY, STATE, ZIP, ZIP_PLUS4, COUNTY_CODE, AREA_CODE,   00026530
                 PHONE,  PHONE_EXTENSION,  SSN, SEX, BIRTH_DATE,        00026540
                 FINALST_REAS_CODE, FINALST_OVRD_IND, DUP_ADDR_OVRD_IND,00026550
                 EFFECTIVE_DATE, CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,00026560
                 RECORD_STATUS, ALT_ADDRESS_IND, FUTURE_ADDRESS_IND,    00026570
                 RECORD_ORIGIN, COMBINED_STATUS, SITE_CODE, NAME_KEY1,  00026580
                 NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,                    00026590
                 ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,              00026600
                 FAX_AREA_CODE, FAX_PHONE, OFF_PRODUCERID, PASSWORD)    00026610
           VALUES (:IDNTITY, :LAST-NAME, :FIRST-NAME, :MIDDLE-NAME,     00026620
             :PREFIX, :SUFFIX1, :SUFFIX2,                               00026630
             :COMPANY-IND, :COMPANY-IN-ADDRESS,                         00026640
             :COMPANY-NAME, :DISPLAY-NAME, :NICKNAME,                   00026650
             :ADDRESS1, :ADDRESS2,                                      00026660
             :CITY, :STATE, :ZIP, :ZIP-PLUS4, :COUNTY-CODE, :AREA-CODE, 00026670
             :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,                  00026680
             :BIRTH-DATE :IND-BIRTH-DATE,                               00026690
             :FINALST-REAS-CODE, :FINALST-OVRD-IND, :DUP-ADDR-OVRD-IND, 00026700
             :EFFECTIVE-DATE, CURRENT TIMESTAMP,                        00026710
             :CHANGE-LOGON, :ENTITY-TYPE,                               00026720
             :RECORD-STATUS, :ALT-ADDRESS-IND, :FUTURE-ADDRESS-IND,     00026730
             :RECORD-ORIGIN, :COMBINED-STATUS, :SITE-CODE, :NAME-KEY1,  00026740
             :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,                     00026750
             :ASSOCIATION1, :ASSOCIATION2, :ASSOCIATION3,               00026760
             :FAX-AREA-CODE, :FAX-PHONE, :WS-OFF-PRODUCERID,            00026770
             :PASS-WORD)                                                00026780
           END-EXEC.                                                    00026790
               DISPLAY 'SQLCODE' SQLCODE
           IF SQLCODE NOT = ZERO                                        00026810
              EXEC CICS SYNCPOINT ROLLBACK                              00026820
                               END-EXEC                                 00026830
              GO TO 0410-DB2-ERROR                                      00026840
           ELSE                                                         00026850
              MOVE 'NA153'           TO WS-MESSAGE-NUMBER1.             00026860
                                                                        00026870
           IF NOT (COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'RP') 00026880
               EXEC SQL                                                 00026890
                   UPDATE NEXT_IDNTITY                                  00026900
                      SET NEXT_IDNTITY_NUM = :HOLD-NEXT-ID-R            00026910
               END-EXEC                                                 00026920
               IF SQLCODE NOT = 0                                       00026930
                 EXEC CICS SYNCPOINT ROLLBACK                           00026940
                                  END-EXEC                              00026950
                 GO TO 0410-DB2-ERROR                                   00026960
           END-IF.                                                      00026970
                                                                        00026980
      *---------------------------------------------------------------* 00026990
      *   WRITE AUDIT RECORD TO REFLECT THAT A CHANGE HAS TAKEN PLACE * 00027000
      *---------------------------------------------------------------* 00027010
                                                                        00027020
           MOVE SPACES TO AUDIT-COMM-AREA.                              00027030
           EXEC CICS ASSIGN OPID(OPER-ID)                               00027040
                            RESP   (WS-CICS-RESP)                       00027050
                            END-EXEC.                                   00027060
                                                                        00027070
           MOVE OPER-ID      TO INITIALS.                               00027080
           MOVE IDNTITY      TO AC-ID-NUMBER.                           00027090
           MOVE SPACES       TO AC-PREV-RECORD.                         00027100
           MOVE DCLAGNTNAME  TO AC-CURR-RECORD.                         00027110
           MOVE 'NAA0'       TO AC-TRANID.                              00027120
           MOVE 'NI'         TO AUDIT-CODE.                             00027130
           COMPUTE AC-REMAINDER-LENGTH = 2 * LENGTH OF DCLAGNTNAME.     00027140
                                                                        00027150
      *    EXEC CICS LINK PROGRAM ('AUDPRG2')                           00027160
      *                   COMMAREA (AUDIT-COMM-AREA)                    00027170
      **                  LENGTH (WS-AUD-LENGTH)                        00027180
      *                   LENGTH (LENGTH OF AUDIT-COMM-AREA)            00027190
      *                   END-EXEC.                                     00027200
                                                                        00027210
      *---------------------------------------------------------------* 00027220
      *   THIS CODE IS USED TO CHECK FOR A DISABLED OR NOT OPEN       * 00027230
      *   CONDITION ON THE WRITE OF THE AUDIT PROGRAM.                * 00027240
      *---------------------------------------------------------------* 00027250
      *                                                               * 00027260
      *    IF CICS-ERROR-SW = 'Y'                                     * 00027270
      *        EXEC CICS RETURN                                       * 00027280
      *                  END-EXEC.                                    * 00027290
      *                                                               * 00027300
      *---------------------------------------------------------------* 00027310
                                                                        00027320
           EXEC SQL                                                     00027330
               CLOSE AGNTCUR                                            00027340
           END-EXEC.                                                    00027350
                                                                        00027360
                                                                        00027450
      *---------------------------------------------------------------* 00027460
      *    IF COM-FINALST-OVRD-IND = 'Y'                             *  00027470
      *        PERFORM 0240-WRITE-ADDR-OVRD-TABLE.                    * 00027480
      *---------------------------------------------------------------* 00027490
                                                                        00027500
      *---------------------------------------------------------------* 00027510
      *    IN THE FUTURE ADD ADDITIONAL PARAGRAPHS HERE FOR           * 00027520
      *    CASES MAGICS AND ANY OTHER ADD FUNCTIONS                   * 00027530
      *---------------------------------------------------------------* 00027540
                                                                        00027550
MANJU *    IF COM-ENTITY-TYPE = 'BR'                                    00027560
      *        PERFORM 0250-COMPLETE-BROKER-ADD.                        00027570
                                                                        00027580
           PERFORM  0230-RETURN-TO-UPDATE.                              00027590
                                                                        00027600
       0210-ADD-CASENAME-ROW.                                           00027610
                                                                        00027620
           EXEC SQL                                                     00027630
               LOCK TABLE NEXT_IDNTITY IN EXCLUSIVE MODE                00027640
           END-EXEC.                                                    00027650
           IF SQLCODE = (-904 OR -911 OR -913) AND                      00027660
               WS-TABLE-RETRY-CNT < 10                                  00027670
              ADD 1 TO WS-TABLE-RETRY-CNT                               00027680
              GO TO 0210-ADD-CASENAME-ROW                               00027690
           END-IF.                                                      00027700
           IF SQLCODE NOT = ZERO                                        00027710
              MOVE 'DB008'       TO WS-MESSAGE-NUMBER1                  00027720
              MOVE SQLCODE TO WS-DISPLAY-SQLCODE-N                      00027730
              MOVE DFHBMASB TO MAP-CIMA                                 00027740
              EXEC CICS SYNCPOINT ROLLBACK                              00027750
                            END-EXEC                                    00027760
              GO TO 0190-SEND-NA330M1-MAP                               00027770
           END-IF.                                                      00027780
           EXEC SQL                                                     00027790
               SELECT NEXT_IDNTITY_NUM                                  00027800
                 INTO :HOLD-NEXT-ID-R                                   00027810
               FROM NEXT_IDNTITY                                        00027820
           END-EXEC.                                                    00027830
           ADD 1 TO HOLD-NEXT-ID.                                       00027840
           MOVE HOLD-NEXT-ID-R    TO WS-IDNTITY-NUM.                    00027850
                                                                        00027860
           EXEC SQL                                                     00027870
               OPEN CASECUR                                             00027880
           END-EXEC.                                                    00027890
                                                                        00027900
           IF SQLCODE NOT = ZERO                                        00027910
              EXEC CICS SYNCPOINT ROLLBACK                              00027920
                            END-EXEC                                    00027930
              GO TO 0190-SEND-NA330M1-MAP                               00027940
           END-IF.                                                      00027950
                                                                        00027960
           EXEC SQL FETCH CASECUR                                       00027970
           INTO  :IDNTITY, :LAST-NAME, :FIRST-NAME, :MIDDLE-NAME,       00027980
             :PREFIX, :SUFFIX1, :SUFFIX2,                               00027990
             :COMPANY-IND, :COMPANY-IN-ADDRESS,                         00028000
             :COMPANY-NAME, :DISPLAY-NAME, :NICKNAME,                   00028010
             :ADDRESS1, :ADDRESS2,                                      00028020
             :CITY, :STATE, :ZIP, :ZIP-PLUS4, :COUNTY-CODE, :AREA-CODE, 00028030
             :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,                  00028040
             :BIRTH-DATE :IND-BIRTH-DATE,                               00028050
             :FINALST-REAS-CODE, :FINALST-OVRD-IND, :DUP-ADDR-OVRD-IND, 00028060
             :EFFECTIVE-DATE, :CHANGE-DATE,                             00028070
             :CHANGE-LOGON, :ENTITY-TYPE,                               00028080
             :RECORD-STATUS, :ALT-ADDRESS-IND, :FUTURE-ADDRESS-IND,     00028090
             :RECORD-ORIGIN, :COMBINED-STATUS, :SITE-CODE, :NAME-KEY1,  00028100
             :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,                     00028110
             :ASSOCIATION1, :ASSOCIATION2, :ASSOCIATION3,               00028120
             :FAX-AREA-CODE, :FAX-PHONE, :EMAIL1, :PASS-WORD            00028130
      *        INTO :DCLAGNTNAME:AGNTNAME-INDICATORS                    00028140
           END-EXEC.                                                    00028150
                                                                        00028160
           IF SQLCODE = ZERO                                            00028170
               MOVE 'NA152'           TO WS-MESSAGE-NUMBER1             00028180
               MOVE 'Y' TO WS-DUPLICATE-SW                              00028190
               MOVE DFHBMASB TO MAP-CIMA                                00028200
               EXEC CICS SYNCPOINT ROLLBACK                             00028210
                                END-EXEC                                00028220
               EXEC SQL                                                 00028230
                 CLOSE CASECUR                                          00028240
               END-EXEC                                                 00028250
               GO TO 0190-SEND-NA330M1-MAP                              00028260
           ELSE                                                         00028270
              IF SQLCODE NOT = ZERO AND SQLCODE NOT = 100               00028280
                 EXEC CICS SYNCPOINT ROLLBACK                           00028290
                                  END-EXEC                              00028300
                 GO TO 0410-DB2-ERROR.                                  00028310
                                                                        00028320
           IF IND-LAST-NAME IS NEGATIVE                                 00028330
               MOVE SPACES TO LAST-NAME.                                00028340
           IF IND-FIRST-NAME IS NEGATIVE                                00028350
               MOVE SPACES TO FIRST-NAME.                               00028360
           IF IND-MIDDLE-NAME IS NEGATIVE                               00028370
               MOVE SPACES TO MIDDLE-NAME.                              00028380
           IF IND-PREFIX IS NEGATIVE                                    00028390
               MOVE SPACES TO PREFIX.                                   00028400
           IF IND-SUFFIX1 IS NEGATIVE                                   00028410
               MOVE SPACES TO SUFFIX1.                                  00028420
           IF IND-SUFFIX2 IS NEGATIVE                                   00028430
               MOVE SPACES TO SUFFIX2.                                  00028440
           IF IND-COMPANY-NAME IS NEGATIVE                              00028450
               MOVE SPACES TO COMPANY-NAME.                             00028460
           IF IND-BIRTH-DATE IS NEGATIVE                                00028470
               MOVE SPACES TO BIRTH-DATE.                               00028480
           IF IND-DISPLAY-NAME IS NEGATIVE                              00028490
               MOVE SPACES TO DISPLAY-NAME.                             00028500
           IF IND-NICKNAME IS NEGATIVE                                  00028510
               MOVE SPACES TO NICKNAME.                                 00028520
           IF IND-ADDRESS2 IS NEGATIVE                                  00028530
               MOVE SPACES TO ADDRESS2.                                 00028540
           IF IND-ASSOCIATION1 IS NEGATIVE                              00028550
               MOVE SPACES TO ASSOCIATION1.                             00028560
           IF IND-ASSOCIATION2 IS NEGATIVE                              00028570
               MOVE SPACES TO ASSOCIATION2.                             00028580
           IF IND-ASSOCIATION3 IS NEGATIVE                              00028590
               MOVE SPACES TO ASSOCIATION3.                             00028600
                                                                        00028610
           PERFORM  0220-MOVE-COMMAREA-TO-TABLE.                        00028620
                                                                        00028630
      *---------------------------------------------------------------* 00028640
      *    NOTE YOU MAY HAVE TO ADD OTHER ENTITY TYPES TO THE FOLLOWIN* 00028650
      *    IF STATEMENT. THIS ALLOWS THE PROGRAM TO TRANFER TO THE    * 00028660
      *    BROKER ADD PROGRAM AND IF THE BROKER ADD IS CANCELLED THE  * 00028670
      *    TYPE REMAINS AS A MAIL AND IS NOT UPDATED IN THE SYSTEM.   * 00028680
      *    FUTURE TYPES THAT MAY NEED TO BE ADDED ARE CAMS, CASES AND * 00028690
      *    MAGICS.                                                    * 00028700
      *---------------------------------------------------------------* 00028710
                                                                        00028720
           IF COM-ENTITY-TYPE = 'BR'                                    00028730
               MOVE 'ML'  TO ENTITY-TYPE.                               00028740
           MOVE WS-IDNTITY-NUM TO COM-IDNTITY.                          00028750
           MOVE WS-IDNTITY-NUM TO IDNTITY.                              00028760
           MOVE IDNTITY        TO MAP-CIMO,                             00028770
                                 PASS-WORD.                             00028780
           IF COM-ENTITY-TYPE NOT = 'BA'                                00028790
               MOVE 'W'           TO RECORD-STATUS.                     00028800
                                                                        00028810
           IF COM-ENTITY-TYPE = 'LD'                                    00028820
               MOVE 'A'           TO RECORD-STATUS.                     00028830
                                                                        00028840
      *---------------------------------------------------------------* 00028850
      *                                                               * 00028860
      *    WHEN EXECUTING THE ADD FUNCTION CHANGE THE ENTITY TYPE     * 00028870
      *    TO MAIL. THEN WHEN THE SYSTEM SUCH AS BROKER COMPLETES     * 00028880
      *    ITS PORTION OF THE ADD THE 2BYTES ENTITY TYPE WILL BE      * 00028890
      *    UPDATED TO REFLECT THE PARTICULAR ADD THAT WAS EXECUTED    * 00028900
      *    (BROKER, CASE, MAGIC). IF THE CUSTOMER DOES NOT COMPLETE   * 00028910
      *    THE ADD THE MAILING TYPE REMAINS.                          * 00028920
      *---------------------------------------------------------------* 00028930
                                                                        00028940
           EXEC SQL                                                     00028950
              INSERT INTO  CASENAME                                     00028960
                (IDNTITY, LAST_NAME, FIRST_NAME, MIDDLE_NAME,           00028970
                 PREFIX, SUFFIX1, SUFFIX2,                              00028980
                 COMPANY_IND, COMPANY_IN_ADDRESS,                       00028990
                 COMPANY_NAME, DISPLAY_NAME, NICKNAME,                  00029000
                 ADDRESS1, ADDRESS2,                                    00029010
                 CITY, STATE, ZIP, ZIP_PLUS4, COUNTY_CODE, AREA_CODE,   00029020
                 PHONE,  PHONE_EXTENSION,  SSN, SEX, BIRTH_DATE,        00029030
                 FINALST_REAS_CODE, FINALST_OVRD_IND, DUP_ADDR_OVRD_IND,00029040
                 EFFECTIVE_DATE, CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,00029050
                 RECORD_STATUS, ALT_ADDRESS_IND, FUTURE_ADDRESS_IND,    00029060
                 RECORD_ORIGIN, COMBINED_STATUS, SITE_CODE, NAME_KEY1,  00029070
                 NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,                    00029080
                 ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,              00029090
                 FAX_AREA_CODE, FAX_PHONE, EMAIL, PASSWORD,             00029100
                 COUNTRY_CODE, POSTAL_CODE,                             00029110
                 ORIGINAL_STATE, ORIGINAL_ZIP, ORIGINAL_ZIP_PLUS4,
                 EMAIL_STATUS)
      *          FAX_AREA_CODE, FAX_PHONE, PASSWORD)                    00029120
      *          FAX_AREA_CODE, FAX_PHONE)                              00029130
           VALUES (:IDNTITY, :LAST-NAME, :FIRST-NAME, :MIDDLE-NAME,     00029140
             :PREFIX, :SUFFIX1, :SUFFIX2,                               00029150
             :COMPANY-IND, :COMPANY-IN-ADDRESS,                         00029160
             :COMPANY-NAME, :DISPLAY-NAME, :NICKNAME,                   00029170
             :ADDRESS1, :ADDRESS2,                                      00029180
             :CITY, :STATE, :ZIP, :ZIP-PLUS4, :COUNTY-CODE, :AREA-CODE, 00029190
             :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,                  00029200
             :BIRTH-DATE :IND-BIRTH-DATE,                               00029210
             :FINALST-REAS-CODE, :FINALST-OVRD-IND, :DUP-ADDR-OVRD-IND, 00029220
             :EFFECTIVE-DATE, CURRENT TIMESTAMP,                        00029230
             :CHANGE-LOGON, :ENTITY-TYPE,                               00029240
             :RECORD-STATUS, :ALT-ADDRESS-IND, :FUTURE-ADDRESS-IND,     00029250
             :RECORD-ORIGIN, :COMBINED-STATUS, :SITE-CODE, :NAME-KEY1,  00029260
             :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,                     00029270
             :ASSOCIATION1, :ASSOCIATION2, :ASSOCIATION3,               00029280
             :FAX-AREA-CODE, :FAX-PHONE, :EMAIL1, :PASS-WORD,           00029290
             :COUNTRY-CODE, :POSTAL-CODE,                               00029300
             :WS-SPACES-2, :WS-SPACES-5, :WS-SPACES-4,
             :WS-SPACES-1)
      *      :FAX-AREA-CODE, :FAX-PHONE, :PASS-WORD)                    00029310
      *      :FAX-AREA-CODE, :FAX-PHONE)                                00029320
           END-EXEC.                                                    00029330
                                                                        00029340
           IF SQLCODE NOT = ZERO                                        00029350
                 EXEC CICS SYNCPOINT ROLLBACK                           00029360
                                  END-EXEC                              00029370
                 GO TO 0410-DB2-ERROR                                   00029380
           ELSE                                                         00029390
              MOVE 'NA153'           TO WS-MESSAGE-NUMBER1.             00029400
                                                                        00029410
           EXEC SQL                                                     00029420
               UPDATE NEXT_IDNTITY                                      00029430
                  SET NEXT_IDNTITY_NUM = :HOLD-NEXT-ID-R                00029440
           END-EXEC.                                                    00029450
           IF SQLCODE NOT = 0                                           00029460
                 GO TO 0410-DB2-ERROR.                                  00029470
                                                                        00029480
           EXEC SQL                                                     00029490
               CLOSE CASECUR                                            00029500
           END-EXEC.                                                    00029510
                                                                        00029520
                                                                        00029610
      *---------------------------------------------------------------* 00029620
      *    IF COM-FINALST-OVRD-IND = 'Y'                             *  00029630
      *        PERFORM 0240-WRITE-ADDR-OVRD-TABLE.                    * 00029640
      *---------------------------------------------------------------* 00029650
                                                                        00029660
           IF COM-ENTITY-TYPE = 'AM' OR 'CA' OR 'LB'                    00029670
               PERFORM 0260-COMPLETE-CASE-ADD.                          00029680
                                                                        00029690
           PERFORM  0230-RETURN-TO-UPDATE.                              00029700
                                                                        00029710
       0220-MOVE-COMMAREA-TO-TABLE.                                     00029720
                                                                        00029730
           MOVE COM-LAST-NAME          TO LAST-NAME.                    00029740
           MOVE COM-FIRST-NAME         TO FIRST-NAME.                   00029750
           MOVE COM-MIDDLE-NAME        TO MIDDLE-NAME.                  00029760
           MOVE COM-PREFIX             TO PREFIX.                       00029770
           MOVE COM-SUFFIX1            TO SUFFIX1.                      00029780
           MOVE COM-SUFFIX2            TO SUFFIX2.                      00029790
           MOVE COM-COMPANY-IND        TO COMPANY-IND.                  00029800
           MOVE COM-COMPANY-IN-ADDRESS TO COMPANY-IN-ADDRESS.           00029810
           MOVE COM-COMPANY-NAME       TO COMPANY-NAME.                 00029820
           MOVE COM-DISPLAY-NAME       TO DISPLAY-NAME.                 00029830
           MOVE COM-NICKNAME           TO NICKNAME.                     00029840
           MOVE COM-ADDRESS1           TO ADDRESS1.                     00029850
           MOVE COM-ADDRESS2           TO ADDRESS2.                     00029860
           MOVE COM-CITY               TO CITY.                         00029870
           MOVE COM-STATE              TO STATE.                        00029880
           MOVE COM-ZIP                TO ZIP.                          00029890
           MOVE COM-ZIP-PLUS4          TO ZIP-PLUS4.                    00029900
           MOVE COM-COUNTY-CODE        TO COUNTY-CODE.                  00029910
           MOVE COM-AREA-CODE          TO AREA-CODE.                    00029920
           MOVE COM-PHONE              TO PHONE.                        00029930
           MOVE COM-PHONE-EXTENSION    TO PHONE-EXTENSION.              00029940
           MOVE COM-FAX-AREA-CODE      TO FAX-AREA-CODE.                00029950
           MOVE COM-FAX-PHONE          TO FAX-PHONE.                    00029960
           MOVE COM-SSN                TO SSN-NUM.                      00029970
           MOVE COM-SEX                TO SEX.                          00029980
           MOVE COM-FINALST-REAS-CODE  TO FINALST-REAS-CODE.            00029990
           MOVE COM-FINALST-OVRD-IND   TO FINALST-OVRD-IND.             00030000
           MOVE COM-DUP-ADDR-OVRD-IND  TO DUP-ADDR-OVRD-IND.            00030010
           MOVE COM-CHANGE-LOGON       TO CHANGE-LOGON.                 00030020
           MOVE COM-ENTITY-TYPE        TO ENTITY-TYPE.                  00030030
           MOVE COM-RECORD-STATUS      TO RECORD-STATUS.                00030040
           MOVE COM-ALT-ADDRESS-IND    TO ALT-ADDRESS-IND.              00030050
           MOVE COM-FUTURE-ADDRESS-IND TO FUTURE-ADDRESS-IND.           00030060
           MOVE COM-RECORD-ORIGIN      TO RECORD-ORIGIN.                00030070
           MOVE COM-COMBINED-STATUS    TO COMBINED-STATUS.              00030080
           MOVE COM-SITE-CODE          TO SITE-CODE.                    00030090
           MOVE COM-NAME-KEY1          TO NAME-KEY1.                    00030100
           MOVE COM-NAME-KEY2          TO NAME-KEY2.                    00030110
           MOVE COM-NAME-KEY3          TO NAME-KEY3.                    00030120
           MOVE COM-ADDRESS-KEY1       TO ADDRESS-KEY1.                 00030130
           MOVE COM-ASSOCIATION1       TO ASSOCIATION1.                 00030140
           MOVE COM-ASSOCIATION2       TO ASSOCIATION2.                 00030150
           MOVE COM-ASSOCIATION3       TO ASSOCIATION3.                 00030160
           MOVE COM-EMAIL1             TO EMAIL1.                       00030170
           MOVE COM-POSTAL-CODE        TO POSTAL-CODE.                  00030180
           MOVE COM-COUNTRY-CODE       TO COUNTRY-CODE.                 00030190
           DISPLAY 'COM-EFFECTIVE-DATE:' COM-EFFECTIVE-DATE
           MOVE COM-EFFECTIVE-DATE     TO WS-DATE-R.                    00030200
           MOVE WS-DATE-R              TO WS-DATE.                      00030210
      *                                                                 00030220
      * IMR CHANGE IMR4 BEGIN                                           00030230
      *                                                                 00030240
      *    MOVE '19'                   TO EF-CC.                        00030250
           MOVE WS-YY-A  TO  Y2K-WK-DATE1R-X-YY.                        00030260
           MOVE WS-MM-A  TO  Y2K-WK-DATE1R-X-MM.                        00030270
           MOVE WS-DD-A  TO  Y2K-WK-DATE1R-X-DD.                        00030280
                                                                        00030290
               IF Y2K-WK-DATE1R-X-YY LESS THAN Y2K-WK-CUTOFF-YR-X       00030300
                  MOVE '20' TO Y2K-WK-DATE1-X-CC                        00030310
               ELSE                                                     00030320
                  MOVE '19' TO Y2K-WK-DATE1-X-CC                        00030330
               END-IF.                                                  00030340
                                                                        00030350
           MOVE Y2K-WK-DATE1-X-CC      TO EF-CC.                        00030360
      *                                                                 00030370
      * IMR CHANGE IMR4 END                                             00030380
      *                                                                 00030390
           MOVE WS-YY-A                TO EF-YY.                        00030400
           MOVE WS-MM-A                TO EF-MM.                        00030410
           MOVE WS-DD-A                TO EF-DD.                        00030420
           MOVE WS-EFFECTIVE-DATE      TO EFFECTIVE-DATE.               00030430
           DISPLAY 'WS-EFFECTIVE-DATE:' EFFECTIVE-DATE
                                                                        00030440
           IF COM-BIRTH-DATE NOT =  ZEROS                               00030450
      *                                                                 00030460
      * IMR CHANGE IMR5 BEGIN                                           00030470
      *                                                                 00030480
      *        MOVE COM-BIRTH-DATE TO WS-DATE-R                         00030490
      *        MOVE WS-DATE-R      TO WS-DATE                           00030500
      *        MOVE '19'           TO BR-CC                             00030510
      *        MOVE WS-YY-A        TO BR-YY                             00030520
      *        MOVE WS-MM-A        TO BR-MM                             00030530
      *        MOVE WS-DD-A        TO BR-DD                             00030540
      *                                                                 00030550
               MOVE COM-BIRTH-DATE TO Y2K-WK-VARIABLE2-N                00030560
               MOVE Y2K-WK-VARIABLE2-CC TO BR-CC                        00030570
               MOVE Y2K-WK-VARIABLE2-YY TO BR-YY                        00030580
               MOVE Y2K-WK-VARIABLE2-MM TO BR-MM                        00030590
               MOVE Y2K-WK-VARIABLE2-DD TO BR-DD                        00030600
      *                                                                 00030610
      * IMR CHANGE IMR5 END                                             00030620
      *                                                                 00030630
               MOVE WS-BIRTH-DATE  TO BIRTH-DATE                        00030640
           ELSE                                                         00030650
               MOVE -1 TO IND-BIRTH-DATE.                               00030660
                                                                        00030670
       0230-RETURN-TO-UPDATE.                                           00030680
           DISPLAY '0230-RETURN-TO-UPDATE PARA'                         00030690
           MOVE '00' TO COMM-NEXT-FUNCTION.                             00030700
           MOVE ZERO TO COMM-CURSOR-POSN     COMM-MSG-COUNT             00030710
                        COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.     00030720
                                                                        00030730
           MOVE SPACES             TO COMM-MSG-ID(1) COMM-MSG-ID(2)     00030740
                                      COMM-MSG-ID(3) COMM-MSG-ID(4).    00030750
           MOVE 'NAA0'             TO COMM-PREVIOUS-TRANID.             00030760
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00030770
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00030780
                                                                        00030790
      *---------------------------------------------------------------* 00030800
      *    THIS CODE RETURNS THE PROGRAM BACK TO THE NAME AND ADDRESS * 00030810
      *    PROGRAM AFTER THE RE-ASSEMBLE NAME HAS BEEN CHANGED.       * 00030820
      *---------------------------------------------------------------* 00030830
                                                                        00030840
           MOVE 'NA'              TO COMM-SYSTEM-CODE                   00030850
           MOVE 'U_____'          TO COMM-ACTION-CODE                   00030860
           MOVE COM-IDNTITY       TO COMM-IDNTITY                       00030870
           MOVE 'NA153'           TO COMM-MSG-ID(1)                     00030880
           MOVE COM-DISPLAY-NAME  TO COMM-MAIL-LINE(1)                  00030890
           MOVE COM-ADDRESS1      TO COMM-MAIL-LINE(2)                  00030900
           MOVE COM-CITY          TO COMM-MAIL-LINE(3)                  00030910
           MOVE COM-STATE         TO WS-STATE                           00030920
           MOVE COM-ZIP           TO WS-ZIP                             00030930
           MOVE WS-STATE-ZIP      TO COMM-MAIL-LINE(4)                  00030940
           MOVE COM-AREA-CODE     TO COMM-CUST-PHONE-AREA               00030950
           MOVE COM-RECORD-STATUS TO COMM-CUST-STATUS                   00030960
           MOVE COM-ENTITY-TYPE   TO COMM-CUST-TYPE                     00030970
                                                                        00030980
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00030990
                               FROM   (NA-COMMAREA)                     00031000
                               LENGTH (WS-COMM-LENGTH)                  00031010
                               ITEM   (WS-TS-ITEM)                      00031020
                               RESP   (WS-CICS-RESP)                    00031030
                               REWRITE                                  00031040
                               MAIN                                     00031050
                               END-EXEC.                                00031060
                                                                        00031070
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00031080
                GO TO 9999-CICS-ERROR.                                  00031090
                                                                        00031100
      *    EXEC CICS SEND MAP ('SYSBUSY')                               00031110
      *                   RESP (WS-CICS-RESP)                           00031120
      *                   END-EXEC.                                     00031130
      *                                                                 00031140
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00031150
      *         GO TO 9999-CICS-ERROR.                                  00031160
                                                                        00031170
           EXEC CICS START TRANSID('NAU0')                              00031180
                           TERMID (EIBTRMID)                            00031190
                           RESP   (WS-CICS-RESP)                        00031200
                           END-EXEC.                                    00031210
                                                                        00031220
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00031230
                GO TO 9999-CICS-ERROR.                                  00031240
                                                                        00031250
           EXEC CICS RETURN                                             00031260
                     END-EXEC.                                          00031270
                                                                        00031280
      *---------------------------------------------------------------* 00031290
      *0240-WRITE-ADDR-OVRD-TABLE.                                    * 00031300
      *---------------------------------------------------------------* 00031310
                                                                        00031320
       0250-COMPLETE-BROKER-ADD.                                        00031330
           DISPLAY '0250-COMPLETE-BROKER-ADD PARA'                      00031340
           MOVE '00' TO COMM-NEXT-FUNCTION.                             00031350
           MOVE ZERO TO COMM-CURSOR-POSN     COMM-MSG-COUNT             00031360
                        COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.     00031370
                                                                        00031380
           MOVE SPACES             TO COMM-MSG-ID(1) COMM-MSG-ID(2)     00031390
                                      COMM-MSG-ID(3) COMM-MSG-ID(4).    00031400
           MOVE 'NAA0'             TO COMM-PREVIOUS-TRANID.             00031410
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00031420
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00031430
                                                                        00031440
      *---------------------------------------------------------------* 00031450
      *    ADD FUTURE ADD TYPE COMMAREA'S AT THIS LOCATION            * 00031460
      *    INCLUDING CASES AND MAGIC'S                                * 00031470
      *---------------------------------------------------------------* 00031480
                                                                        00031490
           IF COM-ENTITY-TYPE = 'BR'                                    00031500
               MOVE 'BR'              TO COMM-SYSTEM-CODE               00031510
               MOVE 'A_____'          TO COMM-ACTION-CODE               00031520
               MOVE IDNTITY           TO COMM-IDNTITY                   00031530
               MOVE 'NA042'           TO COMM-MSG-ID(1)                 00031540
               MOVE COM-DISPLAY-NAME  TO COMM-MAIL-LINE(1)              00031550
               MOVE COM-ADDRESS1      TO COMM-MAIL-LINE(2)              00031560
               MOVE COM-CITY          TO COMM-MAIL-LINE(3)              00031570
               MOVE COM-STATE         TO WS-STATE                       00031580
               MOVE COM-ZIP           TO WS-ZIP                         00031590
               MOVE WS-STATE-ZIP      TO COMM-MAIL-LINE(4)              00031600
               MOVE COM-AREA-CODE     TO COMM-CUST-PHONE-AREA           00031610
               MOVE COM-PHONE         TO WS-PHONE                       00031620
               MOVE WS-PHONE3         TO COMM-CUST-PHONE-EXCH           00031630
               MOVE WS-PHONE4         TO COMM-CUST-PHONE-NUMB           00031640
               MOVE COM-RECORD-STATUS TO COMM-CUST-STATUS               00031650
               MOVE COM-ENTITY-TYPE   TO COMM-CUST-TYPE                 00031660
               MOVE NA-COMMAREA       TO WS-AGENT-COMMAREA              00031670
               MOVE SPACES            TO WS-BROKER-FIELDS               00031680
               MOVE COM-ENTITY-TYPE   TO WS-SYSTEM-ADD-TYPE.            00031690
                                                                        00031700
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00031710
                               FROM   (WS-BROKER-COMMAREA)              00031720
                               LENGTH (WS-BROKER-LENGTH)                00031730
                               ITEM   (WS-TS-ITEM)                      00031740
                               RESP   (WS-CICS-RESP)                    00031750
                               REWRITE                                  00031760
                               MAIN                                     00031770
                               END-EXEC.                                00031780
                                                                        00031790
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00031800
                GO TO 9999-CICS-ERROR.                                  00031810
                                                                        00031820
MANJU *    EXEC CICS SEND MAP ('SYSBUSY')                               00031830
      *                   RESP (WS-CICS-RESP)                           00031840
      *                   END-EXEC.                                     00031850
                                                                        00031860
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00031870
      *         GO TO 9999-CICS-ERROR.                                  00031880
                                                                        00031890
      *    EXEC CICS START TRANSID('BRA0')                              00031900
      *                    TERMID (EIBTRMID)                            00031910
      *                    RESP   (WS-CICS-RESP)                        00031920
      *                    END-EXEC.                                    00031930
      *                                                                 00031940
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00031950
      *         GO TO 9999-CICS-ERROR.                                  00031960
      *                                                                 00031970
           EXEC CICS RETURN                                             00031980
                     END-EXEC.                                          00031990
                                                                        00032000
       0260-COMPLETE-CASE-ADD.                                          00032010
                                                                        00032020
           IF (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'AA')     00032030
             IF COM-ENTITY-TYPE = 'AM' OR 'CA'                          00032040
               MOVE 'NAQ1'            TO WS-TS-QUEUE-TRANID             00032050
               MOVE EIBTRMID          TO WS-TS-QUEUE-TERMID             00032060
               MOVE 'NP'              TO COMM-NEXT-FUNCTION             00032070
               MOVE 'NAA0'            TO COMM-PREVIOUS-TRANID           00032080
               MOVE 'AM'              TO COMM-SYSTEM-CODE               00032090
               MOVE 'AA___'           TO COMM-ACTION-CODE               00032100
               MOVE IDNTITY           TO WS-CMDLINE-1                   00032110
               MOVE 'CIM'             TO WS-CMDLINE-2                   00032120
               MOVE WS-CMDLINE        TO COMM-CUSTOMER-INFO             00032130
               MOVE 'Y'               TO COMM-KEY-CHANGED               00032140
               MOVE ZEROS             TO COMM-MSG-COUNT                 00032150
               MOVE SPACES            TO COMM-MSG-ID (1)                00032160
                                         COMM-MSG-ID (2)                00032170
                                         COMM-MSG-ID (3)                00032180
                                         COMM-MSG-ID (4)                00032190
                                         COMM-MSG-MAX-SEVERITY.         00032200
                                                                        00032210
           IF (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'BR')     00032220
             IF COM-ENTITY-TYPE = 'AM' OR 'CA'                          00032230
               MOVE 'NAQ1'            TO WS-TS-QUEUE-TRANID             00032240
               MOVE EIBTRMID          TO WS-TS-QUEUE-TERMID             00032250
               MOVE 'NP'              TO COMM-NEXT-FUNCTION             00032260
               MOVE 'NAA0'            TO COMM-PREVIOUS-TRANID           00032270
               MOVE 'AM'              TO COMM-SYSTEM-CODE               00032280
               MOVE 'BR___'           TO COMM-ACTION-CODE               00032290
               MOVE IDNTITY           TO WS-CMDLINE-1                   00032300
               MOVE 'CIM'             TO WS-CMDLINE-2                   00032310
               MOVE WS-CMDLINE        TO COMM-CUSTOMER-INFO             00032320
               MOVE 'Y'               TO COMM-KEY-CHANGED               00032330
               MOVE ZEROS             TO COMM-MSG-COUNT                 00032340
               MOVE SPACES            TO COMM-MSG-ID (1)                00032350
                                         COMM-MSG-ID (2)                00032360
                                         COMM-MSG-ID (3)                00032370
                                         COMM-MSG-ID (4)                00032380
                                         COMM-MSG-MAX-SEVERITY.         00032390
                                                                        00032400
           IF COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'STA'      00032410
             IF COM-ENTITY-TYPE = 'AM' OR 'CA'                          00032420
               MOVE 'NAQ1'            TO WS-TS-QUEUE-TRANID             00032430
               MOVE EIBTRMID          TO WS-TS-QUEUE-TERMID             00032440
               MOVE 'NP'              TO COMM-NEXT-FUNCTION             00032450
               MOVE 'NAA0'            TO COMM-PREVIOUS-TRANID           00032460
               MOVE 'AM'              TO COMM-SYSTEM-CODE               00032470
               MOVE 'STA__'           TO COMM-ACTION-CODE               00032480
               MOVE IDNTITY           TO WS-CMDLINE-1                   00032490
               MOVE 'CIM'             TO WS-CMDLINE-2                   00032500
               MOVE WS-CMDLINE        TO COMM-CUSTOMER-INFO             00032510
               MOVE 'Y'               TO COMM-KEY-CHANGED               00032520
               MOVE ZEROS             TO COMM-MSG-COUNT                 00032530
               MOVE SPACES            TO COMM-MSG-ID (1)                00032540
                                         COMM-MSG-ID (2)                00032550
                                         COMM-MSG-ID (3)                00032560
                                         COMM-MSG-ID (4)                00032570
                                         COMM-MSG-MAX-SEVERITY.         00032580
                                                                        00032590
           IF COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'CD'       00032600
             IF COM-ENTITY-TYPE = 'AM' OR 'CA'                          00032610
               MOVE 'NAQ1'            TO WS-TS-QUEUE-TRANID             00032620
               MOVE EIBTRMID          TO WS-TS-QUEUE-TERMID             00032630
               MOVE 'NP'              TO COMM-NEXT-FUNCTION             00032640
               MOVE 'NAA0'            TO COMM-PREVIOUS-TRANID           00032650
               MOVE 'AM'              TO COMM-SYSTEM-CODE               00032660
               MOVE 'CD___'           TO COMM-ACTION-CODE               00032670
               MOVE IDNTITY           TO WS-CMDLINE-1                   00032680
               MOVE 'CIM'             TO WS-CMDLINE-2                   00032690
               MOVE WS-CMDLINE        TO COMM-CUSTOMER-INFO             00032700
               MOVE 'Y'               TO COMM-KEY-CHANGED               00032710
               MOVE ZEROS             TO COMM-MSG-COUNT                 00032720
               MOVE SPACES            TO COMM-MSG-ID (1)                00032730
                                         COMM-MSG-ID (2)                00032740
                                         COMM-MSG-ID (3)                00032750
                                         COMM-MSG-ID (4)                00032760
                                         COMM-MSG-MAX-SEVERITY.         00032770
                                                                        00032780
           IF COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'LB'       00032790
             IF COM-ENTITY-TYPE = 'LB'                                  00032800
               MOVE 'NAQ1'            TO WS-TS-QUEUE-TRANID             00032810
               MOVE EIBTRMID          TO WS-TS-QUEUE-TERMID             00032820
               MOVE 'NP'              TO COMM-NEXT-FUNCTION             00032830
               MOVE 'NAA0'            TO COMM-PREVIOUS-TRANID           00032840
               MOVE 'AM'              TO COMM-SYSTEM-CODE               00032850
               MOVE 'LB___'           TO COMM-ACTION-CODE               00032860
               MOVE IDNTITY               TO WS-CMDLINE-1               00032870
               MOVE 'CIM'             TO WS-CMDLINE-2                   00032880
               MOVE WS-CMDLINE        TO COMM-CUSTOMER-INFO             00032890
               MOVE 'Y'               TO COMM-KEY-CHANGED               00032900
               MOVE ZEROS             TO COMM-MSG-COUNT                 00032910
               MOVE SPACES            TO COMM-MSG-ID (1)                00032920
                                         COMM-MSG-ID (2)                00032930
                                         COMM-MSG-ID (3)                00032940
                                         COMM-MSG-ID (4)                00032950
                                         COMM-MSG-MAX-SEVERITY.         00032960
                                                                        00032970
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00032980
                               FROM   (NA-COMMAREA)                     00032990
                               LENGTH (WS-COMM-LENGTH)                  00033000
                               ITEM   (WS-TS-ITEM)                      00033010
                               RESP   (WS-CICS-RESP)                    00033020
                               REWRITE                                  00033030
                               MAIN                                     00033040
                               END-EXEC.                                00033050
                                                                        00033060
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00033070
                GO TO 9999-CICS-ERROR.                                  00033080
                                                                        00033090
      *    EXEC CICS SEND MAP ('SYSBUSY')                               00033100
      *                   RESP (WS-CICS-RESP)                           00033110
      *                   END-EXEC.                                     00033120
                                                                        00033130
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00033140
      *         GO TO 9999-CICS-ERROR.                                  00033150
                                                                        00033160
                                                                        00033170
      *    EXEC CICS START TRANSID('NAA0')                              00033180
      *                    TERMID (EIBTRMID)                            00033190
      *                    RESP   (WS-CICS-RESP)                        00033200
      *                    END-EXEC.                                    00033210
                                                                        00033220
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00033230
      *         GO TO 9999-CICS-ERROR.                                  00033240
                                                                        00033250
      *    EXEC CICS RETURN                                             00033260
      *              END-EXEC.                                          00033270
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
           MOVE SPACES  TO WSQ-AGNTNAME.                                00033480
           MOVE 'A'                TO WT-C2006-STATUS-CD.               00033490
           MOVE WT-CNTL2006        TO TURBO-CNTL-AREA.                  00033500
           PERFORM 10000-CALL-GSF.                                      00033510
           MOVE TURBO-CNTL-AREA    TO WT-CNTL2006.                      00033520
           IF  WT-C2006-RETURN EQUAL ZEROES                             00033530
               MOVE WT-C2006-STAT-DESC TO MAP-CUST-STATUSO              00033540
               MOVE 'A'                TO WSQ-RECORD-STATUS             00033550
           ELSE                                                         00033560
               MOVE 'A' TO MAP-CUST-STATUSO                             00033570
                           WSQ-RECORD-STATUS.                           00033580
                                                                        00033590
           MOVE ZEROES  TO WSQ-BIRTH-DATE.                              00033600
           MOVE 'N'     TO WSQ-FINALST-OVRD-IND  MAP-ADDR-OVRIDEO.      00033610
           MOVE 'Y'     TO WSQ-COMPANY-IN-ADDRESS MAP-COMP-IN-ADDO.     00033620
MANJU *    EXEC CICS ASSIGN TCTUALENG(TCTUAL) END-EXEC.                 00033630
      *    EXEC CICS ADDRESS TCTUA(ADDRESS OF TCTUAR) END-EXEC.         00033640
           MOVE TCTUA-DEMO-DATA TO WS-DEMOGRAPHIC-COMM-AREA.            00033650
                                                                        00033660
           MOVE WS-DEMO-LOGONID   TO WSQ-CHANGE-LOGON.                  00033670
MANJU1     MOVE 'WIPRO123' TO     WS-DEMO-USER-NAME.
MANJU1     MOVE '01'       TO     WS-DEMO-SITE-CODE.
           MOVE WS-DEMO-USER-NAME TO MAP-LOGONO.                        00033680
           MOVE WS-DEMO-SITE-CODE    TO WSQ-SITE-CODE MAP-SITE-CODEO    00033690
           MOVE WSQ-SITE-CODE          TO WT-C0161-SITE.                00033700
           MOVE WT-CNTL0161            TO TURBO-CNTL-AREA.              00033710
           PERFORM 10000-CALL-GSF.                                      00033720
           MOVE TURBO-CNTL-AREA    TO WT-CNTL0161.                      00033730
           IF  WT-C0161-RETURN EQUAL ZEROES                             00033740
               MOVE WT-C0161-COMPANY-NAME TO MAP-SITE-DESCO             00033750
           ELSE                                                         00033760
               MOVE SPACES         TO MAP-SITE-DESCO.                   00033770
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
           DISPLAY 'WS-MAP-BIF:' WS-MAP-BIF
           MOVE WS-MAP-BIF      TO WSQ-CHANGE-DATE WSQ-EFFECTIVE-DATE.  00033940
           MOVE WSQ-CHANGE-DATE TO WS-DATE-R                            00033950
           MOVE WS-DATE-R       TO WS-DATE.                             00033960
           MOVE WS-MM           TO WS-MONTH.                            00033970
           MOVE WS-DD           TO WS-DAY.                              00033980
           MOVE WS-YY           TO WS-YEAR.                             00033990
           MOVE '/'             TO SLASH-1 SLASH-2.                     00034000
           MOVE WS-DATE-EIGHT   TO MAP-CHANGE-DATEO MAP-EFF-DATEO.      00034010
           DISPLAY 'MAP-EFF-DATEO:' MAP-EFF-DATEO
                                                                        00034020
           MOVE COMM-CUSTOMER-INFO1 TO WS-NUMERIC-TEST8.                00034030
           IF COMM-CUSTOMER-INFO1 = 'WELCOME TO THE CUSTOMER INFORM'    00034040
             OR COMM-CUSTOMER-INFO2 = 'CIM'                             00034050
                NEXT SENTENCE                                           00034060
           ELSE                                                         00034070
           IF COMM-CUSTOMER-INFO1 NOT = SPACES AND                      00034080
              COMM-CUSTOMER-INFO2 = SPACES AND                          00034090
              WS-NUMERIC-TEST8 NOT NUMERIC                              00034100
                 MOVE COMM-CUSTOMER-INFO1 TO MAP-COMP-NAMEO             00034110
                                             WSQ-COMPANY-NAME           00034120
           ELSE                                                         00034130
           IF COMM-CUSTOMER-INFO1 NOT = SPACES AND                      00034140
              COMM-CUSTOMER-INFO2 NOT = SPACES                          00034150
                 MOVE COMM-CUSTOMER-INFO1 TO MAP-LAST-NAMEO             00034160
                                             WSQ-LAST-NAME              00034170
                 MOVE COMM-CUSTOMER-INFO2 TO MAP-FIRST-NAMEO            00034180
                                             WSQ-FIRST-NAME             00034190
                 IF COMM-CUSTOMER-INFO3 NOT = SPACES                    00034200
                    MOVE COMM-CUSTOMER-INFO3 TO MAP-MIDDLE-NAMEO        00034210
                                                WSQ-MIDDLE-NAME.        00034220
                                                                        00034230
           INSPECT COMM-ACTION-CODE REPLACING ALL '_' BY SPACE.         00034240
                                                                        00034250
           IF (COMM-SYSTEM-CODE = 'AM' AND                              00034260
           (COMM-ACTION-CODE = 'AA' OR COMM-ACTION-CODE = 'LB' OR       00034270
            COMM-ACTION-CODE = 'BR'))                                   00034280
           OR (COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'BA')     00034290
           OR (COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'LD')     00034300
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-LITERAL1A MAP-LITERAL2A 00034310
               MAP-LITERAL3A MAP-LITERAL4A MAP-LITERAL6A                00034320
               MAP-LITERAL7A MAP-LITERAL9A MAP-LITERAL10A               00034330
               MAP-LITERAL5A MAP-LITERAL8A                              00034340
               MOVE SPACES  TO MAP-FIRST-NAMEO  WSQ-FIRST-NAME          00034350
               MOVE SPACES  TO MAP-MIDDLE-NAMEO WSQ-MIDDLE-NAME         00034360
               MOVE SPACES  TO MAP-LAST-NAMEO   WSQ-LAST-NAME           00034370
               MOVE SPACES  TO MAP-SUFFIX1O     WSQ-SUFFIX1             00034380
               MOVE SPACES  TO MAP-SUFFIX2O     WSQ-SUFFIX2             00034390
               MOVE SPACES  TO MAP-NICKNAMEO    WSQ-NICKNAME            00034400
               MOVE SPACES  TO MAP-BIRTH-DATEO                          00034410
               MOVE SPACES  TO MAP-COMP-IN-ADDO                         00034420
               MOVE SPACES  TO MAP-SEXO         WSQ-SEX                 00034430
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-FIRST-NAMEA             00034440
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-MIDDLE-NAMEA            00034450
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-LAST-NAMEA              00034460
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SUFFIX1A                00034470
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SUFFIX2A                00034480
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-NICKNAMEA               00034490
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-BIRTH-DATEA             00034500
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-COMP-IN-ADDA            00034510
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SEXA.                   00034520
                                                                        00034530
           IF (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'STA')    00034540
           OR (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'CD')     00034550
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-LITERAL6A               00034560
               MAP-LITERAL7A MAP-LITERAL9A MAP-LITERAL10A               00034570
               MAP-LITERAL5A MAP-LITERAL8A MAP-LITERAL4A                00034580
               MOVE SPACES  TO MAP-SUFFIX1O     WSQ-SUFFIX1             00034590
               MOVE SPACES  TO MAP-SUFFIX2O     WSQ-SUFFIX2             00034600
               MOVE SPACES  TO MAP-NICKNAMEO    WSQ-NICKNAME            00034610
               MOVE SPACES  TO MAP-BIRTH-DATEO                          00034620
               MOVE SPACES  TO MAP-COMP-IN-ADDO                         00034630
               MOVE SPACES  TO MAP-SEXO         WSQ-SEX                 00034640
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SUFFIX1A                00034650
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SUFFIX2A                00034660
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-NICKNAMEA               00034670
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-BIRTH-DATEA             00034680
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-COMP-IN-ADDA            00034690
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SEXA                    00034700
               MOVE ATTRB-PROT-ASKIP     TO MAP-COMP-NAMEA              00034710
               MOVE ATTRB-PROT-ASKIP     TO MAP-SSNA.                   00034720
                                                                        00034730
           IF (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'AA') OR  00034740
              (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'STA') OR 00034750
              (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'CD') OR  00034760
              (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'BR')     00034770
               MOVE 'AM' TO WT-C8997-SYSTEM-CD                          00034780
               MOVE WT-CNTL8997 TO TURBO-CNTL-AREA                      00034790
               PERFORM 10000-CALL-GSF                                   00034800
               MOVE TURBO-CNTL-AREA TO WT-CNTL8997                      00034810
               IF WT-C8997-RETURN = ZEROES                              00034820
                   MOVE WT-C8997-SYS-TYPE TO MAP-ENTITY-TYPEO           00034830
      ********     MOVE UNPROT-NORM-FSET  TO MAP-ENTITY-TYPEA           00034840
                   MOVE ATTRB-PROT-ASKIP-FSET TO MAP-ENTITY-TYPEA.      00034850
                                                                        00034860
           IF  COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'LB'      00034870
               MOVE 'LB' TO WT-C8997-SYSTEM-CD                          00034880
               MOVE WT-CNTL8997 TO TURBO-CNTL-AREA                      00034890
               PERFORM 10000-CALL-GSF                                   00034900
               MOVE TURBO-CNTL-AREA TO WT-CNTL8997                      00034910
               IF WT-C8997-RETURN = ZEROES                              00034920
                   MOVE WT-C8997-SYS-TYPE TO MAP-ENTITY-TYPEO           00034930
      ********     MOVE UNPROT-NORM-FSET  TO MAP-ENTITY-TYPEA           00034940
                   MOVE ATTRB-PROT-ASKIP-FSET TO MAP-ENTITY-TYPEA.      00034950
                                                                        00034960
           IF  COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'BA'      00034970
               MOVE 'BA' TO WT-C8997-SYSTEM-CD                          00034980
               MOVE WT-CNTL8997 TO TURBO-CNTL-AREA                      00034990
               PERFORM 10000-CALL-GSF                                   00035000
               MOVE TURBO-CNTL-AREA TO WT-CNTL8997                      00035010
               IF WT-C8997-RETURN = ZEROES                              00035020
                   MOVE WT-C8997-SYS-TYPE TO MAP-ENTITY-TYPEO           00035030
      ********     MOVE UNPROT-NORM-FSET  TO MAP-ENTITY-TYPEA           00035040
                   MOVE ATTRB-PROT-ASKIP-FSET TO MAP-ENTITY-TYPEA.      00035050
                                                                        00035060
           IF  COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'LD'      00035070
               MOVE 'LD' TO WT-C8997-SYSTEM-CD                          00035080
               MOVE WT-CNTL8997 TO TURBO-CNTL-AREA                      00035090
               PERFORM 10000-CALL-GSF                                   00035100
               MOVE TURBO-CNTL-AREA TO WT-CNTL8997                      00035110
               IF WT-C8997-RETURN = ZEROES                              00035120
                   MOVE WT-C8997-SYS-TYPE TO MAP-ENTITY-TYPEO           00035130
                   MOVE ATTRB-PROT-ASKIP-FSET TO MAP-ENTITY-TYPEA.      00035140
                                                                        00035150
           IF  COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'RP'      00035160
               MOVE 'RP' TO WT-C8997-SYSTEM-CD                          00035170
               MOVE WT-CNTL8997 TO TURBO-CNTL-AREA                      00035180
               PERFORM 10000-CALL-GSF                                   00035190
               MOVE TURBO-CNTL-AREA TO WT-CNTL8997                      00035200
               IF WT-C8997-RETURN = ZEROES                              00035210
                   MOVE WT-C8997-SYS-TYPE TO MAP-ENTITY-TYPEO           00035220
                   MOVE ATTRB-PROT-ASKIP-FSET TO MAP-ENTITY-TYPEA.      00035230
                                                                        00035240
           IF COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'A '       00035250
               MOVE 'ML' TO WT-C8997-SYSTEM-CD                          00035260
               MOVE WT-CNTL8997 TO TURBO-CNTL-AREA                      00035270
               PERFORM 10000-CALL-GSF                                   00035280
               MOVE TURBO-CNTL-AREA TO WT-CNTL8997                      00035290
               IF WT-C8997-RETURN = ZEROES                              00035300
                   MOVE WT-C8997-SYS-TYPE TO MAP-ENTITY-TYPEO           00035310
                   MOVE UNPROT-NORM-FSET  TO MAP-ENTITY-TYPEA.          00035320
                                                                        00035330
                                                                        00035340
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
           MOVE COM-IDNTITY                TO MAP-CIMO.                 00035740
           MOVE COM-FINALST-REAS-CODE  TO MAP-FINALST-CDO.              00035750
           MOVE WS-DEMO-USER-NAME      TO MAP-LOGONO.                   00035760
           MOVE COM-POSTAL-CODE        TO MAP-POSTAL-CDO.               00035770
           MOVE COM-COUNTRY-CODE       TO MAP-COUNTRY-CDO.              00035780
                                                                        00035790
           IF COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'RP'       00035800
              MOVE WS-IDNTITY-CHECK  TO  MAP-CIMO                       00035810
           END-IF.                                                      00035820
                                                                        00035830
           MOVE COM-RECORD-STATUS  TO WT-C2006-STATUS-CD.               00035840
           MOVE WT-CNTL2006        TO TURBO-CNTL-AREA.                  00035850
           PERFORM 10000-CALL-GSF.                                      00035860
           MOVE TURBO-CNTL-AREA    TO WT-CNTL2006.                      00035870
           IF  WT-C2006-RETURN EQUAL ZEROES                             00035880
               MOVE WT-C2006-STAT-DESC TO MAP-CUST-STATUSO              00035890
           ELSE                                                         00035900
               MOVE COM-RECORD-STATUS  TO MAP-CUST-STATUSO.             00035910
                                                                        00035920
           MOVE COM-SITE-CODE   TO WT-C0161-SITE.                       00035930
           MOVE WT-CNTL0161     TO TURBO-CNTL-AREA.                     00035940
           PERFORM 10000-CALL-GSF.                                      00035950
           MOVE TURBO-CNTL-AREA TO WT-CNTL0161.                         00035960
           IF  WT-C0161-RETURN EQUAL ZEROES                             00035970
               MOVE WT-C0161-COMPANY-NAME TO MAP-SITE-DESCO             00035980
           ELSE                                                         00035990
               MOVE SPACES                TO MAP-SITE-DESCO.            00036000
                                                                        00036010
           MOVE COM-ENTITY-TYPE TO WT-C8997-SYSTEM-CD.                  00036020
           MOVE WT-CNTL8997     TO TURBO-CNTL-AREA.                     00036030
           PERFORM 10000-CALL-GSF.                                      00036040
           MOVE TURBO-CNTL-AREA TO WT-CNTL8997.                         00036050
           IF  WT-C8997-RETURN EQUAL ZEROES                             00036060
               MOVE WT-C8997-SYS-TYPE TO MAP-ENTITY-TYPEO               00036070
           ELSE                                                         00036080
               MOVE COM-ENTITY-TYPE   TO MAP-ENTITY-TYPEO.              00036090
                                                                        00036100
           PERFORM 0290-FORMAT-DISPLAY-NAME.                            00036110
                                                                        00036120
      *                                                                 00036130
      * IMR CHANGE IMR7 BEGIN                                           00036140
      *                                                                 00036150
      *    MOVE COM-BIRTH-DATE TO WS-DATE-R.                            00036160
      *    MOVE WS-DATE-R      TO WS-DATE.                              00036170
      *    MOVE WS-MM          TO WS-MONTH.                             00036180
      *    MOVE WS-DD          TO WS-DAY.                               00036190
      *    MOVE WS-YY          TO WS-YEAR.                              00036200
      *    MOVE '/'            TO SLASH-1 SLASH-2.                      00036210
      *    MOVE WS-DATE-EIGHT  TO MAP-BIRTH-DATEO.                      00036220
      *                                                                 00036230
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
           DISPLAY 'COM EFF DATE:' COM-EFFECTIVE-DATE
           MOVE COM-EFFECTIVE-DATE TO WS-DATE-R.                        00036430
           MOVE WS-DATE-R          TO WS-DATE.                          00036440
           MOVE WS-MM              TO WS-MONTH.                         00036450
           MOVE WS-DD              TO WS-DAY.                           00036460
           MOVE WS-YY              TO WS-YEAR.                          00036470
           MOVE '/'                TO SLASH-1 SLASH-2.                  00036480
           MOVE WS-DATE-EIGHT      TO MAP-EFF-DATEO.                    00036490
           DISPLAY 'MAP EFF DATEO:' MAP-EFF-DATEO
                                                                        00036500
           INSPECT COMM-ACTION-CODE REPLACING ALL '_' BY SPACE.         00036510
                                                                        00036520
           IF COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'STA'      00036530
           OR COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'CD'       00036540
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-LITERAL6A               00036550
               MAP-LITERAL7A MAP-LITERAL9A MAP-LITERAL10A               00036560
               MAP-LITERAL5A MAP-LITERAL8A MAP-LITERAL4A                00036570
               MOVE SPACES  TO MAP-SUFFIX1O                             00036580
               MOVE SPACES  TO MAP-SUFFIX2O                             00036590
               MOVE SPACES  TO MAP-NICKNAMEO                            00036600
               MOVE SPACES  TO MAP-BIRTH-DATEO                          00036610
               MOVE SPACES  TO MAP-COMP-IN-ADDO                         00036620
               MOVE SPACES  TO MAP-SEXO                                 00036630
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SUFFIX1A                00036640
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SUFFIX2A                00036650
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-NICKNAMEA               00036660
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-BIRTH-DATEA             00036670
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-COMP-IN-ADDA            00036680
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SEXA                    00036690
               MOVE ATTRB-PROT-ASKIP     TO MAP-COMP-NAMEA              00036700
               MOVE ATTRB-PROT-ASKIP     TO MAP-SSNA                    00036710
           ELSE                                                         00036720
           IF (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'LB')     00036730
           OR (COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'BA')     00036740
           OR (COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'LD')     00036750
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-LITERAL1A MAP-LITERAL2A 00036760
               MAP-LITERAL3A MAP-LITERAL4A MAP-LITERAL6A                00036770
               MAP-LITERAL7A MAP-LITERAL9A MAP-LITERAL10A               00036780
               MAP-LITERAL5A MAP-LITERAL8A                              00036790
               MOVE SPACES  TO MAP-FIRST-NAMEO                          00036800
               MOVE SPACES  TO MAP-MIDDLE-NAMEO                         00036810
               MOVE SPACES  TO MAP-LAST-NAMEO                           00036820
               MOVE SPACES  TO MAP-SUFFIX1O                             00036830
               MOVE SPACES  TO MAP-SUFFIX2O                             00036840
               MOVE SPACES  TO MAP-NICKNAMEO                            00036850
               MOVE SPACES  TO MAP-BIRTH-DATEO                          00036860
               MOVE SPACES  TO MAP-COMP-IN-ADDO                         00036870
               MOVE SPACES  TO MAP-SEXO                                 00036880
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-FIRST-NAMEA             00036890
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-MIDDLE-NAMEA            00036900
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-LAST-NAMEA              00036910
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SUFFIX1A                00036920
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SUFFIX2A                00036930
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-NICKNAMEA               00036940
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-BIRTH-DATEA             00036950
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-COMP-IN-ADDA            00036960
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SEXA                    00036970
           ELSE                                                         00036980
           IF (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'AA') OR  00036990
              (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'BR') OR  00037000
              (COM-ENTITY-TYPE = 'AM' OR 'CA')                          00037010
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-LITERAL1A MAP-LITERAL2A 00037020
               MAP-LITERAL3A MAP-LITERAL4A MAP-LITERAL6A                00037030
               MAP-LITERAL7A MAP-LITERAL9A MAP-LITERAL10A               00037040
               MAP-LITERAL5A MAP-LITERAL8A                              00037050
               MOVE SPACES  TO MAP-FIRST-NAMEO                          00037060
               MOVE SPACES  TO MAP-MIDDLE-NAMEO                         00037070
               MOVE SPACES  TO MAP-LAST-NAMEO                           00037080
               MOVE SPACES  TO MAP-SUFFIX1O                             00037090
               MOVE SPACES  TO MAP-SUFFIX2O                             00037100
               MOVE SPACES  TO MAP-NICKNAMEO                            00037110
               MOVE SPACES  TO MAP-BIRTH-DATEO                          00037120
               MOVE SPACES  TO MAP-COMP-IN-ADDO                         00037130
               MOVE SPACES  TO MAP-SEXO                                 00037140
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-FIRST-NAMEA             00037150
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-MIDDLE-NAMEA            00037160
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-LAST-NAMEA              00037170
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SUFFIX1A                00037180
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SUFFIX2A                00037190
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-NICKNAMEA               00037200
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-BIRTH-DATEA             00037210
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-COMP-IN-ADDA            00037220
               MOVE ATTRB-PROT-ASKIP-DRK TO MAP-SEXA.                   00037230
                                                                        00037240
           IF (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'AA') OR  00037250
              (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'STA') OR 00037260
              (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'CD') OR  00037270
              (COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'BR')     00037280
              IF COM-ENTITY-TYPE = 'AM' OR 'CA'                         00037290
                 MOVE ATTRB-PROT-ASKIP TO MAP-ENTITY-TYPEA.             00037300
                                                                        00037310
           IF  COMM-SYSTEM-CODE = 'AM' AND COMM-ACTION-CODE = 'LB'      00037320
              IF COM-ENTITY-TYPE = 'LB'                                 00037330
                 MOVE ATTRB-PROT-ASKIP TO MAP-ENTITY-TYPEA.             00037340
                                                                        00037350
           IF  COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'BA'      00037360
              IF COM-ENTITY-TYPE = 'BA'                                 00037370
                 MOVE ATTRB-PROT-ASKIP TO MAP-ENTITY-TYPEA.             00037380
                                                                        00037390
           IF  COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'LD'      00037400
              IF COM-ENTITY-TYPE = 'LD'                                 00037410
                 MOVE ATTRB-PROT-ASKIP TO MAP-ENTITY-TYPEA              00037420
              END-IF                                                    00037430
           END-IF.                                                      00037440
                                                                        00037450
           IF  COMM-SYSTEM-CODE = 'NA' AND COMM-ACTION-CODE = 'RP'      00037460
              IF COM-ENTITY-TYPE = 'RP'                                 00037470
                 MOVE ATTRB-PROT-ASKIP TO MAP-ENTITY-TYPEA              00037480
              END-IF                                                    00037490
           END-IF.                                                      00037500
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
                                                                        00038200
                                                                        00038210
       0300-INITIATE-ROUTER.                                            00038220
                                                                        00038230
           MOVE '00' TO COMM-NEXT-FUNCTION.                             00038240
           MOVE ZERO TO COMM-CURSOR-POSN     COMM-MSG-COUNT             00038250
                        COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.     00038260
      *---------------------------------------------------------------* 00038270
      * NOT FOUND ERROR MESSAGE ON DB2 LOOKUP                         * 00038280
      *---------------------------------------------------------------* 00038290
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED  = 'Y'     00038300
               MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)           00038310
                              COMM-MSG-ID (3) COMM-MSG-ID (4)           00038320
               MOVE 'NAA0' TO COMM-PREVIOUS-TRANID                      00038330
           ELSE                                                         00038340
              IF SQLCODE = 100                                          00038350
                  MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1)             00038360
                  MOVE 'E' TO COMM-MSG-MAX-SEVERITY                     00038370
              ELSE                                                      00038380
                  MOVE 'NAA0' TO COMM-PREVIOUS-TRANID.                  00038390
                                                                        00038400
           MOVE 'NAQ1'          TO WS-TS-QUEUE-TRANID.                  00038410
           MOVE EIBTRMID        TO WS-TS-QUEUE-TERMID.                  00038420
                                                                        00038430
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00038440
               MOVE NA-COMMAREA TO WS-LINK-SIX-HUNDRED                  00038450
               EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)            00038460
                                   FROM   (WS-LINK-STORAGE)             00038470
                                   LENGTH (WS-ZERO-LENGTH)              00038480
                                   ITEM   (WS-TS-ITEM)                  00038490
                                   RESP   (WS-CICS-RESP)                00038500
                                   REWRITE                              00038510
                                   MAIN                                 00038520
                                   END-EXEC                             00038530
           ELSE                                                         00038540
               MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS               00038550
               MOVE COM-AGNTNAME       TO WSQ-AGNTNAME                  00038560
               MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH        00038570
               EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)            00038580
                                   FROM   (WSQ-COMMAREA)                00038590
                                   LENGTH (WS-COMM-DB2-LENGTH)          00038600
                                   ITEM   (WS-TS-ITEM)                  00038610
                                   RESP   (WS-CICS-RESP)                00038620
                                   REWRITE                              00038630
                                   MAIN                                 00038640
                                   END-EXEC.                            00038650
                                                                        00038660
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00038670
                GO TO 9999-CICS-ERROR.                                  00038680
                                                                        00038690
MANJU *    EXEC CICS SEND MAP ('SYSBUSY')                               00038700
      *                   RESP (WS-CICS-RESP)                           00038710
      *                   END-EXEC.                                     00038720
                                                                        00038730
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00038740
      *         GO TO 9999-CICS-ERROR.                                  00038750
                                                                        00038760
           EXEC CICS START TRANSID('NAA0')                              00038770
                           TERMID (EIBTRMID)                            00038780
                           RESP   (WS-CICS-RESP)                        00038790
                           END-EXEC.                                    00038800
                                                                        00038810
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00038820
                GO TO 9999-CICS-ERROR.                                  00038830
                                                                        00038840
           EXEC CICS RETURN                                             00038850
                     END-EXEC.                                          00038860
                                                                        00038870
                                                                        00038880
      *---------------------------------------------------------------* 00038890
      *   THE FOLLOWING CODE EXECUTES THE PSI TUTORIAL OR ONLINE HELP * 00038900
      *   FUNCTION  IT STORES ANY CHANGES MADE ON THE MAP IN A        * 00038910
      *   TEMPORARY STORAGE QUEUE, THEN IT INITIATES THE TUTORIAL     * 00038920
      *   COMMAREA AND XCNTL'S TO THE TUTORIAL PROGRAM                * 00038930
      *---------------------------------------------------------------* 00038940
                                                                        00038950
      *0310-PROCESS-TUTORIAL-REQUEST.                                   00038960
                                                                        00038970
      *    MOVE 'NA330M1' TO T3-SCREEN-NAME                             00038980
      *    MOVE 'Y'       TO T3-COMMAND-LINE                            00038990
      *    MOVE SPACES    TO T3-CARRIER                                 00039000
      *    EXEC CICS LINK PROGRAM  ('TU003')                            00039010
      *                   COMMAREA (T3-COMMAREA)                        00039020
      *                   LENGTH   (LENGTH OF T3-COMMAREA)              00039030
      *                   END-EXEC.                                     00039040
                                                                        00039050
      *    EXEC CICS RETURN TRANSID ('NAA0') END-EXEC.                  00039060
                                                                        00039070
      *---------------------------------------------------------------* 00039080
      *    CODE NO LONGER NEEDED FOR VER. 3.1 TUTORIAL                * 00039090
      *---------------------------------------------------------------* 00039100
      *    EXEC CICS RECEIVE MAP    ('NA330M1')                       * 00039110
      *                      RESP   (WS-CICS-RESP)                    * 00039120
      *                      END-EXEC.                                * 00039130
      *                                                               * 00039140
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                  * 00039150
      *         WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                   * 00039160
      *             GO TO 9999-CICS-ERROR.                            * 00039170
      *                                                               * 00039180
      *    PERFORM 0110-UPDATE-COMMAND-LINE.                          * 00039190
      *    PERFORM 0120-UPDATE-ID-MAP-FIELDS.                         * 00039200
      *                                                               * 00039210
      *    MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.             * 00039220
      *    MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.             * 00039230
      *                                                               * 00039240
      *    MOVE 'NA330M1'          TO TC-SCREEN-NAME.                 * 00039250
      *    MOVE 'NAA0'             TO TC-TRANSACTION-ID.              * 00039260
      *    MOVE  1                 TO TC-PAGE-NUMBER.                 * 00039270
      *    MOVE 'Y'                TO TC-COMMAND-LINE.                * 00039280
      *    MOVE 'NA330'            TO TC-PROGRAM-NAME.                * 00039290
      *    MOVE EIBCPOSN           TO TC-CURSOR-POSITION              * 00039300
      *                               COMM-CURSOR-POSN.               * 00039310
      *    MOVE SPACE              TO TC-KEY-DATA.                    * 00039320
      *    MOVE COMM-MSG-ID(1)     TO TC-MESSAGE(1).                  * 00039330
      *    MOVE COMM-MSG-ID(2)     TO TC-MESSAGE(2).                  * 00039340
      *    MOVE COMM-MSG-ID(3)     TO TC-MESSAGE(3).                  * 00039350
      *    MOVE COMM-MSG-ID(4)     TO TC-MESSAGE(4).                  * 00039360
      *                                                               * 00039370
      *                                                               * 00039380
      *    IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED = 'Y'    * 00039390
      *        MOVE 'Y' TO COMM-KEY-CHANGED                           * 00039400
      *        MOVE 'Y' TO COMM-CMDLINE-CHANGED.                      * 00039410
      *                                                               * 00039420
      *                                                               * 00039430
      *    MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                * 00039440
      *    MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                   * 00039450
      *    MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.         * 00039460
      *    EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)              * 00039470
      *                        FROM   (WSQ-COMMAREA)                  * 00039480
      *                        LENGTH (WS-COMM-DB2-LENGTH)            * 00039490
      *                        ITEM   (WS-TS-ITEM)                    * 00039500
      *                        RESP   (WS-CICS-RESP)                  * 00039510
      *                        REWRITE                                * 00039520
      *                        MAIN                                   * 00039530
      *                        END-EXEC.                              * 00039540
      *                                                               * 00039550
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                      * 00039560
      *         GO TO 9999-CICS-ERROR.                                * 00039570
      *                                                               * 00039580
      *    EXEC CICS XCTL PROGRAM  ('TU001')                          * 00039590
      *                   COMMAREA (TUTORIAL-COMM-AREA)               * 00039600
      *                   LENGTH   (WS-TUTOR-COMM-LENGTH)             * 00039610
      *                   END-EXEC.                                   * 00039620
      *                                                               * 00039630
      *                                                               * 00039640
      *---------------------------------------------------------------* 00039650
      *  CONTROL WAS RETURNED FROM THE TUTORIAL PROGRAM. READ THE     * 00039660
      *  'HELP' TS QUEUE AND DISPLAY THE MAP AS IT APPEARED PRIOR     * 00039670
      *  TO GOING TO THE HELP FACILITY.                               * 00039680
      *---------------------------------------------------------------* 00039690
      *                                                               * 00039700
      *0320-RETURN-FROM-TUTORIAL.                                     * 00039710
      *                                                               * 00039720
      *    MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.         * 00039730
      *    MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.         * 00039740
      *    IF  EIBAID = DFHCLEAR                                      * 00039750
      *        PERFORM 0330-PROCESS-SCREEN-CLEAR.                     * 00039760
      *                                                               * 00039770
      *                                                               * 00039780
      *    IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                 * 00039790
      *        MOVE ALL '_' TO MAP-SYSTEM-CDO                         * 00039800
      *    ELSE                                                       * 00039810
      *       MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.              * 00039820
      *    IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                 * 00039830
      *        MOVE ALL '_' TO MAP-ACTION-CDO                         * 00039840
      *    ELSE                                                       * 00039850
      *       MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.              * 00039860
      *    IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES               * 00039870
      *        MOVE ALL '_' TO MAP-CUST-INFOO                         * 00039880
      *    ELSE                                                       * 00039890
      *       MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.              * 00039900
      *                                                               * 00039910
      *    MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                * 00039920
      *    MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                   * 00039930
      *    MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.         * 00039940
      *    EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)              * 00039950
      *                        FROM   (WSQ-COMMAREA)                  * 00039960
      *                        LENGTH (WS-COMM-DB2-LENGTH)            * 00039970
      *                        ITEM   (WS-TS-ITEM)                    * 00039980
      *                        RESP   (WS-CICS-RESP)                  * 00039990
      *                        REWRITE                                * 00040000
      *                        MAIN                                   * 00040010
      *                        END-EXEC.                              * 00040020
      *                                                               * 00040030
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                      * 00040040
      *         GO TO 9999-CICS-ERROR.                                * 00040050
      *                                                               * 00040060
      *    PERFORM 0380-CHECK-ERROR-MESSAGES                          * 00040070
      *        VARYING ACTION-SUB FROM 1 BY 1                         * 00040080
      *            UNTIL ACTION-SUB > 4.                              * 00040090
      *                                                               * 00040100
      *    PERFORM 0280-MOVE-OUT-MAP-FIELDS.                          * 00040110
      *                                                               * 00040120
      *    EXEC CICS SEND MAP    ('NA330M1')                          * 00040130
      *                   CURSOR (COMM-CURSOR-POSN)                   * 00040140
      *                   RESP   (WS-CICS-RESP)                       * 00040150
      *                   ERASE                                       * 00040160
      *                   END-EXEC.                                   * 00040170
      *                                                               * 00040180
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                      * 00040190
      *         GO TO 9999-CICS-ERROR.                                * 00040200
      *                                                               * 00040210
      *    EXEC CICS RETURN TRANSID ('NAA0')                          * 00040220
      *                     END-EXEC.                                 * 00040230
      *                                                               * 00040240
      *    GOBACK.                                                    * 00040250
      *                                                               * 00040260
      *---------------------------------------------------------------* 00040270
                                                                        00040280
       0330-PROCESS-SCREEN-CLEAR.                                       00040290
                                                                        00040300
           MOVE 'NAA0' TO COMM-PREVIOUS-TRANID.                         00040310
           MOVE '00'   TO COMM-NEXT-FUNCTION.                           00040320
           MOVE ZERO   TO COMM-CURSOR-POSN     COMM-MSG-COUNT           00040330
                          COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.   00040340
           MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                        00040350
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00040360
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00040370
                                                                        00040380
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00040390
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00040400
                                                                        00040410
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00040420
                               FROM   (WS-TS-QUEUE)                     00040430
                               LENGTH (WS-ZERO-LENGTH)                  00040440
                               ITEM   (WS-TS-ITEM)                      00040450
                               RESP   (WS-CICS-RESP)                    00040460
                               REWRITE                                  00040470
                               MAIN                                     00040480
                               END-EXEC.                                00040490
                                                                        00040500
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00040510
                GO TO 9999-CICS-ERROR.                                  00040520
                                                                        00040530
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
                                                                        00040660
                                                                        00040670
       0340-WRITE-NAQ1-QUEUE.                                           00040680
                                                                        00040690
           MOVE SPACES TO WSQ-COMMAREA.                                 00040700
           MOVE ZERO   TO WSQ-CURSOR-POSN     WSQ-MSG-COUNT             00040710
                          WSQ-MAIL-LINE-COUNT WSQ-CUST-TOKEN-COUNT.     00040720
           MOVE '00'   TO WSQ-NEXT-FUNCTION.                            00040730
                                                                        00040740
           MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.           00040750
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00040760
                               FROM   (WSQ-COMMAREA)                    00040770
                               LENGTH (WS-COMM-DB2-LENGTH)              00040780
                               ITEM   (WS-TS-ITEM)                      00040790
                               RESP   (WS-CICS-RESP)                    00040800
                               MAIN                                     00040810
                               END-EXEC.                                00040820
                                                                        00040830
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00040840
                GO TO 9999-CICS-ERROR.                                  00040850
                                                                        00040860
           MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.           00040870
           MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.           00040880
           EXEC CICS READQ TS QUEUE  (WS-TS-QUEUE-NAME)                 00040890
                              SET    (ADDRESS OF WS-LINK-STORAGE)       00040900
                              LENGTH (WS-ZERO-LENGTH)                   00040910
                              ITEM   (WS-TS-ITEM)                       00040920
                              RESP   (WS-CICS-RESP)                     00040930
                              END-EXEC.                                 00040940
                                                                        00040950
                                                                        00040960
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00040970
                GO TO 9999-CICS-ERROR.                                  00040980
                                                                        00040990
           MOVE WS-ZERO-LENGTH TO WS-ZERO-LENGTH.                       00041000
           COMPUTE WS-LINK-LENGTH = (WS-ZERO-LENGTH - +600).            00041010
           MOVE WS-LINK-STORAGE TO WS-TS-QUEUE.                         00041020
                                                                        00041030
       0350-WRONG-KEY-HIT.                                              00041040
                                                                        00041050
           MOVE LOW-VALUES TO NA330M1I.                                 00041060
                                                                        00041070
           EXEC CICS RECEIVE MAP    ('NA330M1')                         00041080
                             RESP   (WS-CICS-RESP)                      00041090
                             END-EXEC.                                  00041100
                                                                        00041110
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00041120
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00041130
                GO TO 9999-CICS-ERROR.                                  00041140
                                                                        00041150
           PERFORM 0110-UPDATE-COMMAND-LINE.                            00041160
           PERFORM 0120-UPDATE-ID-MAP-FIELDS.                           00041170
                                                                        00041180
      *---------------------------------------------------------------* 00041190
      * THIS CODE KEEPS THE UPDATE MESSAGE BR152 FROM BEING DISPLAYED * 00041200
      *  IF AN UPDATE HAD BEEN COMMPLETED PRIOR AND THEN THE USER     * 00041210
      *  HIST AN INVALID KEY.                                         * 00041220
      *---------------------------------------------------------------* 00041230
                                                                        00041240
           IF COMM-MSG-ID (1) = 'NA153'                                 00041250
               MOVE SPACES TO COMM-MSG-ID (1)                           00041260
           ELSE                                                         00041270
           IF COMM-MSG-ID (2) = 'NA153'                                 00041280
               MOVE SPACES TO COMM-MSG-ID (2)                           00041290
           ELSE                                                         00041300
           IF COMM-MSG-ID (3) = 'NA153'                                 00041310
               MOVE SPACES TO COMM-MSG-ID (3)                           00041320
           ELSE                                                         00041330
           IF COMM-MSG-ID (4) = 'NA153'                                 00041340
               MOVE SPACES TO COMM-MSG-ID (4).                          00041350
                                                                        00041360
           MOVE 'NA030'           TO WS-MESSAGE-NUMBER1.                00041370
           PERFORM 0370-BUMP-ERROR-MESSAGES.                            00041380
           MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(1).                    00041390
           MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(2).                    00041400
           MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(3).                    00041410
           MOVE WSQ-MSG-ID(4)     TO COMM-MSG-ID(4).                    00041420
                                                                        00041430
           MOVE EIBCPOSN           TO COMM-CURSOR-POSN.                 00041440
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00041450
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00041460
                                                                        00041470
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED = 'Y'      00041480
               MOVE 'Y' TO COMM-KEY-CHANGED                             00041490
               MOVE 'Y' TO COMM-CMDLINE-CHANGED.                        00041500
                                                                        00041510
           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                  00041520
           MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                     00041530
           MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.           00041540
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00041550
                               FROM   (WSQ-COMMAREA)                    00041560
                               LENGTH (WS-COMM-DB2-LENGTH)              00041570
                               ITEM   (WS-TS-ITEM)                      00041580
                               RESP   (WS-CICS-RESP)                    00041590
                               REWRITE                                  00041600
                               MAIN                                     00041610
                               END-EXEC.                                00041620
                                                                        00041630
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00041640
                GO TO 9999-CICS-ERROR.                                  00041650
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
                                                                        00041790
                                                                        00041800
           PERFORM 0280-MOVE-OUT-MAP-FIELDS.                            00041810
                                                                        00041820
           EXEC CICS SEND MAP    ('NA330M1')                            00041830
                          CURSOR (COMM-CURSOR-POSN)                     00041840
                          RESP   (WS-CICS-RESP)                         00041850
                          ERASE                                         00041860
                          END-EXEC.                                     00041870
                                                                        00041880
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00041890
                GO TO 9999-CICS-ERROR.                                  00041900
                                                                        00041910
           EXEC CICS RETURN TRANSID ('NAA0')                            00041920
                            END-EXEC.                                   00041930
                                                                        00041940
       0360-RETURN-TO-MT-BILLION.                                       00041950
                                                                        00041960
           MOVE 'NAA0' TO COMM-PREVIOUS-TRANID.                         00041970
           MOVE '00'   TO COMM-NEXT-FUNCTION.                           00041980
           MOVE ZERO   TO COMM-CURSOR-POSN     COMM-MSG-COUNT           00041990
                          COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.   00042000
           MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                        00042010
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00042020
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00042030
                                                                        00042040
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00042050
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00042060
                                                                        00042070
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00042080
                               FROM   (WS-TS-QUEUE)                     00042090
                               LENGTH (WS-ZERO-LENGTH)                  00042100
                               ITEM   (WS-TS-ITEM)                      00042110
                               RESP   (WS-CICS-RESP)                    00042120
                               REWRITE                                  00042130
                               MAIN                                     00042140
                               END-EXEC.                                00042150
                                                                        00042160
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00042170
                GO TO 9999-CICS-ERROR.                                  00042180
                                                                        00042190
      *    EXEC CICS SEND MAP  ('SYSBUSY')                              00042200
      *                   RESP (WS-CICS-RESP)                           00042210
      *                   END-EXEC.                                     00042220
      *                                                                 00042230
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00042240
      *         GO TO 9999-CICS-ERROR.                                  00042250
      *                                                                 00042260
      *    EXEC CICS START TRANSID('PA3')                               00042270
      *                    RESP (WS-CICS-RESP)                          00042280
      *                    TERMID (EIBTRMID)                            00042290
      *                    END-EXEC.                                    00042300
      *                                                                 00042310
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00042320
      *         GO TO 9999-CICS-ERROR.                                  00042330
      *                                                                 00042340
           EXEC CICS RETURN                                             00042350
                     END-EXEC.                                          00042360
                                                                        00042370
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
       0405-CHK-VALID-CUSTOMER.                                         00042830
      **********************************                                00042840
      *   LINK TO SECURITY MODULE                                       00042850
      **********************************                                00042860
           MOVE 'DEVSRV01'         TO SEC-RESOURCE-NAME.                00042870
           MOVE 'Y'                TO SEC-CHK-RESOURCE.                 00042880
           MOVE SPACE              TO SEC-RETURN-CODE.                  00042890
           MOVE 'U'                TO SEC-FUNCTION-CODE.                00042900
                                                                        00042910
           EXEC CICS LINK                                               00042920
                     PROGRAM('SECCHECK')                                00042930
                     COMMAREA(SECURITY-COMM-AREA)                       00042940
      *              LENGTH(31)                                         00042950
                     LENGTH(LENGTH OF SECURITY-COMM-AREA)               00042960
           END-EXEC.                                                    00042970
                                                                        00042980
           IF  SEC-RETURN-CODE = 'A'                                    00042990
               GO TO 0405-EXIT.                                         00043000
                                                                        00043010
           MOVE 100 TO SQLCODE.                                         00043020
           MOVE 'SC002' TO WS-MESSAGE-NUMBER1.                          00043030
           GO TO 0300-INITIATE-ROUTER.                                  00043040
                                                                        00043050
       0405-EXIT. EXIT.                                                 00043060
           EJECT                                                        00043070
                                                                        00043080
                                                                        00043090
       0410-DB2-ERROR.                                                  00043100
MANJU5     MOVE SQLCODE TO WS-DB2I-MESSAGE
MANJU5     MOVE WS-DB2I-MESSAGE TO WS-C9999-ERROR-CODE.
MANJU5     DISPLAY WS-DB2I-MESSAGE WS-C9999-ERROR-CODE.
           MOVE "DB2 SQL ERROR" TO WS-C9999-ERROR-MESSAGE.
           MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (1).
           PERFORM 0280-MOVE-OUT-MAP-FIELDS.
           EXEC CICS SEND MAP    ('NA330M1')
                          CURSOR
                          ERASE
                          RESP (WS-CICS-RESP)
                          END-EXEC.
           EXEC CICS RETURN TRANSID ('NAA0')
                     END-EXEC.
           EXEC CICS RETURN
                     END-EXEC.
      *---------------------------------------------------------------* 00043120
      * THIS ROUTINE TRANSFERS CONTROL TO AN ONLINE ERROR PROGRAM     * 00043130
      * WHICH DISPLAYS THE ERRORS ON THE SCREEN SUPPLYING INFORMATION * 00043140
      * FOR DB2 ERRORS                                                * 00043150
      *---------------------------------------------------------------* 00043160
      *    EXEC CICS XCTL PROGRAM('SQLERRTN')                           00043170
      *                   COMMAREA (SQLCA)                              00043180
      *                   LENGTH (214)                                  00043190
      *                   LENGTH (LENGTH OF SQLCA)                      00043200
      *                   END-EXEC.                                     00043210
                                                                        00043220
      *---------------------------------------------------------------* 00043230
      * THIS ROUTINE TRANSFERS CONTROL TO AN ONLINE ERROR PROGRAM     * 00043240
      * WHICH DISPLAYS THE ERRORS ON THE SCREEN SUPPLYING INFORMATION * 00043250
      * FOR THE HELP-DESK  TO AID PROGRAMMERS IN DEBUGGING            * 00043260
      *---------------------------------------------------------------* 00043270
                                                                        00043280
           COPY CICSERR.                                                00043290
                                                                        00043300
       9999-EXIT. EXIT.                                                 00043310
      *---------------------------------------------------------------* 00043320
      * THIS ROUTINE TRANSFERS CONTROL TO THE TURBO TABLES FOR EDIT   * 00043330
      * VERIFICATION                                                  * 00043340
      *---------------------------------------------------------------* 00043350
                                                                        00043360
           COPY TURBOCAL.                                               00043370
