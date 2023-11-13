   CBL DATA(24)                                                         00000010
       IDENTIFICATION DIVISION.                                         00000020
       PROGRAM-ID. RQ09999.                                             00000030
        DATE-WRITTEN. 07/14/98.                                          0000028
       AUTHOR. MARY SHERMAN.                                            00000290
       INSTALLATION.                                                    00000300
      *DATE-COMPILED.                                                   00000310
      *REMARKS.                                                         00000320
       ENVIRONMENT DIVISION.                                            00001060
       CONFIGURATION SECTION.                                           00001070
       INPUT-OUTPUT SECTION.                                            00001080
       FILE-CONTROL.                                                    00001090
       DATA DIVISION.                                                   00001100
       FILE SECTION.                                                    00001110
       WORKING-STORAGE SECTION.                                         00001120
           COPY AUDCICS.                                                00001130
       01  PRG-ID.                                                      00001140
           05  FILLER              PIC X(15) VALUE 'WORKING STORAGE'.   00001150
           05  FILLER              PIC X(11) VALUE 'FOR RQ09999'.       00001160
           EJECT                                                        00001170
R00946 01  WS-PSIGSFC                       PIC X(08) VALUE 'PSIGSFC '. 00001180
Y2KIMR*                                                                 00001190
Y2KIMR* IMR CHANGE BEGIN                                                00001200
Y2KIMR*                                                                 00001210
Y2KIMR 01  Y2K-WK-CUTOFF-YR-9               PIC 9(2) VALUE 50.          00001220
Y2KIMR*                                                                 00001230
Y2KIMR 01  Y2K-WK-DATE1-9.                                              00001240
Y2KIMR     05  Y2K-WK-DATE1-9-CC            PIC 9(2).                   00001250
Y2KIMR     05  Y2K-WK-DATE1-9-YYMMDD        PIC 9(6).                   00001260
Y2KIMR     05  Y2K-WK-DATE1R-9-YYMMDD REDEFINES Y2K-WK-DATE1-9-YYMMDD.  00001270
Y2KIMR         10  Y2K-WK-DATE1R-9-YY       PIC 9(2).                   00001280
Y2KIMR         10  Y2K-WK-DATE1R-9-MM       PIC 9(2).                   00001290
Y2KIMR         10  Y2K-WK-DATE1R-9-DD       PIC 9(2).                   00001300
Y2KIMR*                                                                 00001310
Y2KIMR 01  Y2K-WK-DATE1RR-9 REDEFINES Y2K-WK-DATE1-9.                   00001320
Y2KIMR     05  Y2K-WK-DATE1RR-9-CCYY        PIC 9(4).                   00001330
Y2KIMR     05  Y2K-WK-DATE1RR-9-MMDD        PIC 9(4).                   00001340
Y2KIMR*                                                                 00001350
Y2KIMR 01  Y2K-WK-DATE1RRR-9 REDEFINES Y2K-WK-DATE1-9 PIC 9(8).         00001360
Y2KIMR*                                                                 00001370
Y2KIMR* IMR CHANGE END                                                  00001380
Y2KIMR*                                                                 00001390
      *---------------------------------------------------------------* 00001400
      *    TURBOINC IS USED FOR TURBO FILE ACCESS REQUESTS.           * 00001410
      *---------------------------------------------------------------* 00001420
           COPY TURBINC.                                                00001430
           EJECT                                                        00001440
      *---------------------------------------------------------------* 00001450
      *    TU003COM IS USED BY TUTORIAL PROGRAM.                      * 00001460
      *---------------------------------------------------------------* 00001470
           COPY TU003COM.                                               00001480
           EJECT                                                        00001490
      *---------------------------------------------------------------* 00001500
      *    CPWSINC IS USED BY CARRIER PROFITABILITY CODE, CPPRCINC.   * 00001510
      *---------------------------------------------------------------* 00001520
           COPY CAWSINC.                                                00001530
           EJECT                                                        00001540
           COPY DFHAID.                                                 00001550
           EJECT                                                        00001560
      *---------------------------------------------------------------* 00001570
      *    CICSWS USED BY CICSERR ROUTINE.                            * 00001580
      *---------------------------------------------------------------* 00001590
           COPY CICSWS.                                                 00001600
           EJECT                                                        00001610
           COPY DEMOCOMM.                                               00001620
           EJECT                                                        00001630
           COPY TURB0160.                                               00001640
           EJECT                                                        00001650
930318     COPY TURB3002.                                               00001660
           EJECT                                                        00001670
920258     COPY TURB0010.                                               00001680
920258     EJECT                                                        00001690
920435     COPY TURB0020.                                               00001700
920435     EJECT                                                        00001710
           COPY TURB9999.                                               00001720
           EJECT                                                        00001730
           COPY TURBDATA.                                               00001740
           EJECT                                                        00001750
R00946     COPY BLMDC01.                                                00001760
           EJECT                                                        00001770
           EJECT                                                        00001780
           COPY DFHBMSCA.                                               00001790
           EJECT                                                        00001800
           COPY SYSBUSY.                                                00001810
           EJECT                                                        00001820
           COPY EI100C01.                                               00001830
           EJECT                                                        00001840
                                                                        00001850
       01  CASE-RECORD.                                                 00001860
           COPY CASECOBQ.                                               00001870
                                                                        00001880
       01  SECURITY-AREA.                                               00001890
           COPY SECCOMMC.                                               00001900
                                                                        00001910
Y2KIMR*                                                                 00001920
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE W/S REFERENCES BEGIN              00001930
Y2KIMR*                                                                 00001940
Y2KIMR*01  WS-DATEPROG-PARAMETERS.                                      00001950
Y2KIMR*    COPY DATEPARM.                                               00001960
Y2KIMR*                                                                 00001970
Y2KIMR 01  WS-DATEPROG-PARAMETERS.                                      00001980
Y2KIMR     COPY DTEPARM2.                                               00001990
Y2KIMR*                                                                 00002000
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE W/S REFERENCES END                00002010
Y2KIMR*                                                                 00002020
                                                                        00002030
       01  WS-COMMAREA-STORAGE.                                         00002040
               05  WS-DATE1                      PIC X(10).             00002050
               05  FILLER REDEFINES WS-DATE1.                           00002060
                   10  WS-DATE1-CC               PIC XX.                00002070
                   10  WS-DATE1-YY               PIC XX.                00002080
                   10  WS-DATE1-H1               PIC X.                 00002090
                   10  WS-DATE1-MM               PIC XX.                00002100
                   10  WS-DATE1-H2               PIC X.                 00002110
                   10  WS-DATE1-DD               PIC XX.                00002120
               05  WS-DATE2                      PIC X(10).             00002130
               05  FILLER REDEFINES WS-DATE2.                           00002140
                   10  WS-DATE2-CC               PIC XX.                00002150
                   10  WS-DATE2-YY               PIC XX.                00002160
                   10  WS-DATE2-H1               PIC X.                 00002170
                   10  WS-DATE2-MM               PIC XX.                00002180
                   10  WS-DATE2-H2               PIC X.                 00002190
                   10  WS-DATE2-DD               PIC XX.                00002200
970614         05  WS-CASE-COMP                  PIC X(6).              00002210
                                                                        00002220
       01  WS-MISC.                                                     00002230
           05  WS-DEP-COUNT             PIC S9(5) COMP-3 VALUE 0.       00002240
           05  WS-LIST-BILL-COUNT       PIC S9(5) COMP-3 VALUE 0.       00002250
           05  WS-GASET                 COMP     PIC S9(9).             00002260
           05  WS-GALENGTH              COMP     PIC S9(4).             00002270
           05  WS-BIN-ONE               COMP     PIC S9(4) VALUE +1.    00002280
           05  WS-CICS-RESP             COMP     PIC S9(8).             00002290
           05  WS-LINK-LENGTH           COMP     PIC S9(8).             00002300
           05  WS-COMM-LENGTH           COMP     PIC S9(4).             00002310
           05  LIST-SQLCODE             COMP     PIC S9(4).             00002320
Y2KIMR*                                                                 00002330
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE W/S REFERENCES BEGIN              00002340
Y2KIMR*                                                                 00002350
Y2KIMR*    05  COB2-DATEPROG                  PIC X(8) VALUE 'DATEPRG2'.00002360
Y2KIMR*                                                                 00002370
Y2KIMR     05  COB2-DATEPROG                  PIC X(8) VALUE 'DTEPROG2'.00002380
Y2KIMR*                                                                 00002390
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE W/S REFERENCES END                00002400
Y2KIMR*                                                                 00002410
           05  OPER-ID                           PIC X(3).              00002420
           05  WS-QUEUE-NAME.                                           00002430
               10  WS-QUEUE-NAME-TYPE            PIC X(4).              00002440
               10  WS-QUEUE-NAME-TERM            PIC X(4).              00002450
           05  WS-MESSAGE-NUMBER                 PIC X(05) VALUE SPACES.00002460
           05  WS-HOLD-MESSAGE-NUMBER            PIC X(05).             00002470
           05  WS-ERROR-FIELDS.                                         00002480
               10  WS-C9999-ERROR-CODE           PIC X(5).              00002490
               10  WS-C9999-SEVERITY-CODE        PIC X.                 00002500
               10  FILLER                        PIC X.                 00002510
               10  WS-C9999-ERROR-MESSAGE        PIC X(30).             00002520
           05  WS-SUBCRIPT COMP.                                        00002530
               10  ERROR-SUB                     PIC S99.               00002540
           05  WS-HEX80                 COMP     PIC S9(4) VALUE +128.  00002550
           05  WS-HEX80-REDEF REDEFINES WS-HEX80.                       00002560
               10  FILLER                        PIC X.                 00002570
               10  HEX80                         PIC X.                 00002580
920435     05  WS-ADV-BILL-SW                    PIC 9(1) VALUE ZEROES. 00002590
920435         88  ADVANCE-BILL                      VALUE 1 THRU 9.    00002600
920435         88  REGULAR-BILL                      VALUE 0.           00002610
           05  WS-OK-TO-INSERT-SW                PIC X(1) VALUE 'N'.    00002620
               88  OK-TO-INSERT                      VALUE 'Y'.         00002630
           05  WS-LIST-BILL-CASE-SW              PIC X(1) VALUE 'N'.    00002640
               88  LIST-BILL-CASE                    VALUE 'Y'.         00002650
           05  WS-HOLD-CIM                       PIC X(8).              00002660
           05  WS-CASE-NUM-X                     PIC X(6).              00002670
970614*    05  WS-CASE-NUM-N                     PIC 9(6).              00002680
970614     05  WS-CASE-NUM-N                     PIC X(6).              00002690
           05  RUN-NUM-COMP                      PIC S9(3) COMP-3.      00002700
           05  RUN-NUM-IND                       PIC S9(4) COMP.        00002710
970614*    05  WS-HOLD-CASE-NUMBER               PIC S9(6) COMP-3.      00002720
970614     05  WS-HOLD-CASE-NUMBER               PIC X(6).              00002730
970614*    05  WS-HOLD-CASE                      PIC S9(6) COMP-3.      00002740
970614     05  WS-HOLD-CASE                      PIC X(6).              00002750
           05  WS-HOLD-CHECK                     PIC X.                 00002760
           05  WS-HOLD-LIST-CIM                  PIC X(8).              00002770
           05  WS-ERROR-PRESENT                  PIC X     VALUE 'N'.   00002780
           05  WS-XFER-INFO.                                            00002790
               10  WS-XFER-TYPE                  PIC X(2).              00002800
               10  FILLER                        PIC X     VALUE '\'.   00002810
               10  WS-XFER-CASE                  PIC X(6).              00002820
               10  FILLER                        PIC X     VALUE '\'.   00002830
               10  WS-XFER-TIMESTAMP             PIC X(26).             00002840
920435     05  WS-COMP-DATE-ADV.                                        00002850
920435         10  WS-COMP-YY-ADV                PIC 99.                00002860
920435         10  WS-COMP-MM-ADV                PIC 99.                00002870
920435         10  WS-COMP-DD-ADV                PIC 99.                00002880
           05  WS-COMP-DATE.                                            00002890
               10  WS-COMP-YY                    PIC 99.                00002900
               10  WS-COMP-MM                    PIC 99.                00002910
               10  WS-COMP-DD                    PIC 99.                00002920
           05  WS-DEFAULT-DATE             PIC X(10) VALUE '1111-11-11'.00002930
           05  WS-COMPARE-DB2-DATE         PIC X(10).                   00002940
           05  WS-DEFAULT-TIMESTAMP        PIC X(26) VALUE              00002950
               '1111-11-11-00.00.00.000000'.                            00002960
920435     05  WS-DB2-DATE-ADV                   PIC X(10).             00002970
920435     05  WS-DB2-DATER-ADV REDEFINES WS-DB2-DATE-ADV.              00002980
920435         10  WS-DB2-CC-ADV                 PIC XX.                00002990
920435         10  WS-DB2-YY-ADV                 PIC 99.                00003000
920435         10  FILLER                        PIC X.                 00003010
920435         10  WS-DB2-MM-ADV                 PIC 99.                00003020
920435         10  FILLER                        PIC X.                 00003030
920435         10  WS-DB2-DD-ADV                 PIC 99.                00003040
           05  WS-DB2-DATE                       PIC X(10).             00003050
           05  WS-DB2-DATER REDEFINES WS-DB2-DATE.                      00003060
               10  WS-DB2-CC                     PIC XX.                00003070
               10  WS-DB2-YY                     PIC XX.                00003080
               10  FILLER                        PIC X.                 00003090
               10  WS-DB2-MM                     PIC XX.                00003100
               10  FILLER                        PIC X.                 00003110
               10  WS-DB2-DD                     PIC XX.                00003120
       01  WS-SYS-TIMESTAMP                      PIC X(26).             00003130
       01  FILLER REDEFINES WS-SYS-TIMESTAMP.                           00003140
           05  WS-SYS-DATE.                                             00003150
               10  WS-SYS-DATE-CC                PIC 99.                00003160
               10  WS-SYS-DATE-YY                PIC 99.                00003170
               10  FILLER                        PIC X.                 00003180
               10  WS-SYS-DATE-MM                PIC 99.                00003190
               10  FILLER                        PIC X.                 00003200
               10  WS-SYS-DATE-DD                PIC 99.                00003210
               10  FILLER                        PIC X.                 00003220
           05  WS-SYS-TIME.                                             00003230
               10  WS-SYS-TIME-HH                PIC 99.                00003240
               10  FILLER                        PIC X.                 00003250
               10  WS-SYS-TIME-MM                PIC 99.                00003260
               10  FILLER                        PIC X.                 00003270
               10  WS-SYS-TIME-SS                PIC 99.                00003280
               10  FILLER                        PIC X.                 00003290
               10  WS-SYS-TIME-NN                PIC 9(6).              00003300
       01  TCTUAL                      PIC S9(4) COMP.                  00003310
           88  INVALID-TCTUAL                          VALUE 0 THRU 135.00003320
                                                                        00003330
       01  BILLING-REQUEST-RECORDX.                                     00003340
R00946     COPY BILLREQ.                                                00003350
                                                                        00003360
           EXEC SQL INCLUDE SQLCA END-EXEC.                             00003370
                                                                        00003380
           COPY REQUEST.                                                00003390
           COPY CASEXV06.                                               00003400
                                                                        00003410
           EXEC SQL                                                     00003420
                DECLARE LIST-CUR CURSOR FOR                             00003430
                SELECT CASE_NUM                                         00003440
                FROM CASEXV06                                           00003450
                WHERE LIST_BILL_CIM = :WS-HOLD-LIST-CIM                 00003460
                AND NOT ELIGIBLE_DATE > :WS-DATE1                       00003470
                AND ACTIVE_CODE = 'CM'                                  00003480
           END-EXEC.                                                    00003490
                                                                        00003500
                                                                        00003510
       LINKAGE SECTION.                                                 00003520
       01  DFHCOMMAREA.                                                 00003530
           05  COMM-AREA    PIC X(143).                                 00003540
                                                                        00003550
       01  TCTUAR.                                                      00003560
           05  FILLER                  PIC X(36).                       00003570
           05  TCTUA-DEMO-DATA.                                         00003580
               10  FILLER              PIC S9(4) COMP.                  00003590
                   88  INVALID-DEMO-DATA               VALUE 0.         00003600
               10  FILLER              PIC X(98).                       00003610
           EJECT                                                        00003620
                                                                        00003630
       PROCEDURE DIVISION.                                              00003640
                                                                        00003650
           INITIALIZE BILLING-REQUEST-RECORDX, BILLING-REQUEST-RECORD.  00003660
                                                                        00003670
           MOVE COMM-AREA TO BILLING-REQUEST-RECORDX.                   00003680
                                                                        00003690
991112     GO TO 0020-RETURN.                                           00003700
                                                                        00003710
       1000-MAINLINE SECTION.                                           00003720
                                                                        00003730
           PERFORM 2000-POPULATE-REQUEST-RECORD THRU 2000-EXIT.         00003740
                                                                        00003750
       0020-RETURN             SECTION.                                 00003760
                                                                        00003770
           MOVE BILLING-REQUEST-RECORDX TO COMM-AREA.                   00003780
                                                                        00003790
           EXEC CICS RETURN END-EXEC.                                   00003800
           GOBACK.                                                      00003810
                                                                        00003820
       0020-EXIT.                                                       00003830
           EXIT.                                                        00003840
                                                                        00003850
       2000-POPULATE-REQUEST-RECORD SECTION.                            00003860
                                                                        00003870
           EXEC SQL                                                     00003880
               SELECT CURRENT TIMESTAMP                                 00003890
               INTO :WS-SYS-TIMESTAMP                                   00003900
               FROM DUMMY                                               00003910
           END-EXEC.                                                    00003920
                                                                        00003930
           IF SQLCODE = 0                                               00003940
               NEXT SENTENCE                                            00003950
           ELSE                                                         00003960
               MOVE 'DB002' TO BRR-ERROR-CODE                           00003970
               GO TO 0020-RETURN.                                       00003980
                                                                        00003990
           MOVE WS-SYS-DATE-CC   TO WS-DB2-CC WS-DB2-CC-ADV.            00004000
           MOVE BRR-NEXT-BILL-YY TO WS-DB2-YY WS-DB2-YY-ADV.            00004010
           MOVE BRR-NEXT-BILL-MM TO WS-DB2-MM WS-DB2-MM-ADV.            00004020
           MOVE BRR-NEXT-BILL-DD TO WS-DB2-DD WS-DB2-DD-ADV.            00004030
                                                                        00004040
           MOVE WS-DB2-YY   TO WS-COMP-YY.                              00004050
           MOVE WS-DB2-MM   TO WS-COMP-MM.                              00004060
           MOVE WS-DB2-DD   TO WS-COMP-DD.                              00004070
                                                                        00004080
920435     ADD 1                TO WS-DB2-MM-ADV.                       00004090
920435     IF WS-DB2-MM-ADV GREATER THAN 12                             00004100
920435         MOVE 1               TO WS-DB2-MM-ADV                    00004110
Y2KIMR*                                                                 00004120
Y2KIMR* IMR CHANGE IMR1 BEGIN                                           00004130
Y2KIMR*                                                                 00004140
Y2KIMR*        ADD 1                TO WS-DB2-YY-ADV.                   00004150
Y2KIMR*                                                                 00004160
Y2KIMR         MOVE WS-DB2-YY-ADV TO Y2K-WK-DATE1R-9-YY                 00004170
Y2KIMR         IF  Y2K-WK-DATE1R-9-YY LESS THAN Y2K-WK-CUTOFF-YR-9      00004180
Y2KIMR             MOVE 20 TO Y2K-WK-DATE1-9-CC                         00004190
Y2KIMR         ELSE                                                     00004200
Y2KIMR             MOVE 19 TO Y2K-WK-DATE1-9-CC                         00004210
Y2KIMR         END-IF                                                   00004220
Y2KIMR         ADD 1 TO Y2K-WK-DATE1RR-9-CCYY                           00004230
Y2KIMR         MOVE Y2K-WK-DATE1R-9-YY TO WS-DB2-YY-ADV                 00004240
Y2KIMR         MOVE Y2K-WK-DATE1-9-CC TO WS-DB2-CC-ADV.                 00004250
Y2KIMR*                                                                 00004260
Y2KIMR* IMR CHANGE IMR1 END                                             00004270
Y2KIMR*                                                                 00004280
                                                                        00004290
920435     MOVE WS-DB2-YY-ADV   TO WS-COMP-YY-ADV.                      00004300
920435     MOVE WS-DB2-MM-ADV   TO WS-COMP-MM-ADV.                      00004310
920435     MOVE WS-DB2-DD-ADV   TO WS-COMP-DD-ADV.                      00004320
                                                                        00004330
*********DETERMINE IF CARRIER IS ADVANCE BILLING CARRIER                00004340
                                                                        00004350
           MOVE BRR-CARRIER-CODE   TO WT-C0020-CARRIER.                 00004360
           MOVE SPACE              TO WT-C0020-LANGUAGE.                00004370
           MOVE WT-CNTL0020 TO TURBO-CNTL-AREA.                         00004380
           PERFORM 10000-CALL-GSF.                                      00004390
           MOVE TURBO-CNTL-AREA TO WT-CNTL0020.                         00004400
           IF WT-C0020-RETURN  EQUAL TO +0                              00004410
               MOVE WT-C0020-NUM-ADV-BILL-MTH TO WS-ADV-BILL-SW         00004420
           ELSE                                                         00004430
               MOVE 'TB005'  TO BRR-ERROR-CODE                          00004440
               GO TO 0020-RETURN.                                       00004450
                                                                        00004460
********GET BILL-DATE-1 FROM TABLE 3002.                                00004470
                                                                        00004480
           MOVE ZERO TO WT-C3002-RETURN.                                00004490
           MOVE 'G' TO WT-C3002-REQUEST.                                00004500
           MOVE BRR-CARRIER-CODE  TO WT-C3002-CARR.                     00004510
           IF ADVANCE-BILL                                              00004520
               MOVE WS-DB2-DD-ADV TO WT-C3002-CO-DD                     00004530
               MOVE WS-DB2-MM-ADV TO WT-C3002-CO-MM                     00004540
               MOVE WS-DB2-YY-ADV TO WT-C3002-CO-YY                     00004550
           ELSE                                                         00004560
               MOVE WS-DB2-DD  TO WT-C3002-CO-DD                        00004570
               MOVE WS-DB2-MM  TO WT-C3002-CO-MM                        00004580
               MOVE WS-DB2-YY  TO WT-C3002-CO-YY                        00004590
           END-IF.                                                      00004600
           MOVE WT-CNTL3002 TO TURBO-CNTL-AREA.                         00004610
           PERFORM 10000-CALL-GSF.                                      00004620
           MOVE TURBO-CNTL-AREA TO WT-CNTL3002.                         00004630
           IF WT-C3002-RETURN NOT = 0                                   00004640
               MOVE 'TB005'  TO BRR-ERROR-CODE                          00004650
               GO TO 0020-RETURN.                                       00004660
                                                                        00004670
           IF ADVANCE-BILL                                              00004680
               MOVE WT-C3002-ON-MONTH TO WS-DATE2-MM                    00004690
               MOVE '01'              TO WS-DATE2-DD                    00004700
               MOVE WT-C3002-ON-YEAR  TO WS-DATE2-YY                    00004710
Y2KIMR*                                                                 00004720
Y2KIMR* IMR CHANGE IMR2 BEGIN                                           00004730
Y2KIMR*                                                                 00004740
Y2KIMR*        MOVE WS-DB2-CC         TO WS-DATE2-CC                    00004750
Y2KIMR*                                                                 00004760
Y2KIMR         MOVE WT-C3002-ON-CENTURY  TO WS-DATE2-CC                 00004770
Y2KIMR*                                                                 00004780
Y2KIMR* IMR CHANGE IMR2 END                                             00004790
Y2KIMR*                                                                 00004800
               MOVE '-'               TO WS-DATE2-H1                    00004810
                                         WS-DATE2-H2                    00004820
               MOVE WS-DATE2          TO WS-COMPARE-DB2-DATE            00004830
               MOVE WS-DEFAULT-DATE   TO WS-DATE1                       00004840
           ELSE                                                         00004850
               MOVE WT-C3002-ON-MONTH TO WS-DATE1-MM                    00004860
               MOVE '01'              TO WS-DATE1-DD                    00004870
               MOVE WT-C3002-ON-YEAR  TO WS-DATE1-YY                    00004880
Y2KIMR*                                                                 00004890
Y2KIMR* IMR CHANGE IMR3 BEGIN                                           00004900
Y2KIMR*                                                                 00004910
Y2KIMR*        MOVE WS-DB2-CC         TO WS-DATE1-CC                    00004920
Y2KIMR*                                                                 00004930
Y2KIMR         MOVE WT-C3002-ON-CENTURY  TO WS-DATE1-CC                 00004940
Y2KIMR*                                                                 00004950
Y2KIMR* IMR CHANGE IMR3 END                                             00004960
Y2KIMR*                                                                 00004970
               MOVE '-'               TO WS-DATE1-H1                    00004980
                                         WS-DATE1-H2                    00004990
               MOVE WS-DATE1          TO WS-COMPARE-DB2-DATE            00005000
               MOVE WS-DEFAULT-DATE   TO WS-DATE2.                      00005010
                                                                        00005020
           MOVE 'CO'     TO RQST-REQUEST-TYPE.                          00005030
           MOVE BRR-CASE-NUMBER TO RQST-CASE-NUMBER.                    00005040
           MOVE BRR-REQUEST-LOGONID TO RQST-REQUEST-LOGONID.            00005050
           MOVE BRR-CARRIER-CODE TO RQST-CARRIER-CODE.                  00005060
           MOVE WS-SYS-TIMESTAMP TO RQST-REQUEST-TIMESTAMP.             00005070
           MOVE BRR-SITE-CODE-1 TO RQST-SITE-CODE-1                     00005080
                                   RQST-SITE-CODE-2.                    00005090
           MOVE WS-DATE1 TO RQST-BILL-PERIOD-1.                         00005100
           MOVE WS-DATE2 TO RQST-BILL-PERIOD-2.                         00005110
           MOVE ZEROS    TO RQST-AMOUNT.                                00005120
           MOVE SPACES   TO RQST-REPORT-TYPE, RQST-REASON-CODE.         00005130
           MOVE '1'      TO RQST-REQUEST-COPIES, RQST-CHECK-DIGIT.      00005140
           MOVE WS-DEFAULT-TIMESTAMP TO RQST-PROCESS-TIMESTAMP.         00005150
           MOVE 'N'      TO RQST-PROCESS-STATUS.                        00005160
           MOVE 'D'      TO RQST-DATA-LOCATION.                         00005170
           MOVE BRR-EMPLOYEE-NUMBER TO RQST-EMPLOYEE-NUMBER.            00005180
           MOVE BRR-DEPENDENT-NUMBER TO RQST-DEPENDENT-NUMBER.          00005190
           MOVE BRR-DIVISION-NUMBER TO RQST-DIVISION-NUMBER.            00005200
           MOVE BRR-BRANCH-NUMBER TO RQST-BRANCH-NUMBER.                00005210
           MOVE BRR-SOURCE-PROGRAM TO RQST-SOURCE-PROGRAM.              00005220
                                                                        00005230
                                                                        00005240
**********GET BILL FORM TYPE FROM TABLE 0160                            00005250
                                                                        00005260
           MOVE SPACES TO WT-C0160-KEY.                                 00005270
           MOVE 'T'    TO WT-C0160-REQUEST.                             00005280
           MOVE WT-CNTL0160 TO TURBO-CNTL-AREA.                         00005290
           PERFORM 10000-CALL-GSF.                                      00005300
           MOVE TURBO-CNTL-AREA TO WT-CNTL0160.                         00005310
           IF WT-C0160-RETURN NOT = 0                                   00005320
               MOVE 'ST005'  TO BRR-ERROR-CODE                          00005330
               GO TO 0020-RETURN.                                       00005340
                                                                        00005350
           MOVE CARRIER-CODE      TO BLMD-CARRIER-CODE.                 00005360
R00946**   MOVE CASE-MKT-CODE     TO BLMD-MARKET-CODE.                  00005370
R00946     MOVE BRR-AFFL-CODE     TO BLMD-MARKET-CODE.                  00005380
           CALL WS-FORM-TYPE-MODULE USING BLMD-CARRIER-CODE             00005390
                                          BLMD-MARKET-CODE              00005400
                                          BLMD-FORM-TYPE                00005410
                                          BLMD-RETURN-CODE.             00005420
                                                                        00005430
           MOVE BLMD-FORM-TYPE TO RQST-BILL-FORM-TYPE.                  00005440
                                                                        00005450
           MOVE 'N' TO WS-LIST-BILL-CASE-SW.                            00005460
                                                                        00005470
R03179*    IF (BRR-HOW-BILLED = '05' OR '07' OR '08' OR '09')           00005480
R03179     IF (BRR-HOW-BILLED = '05' OR '07' OR '08' OR '09' OR 'E5')   00005490
               PERFORM 0570-GET-LIST-BILL                               00005500
               MOVE WS-HOLD-LIST-CIM TO RQST-LIST-BILL-CIM              00005510
               MOVE 'Y' TO WS-LIST-BILL-CASE-SW                         00005520
           ELSE                                                         00005530
               MOVE SPACES TO RQST-LIST-BILL-CIM.                       00005540
                                                                        00005550
R03179*    IF  (BRR-HOW-BILLED = '05' OR '07' OR '08' OR '09')          00005560
R03179     IF  (BRR-HOW-BILLED = '05' OR '07' OR '08' OR '09' OR 'E5')  00005570
           AND WS-HOLD-LIST-CIM = SPACES                                00005580
               MOVE 'BL051' TO  BRR-ERROR-CODE                          00005590
               GO TO 0020-RETURN.                                       00005600
                                                                        00005610
*********IF THIS IS A CASE LEVEL REQUEST, THEN MUST CHECK TABLE TO SEE  00005620
*********IF A REQUEST ALREADY EXISTS AT THE EMPLOYEE OR DEPENDENT LEVEL.00005630
*********IF SO, THEN THE PROCESS_STATUS ON THE ROW MUST BE SET TO 'B'   00005640
*********SO BILLING WILL BYPASS REQUEST.                                00005650
                                                                        00005660
           PERFORM 0600-CHECK-REQUEST-TABLE THRU 0600-EXIT.             00005670
                                                                        00005680
           IF OK-TO-INSERT                                              00005690
               EXEC SQL                                                 00005700
                   INSERT                                               00005710
                   INTO REQUEST                                         00005720
                   VALUES (:BILLING-REQUEST-RECORD)                     00005730
               END-EXEC                                                 00005740
           ELSE                                                         00005750
               MOVE 'BL055'       TO BRR-ERROR-CODE                     00005760
               GO TO 0020-RETURN.                                       00005770
                                                                        00005780
           IF SQLCODE = 0                                               00005790
               PERFORM 0575-LIST-BILL THRU 0575-EXIT                    00005800
               MOVE 'BL054'       TO BRR-ERROR-CODE                     00005810
               GO TO 0020-RETURN                                        00005820
           ELSE                                                         00005830
               MOVE 'DB002' TO BRR-ERROR-CODE                           00005840
               GO TO 0020-RETURN.                                       00005850
                                                                        00005860
       2000-EXIT.                                                       00005870
           EXIT.                                                        00005880
                                                                        00005890
       0570-GET-LIST-BILL.                                              00005900
                                                                        00005910
           EXEC SQL                                                     00005920
                SELECT LIST_BILL_CIM                                    00005930
                INTO :WS-HOLD-LIST-CIM                                  00005940
                FROM CASEXV06                                           00005950
                WHERE CASE_NUM = :BRR-CASE-NUMBER                       00005960
           END-EXEC.                                                    00005970
                                                                        00005980
           IF SQLCODE = 0                                               00005990
               NEXT SENTENCE                                            00006000
           ELSE                                                         00006010
               MOVE 'DB002' TO BRR-ERROR-CODE                           00006020
               GO TO 0020-RETURN.                                       00006030
                                                                        00006040
                                                                        00006050
       0570-EXIT.                                                       00006060
           EXIT.                                                        00006070
                                                                        00006080
       0575-LIST-BILL.                                                  00006090
                                                                        00006100
**********FIRST, SEE IF THE REST OF THE CASES ASSOCIATED WITH THIS LIST 00006110
**********BILL ARE ALREADY ON THE REQUEST TABLE.  IF SO, DON'T NEED TO  00006120
**********PUT THEM THERE AGAIN.                                         00006130
                                                                        00006140
           INITIALIZE WS-LIST-BILL-COUNT.                               00006150
                                                                        00006160
           EXEC SQL                                                     00006170
             SELECT COUNT(*)                                            00006180
               INTO :WS-LIST-BILL-COUNT                                 00006190
               FROM REQUEST                                             00006200
              WHERE LIST_BILL_CIM = :RQST-LIST-BILL-CIM                 00006210
                AND REQUEST_TYPE = 'CO'                                 00006220
                AND PROCESS_STATUS = 'N'                                00006230
                AND BILL_PERIOD_1 = :WS-COMPARE-DB2-DATE                00006240
           END-EXEC.                                                    00006250
                                                                        00006260
           IF SQLCODE = 0                                               00006270
               IF WS-LIST-BILL-COUNT > 1                                00006280
                   GO TO 0575-EXIT                                      00006290
               ELSE                                                     00006300
                   NEXT SENTENCE                                        00006310
               END-IF                                                   00006320
           ELSE                                                         00006330
               MOVE 'DB002' TO BRR-ERROR-CODE                           00006340
               GO TO 0020-RETURN                                        00006350
           END-IF.                                                      00006360
                                                                        00006370
R03179*    IF (BRR-HOW-BILLED = '05' OR '07' OR '08' OR '09')           00006380
R03179     IF (BRR-HOW-BILLED = '05' OR '07' OR '08' OR '09' OR 'E5')   00006390
               NEXT SENTENCE                                            00006400
           ELSE                                                         00006410
               GO TO 0575-EXIT.                                         00006420
                                                                        00006430
           EXEC SQL OPEN LIST-CUR END-EXEC.                             00006440
           IF SQLCODE = 0                                               00006450
               NEXT SENTENCE                                            00006460
           ELSE                                                         00006470
               MOVE 'DB002' TO BRR-ERROR-CODE                           00006480
               GO TO 0020-RETURN.                                       00006490
                                                                        00006500
           PERFORM 0580-EXPLODE-LIST THRU 0580-EXIT                     00006510
               UNTIL LIST-SQLCODE NOT = 0.                              00006520
                                                                        00006530
           EXEC SQL CLOSE LIST-CUR END-EXEC.                            00006540
           IF SQLCODE = 0                                               00006550
               NEXT SENTENCE                                            00006560
           ELSE                                                         00006570
               MOVE 'DB002' TO BRR-ERROR-CODE                           00006580
               GO TO 0020-RETURN.                                       00006590
                                                                        00006600
       0575-EXIT.                                                       00006610
           EXIT.                                                        00006620
                                                                        00006630
       0580-EXPLODE-LIST.                                               00006640
                                                                        00006650
           EXEC SQL                                                     00006660
                FETCH LIST-CUR                                          00006670
                INTO :WS-HOLD-CASE                                      00006680
           END-EXEC.                                                    00006690
                                                                        00006700
           MOVE SQLCODE TO LIST-SQLCODE.                                00006710
                                                                        00006720
           EVALUATE TRUE                                                00006730
               WHEN LIST-SQLCODE = 0                                    00006740
                    CONTINUE                                            00006750
               WHEN LIST-SQLCODE = +100                                 00006760
                    GO TO 0580-EXIT                                     00006770
               WHEN OTHER                                               00006780
                    MOVE 'DB002' TO BRR-ERROR-CODE                      00006790
                    GO TO 0020-RETURN                                   00006800
           END-EVALUATE.                                                00006810
                                                                        00006820
***********DON'T WANT TO INCLUDE THE REQUEST CASE IN THIS PROCESSING    00006830
***********SINCE WE HAVE ALREADY PROCESSED IT.                          00006840
                                                                        00006850
           IF WS-HOLD-CASE = BRR-CASE-NUMBER                            00006860
               GO TO 0580-EXIT.                                         00006870
                                                                        00006880
           MOVE WS-HOLD-CASE    TO RQST-CASE-NUMBER.                    00006890
           MOVE '1'             TO RQST-CHECK-DIGIT.                    00006900
                                                                        00006910
                                                                        00006920
           EXEC SQL                                                     00006930
               INSERT                                                   00006940
               INTO REQUEST                                             00006950
               VALUES (:BILLING-REQUEST-RECORD)                         00006960
           END-EXEC.                                                    00006970
                                                                        00006980
           EVALUATE TRUE                                                00006990
               WHEN SQLCODE = 0                                         00007000
                    CONTINUE                                            00007010
               WHEN OTHER                                               00007020
                    MOVE 'DB002' TO BRR-ERROR-CODE                      00007030
                    GO TO 0020-RETURN                                   00007040
           END-EVALUATE.                                                00007050
                                                                        00007060
       0580-EXIT.                                                       00007070
           EXIT.                                                        00007080
                                                                        00007090
       0600-CHECK-REQUEST-TABLE    SECTION.                             00007100
                                                                        00007110
           MOVE 'N' TO WS-OK-TO-INSERT-SW.                              00007120
                                                                        00007130
**********IF THIS REQUEST BELONGS TO A CASE, EMPLOYEE OR DEPENDENT THAT 00007140
**********BELONGS TO A LIST BILL, ALWAYS GENERATE A CASE REQUEST.       00007150
                                                                        00007160
           IF LIST-BILL-CASE                                            00007170
               MOVE ZEROS TO RQST-EMPLOYEE-NUMBER BRR-EMPLOYEE-NUMBER   00007180
                             RQST-DEPENDENT-NUMBER BRR-DEPENDENT-NUMBER 00007190
           END-IF.                                                      00007200
                                                                        00007210
**********SEE IF CASE REQUEST ALREADY EXISTS.                           00007220
                                                                        00007230
           EXEC SQL                                                     00007240
            SELECT EMPLOYEE_NUMBER                                      00007250
             INTO :EMPLOYEE-NUMBER                                      00007260
               FROM REQUEST                                             00007270
              WHERE CASE_NUMBER = :RQST-CASE-NUMBER                     00007280
                AND EMPLOYEE_NUMBER = 0                                 00007290
                AND REQUEST_TYPE = 'CO'                                 00007300
                AND PROCESS_STATUS = 'N'                                00007310
                AND BILL_PERIOD_1 = :WS-COMPARE-DB2-DATE                00007320
           END-EXEC.                                                    00007330
                                                                        00007340
**********IF SQLCODE = 0, THEN CASE REQUEST IS ALREADY ON TABLE FOR THIS00007350
**********BILLING PERIOD; DON'T NEED TO DO ANYTHING ELSE.               00007360
                                                                        00007370
           IF SQLCODE NOT = 0 AND +100                                  00007380
               MOVE 'DB002' TO BRR-ERROR-CODE                           00007390
               GO TO 0020-RETURN.                                       00007400
                                                                        00007410
           IF SQLCODE = 0                                               00007420
               MOVE 'BL054' TO BRR-ERROR-CODE                           00007430
               GO TO 0020-RETURN.                                       00007440
                                                                        00007450
**********IF SQLCODE = +100 AND EMPLOYEE NUMBER IS ZERO, THEN NEED TO   00007460
**********INSERT THE CASE REQUEST AND SET ANY EMPLOYEE REQUEST ROWS'    00007470
**********PROCESS STATUS TO 'B' - BYPASSED BY BILLING.  IF THE EMPLOYEE 00007480
**********NUMBER IS NOT ZERO, THEN NEED TO DETERMINE IF A REQUEST       00007490
**********ALREADY EXISTS FOR THIS EMPLOYEE.  IF SO, NO INSERT NEEDED.   00007500
**********OTHERWISE, NEED TO INSERT FOR THIS EMPLOYEE.                  00007510
                                                                        00007520
**********THIS IS A CASE REQUEST                                        00007530
                                                                        00007540
           IF BRR-EMPLOYEE-NUMBER = 0                                   00007550
               MOVE 'Y' TO WS-OK-TO-INSERT-SW                           00007560
               EXEC SQL                                                 00007570
                 UPDATE REQUEST                                         00007580
                   SET PROCESS_STATUS = 'B'                             00007590
                       WHERE CASE_NUMBER = :RQST-CASE-NUMBER            00007600
                       AND   REQUEST_TYPE = 'CO'                        00007610
                       AND   PROCESS_STATUS = 'N'                       00007620
                       AND   BILL_PERIOD_1   = :WS-COMPARE-DB2-DATE     00007630
               END-EXEC                                                 00007640
               IF SQLCODE = 0 OR +100                                   00007650
                   GO TO 0600-EXIT                                      00007660
               ELSE                                                     00007670
                   MOVE 'DB002' TO BRR-ERROR-CODE                       00007680
                   GO TO 0020-RETURN                                    00007690
               END-IF                                                   00007700
           END-IF.                                                      00007710
                                                                        00007720
**********IF EMPLOYEE REQUEST COMES ALONG, CHECK TO SEE IF ROWS ARE     00007730
**********ALREADY OUT THERE FOR DEPENDENTS, IF SO, EMPLOYEE ROW RULES,  00007740
**********AND HAVE TO SET DEP ROWS TO PROCESS STATUS OF 'B' TO BYPASS.  00007750
                                                                        00007760
**********THIS IS AN EMPLOYEE REQUEST                                   00007770
                                                                        00007780
           INITIALIZE WS-DEP-COUNT.                                     00007790
                                                                        00007800
           IF BRR-EMPLOYEE-NUMBER > 0                                   00007810
               IF BRR-DEPENDENT-NUMBER = 0                              00007820
                   EXEC SQL                                             00007830
                       SELECT COUNT(*)                                  00007840
                         INTO :WS-DEP-COUNT                             00007850
                        FROM REQUEST                                    00007860
                        WHERE CASE_NUMBER = :RQST-CASE-NUMBER           00007870
                          AND EMPLOYEE_NUMBER = :RQST-EMPLOYEE-NUMBER   00007880
                          AND DEPENDENT_NUMBER > 0                      00007890
                          AND REQUEST_TYPE = 'CO'                       00007900
                          AND PROCESS_STATUS = 'N'                      00007910
                          AND BILL_PERIOD_1 = :WS-COMPARE-DB2-DATE      00007920
                   END-EXEC                                             00007930
                   IF SQLCODE NOT = 0                                   00007940
                       MOVE 'DB002' TO BRR-ERROR-CODE                   00007950
                       GO TO 0020-RETURN                                00007960
                   ELSE                                                 00007970
                   IF WS-DEP-COUNT > 0                                  00007980
                       MOVE 'Y' TO WS-OK-TO-INSERT-SW                   00007990
                       EXEC SQL                                         00008000
                         UPDATE REQUEST                                 00008010
                         SET PROCESS_STATUS = 'B'                       00008020
                         WHERE CASE_NUMBER = :RQST-CASE-NUMBER          00008030
                         AND EMPLOYEE_NUMBER = :RQST-EMPLOYEE-NUMBER    00008040
                         AND DEPENDENT_NUMBER > 0                       00008050
                         AND REQUEST_TYPE = 'CO'                        00008060
                         AND PROCESS_STATUS = 'N'                       00008070
                         AND BILL_PERIOD_1 = :WS-COMPARE-DB2-DATE       00008080
                       END-EXEC                                         00008090
                   ELSE                                                 00008100
***********IF DEP COUNT IS 0, THEN NEED TO SEE IF EMPLOYEE ROW IS       00008110
***********ALREADY OUT THERE.  IF SO, NO INSERT, IF NOT, THEN INSERT    00008120
                   EXEC SQL                                             00008130
                     SELECT EMPLOYEE_NUMBER                             00008140
                       INTO :EMPLOYEE-NUMBER                            00008150
                       FROM REQUEST                                     00008160
                      WHERE CASE_NUMBER = :RQST-CASE-NUMBER             00008170
                      AND EMPLOYEE_NUMBER = :RQST-EMPLOYEE-NUMBER       00008180
                      AND DEPENDENT_NUMBER = 0                          00008190
                      AND REQUEST_TYPE = 'CO'                           00008200
                      AND PROCESS_STATUS = 'N'                          00008210
                      AND BILL_PERIOD_1 = :WS-COMPARE-DB2-DATE          00008220
                   END-EXEC                                             00008230
                   IF SQLCODE = +100                                    00008240
                       MOVE 'Y' TO WS-OK-TO-INSERT-SW                   00008250
                       GO TO 0600-EXIT                                  00008260
                   ELSE                                                 00008270
                   IF SQLCODE = 0                                       00008280
                       MOVE 'BL055' TO BRR-ERROR-CODE                   00008290
                       GO TO 0020-RETURN                                00008300
                   ELSE                                                 00008310
                       MOVE 'DB002' TO BRR-ERROR-CODE                   00008320
                       GO TO 0020-RETURN                                00008330
                   END-IF.                                              00008340
                                                                        00008350
**********IF DEPENDENT REQUEST COMES ALONG, CHECK TO SEE IF EITHER THE  00008360
**********EMPLOYEE OR THE DEPENDENT REQUEST ALREADY EXISTS.  IF EITHER  00008370
**********EXISTS FOR THEN NO NEED TO INSERT, OTHERWISE, MUST INSERT     00008380
**********DEPENDENT REQUEST.                                            00008390
                                                                        00008400
**********THIS IS A DEPENDENT REQUEST                                   00008410
                                                                        00008420
           IF BRR-EMPLOYEE-NUMBER > 0                                   00008430
               IF BRR-DEPENDENT-NUMBER > 0                              00008440
                   EXEC SQL                                             00008450
                     SELECT EMPLOYEE_NUMBER                             00008460
                     INTO :EMPLOYEE-NUMBER                              00008470
                     FROM REQUEST                                       00008480
                    WHERE CASE_NUMBER = :RQST-CASE-NUMBER               00008490
                    AND EMPLOYEE_NUMBER = :RQST-EMPLOYEE-NUMBER         00008500
                    AND DEPENDENT_NUMBER IN (0, :RQST-DEPENDENT-NUMBER) 00008510
                    AND REQUEST_TYPE = 'CO'                             00008520
                    AND PROCESS_STATUS = 'N'                            00008530
                    AND BILL_PERIOD_1 = :WS-COMPARE-DB2-DATE            00008540
                   END-EXEC                                             00008550
                   IF SQLCODE = +100                                    00008560
                       MOVE 'Y' TO WS-OK-TO-INSERT-SW                   00008570
                       GO TO 0600-EXIT                                  00008580
                   ELSE                                                 00008590
                   IF SQLCODE = 0                                       00008600
                       MOVE 'BL055' TO BRR-ERROR-CODE                   00008610
                       GO TO 0020-RETURN                                00008620
                   ELSE                                                 00008630
                       MOVE 'DB002' TO BRR-ERROR-CODE                   00008640
                       GO TO 0020-RETURN                                00008650
                   END-IF.                                              00008660
                                                                        00008670
       0600-EXIT.                                                       00008680
           EXIT.                                                        00008690
                                                                        00008700
      *---------------------------------------------------------------* 00008710
      *    TURBOCAL IS INCLUDED CODE FOR TURBO FILE ACCESS REQUESTS.  * 00008720
      *---------------------------------------------------------------* 00008730
           COPY TURBOCAL.                                               00008740
           EJECT                                                        00008750
                                                                        00008760
      *---------------------------------------------------------------* 00008770
      *    END SOURCE CODE; MODULE RQ09999.                             00008780
      *---------------------------------------------------------------* 00008790
