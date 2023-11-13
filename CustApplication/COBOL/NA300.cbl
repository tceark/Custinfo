 CBL DATA(24)                                                           00000010
       IDENTIFICATION DIVISION.                                         00000020
       PROGRAM-ID. NA300.                                               00000030
      *---------------START OF INFORMATION BLOCK----------------------* 00000040
      * DATE-WRITTEN. 12/30/87.                                         00000270
      * REMARKS. NA300 IS THE CUSTOMER INFORMATION MANAGER'S MAIN MENU. 00000280
      *          ENTERING THE SYSTEM SELECTION AND ACTION CODE DIRECTS  00000290
      *          THE ROUTER TO THE CORRECT SYSTEM.                      00000300
      *------------------------PROGRAM PURPOSE-------------------------*00000310
      *                                                                *00000320
      *  PROGRAM TITLE: NA300                                          *00000330
      *  PROGRAM TEXT:                                                 *00000340
      *----------------------------------------------------------------*00000350
       ENVIRONMENT DIVISION.                                            00000690
       DATA DIVISION.                                                   00000700
       WORKING-STORAGE SECTION.                                         00000710
       77  WS-ISEIB-ROUTINE                PIC X(8)    VALUE 'ISEIB   '.00000720
       77  WS-PSIGSFC                      PIC X(08) VALUE 'PSIGSFC '.  00000730
       77  CAPPLID                         PIC X(8).                    00000740
       77  WS-ZERO-LENGTH                   PIC S9(4) VALUE +0   COMP.  00000750
       77  WS-COMM-LENGTH                   PIC S9(4) VALUE +600 COMP.  00000760
       77  WS-TUTOR-COMM-LENGTH             PIC S9(4) VALUE +906 COMP.  00000770
       77  WS-LINK-LENGTH                   PIC S9(8) VALUE +0   COMP.  00000780
       77  WS-TS-ITEM                       PIC S9(4) VALUE +1   COMP.  00000790
           COPY AUDCICS.                                                00000800
           COPY CAWSINC.                                                00000810
           COPY DEMOCOMM.                                               00000820
           COPY TURB9999.                                               00000830
           COPY TURB8991.                                               00000840
      *---------------------------------------------------------------* 00000850
      *    COPY TUTORCOM.                                             * 00000860
      *---------------------------------------------------------------* 00000870
           COPY TU003COM.                                               00000880
           COPY CICSWS.                                                 00000890
           COPY ATTRB.                                                  00000900
           COPY TURBINC.                                                00000910
           COPY TURBDATA.                                               00000920
           EJECT                                                        00000930
       01  WS-WORK-AREA.                                                00000940
           05  WS-GASET                     PIC S9(9) COMP.             00000950
           05  WS-GALENGTH                  PIC S9(4) COMP.             00000960
           05  WS-CICS-RESP                 PIC S9(4) COMP.             00000970
           05  ERROR-STATUS                PIC S9(8) COMP VALUE +0.     00000980
           05  WS-TS-QUEUE-NAME.                                        00000990
               10  WS-TS-QUEUE-TRANID       PIC X(4).                   00001000
               10  WS-TS-QUEUE-TERMID       PIC X(4).                   00001010
           05  WS-HEX80                     PIC S9(4) VALUE +128 COMP.  00001020
           05  WS-HEX80-REDEF REDEFINES WS-HEX80.                       00001030
               10  FILLER                   PIC X.                      00001040
               10  HEX80                    PIC X.                      00001050
           05  WS-NULL-FIELD                PIC X(72).                  00001060
           05  WS-ERROR-FIELDS.                                         00001070
               10  WS-C9999-ERROR-CODE      PIC X(5).                   00001080
               10  WS-C9999-SEVERITY-CODE   PIC X.                      00001090
      **       10  FILLER                   PIC X.                      00001100
               10  WS-C9999-FILLER          PIC X.                      00001110
               10  WS-C9999-ERROR-MESSAGE   PIC X(30).                  00001120
           05  WS-SUBCRIPT COMP.                                        00001130
               10  ACTION-SUB               PIC S99.                    00001140
           05  WS-MESSAGE-NUMBER            PIC X(5).                   00001150
           05  WS-VALID-ECI-CODE-SW         PIC X  VALUE SPACE.         00001160
               88  VALID-ECI-CODE           VALUE 'Y'.                  00001170
           05  WS-LOGON-HOLD                     PIC X(8).              00001180
           05  WS-SQL-ERROR-MSG.                                        00001190
               10  FILLER       PIC X(13)  VALUE '++++++ Error '.       00001200
               10  FILLER       PIC X(18)  VALUE 'Occured in Program'.  00001210
               10  FILLER       PIC X(17)  VALUE ' NA300  -  Para. '.   00001220
               10  WS-SQL-PARA  PIC X(30)  VALUE SPACES.                00001230
           05  WS-A                              PIC X(1) VALUE 'A'.    00001180
           05  WS-SPACES                         PIC X(5) VALUE SPACES. 00001180
                                                                        00001240
       01  TCTUAL                      PIC S9(4) COMP.                  00001250
           88  INVALID-TCTUAL              VALUE 0 THRU 135.            00001260
                                                                        00001270
       01  WSQ-COMMAREA.                                                00001280
           05  WSQ-CICS-COMMAREA-LENGTH COMP PIC S9(4) VALUE +600.      00001290
           05  WSQ-SYSTEM-CODE              PIC X(02).                  00001300
           05  WSQ-ACTION-CODE              PIC X(05).                  00001310
           05  WSQ-CUSTOMER-INFO            PIC X(40).                  00001320
           05  WSQ-CUST-TOKEN-COUNT         PIC S9.                     00001330
           05  WSQ-CUSTOMER-INFO1           PIC X(40).                  00001340
           05  WSQ-CUSTOMER-INFO2           PIC X(30).                  00001350
           05  WSQ-CUSTOMER-INFO3           PIC X(30).                  00001360
           05  WSQ-CUSTOMER-INFO4           PIC X(30).                  00001370
           05  WSQ-CUSTOMER-INFO5           PIC X(30).                  00001380
           05  WSQ-CUSTOMER-INFO6           PIC X(30).                  00001390
           05  FILLER                       PIC X(46).                  00001400
           05  WSQ-PREVIOUS-TRANID          PIC X(4).                   00001410
           05  WSQ-CMDLINE-CHANGED          PIC X(1).                   00001420
           05  WSQ-KEY-CHANGED              PIC X(1).                   00001430
           05  WSQ-IDNTITY-NUMBER           PIC X(8).                   00001440
           05  WSQ-MAIL-LINE-COUNT          PIC 9.                      00001450
           05  WSQ-MAIL-LINE                PIC X(30) OCCURS 5 TIMES.   00001460
           05  WSQ-LAST-ITEM-NUM-BROWSED    PIC S9(3) COMP-3.           00001470
           05  FILLER                       PIC X(29).                  00001480
           05  WSQ-EXTERNAL-CUST            PIC X.                      00001490
           05  FILLER                       PIC X(8).                   00001500
           05  WSQ-MSG-COUNT                PIC 9.                      00001510
           05  WSQ-MSG-ID                   PIC X(5) OCCURS 4 TIMES.    00001520
           05  WSQ-MSG-MAX-SEVERITY         PIC X(1).                   00001530
           05  WSQ-NEXT-FUNCTION            PIC X(2).                   00001540
           05  WSQ-CURSOR-POSN              PIC S9(4) COMP.             00001550
           05  FILLER                       PIC X(83).                  00001560
           05  WSQ-TUTORIAL-SW              PIC X.                      00001570
                                                                        00001580
       01  WS-TS-QUEUE.                                                 00001590
           COPY NA200C01.                                               00001600
           02  WS-COM-LINK   OCCURS 0 TO 3496 TIMES                     00001610
                             DEPENDING ON WS-LINK-LENGTH                00001620
                                            PIC X.                      00001630
                                                                        00001640
      *---------------------------------------------------------        00001650
      *    GET SQL COMMUNICATION AREA.                                  00001660
      *---------------------------------------------------------        00001670
      *    EXEC SQL INCLUDE SQLCA END-EXEC.                             00001680
      *    05  SQL-ERROR-MESSAGE             PIC X(78) VALUE SPACES.    00001690
                                                                        00001700
      *    COPY SYSBUSY.                                                00001710
           COPY NA300M1.                                                00001720
      *    COPY ECLIDV01.                                               00001730
      *    COPY MTRXV01.                                                00001740
           COPY ECACFC01.                                               00001750
           COPY DFHAID.                                                 00001760
           COPY DFHBMSCA.                                               00001770
                                                                        00001780
       LINKAGE SECTION.                                                 00001790
                                                                        00001800
       01  DFHCOMMAREA.                                                 00001810
                                                                        00001820
           05  FILLER                       PIC X.                      00001830
                                                                        00001840
       01  WS-LINK-STORAGE.                                             00001850
           02  WS-LINK-SIX-HUNDRED          PIC X(600).                 00001860
           02  WS-LINK-STUFF OCCURS 0 TO 3496 TIMES                     00001870
                             DEPENDING ON WS-LINK-LENGTH                00001880
                                            PIC X.                      00001890
                                                                        00001900
       01  TCTUAR.                                                      00001910
           05  FILLER                  PIC X(36).                       00001920
           05  TCTUA-DEMO-DATA.                                         00001930
               10  FILLER              PIC S9(4) COMP.                  00001940
                   88  INVALID-DEMO-DATA               VALUE 0.         00001950
               10  FILLER              PIC X(98).                       00001960
                                                                        00001970
      ******************************************************************00001980
                                                                        00001990
       PROCEDURE DIVISION.                                              00002000
                                                                        00002010
       0010-BEGIN-PROGRAM.                                              00002020
                                                                        00002030
           DISPLAY 'START OF PROGRAM - DBB'.
           EXEC CICS HANDLE CONDITION ERROR(9999-CICS-ERROR)            00002040
                                      END-EXEC.                         00002050
                                                                        00002060
           MOVE LOW-VALUES TO NA300M1O.                                 00002070
           MOVE SPACES     TO WS-COST-CR-CODE.                          00002080
                                                                        00002090
                                                                        00002100
      *---------------------------------------------------------------* 00002110
      * THIS IS THE COPY BOOK FOR THE COST JOURNAL.                   * 00002130
      *---------------------------------------------------------------* 00002140
                                                                        00002150
           COPY CAPRCINC.                                               00002160
                                                                        00002170
       0020-READ-NAQ1-TS-QUEUE.                                         00002180
                                                                        00002190
      *---------------------------------------------------------------* 00002200
      *    THIS IS THE TEMPORARY STORAGE QUEUE THAT IS MAINTAINED     * 00002210
      *    THROUGH OUT THE ENTIRE     SYSTEM. THE QUEUE IS            * 00002220
      *    ESTABLISHED THE FIRST TIME THE CUSTOMER SIGNS ON           * 00002230
      *    AND IS ONLY DELETED IF CICS COMES DOWN.                    * 00002240
      *---------------------------------------------------------------* 00002250
                                                                        00002260
           MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.           00002270
           MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.           00002280
           DISPLAY '0020-READ-NAQ1-TS-QUEUE'.
           EXEC CICS READQ TS QUEUE  (WS-TS-QUEUE-NAME)                 00002290
                              SET    (ADDRESS OF WS-LINK-STORAGE)       00002300
                              LENGTH (WS-ZERO-LENGTH)                   00002310
                              ITEM   (WS-TS-ITEM)                       00002320
                              RESP   (WS-CICS-RESP)                     00002330
                              END-EXEC.                                 00002340
                                                                        00002350
           DISPLAY 'END OF QUEUE'.
           DISPLAY 'WS-CICS-RESP:' WS-CICS-RESP.
                                                                        00002360
           IF WS-CICS-RESP = DFHRESP(QIDERR)                            00002370
               DISPLAY 'START WRITE NEW QUEUE'
               PERFORM 0150-WRITE-NAQ1-QUEUE                            00002380
               DISPLAY 'END WRITE NEW QUEUE'
           ELSE                                                         00002390
              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                     00002400
                  GO TO 9999-CICS-ERROR.                                00002410
                                                                        00002420
           MOVE WS-ZERO-LENGTH TO WS-ZERO-LENGTH.                       00002430
           COMPUTE WS-LINK-LENGTH = (WS-ZERO-LENGTH - +600).            00002440
           MOVE WS-LINK-STORAGE TO WS-TS-QUEUE.                         00002450

           DISPLAY 'EIBTRNID:' EIBTRNID.
           DISPLAY 'WS-LINK-LENGTH:' WS-LINK-LENGTH
                                                                        00002460
           IF EIBTRNID = 'TU01'                                         00002470
                 GO TO 0115-INITIATE-NA300.                             00002480
                                                                        00002490
MANJU *    DISPLAY 'DB2 ATTACH FACILITY'.
      *---------------------------------------------------------        00002500
      *    CHECK TO SEE IF THE DB2 ATTACH FACILITY IS ACTIVE.           00002510
      *---------------------------------------------------------        00002520
      *    EXEC CICS EXTRACT EXIT                                       00002530
      *              PROGRAM   ('DSNCEXT1')                             00002540
      *              ENTRYNAME ('DSNCSQL')                              00002550
      *              GASET     (WS-GASET)                               00002560
      *              GALENGTH  (WS-GALENGTH)                            00002570
      *              RESP      (ERROR-STATUS)                           00002580
      *    END-EXEC.                                                    00002590
      *                                                                 00002600
      *    DISPLAY 'DB2 ATTACH FACILITY END:' ERROR-STATUS.
      *    IF ERROR-STATUS = DFHRESP(INVEXITREQ)                        00002610
      *        EXEC CICS SEND CONTROL ALARM END-EXEC                    00002620
      *        MOVE 'DB2 Attach Failure.'                               00002630
      *          TO MAP-ERROR-MSGO (1)                                  00002640
      *        MOVE 'Contact Support Group Immediately.'                00002650
      *          TO MAP-ERROR-MSGO (2)                                  00002660
      *        EXEC CICS SEND MAP    ('NA300M1')                        00002670
      *                   CURSOR                                        00002680
      *                   ERASE                                         00002690
      *                   RESP (WS-CICS-RESP)                           00002700
      *                   END-EXEC                                      00002710
      *        IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                    00002720
      *             GO TO 9999-CICS-ERROR                               00002730
      *        ELSE                                                     00002740
MANJU *        EXEC CICS RETURN END-EXEC.                               00002750
                                                                        00002760
      *    PERFORM 0155-CHECK-FOR-EXTERNAL-CUST THRU 0155-EXIT.         00002770
                                                                        00002780
                                                                        00002790
       0030-PROCESS-MENU-SCREEN.                                        00002800
                                                                        00002810
           DISPLAY 'COMM-NEXT-FUNCTION:' COMM-NEXT-FUNCTION
           DISPLAY 'COMM-MSG-MAX-SEVERITY:' COMM-MSG-MAX-SEVERITY
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00002820
               DISPLAY '1'
               GO TO 0100-SEND-NA300M1-MENU                             00002830
           ELSE                                                         00002840
              IF COMM-NEXT-FUNCTION = '00'                              00002850
               DISPLAY '2'
                  GO TO 0050-RETURN-FROM-ROUTER                         00002860
              ELSE                                                      00002870
                 IF EIBAID = DFHENTER                                   00002880
               DISPLAY '3'
                     GO TO 0040-EDIT-COMMAND-LINE                       00002890
                 ELSE                                                   00002900
                    IF EIBAID = DFHCLEAR                                00002910
               DISPLAY '4'
                        GO TO 0140-PROCESS-SCREEN-CLEAR                 00002920
                    ELSE                                                00002930
                       IF EIBAID = DFHPF18                              00002940
               DISPLAY '5'
MANJU *                    GO TO 0120-PROCESS-TUTORIAL-REQUEST          00002950
      *---------------------------------------------------------------* 00002960
      *                   ELSE                                        * 00002970
      *                     IF COMM-NEXT-FUNCTION = 'TU'              * 00002980
      *                         GO TO 0130-RETURN-FROM-TUTORIAL       * 00002990
      *---------------------------------------------------------------* 00003000
                            ELSE                                        00003010
                             IF EIBAID = DFHPA1 AND                     00003020
                                COMM-EXTERNAL-CUST NOT = 'Y'            00003030
               DISPLAY '6'
                                 GO TO 0170-RETURN-TO-MT-BILLION        00003040
                             ELSE                                       00003050
                                IF EIBAID = DFHPF2                      00003060
               DISPLAY '7'
                                    GO TO 0060-EXECUTE-STACK-FUNCTION   00003070
                                ELSE                                    00003080
                                   IF EIBAID = DFHPF3                   00003090
               DISPLAY '8'
                                       GO TO 0070-RETURN-TO-BROWSE      00003100
                                   ELSE                                 00003110
               DISPLAY '9'
                                      GO TO 0160-WRONG-KEY-HIT.         00003120
                                                                        00003130
       0040-EDIT-COMMAND-LINE.                                          00003140
           MOVE ALL LOW-VALUES TO NA300M1I.                             00003150
           EXEC CICS RECEIVE MAP  ('NA300M1')                           00003160
                             RESP (WS-CICS-RESP)                        0      0
                             END-EXEC.                                  00003180
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00003190
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00003200
                   GO TO 9999-CICS-ERROR.                               00003210
           PERFORM 0080-UPDATE-QUEUE-AREA.                              00003220
                                                                        00003230
      *---------------------------------------------------------------* 00003240
      * IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE = 'Y' THEN SOMETHING* 00003250
      * ON THE COMMAND LINE CHANGED AND THE ROUTER SHOULD BE STARTED. * 00003260
      *---------------------------------------------------------------* 00003270
                                                                        00003280
                                                                        00003290
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED  = 'Y'     00003300
               GO TO 0110-INITIATE-ROUTER.                              00003310
                                                                        00003320
      *---------------------------------------------------------------* 00003330
      * IF COMM-NEXT-FUNCTION = '01' THEN THE CUSTOMER IS RE-ENTERING * 00003340
      * THE PROGRAM WITHOUT CHANGING THE COMMAND LINE.                * 00003350
      *---------------------------------------------------------------* 00003360
                                                                        00003370
           IF COMM-NEXT-FUNCTION = '01'                                 00003380
               GO TO 0100-SEND-NA300M1-MENU.                            00003390
                                                                        00003400
       0050-RETURN-FROM-ROUTER.                                         00003410
      *---------------------------------------------------------------* 00003420
      * IF COMM-NEXT-FUNCTION = '00' SEND OUT THE SYSTEM SELECTION    * 00003430
      * MAP                                                           * 00003440
      *---------------------------------------------------------------* 00003450
                                                                        00003460
           GO TO 0100-SEND-NA300M1-MENU.                                00003470
                                                                        00003480
       0060-EXECUTE-STACK-FUNCTION.                                     00003490
                                                                        00003500
      *---------------------------------------------------------------* 00003510
      * THIS FUNCTION TELLS THE ROUTER THAT THE CUSTOMER WISHES TO    * 00003520
      * EXECUTE A SERIES OF COMMAND LINE CHANGES THAT ARE STORED      * 00003530
      * IN THE STACK COMMAND DATASET (THIS IS A FUTURE FUNCTION)      * 00003540
      *---------------------------------------------------------------* 00003550
                                                                        00003560
           MOVE 'GO'   TO COMM-SYSTEM-CODE.                             00003570
           MOVE SPACES TO COMM-ACTION-CODE.                             00003580
           MOVE 'Y'    TO COMM-CMDLINE-CHANGED.                         00003590
           GO TO 0110-INITIATE-ROUTER.                                  00003600
                                                                        00003610
       0070-RETURN-TO-BROWSE.                                           00003620
                                                                        00003630
           MOVE SPACES   TO COMM-SYSTEM-CODE.
           MOVE SPACES   TO COMM-ACTION-CODE.
           MOVE SPACES   TO COMM-CUSTOMER-INFO.

           MOVE 'NAM0'            TO COMM-PREVIOUS-TRANID.
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

           EXEC CICS START TRANSID('NAM0')
                           TERMID (EIBTRMID)
                           RESP   (WS-CICS-RESP)
                           END-EXEC
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           EXEC CICS RETURN
                     END-EXEC.
           MOVE 'NA'   TO COMM-SYSTEM-CODE.                             00003640
           MOVE 'RB'   TO COMM-ACTION-CODE.                             00003650
           MOVE 'Y'    TO COMM-CMDLINE-CHANGED.                         00003660
           GO TO 0110-INITIATE-ROUTER.                                  00003670
                                                                        00003680
       0080-UPDATE-QUEUE-AREA.                                          00003690
                                                                        00003700
           IF MAP-SYSTEM-CDF = HEX80                                    00003710
               MOVE SPACES TO COMM-SYSTEM-CODE                          00003720
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED                      00003730
           ELSE                                                         00003740
               IF MAP-SYSTEM-CDL > ZERO                                 00003750
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED           00003760
                  MOVE MAP-SYSTEM-CDI TO COMM-SYSTEM-CODE.              00003770
                                                                        00003780
           IF MAP-ACTION-CDF = HEX80                                    00003790
               MOVE SPACES TO COMM-ACTION-CODE                          00003800
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED                      00003810
           ELSE                                                         00003820
               IF MAP-ACTION-CDL > ZERO                                 00003830
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED           00003840
                  MOVE MAP-ACTION-CDI TO COMM-ACTION-CODE.              00003850
                                                                        00003860
           IF MAP-CUST-INFOF = HEX80                                    00003870
               MOVE SPACES TO COMM-CUSTOMER-INFO                        00003880
               MOVE 'Y'            TO COMM-KEY-CHANGED                  00003890
           ELSE                                                         00003900
               IF MAP-CUST-INFOL > ZERO                                 00003910
                  MOVE 'Y'            TO COMM-KEY-CHANGED               00003920
                  MOVE MAP-CUST-INFOI TO COMM-CUSTOMER-INFO.            00003930
                                                                        00003940
       0100-SEND-NA300M1-MENU.                                          00003950
                                                                        00003960
           DISPLAY '0100-SEND-NA300M1-MENU'.

           PERFORM 0190-CHECK-ERROR-MESSAGES                            00003970
               VARYING ACTION-SUB FROM 1 BY 1                           00003980
                   UNTIL ACTION-SUB > 4.                                00003990
                                                                        00004000
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00004010
               MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                    00004020
                                                                        00004030
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00004040
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00004050
           MOVE -1      TO MAP-SYSTEM-CDL.                              00004060
           MOVE '01'    TO COMM-NEXT-FUNCTION.                          00004070
           MOVE 'NAM0'             TO COMM-PREVIOUS-TRANID.             00004080
                                                                        00004090
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00004100
                               FROM   (WS-TS-QUEUE)                     00004110
                               LENGTH (WS-ZERO-LENGTH)                  00004120
                               ITEM   (WS-TS-ITEM)                      00004130
                               RESP   (WS-CICS-RESP)                    00004140
                               REWRITE                                  00004150
                               MAIN                                     00004160
                               END-EXEC.                                00004170
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME
           DISPLAY 'WS-CICS-RESP:' WS-CICS-RESP
                                                                        00004180
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00004190
                GO TO 9999-CICS-ERROR.                                  00004200
                                                                        00004210
                                                                        00004220
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00004230
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00004240
               MOVE SPACES  TO COMM-SYSTEM-CODE                         00004250
           ELSE                                                         00004260
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00004270
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00004280
               MOVE ALL '_' TO MAP-ACTION-CDO                           00004290
               MOVE SPACES  TO COMM-ACTION-CODE                         00004300
           ELSE                                                         00004310
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00004320
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00004330
               MOVE ALL '_' TO MAP-CUST-INFOO                           00004340
               MOVE SPACES  TO COMM-CUSTOMER-INFO                       00004350
           ELSE                                                         00004360
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00004370
                                                                        00004380
      *    EXEC CICS ASSIGN APPLID(CAPPLID) END-EXEC.                   00004390
                                                                        00004400
      *    IF  (CAPPLID IS EQUAL TO 'CICST6TC' OR                       00004410
      *                             'CICSH70P' OR                       00004420
      *                             'CICSH70T' OR                       00004430
      *                             'CICSMG1T' OR                       00004440
      *                             'CICSMG1P')                         00004450
MANJU          MOVE 'NA'          TO MAP-CODEO (ACTION-SUB)             00004460
MANJU          MOVE 'NAME AND ADDRESS'  TO MAP-DESCO (ACTION-SUB)       00004470
               ADD 1 TO ACTION-SUB                                      00004480
MANJU *        MOVE 'SC'          TO MAP-CODEO (ACTION-SUB)             00004490
MANJU *        MOVE 'SCORE/CARD MENU' TO MAP-DESCO (ACTION-SUB)         00004500
               GO TO 0100-AFTER-8991-CALL                               00004510
      *    END-IF.                                                      00004520
                                                                        00004530
           MOVE 'T' TO WT-C8991-REQUEST.                                00004540
           MOVE WT-CNTL8991 TO TURBO-CNTL-AREA.                         00004550
           DISPLAY 'GSF CALL START'.
           PERFORM 10000-CALL-GSF.                                      00004560
           DISPLAY 'GSF CALL END'.
           MOVE TURBO-CNTL-AREA TO WT-CNTL8991.                         00004570
           MOVE 'D' TO WT-C8991-REQUEST.                                00004580
           MOVE 1 TO ACTION-SUB.                                        00004590
           PERFORM 0220-RETRIEVE-SYSTEM-CODES THRU 0220-EXIT            00004600
              UNTIL WT-C8991-RETURN NOT = ZEROS  OR                     00004610
                  ACTION-SUB > 45.                                      00004620
                                                                        00004630
       0100-AFTER-8991-CALL.                                            00004640
                                                                        00004650
           DISPLAY '0100-AFTER-8991-CALL'.

           EXEC CICS SEND MAP    ('NA300M1')                            00004660
                          CURSOR                                        00004670
                          ERASE                                         00004680
                          RESP (WS-CICS-RESP)                           00004690
                          END-EXEC.                                     00004700

           DISPLAY 'WS-CICS-RESP:' WS-CICS-RESP.
                                                                        00004710
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00004720
                GO TO 9999-CICS-ERROR.                                  00004730
                                                                        00004740
           EXEC CICS RETURN TRANSID ('NAM0')                            00004750
                            END-EXEC.                                   00004760
                                                                        00004770
       0110-INITIATE-ROUTER.                                            00004780
                                                                        00004790
MANJU *    EXEC CICS ASSIGN APPLID(CAPPLID) END-EXEC.                   00004800
                                                                        00004810
      *    IF  (CAPPLID IS EQUAL TO 'CICSMG1T' OR                       00004820
      *                             'CICSMG1P') AND                     00004830
      *         MAP-SYSTEM-CDO IS NOT EQUAL TO 'MG' AND 'SC'            00004840
      *         MOVE 'NA999' TO WS-MESSAGE-NUMBER                       00004850
      *         GO TO 0160-WRONG-KEY-HIT1                               00004860
      *    END-IF.                                                      00004870
                                                                        00004880
      *    IF  (CAPPLID IS EQUAL TO 'CICSHADT' OR                       00004890
      *                             'CICSHADP') AND                     00004900
      *         MAP-SYSTEM-CDO IS EQUAL TO 'MG'                         00004910
      *         MOVE ATTRB-UNPROT-FSET  TO MAP-SYSTEM-CDA               00004920
      *         MOVE 'NA999' TO WS-MESSAGE-NUMBER                       00004930
      *         GO TO 0160-WRONG-KEY-HIT1                               00004940
MANJU *    END-IF.                                                      00004950
                                                                        00004960
           MOVE 'NAM0'            TO COMM-PREVIOUS-TRANID.              00004970
           MOVE '00'              TO COMM-NEXT-FUNCTION.                00004980
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT               00004990
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.              00005000
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00005010
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00005020
                                                                        00005030
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00005040
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00005050
                                                                        00005060
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00005070
                               FROM   (WS-TS-QUEUE)                     00005080
                               LENGTH (WS-ZERO-LENGTH)                  00005090
                               ITEM   (WS-TS-ITEM)                      00005100
                               RESP   (WS-CICS-RESP)                    00005110
                               REWRITE                                  00005120
                               MAIN                                     00005130
                               END-EXEC.                                00005140
                                                                        00005150
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00005160
                GO TO 9999-CICS-ERROR.                                  00005170
                                                                        00005180
MANJU *    EXEC CICS SEND MAP ('SYSBUSY')                               00005190
      *                   RESP (WS-CICS-RESP)                           00005200
      *                   END-EXEC.                                     00005210
                                                                        00005220
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00005230
      *         GO TO 9999-CICS-ERROR.                                  00005240
                                                                        00005250
           EXEC CICS START TRANSID('NAM1')                              00005260
                           TERMID (EIBTRMID)                            00005270
                           RESP   (WS-CICS-RESP)                        00005280
                           END-EXEC.                                    00005290
                                                                        00005300
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00005310
                GO TO 9999-CICS-ERROR.                                  00005320
                                                                        00005330
           EXEC CICS RETURN                                             00005340
                     END-EXEC.                                          00005350
                                                                        00005360
       0115-INITIATE-NA300.                                             00005370
                                                                        00005460
           EXEC CICS START TRANSID('NAM0')                              00005470
                           TERMID (EIBTRMID)                            00005480
                           RESP   (WS-CICS-RESP)                        00005490
                           END-EXEC.                                    00005500
                                                                        00005510
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00005520
                GO TO 9999-CICS-ERROR.                                  00005530
                                                                        00005540
           EXEC CICS RETURN                                             00005550
                     END-EXEC.                                          00005560
                                                                        00005570
      *---------------------------------------------------------------* 00005580
      *   THE FOLLOWING CODE EXECUTES THE PSI TUTORIAL OR ONLINE HELP * 00005590
      *   FUNCTION  IT STORES ANY CHANGES MADE ON THE MAP IN A        * 00005600
      *   TEMPORARY STORAGE QUEUE, THEN IT INITIATES THE TUTORIAL     * 00005610
      *   COMMAREA AND XCNTL'S TO THE TUTORIAL PROGRAM                * 00005620
      *---------------------------------------------------------------* 00005630
                                                                        00005640
                                                                        00005650
       0120-PROCESS-TUTORIAL-REQUEST.                                   00005660
            MOVE ALL LOW-VALUES TO NA300M1I.                            00005670
           EXEC CICS RECEIVE MAP    ('NA300M1')                         00005680
                             RESP   (WS-CICS-RESP)                      00005690
                             END-EXEC.                                  00005700
                                                                        00005710
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00005720
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00005730
                    GO TO 9999-CICS-ERROR.                              00005740
                                                                        00005750
           MOVE 'NA300M1' TO T3-SCREEN-NAME                             00005760
           MOVE 'Y'       TO T3-COMMAND-LINE                            00005770
           MOVE SPACES    TO T3-CARRIER                                 00005780
           EXEC CICS LINK PROGRAM  ('TU003')                            00005790
                          COMMAREA (T3-COMMAREA)                        00005800
                           LENGTH   (LENGTH OF T3-COMMAREA)             00005810
                          END-EXEC.                                     00005820
                                                                        00005830
           EXEC CICS RETURN TRANSID ('NAM0') END-EXEC.                  00005840
                                                                        00005850
      *---------------------------------------------------------------* 00005860
      *    CODE NO LONGER NEEDED FOR VER. 3.1 TUTORIAL                * 00005870
      *---------------------------------------------------------------* 00005880
      *    MOVE 'TU' TO COMM-NEXT-FUNCTION.                           * 00005890
      *    PERFORM 0080-UPDATE-QUEUE-AREA.                            * 00005900
      *                                                               * 00005910
      *    MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.             * 00005920
      *    MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.             * 00005930
      *                                                               * 00005940
      *    MOVE 'NA300M1'          TO TC-SCREEN-NAME.                 * 00005950
      *    MOVE 'NAM0'             TO TC-TRANSACTION-ID.              * 00005960
      *    MOVE  1                 TO TC-PAGE-NUMBER.                 * 00005970
      *    MOVE 'Y'                TO TC-COMMAND-LINE.                * 00005980
      *    MOVE 'NA300'            TO TC-PROGRAM-NAME.                * 00005990
      *    MOVE EIBCPOSN           TO TC-CURSOR-POSITION              * 00006000
      *                               COMM-CURSOR-POSN.               * 00006010
      *    MOVE SPACE              TO TC-KEY-DATA.                    * 00006020
      *    MOVE COMM-MSG-ID(1)     TO TC-MESSAGE(1).                  * 00006030
      *    MOVE COMM-MSG-ID(2)     TO TC-MESSAGE(2).                  * 00006040
      *    MOVE COMM-MSG-ID(3)     TO TC-MESSAGE(3).                  * 00006050
      *    MOVE COMM-MSG-ID(4)     TO TC-MESSAGE(4).                  * 00006060
      *                                                               * 00006070
      *    IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED = 'Y'    * 00006080
      *        MOVE 'Y' TO COMM-KEY-CHANGED                           * 00006090
      *        MOVE 'Y' TO COMM-CMDLINE-CHANGED.                      * 00006100
      *                                                               * 00006110
      *    EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)              * 00006120
      *                        FROM   (WS-TS-QUEUE)                   * 00006130
      *                        LENGTH (WS-COMM-LENGTH)                * 00006140
      *                        ITEM   (WS-TS-ITEM)                    * 00006150
      *                        RESP   (WS-CICS-RESP)                  * 00006160
      *                        REWRITE                                * 00006170
      *                        MAIN                                   * 00006180
      *                        END-EXEC.                              * 00006190
      *                                                               * 00006200
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                      * 00006210
      *         GO TO 9999-CICS-ERROR.                                * 00006220
      *                                                               * 00006230
      *    EXEC CICS XCTL PROGRAM  ('TU001')                          * 00006240
      *                   COMMAREA (TUTORIAL-COMM-AREA)               * 00006250
      *                   LENGTH   (WS-TUTOR-COMM-LENGTH)             * 00006260
      *                   END-EXEC.                                   * 00006270
      *---------------------------------------------------------------* 00006280
      *  CONTROL WAS RETURNED FROM THE TUTORIAL PROGRAM. READ THE     * 00006290
      *  'HELP' TS QUEUE AND DISPLAY THE MAP AS IT APPEARED PRIOR     * 00006300
      *  TO GOING TO THE HELP FACILITY.                               * 00006310
      *---------------------------------------------------------------* 00006320
                                                                        00006330
      *---------------------------------------------------------------* 00006340
      *0130-RETURN-FROM-TUTORIAL.                                     * 00006350
      *                                                               * 00006360
      *    MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.         * 00006370
      *    MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.         * 00006380
      *    IF  EIBAID = DFHCLEAR                                      * 00006390
      *        PERFORM 0140-PROCESS-SCREEN-CLEAR.                     * 00006400
      *                                                               * 00006410
      *                                                               * 00006420
      *    MOVE '01' TO COMM-NEXT-FUNCTION.                           * 00006430
      *    IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                 * 00006440
      *        MOVE ALL '_' TO MAP-SYSTEM-CDO                         * 00006450
      *    ELSE                                                       * 00006460
      *       MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.              * 00006470
      *    IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                 * 00006480
      *        MOVE ALL '_' TO MAP-ACTION-CDO                         * 00006490
      *    ELSE                                                       * 00006500
      *       MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.              * 00006510
      *    IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES               * 00006520
      *        MOVE ALL '_' TO MAP-CUST-INFOO                         * 00006530
      *    ELSE                                                       * 00006540
      *       MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.              * 00006550
      *                                                               * 00006560
      *    EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)              * 00006570
      *                        FROM   (WS-TS-QUEUE)                   * 00006580
      *                        LENGTH (WS-COMM-LENGTH)                * 00006590
      *                        ITEM   (WS-TS-ITEM)                    * 00006600
      *                        RESP   (WS-CICS-RESP)                  * 00006610
      *                        REWRITE                                * 00006620
      *                        MAIN                                   * 00006630
      *                        END-EXEC.                              * 00006640
      *                                                               * 00006650
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                      * 00006660
      *         GO TO 9999-CICS-ERROR.                                * 00006670
      *                                                               * 00006680
      *    PERFORM 0190-CHECK-ERROR-MESSAGES                          * 00006690
      *        VARYING ACTION-SUB FROM 1 BY 1                         * 00006700
      *            UNTIL ACTION-SUB > 4.                              * 00006710
      *                                                               * 00006720
      *    MOVE 'T' TO WT-C8991-REQUEST.                              * 00006730
      *    MOVE WT-CNTL8991 TO TURBO-CNTL-AREA.                       * 00006740
      *    PERFORM 10000-CALL-GSF.                                    * 00006750
      *    MOVE TURBO-CNTL-AREA TO WT-CNTL8991.                       * 00006760
      *    MOVE 'D' TO WT-C8991-REQUEST.                              * 00006770
      *    MOVE 1 TO ACTION-SUB.                                      * 00006780
      *    PERFORM 0220-RETRIEVE-SYSTEM-CODES THRU 0220-EXIT          * 00006790
      *       UNTIL WT-C8991-RETURN NOT = ZEROS  OR                   * 00006800
      *           ACTION-SUB > 45.                                    * 00006810
      *                                                               * 00006820
      *    EXEC CICS SEND MAP    ('NA300M1')                          * 00006830
      *                   CURSOR (COMM-CURSOR-POSN)                   * 00006840
      *                   RESP   (WS-CICS-RESP)                       * 00006850
      *                   ERASE                                       * 00006860
      *                   END-EXEC.                                   * 00006870
      *                                                               * 00006880
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                      * 00006890
      *         GO TO 9999-CICS-ERROR.                                * 00006900
      *                                                               * 00006910
      *    EXEC CICS RETURN TRANSID ('NAM0')                          * 00006920
      *                     END-EXEC.                                 * 00006930
      *                                                               * 00006940
      *    GOBACK.                                                    * 00006950
      *---------------------------------------------------------------* 00006960
                                                                        00006970
                                                                        00006980
       0140-PROCESS-SCREEN-CLEAR.                                       00006990
                                                                        00007000
           MOVE 'NAM0'            TO COMM-PREVIOUS-TRANID.              00007010
           MOVE '00'              TO COMM-NEXT-FUNCTION.                00007020
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT               00007030
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.              00007040
           MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                        00007050
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00007060
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00007070
                                                                        00007080
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00007090
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00007100
                                                                        00007110
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00007120
                               FROM   (WS-TS-QUEUE)                     00007130
                               LENGTH (WS-ZERO-LENGTH)                  00007140
                               ITEM   (WS-TS-ITEM)                      00007150
                               RESP   (WS-CICS-RESP)                    00007160
                               REWRITE                                  00007170
                               MAIN                                     00007180
                               END-EXEC.                                00007190
                                                                        00007200
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00007210
                GO TO 9999-CICS-ERROR.                                  00007220
                                                                        00007230
                                                                        00007240
MANJU *    EXEC CICS ASSIGN TCTUALENG(TCTUAL) END-EXEC.                 00007250
      *    EXEC CICS ADDRESS TCTUA(ADDRESS OF TCTUAR) END-EXEC.         00007260
           MOVE TCTUA-DEMO-DATA TO WS-DEMOGRAPHIC-COMM-AREA.            00007270
                                                                        00007280
           IF COMM-EXTERNAL-CUST = 'Y'                                  00007290
               IF WS-DEMO-LID-1-4 = 'DPAD'                              00007300
                   EXEC CICS SEND FROM(DFHNULL) LENGTH(1) END-EXEC      00007310
                   EXEC CICS RETURN END-EXEC                            00007320
               ELSE                                                     00007330
                   PERFORM 8999-DISCONNECT-TERMINAL THRU 8999-EXIT      00007340
           ELSE                                                         00007350
               EXEC CICS SEND FROM(DFHNULL) LENGTH(1) END-EXEC          00007360
               EXEC CICS RETURN END-EXEC.                               00007370
                                                                        00007380
                                                                        00007390
                                                                        00007400
                                                                        00007410
       0150-WRITE-NAQ1-QUEUE.                                           00007420
                                                                        00007430
           DISPLAY '0150-WRITE-NAQ1-QUEUE'.

           MOVE SPACES TO WSQ-COMMAREA.                                 00007440
           MOVE ZEROES TO WSQ-CURSOR-POSN WSQ-MSG-COUNT                 00007450
               WSQ-MAIL-LINE-COUNT WSQ-CUST-TOKEN-COUNT.                00007460
           MOVE +600 TO WSQ-CICS-COMMAREA-LENGTH.                       00007470
           MOVE '00' TO WSQ-NEXT-FUNCTION.                              00007480
      *    PERFORM 0155-CHECK-FOR-EXTERNAL-CUST THRU 0155-EXIT.         00007490
                                                                        00007500
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00007510
                               FROM   (WSQ-COMMAREA)                    00007520
                               LENGTH (WS-COMM-LENGTH)                  00007530
                               ITEM   (WS-TS-ITEM)                      00007540
                               RESP   (WS-CICS-RESP)                    00007550
                               MAIN                                     00007560
                               END-EXEC.                                00007570
                                                                        00007580
           DISPLAY '0150 WS-CICS-RESP:' WS-CICS-RESP
                                                                        00007590
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00007600
                GO TO 9999-CICS-ERROR.                                  00007610
                                                                        00007620
           MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.           00007630
           MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.           00007640
           EXEC CICS READQ TS QUEUE  (WS-TS-QUEUE-NAME)                 00007650
                              SET    (ADDRESS OF WS-LINK-STORAGE)       00007660
                              LENGTH (WS-ZERO-LENGTH)                   00007670
                              ITEM   (WS-TS-ITEM)                       00007680
                              RESP   (WS-CICS-RESP)                     00007690
                              END-EXEC.                                 00007700
                                                                        00007710
           DISPLAY '0150 WS-CICS-RESP:' WS-CICS-RESP
           DISPLAY 'WS-ZERO-LENGTH:' WS-ZERO-LENGTH
                                                                        00007720
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00007730
                GO TO 9999-CICS-ERROR.                                  00007740
                                                                        00007750
           MOVE WS-ZERO-LENGTH TO WS-ZERO-LENGTH.                       00007760
           COMPUTE WS-LINK-LENGTH = (WS-ZERO-LENGTH - +600).            00007770
           DISPLAY 'WS-LINK-LENGTH:' WS-LINK-LENGTH
           MOVE WS-LINK-STORAGE TO WS-TS-QUEUE.                         00007780
                                                                        00007790
                                                                        00007800
                                                                        00007810
      *0155-CHECK-FOR-EXTERNAL-CUST.                                    00007820
                                                                        00007830
      *    DISPLAY '0155-CHECK-FOR-EXTERNAL-CUST'.
MANJU *    EXEC CICS ASSIGN TCTUALENG(TCTUAL) END-EXEC.                 00007840
      *    EXEC CICS ADDRESS TCTUA(ADDRESS OF TCTUAR) END-EXEC.         00007850
      *    MOVE TCTUA-DEMO-DATA TO WS-DEMOGRAPHIC-COMM-AREA.            00007860
                                                                        00007870
      *    MOVE WS-DEMO-LOGONID TO WS-LOGON-HOLD.                       00007880
      *    DISPLAY 'WS-LOGON-HOLD:' WS-LOGON-HOLD
                                                                        00007890
      *    EXEC SQL                                                     00007900
      *         SELECT AGNT#IDNTITY                                     00007910
      *         INTO :LID-AGNT-IDNTITY                                  00007920
      *         FROM ECLIDV01                                           00007930
      *         WHERE LOGONID = :WS-LOGON-HOLD                          00007940
      *           AND STATUS = :WS-A                                    00007950
      *    END-EXEC.                                                    00007960
                                                                        00007970
MANJU *    DISPLAY 'SQLCODE:' SQLCODE
      *    DISPLAY 'SQLERRM:' SQLERRM
      *    DISPLAY 'SQLERRML:' SQLERRML
      *    DISPLAY 'SQLERRMC:' SQLERRMC
      *    DISPLAY 'SQLERRP:' SQLERRP
      *    DISPLAY 'SQL-ERROR-MESSAGE:' SQL-ERROR-MESSAGE
MANJU *    MOVE 0 TO SQLCODE
      *    IF SQLCODE = 0                                               00007980
      *        MOVE 'Y' TO COMM-EXTERNAL-CUST                           00007990
      *      ELSE                                                       00008000
      *        IF SQLCODE = +100                                        00008010
      *            MOVE ' ' TO COMM-EXTERNAL-CUST                       00008020
      *          ELSE                                                   00008030
      *            MOVE '0155-CHECK-FOR-EXTERNAL' TO WS-SQL-PARA        00008040
      *            PERFORM 9999-DB2-ERRORS.                             00008050
      *                                                                 00008060
      *    DISPLAY 'COMM-EXTERNAL-CUST:' COMM-EXTERNAL-CUST.
      *                                                                 00008070
                                                                        00008080
      *0155-EXIT.                                                       00008090
      *    EXIT.                                                        00008100
                                                                        00008110
                                                                        00008120
                                                                        00008130
       0160-WRONG-KEY-HIT.                                              00008140
            MOVE ALL LOW-VALUES TO NA300M1I.                            00008150
           EXEC CICS RECEIVE MAP    ('NA300M1')                         00008160
                             RESP (WS-CICS-RESP)                        00008170
                             END-EXEC.                                  00008180
                                                                        00008190
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00008200
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00008210
                GO TO 9999-CICS-ERROR.                                  00008220
                                                                        00008230
           MOVE 'NA030'           TO WS-MESSAGE-NUMBER.                 00008240
                                                                        00008250
       0160-WRONG-KEY-HIT1.                                             00008260
                                                                        00008270
           PERFORM 0080-UPDATE-QUEUE-AREA.                              00008280
                                                                        00008290
           PERFORM 0180-BUMP-ERROR-MESSAGES.                            00008300
           MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(1).                    00008310
           MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(2).                    00008320
           MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(3).                    00008330
           MOVE WSQ-MSG-ID(4)     TO COMM-MSG-ID(4).                    00008340
                                                                        00008350
           MOVE EIBCPOSN           TO COMM-CURSOR-POSN.                 00008360
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00008370
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00008380
                                                                        00008390
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED = 'Y'      00008400
               MOVE 'Y' TO COMM-KEY-CHANGED                             00008410
               MOVE 'Y' TO COMM-CMDLINE-CHANGED.                        00008420
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00008430
                               FROM   (WS-TS-QUEUE)                     00008440
                               LENGTH (WS-COMM-LENGTH)                  00008450
                               ITEM   (WS-TS-ITEM)                      00008460
                               RESP   (WS-CICS-RESP)                    00008470
                               REWRITE                                  00008480
                               MAIN                                     00008490
                               END-EXEC.                                00008500
                                                                        00008510
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00008520
                GO TO 9999-CICS-ERROR.                                  00008530
                                                                        00008540
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00008550
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00008560
           ELSE                                                         00008570
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00008580
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00008590
               MOVE ALL '_' TO MAP-ACTION-CDO                           00008600
           ELSE                                                         00008610
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00008620
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00008630
               MOVE ALL '_' TO MAP-CUST-INFOO                           00008640
           ELSE                                                         00008650
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00008660
                                                                        00008670
      *    IF  (CAPPLID IS EQUAL TO 'CICST6TC' OR                       00008680
      *                             'CICSH70P' OR                       00008690
      *                             'CICSH70T' OR                       00008700
      *                             'CICSMG1P' OR                       00008710
      *                             'CICSMG1T')                         00008720
MANJU          MOVE 'NA'          TO MAP-CODEO (ACTION-SUB)             00008730
               MOVE 'NAME ADDRESS'  TO MAP-DESCO (ACTION-SUB)           00008740
               ADD 1 TO ACTION-SUB                                      00008750
      *        MOVE 'SC'          TO MAP-CODEO (ACTION-SUB)             00008760
      *        MOVE 'Score/Card Menu' TO MAP-DESCO (ACTION-SUB)         00008770
               GO TO 0160-AFTER-8991-CALL.                              00008780
                                                                        00008790
           MOVE 'T' TO WT-C8991-REQUEST.                                00008800
           MOVE WT-CNTL8991 TO TURBO-CNTL-AREA.                         00008810
           PERFORM 10000-CALL-GSF.                                      00008820
           MOVE TURBO-CNTL-AREA TO WT-CNTL8991.                         00008830
           MOVE 'D' TO WT-C8991-REQUEST.                                00008840
           MOVE 1 TO ACTION-SUB.                                        00008850
           PERFORM 0220-RETRIEVE-SYSTEM-CODES THRU 0220-EXIT            00008860
              UNTIL WT-C8991-RETURN NOT = ZEROS  OR                     00008870
                  ACTION-SUB > 45.                                      00008880
                                                                        00008890
       0160-AFTER-8991-CALL.                                            00008900
                                                                        00008910
           EXEC CICS SEND MAP    ('NA300M1')                            00008920
                          CURSOR (COMM-CURSOR-POSN)                     00008930
                          RESP   (WS-CICS-RESP)                         00008940
                          ERASE                                         00008950
                          END-EXEC.                                     00008960
                                                                        00008970
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00008980
                GO TO 9999-CICS-ERROR.                                  00008990
                                                                        00009000
           EXEC CICS RETURN TRANSID ('NAM0')                            00009010
                            END-EXEC.                                   00009020
                                                                        00009030
       0170-RETURN-TO-MT-BILLION.                                       00009040
                                                                        00009050
           MOVE 'NAM0'            TO COMM-PREVIOUS-TRANID.              00009060
           MOVE '00'              TO COMM-NEXT-FUNCTION.                00009070
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT               00009080
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.              00009090
           MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                        00009100
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00009110
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00009120
                                                                        00009130
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00009140
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00009150
                                                                        00009160
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00009170
                               FROM   (WS-TS-QUEUE)                     00009180
                               LENGTH (WS-ZERO-LENGTH)                  00009190
                               ITEM   (WS-TS-ITEM)                      00009200
                               RESP   (WS-CICS-RESP)                    00009210
                               REWRITE                                  00009220
                               MAIN                                     00009230
                               END-EXEC.                                00009240
                                                                        00009250
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00009260
                GO TO 9999-CICS-ERROR.                                  00009270
                                                                        00009280
      *    EXEC CICS SEND MAP  ('SYSBUSY')                              00009290
      *                  RESP (WS-CICS-RESP)                            00009300
      *                  END-EXEC.                                      00009310
                                                                        00009320
      *   IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                         00009330
      *        GO TO 9999-CICS-ERROR.                                   00009340
                                                                        00009350
           EXEC CICS START TRANSID('PA3')                               00009360
                           RESP (WS-CICS-RESP)                          00009370
                           TERMID (EIBTRMID)                            00009380
                           END-EXEC.                                    00009390
                                                                        00009400
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00009410
                GO TO 9999-CICS-ERROR.                                  00009420
                                                                        00009430
           EXEC CICS RETURN                                             00009440
                     END-EXEC.                                          00009450
                                                                        00009460
       0180-BUMP-ERROR-MESSAGES.                                        00009470
                                                                        00009480
           MOVE COMM-MSG-ID(1)    TO WSQ-MSG-ID(1).                     00009490
           MOVE COMM-MSG-ID(2)    TO WSQ-MSG-ID(2).                     00009500
           MOVE COMM-MSG-ID(3)    TO WSQ-MSG-ID(3).                     00009510
           MOVE COMM-MSG-ID(4)    TO WSQ-MSG-ID(4).                     00009520
           MOVE WS-MESSAGE-NUMBER TO COMM-MSG-ID(1).                    00009530
           MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(2).                    00009540
           MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(3).                    00009550
           MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(4).                    00009560
           PERFORM 0190-CHECK-ERROR-MESSAGES                            00009570
               VARYING ACTION-SUB FROM 1 BY 1                           00009580
                   UNTIL ACTION-SUB > 4.                                00009590
                                                                        00009600
       0190-CHECK-ERROR-MESSAGES.                                       00009610
                                                                        00009620
           IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND                 00009630
               COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES                00009640
               MOVE COMM-MSG-ID (ACTION-SUB) TO WT-C9999-ERROR-CODE     00009650
               PERFORM 0210-GET-ERROR-MESSAGE                           00009660
               PERFORM 0200-CHECK-RETURN-CODE.                          00009670
                                                                        00009680
       0200-CHECK-RETURN-CODE.                                          00009690
                INITIALIZE WS-ERROR-FIELDS.                             00009700
               IF WT-C9999-RETURN = ZERO                                00009710
                   MOVE WT-C9999-ERROR-CODE TO WS-C9999-ERROR-CODE      00009720
                   MOVE WT-C9999-SEVERITY-CODE TO WS-C9999-SEVERITY-CODE00009730
                   MOVE WT-C9999-ERROR-MESSAGE TO WS-C9999-ERROR-MESSAGE00009740
                   MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (ACTION-SUB)  00009750
               ELSE                                                     00009760
                  IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND          00009770
                      COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES         00009780
                      MOVE '** INVALID ERROR MESSAGE ** ' TO            00009790
                           MAP-ERROR-MSGO (ACTION-SUB).                 00009800
                                                                        00009810
       0210-GET-ERROR-MESSAGE.                                          00009820
                                                                        00009830
           MOVE WT-CNTL9999 TO TURBO-CNTL-AREA.                         00009840
           PERFORM 10000-CALL-GSF.                                      00009850
           MOVE TURBO-CNTL-AREA TO WT-CNTL9999.                         00009860
                                                                        00009870
       0220-RETRIEVE-SYSTEM-CODES.                                      00009880
                                                                        00009890
           MOVE WT-CNTL8991 TO TURBO-CNTL-AREA.                         00009900
           PERFORM 10000-CALL-GSF.                                      00009910
           MOVE TURBO-CNTL-AREA TO WT-CNTL8991.                         00009920
                                                                        00009930
           IF WT-C8991-RETURN > ZEROS                                   00009940
              MOVE 46 TO ACTION-SUB                                     00009950
                  GO TO 0220-EXIT.                                      00009960
                                                                        00009970
           IF WT-C8991-ACTION-CD > SPACES OR                            00009980
              WT-C8991-DISP-ON-MAIN NOT = 'Y'                           00009990
                  GO TO 0220-EXIT.                                      00010000
                                                                        00010010
      *    IF COMM-EXTERNAL-CUST = 'Y'                                  00010020
      *        PERFORM 0300-VERIFY-ECI-SYSTEM-CODES THRU 0300-EXIT      00010030
      *        IF NOT VALID-ECI-CODE                                    00010040
      *          GO TO 0220-EXIT.                                       00010050
                                                                        00010060
           MOVE WT-C8991-SYSTEM-CD   TO MAP-CODEO (ACTION-SUB).         00010070
           MOVE WT-C8991-DESCRIPTION TO MAP-DESCO (ACTION-SUB).         00010080
                                                                        00010090
           ADD 1 TO ACTION-SUB.                                         00010100
                                                                        00010110
       0220-EXIT.                                                       00010120
           EXIT.                                                        00010130
                                                                        00010140
                                                                        00010150
                                                                        00010160
      *0300-VERIFY-ECI-SYSTEM-CODES.                                    00010170
      *                                                                 00010180
      *    MOVE SPACE TO WS-VALID-ECI-CODE-SW.                          00010190
      *    MOVE WS-DEMO-LOGONID TO WS-LOGON-HOLD.                       00010200
      *                                                                 00010210
      *    EXEC SQL                                                     00010220
      *         SELECT ECLIDV01.AGNT#IDNTITY, SYSTEM_ID                 00010230
      *         INTO :LID-AGNT-IDNTITY, :MTX-SYSTEM-ID                  00010240
      *         FROM ECLIDV01, MTRXV01                                  00010250
      *         WHERE LOGONID = :WS-LOGON-HOLD                          00010260
      *           AND STATUS = :WS-A                                    00010270
      *           AND ECLIDV01.AGNT#IDNTITY = MTRXV01.AGNT#IDNTITY      00010280
      *           AND SYSTEM_ID = :WT-C8991-SYSTEM-CD                   00010290
      *           AND ACTION_CODE = :WS-SPACES                          00010300
      *           AND (CURRENT DATE BETWEEN EFFECTIVE_DATE              00010310
      *                AND TERMINATION_DATE)                            00010320
      *    END-EXEC.                                                    00010330
      *                                                                 00010340
      *    IF SQLCODE = 0                                               00010350
      *        MOVE 'Y' TO WS-VALID-ECI-CODE-SW                         00010360
      *      ELSE                                                       00010370
      *        IF SQLCODE = +100                                        00010380
      *            NEXT SENTENCE                                        00010390
      *          ELSE                                                   00010400
      *            MOVE '0300-VERIFY-ECI-CODE' TO WS-SQL-PARA           00010410
      *            PERFORM 9999-DB2-ERRORS.                             00010420
      *                                                                 00010430
      *                                                                 00010440
      *                                                                 00010450
      *0300-EXIT.                                                       00010460
      *    EXIT.                                                        00010470
                                                                        00010480
                                                                        00010490
       8999-DISCONNECT-TERMINAL.                                        00010500
                                                                        00010510
           EXEC CICS SEND FROM(DFHNULL) LENGTH(1) END-EXEC.             00010520
                                                                        00010530
      *                                                                 00010540
      * SIGN OFF THE TERMINAL IF IT IS ALREADY SIGNED ON                00010550
      *                                                                 00010560
           MOVE 'L' TO UC-REQUEST-CODE.                                 00010570
                                                                        00010580
MANJU *    EXEC CICS LINK PROGRAM('PSIACFUC')                           00010590
      *              COMMAREA(PGM-CONTROL-BLOCK)                        00010600
      ***             LENGTH(486)                                       00010610
      *               LENGTH(LENGTH OF PGM-CONTROL-BLOCK)               00010620
MANJU *              END-EXEC.                                          00010630
                                                                        00010640
           EXEC CICS RETURN END-EXEC.                                   00010650
                                                                        00010660
       8999-EXIT.  EXIT.                                                00010670
                                                                        00010680
                                                                        00010690
      *---------------------------------------------------------------* 00010700
      * THIS ROUTINE TRANSFERS CONTROL TO AN ONLINE ERROR PROGRAM     * 00010710
      * WHICH DISPLAYS THE ERRORS ON THE SCREEN SUPPLYING INFORMATION * 00010720
      * FOR THE HELP-DESK  TO AID PROGRAMMERS IN DEBUGGING.           * 00010730
      *---------------------------------------------------------------* 00010740
                                                                        00010750
           COPY CICSERR.                                                00010760
                                                                        00010770
                                                                        00010780
                                                                        00010790
       9999-DB2-ERRORS.                                                 00010800
                                                                        00010810
      *    MOVE WS-SQL-ERROR-MSG TO SQL-ERROR-MESSAGE.                  00010820
                                                                        00010830
           EXEC CICS SYNCPOINT                                          00010840
                     ROLLBACK                                           00010850
           END-EXEC.                                                    00010860
                                                                        00010870
MANJU *    EXEC CICS XCTL                                               00010880
      *              PROGRAM  ('SQLERRTN')                              00010890
      *              COMMAREA (SQLCA)                                   00010900
      ***             LENGTH   (214)                                    00010910
      *               LENGTH   (LENGTH OF SQLCA)                        00010920
      *    END-EXEC.                                                    00010930
                                                                        00010940
       9999-EXIT.                                                       00010950
           EXIT.                                                        00010960
                                                                        00010970
      *---------------------------------------------------------------* 00010980
      * THIS ROUTINE TRANSFERS CONTROL TO THE TURBO TABLES FOR EDIT   * 00010990
      * VERIFICATION                                                  * 00011000
      *---------------------------------------------------------------* 00011010
                                                                        00011020
           COPY TURBOCAL.                                               00011030
