       IDENTIFICATION DIVISION.                                         00000010
       PROGRAM-ID. NA301.                                               00000020
                                                                        00000030
 INFO *---------------START OF INFORMATION BLOCK----------------------* 00000040
 INFO *---------------------------------------------------------------* 00000050
 INFO * PLACE AN 'X' NEXT TO THE OPTION THAT MATCHES YOUR NEEDS FOR   * 00000060
 INFO * THIS PROGRAM.                                                 * 00000070
 INFO *---------------------------------------------------------------* 00000080
           EJECT                                                        00000270
                                                                        00000280
       DATE-WRITTEN.                                                    00000290
      *REMARKS. NA301 IS THE CUSTOMER INFORMATION MANAGER'S ACTION CODE 00000300
      *         MENU. BY ENTERING ONLY THE SYSTEM TYPE THE MENU RETURNS 00000310
      *         ALL VALID ACTION CODES.                                 00000320
                                                                        00000330
      *------------------------PROGRAM PURPOSE-------------------------*00000340
      *                                                                *00000350
      *  PROGRAM TITLE: NA301                                          *00000360
      *  PROGRAM TEXT:                                                 *00000370
      *                                                                *00000380
      *------------------------PROGRAM FILES---------------------------*00000390
      *                                                                *00000400
      * DLBL    FILE    FILE   FILE    LIBRARIAN   FILE   BRIEF FILE   *00000410
      * NAME    USE     TYPE  ACCESS    INCLUDE   PREFIX  DESCRIPTION  *00000420
      * ----    ----    ----  ------   ---------  ------  ------------ *00000430
      *                                                                *00000440
      *                                                                *00000450
      *------------------------CALL INTERFACES-------------------------*00000460
      * IOMOD -                                                        *00000470
      *                                                                *00000480
      *                                                                *00000490
      *                                                                *00000500
      *----------------------------------------------------------------*00000710
       ENVIRONMENT DIVISION.                                            00000720
       DATA DIVISION.                                                   00000730
       WORKING-STORAGE SECTION.                                         00000740
       77  WS-ISEIB-ROUTINE            PIC X(8)    VALUE 'ISEIB   '.    00000750
       77  WS-PSIGSFC                       PIC X(08) VALUE 'PSIGSFC '. 00000760
       77  WS-ZERO-LENGTH                   PIC S9(4) VALUE +0   COMP.  00000770
       77  WS-COMM-LENGTH                   PIC S9(4) VALUE +600 COMP.  00000780
       77  WS-LINK-LENGTH                   PIC S9(8) VALUE +0   COMP.  00000790
       77  WS-TUTOR-COMM-LENGTH             PIC S9(4) VALUE +906 COMP.  00000800
       77  WS-TS-ITEM                       PIC S9(4) VALUE +1   COMP.  00000810
           COPY AUDCICS.                                                00000820
           COPY CAWSINC.                                                00000830
           COPY DEMOCOMM.                                               00000840
           COPY TURB9999.                                               00000850
           COPY TURB8991.                                               00000860
      *---------------------------------------------------------------* 00000870
      *    COPY TUTORCOM.                                             * 00000880
      *---------------------------------------------------------------* 00000890
           COPY TU003COM.                                               00000900
           COPY CICSWS.                                                 00000910
           COPY TURBINC.                                                00000920
           COPY TURBDATA.                                               00000930
           EJECT                                                        00000940
       01  WS-WORK-AREA.                                                00000950
           05  WS-GASET                     PIC S9(9) COMP.             00000960
           05  WS-GALENGTH                  PIC S9(4) COMP.             00000970
           05  WS-CICS-RESP                 PIC S9(4) COMP.             00000980
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
           05  WS-SYSTEM-CODE.                                          00001160
               10  WS-SYSTEM-CODE-2BYTES    PIC X(2).                   00001170
               10  FILLER                   PIC X(2).                   00001180
           05  WS-ACTION-CODE.                                          00001190
               10  WS-ACTION-CODE-5BYTES    PIC X(5).                   00001200
               10  FILLER                   PIC X.                      00001210
           05  WS-VALID-ECI-CODE-SW         PIC X  VALUE SPACE.         00001220
               88  VALID-ECI-CODE           VALUE 'Y'.                  00001230
           05  WS-LOGON-HOLD                     PIC X(8).              00001240
           05  WS-SQL-ERROR-MSG.                                        00001250
               10  FILLER       PIC X(13)  VALUE '++++++ Error '.       00001260
               10  FILLER       PIC X(18)  VALUE 'Occured in Program'.  00001270
               10  FILLER       PIC X(17)  VALUE ' IS201  -  Para. '.   00001280
               10  WS-SQL-PARA  PIC X(30)  VALUE SPACES.                00001290
           05  WS-A                              PIC X(1) value 'A'.    00001240
                                                                        00001300
       01  TCTUAL                      PIC S9(4) COMP.                  00001310
           88  INVALID-TCTUAL              VALUE 0 THRU 135.            00001320
       01  WS-TRAN-ID                  PIC X(4).                        00001330
       01  WSQ-COMMAREA.                                                00001340
           05  WSQ-CICS-COMMAREA-LENGTH COMP PIC S9(4) VALUE +600.      00001350
           05  WSQ-SYSTEM-CODE              PIC X(02).                  00001360
           05  WSQ-ACTION-CODE              PIC X(05).                  00001370
           05  WSQ-CUSTOMER-INFO            PIC X(40).                  00001380
           05  WSQ-CUST-TOKEN-COUNT         PIC S9.                     00001390
           05  WSQ-CUSTOMER-INFO1           PIC X(40).                  00001400
           05  WSQ-CUSTOMER-INFO2           PIC X(30).                  00001410
           05  WSQ-CUSTOMER-INFO3           PIC X(30).                  00001420
           05  WSQ-CUSTOMER-INFO4           PIC X(30).                  00001430
           05  WSQ-CUSTOMER-INFO5           PIC X(30).                  00001440
           05  WSQ-CUSTOMER-INFO6           PIC X(30).                  00001450
           05  FILLER                       PIC X(46).                  00001460
           05  WSQ-PREVIOUS-TRANID          PIC X(4).                   00001470
           05  WSQ-CMDLINE-CHANGED          PIC X(1).                   00001480
           05  WSQ-KEY-CHANGED              PIC X(1).                   00001490
           05  WSQ-IDNTITY-NUMBER               PIC X(8).               00001500
           05  WSQ-MAIL-LINE-COUNT          PIC 9.                      00001510
           05  WSQ-MAIL-LINE                PIC X(30) OCCURS 5 TIMES.   00001520
           05  WSQ-LAST-ITEM-NUM-BROWSED    PIC S9(3) COMP-3.           00001530
           05  FILLER                       PIC X(38).                  00001540
           05  WSQ-MSG-COUNT                PIC 9.                      00001550
           05  WSQ-MSG-ID                   PIC X(5) OCCURS 4 TIMES.    00001560
           05  WSQ-MSG-MAX-SEVERITY         PIC X(1).                   00001570
           05  WSQ-NEXT-FUNCTION            PIC X(2).                   00001580
           05  WSQ-CURSOR-POSN              PIC S9(4) COMP.             00001590
           05  FILLER                       PIC X(83).                  00001600
           05  WSQ-TUTORIAL-SW              PIC X.                      00001610
                                                                        00001620
       01  WS-TS-QUEUE.                                                 00001630
           COPY NA200C01.                                               00001640
           02  WS-COM-LINK   OCCURS 0 TO 3496 TIMES                     00001650
                             DEPENDING ON WS-LINK-LENGTH                00001660
                                            PIC X.                      00001670
      *---------------------------------------------------------        00001680
      *    GET SQL COMMUNICATION AREA.                                  00001690
      *---------------------------------------------------------        00001700
      *    EXEC SQL INCLUDE SQLCA END-EXEC.                             00001710
      *    05  SQL-ERROR-MESSAGE             PIC X(78) VALUE SPACES.    00001720
                                                                        00001730
                                                                        00001740
      *    COPY SYSBUSY.                                                00001750
           COPY NA300M2.                                                00001760
      *    COPY ECLIDV01.                                               00001770
      *    COPY MTRXV01.                                                00001780
           COPY ECACFC01.                                               00001790
           COPY DFHAID.                                                 00001800
           COPY DFHBMSCA.                                               00001810
                                                                        00001820
       LINKAGE SECTION.                                                 00001830
                                                                        00001840
       01  DFHCOMMAREA.                                                 00001850
                                                                        00001860
           05  FILLER                       PIC X.                      00001870
                                                                        00001880
       01  WS-LINK-STORAGE.                                             00001890
           02  WS-LINK-SIX-HUNDRED          PIC X(600).                 00001900
           02  WS-LINK-STUFF OCCURS 0 TO 3496 TIMES                     00001910
                             DEPENDING ON WS-LINK-LENGTH                00001920
                                            PIC X.                      00001930
                                                                        00001940
       01  TCTUAR.                                                      00001950
           05  FILLER                  PIC X(36).                       00001960
           05  TCTUA-DEMO-DATA.                                         00001970
               10  FILLER              PIC S9(4) COMP.                  00001980
                   88  INVALID-DEMO-DATA               VALUE 0.         00001990
               10  FILLER              PIC X(98).                       00002000
                                                                        00002010
      ******************************************************************00002020
                                                                        00002030
       PROCEDURE DIVISION.                                              00002040
                                                                        00002050
       0010-BEGIN-PROGRAM.                                              00002060
                                                                        00002070
           DISPLAY '0010-BEGIN-PROGRAM:'.
           EXEC CICS HANDLE CONDITION ERROR(9999-CICS-ERROR)            00002080
                                      END-EXEC.                         00002090
                                                                        00002100
           MOVE LOW-VALUES TO NA300M2O.                                 00002110
      *    MOVE SPACES     TO WS-COST-CARR-CODE.                        00002120
           INITIALIZE WS-ERROR-FIELDS.                                  00002130
                                                                        00002140
                                                                        00002150
      *---------------------------------------------------------------* 00002160
      * THIS IS THE COPY BOOK FOR THE COST JOURNAL USED TO DETERMINE  * 00002170
      * CUSTOMER INFORMATION MANAGEMENT CHARGES BY CARRIER.           * 00002180
      *---------------------------------------------------------------* 00002190
                                                                        00002200
           COPY CAPRCINC.                                               00002210
                                                                        00002220
       0020-READ-NAQ1-TS-QUEUE.                                         00002230
                                                                        00002240
      *---------------------------------------------------------------* 00002250
      *    THIS IS THE TEMPORARY STORAGE QUEUE THAT IS MAINTAINED     * 00002260
      *    THROUGH OUT THE ENTIRE     SYSTEM. THE QUEUE IS            * 00002270
      *    ESTABLISHED THE FIRST TIME THE CUSTOMER SIGNS ON           * 00002280
      *    AND IS ONLY DELETED IF CICS COMES DOWN.                    * 00002290
      *---------------------------------------------------------------* 00002300
                                                                        00002310
           MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.           00002320
           MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.           00002330
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME.
           EXEC CICS READQ TS QUEUE  (WS-TS-QUEUE-NAME)                 00002340
                              SET    (ADDRESS OF WS-LINK-STORAGE)       00002350
                              LENGTH (WS-ZERO-LENGTH)                   00002360
                              ITEM   (WS-TS-ITEM)                       00002370
                              RESP   (WS-CICS-RESP)                     00002380
                              END-EXEC.                                 00002390
                                                                        00002400
           DISPLAY 'WS-CICS-RESP:' WS-CICS-RESP
                                                                        00002410
           IF WS-CICS-RESP = DFHRESP(QIDERR)                            00002420
               DISPLAY 'START NAQ1 QUEUE'
               PERFORM 0150-WRITE-NAQ1-QUEUE                            00002430
               DISPLAY 'END NAQ1 QUEUE'
           ELSE                                                         00002440
              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                     00002450
                  GO TO 9999-CICS-ERROR.                                00002460
                                                                        00002470
           MOVE WS-ZERO-LENGTH TO WS-ZERO-LENGTH.                       00002480
           COMPUTE WS-LINK-LENGTH = (WS-ZERO-LENGTH - +600).            00002490
           MOVE WS-LINK-STORAGE TO WS-TS-QUEUE.                         00002500
           DISPLAY 'WS-LINK-LENGTH:' WS-LINK-LENGTH.

           IF EIBTRNID = 'TU01'                                         00002510
                  GO TO 0115-INITIATE-NA301.                            00002520
                                                                        00002530
      *    PERFORM 0155-CHECK-FOR-EXTERNAL-CUST THRU 0155-EXIT.         00002540
           DISPLAY 'COMM-NEXT-FUNCTION:' COMM-NEXT-FUNCTION.
                                                                        00002550
       0030-PROCESS-MENU-SCREEN.                                        00002560
                                                                        00002570
           DISPLAY 'EIBAID:' EIBAID
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00002580
               DISPLAY '3'
               GO TO 0100-SEND-NA300M2-MENU                             00002590
           ELSE                                                         00002600
              IF COMM-NEXT-FUNCTION = '00'                              00002610
                 DISPLAY '4'
                  GO TO 0050-RETURN-FROM-ROUTER                         00002620
              ELSE                                                      00002630
                 IF EIBAID = DFHENTER                                   00002640
                     DISPLAY 'inside enter'
                     GO TO 0040-EDIT-COMMAND-LINE                       00002650
                 ELSE                                                   00002660
                    IF EIBAID = DFHCLEAR                                00002670
                        GO TO 0140-PROCESS-SCREEN-CLEAR                 00002680
      *             ELSE                                                00002690
      *                IF EIBAID = DFHPF18                              00002700
MANJU *                    GO TO 0120-PROCESS-TUTORIAL-REQUEST          00002710
      *---------------------------------------------------------------* 00002720
      *                ELSE                                           * 00002730
      *                   IF COMM-NEXT-FUNCTION = 'TU'                * 00002740
      *                       GO TO 0130-RETURN-FROM-TUTORIAL         * 00002750
      *---------------------------------------------------------------* 00002760
      *                   ELSE                                          00002770
      *                      IF EIBAID = DFHPA1 AND                     00002780
      *                         COMM-EXTERNAL-CUST NOT = 'Y'            00002790
      *                          GO TO 0170-RETURN-TO-MT-BILLION        00002800
                             ELSE                                       00002810
                                IF EIBAID = DFHPF2                      00002820
                                    GO TO 0060-EXECUTE-STACK-FUNCTION   00002830
                                ELSE                                    00002840
                                   IF EIBAID = DFHPF3                   00002850
                                       GO TO 0070-RETURN-TO-BROWSE      00002860
                                   ELSE                                 00002870
                                     IF EIBAID = DFHPF8 OR DFHPF7       00002880
                                         GO TO 0100-SEND-NA300M2-MENU   00002890
                                     ELSE                               00002900
                                     DISPLAY 'WRONG'
                                            GO TO 0160-WRONG-KEY-HIT.   00002910
                                                                        00002920
       0040-EDIT-COMMAND-LINE.                                          00002930
           MOVE ALL LOW-VALUES TO NA300M2I.                             00002940
           EXEC CICS RECEIVE MAP  ('NA300M2')                           00002950
                             RESP (WS-CICS-RESP)                        00002960
                             END-EXEC.                                  00002970

           DISPLAY '0040 WS-CICS-RESP:' WS-CICS-RESP.
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00002980
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00002990
                   GO TO 9999-CICS-ERROR.                               00003000
           PERFORM 0080-UPDATE-QUEUE-AREA.                              00003010
                                                                        00003020
      *---------------------------------------------------------------* 00003030
      * IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE = 'Y' THEN SOMETHING* 00003040
      * ON THE COMMAND LINE CHANGED AND THE ROUTER SHOULD BE STARTED. * 00003050
      *---------------------------------------------------------------* 00003060
                                                                        00003070
                                                                        00003080
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED  = 'Y'     00003090
               GO TO 0110-INITIATE-ROUTER.                              00003100
                                                                        00003110
           IF COMM-NEXT-FUNCTION = '01'                                 00003170
               GO TO 0100-SEND-NA300M2-MENU.                            00003180
      *---------------------------------------------------------------* 00003120
      * IF COMM-NEXT-FUNCTION = '01' THEN THE CUSTOMER IS RE-ENTERING * 00003130
      * THE PROGRAM WITHOUT CHANGING THE COMMAND LINE.                * 00003140
      *---------------------------------------------------------------* 00003150
                                                                        00003160
                                                                        00003190
       0050-RETURN-FROM-ROUTER.                                         00003200
      *---------------------------------------------------------------* 00003210
      * IF COMM-NEXT-FUNCTION = '00' SEND OUT THE SYSTEM SELECTION    * 00003220
      * MAP                                                           * 00003230
      *---------------------------------------------------------------* 00003240
                                                                        00003250
           GO TO 0100-SEND-NA300M2-MENU.                                00003260
                                                                        00003270
       0060-EXECUTE-STACK-FUNCTION.                                     00003280
                                                                        00003290
      *---------------------------------------------------------------* 00003300
      * THIS FUNCTION TELLS THE ROUTER THAT THE CUSTOMER WISHES TO    * 00003310
      * EXECUTE A SERIES OF COMMAND LINE CHANGES THAT ARE STORED      * 00003320
      * IN THE STACK COMMAND DATASET (THIS IS A FUTURE FUNCTION)      * 00003330
      *---------------------------------------------------------------* 00003340
                                                                        00003350
           MOVE 'GO'   TO COMM-SYSTEM-CODE.                             00003360
           MOVE SPACES TO COMM-ACTION-CODE.                             00003370
           MOVE 'Y'    TO COMM-CMDLINE-CHANGED.                         00003380
           GO TO 0110-INITIATE-ROUTER.                                  00003390
                                                                        00003400
       0070-RETURN-TO-BROWSE.                                           00003410
                                                                        00003420
           MOVE SPACES   TO COMM-SYSTEM-CODE.                           00003430
           MOVE SPACES   TO COMM-ACTION-CODE.                           00003440
           MOVE SPACES   TO COMM-CUSTOMER-INFO.                         00003450
                                                                        00003470
           MOVE 'NAM1'            TO COMM-PREVIOUS-TRANID.              00004780
           MOVE '00'              TO COMM-NEXT-FUNCTION.                00004790
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT               00004800
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.              00004810
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00004820
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00004830
                                                                        00004840
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00004850
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00004860
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME.
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00004880
                               FROM   (WS-TS-QUEUE)                     00004890
                               LENGTH (WS-ZERO-LENGTH)                  00004900
                               ITEM   (WS-TS-ITEM)                      00004910
                               RESP   (WS-CICS-RESP)                    00004920
                               REWRITE                                  00004930
                               MAIN                                     00004940
                               END-EXEC.                                00004950
                                                                        00004960
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00004970
                GO TO 9999-CICS-ERROR.                                  00004980
                                                                        00004990
           EXEC CICS START TRANSID('NAM0')                              00005070
                           TERMID (EIBTRMID)                            00005080
                           RESP   (WS-CICS-RESP)                        00005090
                           END-EXEC                                     00005100
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00005120
                GO TO 9999-CICS-ERROR.                                  00005130
                                                                        00005140
           EXEC CICS RETURN                                             00005150
                     END-EXEC.                                          00005160
                                                                        00005170
                                                                        00005180
       0080-UPDATE-QUEUE-AREA.                                          00003480
                                                                        00003490
           DISPLAY '0080-UPDATE-QUEUE-AREA'
           IF MAP-SYSTEM-CD2F = HEX80                                   00003500
               MOVE SPACES TO COMM-SYSTEM-CODE                          00003510
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED                      00003520
           ELSE                                                         00003530
               IF MAP-SYSTEM-CD2L > ZERO                                00003540
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED           00003550
                  MOVE MAP-SYSTEM-CD2I TO COMM-SYSTEM-CODE.             00003560
                                                                        00003570
           IF MAP-ACTION-CD2F = HEX80                                   00003580
               MOVE SPACES TO COMM-ACTION-CODE                          00003590
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED                      00003600
           ELSE                                                         00003610
               IF MAP-ACTION-CD2L > ZERO                                00003620
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED           00003630
                  MOVE MAP-ACTION-CD2I TO COMM-ACTION-CODE.             00003640
                                                                        00003650
           IF MAP-CUST-INFO2F = HEX80                                   00003660
               MOVE SPACES TO COMM-CUSTOMER-INFO                        00003670
               MOVE 'Y'            TO COMM-KEY-CHANGED                  00003680
           ELSE                                                         00003690
               IF MAP-CUST-INFO2L > ZERO                                00003700
                  MOVE 'Y'            TO COMM-KEY-CHANGED               00003710
                  MOVE MAP-CUST-INFO2I TO COMM-CUSTOMER-INFO.           00003720
                                                                        00003730
                                                                        00003740
       0100-SEND-NA300M2-MENU.                                          00003750
                                                                        00003760
           DISPLAY '0100-SEND-NA300M2-MENU'.
           PERFORM 0190-CHECK-ERROR-MESSAGES                            00003770
               VARYING ACTION-SUB FROM 1 BY 1                           00003780
                   UNTIL ACTION-SUB > 4.                                00003790
                                                                        00003800
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00003810
               MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                    00003820
                                                                        00003830
                                                                        00003840
           MOVE -1      TO MAP-ACTION-CD2L.                             00003850
           MOVE '01'    TO COMM-NEXT-FUNCTION.                          00003860
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00003870
               MOVE ALL '_' TO MAP-SYSTEM-CD2O                          00003880
               MOVE SPACES  TO COMM-SYSTEM-CODE                         00003890
           ELSE                                                         00003900
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CD2O.               00003910
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00003920
               MOVE ALL '_' TO MAP-ACTION-CD2O                          00003930
               MOVE SPACES  TO COMM-ACTION-CODE                         00003940
           ELSE                                                         00003950
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CD2O.               00003960
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00003970
               MOVE ALL '_' TO MAP-CUST-INFO2O                          00003980
               MOVE SPACES  TO COMM-CUSTOMER-INFO                       00003990
           ELSE                                                         00004000
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFO2O                00004010
MANJU         MOVE COMM-CUSTOMER-INFO(1:8) TO COMM-IDNTITY.             00004020
                                                                        00004030
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00004040
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00004050
                                                                        00004060
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00004070
                               FROM   (WS-TS-QUEUE)                     00004080
                               LENGTH (WS-ZERO-LENGTH)                  00004090
                               ITEM   (WS-TS-ITEM)                      00004100
                               RESP   (WS-CICS-RESP)                    00004110
                               REWRITE                                  00004120
                               MAIN                                     00004130
                               END-EXEC.                                00004140

           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME.
           DISPLAY 'WS-CICS-RESP:' WS-CICS-RESP.
                                                                        00004150
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00004160
                GO TO 9999-CICS-ERROR.                                  00004170
                                                                        00004180
           DISPLAY 'COMM-SYSTEM-CODE:' COMM-SYSTEM-CODE.
           MOVE SPACES           TO WS-SYSTEM-CODE WS-ACTION-CODE.      00004190
           MOVE COMM-SYSTEM-CODE TO WS-SYSTEM-CODE-2BYTES.              00004200
           MOVE WS-SYSTEM-CODE   TO WT-C8991-SYSTEM-CD.                 00004210
           MOVE WS-ACTION-CODE   TO WT-C8991-ACTION-CD.                 00004220
                                                                        00004230
           MOVE WT-CNTL8991          TO TURBO-CNTL-AREA.                00004240
MANJU *    PERFORM 10000-CALL-GSF.                                      00004250
           MOVE TURBO-CNTL-AREA      TO WT-CNTL8991.                    00004260
           MOVE 1                    TO ACTION-SUB                      00004270
                                                                        00004280
      *** FOR PF8 ONLY THE SECOND PAGE WILL BE FORMATTED SO             00004290
      *** JUST SET A 'BYPASS' SWITCH FOR FIRST PAGE OF MENU             00004300
           IF EIBAID = DFHPF8                                           00004310
                  MOVE 'BYPASS' TO MAP-DESCO (1)                        00004320
           END-IF.                                                      00004330
                                                                        00004340
           PERFORM 0220-RETRIEVE-ACTION-CODES THRU 0220-EXIT.           00004350
      *       UNTIL COMM-SYSTEM-CODE NOT = WT-C8991-SYSTEM-CD OR        00004360
      *           ACTION-SUB > 30.                                      00004370
                                                                        00004380
      *** FOR PF8 ALWAYS RETRIEVE AND FORMAT THE SECOND PAGE OF THE MENU00004390
           IF EIBAID = DFHPF8                                           00004400
              MOVE 1 TO ACTION-SUB                                      00004410
              MOVE '      ' TO MAP-DESCO (1)                            00004420
              PERFORM 0220-RETRIEVE-ACTION-CODES THRU 0220-EXIT         00004430
                  UNTIL COMM-SYSTEM-CODE NOT = WT-C8991-SYSTEM-CD OR    00004440
                    ACTION-SUB > 30                                     00004450
                  MOVE 'NA301'           TO WS-MESSAGE-NUMBER           00004460
                  PERFORM 0180-BUMP-ERROR-MESSAGES                      00004470
                  MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(1)              00004480
                  MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(2)              00004490
                  MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(3)              00004500
                  MOVE WSQ-MSG-ID(4)     TO COMM-MSG-ID(4)              00004510
           ELSE                                                         00004520
      *** DISPLAY MSG TO'PRESS PF8 FOR MORE MENU OPTIONS'IF PAGE IS FULL00004530
               IF ACTION-SUB > 30                                       00004540
                  MOVE 'NA300'           TO WS-MESSAGE-NUMBER           00004550
                  PERFORM 0180-BUMP-ERROR-MESSAGES                      00004560
                  MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(1)              00004570
                  MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(2)              00004580
                  MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(3)              00004590
                  MOVE WSQ-MSG-ID(4)     TO COMM-MSG-ID(4)              00004600
               END-IF                                                   00004610
           END-IF.                                                      00004620
                                                                        00004630
           EXEC CICS SEND MAP    ('NA300M2')                            00004640
                          CURSOR                                        00004650
                          ERASE                                         00004660
                          RESP (WS-CICS-RESP)                           00004670
                          END-EXEC.                                     00004680
                                                                        00004690
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00004700
                GO TO 9999-CICS-ERROR.                                  00004710
                                                                        00004720
           EXEC CICS RETURN TRANSID ('NAM1')                            00004730
                            END-EXEC.                                   00004740
                                                                        00004750
       0110-INITIATE-ROUTER.                                            00004760
                                                                        00004770
           MOVE 'NAM1'            TO COMM-PREVIOUS-TRANID.              00004780
           MOVE '00'              TO COMM-NEXT-FUNCTION.                00004790
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT               00004800
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.              00004810
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00004820
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00004830
                                                                        00004840
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00004850
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00004860
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME.
           IF COMM-CUSTOMER-INFO NOT = SPACES OR LOW-VALUES             00003970
MANJU      MOVE COMM-CUSTOMER-INFO(1:8) TO COMM-IDNTITY                 00004020
           END-IF.
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00004880
                               FROM   (WS-TS-QUEUE)                     00004890
                               LENGTH (WS-ZERO-LENGTH)                  00004900
                               ITEM   (WS-TS-ITEM)                      00004910
                               RESP   (WS-CICS-RESP)                    00004920
                               REWRITE                                  00004930
                               MAIN                                     00004940
                               END-EXEC.                                00004950
                                                                        00004960
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00004970
                GO TO 9999-CICS-ERROR.                                  00004980
                                                                        00004990
      *    EXEC CICS SEND MAP ('SYSBUSY')                               00005000
      *                   RESP (WS-CICS-RESP)                           00005010
      *                   END-EXEC.                                     00005020
                                                                        00005030
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00005040
      *         GO TO 9999-CICS-ERROR.                                  00005050
            DISPLAY 'RESP BEFORE ' WS-CICS-RESP
            DISPLAY 'COMM-ACTION-CODE' COMM-ACTION-CODE
           IF COMM-ACTION-CODE(1:1) = 'A'                               00005060
           EXEC CICS START TRANSID('NAA1')                              00005070
                           TERMID (EIBTRMID)                            00005080
                           RESP   (WS-CICS-RESP)                        00005090
                           END-EXEC                                     00005100
           ELSE IF COMM-ACTION-CODE(1:1) = 'U'
           EXEC CICS START TRANSID('NAU1')                              00005070
                           TERMID (EIBTRMID)                            00005080
                           RESP   (WS-CICS-RESP)                        00005090
                           END-EXEC                                     00005100
           ELSE IF COMM-ACTION-CODE(1:1) = 'I'
           EXEC CICS START TRANSID('NAI1')                              00005070
                           TERMID (EIBTRMID)                            00005080
                           RESP   (WS-CICS-RESP)                        00005090
                           END-EXEC                                     00005100
           END-IF.
                                                                        00005110
            DISPLAY 'RESP AFTER  ' WS-CICS-RESP
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00005120
                GO TO 9999-CICS-ERROR.                                  00005130
                                                                        00005140
           EXEC CICS RETURN                                             00005150
                     END-EXEC.                                          00005160
                                                                        00005170
                                                                        00005180
       0115-INITIATE-NA301.                                             00005190
                                                                        00005250
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00005260
                GO TO 9999-CICS-ERROR.                                  00005270
                                                                        00005280
           EXEC CICS START TRANSID('NAM1')                              00005290
                           TERMID (EIBTRMID)                            00005300
                           RESP   (WS-CICS-RESP)                        00005310
                           END-EXEC.                                    00005320
                                                                        00005330
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00005340
                GO TO 9999-CICS-ERROR.                                  00005350
                                                                        00005360
           EXEC CICS RETURN                                             00005370
                     END-EXEC.                                          00005380
                                                                        00005390
      *---------------------------------------------------------------* 00005400
      *   THE FOLLOWING CODE EXECUTES THE PSI TUTORIAL OR ONLINE HELP * 00005410
      *   FUNCTION  IT STORES ANY CHANGES MADE ON THE MAP IN A        * 00005420
      *   TEMPORARY STORAGE QUEUE, THEN IT INITIATES THE TUTORIAL     * 00005430
      *   COMMAREA AND XCNTL'S TO THE TUTORIAL PROGRAM                * 00005440
      *---------------------------------------------------------------* 00005450
                                                                        00005460
                                                                        00005470
       0120-PROCESS-TUTORIAL-REQUEST.                                   00005480
           MOVE ALL LOW-VALUES TO NA300M2I.                             00005490
           EXEC CICS RECEIVE MAP    ('NA300M2')                         00005500
                             RESP   (WS-CICS-RESP)                      00005510
                             END-EXEC.                                  00005520
                                                                        00005530
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00005540
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00005550
                    GO TO 9999-CICS-ERROR.                              00005560
                                                                        00005570
           MOVE 'NA300M2' TO T3-SCREEN-NAME                             00005580
           MOVE 'Y'       TO T3-COMMAND-LINE                            00005590
           MOVE SPACES    TO T3-CARRIER                                 00005600
           EXEC CICS LINK PROGRAM  ('TU003')                            00005610
                          COMMAREA (T3-COMMAREA)                        00005620
                          LENGTH   (LENGTH OF T3-COMMAREA)              00005630
                          END-EXEC.                                     00005640
                                                                        00005650
           EXEC CICS RETURN TRANSID ('NAM1') END-EXEC.                  00005660
                                                                        00005670
       0140-PROCESS-SCREEN-CLEAR.                                       00006850
                                                                        00006860
           MOVE 'NAM1'            TO COMM-PREVIOUS-TRANID.              00006870
           MOVE '00'              TO COMM-NEXT-FUNCTION.                00006880
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT               00006890
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.              00006900
           MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                        00006910
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00006920
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00006930
                                                                        00006940
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00006950
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00006960
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME
                                                                        00006970
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00006980
                               FROM   (WS-TS-QUEUE)                     00006990
                               LENGTH (WS-ZERO-LENGTH)                  00007000
                               ITEM   (WS-TS-ITEM)                      00007010
                               RESP   (WS-CICS-RESP)                    00007020
                               REWRITE                                  00007030
                               MAIN                                     00007040
                               END-EXEC.                                00007050
                                                                        00007060
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00007070
                GO TO 9999-CICS-ERROR.                                  00007080
                                                                        00007090
           EXEC CICS ASSIGN TCTUALENG(TCTUAL) END-EXEC.                 00007100
           EXEC CICS ADDRESS TCTUA(ADDRESS OF TCTUAR) END-EXEC.         00007110
           MOVE TCTUA-DEMO-DATA TO WS-DEMOGRAPHIC-COMM-AREA.            00007120
                                                                        00007130
           IF COMM-EXTERNAL-CUST = 'Y'                                  00007140
               IF WS-DEMO-LID-1-4 = 'DPAD'                              00007150
                   EXEC CICS SEND FROM(DFHNULL) LENGTH(1) END-EXEC      00007160
                   EXEC CICS RETURN END-EXEC                            00007170
               ELSE                                                     00007180
                   PERFORM 8999-DISCONNECT-TERMINAL THRU 8999-EXIT      00007190
           ELSE                                                         00007200
               EXEC CICS SEND FROM(DFHNULL) LENGTH(1) END-EXEC          00007210
               EXEC CICS RETURN END-EXEC.                               00007220
                                                                        00007230
                                                                        00007240
                                                                        00007250
       0150-WRITE-NAQ1-QUEUE.                                           00007260
                                                                        00007270
           MOVE SPACES TO WSQ-COMMAREA.                                 00007280
           MOVE ZEROES TO WSQ-CURSOR-POSN WSQ-MSG-COUNT                 00007290
               WSQ-MAIL-LINE-COUNT WSQ-CUST-TOKEN-COUNT.                00007300
           MOVE +600 TO WSQ-CICS-COMMAREA-LENGTH.                       00007310
           MOVE '00' TO WSQ-NEXT-FUNCTION.                              00007320
      *    PERFORM 0155-CHECK-FOR-EXTERNAL-CUST THRU 0155-EXIT.         00007330
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME
                                                                        00007340
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00007350
                               FROM   (WSQ-COMMAREA)                    00007360
                               LENGTH (WS-COMM-LENGTH)                  00007370
                               ITEM   (WS-TS-ITEM)                      00007380
                               RESP   (WS-CICS-RESP)                    00007390
                               MAIN                                     00007400
                               END-EXEC.                                00007410
                                                                        00007420
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00007430
                GO TO 9999-CICS-ERROR.                                  00007440
                                                                        00007450
           MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.           00007460
           MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.           00007470
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME
           EXEC CICS READQ TS QUEUE  (WS-TS-QUEUE-NAME)                 00007480
                              SET    (ADDRESS OF WS-LINK-STORAGE)       00007490
                              LENGTH (WS-ZERO-LENGTH)                   00007500
                              ITEM   (WS-TS-ITEM)                       00007510
                              RESP   (WS-CICS-RESP)                     00007520
                              END-EXEC.                                 00007530
                                                                        00007540
                                                                        00007550
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00007560
                GO TO 9999-CICS-ERROR.                                  00007570
                                                                        00007580
           MOVE WS-ZERO-LENGTH TO WS-ZERO-LENGTH.                       00007590
           COMPUTE WS-LINK-LENGTH = (WS-ZERO-LENGTH - +600).            00007600
           MOVE WS-LINK-STORAGE TO WS-TS-QUEUE.                         00007610
                                                                        00007620
                                                                        00007630
                                                                        00007640
      *0155-CHECK-FOR-EXTERNAL-CUST.                                    00007650
                                                                        00007660
      *    DISPLAY '1'.
      *    EXEC CICS ASSIGN TCTUALENG(TCTUAL) END-EXEC.                 00007670
      *    DISPLAY 'TCTUAL:' TCTUAL
      *    DISPLAY '2'.
      *    EXEC CICS ADDRESS TCTUA(ADDRESS OF TCTUAR) END-EXEC.         00007680
      *    DISPLAY '3'.
ARUN  *    MOVE TCTUA-DEMO-DATA TO WS-DEMOGRAPHIC-COMM-AREA.            00007690
      *    DISPLAY '4'.
      *                                                                 00007700
      *    MOVE WS-DEMO-LOGONID TO WS-LOGON-HOLD.                       00007710
      *    DISPLAY 'WS-LOGON-HOLD:' WS-LOGON-HOLD
                                                                        00007720
ARUN  *    PERFORM 0157-CHECK-ATTACH THRU 0157-EXIT.                    00007730
      *                                                                 00007740
      *    EXEC SQL                                                     00007750
      *         SELECT AGNT#IDNTITY                                     00007760
      *         INTO :LID-AGNT-IDNTITY                                  00007770
      *         FROM ECLIDV01                                           00007780
      *         WHERE LOGONID = :WS-LOGON-HOLD                          00007790
      *           AND STATUS = :WS-A                                    00007800
      *    END-EXEC.                                                    00007810
MANJU *    MOVE 0 TO SQLCODE                                            00007820
      *    DISPLAY 'SQLCODE:' SQLCODE.
      *    IF SQLCODE = 0                                               00007830
      *        MOVE 'Y' TO COMM-EXTERNAL-CUST                           00007840
      *      ELSE                                                       00007850
      *        IF SQLCODE = +100                                        00007860
      *            MOVE ' ' TO COMM-EXTERNAL-CUST                       00007870
      *          ELSE                                                   00007880
      *            MOVE '0155-CHECK-FOR-EXTERNAL' TO WS-SQL-PARA        00007890
      *            PERFORM 9999-DB2-ERRORS.                             00007900
      *                                                                 00007910
      *                                                                 00007920
      *                                                                 00007930
      *0155-EXIT.                                                       00007940
      *    EXIT.                                                        00007950
                                                                        00008270
       0160-WRONG-KEY-HIT.                                              00008280
           MOVE ALL LOW-VALUES TO NA300M2I.                             00008290
           EXEC CICS RECEIVE MAP    ('NA300M2')                         00008300
                             RESP (WS-CICS-RESP)                        00008310
                             END-EXEC.                                  00008320
                                                                        00008330
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00008340
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00008350
                GO TO 9999-CICS-ERROR.                                  00008360
                                                                        00008370
           PERFORM 0080-UPDATE-QUEUE-AREA.                              00008380
                                                                        00008390
           MOVE 'NA030'           TO WS-MESSAGE-NUMBER.                 00008400
           PERFORM 0180-BUMP-ERROR-MESSAGES.                            00008410
           MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(1).                    00008420
           MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(2).                    00008430
           MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(3).                    00008440
           MOVE WSQ-MSG-ID(4)     TO COMM-MSG-ID(4).                    00008450
                                                                        00008460
           MOVE EIBCPOSN           TO COMM-CURSOR-POSN.                 00008470
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00008480
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00008490
                                                                        00008500
           DISPLAY 'COMM-KEY-CHANGED:' COMM-KEY-CHANGED
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED = 'Y'      00008510
               MOVE 'Y' TO COMM-KEY-CHANGED                             00008520
               MOVE 'Y' TO COMM-CMDLINE-CHANGED.                        00008530
            DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00008540
                               FROM   (WS-TS-QUEUE)                     00008550
                               LENGTH (WS-COMM-LENGTH)                  00008560
                               ITEM   (WS-TS-ITEM)                      00008570
                               RESP   (WS-CICS-RESP)                    00008580
                               REWRITE                                  00008590
                               MAIN                                     00008600
                               END-EXEC.                                00008610
                                                                        00008620
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00008630
                GO TO 9999-CICS-ERROR.                                  00008640
                                                                        00008650
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00008660
               MOVE ALL '_' TO MAP-SYSTEM-CD2O                          00008670
           ELSE                                                         00008680
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CD2O.               00008690
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00008700
               MOVE ALL '_' TO MAP-ACTION-CD2O                          00008710
           ELSE                                                         00008720
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CD2O.               00008730
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00008740
               MOVE ALL '_' TO MAP-CUST-INFO2O                          00008750
           ELSE                                                         00008760
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFO2O.               00008770
                                                                        00008780
           DISPLAY 'COMM-SYSTEM-CODE:' COMM-SYSTEM-CODE
           MOVE SPACES           TO WS-SYSTEM-CODE WS-ACTION-CODE.      00008790
           MOVE COMM-SYSTEM-CODE TO WS-SYSTEM-CODE-2BYTES.              00008800
           MOVE WS-SYSTEM-CODE   TO WT-C8991-SYSTEM-CD.                 00008810
           MOVE WS-ACTION-CODE   TO WT-C8991-ACTION-CD.                 00008820
                                                                        00008830
           MOVE WT-CNTL8991          TO TURBO-CNTL-AREA.                00008840
      *    PERFORM 10000-CALL-GSF.                                      00008850
           MOVE TURBO-CNTL-AREA      TO WT-CNTL8991.                    00008860
           MOVE 1                    TO ACTION-SUB                      00008870
           PERFORM 0220-RETRIEVE-ACTION-CODES THRU 0220-EXIT.           00008880
      *       UNTIL COMM-SYSTEM-CODE NOT = WT-C8991-SYSTEM-CD OR        00008890
      *           ACTION-SUB > 30.                                      00008900
                                                                        00008910
           EXEC CICS SEND MAP    ('NA300M2')                            00008920
                          CURSOR (COMM-CURSOR-POSN)                     00008930
                          RESP   (WS-CICS-RESP)                         00008940
                          ERASE                                         00008950
                          END-EXEC.                                     00008960
                                                                        00008970
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00008980
                GO TO 9999-CICS-ERROR.                                  00008990
                                                                        00009000
           EXEC CICS RETURN TRANSID ('NAM1')                            00009010
                            END-EXEC.                                   00009020
                                                                        00009030
       0170-RETURN-TO-MT-BILLION.                                       00009040
                                                                        00009050
           MOVE 'NAM1'            TO COMM-PREVIOUS-TRANID.              00009060
           MOVE '00'              TO COMM-NEXT-FUNCTION.                00009070
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT               00009080
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.              00009090
           MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                        00009100
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00009110
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00009120
                                                                        00009130
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00009140
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00009150
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME
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
               IF WT-C9999-RETURN = ZERO                                00009700
                   MOVE WT-C9999-ERROR-CODE TO WS-C9999-ERROR-CODE      00009710
                   MOVE WT-C9999-SEVERITY-CODE TO WS-C9999-SEVERITY-CODE00009720
                   MOVE WT-C9999-ERROR-MESSAGE TO WS-C9999-ERROR-MESSAGE00009730
                   MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (ACTION-SUB)  00009740
               ELSE                                                     00009750
                  IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND          00009760
                      COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES         00009770
                      MOVE '** INVALID ERROR MESSAGE ** ' TO            00009780
                           MAP-ERROR-MSGO (ACTION-SUB).                 00009790
                                                                        00009800
       0210-GET-ERROR-MESSAGE.                                          00009810
                                                                        00009820
           MOVE WT-CNTL9999 TO TURBO-CNTL-AREA.                         00009830
      *    PERFORM 10000-CALL-GSF.                                      00009840
           MOVE TURBO-CNTL-AREA TO WT-CNTL9999.                         00009850
                                                                        00009860
       0220-RETRIEVE-ACTION-CODES.                                      00009870
                                                                        00009880
      *    MOVE 'D'                  TO WT-C8991-REQUEST.               00009890
      *    MOVE WT-CNTL8991          TO TURBO-CNTL-AREA.                00009900
      *    PERFORM 10000-CALL-GSF.                                      00009910
      *    MOVE TURBO-CNTL-AREA      TO WT-CNTL8991.                    00009920
                                                                        00009930
      *    IF WT-C8991-RETURN > 0                                       00009940
      *       MOVE 31 TO ACTION-SUB                                     00009950
      *           GO TO 0220-EXIT.                                      00009960
                                                                        00009970
      *    IF COMM-SYSTEM-CODE NOT = WT-C8991-SYSTEM-CD OR              00009980
      *       WT-C8991-DISP-ON-SUBS NOT = 'Y'                           00009990
      *           GO TO 0220-EXIT.                                      00010000
                                                                        00010010
      *    IF COMM-EXTERNAL-CUST = 'Y'                                  00010020
      *        PERFORM 0300-VERIFY-ECI-ACTION-CODE THRU 0300-EXIT       00010030
      *        IF NOT VALID-ECI-CODE                                    00010040
      *           GO TO 0220-EXIT.                                      00010050
                                                                        00010060
                                                                        00010070
      **** WHEN 'BYPASS' SWITCH IS SET, DON'T DISPLAY FIRST PAGE OF MAP 00010080
           IF EIBAID = DFHPF8                                           00010090
              AND MAP-DESCO (1) = 'BYPASS'                              00010100
                ADD 1 TO ACTION-SUB                                     00010110
                GO TO 0220-EXIT.                                        00010120
                                                                        00010130
           MOVE 'A'                  TO MAP-CODEO (ACTION-SUB).         00010140
           MOVE 'Name/Address Addition' TO MAP-DESCO (ACTION-SUB).      00010150
                                                                        00010160
           ADD 1 TO ACTION-SUB.                                         00010170
                                                                        00010180
           MOVE 'U'                  TO MAP-CODEO (ACTION-SUB).         00010140
           MOVE 'Name/Address Update' TO MAP-DESCO (ACTION-SUB).        00010150

           ADD 1 TO ACTION-SUB.                                         00010170
                                                                        00010180
           MOVE 'I'                  TO MAP-CODEO (ACTION-SUB).         00010140
           MOVE 'Name/Address Inquire' TO MAP-DESCO (ACTION-SUB).       00010150
                                                                        00010190
       0220-EXIT.                                                       00010200
           EXIT.                                                        00010210
                                                                        00010220
      *0300-VERIFY-ECI-ACTION-CODE.                                     00010230
      *                                                                 00010240
      *    MOVE SPACE TO WS-VALID-ECI-CODE-SW.                          00010250
      *                                                                 00010260
MANJU *    EXEC CICS ASSIGN TCTUALENG(TCTUAL) END-EXEC.                 00010270
      *    EXEC CICS ADDRESS TCTUA(ADDRESS OF TCTUAR) END-EXEC.         00010280
      *    MOVE TCTUA-DEMO-DATA TO WS-DEMOGRAPHIC-COMM-AREA.            00010290
      *                                                                 00010300
      *    MOVE WS-DEMO-LOGONID TO WS-LOGON-HOLD.                       00010310
      *                                                                 00010320
      *    EXEC SQL                                                     00010330
      *         SELECT MTRXV01.AGNT#IDNTITY, ACTION_CODE                00010340
      *         INTO :MTX-AGNT-IDNTITY, :MTX-ACTION-CODE                00010350
      *         FROM ECLIDV01, MTRXV01                                  00010360
      *         WHERE LOGONID = :WS-LOGON-HOLD                          00010370
      *           AND STATUS = :WS-A                                    00010380
      *        AND ECLIDV01.AGNT#IDNTITY = MTRXV01.AGNT#IDNTITY         00010390
      *           AND SYSTEM_ID = :WT-C8991-SYSTEM-CD                   00010400
      *           AND ACTION_CODE = :WT-C8991-ACTION-CD                 00010410
      *           AND (CURRENT DATE BETWEEN EFFECTIVE_DATE              00010420
      *                AND TERMINATION_DATE)                            00010430
      *    END-EXEC.                                                    00010440
MANJU *    MOVE 0 TO SQLCODE                                            00010450
      *    IF SQLCODE = 0                                               00010460
      *        MOVE 'Y' TO WS-VALID-ECI-CODE-SW                         00010470
      *      ELSE                                                       00010480
      *        IF SQLCODE = +100                                        00010490
      *            NEXT SENTENCE                                        00010500
      *          ELSE                                                   00010510
      *            MOVE '0300-VERIFY-ECI-CODE' TO WS-SQL-PARA           00010520
      *            PERFORM 9999-DB2-ERRORS.                             00010530
      *                                                                 00010540
      *                                                                 00010550
      *                                                                 00010560
      *0300-EXIT.                                                       00010570
      *    EXIT.                                                        00010580
                                                                        00010590
                                                                        00010600
       8999-DISCONNECT-TERMINAL.                                        00010610
                                                                        00010620
           EXEC CICS SEND FROM(DFHNULL) LENGTH(1) END-EXEC.             00010630
                                                                        00010640
      *                                                                 00010650
MANJU * SIGN OFF THE TERMINAL IF IT IS ALREADY SIGNED ON                00010660
      *                                                                 00010670
      *    MOVE 'L' TO UC-REQUEST-CODE.                                 00010680
                                                                        00010690
      *    EXEC CICS LINK PROGRAM('PSIACFUC')                           00010700
      *              COMMAREA(PSIACFUC-CONTROL-BLOCK)                   00010710
      ***            LENGTH(486)                                        00010720
      *              LENGTH(LENGTH OF PSIACFUC-CONTROL-BLOCK)           00010730
      *              END-EXEC.                                          00010740
                                                                        00010750
           EXEC CICS RETURN END-EXEC.                                   00010760
                                                                        00010770
       8999-EXIT.  EXIT.                                                00010780
                                                                        00010790
      *---------------------------------------------------------------* 00010800
      * THIS ROUTINE TRANSFERS CONTROL TO AN ONLINE ERROR PROGRAM     * 00010810
      * WHICH DISPLAYS THE ERRORS ON THE SCREEN SUPPLYING INFORMATION * 00010820
      * FOR THE HELP-DESK  TO AID PROGRAMMERS IN DEBUGGING.           * 00010830
      *---------------------------------------------------------------* 00010840
                                                                        00010850
           COPY CICSERR.                                                00010860
                                                                        00010870
                                                                        00010880
                                                                        00010890
       9999-DB2-ERRORS.                                                 00010900
                                                                        00010910
      *    MOVE WS-SQL-ERROR-MSG TO SQL-ERROR-MESSAGE.                  00010920
                                                                        00010930
           EXEC CICS SYNCPOINT                                          00010940
                     ROLLBACK                                           00010950
           END-EXEC.                                                    00010960
                                                                        00010970
      *    EXEC CICS XCTL                                               00010980
MANJU *              PROGRAM  ('SQLERRTN')                              00010990
      *              COMMAREA (SQLCA)                                   00011000
      ***            LENGTH   (214)                                     00011010
      *              LENGTH   (LENGTH OF SQLCA)                         00011020
      *    END-EXEC.                                                    00011030
                                                                        00011040
       9999-EXIT.                                                       00011050
           EXIT.                                                        00011060
                                                                        00011070
      *---------------------------------------------------------------* 00011080
      * THIS ROUTINE TRANSFERS CONTROL TO THE TURBO TABLES FOR EDIT   * 00011090
      * VERIFICATION                                                  * 00011100
      *---------------------------------------------------------------* 00011110
                                                                        00011120
           COPY TURBOCAL.                                               00011130
