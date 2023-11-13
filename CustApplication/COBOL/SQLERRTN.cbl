       IDENTIFICATION DIVISION.                                         00000010
       PROGRAM-ID. SQLERRTN.                                            00000020
       ENVIRONMENT DIVISION.                                            00000810
       CONFIGURATION SECTION.                                           00000820
       INPUT-OUTPUT SECTION.                                            00000830
       FILE-CONTROL.                                                    00000840
       DATA DIVISION.                                                   00000850
       FILE SECTION.                                                    00000860
       WORKING-STORAGE SECTION.                                         00000870
           COPY AUDCICS.                                                00000880
       01  WS-STUFF.                                                    00000890
           05  ERROR-STATUS                PIC S9(8) COMP VALUE +0.     00000900
           05  ERR-LEN                     PIC 9(8) COMP VALUE 78.      00000910
           05  HOLD-ERR-MESS.                                           00000920
               10  ERR-AREA-LEN              PIC 9(4) COMP VALUE 546.   00000930
               10  ERR-MESS-1                PIC X(78) VALUE SPACE.     00000940
               10  ERR-MESS-2                PIC X(78) VALUE SPACE.     00000950
               10  ERR-MESS-3                PIC X(78) VALUE SPACE.     00000960
               10  ERR-MESS-4                PIC X(78) VALUE SPACE.     00000970
               10  ERR-MESS-5                PIC X(78) VALUE SPACE.     00000980
               10  ERR-MESS-6                PIC X(78) VALUE SPACE.     00000990
               10  ERR-MESS-7                PIC X(78) VALUE SPACE.     00001000
                                                                        00001010
       01  WS-CICS-QUERY-FIELDS.                                        00001020
           05  CICS-RESPONSE               PIC S9(9) COMP VALUE +0.     00001030
           05  QF-LEN                      PIC S9(4) COMP VALUE +300.   00001040
           05  QF-MAXLEN                   PIC S9(4) COMP VALUE +800.   00001050
           05  QF-ANSWER                   PIC X(300) VALUE SPACE.      00001060
                                                                        00001070
      *---------------------------------------------------------        00001080
      *    GET SQL COMMUNICATION AREA.                                  00001090
      *---------------------------------------------------------        00001100
           EXEC SQL INCLUDE SQLCA END-EXEC.                             00001110
                                                                        00001120
           05  ERR-MESS-8                    PIC X(78) VALUE SPACE.     00001130
                                                                        00001140
           COPY CAWSINC.                                                00001150
                                                                        00001160
           COPY DEMOCOMM.                                               00001170
                                                                        00001180
           COPY SQLERRM.                                                00001190
                                                                        00001200
R04209     EXEC SQL DECLARE DUMMY CURSOR FOR                            00001210
R04209         SELECT * FROM DUMMY                                      00001220
           END-EXEC.                                                    00001230
                                                                        00001240
       LINKAGE SECTION.                                                 00001250
       01  DFHCOMMAREA.                                                 00001260
           05  COMM-AREA                          PIC X(214).           00001270
                                                                        00001280
R01717** AUTO-ISSUE WITH TRNID H500 HAS A 250 BYTE TWA                  00001290
R01717 01  TWA-WORK-AREA.                                               00001300
R01717     05  TWA-COMM-AREA.                                           00001310
R01717         10  TWA-SQLCA                  PIC X(136).               00001320
R01717         10  TWA-SQL-ERROR-MESSAGE-8    PIC X(078).               00001330
R01717     05  FILLER                         PIC X(036).               00001340
                                                                        00001350
      **********************************************************        00001360
      *                                                                 00001370
      *  PROCEDURE DIVISION                                             00001380
      *                                                                 00001390
      **********************************************************        00001400
       PROCEDURE DIVISION.                                              00001410
                                                                        00001420
                                                                        00001430
                                                                        00001440
       0000-MAINLINE  SECTION.                                          00001450
                                                                        00001460
R01717***** EIBTRMID EX1  IS ASSOCIATED WITH BATCH/CICS INTERFACE       00001470
R01717***** BUT THERE IS NO TERMINAL. HU100500/ISSUE PROGRAMS ARE       00001480
R01717***** RUNNING AS TRNID 'H500' AND INVOKING ALL PROGRAMS FOR       00001490
R01717***** AM VR AND AM ISSUE VIA BATCH/CICS INTERFACE. IF ANY DB2     00001500
R01717***** ERROR OCCURED, SAVE THE SQLCA IN THE TWA SO THAT THE PROGRAM00001510
R01717***** IT RETURNS TO (ISSUE) WILL KNOW THE DETAILS OF WHAT HAPPENED00001520
R01717***** AND PASS IT TO HU100500 WHICH WILL THEN PRINT THE ERROR     00001530
R01717***** ON THE AUTO-ISSUE REPORT (INSTEAD OF A SCREEN).             00001540
R01717     IF (EIBTRNID = 'H500')                                       00001550
R01717        EXEC CICS ADDRESS TWA(ADDRESS OF TWA-WORK-AREA) END-EXEC  00001560
R01717        MOVE COMM-AREA TO TWA-COMM-AREA                           00001570
R01717        EXEC CICS RETURN                                          00001580
R01717        END-EXEC.                                                 00001590
                                                                        00001600
R04209     IF (EIBTRNID = 'CWBA')                                       00001610
R04209        EXEC CICS RETURN                                          00001620
R04209        END-EXEC.                                                 00001630
                                                                        00001640
010124*                                                                 00001650
010124***** TERMINAL OPRI IS ASSOCIATED WITH TALX.  SINCE IT IS NOT     00001660
010124*****     A 'REAL' TERMINAL, WE CANNOT CONTINUE IN THIS PROGRAM   00001670
010124*                                                                 00001680
010124     IF  (EIBTRMID IS EQUAL TO 'OPRI') OR                         00001690
R01924         (EIBTRMID(1:2) IS EQUAL TO 'SM')                         00001700
C28489         MOVE COMM-AREA TO SQLCA                                  00001710
C28489         CALL 'DSNTIAR' USING SQLCA HOLD-ERR-MESS ERR-LEN         00001720
C28489         EXEC CICS WRITEQ TD                                      00001730
C28489              QUEUE('CSML')                                       00001740
C28489              FROM(ERR-MESS-1)                                    00001750
C28489              LENGTH(LENGTH OF ERR-MESS-1)                        00001760
C28489         END-EXEC                                                 00001770
C28489         EXEC CICS WRITEQ TD                                      00001780
C28489              QUEUE('CSML')                                       00001790
C28489              FROM(ERR-MESS-2)                                    00001800
C28489              LENGTH(LENGTH OF ERR-MESS-2)                        00001810
C28489         END-EXEC                                                 00001820
C28489         EXEC CICS WRITEQ TD                                      00001830
C28489              QUEUE('CSML')                                       00001840
C28489              FROM(ERR-MESS-3)                                    00001850
C28489              LENGTH(LENGTH OF ERR-MESS-3)                        00001860
C28489         END-EXEC                                                 00001870
C28489         EXEC CICS WRITEQ TD                                      00001880
C28489              QUEUE('CSML')                                       00001890
C28489              FROM(ERR-MESS-4)                                    00001900
C28489              LENGTH(LENGTH OF ERR-MESS-4)                        00001910
C28489         END-EXEC                                                 00001920
C28489         EXEC CICS WRITEQ TD                                      00001930
C28489              QUEUE('CSML')                                       00001940
C28489              FROM(ERR-MESS-5)                                    00001950
C28489              LENGTH(LENGTH OF ERR-MESS-5)                        00001960
C28489         END-EXEC                                                 00001970
C28489         EXEC CICS WRITEQ TD                                      00001980
C28489              QUEUE('CSML')                                       00001990
C28489              FROM(ERR-MESS-6)                                    00002000
C28489              LENGTH(LENGTH OF ERR-MESS-6)                        00002010
C28489         END-EXEC                                                 00002020
C28489         EXEC CICS WRITEQ TD                                      00002030
C28489              QUEUE('CSML')                                       00002040
C28489              FROM(ERR-MESS-7)                                    00002050
C28489              LENGTH(LENGTH OF ERR-MESS-7)                        00002060
C28489         END-EXEC                                                 00002070
C28489         EXEC CICS WRITEQ TD                                      00002080
C28489              QUEUE('CSML')                                       00002090
C28489              FROM(ERR-MESS-8)                                    00002100
C28489              LENGTH(LENGTH OF ERR-MESS-8)                        00002110
C28489         END-EXEC                                                 00002120
010124         GO TO 1001-RETURN                                        00002130
010124     END-IF.                                                      00002140
                                                                        00002150
      **********************************************************        00002160
      *  HANDLE ALL ERROR CONDITIONS                                    00002170
      **********************************************************        00002180
                                                                        00002190
           EXEC CICS HANDLE CONDITION                                   00002200
                            ERROR (9999-ERROR)                          00002210
                            NOTOPEN (9999-NOTOPEN)                      00002220
                            DSIDERR (9999-DSIDERR)                      00002230
                            ILLOGIC (9999-ILLOGIC)                      00002240
                            INVREQ  (9999-INVREQ)                       00002250
                            LENGERR (9999-LENGERR)                      00002260
                            IOERR   (9999-IOERR)                        00002270
                            DUPKEY  (9999-DUPKEY)                       00002280
                            END-EXEC.                                   00002290
                                                                        00002300
                                                                        00002310
                                                                        00002320
                                                                        00002330
           MOVE COMM-AREA TO SQLCA.                                     00002340
                                                                        00002350
           CALL 'DSNTIAR' USING SQLCA HOLD-ERR-MESS ERR-LEN.            00002360
                                                                        00002370
970917     MOVE ALL LOW-VALUES TO SQLERRMO.                             00002380
           MOVE ERR-MESS-1 TO MAP-MESS1O.                               00002390
           MOVE ERR-MESS-2 TO MAP-MESS2O.                               00002400
           MOVE ERR-MESS-3 TO MAP-MESS3O.                               00002410
           MOVE ERR-MESS-4 TO MAP-MESS4O.                               00002420
           MOVE ERR-MESS-5 TO MAP-MESS5O.                               00002430
           MOVE ERR-MESS-6 TO MAP-MESS6O.                               00002440
           MOVE ERR-MESS-7 TO MAP-MESS7O.                               00002450
                                                                        00002460
           IF EIBCALEN > 136                                            00002470
               MOVE ERR-MESS-8 TO MAP-MESS8O.                           00002480
                                                                        00002490
                                                                        00002500
                                                                        00002510
           PERFORM 1000-SEND-MAP.                                       00002520
                                                                        00002530
           PERFORM 1001-RETURN.                                         00002540
                                                                        00002550
       0000-EXIT.                                                       00002560
           EXIT.                                                        00002570
                                                                        00002580
                                                                        00002590
       1000-SEND-MAP SECTION.                                           00002600
                                                                        00002610
               EXEC CICS SEND MAP('SQLERRM')                            00002620
                              MAPSET('SQLERRM')                         00002630
                              ERASE                                     00002640
                              FRSET                                     00002650
                              FREEKB                                    00002660
                              END-EXEC.                                 00002670
                                                                        00002680
               EXEC CICS RECEIVE INTO(QF-ANSWER)                        00002690
                                 LENGTH(QF-LEN)                         00002700
                                 MAXLENGTH(QF-MAXLEN)                   00002710
                                 RESP(CICS-RESPONSE)                    00002720
               END-EXEC.                                                00002730
                                                                        00002740
       1000-EXIT.                                                       00002750
           EXIT.                                                        00002760
                                                                        00002770
                                                                        00002780
                                                                        00002790
                                                                        00002800
       1001-RETURN SECTION.                                             00002810
                                                                        00002820
      ************************************************                  00002830
      *  WRITE COST JOURNAL RECORD                                      00002840
      ************************************************                  00002850
           MOVE '  ' TO WS-COST-CARR-CODE.                              00002860
      *    PERFORM 9000-WRITE-COST-JOURNAL.                             00002870
                                                                        00002880
           EXEC CICS RETURN END-EXEC.                                   00002890
                                                                        00002900
       1001-EXIT.                                                       00002910
           EXIT.                                                        00002920
                                                                        00002930
       9000-WRITE-COST-JOURNAL SECTION.                                 00002940
                                                                        00002950
           COPY CAPRCINC.                                               00002960
                                                                        00002970
       9000-EXIT.                                                       00002980
           EXIT.                                                        00002990
                                                                        00003000
       9999-HANDLE-ERRORS SECTION.                                      00003010
                                                                        00003020
           IF ERROR-STATUS = DFHRESP(NOTOPEN)                           00003030
               GO TO 9999-NOTOPEN.                                      00003040
                                                                        00003050
           IF ERROR-STATUS = DFHRESP(DISABLED)                          00003060
               GO TO 9999-DISABLED.                                     00003070
                                                                        00003080
           IF ERROR-STATUS = DFHRESP(IOERR)                             00003090
               GO TO 9999-IOERR.                                        00003100
                                                                        00003110
           IF ERROR-STATUS = DFHRESP(LENGERR)                           00003120
               GO TO 9999-LENGERR.                                      00003130
                                                                        00003140
           IF ERROR-STATUS = DFHRESP(ILLOGIC)                           00003150
               GO TO 9999-ILLOGIC.                                      00003160
                                                                        00003170
           IF ERROR-STATUS = DFHRESP(INVREQ)                            00003180
               GO TO 9999-INVREQ.                                       00003190
                                                                        00003200
           IF ERROR-STATUS = DFHRESP(DSIDERR)                           00003210
               GO TO 9999-DSIDERR.                                      00003220
                                                                        00003230
           IF ERROR-STATUS = DFHRESP(DUPKEY)                            00003240
               GO TO 9999-DUPKEY.                                       00003250
                                                                        00003260
           IF ERROR-STATUS = DFHRESP(DUPREC)                            00003270
               GO TO 9999-DUPREC.                                       00003280
                                                                        00003290
       9999-ERROR.                                                      00003300
           MOVE 'SEVERE ERROR HAS OCCURRED, CALL HELP DESK'             00003310
           TO MAP-MESSAGEO.                                             00003320
           GO TO 9999-SEND-MAP.                                         00003330
                                                                        00003340
       9999-NOTOPEN.                                                    00003350
           MOVE 'FILE NOT OPEN, CALL HELP DESK'                         00003360
           TO MAP-MESSAGEO.                                             00003370
           GO TO 9999-SEND-MAP.                                         00003380
                                                                        00003390
       9999-DISABLED.                                                   00003400
           MOVE 'FILE DISABLED, CALL HELP DESK'                         00003410
           TO MAP-MESSAGEO.                                             00003420
           GO TO 9999-SEND-MAP.                                         00003430
                                                                        00003440
       9999-DSIDERR.                                                    00003450
           MOVE 'A DSIDERR ERROR HAS OCCURRED, CALL HELP DESK'          00003460
           TO MAP-MESSAGEO.                                             00003470
           GO TO 9999-SEND-MAP.                                         00003480
                                                                        00003490
       9999-ILLOGIC.                                                    00003500
           MOVE 'AN ILLOGIC ERROR HAS OCCURRED, CALL HELP DESK'         00003510
           TO MAP-MESSAGEO.                                             00003520
           GO TO 9999-SEND-MAP.                                         00003530
                                                                        00003540
       9999-INVREQ.                                                     00003550
           MOVE 'AN INVREQ ERROR HAS OCCURRED, CALL HELP DESK'          00003560
           TO MAP-MESSAGEO.                                             00003570
           GO TO 9999-SEND-MAP.                                         00003580
                                                                        00003590
       9999-IOERR.                                                      00003600
           MOVE 'A FILE I/O ERROR HAS OCCURRED, CALL HELP DESK'         00003610
           TO MAP-MESSAGEO.                                             00003620
           GO TO 9999-SEND-MAP.                                         00003630
                                                                        00003640
       9999-LENGERR.                                                    00003650
           MOVE 'A MAP LENGTH ERROR HAS OCCURRED, CALL HELP DESK'       00003660
           TO MAP-MESSAGEO.                                             00003670
           GO TO 9999-SEND-MAP.                                         00003680
                                                                        00003690
       9999-DUPKEY.                                                     00003700
           MOVE 'A DUPKEY ERROR HAS OCCURRED, CALL HELP DESK'           00003710
           TO MAP-MESSAGEO.                                             00003720
           GO TO 9999-SEND-MAP.                                         00003730
                                                                        00003740
       9999-DUPREC.                                                     00003750
           MOVE 'A DUPREC ERROR HAS OCCURRED, CALL HELP DESK'           00003760
           TO MAP-MESSAGEO.                                             00003770
           GO TO 9999-SEND-MAP.                                         00003780
                                                                        00003790
       9999-SEND-MAP.                                                   00003800
                                                                        00003810
                                                                        00003820
           PERFORM 1000-SEND-MAP.                                       00003830
                                                                        00003840
           PERFORM 1001-RETURN.                                         00003850
                                                                        00003860
       9999-EXIT.                                                       00003870
           EXIT.                                                        00003880
                                                                        00003890
                                                                        00003900
                                                                        00003910
                                                                        00003920
                                                                        00003930
                                                                        00003940
