       10000-CALL-GSF SECTION.                                          TURBOCAL
       10000-CALL-GSF-PARA.                                             TURBOCAL
                                                                        TURBOCAL
           MOVE TURBO-GROUP TO WT-GROUP.                                TURBOCAL
           MOVE TURBO-CNTL-AREA TO WT-HOLDAREA.                         TURBOCAL
           MOVE WT-KEY-LENGTH TO WT-KEY-LENGTH.                         TURBOCAL
           MOVE WT-ENTRY-LENGTH TO WT-ENTRY-LENGTH.                     TURBOCAL
           MOVE WT-PARM-LENGTH TO WT-PARM-LENGTH.                       TURBOCAL
           MOVE WT-PARM TO TB-SAVE-AREA.                                TURBOCAL
                                                                        TURBOCAL
           PERFORM 10001-PREPARE-COPYBOOK.                              TURBOCAL
                                                                        TURBOCAL
           MOVE WT-GROUP TO TURBO-GROUP.                                TURBOCAL
           MOVE TB-SAVE-AREA TO WT-PARM.                                TURBOCAL
           MOVE WT-HOLDAREA TO TURBO-CNTL-AREA.                         TURBOCAL
                                                                        TURBOCAL
       10000-EXIT.                                                      TURBOCAL
           EXIT.                                                        TURBOCAL
                                                                        TURBOCAL
                                                                        TURBOCAL
                                                                        TURBOCAL
       10001-PREPARE-COPYBOOK  SECTION.                                 TURBOCAL
                                                                        TURBOCAL
           MOVE WT-PARM TO TB-HOLD-AREA.                                TURBOCAL
                                                                        TURBOCAL
           IF TB-FIRST-BYTE = 'C'                                       TURBOCAL
               MOVE TB-HOLD-AREA TO TB-COPY-AREA                        TURBOCAL
               MOVE TB-COPY-AREA TO TB-HOLD-REMAINDER                   TURBOCAL
               MOVE SPACE TO TB-FIRST-BYTE                              TURBOCAL
               MOVE TB-HOLD-AREA TO WT-PARM.                            TURBOCAL
                                                                        TURBOCAL
           IF TB-HOLD-DD-NAME NOT = 'IS600V02' AND 'TURBDATA'           TURBOCAL
               MOVE TURBO-CNTL-AREA TO WT-HOLDAREA                      TURBOCAL
               GO TO 10001-CALL-GSF.                                    TURBOCAL
                                                                        TURBOCAL
           MOVE 'TURBDATA' TO TB-HOLD-DD-NAME.                          TURBOCAL
           MOVE TB-HOLD-AREA TO WT-PARM.                                TURBOCAL
           MOVE 'GSFRB.10' TO WT-TERMINATOR.                            TURBOCAL
                                                                        TURBOCAL
      *    IF TB-APPLID = '       '                                     TURBOCAL
      *       EXEC CICS ASSIGN APPLID(TB-APPLID) END-EXEC.              TURBOCAL
                                                                        TURBOCAL
      *    IF TB-APPLID = 'PSICICT' OR 'PSI1CICS'                       TURBOCAL
      *        MOVE 'IS600V04' TO TB-HOLD-DD-NAME                       TURBOCAL
      *        MOVE TB-HOLD-AREA TO WT-PARM                             TURBOCAL
      *        GO TO 10001-CALL-GSF.                                    TURBOCAL
      *                                                                 TURBOCAL
      *    IF WS-DEMO-PROGRAM = SPACES                                  TURBOCAL
      *        EXEC CICS ASSIGN TCTUALENG(TCTUAL) END-EXEC              TURBOCAL
      *        EXEC CICS ADDRESS TCTUA(ADDRESS OF TCTUAR) END-EXEC      TURBOCAL
      *        MOVE TCTUA-DEMO-DATA TO WS-DEMOGRAPHIC-COMM-AREA.        TURBOCAL
      *                                                                 TURBOCAL
      *    IF WS-DEMO-CICS-TEST-FLAG NOT = 'T'                          TURBOCAL
      *        GO TO 10001-CALL-GSF.                                    TURBOCAL
      *                                                                 TURBOCAL
      *    PERFORM 10005-CHECK-TRANSID.                                 TURBOCAL
      *    IF TB-ERROR-STATUS = DFHRESP(NOTFND)                         TURBOCAL
      *        GO TO 10001-CALL-GSF.                                    TURBOCAL
      *                                                                 TURBOCAL
      *    PERFORM 10006-CHECK-TABLE.                                   TURBOCAL
      *    IF TB-ERROR-STATUS = DFHRESP(NOTFND)                         TURBOCAL
      *        GO TO 10001-CALL-GSF.                                    TURBOCAL
      *                                                                 TURBOCAL
      *    MOVE 'TST' TO WT-NAME-PREF.                                  TURBOCAL
      *    MOVE 'IS600V04' TO TB-HOLD-DD-NAME.                          TURBOCAL
      *    MOVE TB-HOLD-AREA TO WT-PARM.                                TURBOCAL
                                                                        TURBOCAL
       10001-CALL-GSF.                                                  TURBOCAL
                                                                        TURBOCAL
      *    IF WS-DEMO-LOGONID = 'DPADSPC' OR 'DPSPBSF '                 TURBOCAL
      *      IF EIBAID = DFHPF1                                         TURBOCAL
      *        EXEC CICS SEND TEXT FROM (WT-PARM)                       TURBOCAL
      *                            LENGTH(80)                           TURBOCAL
      *                            ERASE END-EXEC                       TURBOCAL
      *         EXEC CICS RETURN END-EXEC.                              TURBOCAL
      ***  CALL DL-GSF-ROUTINE USING WT-HOLDAREA.                       TURBOCAL
      *    CALL 'PSIGSFC'      USING WT-HOLDAREA.                       TURBOCAL
      *    EXEC CICS ASSIGN APPLID(TB-APPLID) END-EXEC.
      *    CALL WS-PSIGSFC     USING WT-HOLDAREA.                       TURBOCAL
      *    EXEC CICS ASSIGN APPLID(TB-APPLID) END-EXEC.
                                                                        TURBOCAL
       10001-EXIT.                                                      TURBOCAL
           EXIT.                                                        TURBOCAL
                                                                        TURBOCAL
                                                                        TURBOCAL
      *10005-CHECK-TRANSID SECTION.                                     TURBOCAL
      *                                                                 TURBOCAL
      *    MOVE SPACES TO TRANID-KEY.                                   TURBOCAL
      *    MOVE '1001' TO TRANID-RECORD-TYPE.                           TURBOCAL
      *    MOVE EIBTRNID TO TRANID-TRANID.                              TURBOCAL
      *    MOVE ZEROS TO TRANID-EFF-DATE.                               TURBOCAL
      *                                                                 TURBOCAL
      *    EXEC CICS READ DATASET ('IS600V04')                          TURBOCAL
      *                   INTO (CARRIER-CONTROL-RECORD)                 TURBOCAL
      *                   RIDFLD (TRANID-KEY)                           TURBOCAL
      *                   LENGTH (TB-RECORD-LEN)                        TURBOCAL
      *                   RESP (TB-ERROR-STATUS)                        TURBOCAL
      *                   END-EXEC.                                     TURBOCAL
      *                                                                 TURBOCAL
      *    IF TB-ERROR-STATUS = DFHRESP(NOTFND)                         TURBOCAL
      *        GO TO 10005-EXIT.                                        TURBOCAL
      *                                                                 TURBOCAL
      *    IF TB-ERROR-STATUS NOT = DFHRESP(NORMAL)                     TURBOCAL
      *        PERFORM 10007-HANDLE-ERRORS.                             TURBOCAL
      *                                                                 TURBOCAL
      *                                                                 TURBOCAL
      *10005-EXIT.                                                      TURBOCAL
      *    EXIT.                                                        TURBOCAL
      *                                                                 TURBOCAL
      *10006-CHECK-TABLE SECTION.                                       TURBOCAL
      *                                                                 TURBOCAL
      *    MOVE SPACES TO TESTABLE-KEY.                                 TURBOCAL
      *    MOVE '1002' TO TESTABLE-RECORD-TYPE.                         TURBOCAL
      *    MOVE WS-DEMO-LOGONID   TO TESTABLE-LID.                      TURBOCAL
      *    MOVE TB-HOLD-TABLE     TO TESTABLE-TABLE.                    TURBOCAL
      *    MOVE ZEROS TO TESTABLE-EFF-DATE.                             TURBOCAL
      *                                                                 TURBOCAL
      *    EXEC CICS READ DATASET ('IS600V04')                          TURBOCAL
      *                   INTO (CARRIER-CONTROL-RECORD)                 TURBOCAL
      *                   RIDFLD (TESTABLE-KEY)                         TURBOCAL
      *                   LENGTH (TB-RECORD-LEN)                        TURBOCAL
      *                   RESP (TB-ERROR-STATUS)                        TURBOCAL
      *                   END-EXEC.                                     TURBOCAL
      *                                                                 TURBOCAL
      *    IF TB-ERROR-STATUS = DFHRESP(NOTFND)                         TURBOCAL
      *        GO TO 10006-EXIT.                                        TURBOCAL
      *                                                                 TURBOCAL
      *    IF TB-ERROR-STATUS NOT = DFHRESP(NORMAL)                     TURBOCAL
      *        PERFORM 10007-HANDLE-ERRORS.                             TURBOCAL
      *                                                                 TURBOCAL
      *                                                                 TURBOCAL
      *10006-EXIT.                                                      TURBOCAL
      *    EXIT.                                                        TURBOCAL
      *                                                                 TURBOCAL
      *10007-HANDLE-ERRORS SECTION.                                     TURBOCAL
      *                                                                 TURBOCAL
      *    IF TB-ERROR-STATUS = DFHRESP(NOTOPEN)                        TURBOCAL
      *        MOVE 403 TO WT-RETURN.                                   TURBOCAL
      *                                                                 TURBOCAL
      *    IF TB-ERROR-STATUS = DFHRESP(IOERR)                          TURBOCAL
      *        MOVE 500 TO WT-RETURN.                                   TURBOCAL
      *                                                                 TURBOCAL
      *    IF TB-ERROR-STATUS = DFHRESP(ILLOGIC)                        TURBOCAL
      *        MOVE 501 TO WT-RETURN.                                   TURBOCAL
      *                                                                 TURBOCAL
      *    IF TB-ERROR-STATUS = DFHRESP(INVREQ)                         TURBOCAL
      *        MOVE 502 TO WT-RETURN.                                   TURBOCAL
      *                                                                 TURBOCAL
      *    IF TB-ERROR-STATUS = DFHRESP(DSIDERR)                        TURBOCAL
      *        MOVE 503 TO WT-RETURN.                                   TURBOCAL
      *                                                                 TURBOCAL
      *    IF TB-ERROR-STATUS = DFHRESP(DUPKEY)                         TURBOCAL
      *        MOVE 504 TO WT-RETURN.                                   TURBOCAL
      *                                                                 TURBOCAL
      *                                                                 TURBOCAL
      *    EXEC CICS RETURN END-EXEC.                                   TURBOCAL
      *    GOBACK.                                                      TURBOCAL
      *                                                                 TURBOCAL
      *10007-EXIT.                                                      TURBOCAL
      *    EXIT.                                                        TURBOCAL
       11111-CALL-GSF-TERMINATOR  SECTION.                              TURBOCAL
                                                                        TURBOCAL
