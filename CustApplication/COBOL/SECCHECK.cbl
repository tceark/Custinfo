       IDENTIFICATION DIVISION.                                         00000010
       PROGRAM-ID.    SECCHECK.                                         00000020
       DATE-COMPILED.                                                   00000030
      *------------------------PROGRAM PURPOSE-------------------------*00000360
      *  PROGRAM TITLE: SECCHECK                                       *00000370
      *  PROGRAM TEXT:  DETERMINE IF USER HAS SECURITY TO PERFORM      *00000380
      *                 REQUESTED ACT                              *0000039
      *                                                                *00000400
      *----------------------------------------------------------------*00000410
                                                                        00000600
       ENVIRONMENT DIVISION.                                            00000610
       CONFIGURATION SECTION.                                           00000620
       SOURCE-COMPUTER.        IBM-370.                                 00000630
       OBJECT-COMPUTER.        IBM-370.                                 00000640
       DATA DIVISION.                                                   00000650
       WORKING-STORAGE SECTION.                                         00000660
       01  WS-USERID01                 PIC X(8).
           COPY AUDCICS.                                                00000670
       77  LC-CLASS-NAME               PIC X(8).                        00000680
       77  LC-APPLID                   PIC X(8).                        00000690
       77  LV-RESOURCE                 PIC X(45)   VALUE ALL SPACES.    00000700
       77  LV-RESID-LENGTH             PIC S9(8)                 BINARY.00000710
       77  LV-READ-RESULT              PIC S9(8)                 BINARY.00000720
       77  LV-UPDATE-RESULT            PIC S9(8)                 BINARY.00000730
       77  LV-CONTROL-RESULT           PIC S9(8)                 BINARY.00000740
       77  LV-ALTER-RESULT             PIC S9(8)                 BINARY.00000750
           TITLE 'SECCHECK --> LINKAGE SECTION'.                        00000760
       LINKAGE SECTION.                                                 00000770
       01  DFHCOMMAREA.                                                 00000780
           COPY SECCOMMC.                                               00000790
           TITLE 'SECCHECK --> MAINLINE LOGIC'.                         00000800
       PROCEDURE DIVISION.                                              00000810
       P000000-MAINLINE  SECTION.                                       00000820
      *                                                                 00000830
      ***** VALIDATE FUNCTION CODE                                      00000840
      *                                                                 00000850
           EVALUATE TRUE                                                00000860
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'I'                   00000870
                   CONTINUE                                             00000880
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'A'                   00000890
                   CONTINUE                                             00000900
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'U'                   00000910
                   CONTINUE                                             00000920
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'D'                   00000930
                   CONTINUE                                             00000940
               WHEN OTHER                                               00000950
                   MOVE 'G' TO SEC-RETURN-CODE                          00000960
                   GO TO P000000-RETURN                                 00000970
           END-EVALUATE.                                                00000980
      *                                                                 00000990
      ***** SET CLASS NAME                                              00001000
      *                                                                 00001010
           EXEC CICS ASSIGN                                             00001020
               APPLID (LC-APPLID)                                       00001030
               USERID (WS-USERID01)
           END-EXEC.                                                    00001040
      *    IF  LC-APPLID IS EQUAL TO 'CICSHADT' OR                      00001050
      *                              'CICSMG1T' OR                      00001060
      *                              'CICSMG2T' OR                      00001070
      *                              'CICSHADA' OR                      00001080
      *                              'CICSHADG' OR                      00001090
      *                              'CICSHADK' OR                      00001100
      *                              'CICSHADN' OR                      00001110
      *                              'CICSHADX' OR                      00001120
      *                              'CICSHADY' OR                      00001130
      *                              'CICSHADU'                         00001140
      *        MOVE 'R$PTST  ' TO LC-CLASS-NAME                         00001150
      *    ELSE                                                         00001160
      *        MOVE 'R$PPRD  ' TO LC-CLASS-NAME                         00001170
      *    END-IF.                                                      00001180
           IF  LC-APPLID IS EQUAL TO 'CICSHADP' OR                      00001181
                                     'CICSMG1P' OR                      00001182
                                     'CICSNHAP' OR                      00001183
                                     'CICSNHMP'                         00001184
               MOVE 'R$PPRD  ' TO LC-CLASS-NAME                         00001191
           ELSE                                                         00001192
               MOVE 'R$PTST  ' TO LC-CLASS-NAME                         00001193
           END-IF.                                                      00001194
      *                                                                 00001195
      ***** GO DO REQUESTED SECURITY CHECK                              00001200
      *                                                                 00001210
           EVALUATE TRUE                                                00001220
               WHEN SEC-CHK-RESOURCE IS EQUAL TO 'Y'                    00001230
                   PERFORM P100000-CHECK-RESOURCE                       00001240
               WHEN SEC-CHK-CARRIER IS EQUAL TO 'Y'                     00001250
                   PERFORM P200000-CHECK-CARR                           00001260
               WHEN SEC-CHK-SITE IS EQUAL TO 'Y'                        00001270
                   PERFORM P300000-CHECK-SITE                           00001280
               WHEN OTHER                                               00001290
                   PERFORM P400000-CHECK-OTHER                          00001300
           END-EVALUATE.                                                00001310
                                                                        00001320
      *** WHEN SEC-CHK-RESOURCE = 'Y' AND THE SEC-CHK-CARR ALSO = 'Y'   00001330
      *** THEN P200000-CHECK-CARR DID NOT GET EXECUTED ABOVE. STILL     00001340
      *** NEED TO DO IT SO LONG AS NO SECCHECK ERROR OCCURRED ABOVE.    00001350
           IF SEC-CHK-RESOURCE = 'Y' AND SEC-RETURN-CODE = 'A'          00001360
              IF SEC-CHK-CARRIER = 'Y'                                  00001370
                 PERFORM P200000-CHECK-CARR                             00001380
              END-IF                                                    00001390
           END-IF.                                                      00001400
      *                                                                 00001410
      ***** RETURN                                                      00001420
      *                                                                 00001430
       P000000-RETURN.                                                  00001440
                                                                        00001450
           EXEC CICS                                                    00001460
               RETURN                                                   00001470
           END-EXEC.                                                    00001480
                                                                        00001490
       P000000-EXIT.                                                    00001500
           EXIT.                                                        00001510
           TITLE 'SECCHECK --> CHECK RESOURCE'.                         00001520
       P100000-CHECK-RESOURCE SECTION.                                  00001530
      *                                                                 00001540
      ***** SET DEFAULT RETURN CODE                                     00001550
      *                                                                 00001560
           MOVE 'A' TO SEC-RETURN-CODE.                                 00001570
      *                                                                 00001580
      ***** HANDLE ERRORS                                               00001590
      *                                                                 00001600
           EXEC CICS HANDLE CONDITION                                   00001610
               INVREQ(P100000-RETURNE)                                  00001620
               LENGERR(P100000-RETURNE)                                 00001630
               NOTFND(P100000-RETURNE)                                  00001640
               QIDERR(P100000-RETURNE)                                  00001650
           END-EXEC.                                                    00001660
      *                                                                 00001670
      ***** BUILD RESOURCE ID                                           00001680
      *                                                                 00001690
           MOVE +1 TO LV-RESID-LENGTH.                                  00001700
           STRING 'RESOURCE.' DELIMITED BY SIZE                         00001710
               SEC-RESOURCE-NAME DELIMITED BY SIZE                      00001720
               INTO LV-RESOURCE                                         00001730
               WITH POINTER LV-RESID-LENGTH                             00001740
           END-STRING.                                                  00001750
           SUBTRACT +1 FROM LV-RESID-LENGTH.                            00001760
      *                                                                 00001770
      ***** CHECK WITH RACF                                             00001780
      *                                                                 00001790
           EXEC CICS QUERY SECURITY                                     00001800
               RESCLASS(LC-CLASS-NAME)                                  00001810
               RESID(LV-RESOURCE)                                       00001820
               RESIDLENGTH(LV-RESID-LENGTH)                             00001830
               NOLOG                                                    00001840
      ***      LOG                                                      00001850
               READ(LV-READ-RESULT)                                     00001860
               UPDATE(LV-UPDATE-RESULT)                                 00001870
               CONTROL(LV-CONTROL-RESULT)                               00001880
               ALTER(LV-ALTER-RESULT)                                   00001890
           END-EXEC.                                                    00001900
      *                                                                 00001910
      ***** CHECK RACF RESULTS                                          00001920
      *                                                                 00001930
           EVALUATE TRUE                                                00001940
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'I'                   00001950
                   IF  LV-READ-RESULT IS EQUAL TO DFHVALUE(NOTREADABLE) 00001960
                       GO TO P100000-RETURNB                            00001970
                   END-IF                                               00001980
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'A'                   00001990
                   IF  LV-UPDATE-RESULT IS EQUAL TO                     00002000
                           DFHVALUE(NOTUPDATABLE)                       00002010
                       GO TO P100000-RETURNB                            00002020
                   END-IF                                               00002030
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'U'                   00002040
                   IF  LV-UPDATE-RESULT IS EQUAL TO                     00002050
                           DFHVALUE(NOTUPDATABLE)                       00002060
                       GO TO P100000-RETURNB                            00002070
                   END-IF                                               00002080
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'D'                   00002090
                   IF  LV-ALTER-RESULT IS EQUAL TO                      00002100
                           DFHVALUE(NOTALTERABLE)                       00002110
                       GO TO P100000-RETURNB                            00002120
                   END-IF                                               00002130
           END-EVALUATE.                                                00002140
           GO TO P100000-EXIT.                                          00002150
                                                                        00002160
       P100000-RETURNB.                                                 00002170
           MOVE 'B' TO SEC-RETURN-CODE.                                 00002180
           GO TO P100000-EXIT.                                          00002190
       P100000-RETURNE.                                                 00002200
           MOVE 'E' TO SEC-RETURN-CODE.                                 00002210
           GO TO P100000-EXIT.                                          00002220
                                                                        00002230
       P100000-EXIT.                                                    00002240
           EXIT.                                                        00002250
           TITLE 'SECCHECK --> CHECK CARR'.                             00002260
       P200000-CHECK-CARR SECTION.                                      00002270
      *                                                                 00002280
      ***** SET DEFAULT RETURN CODE                                     00002290
      *                                                                 00002300
           MOVE 'A' TO SEC-RETURN-CODE.                                 00002310
      *                                                                 00002320
      ***** HANDLE ERRORS                                               00002330
      *                                                                 00002340
           EXEC CICS HANDLE CONDITION                                   00002350
               INVREQ(P200000-DOCARR)                                   00002360
               LENGERR(P200000-DOCARR)                                  00002370
               NOTFND(P200000-DOCARR)                                   00002380
               QIDERR(P200000-DOCARR)                                   00002390
           END-EXEC.                                                    00002400
      *                                                                 00002410
      ***** BUILD RESOURCE ID                                           00002420
      *                                                                 00002430
           MOVE +1 TO LV-RESID-LENGTH.                                  00002440
           STRING 'ALLCARRS' DELIMITED BY SIZE                          00002450
               INTO LV-RESOURCE                                         00002460
               WITH POINTER LV-RESID-LENGTH                             00002470
           END-STRING.                                                  00002480
           SUBTRACT +1 FROM LV-RESID-LENGTH.                            00002490
      *                                                                 00002500
      ***** CHECK WITH RACF                                             00002510
      *                                                                 00002520
           EXEC CICS QUERY SECURITY                                     00002530
               RESCLASS(LC-CLASS-NAME)                                  00002540
               RESID(LV-RESOURCE)                                       00002550
               RESIDLENGTH(LV-RESID-LENGTH)                             00002560
               NOLOG                                                    00002570
      ***      LOG                                                      00002580
               READ(LV-READ-RESULT)                                     00002590
               UPDATE(LV-UPDATE-RESULT)                                 00002600
               CONTROL(LV-CONTROL-RESULT)                               00002610
               ALTER(LV-ALTER-RESULT)                                   00002620
           END-EXEC.                                                    00002630
      *                                                                 00002640
      ***** CHECK RACF RESULTS                                          00002650
      *                                                                 00002660
           EVALUATE TRUE                                                00002670
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'I'                   00002680
                   IF  LV-READ-RESULT IS EQUAL TO DFHVALUE(NOTREADABLE) 00002690
                       GO TO P200000-DOCARR                             00002700
                   END-IF                                               00002710
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'A'                   00002720
                   IF  LV-UPDATE-RESULT IS EQUAL TO                     00002730
                           DFHVALUE(NOTUPDATABLE)                       00002740
                       GO TO P200000-DOCARR                             00002750
                   END-IF                                               00002760
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'U'                   00002770
                   IF  LV-UPDATE-RESULT IS EQUAL TO                     00002780
                           DFHVALUE(NOTUPDATABLE)                       00002790
                       GO TO P200000-DOCARR                             00002800
                   END-IF                                               00002810
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'D'                   00002820
                   IF  LV-ALTER-RESULT IS EQUAL TO                      00002830
                           DFHVALUE(NOTALTERABLE)                       00002840
                       GO TO P200000-DOCARR                             00002850
                   END-IF                                               00002860
           END-EVALUATE.                                                00002870
           GO TO P200000-EXIT.                                          00002880
                                                                        00002890
       P200000-DOCARR.                                                  00002900
      *                                                                 00002910
      ***** HANDLE ERRORS                                               00002920
      *                                                                 00002930
           EXEC CICS HANDLE CONDITION                                   00002940
               INVREQ(P200000-RETURNE)                                  00002950
               LENGERR(P200000-RETURNE)                                 00002960
               NOTFND(P200000-RETURNE)                                  00002970
               QIDERR(P200000-RETURNE)                                  00002980
           END-EXEC.                                                    00002990
      *                                                                 00003000
      ***** BUILD RESOURCE ID                                           00003010
      *                                                                 00003020
           MOVE +1 TO LV-RESID-LENGTH.                                  00003030
           STRING 'CARR.CARR' DELIMITED BY SIZE                         00003040
               SEC-CARRIER-CODE DELIMITED BY SIZE                       00003050
               INTO LV-RESOURCE                                         00003060
               WITH POINTER LV-RESID-LENGTH                             00003070
           END-STRING.                                                  00003080
           SUBTRACT +1 FROM LV-RESID-LENGTH.                            00003090
      *                                                                 00003100
      ***** CHECK WITH RACF                                             00003110
      *                                                                 00003120
           EXEC CICS QUERY SECURITY                                     00003130
               RESCLASS(LC-CLASS-NAME)                                  00003140
               RESID(LV-RESOURCE)                                       00003150
               RESIDLENGTH(LV-RESID-LENGTH)                             00003160
               NOLOG                                                    00003170
      ***      LOG                                                      00003180
               READ(LV-READ-RESULT)                                     00003190
               UPDATE(LV-UPDATE-RESULT)                                 00003200
               CONTROL(LV-CONTROL-RESULT)                               00003210
               ALTER(LV-ALTER-RESULT)                                   00003220
           END-EXEC.                                                    00003230
      *                                                                 00003240
      ***** CHECK RACF RESULTS                                          00003250
      *                                                                 00003260
           EVALUATE TRUE                                                00003270
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'I'                   00003280
                   IF  LV-READ-RESULT IS EQUAL TO DFHVALUE(NOTREADABLE) 00003290
                       GO TO P200000-RETURNC                            00003300
                   END-IF                                               00003310
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'A'                   00003320
                   IF  LV-UPDATE-RESULT IS EQUAL TO                     00003330
                           DFHVALUE(NOTUPDATABLE)                       00003340
                       GO TO P200000-RETURNC                            00003350
                   END-IF                                               00003360
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'U'                   00003370
                   IF  LV-UPDATE-RESULT IS EQUAL TO                     00003380
                           DFHVALUE(NOTUPDATABLE)                       00003390
                       GO TO P200000-RETURNC                            00003400
                   END-IF                                               00003410
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'D'                   00003420
                   IF  LV-ALTER-RESULT IS EQUAL TO                      00003430
                           DFHVALUE(NOTALTERABLE)                       00003440
                       GO TO P200000-RETURNC                            00003450
                   END-IF                                               00003460
           END-EVALUATE.                                                00003470
           GO TO P200000-EXIT.                                          00003480
                                                                        00003490
       P200000-RETURNC.                                                 00003500
           MOVE 'C' TO SEC-RETURN-CODE.                                 00003510
           GO TO P200000-EXIT.                                          00003520
       P200000-RETURNE.                                                 00003530
           MOVE 'E' TO SEC-RETURN-CODE.                                 00003540
           GO TO P200000-EXIT.                                          00003550
                                                                        00003560
       P200000-EXIT.                                                    00003570
           EXIT.                                                        00003580
           TITLE 'SECCHECK --> CHECK SITE'.                             00003590
       P300000-CHECK-SITE SECTION.                                      00003600
      *                                                                 00003610
      ***** SET DEFAULT RETURN CODE                                     00003620
      *                                                                 00003630
           MOVE 'A' TO SEC-RETURN-CODE.                                 00003640
      *                                                                 00003650
      ***** HANDLE ERRORS                                               00003660
      *                                                                 00003670
           EXEC CICS HANDLE CONDITION                                   00003680
               INVREQ(P300000-DOSITE)                                   00003690
               LENGERR(P300000-DOSITE)                                  00003700
               NOTFND(P300000-DOSITE)                                   00003710
               QIDERR(P300000-DOSITE)                                   00003720
           END-EXEC.                                                    00003730
      *                                                                 00003740
      ***** BUILD RESOURCE ID                                           00003750
      *                                                                 00003760
           MOVE +1 TO LV-RESID-LENGTH.                                  00003770
           STRING 'ALLSITES' DELIMITED BY SIZE                          00003780
               INTO LV-RESOURCE                                         00003790
               WITH POINTER LV-RESID-LENGTH                             00003800
           END-STRING.                                                  00003810
           SUBTRACT +1 FROM LV-RESID-LENGTH.                            00003820
      *                                                                 00003830
      ***** CHECK WITH RACF                                             00003840
      *                                                                 00003850
           EXEC CICS QUERY SECURITY                                     00003860
               RESCLASS(LC-CLASS-NAME)                                  00003870
               RESID(LV-RESOURCE)                                       00003880
               RESIDLENGTH(LV-RESID-LENGTH)                             00003890
               NOLOG                                                    00003900
      ***      LOG                                                      00003910
               READ(LV-READ-RESULT)                                     00003920
               UPDATE(LV-UPDATE-RESULT)                                 00003930
               CONTROL(LV-CONTROL-RESULT)                               00003940
               ALTER(LV-ALTER-RESULT)                                   00003950
           END-EXEC.                                                    00003960
      *                                                                 00003970
      ***** CHECK RACF RESULTS                                          00003980
      *                                                                 00003990
           EVALUATE TRUE                                                00004000
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'I'                   00004010
                   IF  LV-READ-RESULT IS EQUAL TO DFHVALUE(NOTREADABLE) 00004020
                       GO TO P300000-DOSITE                             00004030
                   END-IF                                               00004040
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'A'                   00004050
                   IF  LV-UPDATE-RESULT IS EQUAL TO                     00004060
                           DFHVALUE(NOTUPDATABLE)                       00004070
                       GO TO P300000-DOSITE                             00004080
                   END-IF                                               00004090
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'U'                   00004100
                   IF  LV-UPDATE-RESULT IS EQUAL TO                     00004110
                           DFHVALUE(NOTUPDATABLE)                       00004120
                       GO TO P300000-DOSITE                             00004130
                   END-IF                                               00004140
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'D'                   00004150
                   IF  LV-ALTER-RESULT IS EQUAL TO                      00004160
                           DFHVALUE(NOTALTERABLE)                       00004170
                       GO TO P300000-DOSITE                             00004180
                   END-IF                                               00004190
           END-EVALUATE.                                                00004200
           GO TO P300000-EXIT.                                          00004210
                                                                        00004220
       P300000-DOSITE.                                                  00004230
      *                                                                 00004240
      ***** HANDLE ERRORS                                               00004250
      *                                                                 00004260
           EXEC CICS HANDLE CONDITION                                   00004270
               INVREQ(P300000-RETURNE)                                  00004280
               LENGERR(P300000-RETURNE)                                 00004290
               NOTFND(P300000-RETURNE)                                  00004300
               QIDERR(P300000-RETURNE)                                  00004310
           END-EXEC.                                                    00004320
      *                                                                 00004330
      ***** BUILD RESOURCE ID                                           00004340
      *                                                                 00004350
           MOVE +1 TO LV-RESID-LENGTH.                                  00004360
           STRING 'SITE.SITE' DELIMITED BY SIZE                         00004370
               SEC-SITE-CODE DELIMITED BY SIZE                          00004380
               INTO LV-RESOURCE                                         00004390
               WITH POINTER LV-RESID-LENGTH                             00004400
           END-STRING.                                                  00004410
           SUBTRACT +1 FROM LV-RESID-LENGTH.                            00004420
      *                                                                 00004430
      ***** CHECK WITH RACF                                             00004440
      *                                                                 00004450
           EXEC CICS QUERY SECURITY                                     00004460
               RESCLASS(LC-CLASS-NAME)                                  00004470
               RESID(LV-RESOURCE)                                       00004480
               RESIDLENGTH(LV-RESID-LENGTH)                             00004490
               NOLOG                                                    00004500
      ***      LOG                                                      00004510
               READ(LV-READ-RESULT)                                     00004520
               UPDATE(LV-UPDATE-RESULT)                                 00004530
               CONTROL(LV-CONTROL-RESULT)                               00004540
               ALTER(LV-ALTER-RESULT)                                   00004550
           END-EXEC.                                                    00004560
      *                                                                 00004570
      ***** CHECK RACF RESULTS                                          00004580
      *                                                                 00004590
           EVALUATE TRUE                                                00004600
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'I'                   00004610
                   IF  LV-READ-RESULT IS EQUAL TO DFHVALUE(NOTREADABLE) 00004620
                       GO TO P300000-RETURND                            00004630
                   END-IF                                               00004640
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'A'                   00004650
                   IF  LV-UPDATE-RESULT IS EQUAL TO                     00004660
                           DFHVALUE(NOTUPDATABLE)                       00004670
                       GO TO P300000-RETURND                            00004680
                   END-IF                                               00004690
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'U'                   00004700
                   IF  LV-UPDATE-RESULT IS EQUAL TO                     00004710
                           DFHVALUE(NOTUPDATABLE)                       00004720
                       GO TO P300000-RETURND                            00004730
                   END-IF                                               00004740
               WHEN SEC-FUNCTION-CODE IS EQUAL TO 'D'                   00004750
                   IF  LV-ALTER-RESULT IS EQUAL TO                      00004760
                           DFHVALUE(NOTALTERABLE)                       00004770
                       GO TO P300000-RETURND                            00004780
                   END-IF                                               00004790
           END-EVALUATE.                                                00004800
           GO TO P300000-EXIT.                                          00004810
                                                                        00004820
       P300000-RETURND.                                                 00004830
           MOVE 'D' TO SEC-RETURN-CODE.                                 00004840
           GO TO P300000-EXIT.                                          00004850
       P300000-RETURNE.                                                 00004860
           MOVE 'E' TO SEC-RETURN-CODE.                                 00004870
           GO TO P300000-EXIT.                                          00004880
                                                                        00004890
       P300000-EXIT.                                                    00004900
           EXIT.                                                        00004910
           TITLE 'SECCHECK --> CHECK OTHER'.                            00004920
       P400000-CHECK-OTHER SECTION.                                     00004930
                                                                        00004940
           MOVE 'F' TO SEC-RETURN-CODE.                                 00004950
                                                                        00004960
       P400000-EXIT.                                                    00004970
           EXIT.                                                        00004980
