       R07063-BUILD-PROG-RULES-TABLE.                                   00002200
           INITIALIZE WS-PROG-RULES-FIELDS.                             00002300
                                                                        00002400
           EXEC SQL                                                     00002500
                OPEN PROG-RULES-CURSOR                                  00002600
           END-EXEC.                                                    00002700
                                                                        00002800
           IF SQLCODE NOT EQUAL ZERO                                    00002810
R9374A        MOVE 'R07063-OPEN-PROG-RULES-TABLE'                       00002830
R9374A          TO WS-RULE-PARAGRAPH-LOCATION                           00002840
              PERFORM 9999-DB2-ERROR                                    00002850
           END-IF.                                                      00002860
                                                                        00003200
           SET WT-RULES-NDX TO +1.                                      00003300
           MOVE 1 TO WS-MAX-NDX.                                        00003400
                                                                        00003500
           PERFORM UNTIL SQLCODE = +100                                 00003600
                OR WT-RULES-NDX > +2000                                 00003700
                                                                        00003800
             EXEC SQL                                                   00003900
                 FETCH PROG-RULES-CURSOR                                00004000
                       INTO :PR-CARRIER        :PR-PROD-TYPE-IND        00004100
                          , :PR-RULE           :PR-RULE-IND             00004200
                          , :PR-RULE-ENTITY    :PR-RULE-ENTITY-IND      00004300
                          , :PR-RULE-QUALIFIER :PR-RULE-QUALIFIER-IND   00004400
                          , :PR-RULE-VALUE     :PR-RULE-VALUE-IND       00004500
             END-EXEC                                                   00004600
                                                                        00004700
             EVALUATE TRUE                                              00004800
                 WHEN SQLCODE = 0                                       00004900
                      MOVE PR-CARRIER    TO WT-CARRIER(WT-RULES-NDX)    00005000
                      MOVE PR-RULE       TO WT-RULE(WT-RULES-NDX)       00005100
                      MOVE PR-RULE-ENTITY                               00005200
                        TO WT-RULE-ENTITY(WT-RULES-NDX)                 00005210
                      MOVE PR-RULE-QUALIFIER                            00005300
                        TO WT-RULE-QUALIFIER(WT-RULES-NDX)              00005400
                      MOVE PR-RULE-VALUE TO WT-RULE-VALUE(WT-RULES-NDX) 00005500
                      SET WT-RULES-NDX UP BY +1                         00005600
                      ADD 1 TO WS-MAX-NDX                               00005700
                 WHEN SQLCODE = +100                                    00005800
                      CONTINUE                                          00005900
                 WHEN OTHER                                             00006000
R9374A                MOVE 'R07063-FETCH-PROG-RULES-TABLE'              00006020
R9374A                  TO WS-RULE-PARAGRAPH-LOCATION                   00006030
                      PERFORM 9999-DB2-ERROR                            00006100
             END-EVALUATE                                               00006200
                                                                        00006300
           END-PERFORM.                                                 00006400
                                                                        00006500
           IF WT-RULES-NDX > 2000 THEN                                  00006600
              PERFORM 9999-DB2-ERROR                                    00006700
           END-IF.                                                      00006800
                                                                        00006900
           EXEC SQL                                                     00007000
                CLOSE PROG-RULES-CURSOR                                 00007100
           END-EXEC.                                                    00007200
                                                                        00007300
           IF SQLCODE NOT EQUAL ZERO                                    00007400
R9374A        MOVE 'R07063-CLOSE-PROG-RULES-TABLE'                      00007420
R9374A          TO WS-RULE-PARAGRAPH-LOCATION                           00007430
              PERFORM 9999-DB2-ERROR                                    00007500
           END-IF.                                                      00007600
                                                                        00007700
       R07063-BUILD-PROG-RULES-EXIT.                                    00007800
           EXIT.                                                        00007900
                                                                        00008000
       R07063-TEST-RULE-VALUE-TBL.                                      00008100
           SET RULE-SWITCH-FAILED   TO TRUE.                            00008200
           SET WT-RULES-NDX         TO +1.                              00008300
                                                                        00008400
           PERFORM VARYING WT-RULES-NDX FROM 1 BY 1                     00008500
                           UNTIL WT-RULES-NDX > WS-MAX-NDX OR           00008600
                                 WS-RULE-SWITCH = 'Y'                   00008700
                                                                        00008800
            IF WT-RULE (WT-RULES-NDX)           = WS-TEST-RULE AND      00008900
               WT-RULE-VALUE (WT-RULES-NDX)     = WS-TEST-VALUE AND     00009000
               WT-RULE-ENTITY (WT-RULES-NDX)    = WS-TEST-ENTITY AND    00009100
               WT-RULE-QUALIFIER (WT-RULES-NDX) = WS-TEST-QUALIFIER AND 00009200
               WT-CARRIER (WT-RULES-NDX)        = WS-TEST-CARRIER       00009300
                              MOVE 'Y'          TO WS-RULE-SWITCH       00009400
            END-IF                                                      00009500
                                                                        00009600
           END-PERFORM.                                                 00009700
                                                                        00009725
116662***************************************************************** 00009770
116662*THE MATCH BELOW IS FOR WHEN A CARRIER PARTICIPATES IN THE RULE   00009780
116662*  AND DID NOT GET AN EXACT MATCH THEN THIS SWITCH CAN BE USED    00009790
116662*  TO APPLY A DEFAULT VALUE. SEE MG004.                           00009791
116662***************************************************************** 00009792
116662     IF  RULE-SWITCH-FAILED                                       00009793
116662         PERFORM VARYING WT-RULES-NDX FROM 1 BY 1                 00009794
116662           UNTIL WT-RULES-NDX > WS-MAX-NDX                        00009795
116662              OR RULE-CARRIER-IN-RULE                             00009796
116662*             OR WS-RULE-SWITCH = 'C'                             00009797
116662            IF   WT-RULE (WT-RULES-NDX)      = WS-TEST-RULE       00009798
116662             AND WT-RULE-ENTITY (WT-RULES-NDX) = WS-TEST-ENTITY   00009799
116662             AND WT-RULE-QUALIFIER (WT-RULES-NDX)                 00009800
116662                                             = WS-TEST-QUALIFIER  00009801
116662             AND WT-CARRIER (WT-RULES-NDX)   = WS-TEST-CARRIER    00009802
116662                 SET RULE-CARRIER-IN-RULE TO TRUE                 00009803
116662            END-IF                                                00009804
116662        END-PERFORM                                               00009805
116662     END-IF.                                                      00009806
                                                                        00009810
       R07063-TEST-RULE-VALUE-EXIT.                                     00009900
           EXIT.                                                        00010000
                                                                        00010100

      ****************************************************************  00011000
R9571A R07063-SELECT-RULE-VALUE.                                        00009900
      ****************************************************************  00011000

           EXEC SQL
               SELECT :WS-Y
                 INTO :WS-RULE-SWITCH                                   00009400
                 FROM PROGRAM_RULES
                WHERE CARRIER        = :PR-CARRIER                      00004100
                  AND RULE           = :PR-RULE                         00004200
                  AND RULE_ENTITY    = :PR-RULE-ENTITY                  00004300
                  AND RULE_QUALIFIER = :PR-RULE-QUALIFIER               00004400
                  AND RULE_VALUE     = :PR-RULE-VALUE                   00004500
                  AND CURRENT_IND    = :WS-C                            00004200
                  AND PROGRAM_ID     = :PR-PROGRAM-ID
            END-EXEC.

           EVALUATE TRUE
               WHEN SQLCODE = +0
                    CONTINUE                                            00009400
               WHEN SQLCODE = -811
                    SET  RULE-SWITCH-PASSED   TO TRUE
               WHEN SQLCODE = +100
                    SET  RULE-SWITCH-FAILED   TO TRUE
               WHEN OTHER
                    SET  RULE-SWITCH-FAILED   TO TRUE
                    MOVE 'R07063-SELECT-RULE-VALUE'                     00009900
                      TO WS-RULE-PARAGRAPH-LOCATION                     00006030
                    PERFORM 9999-DB2-ERROR                              00006100
           END-EVALUATE.

R9571A R07063-SELECT-RULE-VALUE-EXIT.                                   00009900
R9571A     EXIT.


R08510 R07064-RETRIEVE1-RULE-VAL-TBL.                                   00010200
R08510     SET RULE-SWITCH-FAILED   TO TRUE.                            00010300
R08510     SET WT-RULES-NDX         TO +1.                              00010400
R08510                                                                  00010500
R08510     PERFORM VARYING WT-RULES-NDX FROM 1 BY 1                     00010600
R08510                     UNTIL WT-RULES-NDX > WS-MAX-NDX OR           00010700
R08510                           WS-RULE-SWITCH = 'Y'                   00010800
R08510                                                                  00010900
R08510***************************************************************** 00011000
R08510*THE MATCH BELOW IS ON THE FOUR COLUMNS: RULE, CARRIER,           00011100
R08510*  RULE_ENTITY, RULE_QUALIFIER. IF A MATCH IS FOUND, THE          00011200
R08510*  RULE_VALUE COLUMN IS RETURNED. THIS IS A DETAIL LEVEL MATCH.   00011300
R08510***************************************************************** 00011400
R08510      IF WT-RULE (WT-RULES-NDX)           = WS-TEST-RULE AND      00011500
R08510         WT-RULE-ENTITY (WT-RULES-NDX)    = WS-TEST-ENTITY AND    00011600
R08510         WT-RULE-QUALIFIER (WT-RULES-NDX) = WS-TEST-QUALIFIER AND 00011700
R08510         WT-CARRIER (WT-RULES-NDX)        = WS-TEST-CARRIER       00011800
R08510                        MOVE 'Y'          TO WS-RULE-SWITCH       00011900
R08510                        MOVE WT-RULE-VALUE (WT-RULES-NDX)         00012000
R08510                          TO WS-RULE-VALUE-RETRIEVED              00012100
R08510      END-IF                                                      00012200
R08510                                                                  00012300
R08510     END-PERFORM                                                  00012400
R08510                                                                  00012500
R08510     IF RULE-SWITCH-FAILED                                        00012600
R08510      PERFORM VARYING WT-RULES-NDX FROM 1 BY 1                    00012700
R08510                      UNTIL WT-RULES-NDX > WS-MAX-NDX OR          00012800
R08510*                           WS-RULE-SWITCH = 'Y'                  00012900
116662                            WS-RULE-SWITCH = 'D'                  00012910
R08510                                                                  00013000
R08510***************************************************************** 00013100
R08510*THE MATCH BELOW IS FOR A DEFAULT ROW AT CARRIER LEVEL. THE       00013200
R08510*  COLUMNS: RULE, CARRIER, RULE_ENTITY, AND RULE_QUALIFIER = 'XX' 00013300
R08510*  ARE MATCHED. IF A MATCH IS FOUND, THE RULE_VALUE COLUMN IS     00013400
R08510*  RETURNED AS THE DEFAULT ROW AT CARRIER LEVEL.                  00013500
R08510***************************************************************** 00013600
R08510         IF WT-RULE (WT-RULES-NDX)           = WS-TEST-RULE AND   00013700
R08510            WT-RULE-ENTITY (WT-RULES-NDX)    = WS-TEST-ENTITY AND 00013800
R08510            WT-CARRIER (WT-RULES-NDX)        = WS-TEST-CARRIER AND00013900
R08510            WT-RULE-QUALIFIER (WT-RULES-NDX) = 'XX'               00014000
R08510                           MOVE 'D'          TO WS-RULE-SWITCH    00014100
R08510                           MOVE WT-RULE-VALUE (WT-RULES-NDX)      00014200
R08510                             TO WS-RULE-VALUE-RETRIEVED           00014300
R08510         END-IF                                                   00014400
R08510      END-PERFORM                                                 00014500
R08510     END-IF                                                       00014600
R08510                                                                  00014700
R08510     IF RULE-SWITCH-FAILED                                        00014800
R08510      PERFORM VARYING WT-RULES-NDX FROM 1 BY 1                    00014900
R08510                      UNTIL WT-RULES-NDX > WS-MAX-NDX OR          00015000
R08510*                           WS-RULE-SWITCH = 'Y'                  00015100
116662                            WS-RULE-SWITCH = 'D'                  00015110
R08510                                                                  00015200
R08510***************************************************************** 00015300
R08510*THE MATCH BELOW IS FOR A DEFAULT CARRIER ROW AT HPS SYSTEM LEVEL.00015400
R08510*  THE COLUMNS: RULE, RULE_ENTITY, RULE_QUALIFIER, AND CARRIER =  00015500
R08510*  'XX' ARE MATCHED. IF A MATCH IS FOUND, THE RULE_VALUE COLUMN IS00015600
R08510*  RETURNED AS THE DEFAULT CARRIER ROW AT HPS SYSTEM LEVEL.       00015700
R08510***************************************************************** 00015800
R08510         IF WT-RULE (WT-RULES-NDX)           = WS-TEST-RULE AND   00015900
R08510            WT-RULE-ENTITY (WT-RULES-NDX)    = WS-TEST-ENTITY AND 00016000
R08510          WT-RULE-QUALIFIER (WT-RULES-NDX) = WS-TEST-QUALIFIER AND00016100
R08510            WT-CARRIER (WT-RULES-NDX)        = 'XX'               00016200
R08510                           MOVE 'D'          TO WS-RULE-SWITCH    00016300
R08510                           MOVE WT-RULE-VALUE (WT-RULES-NDX)      00016400
R08510                             TO WS-RULE-VALUE-RETRIEVED           00016500
R08510         END-IF                                                   00016600
R08510      END-PERFORM                                                 00016700
R08510     END-IF.                                                      00016800
                                                                        00016900
R08510 R07064-RETRIEVE1-RULE-EXIT.                                      00017010
R08510     EXIT.                                                        00017100
R08510                                                                  00017200
