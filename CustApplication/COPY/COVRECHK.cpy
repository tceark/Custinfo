s       R08041-CHECK-COVERAGE-ROW.                                      00001700
                                                                        00001800
           MOVE WS-COV-EDIT-DATA-LEVEL   TO   CE-DATA-LEVEL.            00001900
                                                                        00002000
R8098A     EVALUATE TRUE                                                00002100
             WHEN  CE-COV-ENTITY = 'STATE     '                         00002200
               EXEC SQL                                                 00002300
                    SELECT RECORD_EFF_DATE                              00002400
                          ,XREF_ENTITY                                  00002500
                          ,XREF_QUALIFIER                               00002600
                          ,XREF_VALUE                                   00002700
                      INTO :CE-RECORD-EFF-DATE                          00002800
                          ,:CE-XREF-ENTITY                              00002900
                          ,:CE-XREF-QUALIFIER                           00003000
                          ,:CE-XREF-VALUE                               00003100
                      FROM COVERAGE_EDITS                               00003200
                     WHERE CARRIER        = :CE-CARRIER                 00003300
                       AND PROD_TYPE      = :CE-PROD-TYPE               00003400
                       AND DATA_LEVEL     = :CE-DATA-LEVEL              00003500
                       AND COV_ENTITY     = :CE-COV-ENTITY              00003600
                       AND COV_VALUE      = :CE-COV-VALUE               00003700
R9470A                 AND XREF_ENTITY    = :CE-XREF-ENTITY             00003800
R9470A                 AND XREF_QUALIFIER = :CE-XREF-QUALIFIER          00003900
R9470A                 AND XREF_VALUE     = :CE-XREF-VALUE              00004000
                       AND CURRENT_IND    = :CE-CURRENT-IND             00004100
               END-EXEC                                                 00004200
R8098A       WHEN  CE-COV-ENTITY = 'HEALTH    '                         00004300
R8098A       WHEN  CE-COV-ENTITY = 'RX        '                         00004400
R8098A       WHEN  CE-COV-ENTITY = 'MATERNITY '                         00004500
R8098A          EXEC SQL                                                00004600
R8098A            SELECT                                                00004700
R8098A               RECORD_EFF_DATE                                    00004800
R8098A             , XREF_ENTITY                                        00004900
R8098A             , XREF_QUALIFIER                                     00005000
R8098A             , XREF_VALUE                                         00005100
R8098A             , DEFAULT_IND                                        00005200
R8098A             , REQUIRED                                           00005300
R8098A             , NEW_BUSINESS_IND                                   00005400
R8098A             , SERIES_IND                                         00005500
R8098A             , EDIT_TYPE                                          00005600
R8098A            INTO                                                  00005700
R8098A              :CE-RECORD-EFF-DATE                                 00005800
R8098A            , :CE-XREF-ENTITY                                     00005900
R8098A            , :CE-XREF-QUALIFIER                                  00006000
R8098A            , :CE-XREF-VALUE                                      00006100
R8098A            , :CE-DEFAULT-IND                                     00006200
R8098A            , :CE-REQUIRED                                        00006300
R8098A            , :DCLCOVERAGE-EDITS.CE-NEW-BUSINESS-IND              00006400
R8098A            , :DCLCOVERAGE-EDITS.CE-SERIES-IND                    00006500
R8098A            , :CE-EDIT-TYPE                                       00006600
R8098A            FROM COVERAGE_EDITS                                   00006700
R8098A           WHERE COV_VALUE     = :CE-COV-VALUE                    00006800
R8098A             AND COV_ENTITY    = :CE-COV-ENTITY                   00006900
R8098A             AND COV_QUALIFIER = :CE-COV-QUALIFIER                00007000
R8098A             AND CARRIER       = :CE-CARRIER                      00007100
R8098A             AND PROD_TYPE     = :CE-PROD-TYPE                    00007200
R8098A             AND CURRENT_IND   = :CE-CURRENT-IND                  00007300
R8098A             AND STATE        IN (:CE-STATE, :WS-SPACE)           00007400
R8098A         END-EXEC                                                 00007500
R8098A       WHEN OTHER                                                 00007600
               EXEC SQL                                                 00007700
                    SELECT RECORD_EFF_DATE                              00007800
                          ,XREF_ENTITY                                  00007900
                          ,XREF_QUALIFIER                               00008000
                          ,XREF_VALUE                                   00008100
R8098A                    ,STATE                                        00008200
                      INTO :CE-RECORD-EFF-DATE                          00008300
                          ,:CE-XREF-ENTITY                              00008400
                          ,:CE-XREF-QUALIFIER                           00008500
                          ,:CE-XREF-VALUE                               00008600
                          ,:CE-STATE                                    00008700
                      FROM COVERAGE_EDITS                               00008800
                     WHERE CARRIER       = :CE-CARRIER                  00008900
                       AND PROD_TYPE     = :CE-PROD-TYPE                00009000
                       AND DATA_LEVEL    = :CE-DATA-LEVEL               00009100
                       AND COV_ENTITY    = :CE-COV-ENTITY               00009200
                       AND COV_QUALIFIER = :CE-COV-QUALIFIER            00009300
                       AND COV_VALUE     = :CE-COV-VALUE                00009400
                       AND CURRENT_IND   = :CE-CURRENT-IND              00009500
               END-EXEC                                                 00009600
R8098A     END-EVALUATE.                                                00009700
R8098A                                                                  00009800
           EVALUATE SQLCODE                                             00009900
               WHEN +0                                                  00010000
                    SET COVREDT-ROW-FOUND        TO TRUE                00010100
               WHEN +100                                                00010200
                    SET COVREDT-ROW-NOT-FND      TO TRUE                00010300
               WHEN OTHER                                               00010400
                    PERFORM 9999-DB2-ERROR                              00010500
           END-EVALUATE.                                                00010600
                                                                        00010700
       R08041-CHECK-COVERAGE-ROW-EXIT.                                  00010800
                                 EXIT.                                  00010900
