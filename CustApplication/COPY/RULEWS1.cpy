       01  WS-COMPILED-DATE                  PIC X(16) VALUE SPACES.
       01  WS-PROG-RULES-FIELDS.
           05  WT-PROGRAM-RULES-TABLE.
                10  WT-PROG-RULES-ENTRY OCCURS 2000 TIMES INDEXED
                                               BY WT-RULES-NDX.
                    15 WT-CARRIER            PIC X(02).
                    15 WT-RULE               PIC X(15).
                    15 WT-RULE-ENTITY        PIC X(10).
                    15 WT-RULE-QUALIFIER     PIC X(10).
                    15 WT-RULE-VALUE         PIC X(15).

           05  WS-TEST-VALUE                 PIC X(15).
           05  WS-TEST-RULE                  PIC X(15).
           05  WS-TEST-ENTITY                PIC X(10).
           05  WS-TEST-QUALIFIER             PIC X(10).
           05  WS-TEST-CARRIER               PIC X(02).
           05  WS-MAX-NDX                    PIC 9(04).
R08510     05  WS-RULE-VALUE-RETRIEVED       PIC X(15) VALUE ' '.

           05  WS-RULE-SWITCH                PIC X(01) VALUE 'N'.
R08510           88 RULE-SWITCH-PASSED       VALUE 'Y' 'D'.
R08510           88 RULE-SW-DEFAULT-CARRIER  VALUE 'D'.
      *          88 RULE-SWITCH-FAILED       VALUE 'N'.
116662           88 RULE-SWITCH-FAILED       VALUE 'N' 'C'.
116662           88 RULE-CARRIER-IN-RULE     VALUE 'C'.

R9374A     05  WS-RULE-1-SWITCH              PIC X(01) VALUE 'N'.
R9374A           88 RULE-1-PASSED            VALUE 'Y' 'D'.
R9374A           88 RULE-1-DEFAULT-CARRIER   VALUE 'D'.
116662           88 RULE-1-FAILED            VALUE 'N' 'C'.
116662           88 RULE-1-CARRIER-IN-RULE   VALUE 'C'.

R9374A     05  WS-RULE-2-SWITCH              PIC X(01) VALUE 'N'.
R9374A           88 RULE-2-PASSED            VALUE 'Y' 'D'.
R9374A           88 RULE-2-DEFAULT-CARRIER   VALUE 'D'.
116662           88 RULE-2-FAILED            VALUE 'N' 'C'.
116662           88 RULE-2-CARRIER-IN-RULE   VALUE 'C'.

R9374A     05  WS-RULE-3-SWITCH              PIC X(01) VALUE 'N'.
R9374A           88 RULE-3-PASSED            VALUE 'Y' 'D'.
R9374A           88 RULE-3-DEFAULT-CARRIER   VALUE 'D'.
116662           88 RULE-3-FAILED            VALUE 'N' 'C'.
116662           88 RULE-3-CARRIER-IN-RULE   VALUE 'C'.

R9374A     05  WS-RULE-4-SWITCH              PIC X(01) VALUE 'N'.
R9374A           88 RULE-4-PASSED            VALUE 'Y' 'D'.
R9374A           88 RULE-4-DEFAULT-CARRIER   VALUE 'D'.
116662           88 RULE-4-FAILED            VALUE 'N' 'C'.
116662           88 RULE-4-CARRIER-IN-RULE   VALUE 'C'.

R9374A     05  WS-RULE-5-SWITCH              PIC X(01) VALUE 'N'.
R9374A           88 RULE-5-PASSED            VALUE 'Y' 'D'.
R9374A           88 RULE-5-DEFAULT-CARRIER   VALUE 'D'.
116662           88 RULE-5-FAILED            VALUE 'N' 'C'.
116662           88 RULE-5-CARRIER-IN-RULE   VALUE 'C'.

R9374A     05  WS-RULE-6-SWITCH              PIC X(01) VALUE 'N'.
R9374A           88 RULE-6-PASSED            VALUE 'Y' 'D'.
R9374A           88 RULE-6-DEFAULT-CARRIER   VALUE 'D'.
116662           88 RULE-6-FAILED            VALUE 'N' 'C'.
116662           88 RULE-6-CARRIER-IN-RULE   VALUE 'C'.

R9374A     05  WS-RULE-7-SWITCH              PIC X(01) VALUE 'N'.
R9374A           88 RULE-7-PASSED            VALUE 'Y' 'D'.
R9374A           88 RULE-7-DEFAULT-CARRIER   VALUE 'D'.
116662           88 RULE-7-FAILED            VALUE 'N' 'C'.
116662           88 RULE-7-CARRIER-IN-RULE   VALUE 'C'.

R9374A     05  WS-RULE-8-SWITCH              PIC X(01) VALUE 'N'.
R9374A           88 RULE-8-PASSED            VALUE 'Y' 'D'.
R9374A           88 RULE-8-DEFAULT-CARRIER   VALUE 'D'.
116662           88 RULE-8-FAILED            VALUE 'N' 'C'.
116662           88 RULE-8-CARRIER-IN-RULE   VALUE 'C'.

R9374A     05  WS-RULE-9-SWITCH              PIC X(01) VALUE 'N'.
R9374A           88 RULE-9-PASSED            VALUE 'Y' 'D'.
R9374A           88 RULE-9-DEFAULT-CARRIER   VALUE 'D'.
116662           88 RULE-9-FAILED            VALUE 'N' 'C'.
116662           88 RULE-9-CARRIER-IN-RULE   VALUE 'C'.

R9374A     05  WS-RULE-PARAGRAPH-LOCATION    PIC  X(30) VALUE SPACES.

           COPY PROGRULE.

           EXEC SQL
              DECLARE PROG-RULES-CURSOR CURSOR FOR
               SELECT CARRIER,
                      RULE,
                      RULE_ENTITY,
                      RULE_QUALIFIER,
                      RULE_VALUE
                 FROM PROGRAM_RULES
                 WHERE CURRENT_IND= :WS-C
                 AND  PROGRAM_ID = :WS-PROGRAM-NAME
            END-EXEC.

      ******************************************************************
      * END OF COPYBOOK RULEWS1                                        *
      ******************************************************************
