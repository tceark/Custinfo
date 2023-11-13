          EXEC SQL DECLARE PROGRAM_RULES TABLE
           ( PROGRAM_ID                     CHAR(8) NOT NULL,
             RULE                           CHAR(15) NOT NULL,
             CARRIER                        CHAR(2) NOT NULL,
             RULE_ENTITY                    CHAR(10) NOT NULL,
             RULE_QUALIFIER                 CHAR(10) NOT NULL,
             RULE_VALUE                     CHAR(15) NOT NULL,
             CURRENT_IND                    CHAR(1) NOT NULL,
             EFFECTIVE_DATE                 DATE NOT NULL,
             TERM_DATE                      DATE,
             SEQ_NUM                        SMALLINT NOT NULL,
             OUT_FOR_UPDATE                 CHAR(1) NOT NULL,
             UPDATE_TEXT                    VARCHAR(20) NOT NULL,
             ADDED_DATE                     TIMESTAMP NOT NULL,
             ADDED_LOGON                    CHAR(15) NOT NULL,
             REVISED_DATE                   TIMESTAMP,
             REVISED_LOGON                  CHAR(15),
             RELEASE_DATE                   DATE,
             CHG_REQUEST_ID                 CHAR(10) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE PROGRAM_RULES                     *
      ******************************************************************
       01  DCLPROGRAM-RULES.
      *                       PROGRAM_ID
           10 PR-PROGRAM-ID        PIC X(8).
      *                       RULE
           10 PR-RULE              PIC X(15).
      *                       CARRIER
           10 PR-CARRIER           PIC X(2).
      *                       RULE_ENTITY
           10 PR-RULE-ENTITY       PIC X(10).
      *                       RULE_QUALIFIER
           10 PR-RULE-QUALIFIER    PIC X(10).
      *                       RULE_VALUE
           10 PR-RULE-VALUE        PIC X(15).
      *                       CURRENT_IND
           10 PR-CURRENT-IND       PIC X(1).
      *                       EFFECTIVE_DATE
           10 PR-EFFECTIVE-DATE    PIC X(10).
      *                       TERM_DATE
           10 PR-TERM-DATE         PIC X(10).
      *                       SEQ_NUM
           10 PR-SEQ-NUM           PIC S9(4) USAGE COMP.
      *                       OUT_FOR_UPDATE
           10 PR-OUT-FOR-UPDATE    PIC X(1).
           10 PR-UPDATE-TEXT.
      *                       UPDATE_TEXT LENGTH
              49 PR-UPDATE-TEXT-LEN
                 PIC S9(4) USAGE COMP.
      *                       UPDATE_TEXT
              49 PR-UPDATE-TEXT-TEXT
                 PIC X(20).
      *                       ADDED_DATE
           10 PR-ADDED-DATE        PIC X(26).
      *                       ADDED_LOGON
           10 PR-ADDED-LOGON       PIC X(15).
      *                       REVISED_DATE
           10 PR-REVISED-DATE      PIC X(26).
      *                       REVISED_LOGON
           10 PR-REVISED-LOGON     PIC X(15).
      *                       RELEASE_DATE
           10 PR-RELEASE-DATE      PIC X(10).
      *                       CHG_REQUEST_ID
           10 PR-CHG-REQUEST-ID    PIC X(10).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  IPROGRAM-RULES.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 18 TIMES.
       01  FILLER REDEFINES IPROGRAM-RULES.                             COVEDITS
           10 PR-PROGRAM-ID-IND        PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-RULE-IND              PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-PROD-TYPE-IND           PIC S9(4) USAGE COMP.          COVEDITS
           10 PR-RULE-ENTITY-IND       PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-RULE-QUALIFIER-IND     PIC S9(4) USAGE COMP.           COVEDITS
           10 PR-RULE-VALUE-IND         PIC S9(4) USAGE COMP.           COVEDITS
           10 PR-CURRENT-IND-IND       PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-RECORD-EFF-DATE-IND   PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-RECORD-TERM-DATE-IND  PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-SEQ-NUM-IND           PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-OUT-FOR-UPDATE-IND    PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-UPDATE-TEXT-IND       PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-ADDED-DATE-IND        PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-ADDED-LOGON-IND       PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-REVISED-DATE-IND      PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-REVISED-LOGON-IND     PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-RELEASE-DATE-IND      PIC S9(4) USAGE COMP.            COVEDITS
           10 PR-CHG-REQUEST-ID-IND    PIC S9(4) USAGE COMP.            COVEDITS
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
