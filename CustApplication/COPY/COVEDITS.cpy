           EXEC SQL DECLARE COVERAGE_EDITS TABLE                        COVEDITS
           ( CARRIER                        CHAR(2) NOT NULL,           COVEDITS
             PROD_TYPE                      CHAR(2) NOT NULL,           COVEDITS
             COV_ENTITY                     CHAR(10) NOT NULL,          COVEDITS
             COV_QUALIFIER                  CHAR(10) NOT NULL,          COVEDITS
             COV_VALUE                      CHAR(15) NOT NULL,          COVEDITS
             CURRENT_IND                    CHAR(1) NOT NULL,           COVEDITS
             RECORD_EFF_DATE                DATE NOT NULL,              COVEDITS
             RECORD_TERM_DATE               DATE,                       COVEDITS
             SEQ_NUM                        SMALLINT NOT NULL,          COVEDITS
             ADDED_DATE                     TIMESTAMP NOT NULL,         COVEDITS
             ADDED_LOGON                    CHAR(15) NOT NULL,          COVEDITS
             UPDATED_DATE                   TIMESTAMP,                  COVEDITS
             UPDATED_LOGON                  CHAR(15),                   COVEDITS
             XREF_ENTITY                    CHAR(10) NOT NULL,          COVEDITS
             XREF_QUALIFIER                 CHAR(10) NOT NULL,          COVEDITS
             XREF_VALUE                     CHAR(15) NOT NULL,          COVEDITS
             DEFAULT_IND                    CHAR(1) NOT NULL,           COVEDITS
R04137       STATE                          CHAR(2) NOT NULL,           COVEDITS
R04209       QUOTING                        CHAR(10) NOT NULL,          COVEDITS
R04209       OUT_FOR_UPDATE                 CHAR(1) NOT NULL,           COVEDITS
R04934       FROM_CASE_SIZE                 DECIMAL(7, 0) NOT NULL,     COVEDITS
R04934       TO_CASE_SIZE                   DECIMAL(7, 0) NOT NULL,     COVEDITS
R04934       REQUIRED                       CHAR(1) NOT NULL,           COVEDITS
R04934       DATA_LEVEL                     CHAR(10) NOT NULL,          COVEDITS
R04934       XREF_DATA_LEVEL                CHAR(10) NOT NULL,          COVEDITS
R05476       TIER_STRUCTURE                 CHAR(2),
R05476       UPDATE_TEXT                    VARCHAR(20) NOT NULL,
R05476       NEW_BUSINESS_IND               CHAR(1) NOT NULL,
R05476       SERIES_IND                     CHAR(5) NOT NULL,
R05476       EDIT_TYPE                      CHAR(10) NOT NULL,
R05476       ROLL_DOWN                      CHAR(10) NOT NULL,
R08754       XREF_PROD_TYPE                 CHAR(02) NOT NULL,          COVEDITS
R08754       DISPLAY_SEQUENCE               SMALLINT NOT NULL,          COVEDITS
391443       PROD_LVL_OWNING_CARRIER        CHAR(2) NOT NULL            COVEDITS
           ) END-EXEC.                                                  COVEDITS
      ******************************************************************COVEDITS
      * COBOL DECLARATION FOR TABLE COVERAGE_EDITS                     *COVEDITS
      ******************************************************************COVEDITS
       01  DCLCOVERAGE-EDITS.                                           COVEDITS
      *                       CARRIER                                   COVEDITS
           10 CE-CARRIER           PIC X(2).                            COVEDITS
      *                       PROD_TYPE                                 COVEDITS
           10 CE-PROD-TYPE         PIC X(2).                            COVEDITS
      *                       COV_ENTITY                                COVEDITS
           10 CE-COV-ENTITY        PIC X(10).                           COVEDITS
      *                       COV_QUALIFIER                             COVEDITS
           10 CE-COV-QUALIFIER     PIC X(10).                           COVEDITS
      *                       COV_VALUE                                 COVEDITS
           10 CE-COV-VALUE         PIC X(15).                           COVEDITS
      *                       CURRENT_IND                               COVEDITS
           10 CE-CURRENT-IND       PIC X(1).                            COVEDITS
      *                       RECORD_EFF_DATE                           COVEDITS
           10 CE-RECORD-EFF-DATE   PIC X(10).                           COVEDITS
      *                       RECORD_TERM_DATE                          COVEDITS
           10 CE-RECORD-TERM-DATE  PIC X(10).                           COVEDITS
      *                       SEQ_NUM                                   COVEDITS
           10 CE-SEQ-NUM           PIC S9(4) USAGE COMP.                COVEDITS
      *                       ADDED_DATE                                COVEDITS
           10 CE-ADDED-DATE        PIC X(26).                           COVEDITS
      *                       ADDED_LOGON                               COVEDITS
           10 CE-ADDED-LOGON       PIC X(15).                           COVEDITS
      *                       UPDATED_DATE                              COVEDITS
           10 CE-UPDATED-DATE      PIC X(26).                           COVEDITS
      *                       UPDATED_LOGON                             COVEDITS
           10 CE-UPDATED-LOGON     PIC X(15).                           COVEDITS
      *                       XREF_ENTITY                               COVEDITS
           10 CE-XREF-ENTITY       PIC X(10).                           COVEDITS
      *                       XREF_QUALIFIER                            COVEDITS
           10 CE-XREF-QUALIFIER    PIC X(10).                           COVEDITS
      *                       XREF_VALUE                                COVEDITS
           10 CE-XREF-VALUE        PIC X(15).                           COVEDITS
      *                       DEFAULT_IND                               COVEDITS
           10 CE-DEFAULT-IND       PIC X(1).                            COVEDITS
R04137*                       STATE                                     COVEDITS
R04137     10 CE-STATE             PIC X(2).                            COVEDITS
R04209*                       STATE                                     COVEDITS
R04209     10 CE-QUOTING           PIC X(10).                           COVEDITS
R04209*                       STATE                                     COVEDITS
R04209     10 CE-OUT-FOR-UPDATE    PIC X(1).                            COVEDITS
R04934*                       FROM_CASE_SIZE                            COVEDITS
R04934     10 CE-FROM-CASE-SIZE    PIC S9(7)V USAGE COMP-3.             COVEDITS
R04934*                       TO_CASE_SIZE                              COVEDITS
R04934     10 CE-TO-CASE-SIZE      PIC S9(7)V USAGE COMP-3.             COVEDITS
R04934*                       REQUIRED                                  COVEDITS
R04934     10 CE-REQUIRED          PIC X(1).                            COVEDITS
R04934*                       DATA_LEVEL                                COVEDITS
R04934     10 CE-DATA-LEVEL        PIC X(10).                           COVEDITS
R04934*                       XREF_DATA_LEVEL                           COVEDITS
R04934     10 CE-XREF-DATA-LEVEL   PIC X(10).                           COVEDITS
R05476*    *************************************************************
R05476     10 CE-TIER-STRUCTURE       PIC X(2).
R05476*    *************************************************************
R05476     10 CE-UPDATE-TEXT.
R05476        49 CE-UPDATE-TEXT-LEN   PIC S9(4) USAGE COMP.
R05476        49 CE-UPDATE-TEXT-TEXT  PIC X(20).
R05476*    *************************************************************
R05476     10 CE-NEW-BUSINESS-IND     PIC X(1).
R05476*    *************************************************************
R05476     10 CE-SERIES-IND           PIC X(5).
R05476*    *************************************************************
R05476     10 CE-EDIT-TYPE            PIC X(10).
R05476*    *************************************************************
R05476     10 CE-ROLL-DOWN            PIC X(10).
R08754     10 CE-XREF-PROD-TYPE       PIC X(10).                        COVEDITS
R08754     10 CE-DISPLAY-SEQUENCE     PIC S9(4) USAGE COMP.             COVEDITS
391443     10 CE-PROD-LVL-OWNING-CARRIER PIC X(02).                     COVEDITS
      ******************************************************************COVEDITS
      * INDICATOR VARIABLE STRUCTURE                                   *COVEDITS
      ******************************************************************COVEDITS
       01  COVERAGE-EDITS-IND-ARRAY.                                    COVEDITS
R08754     10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 34 TIMES.  COVEDITS
       01  FILLER REDEFINES COVERAGE-EDITS-IND-ARRAY.                   COVEDITS
           10 CE-CARRIER-IND           PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-PROD-TYPE-IND         PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-COV-ENTITY-IND        PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-COV-QUALIFIER-IND     PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-COV-VALUE-IND         PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-CURRENT-IND-IND       PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-RECORD-EFF-DATE-IND   PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-RECORD-TERM-DATE-IND  PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-SEQ-NUM-IND           PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-ADDED-DATE-IND        PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-ADDED-LOGON-IND       PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-UPDATED-DATE-IND      PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-UPDATED-LOGON-IND     PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-XREF-ENTITY-IND       PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-XREF-QUALIFIER-IND    PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-XREF-VALUE-IND        PIC S9(4) USAGE COMP.            COVEDITS
           10 CE-DEFAULT-IND-IND       PIC S9(4) USAGE COMP.            COVEDITS
R04137     10 CE-STATE-IND             PIC S9(4) USAGE COMP.            COVEDITS
R04209     10 CE-QUOTING-IND           PIC S9(4) USAGE COMP.            COVEDITS
R04209     10 CE-OUT-FOR-UPDATE-IND    PIC S9(4) USAGE COMP.            COVEDITS
R04934     10 CE-FROM-CASE-SIZE-IND    PIC S9(4) USAGE COMP.            COVEDITS
R04934     10 CE-TO-CASE-SIZE-IND      PIC S9(4) USAGE COMP.            COVEDITS
R04934     10 CE-REQUIRED-IND          PIC S9(4) USAGE COMP.            COVEDITS
R04934     10 CE-DATA-LEVEL-IND        PIC S9(4) USAGE COMP.            COVEDITS
R04934     10 CE-XREF-DATA-LEVEL-IND   PIC S9(4) USAGE COMP.            COVEDITS
R05476     10 CE-TIER-STRUCTURE-IND    PIC S9(4) USAGE COMP.            COVEDITS
R05476     10 CE-UPDATE-TEXT-LEN-IND   PIC S9(4) USAGE COMP.            COVEDITS
R05476     10 CE-UPDATE-TEXT-TEXT-IND  PIC S9(4) USAGE COMP.            COVEDITS
R05476     10 CE-NEW-BUSINESS-IND      PIC S9(4) USAGE COMP.            COVEDITS
R05476     10 CE-SERIES-IND            PIC S9(4) USAGE COMP.            COVEDITS
R05476     10 CE-EDIT-TYPE-IND         PIC S9(4) USAGE COMP.            COVEDITS
R05476     10 CE-ROLL-DOWN-IND         PIC S9(4) USAGE COMP.            COVEDITS
R08754     10 CE-XREF-PROD-TYPE-IND    PIC S9(4) USAGE COMP.            COVEDITS
R08754     10 CE-DISPLAY-SEQUENCE-IND  PIC S9(4) USAGE COMP.            COVEDITS
      ******************************************************************COVEDITS
R08754* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 34      *COVEDITS
R08754* NOTE: THE UPDATE-TEXT LEN AND TEXT IS SPLIT TO TWO INDICATORS! *COVEDITS
      ******************************************************************COVEDITS
