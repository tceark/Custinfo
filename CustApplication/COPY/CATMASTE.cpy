           EXEC SQL DECLARE CATMASTER TABLE                             CATMASTE
           ( CAT_IDNTITY_NUMBER               CHAR(8) NOT NULL,           CATMAS
             CAT_VERSION_QUAL               CHAR(1) NOT NULL,           CATMASTE
             CAT_DIST_CHANNEL               CHAR(1) NOT NULL,           CATMASTE
             CAT_FIELD_FORCE                CHAR(2) NOT NULL,           CATMASTE
             CAT_ALT_FF                     CHAR(2) NOT NULL,           CATMASTE
             CAT_PRODUCT_CODE               CHAR(2) NOT NULL,           CATMASTE
             CAT_ACTIVE_IND                 CHAR(1) NOT NULL,           CATMASTE
             CAT_UNDERWRITER                CHAR(3) NOT NULL,           CATMASTE
             CAT_RECEIVED_DATE              DECIMAL(6, 0) NOT NULL,     CATMASTE
             CAT_PREMIUM_AMOUNT             DECIMAL(7, 2) NOT NULL,     CATMASTE
             CAT_DECLINE_REASON             CHAR(3) NOT NULL,           CATMASTE
             CAT_DECLINED_DATE              DECIMAL(6, 0) NOT NULL,     CATMASTE
             CAT_STATE_CODE                 CHAR(2) NOT NULL,           CATMASTE
             CAT_MKT_SITE                   CHAR(2) NOT NULL,           CATMASTE
             CAT_LICENSE_STATUS             CHAR(1) NOT NULL,           CATMASTE
             CAT_LICENSE_LID                CHAR(8) NOT NULL,           CATMASTE
             CAT_LICENSE_DATE               DECIMAL(6, 0) NOT NULL,     CATMASTE
             CAT_UNDER_STATUS               CHAR(1) NOT NULL,           CATMASTE
             CAT_UNDER_LID                  CHAR(8) NOT NULL,           CATMASTE
             CAT_UNDER_DATE                 DECIMAL(6, 0) NOT NULL,     CATMASTE
             CAT_VERIF_STATUS               CHAR(1) NOT NULL,           CATMASTE
             CAT_VERIF_LID                  CHAR(8) NOT NULL,           CATMASTE
             CAT_VERIF_DATE                 DECIMAL(6, 0) NOT NULL,     CATMASTE
             CAT_CERTIF_STATUS              CHAR(1) NOT NULL,           CATMASTE
             CAT_CERTIF_LID                 CHAR(8) NOT NULL,           CATMASTE
             CAT_CERTIF_DATE                DECIMAL(6, 0) NOT NULL,     CATMASTE
             CAT_ISSUE_STATUS               CHAR(1) NOT NULL,           CATMASTE
             CAT_ISSUE_LID                  CHAR(8) NOT NULL,           CATMASTE
             CAT_ISSUE_DATE                 DECIMAL(6, 0) NOT NULL,     CATMASTE
             CAT_ZIP_CODE                   CHAR(5) NOT NULL,           CATMASTE
             CAT_UNDERWRTN_DATE             DECIMAL(6, 0) NOT NULL,     CATMASTE
             CAT_COCARR_CIMNUM              CHAR(8) NOT NULL,             CATMAS
             CAT_UPDATE_DATE                DATE NOT NULL,              CATMASTE
             CAT_UPDATE_LID                 CHAR(8) NOT NULL,           CATMASTE
             CAT_OVERRIDE_FLAG              CHAR(1) NOT NULL,           CATMASTE
             CAT_FPP_FLAG                   CHAR(1) NOT NULL,           CATMASTE
             CAT_SALES_REP_LID              CHAR(8) NOT NULL,           CATMASTE
941056       CAT_CLOSEOUT_DATE              DATE,                       CATMASTE
941056       RECEIVED_DATE                  DATE,                       CATMASTE
941056       DECLINED_DATE                  DATE,                       CATMASTE
941056       LICENSE_DATE                   DATE,                       CATMASTE
941056       UNDER_DATE                     DATE,                       CATMASTE
941056       VERIF_DATE                     DATE,                       CATMASTE
941056       CERTIF_DATE                    DATE,                       CATMASTE
941056       ISSUE_DATE                     DATE,                       CATMASTE
941056       UNDERWRTN_DATE                 DATE,                       CATMASTE
960903       CAT_UNDER_REC_DATE             DATE,                       CATMASTE
R00723       TVU_STATUS                     CHAR(1) NOT NULL,           CATMASTE
R00723       TVU_DATE                       DATE,                       CATMASTE
R00723       ADV_COMM_STATUS                CHAR(1) NOT NULL,           CATMASTE
R00723       ADV_COMM_DATE                  DATE,                       CATMASTE
R01368       CAT_COM_PREPAY_FLG             CHAR(1) NOT NULL,           CATMASTE
R01368       CAT_COM_PREPAY_AMT             DECIMAL(13, 2) NOT NULL,    CATMASTE
R01368       CAT_COM_PREPAY_DTE             DATE,                       CATMASTE
88136A       APP_SIGNED_DATE                DATE,
88136A       SCAN_DATE                      DATE,
88136A       UW_COMPLETE_DATE               DATE,
88136A       CAT_DECLINE_ID                 VARCHAR(15) NOT NULL,
88136A       UW_DECISION                    VARCHAR(15) NOT NULL,
88136A       HOLD_DATE                      DATE,
88136A       APPL_SCRUB_DT                  DATE,
88136A       APPL_SCRUB_COMP_DT             DATE
           ) END-EXEC.                                                  CATMASTE
      ******************************************************************CATMASTE
      * COBOL DECLARATION FOR TABLE CATMASTER                          *CATMASTE
      ******************************************************************CATMASTE
       01  CAT-MASTER-REC.                                              CATMASTE
           10 CAT1                 PIC X(8).                            CATMASTE
           10 CAT2                 PIC X(1).                            CATMASTE
           10 CAT3                 PIC X(1).                            CATMASTE
           10 CAT4                 PIC X(2).                            CATMASTE
           10 CAT5                 PIC X(2).                            CATMASTE
           10 CAT6                 PIC X(2).                            CATMASTE
           10 CAT7                 PIC X(1).                            CATMASTE
           10 CAT8                 PIC X(3).                            CATMASTE
           10 CAT9                 PIC S999999V USAGE COMP-3.           CATMASTE
           10 CAT10                PIC S99999V99 USAGE COMP-3.          CATMASTE
           10 CAT11                PIC X(3).                            CATMASTE
           10 CAT12                PIC S999999V USAGE COMP-3.           CATMASTE
           10 CAT13                PIC X(2).                            CATMASTE
           10 CAT14                PIC X(2).                            CATMASTE
           10 CAT15                PIC X(1).                            CATMASTE
           10 CAT16                PIC X(8).                            CATMASTE
           10 CAT17                PIC S999999V USAGE COMP-3.           CATMASTE
           10 CAT18                PIC X(1).                            CATMASTE
           10 CAT19                PIC X(8).                            CATMASTE
           10 CAT20                PIC S999999V USAGE COMP-3.           CATMASTE
           10 CAT21                PIC X(1).                            CATMASTE
           10 CAT22                PIC X(8).                            CATMASTE
           10 CAT23                PIC S999999V USAGE COMP-3.           CATMASTE
           10 CAT24                PIC X(1).                            CATMASTE
           10 CAT25                PIC X(8).                            CATMASTE
           10 CAT26                PIC S999999V USAGE COMP-3.           CATMASTE
           10 CAT27                PIC X(1).                            CATMASTE
           10 CAT28                PIC X(8).                            CATMASTE
           10 CAT29                PIC S999999V USAGE COMP-3.           CATMASTE
           10 CAT30                PIC X(5).                            CATMASTE
           10 CAT31                PIC S999999V USAGE COMP-3.           CATMASTE
           10 CAT32                PIC X(8).                            CATMASTE
           10 CAT33                PIC X(10).                           CATMASTE
           10 CAT34                PIC X(8).                            CATMASTE
           10 CAT35                PIC X(1).                            CATMASTE
           10 CAT36                PIC X(1).                            CATMASTE
           10 CAT37                PIC X(8).                            CATMASTE
941056     10 CAT38                PIC X(10).                           CATMASTE
941056     10 CAT39                PIC X(10).                           CATMASTE
941056     10 CAT40                PIC X(10).                           CATMASTE
941056     10 CAT41                PIC X(10).                           CATMASTE
941056     10 CAT42                PIC X(10).                           CATMASTE
941056     10 CAT43                PIC X(10).                           CATMASTE
941056     10 CAT44                PIC X(10).                           CATMASTE
941056     10 CAT45                PIC X(10).                           CATMASTE
941056     10 CAT46                PIC X(10).                           CATMASTE
960903     10 CAT47                PIC X(10).                           CATMASTE
R00723     10 CAT48                PIC X(1).                            CATMASTE
R00723     10 CAT49                PIC X(10).                           CATMASTE
R00723     10 CAT50                PIC X(1).                            CATMASTE
R00723     10 CAT51                PIC X(10).                           CATMASTE
R01368     10 CAT52                PIC X(1).                            CATMASTE
R01368     10 CAT53                PIC S9(11)V9(2) USAGE COMP-3.        CATMASTE
R01368     10 CAT54                PIC X(10).                           CATMASTE
88136A     10 CAT55                PIC X(10).
88136A     10 CAT56                PIC X(10).
88136A     10 CAT57                PIC X(10).
88136A     10 CAT58.
88136A        49 CAT58-LEN         PIC S9(4) USAGE COMP.
88136A        49 CAT58-TEXT        PIC X(15).
88136A     10 CAT59.
88136A        49 CAT59-LEN         PIC S9(4) USAGE COMP.
88136A        49 CAT59-TEXT        PIC X(15).
88136A     10 CAT60                PIC X(10).
88136A     10 CAT61                PIC X(10).
88136A     10 CAT62                PIC X(10).
                                                                        CATMASTE
      ******************************************************************CATMASTE
                                                                        CATMASTE
       01  CAT-INDICATOR-ARRAY.                                         CATMASTE
88136A     05  CAT-INDICATOR OCCURS 62 TIMES PIC S9(4) COMP.            CATMASTE
       01  FILLER REDEFINES CAT-INDICATOR-ARRAY.                        CATMASTE
           05  CAT-IDNTY-NUMBER-IND             PIC S9(4) COMP.           CATMAS
           05  CAT-VERSION-QUAL-IND           PIC S9(4) COMP.           CATMASTE
           05  CAT-DIST-CHANNEL-IND           PIC S9(4) COMP.           CATMASTE
           05  CAT-FIELD-FORCE-IND            PIC S9(4) COMP.           CATMASTE
           05  CAT-ALT-FF-IND                 PIC S9(4) COMP.           CATMASTE
           05  CAT-PRODUCT-CODE-IND           PIC S9(4) COMP.           CATMASTE
           05  CAT-ACTIVE-IND-IND             PIC S9(4) COMP.           CATMASTE
           05  CAT-UNDERWRITER-IND            PIC S9(4) COMP.           CATMASTE
           05  CAT-RECEIVED-DATE-IND          PIC S9(4) COMP.           CATMASTE
           05  CAT-CHECK-AMOUNT-IND           PIC S9(4) COMP.           CATMASTE
           05  CAT-DECLINE-REASON-IND         PIC S9(4) COMP.           CATMASTE
           05  CAT-DECLINED-DATE-IND          PIC S9(4) COMP.           CATMASTE
           05  CAT-STATE-CODE-IND             PIC S9(4) COMP.           CATMASTE
           05  CAT-MKT-SITE-IND               PIC S9(4) COMP.           CATMASTE
           05  CAT-LICENSE-STATUS-IND         PIC S9(4) COMP.           CATMASTE
           05  CAT-LICENSE-LID-IND            PIC S9(4) COMP.           CATMASTE
           05  CAT-LICENSE-DATE-IND           PIC S9(4) COMP.           CATMASTE
           05  CAT-UNDER-STATUS-IND           PIC S9(4) COMP.           CATMASTE
           05  CAT-UNDER-LID-IND              PIC S9(4) COMP.           CATMASTE
           05  CAT-UNDER-DATE-IND             PIC S9(4) COMP.           CATMASTE
           05  CAT-VERIF-STATUS-IND           PIC S9(4) COMP.           CATMASTE
           05  CAT-VERIF-LID-IND              PIC S9(4) COMP.           CATMASTE
           05  CAT-VERIF-DATE-IND             PIC S9(4) COMP.           CATMASTE
           05  CAT-CERTIF-STATUS-IND          PIC S9(4) COMP.           CATMASTE
           05  CAT-CERTIF-LID-IND             PIC S9(4) COMP.           CATMASTE
           05  CAT-CERTIF-DATE-IND            PIC S9(4) COMP.           CATMASTE
           05  CAT-ISSUE-STATUS-IND           PIC S9(4) COMP.           CATMASTE
           05  CAT-ISSUE-LID-IND              PIC S9(4) COMP.           CATMASTE
           05  CAT-ISSUE-DATE-IND             PIC S9(4) COMP.           CATMASTE
           05  CAT-ZIP-CODE-IND               PIC S9(4) COMP.           CATMASTE
           05  CAT-UNDERWRTN-DATE-IND         PIC S9(4) COMP.           CATMASTE
           05  CAT-COCARR-IDNTYNUM-IND          PIC S9(4) COMP.           CATMAS
           05  CAT-UPDATE-DATE-IND            PIC S9(4) COMP.           CATMASTE
           05  CAT-UPDATE-LID-IND             PIC S9(4) COMP.           CATMASTE
           05  CAT-OVERRIDE-FLAG-IND          PIC S9(4) COMP.           CATMASTE
           05  CAT-FPP-FLAG-IND               PIC S9(4) COMP.           CATMASTE
           05  CAT-SALES-REP-LID-IND          PIC S9(4) COMP.           CATMASTE
941056     05  CAT-CLOSEOUT-DATE-IND          PIC S9(4) COMP.           CATMASTE
941056     05  RECEIVED-DATE-IND              PIC S9(4) COMP.           CATMASTE
941056     05  DECLINED-DATE-IND              PIC S9(4) COMP.           CATMASTE
941056     05  LICENSE-DATE-IND               PIC S9(4) COMP.           CATMASTE
941056     05  UNDER-DATE-IND                 PIC S9(4) COMP.           CATMASTE
941056     05  VERIF-DATE-IND                 PIC S9(4) COMP.           CATMASTE
941056     05  CERTIF-DATE-IND                PIC S9(4) COMP.           CATMASTE
941056     05  ISSUE-DATE-IND                 PIC S9(4) COMP.           CATMASTE
941056     05  UNDERWRTN-DATE-IND             PIC S9(4) COMP.           CATMASTE
960903     05  CAT-UNDER-REC-DATE-IND         PIC S9(4) COMP.           CATMASTE
R00723     05  TVU-STATUS-IND                 PIC S9(4) COMP.           CATMASTE
R00723     05  TVU-DATE-IND                   PIC S9(4) COMP.           CATMASTE
R00723     05  ADV-COMM-STATUS-IND            PIC S9(4) COMP.           CATMASTE
R00723     05  ADV-COMM-DATE-IND              PIC S9(4) COMP.           CATMASTE
R01368     05  CAT-COM-PREPAY-FLG-IND         PIC S9(4) COMP.           CATMASTE
R01368     05  CAT-COM-PREPAY-AMT-IND         PIC S9(4) COMP.           CATMASTE
R01368     05  CAT-COM-PREPAY-DTE-IND         PIC S9(4) COMP.           CATMASTE
88136A     05  CAT-APP-SIGNED-DATE-IND        PIC S9(4) COMP.
88136A     05  CAT-SCAN-DATE-IND              PIC S9(4) COMP.
88136A     05  CAT-UW-COMPLETE-DATE-IND       PIC S9(4) COMP.
88136A     05  CAT-DECLINE-ID-IND             PIC S9(4) COMP.
88136A     05  CAT-UW-DECISION-IND            PIC S9(4) COMP.
88136A     05  CAT-HOLD-DATE-IND              PIC S9(4) COMP.
88136A     05  CAT-APPL-SCRUB-DT-IND          PIC S9(4) COMP.
88136A     05  CAT-APPL-SCRUB-COMP-DT-IND     PIC S9(4) COMP.
      ******************************************************************CATMASTE
       01  CAT-MASTER-RECORD.                                           CATMASTE
           05  CAT-IDNTY-NUMBER           PIC X(8) VALUE SPACES.          CATMAS
           05  CAT-VERSION-QUAL         PIC X(1) VALUE SPACES.          CATMASTE
           05  CAT-DIST-CHANNEL         PIC X(1) VALUE SPACES.          CATMASTE
           05  CAT-FIELD-FORCE          PIC X(2) VALUE SPACES.          CATMASTE
           05  CAT-ALT-FF               PIC X(2) VALUE SPACES.          CATMASTE
           05  CAT-PRODUCT-CODE         PIC X(2) VALUE SPACES.          CATMASTE
           05  CAT-ACTIVE-IND           PIC X(1) VALUE SPACES.          CATMASTE
           05  CAT-UNDERWRITER          PIC X(3) VALUE SPACES.          CATMASTE
           05  CAT-RECEIVED-DATE        PIC S9(6)V COMP-3 VALUE +0.     CATMASTE
           05  CAT-PREMIUM-AMOUNT       PIC S9(5)V99 COMP-3 VALUE +0.   CATMASTE
           05  CAT-DECLINE-REASON       PIC X(3) VALUE SPACES.          CATMASTE
           05  CAT-DECLINED-DATE        PIC S9(6)V COMP-3 VALUE +0.     CATMASTE
           05  CAT-STATE-CODE           PIC X(2) VALUE SPACES.          CATMASTE
           05  CAT-MKT-SITE             PIC X(2) VALUE SPACES.          CATMASTE
           05  CAT-LICENSE-STATUS       PIC X(1) VALUE SPACES.          CATMASTE
           05  CAT-LICENSE-LID          PIC X(8) VALUE SPACES.          CATMASTE
           05  CAT-LICENSE-DATE         PIC S9(6)V COMP-3 VALUE +0.     CATMASTE
           05  CAT-UNDER-STATUS         PIC X(1) VALUE SPACES.          CATMASTE
           05  CAT-UNDER-LID            PIC X(8) VALUE SPACES.          CATMASTE
           05  CAT-UNDER-DATE           PIC S9(6)V COMP-3 VALUE +0.     CATMASTE
           05  CAT-VERIF-STATUS         PIC X(1) VALUE SPACES.          CATMASTE
           05  CAT-VERIF-LID            PIC X(8) VALUE SPACES.          CATMASTE
           05  CAT-VERIF-DATE           PIC S9(6)V COMP-3 VALUE +0.     CATMASTE
           05  CAT-CERTIF-STATUS        PIC X(1) VALUE SPACES.          CATMASTE
           05  CAT-CERTIF-LID           PIC X(8) VALUE SPACES.          CATMASTE
           05  CAT-CERTIF-DATE          PIC S9(6)V COMP-3 VALUE +0.     CATMASTE
           05  CAT-ISSUE-STATUS         PIC X(1) VALUE SPACES.          CATMASTE
           05  CAT-ISSUE-LID            PIC X(8) VALUE SPACES.          CATMASTE
           05  CAT-ISSUE-DATE           PIC S9(6)V COMP-3 VALUE +0.     CATMASTE
           05  CAT-ZIP-CODE             PIC X(5) VALUE SPACES.          CATMASTE
           05  CAT-UNDERWRTN-DATE       PIC S9(6)V COMP-3 VALUE +0.     CATMASTE
           05  CAT-COCARR-IDNTYNUM        PIC X(8) VALUE SPACES.          CATMAS
           05  CAT-UPDATE-DATE          PIC X(10) VALUE SPACES.         CATMASTE
           05  CAT-UPDATE-LID           PIC X(8) VALUE SPACES.          CATMASTE
           05  CAT-OVERRIDE-FLAG        PIC X(1) VALUE SPACES.          CATMASTE
           05  CAT-FPP-FLAG             PIC X(1) VALUE SPACES.          CATMASTE
           05  CAT-SALES-REP-LID        PIC X(8) VALUE SPACES.          CATMASTE
941056     05  CAT-CLOSEOUT-DATE        PIC X(10) VALUE SPACES.         CATMASTE
941056     05  RECEIVED-DATE            PIC X(10) VALUE SPACES.         CATMASTE
941056     05  DECLINED-DATE            PIC X(10) VALUE SPACES.         CATMASTE
941056     05  LICENSE-DATE             PIC X(10) VALUE SPACES.         CATMASTE
941056     05  UNDER-DATE               PIC X(10) VALUE SPACES.         CATMASTE
941056     05  VERIF-DATE               PIC X(10) VALUE SPACES.         CATMASTE
941056     05  CERTIF-DATE              PIC X(10) VALUE SPACES.         CATMASTE
941056     05  ISSUE-DATE               PIC X(10) VALUE SPACES.         CATMASTE
941056     05  UNDERWRTN-DATE           PIC X(10) VALUE SPACES.         CATMASTE
960913     05  CAT-UNDER-REC-DATE       PIC X(10) VALUE SPACES.         CATMASTE
R00723     05  TVU-STATUS               PIC X     VALUE SPACES.         CATMASTE
R00723     05  TVU-DATE                 PIC X(10) VALUE SPACES.         CATMASTE
R00723     05  ADV-COMM-STATUS          PIC X     VALUE SPACES.         CATMASTE
R00723     05  ADV-COMM-DATE            PIC X(10) VALUE SPACES.         CATMASTE
R01368     05  CAT-COM-PREPAY-FLG       PIC X(1)  VALUE SPACES.         CATMASTE
R01368     05  CAT-COM-PREPAY-AMT       PIC S9(11)V9(2) COMP-3 VALUE +0.CATMASTE
R01368     05  CAT-COM-PREPAY-DTE       PIC X(10) VALUE SPACES.         CATMASTE
88136A     05  CAT-APP-SIGNED-DATE      PIC X(10) VALUE SPACES.
88136A     05  CAT-SCAN-DATE            PIC X(10) VALUE SPACES.
88136A     05  CAT-UW-COMPLETE-DATE     PIC X(10) VALUE SPACES.
88136A     05  CAT-DECLINE-ID.
88136A         49  CAT-DECLINE-ID-LEN   PIC S9(4) COMP VALUE ZEROES.
88136A         49  CAT-DECLINE-ID-TEXT  PIC X(15) VALUE SPACES.
88136A     05  CAT-UW-DECISION.
88136A         49  CAT-UW-DECISION-LEN  PIC S9(4) COMP VALUE ZEROES.
88136A         49  CAT-UW-DECISION-TEXT PIC X(15) VALUE SPACES.
88136A     05  CAT-HOLD-DATE            PIC X(10) VALUE SPACES.
88136A     05  CAT-APPL-SCRUB-DT        PIC X(10) VALUE SPACES.
88136A     05  CAT-APPL-SCRUB-COMP-DT   PIC X(10) VALUE SPACES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 62      *
      ******************************************************************
