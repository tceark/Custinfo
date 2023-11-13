      ******************************************************************EMPLOYEE
           EXEC SQL DECLARE EMPLOYEE TABLE                              EMPLOYEE
           ( CASENAME#IDNTITY            CHAR(8) NOT NULL,                EMPLOY
             UNIQUE-NUM                  CHAR(6) NOT NULL,                EMPLOY
             EMP_NUM                   DECIMAL(5, 0) NOT NULL,          EMPLOYEE
             LAST_NAME                 CHAR(20) NOT NULL,               EMPLOYEE
             FIRST_NAME                CHAR(15) NOT NULL,               EMPLOYEE
             MIDDLE_NAME               CHAR(15) NOT NULL,               EMPLOYEE
             ACTIVE_IND                CHAR(1) NOT NULL,                EMPLOYEE
             SEX_CODE                  CHAR(1) NOT NULL,                EMPLOYEE
             SSN                       DECIMAL(9, 0) NOT NULL,          EMPLOYEE
             CARRIER                   CHAR(2) NOT NULL,                EMPLOYEE
             SITE_CODE                 CHAR(2) NOT NULL,                EMPLOYEE
             DEPENDENCY_CODE           CHAR(1) NOT NULL,                EMPLOYEE
             MEDICARE_IND              CHAR(1) NOT NULL,                EMPLOYEE
             OPTION_CODE               CHAR(1) NOT NULL,                EMPLOYEE
             POSITION_CODE             CHAR(1) NOT NULL,                EMPLOYEE
             CASE_CONTACT              CHAR(1) NOT NULL,                EMPLOYEE
             SMOKER_QUESTION           CHAR(1) NOT NULL,                EMPLOYEE
             IMPAIRMENT_RIDER          CHAR(1) NOT NULL,                EMPLOYEE
             DATE_OF_BIRTH             DATE NOT NULL,                   EMPLOYEE
             SPOUSE_DOB                DATE,                            EMPLOYEE
             LIFE_VOLUME               DECIMAL(9, 0) NOT NULL,          EMPLOYEE
             OPTIONAL_LIFE             DECIMAL(9, 0) NOT NULL,          EMPLOYEE
             ADANDD_VOLUME             DECIMAL(9, 0) NOT NULL,          EMPLOYEE
             ANNUAL_SALARY             DECIMAL(9, 0) NOT NULL,          EMPLOYEE
             STD_BENEFIT               DECIMAL(5, 0) NOT NULL,          EMPLOYEE
             PREMIUM_FACTOR            DECIMAL(2, 0) NOT NULL,          EMPLOYEE
             TOTAL_PREMIUM             DECIMAL(7, 2) NOT NULL,          EMPLOYEE
             EFFECTIVE_DATE            DATE NOT NULL,                   EMPLOYEE
             DATE_ON_OFF_SYSTEM        DATE NOT NULL,                   EMPLOYEE
             REENTRY_DATE              DATE,                            EMPLOYEE
             STAT_DATE                 DATE NOT NULL,                   EMPLOYEE
             CERT_IND                  CHAR(1) NOT NULL,                EMPLOYEE
             CERT_DATE                 DATE,                            EMPLOYEE
             CERT_SENT_DATE            DATE,                            EMPLOYEE
             TERM_CODE                 CHAR(1) NOT NULL,                EMPLOYEE
             TERM_DATE                 DATE,                            EMPLOYEE
             TERM_ORIGIN               CHAR(1) NOT NULL,                EMPLOYEE
             REVERSAL_CODE             CHAR(1) NOT NULL,                EMPLOYEE
             REINSTATE_DATE            DATE,                            EMPLOYEE
             REINSTATE_COUNT           DECIMAL(1, 0) NOT NULL,          EMPLOYEE
             PCS_COVERAGE_DATE         DATE,                            EMPLOYEE
             PCS_REQUEST               CHAR(1) NOT NULL,                EMPLOYEE
             PCS_DATE                  DATE,                            EMPLOYEE
             PCS_TYPE                  DECIMAL(3, 0) NOT NULL,          EMPLOYEE
             CW_CARD_CODE              CHAR(1) NOT NULL,                EMPLOYEE
             HCARD_FLAG                CHAR(1) NOT NULL,                EMPLOYEE
             HCARD_EFF_DATE            DATE,                            EMPLOYEE
             HCARD_TERM_DATE           DATE,                            EMPLOYEE
             PREV_COV_CODE             CHAR(2) NOT NULL,                EMPLOYEE
             PREV_COV_DEP              CHAR(1) NOT NULL,                EMPLOYEE
             PREV_CARR_CODE            CHAR(2) NOT NULL,                EMPLOYEE
             RATE_CHANGE_FLAG          CHAR(1) NOT NULL,                EMPLOYEE
             AGE_CHANGE_FLAG           CHAR(1) NOT NULL,                EMPLOYEE
             AGE_60_FLAG               CHAR(1) NOT NULL,                EMPLOYEE
             AGE_65_FLAG               CHAR(1) NOT NULL,                EMPLOYEE
             AGE_70_FLAG               CHAR(1) NOT NULL,                EMPLOYEE
             AGE_75_FLAG               CHAR(1) NOT NULL,                EMPLOYEE
             X_OR_LRGP_FLAG            CHAR(1) NOT NULL,                EMPLOYEE
             DATA_ERROR_FLAG           CHAR(1) NOT NULL,                EMPLOYEE
             LIFE_CHANGE_FLAG          CHAR(1) NOT NULL,                EMPLOYEE
             HEALTH_CHANGE_FLAG        CHAR(1) NOT NULL,                EMPLOYEE
             STD_CHANGE_FLAG           CHAR(1) NOT NULL,                EMPLOYEE
             DENT_CHANGE_FLAG          CHAR(1) NOT NULL,                EMPLOYEE
             MAT_CHANGE_FLAG           CHAR(1) NOT NULL,                EMPLOYEE
             PCS_CHANGE_FLAG           CHAR(1) NOT NULL,                EMPLOYEE
             LTD_CHANGE_FLAG           CHAR(1) NOT NULL,                EMPLOYEE
             DEPL_CHANGE_FLAG          CHAR(1) NOT NULL,                EMPLOYEE
             OTHER_CHANGE_FLAG         CHAR(1) NOT NULL,                EMPLOYEE
             CD11_CHANGE_FLAG          CHAR(1) NOT NULL,                EMPLOYEE
             CD12_CHANGE_FLAG          CHAR(1) NOT NULL,                EMPLOYEE
             PREMUPD_ERRCODE           CHAR(2) NOT NULL,                EMPLOYEE
             UNDERWRITER               CHAR(3) NOT NULL,                EMPLOYEE
             ADDED_DATE                TIMESTAMP NOT NULL,              EMPLOYEE
             ADDED_LOGON               CHAR(8) NOT NULL,                EMPLOYEE
             CHANGE_DATE               TIMESTAMP,                       EMPLOYEE
             CHANGE_LOGON              CHAR(8) NOT NULL,                EMPLOYEE
             LTD_CLASS                 CHAR(1) NOT NULL,                EMPLOYEE
             LTD_OCCUPATION_CD         CHAR(2) NOT NULL,                EMPLOYEE
             RATE_AREA                 CHAR(2) NOT NULL,                EMPLOYEE
             ZIP_CODE                  DECIMAL(5, 0) NOT NULL,          EMPLOYEE
             NUM_OF_DEPENDENTS         DECIMAL(3, 0) NOT NULL,          EMPLOYEE
             STATE                     CHAR(2) NOT NULL,                EMPLOYEE
             RATING_ZIP                CHAR(5) NOT NULL,                EMPLOYEE
             ZIP_PLUS_4                CHAR(4) NOT NULL,                EMPLOYEE
             RATE_LEVEL                CHAR(2) NOT NULL,                EMPLOYEE
             NEXT_LEVEL                CHAR(2) NOT NULL,                EMPLOYEE
             AREA_CHANGE               CHAR(2) NOT NULL,                EMPLOYEE
             PROVIDER_NUM              CHAR(15),                        EMPLOYEE
             INDMNTY_PREPD_IND         CHAR(1) NOT NULL,                EMPLOYEE
             CEDED_IND                 CHAR(1) NOT NULL,                EMPLOYEE
             PRODUCT_NUMBER            CHAR(4) NOT NULL,                EMPLOYEE
             DECLARE_IND               CHAR(1) NOT NULL,                EMPLOYEE
             HIRE_DATE                 DATE,                            EMPLOYEE
             POSITION_TYPE             CHAR(1) NOT NULL,                EMPLOYEE
             PREVIOUSLY_UNINSRD        CHAR(1) NOT NULL,                EMPLOYEE
             CONT_EFF_DATE             DATE,                            EMPLOYEE
             MEDICAL_RECORD_NUM        CHAR(12) NOT NULL WITH DEFAULT,  EMPLOYEE
             KAISR_MEDICARE_IND        CHAR(2)  NOT NULL WITH DEFAULT,  EMPLOYEE
             DIVISION_NUM              CHAR(3)  NOT NULL WITH DEFAULT,  EMPLOYEE
             PRODUCT_EFF_DATE          DATE,                            EMPLOYEE
             OPT_OUT                   CHAR(1)  NOT NULL WITH DEFAULT,  EMPLOYEE
R03022       MEDICARE_IND_DATE         DATE,                            EMPLOYEE
R03022       MEDICARE_EFF_DATE         DATE,                            EMPLOYEE
R03022       MEDICARE_TERM_DATE        DATE,                            EMPLOYEE
R03384       PREFIX                    CHAR(4),                         EMPLOYEE
R04045       COURT_ORDERED             CHAR(1)  NOT NULL WITH DEFAULT,  EMPLOYEE
R04164       MARKETING_IND             CHAR(1)  NOT NULL WITH DEFAULT,  EMPLOYEE
R07749       MEDICARE_REASON           VARCHAR(15) NOT NULL,
11269        CARRIER_ENROLLEE_ID       VARCHAR(50) NOT NULL,
11269        EXCHANGE_ENROLLEE_ID      VARCHAR(50) NOT NULL,
317343       LAST_SMOKED_DATE          DATE        NOT NULL,
R12549       INDIV_TAX_PAYER_ID        CHAR(9)     NOT NULL
           ) END-EXEC.                                                  EMPLOYEE
                                                                        EMPLOYEE
      ******************************************************************EMPLOYEE
      * COBOL DECLARATION FOR TABLE EMPLOYEE                           *EMPLOYEE
      ******************************************************************EMPLOYEE
       01  EMPLOYEE-REC.                                                EMPLOYEE
           10  EMPLOYEE-1           PIC X(8).                           EMPLOYEE
           10  EMPLOYEE-2           PIC X(6).                           EMPLOYEE
           10  EMPLOYEE-3           PIC S99999V USAGE COMP-3.           EMPLOYEE
           10  EMPLOYEE-4           PIC X(20).                          EMPLOYEE
           10  EMPLOYEE-5           PIC X(15).                          EMPLOYEE
           10  EMPLOYEE-6           PIC X(15).                          EMPLOYEE
           10  EMPLOYEE-7           PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-8           PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-9           PIC S999999999V USAGE COMP-3.       EMPLOYEE
           10  EMPLOYEE-10          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-11          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-12          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-13          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-14          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-15          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-16          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-17          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-18          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-19          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-20          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-21          PIC S999999999V USAGE COMP-3.       EMPLOYEE
           10  EMPLOYEE-22          PIC S999999999V USAGE COMP-3.       EMPLOYEE
           10  EMPLOYEE-23          PIC S999999999V USAGE COMP-3.       EMPLOYEE
           10  EMPLOYEE-24          PIC S999999999V USAGE COMP-3.       EMPLOYEE
           10  EMPLOYEE-25          PIC S99999V USAGE COMP-3.           EMPLOYEE
           10  EMPLOYEE-26          PIC S99V USAGE COMP-3.              EMPLOYEE
           10  EMPLOYEE-27          PIC S99999V99 USAGE COMP-3.         EMPLOYEE
           10  EMPLOYEE-28          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-29          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-30          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-31          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-32          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-33          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-34          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-35          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-36          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-37          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-38          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-39          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-40          PIC S9V USAGE COMP-3.               EMPLOYEE
           10  EMPLOYEE-41          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-42          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-43          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-44          PIC S999V USAGE COMP-3.             EMPLOYEE
           10  EMPLOYEE-45          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-46          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-47          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-48          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-49          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-50          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-51          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-52          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-53          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-54          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-55          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-56          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-57          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-58          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-59          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-60          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-61          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-62          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-63          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-64          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-65          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-66          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-67          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-68          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-69          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-70          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-71          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-72          PIC X(3).                           EMPLOYEE
           10  EMPLOYEE-73          PIC X(26).                          EMPLOYEE
           10  EMPLOYEE-74          PIC X(8).                           EMPLOYEE
           10  EMPLOYEE-75          PIC X(26).                          EMPLOYEE
           10  EMPLOYEE-76          PIC X(8).                           EMPLOYEE
           10  EMPLOYEE-77          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-78          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-79          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-80          PIC S99999V USAGE COMP-3.           EMPLOYEE
           10  EMPLOYEE-81          PIC S999V USAGE COMP-3.             EMPLOYEE
           10  EMPLOYEE-82          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-83          PIC X(5).                           EMPLOYEE
           10  EMPLOYEE-84          PIC X(4).                           EMPLOYEE
           10  EMPLOYEE-85          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-86          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-87          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-88          PIC X(15).                          EMPLOYEE
           10  EMPLOYEE-89          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-90          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-91          PIC X(4).                           EMPLOYEE
           10  EMPLOYEE-92          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-93          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-94          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-95          PIC X(1).                           EMPLOYEE
           10  EMPLOYEE-96          PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-97          PIC X(12).                          EMPLOYEE
           10  EMPLOYEE-98          PIC X(2).                           EMPLOYEE
           10  EMPLOYEE-99          PIC X(3).                           EMPLOYEE
           10  EMPLOYEE-100         PIC X(10).                          EMPLOYEE
           10  EMPLOYEE-101         PIC X(1).                           EMPLOYEE
R03022     10  EMPLOYEE-102         PIC X(10).                          EMPLOYEE
R03022     10  EMPLOYEE-103         PIC X(10).                          EMPLOYEE
R03022     10  EMPLOYEE-104         PIC X(10).                          EMPLOYEE
R03384     10  EMPLOYEE-105         PIC X(4).                           EMPLOYEE
R04045     10  EMPLOYEE-106         PIC X(1).                           EMPLOYEE
R04164     10  EMPLOYEE-107         PIC X(1).                           EMPLOYEE
R07749     10  EMPLOYEE-108         PIC X(15).
11269      10  EMPLOYEE-109         PIC X(50).
11269      10  EMPLOYEE-110         PIC X(50).
317343     10  EMPLOYEE-111         PIC X(10).
R12549     10  EMPLOYEE-112         PIC X(9).
                                                                        EMPLOYEE
      ******************************************************************EMPLOYEE
      * INDICATOR FIELDS FOR TABLE EMPLOYEE                            *EMPLOYEE
      ******************************************************************EMPLOYEE
       01  EMPLOYEE-INDICATOR-ARRAY.                                    EMPLOYEE
12549      05  EMPLOYEE-INDICATORS OCCURS 112 TIMES PIC S9(4) COMP.     EMPLOYEE
       01  FILLER REDEFINES EMPLOYEE-INDICATOR-ARRAY.                   EMPLOYEE
           05  EE1-CASENAME-IDNTY-IND          PIC S9(4) COMP.            EMPLOY
           05  EE1-UNIQUE-NUM-IND              PIC S9(4) COMP.            EMPLOY
           05  EE1-EMP-NUM-IND               PIC S9(4) COMP.            EMPLOYEE
           05  EE1-LAST-NAME-IND             PIC S9(4) COMP.            EMPLOYEE
           05  EE1-FIRST-NAME-IND            PIC S9(4) COMP.            EMPLOYEE
           05  EE1-MIDDLE-NAME-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-ACTIVE-IND-IND            PIC S9(4) COMP.            EMPLOYEE
           05  EE1-SEX-CODE-IND              PIC S9(4) COMP.            EMPLOYEE
           05  EE1-SSN-IND                   PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CARRIER-IND               PIC S9(4) COMP.            EMPLOYEE
           05  EE1-SITE-CODE-IND             PIC S9(4) COMP.            EMPLOYEE
           05  EE1-DEPENDENCY-CODE-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-MEDICARE-IND-IND          PIC S9(4) COMP.            EMPLOYEE
           05  EE1-OPTION-CODE-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-POSITION-CODE-IND         PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CASE-CONTACT-IND          PIC S9(4) COMP.            EMPLOYEE
           05  EE1-SMOKER-QUESTION-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-IMPAIRMENT-RIDER-IND      PIC S9(4) COMP.            EMPLOYEE
           05  EE1-DATE-OF-BIRTH-IND         PIC S9(4) COMP.            EMPLOYEE
           05  EE1-SPOUSE-DOB-IND            PIC S9(4) COMP.            EMPLOYEE
           05  EE1-LIFE-VOLUME-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-OPTIONAL-LIFE-IND         PIC S9(4) COMP.            EMPLOYEE
           05  EE1-ADANDD-VOLUME-IND         PIC S9(4) COMP.            EMPLOYEE
           05  EE1-ANNUAL-SALARY-IND         PIC S9(4) COMP.            EMPLOYEE
           05  EE1-STD-BENEFIT-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PREMIUM-FACTOR-IND        PIC S9(4) COMP.            EMPLOYEE
           05  EE1-TOTAL-PREMIUM-IND         PIC S9(4) COMP.            EMPLOYEE
           05  EE1-EFFECTIVE-DATE-IND        PIC S9(4) COMP.            EMPLOYEE
           05  EE1-DATE-ON-OFF-SYSTEM-IND    PIC S9(4) COMP.            EMPLOYEE
           05  EE1-REENTRY-DATE-IND          PIC S9(4) COMP.            EMPLOYEE
           05  EE1-STAT-DATE-IND             PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CERT-IND-IND              PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CERT-DATE-IND             PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CERT-SENT-DATE-IND        PIC S9(4) COMP.            EMPLOYEE
           05  EE1-TERM-CODE-IND             PIC S9(4) COMP.            EMPLOYEE
           05  EE1-TERM-DATE-IND             PIC S9(4) COMP.            EMPLOYEE
           05  EE1-TERM-ORIGIN-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-REVERSAL-CODE-IND         PIC S9(4) COMP.            EMPLOYEE
           05  EE1-REINSTATE-DATE-IND        PIC S9(4) COMP.            EMPLOYEE
           05  EE1-REINSTATE-COUNT-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PCS-COVERAGE-DATE-IND     PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PCS-REQUEST-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PCS-DATE-IND              PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PCS-TYPE-IND              PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CW-CARD-CODE-IND          PIC S9(4) COMP.            EMPLOYEE
           05  EE1-HCARD-FLAG-IND            PIC S9(4) COMP.            EMPLOYEE
           05  EE1-HCARD-EFF-DATE-IND        PIC S9(4) COMP.            EMPLOYEE
           05  EE1-HCARD-TERM-DATE-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PREV-COV-CODE-IND         PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PREV-COV-DEP-IND          PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PREV-CARR-CODE-IND        PIC S9(4) COMP.            EMPLOYEE
           05  EE1-RATE-CHANGE-FLAG-IND      PIC S9(4) COMP.            EMPLOYEE
           05  EE1-AGE-CHANGE-FLAG-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-AGE-60-FLAG-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-AGE-65-FLAG-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-AGE-70-FLAG-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-AGE-75-FLAG-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-X-OR-LRGP-FLAG-IND        PIC S9(4) COMP.            EMPLOYEE
           05  EE1-DATA-ERROR-FLAG-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-LIFE-CHANGE-FLAG-IND      PIC S9(4) COMP.            EMPLOYEE
           05  EE1-HEALTH-CHANGE-FLAG-IND    PIC S9(4) COMP.            EMPLOYEE
           05  EE1-STD-CHANGE-FLAG-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-DENT-CHANGE-FLAG-IND      PIC S9(4) COMP.            EMPLOYEE
           05  EE1-MAT-CHANGE-FLAG-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PCS-CHANGE-FLAG-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-LTD-CHANGE-FLAG-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-DEPL-CHANGE-FLAG-IND      PIC S9(4) COMP.            EMPLOYEE
           05  EE1-OTHER-CHANGE-FLAG-IND     PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CD11-CHANGE-FLAG-IND      PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CD12-CHANGE-FLAG-IND      PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PREMUPD-ERRCODE-IND       PIC S9(4) COMP.            EMPLOYEE
           05  EE1-UNDERWRITER-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-ADDED-DATE-IND            PIC S9(4) COMP.            EMPLOYEE
           05  EE1-ADDED-LOGON-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CHANGE-DATE-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CHANGE-LOGON-IND          PIC S9(4) COMP.            EMPLOYEE
           05  EE1-LTD-CLASS-IND             PIC S9(4) COMP.            EMPLOYEE
           05  EE1-LTD-OCCUPATION-CD-IND     PIC S9(4) COMP.            EMPLOYEE
           05  EE1-RATE-AREA-IND             PIC S9(4) COMP.            EMPLOYEE
           05  EE1-ZIP-CODE-IND              PIC S9(4) COMP.            EMPLOYEE
           05  EE1-NUM-OF-DEPENDENTS-IND     PIC S9(4) COMP.            EMPLOYEE
           05  EE1-STATE-IND                 PIC S9(4) COMP.            EMPLOYEE
           05  EE1-RATING-ZIP-IND            PIC S9(4) COMP.            EMPLOYEE
           05  EE1-ZIP-PLUS-4-IND            PIC S9(4) COMP.            EMPLOYEE
           05  EE1-RATE-LEVEL-IND            PIC S9(4) COMP.            EMPLOYEE
           05  EE1-NEXT-LEVEL-IND            PIC S9(4) COMP.            EMPLOYEE
           05  EE1-AREA-CHANGE-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PROVIDER-IND              PIC S9(4) COMP.            EMPLOYEE
           05  EE1-INDMNTY-PREPD-IND-IND     PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CEDED-IND-IND             PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PRODUCT-NUMBER-IND        PIC S9(4) COMP.            EMPLOYEE
           05  EE1-DECLARE-IND-IND           PIC S9(4) COMP.            EMPLOYEE
           05  EE1-HIRE-DATE-IND             PIC S9(4) COMP.            EMPLOYEE
           05  EE1-POSITION-TYPE-IND         PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PREVIOUSLY-UNINSRD-IND    PIC S9(4) COMP.            EMPLOYEE
           05  EE1-CONT-EFF-DATE-IND         PIC S9(4) COMP.            EMPLOYEE
           05  EE1-MEDICAL-RECORD-NUM-IND    PIC S9(4) COMP.            EMPLOYEE
           05  EE1-KAISR-MEDICARE-IND-IND    PIC S9(4) COMP.            EMPLOYEE
           05  EE1-DIVISION-NUM-IND          PIC S9(4) COMP.            EMPLOYEE
           05  EE1-PRODUCT-EFF-DATE-IND      PIC S9(4) COMP.            EMPLOYEE
           05  EE1-OPT-OUT-IND               PIC S9(4) COMP.            EMPLOYEE
R03022     05  EE1-MEDICARE-IND-DATE-IND     PIC S9(4) COMP.            EMPLOYEE
R03022     05  EE1-MEDICARE-EFF-DATE-IND     PIC S9(4) COMP.            EMPLOYEE
R03022     05  EE1-MEDICARE-TERM-DATE-IND    PIC S9(4) COMP.            EMPLOYEE
R03384     05  EE1-PREFIX-IND                PIC S9(4) COMP.            EMPLOYEE
R04045     05  EE1-COURT-ORDERED-IND         PIC S9(4) COMP.            EMPLOYEE
R04164     05  EE1-MARKETING-IND-IND         PIC S9(4) COMP.            EMPLOYEE
R07749     05  EE1-MEDICARE-REASON-IND       PIC S9(4) COMP.
11269      05  EE1-CARRIER-ENROLLEE-ID-IND   PIC S9(4) COMP.
11269      05  EE1-EXCHANGE-ENROLLEE-ID-IND  PIC S9(4) COMP.
317343     05  EE1-LAST-SMOKED-DATE-IND      PIC S9(4) COMP.
R12549     05  EE1-INDIV-TAX-PAYER-ID-IND    PIC S9(4) COMP.            DEPENDEN
      ******************************************************************EMPLOYEE
      * COBOL RECORD LAYOUT FOR TABLE EMPLOYEE                         *EMPLOYEE
      ******************************************************************EMPLOYEE
IDENT  01  EMPLOYEE-RECORD.                                             EMPLOYEE
00010      05  EE1-CASENAME-IDNTY        PIC X(8)     VALUE SPACES.       EMPLOY
E
00020      05  EE1-UNIQUE-NUM            PIC X(6)     VALUE SPACES.       EMPLOY
E
00030      05  EE1-EMP-NUM             PIC S9(5)    VALUE ZEROS COMP-3. EMPLOYEE
00040      05  EE1-LAST-NAME           PIC X(20)    VALUE SPACES.       EMPLOYEE
00050      05  EE1-FIRST-NAME          PIC X(15)    VALUE SPACES.       EMPLOYEE
00060      05  EE1-MIDDLE-NAME         PIC X(15)    VALUE SPACES.       EMPLOYEE
00070      05  EE1-ACTIVE-IND          PIC X(1)     VALUE SPACES.       EMPLOYEE
00080      05  EE1-SEX-CODE            PIC X(1)     VALUE SPACES.       EMPLOYEE
00090      05  EE1-SSN                 PIC S9(9)    VALUE ZEROS COMP-3. EMPLOYEE
00100      05  EE1-CARRIER             PIC X(2)     VALUE SPACES.       EMPLOYEE
00110      05  EE1-SITE-CODE           PIC X(2)     VALUE SPACES.       EMPLOYEE
00120      05  EE1-DEPENDENCY-CODE     PIC X(1)     VALUE SPACES.       EMPLOYEE
00130      05  EE1-MEDICARE-IND        PIC X(1)     VALUE SPACES.       EMPLOYEE
00140      05  EE1-OPTION-CODE         PIC X(1)     VALUE SPACES.       EMPLOYEE
00150      05  EE1-POSITION-CODE       PIC X(1)     VALUE SPACES.       EMPLOYEE
00160      05  EE1-CASE-CONTACT        PIC X(1)     VALUE SPACES.       EMPLOYEE
00170      05  EE1-SMOKER-QUESTION     PIC X(1)     VALUE SPACES.       EMPLOYEE
00180      05  EE1-IMPAIRMENT-RIDER    PIC X(1)     VALUE SPACES.       EMPLOYEE
00190      05  EE1-DATE-OF-BIRTH       PIC X(10)    VALUE SPACES.       EMPLOYEE
00200      05  EE1-SPOUSE-DOB          PIC X(10)    VALUE SPACES.       EMPLOYEE
00210      05  EE1-LIFE-VOLUME         PIC S9(9)    VALUE ZEROS COMP-3. EMPLOYEE
00220      05  EE1-OPTIONAL-LIFE       PIC S9(9)    VALUE ZEROS COMP-3. EMPLOYEE
00230      05  EE1-ADANDD-VOLUME       PIC S9(9)    VALUE ZEROS COMP-3. EMPLOYEE
00240      05  EE1-ANNUAL-SALARY       PIC S9(9)    VALUE ZEROS COMP-3. EMPLOYEE
00250      05  EE1-STD-BENEFIT         PIC S9(5)    VALUE ZEROS COMP-3. EMPLOYEE
00270      05  EE1-PREMIUM-FACTOR      PIC S9(2)    VALUE ZEROS COMP-3. EMPLOYEE
00280      05  EE1-TOTAL-PREMIUM       PIC S9(5)V99 VALUE ZEROS COMP-3. EMPLOYEE
00290      05  EE1-EFFECTIVE-DATE      PIC X(10)    VALUE SPACES.       EMPLOYEE
00300      05  EE1-DATE-ON-OFF-SYSTEM  PIC X(10)    VALUE SPACES.       EMPLOYEE
00310      05  EE1-REENTRY-DATE        PIC X(10)    VALUE SPACES.       EMPLOYEE
00320      05  EE1-STAT-DATE           PIC X(10)    VALUE SPACES.       EMPLOYEE
00330      05  EE1-CERT-IND            PIC X(1)     VALUE SPACES.       EMPLOYEE
00340      05  EE1-CERT-DATE           PIC X(10)    VALUE SPACES.       EMPLOYEE
00350      05  EE1-CERT-SENT-DATE      PIC X(10)    VALUE SPACES.       EMPLOYEE
00360      05  EE1-TERM-CODE           PIC X(1)     VALUE SPACES.       EMPLOYEE
00370      05  EE1-TERM-DATE           PIC X(10)    VALUE SPACES.       EMPLOYEE
00380      05  EE1-TERM-ORIGIN         PIC X(1)     VALUE SPACES.       EMPLOYEE
00390      05  EE1-REVERSAL-CODE       PIC X(1)     VALUE SPACES.       EMPLOYEE
00400      05  EE1-REINSTATE-DATE      PIC X(10)    VALUE SPACES.       EMPLOYEE
00410      05  EE1-REINSTATE-COUNT     PIC S9(1)    VALUE ZEROS COMP-3. EMPLOYEE
00420      05  EE1-PCS-COVERAGE-DATE   PIC X(10)    VALUE SPACES.       EMPLOYEE
00430      05  EE1-PCS-REQUEST         PIC X(1)     VALUE SPACES.       EMPLOYEE
00440      05  EE1-PCS-DATE            PIC X(10)    VALUE SPACES.       EMPLOYEE
00450      05  EE1-PCS-TYPE            PIC S9(3)    VALUE ZEROS COMP-3. EMPLOYEE
00460      05  EE1-CW-CARD-CODE        PIC X(1)     VALUE SPACES.       EMPLOYEE
00470      05  EE1-HCARD-FLAG          PIC X(1)     VALUE SPACES.       EMPLOYEE
00480      05  EE1-HCARD-EFF-DATE      PIC X(10)    VALUE SPACES.       EMPLOYEE
00490      05  EE1-HCARD-TERM-DATE     PIC X(10)    VALUE SPACES.       EMPLOYEE
00500      05  EE1-PREV-COV-CODE       PIC X(2)     VALUE SPACES.       EMPLOYEE
00510      05  EE1-PREV-COV-DEP        PIC X(1)     VALUE SPACES.       EMPLOYEE
00520      05  EE1-PREV-CARR-CODE      PIC X(2)     VALUE SPACES.       EMPLOYEE
00530      05  EE1-RATE-CHANGE-FLAG    PIC X(1)     VALUE SPACES.       EMPLOYEE
00540      05  EE1-AGE-CHANGE-FLAG     PIC X(1)     VALUE SPACES.       EMPLOYEE
00550      05  EE1-AGE-60-FLAG         PIC X(1)     VALUE SPACES.       EMPLOYEE
00560      05  EE1-AGE-65-FLAG         PIC X(1)     VALUE SPACES.       EMPLOYEE
00570      05  EE1-AGE-70-FLAG         PIC X(1)     VALUE SPACES.       EMPLOYEE
00580      05  EE1-AGE-75-FLAG         PIC X(1)     VALUE SPACES.       EMPLOYEE
00590      05  EE1-X-OR-LRGP-FLAG      PIC X(1)     VALUE SPACES.       EMPLOYEE
00600      05  EE1-DATA-ERROR-FLAG     PIC X(1)     VALUE SPACES.       EMPLOYEE
00610      05  EE1-LIFE-CHANGE-FLAG    PIC X(1)     VALUE SPACES.       EMPLOYEE
00620      05  EE1-HEALTH-CHANGE-FLAG  PIC X(1)     VALUE SPACES.       EMPLOYEE
00630      05  EE1-STD-CHANGE-FLAG     PIC X(1)     VALUE SPACES.       EMPLOYEE
00640      05  EE1-DENT-CHANGE-FLAG    PIC X(1)     VALUE SPACES.       EMPLOYEE
00650      05  EE1-MAT-CHANGE-FLAG     PIC X(1)     VALUE SPACES.       EMPLOYEE
00660      05  EE1-PCS-CHANGE-FLAG     PIC X(1)     VALUE SPACES.       EMPLOYEE
00670      05  EE1-LTD-CHANGE-FLAG     PIC X(1)     VALUE SPACES.       EMPLOYEE
00680      05  EE1-DEPL-CHANGE-FLAG    PIC X(1)     VALUE SPACES.       EMPLOYEE
00690      05  EE1-OTHER-CHANGE-FLAG   PIC X(1)     VALUE SPACES.       EMPLOYEE
00700      05  EE1-CD11-CHANGE-FLAG    PIC X(1)     VALUE SPACES.       EMPLOYEE
00710      05  EE1-CD12-CHANGE-FLAG    PIC X(1)     VALUE SPACES.       EMPLOYEE
00720      05  EE1-PREMUPD-ERRCODE     PIC X(2)     VALUE SPACES.       EMPLOYEE
00730      05  EE1-UNDERWRITER         PIC X(3)     VALUE SPACES.       EMPLOYEE
00740      05  EE1-ADDED-DATE          PIC X(26)    VALUE SPACES.       EMPLOYEE
00750      05  EE1-ADDED-LOGON         PIC X(8)     VALUE SPACES.       EMPLOYEE
00760      05  EE1-CHANGE-DATE         PIC X(26)    VALUE SPACES.       EMPLOYEE
00770      05  EE1-CHANGE-LOGON        PIC X(8)     VALUE SPACES.       EMPLOYEE
           05  EE1-LTD-CLASS           PIC X(1)     VALUE SPACES.       EMPLOYEE
           05  EE1-LTD-OCCUPATION-CD   PIC X(2)     VALUE SPACES.       EMPLOYEE
           05  EE1-RATE-AREA           PIC X(2)     VALUE SPACES.       EMPLOYEE
           05  EE1-ZIP-CODE            PIC S9(5)    VALUE ZEROS COMP-3. EMPLOYEE
           05  EE1-NUM-OF-DEPENDENTS   PIC S9(3)    VALUE ZEROS COMP-3. EMPLOYEE
           05  EE1-STATE               PIC X(2)     VALUE SPACES.       EMPLOYEE
           05  EE1-RATING-ZIP          PIC X(5)     VALUE SPACES.       EMPLOYEE
           05  EE1-ZIP-PLUS-4          PIC X(4)     VALUE SPACES.       EMPLOYEE
           05  EE1-RATE-LEVEL          PIC X(2)     VALUE SPACES.       EMPLOYEE
           05  EE1-NEXT-LEVEL          PIC X(2)     VALUE SPACES.       EMPLOYEE
           05  EE1-AREA-CHANGE         PIC X(2)     VALUE SPACES.       EMPLOYEE
           05  EE1-PROVIDER-NUM        PIC X(15)    VALUE SPACES.       EMPLOYEE
           05  EE1-INDMNTY-PREPD-IND   PIC X(1)     VALUE SPACES.       EMPLOYEE
           05  EE1-CEDED-IND           PIC X(1)     VALUE SPACES.       EMPLOYEE
           05  EE1-PRODUCT-NUMBER      PIC X(4)     VALUE SPACES.       EMPLOYEE
           05  EE1-DECLARE-IND         PIC X(1)     VALUE SPACES.       EMPLOYEE
           05  EE1-HIRE-DATE           PIC X(10)    VALUE SPACES.       EMPLOYEE
           05  EE1-POSITION-TYPE       PIC X(1)     VALUE SPACES.       EMPLOYEE
           05  EE1-PREVIOUSLY-UNINSRD  PIC X(1)     VALUE SPACES.       EMPLOYEE
           05  EE1-CONT-EFF-DATE       PIC X(10)    VALUE SPACES.       EMPLOYEE
           05  EE1-MEDICAL-RECORD-NUM  PIC X(12)    VALUE SPACES.       EMPLOYEE
           05  EE1-KAISR-MEDICARE-IND  PIC X(2)     VALUE SPACES.       EMPLOYEE
           05  EE1-DIVISION-NUM        PIC X(3)     VALUE SPACES.       EMPLOYEE
           05  EE1-PRODUCT-EFF-DATE    PIC X(10)    VALUE SPACES.       EMPLOYEE
           05  EE1-OPT-OUT             PIC X(1)     VALUE SPACES.       EMPLOYEE
R03022     05  EE1-MEDICARE-IND-DATE   PIC X(10)    VALUE SPACES.       EMPLOYEE
R03022     05  EE1-MEDICARE-EFF-DATE   PIC X(10)    VALUE SPACES.       EMPLOYEE
R03022     05  EE1-MEDICARE-TERM-DATE  PIC X(10)    VALUE SPACES.       EMPLOYEE
R03384     05  EE1-PREFIX              PIC X(4)     VALUE SPACES.       EMPLOYEE
R04045     05  EE1-COURT-ORDERED       PIC X(1)     VALUE SPACES.       EMPLOYEE
R04164     05  EE1-MARKETING-IND       PIC X(1)     VALUE SPACES.       EMPLOYEE
R07749     05  EE1-MEDICARE-REASON     PIC X(15)    VALUE SPACES.
           05  EE1-CARRIER-ENROLLEE-ID.
              49 EE1-CARRIER-ENROLLEE-ID-LEN
                 PIC S9(4) USAGE COMP.
              49 EE1-CARRIER-ENROLLEE-ID-TEXT
                 PIC X(50).
           05  EE1-EXCHANGE-ENROLLEE-ID.
              49 EE1-EXCHANGE-ENROLLEE-ID-LEN
                 PIC S9(4) USAGE COMP.
              49 EE1-EXCHANGE-ENROLLEE-ID-TEXT
                 PIC X(50).
317343     05  EE1-LAST-SMOKED-DATE    PIC X(10)    VALUE SPACES.       EMPLOYEE
R12549     05  EE1-INDIV-TAX-PAYER-ID  PIC X(9)     VALUE SPACES.       DEPENDEN
                                                                        EMPLOYEE
      ******************************************************************EMPLOYEE
      * END OF COPYBOOK "EMPLOYEE"                                     *EMPLOYEE
      ******************************************************************EMPLOYEE
