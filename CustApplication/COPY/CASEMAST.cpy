           EXEC SQL DECLARE CASE_MASTER TABLE                            ASEMAST
           ( CASENAME#IDNTITY                 CHAR(8) NOT NULL,          ASEMA
             CASE_NUM                       CHAR(6) NOT NULL,            ASEMA
             FEE_CODE                       CHAR(1) NOT NULL,            ASEMAST
             BILL_SORT_ORDER                CHAR(1) NOT NULL,            ASEMAST
             CARRIER_CODE                   CHAR(2) NOT NULL,            ASEMAST
             CARR_AFFL_CODE                 CHAR(2) NOT NULL,            ASEMAST
             ADMIN_SITE_CODE                CHAR(2) NOT NULL,            ASEMAST
             BILL_FREQUENCY                 DECIMAL(3, 0) NOT NULL,      ASEMAST
             EMPS_AT_INCEPTION              DECIMAL(5, 0) NOT NULL,      ASEMAST
             ANNUAL_COMM                    CHAR(1) NOT NULL,            ASEMAST
             RESIDENT_IND                   CHAR(1) NOT NULL,            ASEMAST
             UNDERWRITTEN_IND               CHAR(1) NOT NULL,            ASEMAST
             PREV_COV_IND                   CHAR(1) NOT NULL,            ASEMAST
             WAIT_CODE                      CHAR(1) NOT NULL,            ASEMAST
             WAIT_MONTHS                    CHAR(1) NOT NULL,            ASEMAST
             OVER20_EMPS_IND                CHAR(1) NOT NULL,            ASEMAST
             CLASS_RESTRICTION              CHAR(1) NOT NULL,            ASEMAST
             AGENT_CODE                     CHAR(1) NOT NULL,            ASEMAST
             EASYFLEX_3                     CHAR(1) NOT NULL,            ASEMAST
             HANDLE_BILL_IND                CHAR(1) NOT NULL,            ASEMAST
             LIFE_OPTION                    CHAR(1) NOT NULL,            ASEMAST
             INFORCE_AGING_CODE             CHAR(1) NOT NULL,            ASEMAST
             REINSTATE_FLAG                 CHAR(1) NOT NULL,            ASEMAST
             RECOMPOSITE_IND                CHAR(1) NOT NULL,            ASEMAST
             COMPOSITE_SWITCH               CHAR(1) NOT NULL,            ASEMAST
             CHECK_DIGIT                    CHAR(1) NOT NULL,            ASEMAST
             USER_HANDLE                    CHAR(1) NOT NULL,            ASEMAST
             LATE_CHARGE                    CHAR(1) NOT NULL,            ASEMAST
             BELOW_MINIMUM                  CHAR(1) NOT NULL,           CASEMAST
             TRUST_CODE                     CHAR(2) NOT NULL,           CASEMAST
             CO_CARRIER                     CHAR(2) NOT NULL,           CASEMAST
             RATE_AREA                      CHAR(2) NOT NULL,           CASEMAST
             RATE_LEVEL                     CHAR(2) NOT NULL,           CASEMAST
             ACTIVE_CODE                    CHAR(2) NOT NULL,           CASEMAST
             NEXT_HLTH_LEVEL                CHAR(2) NOT NULL,           CASEMAST
             AREA_CHANGE                    CHAR(2) NOT NULL,           CASEMAST
             BILL_SITE_CODE                 CHAR(2) NOT NULL,           CASEMAST
             COUNTY_CODE                    CHAR(3) NOT NULL,           CASEMAST
             SIC_CODE                       CHAR(4) NOT NULL,           CASEMAST
             SPECIAL_INFO                   CHAR(8) NOT NULL,           CASEMAST
             GENERAL_INFO                   CHAR(15) NOT NULL,          CASEMAST
             TIER_NUM                       DECIMAL(1, 0) NOT NULL,     CASEMAST
             MONTHS_BELOW_MIN               DECIMAL(1, 0) NOT NULL,     CASEMAST
             BILLING_MONTH                  DECIMAL(2, 0) NOT NULL,     CASEMAST
             ROOM_CHARGE                    DECIMAL(3, 0) NOT NULL,     CASEMAST
             NUM_OF_DAYS                    DECIMAL(3, 0) NOT NULL,     CASEMAST
             HLTH_NUM_OF_EMPS               DECIMAL(5, 0) NOT NULL,     CASEMAST
             NUM_OF_ADDITIONS               DECIMAL(5, 0) NOT NULL,     CASEMAST
             NUM_OF_TERMS                   DECIMAL(5, 0) NOT NULL,     CASEMAST
             PREV_FEE                       DECIMAL(5, 0) NOT NULL,     CASEMAST
             CO_CARR_UNIQUE-NUM               CHAR(6) NOT NULL,           CASEMA
             MAGIC_PROPOSAL_NUM             DECIMAL(7, 0) NOT NULL,     CASEMAST
             PREV_EMP_ADJUST                DECIMAL(7, 2) NOT NULL,     CASEMAST
             TERM_ADJUSTMENT                DECIMAL(8, 2) NOT NULL,     CASEMAST
             PREM_COLLECTED_YTD             DECIMAL(9, 0) NOT NULL,     CASEMAST
             PREM_COLLECTED_ITD             DECIMAL(9, 0) NOT NULL,     CASEMAST
             CLAIMS_PAID_YTD                DECIMAL(9, 0) NOT NULL,     CASEMAST
             CLAIMS_PAID_ITD                DECIMAL(9, 0) NOT NULL,     CASEMAST
             CYCLE_CURR_BAL                 DECIMAL(11, 2) NOT NULL,    CASEMAST
             CYCLE_30DAY_BAL                DECIMAL(11, 2) NOT NULL,    CASEMAST
             CYCLE_CASH_RECVD               DECIMAL(11, 2) NOT NULL,    CASEMAST
             CYCLE_ADJUSTMENTS              DECIMAL(11, 2) NOT NULL,    CASEMAST
             CYCLE_PREMIUM                  DECIMAL(11, 2) NOT NULL,    CASEMAST
             PREV_CURR_BAL                  DECIMAL(11, 2) NOT NULL,    CASEMAST
             PREV_30DAY_BAL                 DECIMAL(11, 2) NOT NULL,    CASEMAST
             PREV_CASH_RECVD                DECIMAL(11, 2) NOT NULL,    CASEMAST
             PREV_ADJUSTMENTS               DECIMAL(11, 2) NOT NULL,    CASEMAST
             LAST_BILL_TOT_PREM             DECIMAL(11, 2) NOT NULL,    CASEMAST
             ACTUAL_TOT_PREM                DECIMAL(11, 2) NOT NULL,    CASEMAST
             ADDED_DATE                     TIMESTAMP NOT NULL,         CASEMAST
             ADDED_LOGON                    CHAR(8) NOT NULL,           CASEMAST
             CHANGE_DATE                    TIMESTAMP NOT NULL,         CASEMAST
             CHANGE_LOGON                   CHAR(8) NOT NULL,           CASEMAST
             EFT_ACCT_TYPE                  CHAR(1) NOT NULL,           CASEMAST
             PRENOTE_IND                    CHAR(1) NOT NULL,           CASEMAST
             ACCT_RECONCILED                CHAR(1) NOT NULL,           CASEMAST
             EFT_FLAG                       CHAR(1) NOT NULL,           CASEMAST
             CONTRIB_FACTOR                 CHAR(1) NOT NULL,           CASEMAST
             MENTAL_DIS                     CHAR(1) NOT NULL,           CASEMAST
             ICU_MULTIPLIER                 CHAR(1) NOT NULL,           CASEMAST
             ADJ_TRANS                      CHAR(1) NOT NULL,           CASEMAST
             ALCOHOL_DIS                    CHAR(1) NOT NULL,           CASEMAST
             WELL_CHILD_IND                 CHAR(1) NOT NULL,           CASEMAST
             PREV_DENTAL_COV                CHAR(1) NOT NULL,           CASEMAST
             BUSINESS_IND                   CHAR(1) NOT NULL,           CASEMAST
             LIFE_ROUND_OPT                 CHAR(1) NOT NULL,           CASEMAST
             PREV_CARRIER                   CHAR(2) NOT NULL,           CASEMAST
             PREV_POLICY                    CHAR(2) NOT NULL,           CASEMAST
             BANK_ID_CODE                   CHAR(2) NOT NULL,           CASEMAST
             ALERT_CODE                     CHAR(2) NOT NULL,           CASEMAST
             AGREEMENT_NUM                  CHAR(3) NOT NULL,           CASEMAST
             HMO_PLAN_FOR_COBRA             CHAR(4) NOT NULL,           CASEMAST
             ALT_UNIQUE-NUM                   CHAR(8) NOT NULL,           CASEMA
             TRANSIT_NUM                    CHAR(9) NOT NULL,           CASEMAST
             EFT_ACCT_NUM                   CHAR(18) NOT NULL,          CASEMAST
             CONTACT_NAME                   CHAR(22) NOT NULL,          CASEMAST
             INCEPTION_DATE                 DATE NOT NULL,              CASEMAST
             ELIGIBLE_DATE                  DATE NOT NULL,              CASEMAST
             STAT_DATE                      DATE NOT NULL,              CASEMAST
             PREV_TERM                      DATE NOT NULL,              CASEMAST
             PREV_INCEPTION                 DATE NOT NULL,              CASEMAST
             REINST_PREV_TERM               DATE NOT NULL,              CASEMAST
             TERM_DATE                      DATE NOT NULL,              CASEMAST
             REENTRY_DATE                   DATE NOT NULL,              CASEMAST
             BILL_REINSTATE                 DATE NOT NULL,              CASEMAST
             LAST_BILL_DATE                 DATE NOT NULL,              CASEMAST
             NEXT_BILL_DATE                 DATE NOT NULL,              CASEMAST
             CLAIMS_PAID_THRU               DATE NOT NULL,              CASEMAST
             NEW_SITE_EFF_DATE              DATE NOT NULL,              CASEMAST
             RATE_INCREASE                  DATE NOT NULL,              CASEMAST
             DATE_OFF                       DATE NOT NULL,              CASEMAST
             ADM_FEE_COLL_THRU              DATE NOT NULL,              CASEMAST
             JOINDER_COLL_THRU              DATE NOT NULL,              CASEMAST
             ASSOCFEE_PAID_THRU             DATE NOT NULL,              CASEMAST
             CASH_THRU_DATE                 DATE NOT NULL,              CASEMAST
             RATE_REFERENCE_DTE             DATE NOT NULL,              CASEMAST
             NSF_CHECKS_COUNT               DECIMAL(1, 0) NOT NULL,     CASEMAST
             QUARTER                        DECIMAL(2, 0) NOT NULL,     CASEMAST
             ALCOHOL_DAYS_IN                DECIMAL(2, 0) NOT NULL,     CASEMAST
             ALCOHOL_DAYS_OUT               DECIMAL(2, 0) NOT NULL,     CASEMAST
             DEP_COV_AGE1                   DECIMAL(2, 0) NOT NULL,     CASEMAST
             DEP_COV_AGE2                   DECIMAL(2, 0) NOT NULL,     CASEMAST
             FAMILY_LIMIT_DED               DECIMAL(2, 1) NOT NULL,     CASEMAST
             MENTAL_DAYS_IN                 DECIMAL(3, 0) NOT NULL,     CASEMAST
             MENTAL_PCT_COI_OUT             DECIMAL(3, 0) NOT NULL,     CASEMAST
             WDI_ACCIDENT                   DECIMAL(3, 0) NOT NULL,     CASEMAST
             WDI_SICKNESS                   DECIMAL(3, 0) NOT NULL,     CASEMAST
             SUB_STD_FACTOR                 DECIMAL(3, 2) NOT NULL,     CASEMAST
             COINSURANCE_PCT                DECIMAL(3, 2) NOT NULL,     CASEMAST
             LOAD_FACTOR                    DECIMAL(3, 2) NOT NULL,     CASEMAST
             IND_LIFE_FACTOR                DECIMAL(3, 2) NOT NULL,     CASEMAST
             CASE_SIZE_DISCOUNT             DECIMAL(3, 2) NOT NULL,     CASEMAST
             ALERT_FACTOR                   DECIMAL(3, 2) NOT NULL,     CASEMAST
             DEDUCTIBLE_AMT                 DECIMAL(5, 0) NOT NULL,     CASEMAST
             MENTAL_MAX_AMT_OUT             DECIMAL(5, 0) NOT NULL,     CASEMAST
             SUPPACC_BEN_AMT                DECIMAL(5, 0) NOT NULL,     CASEMAST
             LTD_MAX                        DECIMAL(5, 0) NOT NULL,     CASEMAST
             CLAIMANTS_LAST_YR              DECIMAL(5, 0) NOT NULL,     CASEMAST
             CLAIMANTS_2YRS_AGO             DECIMAL(5, 0) NOT NULL,     CASEMAST
             UNCOLL_EMPADD_CHRG             DECIMAL(5, 2) NOT NULL,     CASEMAST
             RATE_FACTOR                    DECIMAL(5, 2) NOT NULL,     CASEMAST
             LIFE_EXP_ADJ                   DECIMAL(5, 4) NOT NULL,     CASEMAST
             WI_EXP_ADJ                     DECIMAL(5, 4) NOT NULL,     CASEMAST
             MED_EXP_ADJ                    DECIMAL(5, 4) NOT NULL,     CASEMAST
             DENT_EXP_ADJ                   DECIMAL(5, 4) NOT NULL,     CASEMAST
             TREND_FACTOR                   DECIMAL(5, 4) NOT NULL,     CASEMAST
             COINSURANCE_MAX                DECIMAL(7, 0) NOT NULL,     CASEMAST
             COINSURANCE_LIMIT              DECIMAL(7, 0) NOT NULL,     CASEMAST
             LIFETIME_MAX                   DECIMAL(7, 0) NOT NULL,     CASEMAST
             MIN_LIFE_BEN                   DECIMAL(7, 0) NOT NULL,     CASEMAST
             MAX_LIFE_BEN                   DECIMAL(7, 0) NOT NULL,     CASEMAST
             EXPOSURE_LAST_YR               DECIMAL(7, 2) NOT NULL,     CASEMAST
             EXPOSURE_2YRS_AGO              DECIMAL(7, 2) NOT NULL,     CASEMAST
             DUNS_NUM                       DECIMAL(9, 0) NOT NULL,     CASEMAST
             UNCOLL_HCC_CHARGES             DECIMAL(9, 2) NOT NULL,     CASEMAST
             LIFE_DUE_UNP_ADV               DECIMAL(9, 2) NOT NULL,     CASEMAST
             DLIFE_DUE_UNP_ADV              DECIMAL(9, 2) NOT NULL,     CASEMAST
             OTHER_DUE_UNP_ADV              DECIMAL(9, 2) NOT NULL,     CASEMAST
             DISAB_DUE_UNP_ADV              DECIMAL(9, 2) NOT NULL,     CASEMAST
             LTD_DUE_UNP_ADV                DECIMAL(9, 2) NOT NULL,     CASEMAST
             HLTH_DUE_UNP_ADV               DECIMAL(9, 2) NOT NULL,     CASEMAST
             DENTAL_DUE_UNP_ADV             DECIMAL(9, 2) NOT NULL,     CASEMAST
             PCS_DUE_UNP_ADV                DECIMAL(9, 2) NOT NULL,     CASEMAST
             BANK_ACCT_NUM                  DECIMAL(10, 0) NOT NULL,    CASEMAST
             NSF_INCR_CHKS_CNT              DECIMAL(1, 0) NOT NULL,     CASEMAST
             PREMIUM_REFUND_IND             CHAR(1) NOT NULL,           CASEMAST
             PRODUCT_LINE                   CHAR(3) NOT NULL,           CASEMAST
             PREV_UNIQUE-NUM                  CHAR(6) NOT NULL,           CASEMA
             PRODUCT_NUMBER                 CHAR(4) NOT NULL,           CASEMAST
             UW_OR_IND                      CHAR(1) NOT NULL,           CASEMAST
             UW_OR_DATE                     DATE NOT NULL,              CASEMAST
             HOW_BILLED                     CHAR(2) NOT NULL,           CASEMAST
             CREDCARD_CODE                  CHAR(1) NOT NULL,           CASEMAST
             CREDCARD_NUMBER                CHAR(20) NOT NULL,          CASEMAST
             CREDCARD_EXP_DATE              DATE NOT NULL,              CASEMAST
             LTD_TRUST_CODE                 CHAR(2) NOT NULL,           CASEMAST
             LIST_BILL_CIM                  CHAR(8) NOT NULL,           CASEMAST
             BILL_ADDRESS_CIM               CHAR(8) NOT NULL,           CASEMAST
             LAST_GUAR_PREM                 DECIMAL(7, 2) NOT NULL,     CASEMAST
             LAST_GUAR_FLG                  CHAR(1) NOT NULL,           CASEMAST
             CEDED_IND                      CHAR(1) NOT NULL,           CASEMAST
             PREPAID_VENDOR                 CHAR(2) NOT NULL,           CASEMAST
             ASSOCIATION_ID                 CHAR(6) NOT NULL,           CASEMAST
             GURNTEE_ISS_IND                CHAR(1) NOT NULL,           CASEMAST
             GURNTEE_ISS_DATE               DATE NOT NULL,              CASEMAST
             HLTH_TAKEOVER_IND              CHAR(1) NOT NULL,           CASEMAST
             DENT_TAKEOVER_IND              CHAR(1) NOT NULL,           CASEMAST
             MAT_TAKEOVER_IND               CHAR(1) NOT NULL,           CASEMAST
             STD_TAKEOVER_IND               CHAR(1) NOT NULL,           CASEMAST
             VIS_TAKEOVER_IND               CHAR(1) NOT NULL,           CASEMAST
             MINIMUM_HOURS                  DECIMAL(4, 2) NOT NULL,     CASEMAST
             LOB_CODE              CHAR(3)  NOT NULL WITH DEFAULT,      CASEMAST
             OPENENROL_BEGIN_DT    CHAR(4)  NOT NULL WITH DEFAULT,      CASEMAST
             OPENENROL_EFF_DT      CHAR(4)  NOT NULL WITH DEFAULT,      CASEMAST
             OPENENROL_END_DT      CHAR(4)  NOT NULL WITH DEFAULT,      CASEMAST
             SERVICE_AREA          CHAR(4)  NOT NULL WITH DEFAULT,      CASEMAST
             STUDENT_AGE_RULE      CHAR(4)  NOT NULL WITH DEFAULT,      CASEMAST
             COUNT_ME_FLAG         CHAR(1)  NOT NULL WITH DEFAULT,      CASEMAST
             CLM_OFFICE_PAYPNT1    CHAR(3)  NOT NULL WITH DEFAULT,      CASEMAST
             CLM_OFFICE_PAYPNT2    CHAR(3)  NOT NULL WITH DEFAULT,      CASEMAST
             AGENT_REP_ID          CHAR(5)  NOT NULL WITH DEFAULT,      CASEMAST
             ANNIVERSARY_DATE      CHAR(4)  NOT NULL WITH DEFAULT,      CASEMAST
             LAPSE_RULE            CHAR(3)  NOT NULL WITH DEFAULT,      CASEMAST
             LAPSE_DATE            DATE     NOT NULL WITH DEFAULT,      CASEMAST
             LAPSE_COUNTER         DECIMAL(2,0) NOT NULL WITH DEFAULT,  CASEMAST
             SUSPEND_LAPSE         CHAR(1)      NOT NULL WITH DEFAULT,  CASEMAST
             CARRIER_POLICY_NUM    CHAR(20)     NOT NULL WITH DEFAULT,  CASEMAST
             INSURER_POLICY_NUM    CHAR(10)     NOT NULL WITH DEFAULT,  CASEMAST
             FIELD_SALES_REP       CHAR(3)      NOT NULL WITH DEFAULT,  CASEMAST
             ROLLOVER_DATE         DATE         NOT NULL WITH DEFAULT,  CASEMAST
             ROLLOVER_FLAG         CHAR(1)      NOT NULL WITH DEFAULT,  CASEMAST
             PRODUCT_EFF_DATE      DATE,                                CASEMAST
R01873       ORIGIN_CODE           CHAR(2),                             CASEMAST
R01873       PRE_BILL_INDICATOR    CHAR(1),                             CASEMAST
R02194       INITIAL_PYMT_TYPE     CHAR(1)      NOT NULL WITH DEFAULT,  CASEMAST
R04004       OUTPUT_FLAG           CHAR(1)      NOT NULL WITH DEFAULT,  CASEMAST
R03896       CUST_DEFINED_INFO     CHAR(20)     NOT NULL WITH DEFAULT,  CASEMAST
R04004       OUTPUT_RECD_DATE      DATE,                                CASEMAST
R04045       COVERED_DEPS          DECIMAL(5,0) NOT NULL WITH DEFAULT,  CASEMAST
R04045       ASSOCIATE_ID          CHAR(40)     NOT NULL WITH DEFAULT,  CASEMAST
R04045       EMPLOYER_ID           CHAR(15)     NOT NULL WITH DEFAULT,  CASEMAST
R04045       ENROLLMENT_FLAG       CHAR(1)      NOT NULL WITH DEFAULT,  CASEMAST
11269        EXCHANGE_SUBSCRIBER_ID VARCHAR(50) NOT NULL,
11269        CARRIER_SUBSCRIBER_ID  VARCHAR(50) NOT NULL,
11269        OWNING_CARRIER         CHAR(2) NOT NULL,
11269        PROD_LVL_OWNING_CARRIER CHAR(2) NOT NULL WITH DEFAULT
           ) END-EXEC.                                                  CASEMAST
      ******************************************************************CASEMAST
      * COBOL DECLARATION FOR TABLE CASE_MASTER                        *CASEMAST
      ******************************************************************CASEMAST
       01  CASE-MASTER-REC.                                             CASEMAST
      *                       CASENAME#IDNTY                              CASEMA
           10 CS-1                 PIC X(8).                            CASEMAST
      *                       UNIQUE-NUM                                  CASEMA
           10 CS-2                 PIC X(6).                            CASEMAST
      *                       FEE_CODE                                  CASEMAST
           10 CS-3                 PIC X(1).                            CASEMAST
      *                       BILL_SORT_ORDER                           CASEMAST
           10 CS-4                 PIC X(1).                            CASEMAST
      *                       CARRIER_CODE                              CASEMAST
           10 CS-5                 PIC X(2).                            CASEMAST
      *                       CARR_AFFL_CODE                            CASEMAST
           10 CS-6                 PIC X(2).                            CASEMAST
      *                       ADMIN_SITE_CODE                           CASEMAST
           10 CS-7                 PIC X(2).                            CASEMAST
      *                       BILL_FREQUENCY                            CASEMAST
           10 CS-8                 PIC S999V USAGE COMP-3.              CASEMAST
      *                       EMPS_AT_INCEPTION                         CASEMAST
           10 CS-9                 PIC S99999V USAGE COMP-3.            CASEMAST
      *                       ANNUAL_COMM                               CASEMAST
           10 CS-10                PIC X(1).                            CASEMAST
      *                       RESIDENT_IND                              CASEMAST
           10 CS-11                PIC X(1).                            CASEMAST
      *                       UNDERWRITTEN_IND                          CASEMAST
           10 CS-12                PIC X(1).                            CASEMAST
      *                       PREV_COV_IND                              CASEMAST
           10 CS-13                PIC X(1).                            CASEMAST
      *                       WAIT_CODE                                 CASEMAST
           10 CS-14                PIC X(1).                            CASEMAST
      *                       WAIT_MONTHS                               CASEMAST
           10 CS-15                PIC X(1).                            CASEMAST
      *                       OVER20_EMPS_IND                           CASEMAST
           10 CS-16                PIC X(1).                            CASEMAST
      *                       CLASS_RESTRICTION                         CASEMAST
           10 CS-17                PIC X(1).                            CASEMAST
      *                       AGENT_CODE                                CASEMAST
           10 CS-18                PIC X(1).                            CASEMAST
      *                       EASYFLEX_3                                CASEMAST
           10 CS-19                PIC X(1).                            CASEMAST
      *                       HANDLE_BILL_IND                           CASEMAST
           10 CS-20                PIC X(1).                            CASEMAST
      *                       LIFE_OPTION                               CASEMAST
           10 CS-21                PIC X(1).                            CASEMAST
      *                       INFORCE_AGING_CODE                        CASEMAST
           10 CS-22                PIC X(1).                            CASEMAST
      *                       REINSTATE_FLAG                            CASEMAST
           10 CS-23                PIC X(1).                            CASEMAST
      *                       RECOMPOSITE_IND                           CASEMAST
           10 CS-24                PIC X(1).                            CASEMAST
      *                       COMPOSITE_SWITCH                          CASEMAST
           10 CS-25                PIC X(1).                            CASEMAST
      *                       CHECK_DIGIT                               CASEMAST
           10 CS-26                PIC X(1).                            CASEMAST
      *                       USER_HANDLE                               CASEMAST
           10 CS-27                PIC X(1).                            CASEMAST
      *                       LATE_CHARGE                               CASEMAST
           10 CS-28                PIC X(1).                            CASEMAST
      *                       BELOW_MINIMUM                             CASEMAST
           10 CS-29                PIC X(1).                            CASEMAST
      *                       TRUST_CODE                                CASEMAST
           10 CS-30                PIC X(2).                            CASEMAST
      *                       CO_CARRIER                                CASEMAST
           10 CS-31                PIC X(2).                            CASEMAST
      *                       RATE_AREA                                 CASEMAST
           10 CS-32                PIC X(2).                            CASEMAST
      *                       RATE_LEVEL                                CASEMAST
           10 CS-33                PIC X(2).                            CASEMAST
      *                       ACTIVE_CODE                               CASEMAST
           10 CS-34                PIC X(2).                            CASEMAST
      *                       NEXT_HLTH_LEVEL                           CASEMAST
           10 CS-35                PIC X(2).                            CASEMAST
      *                       AREA_CHANGE                               CASEMAST
           10 CS-36                PIC X(2).                            CASEMAST
      *                       BILL_SITE_CODE                            CASEMAST
           10 CS-37                PIC X(2).                            CASEMAST
      *                       COUNTY_CODE                               CASEMAST
           10 CS-38                PIC X(3).                            CASEMAST
      *                       SIC_CODE                                  CASEMAST
           10 CS-39                PIC X(4).                            CASEMAST
      *                       SPECIAL_INFO                              CASEMAST
           10 CS-40                PIC X(8).                            CASEMAST
      *                       GENERAL_INFO                              CASEMAST
           10 CS-41                PIC X(15).                           CASEMAST
      *                       TIER_NUM                                  CASEMAST
           10 CS-42                PIC S9V USAGE COMP-3.                CASEMAST
      *                       MONTHS_BELOW_MIN                          CASEMAST
           10 CS-43                PIC S9V USAGE COMP-3.                CASEMAST
      *                       BILLING_MONTH                             CASEMAST
           10 CS-44                PIC S99V USAGE COMP-3.               CASEMAST
      *                       ROOM_CHARGE                               CASEMAST
           10 CS-45                PIC S999V USAGE COMP-3.              CASEMAST
      *                       NUM_OF_DAYS                               CASEMAST
           10 CS-46                PIC S999V USAGE COMP-3.              CASEMAST
      *                       HLTH_NUM_OF_EMPS                          CASEMAST
           10 CS-47                PIC S99999V USAGE COMP-3.            CASEMAST
      *                       NUM_OF_ADDITIONS                          CASEMAST
           10 CS-48                PIC S99999V USAGE COMP-3.            CASEMAST
      *                       NUM_OF_TERMS                              CASEMAST
           10 CS-49                PIC S99999V USAGE COMP-3.            CASEMAST
      *                       PREV_FEE                                  CASEMAST
           10 CS-50                PIC S99999V USAGE COMP-3.            CASEMAST
      *                       CO_CARR_UNIQUE-NUM                          CASEMA
           10 CS-51                PIC X(6).                            CASEMAST
      *                       MAGIC_PROPOSAL_NUM                        CASEMAST
           10 CS-52                PIC S9999999V USAGE COMP-3.          CASEMAST
      *                       PREV_EMP_ADJUST                           CASEMAST
           10 CS-53                PIC S99999V99 USAGE COMP-3.          CASEMAST
      *                       TERM_ADJUSTMENT                           CASEMAST
           10 CS-54                PIC S999999V99 USAGE COMP-3.         CASEMAST
      *                       PREM_COLLECTED_YTD                        CASEMAST
           10 CS-55                PIC S999999999V USAGE COMP-3.        CASEMAST
      *                       PREM_COLLECTED_ITD                        CASEMAST
           10 CS-56                PIC S999999999V USAGE COMP-3.        CASEMAST
      *                       CLAIMS_PAID_YTD                           CASEMAST
           10 CS-57                PIC S999999999V USAGE COMP-3.        CASEMAST
      *                       CLAIMS_PAID_ITD                           CASEMAST
           10 CS-58                PIC S999999999V USAGE COMP-3.        CASEMAST
      *                       CYCLE_CURR_BAL                            CASEMAST
           10 CS-59                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       CYCLE_30DAY_BAL                           CASEMAST
           10 CS-60                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       CYCLE_CASH_RECVD                          CASEMAST
           10 CS-61                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       CYCLE_ADJUSTMENTS                         CASEMAST
           10 CS-62                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       CYCLE_PREMIUM                             CASEMAST
           10 CS-63                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       PREV_CURR_BAL                             CASEMAST
           10 CS-64                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       PREV_30DAY_BAL                            CASEMAST
           10 CS-65                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       PREV_CASH_RECVD                           CASEMAST
           10 CS-66                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       PREV_ADJUSTMENTS                          CASEMAST
           10 CS-67                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       LAST_BILL_TOT_PREM                        CASEMAST
           10 CS-68                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       ACTUAL_TOT_PREM                           CASEMAST
           10 CS-69                PIC S999999999V99 USAGE COMP-3.      CASEMAST
      *                       ADDED_DATE                                CASEMAST
           10 CS-70                PIC X(26).                           CASEMAST
      *                       ADDED_LOGON                               CASEMAST
           10 CS-71                PIC X(8).                            CASEMAST
      *                       CHANGE_DATE                               CASEMAST
           10 CS-72                PIC X(26).                           CASEMAST
      *                       CHANGE_LOGON                              CASEMAST
           10 CS-73                PIC X(8).                            CASEMAST
      *                       EFT_ACCT_TYPE                             CASEMAST
           10 CS-74                PIC X(1).                            CASEMAST
      *                       PRENOTE_IND                               CASEMAST
           10 CS-75                PIC X(1).                            CASEMAST
      *                       ACCT_RECONCILED                           CASEMAST
           10 CS-76                PIC X(1).                            CASEMAST
      *                       EFT_FLAG                                  CASEMAST
           10 CS-77                PIC X(1).                            CASEMAST
      *                       CONTRIB_FACTOR                            CASEMAST
           10 CS-78                PIC X(1).                            CASEMAST
      *                       MENTAL_DIS                                CASEMAST
           10 CS-79                PIC X(1).                            CASEMAST
      *                       ICU_MULTIPLIER                            CASEMAST
           10 CS-80                PIC X(1).                            CASEMAST
      *                       ADJ_TRANS                                 CASEMAST
           10 CS-81                PIC X(1).                            CASEMAST
      *                       ALCOHOL_DIS                               CASEMAST
           10 CS-82                PIC X(1).                            CASEMAST
      *                       WELL_CHILD_IND                            CASEMAST
           10 CS-83                PIC X(1).                            CASEMAST
      *                       PREV_DENTAL_COV                           CASEMAST
           10 CS-84                PIC X(1).                            CASEMAST
      *                       BUSINESS_IND                              CASEMAST
           10 CS-85                PIC X(1).                            CASEMAST
      *                       LIFE_ROUND_OPT                            CASEMAST
           10 CS-86                PIC X(1).                            CASEMAST
      *                       PREV_CARRIER                              CASEMAST
           10 CS-87                PIC X(2).                            CASEMAST
      *                       PREV_POLICY                               CASEMAST
           10 CS-88                PIC X(2).                            CASEMAST
      *                       BANK_ID_CODE                              CASEMAST
           10 CS-89                PIC X(2).                            CASEMAST
      *                       ALERT_CODE                                CASEMAST
           10 CS-90                PIC X(2).                            CASEMAST
      *                       AGREEMENT_NUM                             CASEMAST
           10 CS-91                PIC X(3).                            CASEMAST
      *                       HMO_PLAN_FOR_COBRA                        CASEMAST
           10 CS-92                PIC X(4).                            CASEMAST
      *                       ALT_UNIQUE-NUM                              CASEMA
           10 CS-93                PIC X(8).                            CASEMAST
      *                       TRANSIT_NUM                               CASEMAST
           10 CS-94                PIC X(9).                            CASEMAST
      *                       EFT_ACCT_NUM                              CASEMAST
           10 CS-95                PIC X(18).                           CASEMAST
      *                       CONTACT_NAME                              CASEMAST
           10 CS-96                PIC X(22).                           CASEMAST
      *                       INCEPTION_DATE                            CASEMAST
           10 CS-97                PIC X(10).                           CASEMAST
      *                       ELIGIBLE_DATE                             CASEMAST
           10 CS-98                PIC X(10).                           CASEMAST
      *                       STAT_DATE                                 CASEMAST
           10 CS-99                PIC X(10).                           CASEMAST
      *                       PREV_TERM                                 CASEMAST
           10 CS-100               PIC X(10).                           CASEMAST
      *                       PREV_INCEPTION                            CASEMAST
           10 CS-101               PIC X(10).                           CASEMAST
      *                       REINST_PREV_TERM                          CASEMAST
           10 CS-102               PIC X(10).                           CASEMAST
      *                       TERM_DATE                                 CASEMAST
           10 CS-103               PIC X(10).                           CASEMAST
      *                       REENTRY_DATE                              CASEMAST
           10 CS-104               PIC X(10).                           CASEMAST
      *                       BILL_REINSTATE                            CASEMAST
           10 CS-105               PIC X(10).                           CASEMAST
      *                       LAST_BILL_DATE                            CASEMAST
           10 CS-106               PIC X(10).                           CASEMAST
      *                       NEXT_BILL_DATE                            CASEMAST
           10 CS-107               PIC X(10).                           CASEMAST
      *                       CLAIMS_PAID_THRU                          CASEMAST
           10 CS-108               PIC X(10).                           CASEMAST
      *                       NEW_SITE_EFF_DATE                         CASEMAST
           10 CS-109               PIC X(10).                           CASEMAST
      *                       RATE_INCREASE                             CASEMAST
           10 CS-110               PIC X(10).                           CASEMAST
      *                       DATE_OFF                                  CASEMAST
           10 CS-111               PIC X(10).                           CASEMAST
      *                       ADM_FEE_COLL_THRU                         CASEMAST
           10 CS-112               PIC X(10).                           CASEMAST
      *                       JOINDER_COLL_THRU                         CASEMAST
           10 CS-113               PIC X(10).                           CASEMAST
      *                       ASSOCFEE_PAID_THRU                        CASEMAST
           10 CS-114               PIC X(10).                           CASEMAST
      *                       CASH_THRU_DATE                            CASEMAST
           10 CS-115               PIC X(10).                           CASEMAST
      *                       RATE_REFERENCE_DTE                        CASEMAST
           10 CS-116               PIC X(10).                           CASEMAST
      *                       NSF_CHECKS_COUNT                          CASEMAST
           10 CS-117               PIC S9V USAGE COMP-3.                CASEMAST
      *                       QUARTER                                   CASEMAST
           10 CS-118               PIC S99V USAGE COMP-3.               CASEMAST
      *                       ALCOHOL_DAYS_IN                           CASEMAST
           10 CS-119               PIC S99V USAGE COMP-3.               CASEMAST
      *                       ALCOHOL_DAYS_OUT                          CASEMAST
           10 CS-120               PIC S99V USAGE COMP-3.               CASEMAST
      *                       DEP_COV_AGE1                              CASEMAST
           10 CS-121               PIC S99V USAGE COMP-3.               CASEMAST
      *                       DEP_COV_AGE2                              CASEMAST
           10 CS-122               PIC S99V USAGE COMP-3.               CASEMAST
      *                       FAMILY_LIMIT_DED                          CASEMAST
           10 CS-123               PIC S9V9 USAGE COMP-3.               CASEMAST
      *                       MENTAL_DAYS_IN                            CASEMAST
           10 CS-124               PIC S999V USAGE COMP-3.              CASEMAST
      *                       MENTAL_PCT_COI_OUT                        CASEMAST
           10 CS-125               PIC S999V USAGE COMP-3.              CASEMAST
      *                       WDI_ACCIDENT                              CASEMAST
           10 CS-126               PIC S999V USAGE COMP-3.              CASEMAST
      *                       WDI_SICKNESS                              CASEMAST
           10 CS-127               PIC S999V USAGE COMP-3.              CASEMAST
      *                       SUB_STD_FACTOR                            CASEMAST
           10 CS-128               PIC S9V99 USAGE COMP-3.              CASEMAST
      *                       COINSURANCE_PCT                           CASEMAST
           10 CS-129               PIC S9V99 USAGE COMP-3.              CASEMAST
      *                       LOAD_FACTOR                               CASEMAST
           10 CS-130               PIC S9V99 USAGE COMP-3.              CASEMAST
      *                       IND_LIFE_FACTOR                           CASEMAST
           10 CS-131               PIC S9V99 USAGE COMP-3.              CASEMAST
      *                       CASE_SIZE_DISCOUNT                        CASEMAST
           10 CS-132               PIC S9V99 USAGE COMP-3.              CASEMAST
      *                       ALERT_FACTOR                              CASEMAST
           10 CS-133               PIC S9V99 USAGE COMP-3.              CASEMAST
      *                       DEDUCTIBLE_AMT                            CASEMAST
           10 CS-134               PIC S99999V USAGE COMP-3.            CASEMAST
      *                       MENTAL_MAX_AMT_OUT                        CASEMAST
           10 CS-135               PIC S99999V USAGE COMP-3.            CASEMAST
      *                       SUPPACC_BEN_AMT                           CASEMAST
           10 CS-136               PIC S99999V USAGE COMP-3.            CASEMAST
      *                       LTD_MAX                                   CASEMAST
           10 CS-137               PIC S99999V USAGE COMP-3.            CASEMAST
      *                       CLAIMANTS_LAST_YR                         CASEMAST
           10 CS-138               PIC S99999V USAGE COMP-3.            CASEMAST
      *                       CLAIMANTS_2YRS_AGO                        CASEMAST
           10 CS-139               PIC S99999V USAGE COMP-3.            CASEMAST
      *                       UNCOLL_EMPADD_CHRG                        CASEMAST
           10 CS-140               PIC S999V99 USAGE COMP-3.            CASEMAST
      *                       RATE_FACTOR                               CASEMAST
           10 CS-141               PIC S999V99 USAGE COMP-3.            CASEMAST
      *                       LIFE_EXP_ADJ                              CASEMAST
           10 CS-142               PIC S9V9999 USAGE COMP-3.            CASEMAST
      *                       WI_EXP_ADJ                                CASEMAST
           10 CS-143               PIC S9V9999 USAGE COMP-3.            CASEMAST
      *                       MED_EXP_ADJ                               CASEMAST
           10 CS-144               PIC S9V9999 USAGE COMP-3.            CASEMAST
      *                       DENT_EXP_ADJ                              CASEMAST
           10 CS-145               PIC S9V9999 USAGE COMP-3.            CASEMAST
      *                       TREND_FACTOR                              CASEMAST
           10 CS-146               PIC S9V9999 USAGE COMP-3.            CASEMAST
      *                       COINSURANCE_MAX                           CASEMAST
           10 CS-147               PIC S9999999V USAGE COMP-3.          CASEMAST
      *                       COINSURANCE_LIMIT                         CASEMAST
           10 CS-148               PIC S9999999V USAGE COMP-3.          CASEMAST
      *                       LIFETIME_MAX                              CASEMAST
           10 CS-149               PIC S9999999V USAGE COMP-3.          CASEMAST
      *                       MIN_LIFE_BEN                              CASEMAST
           10 CS-150               PIC S9999999V USAGE COMP-3.          CASEMAST
      *                       MAX_LIFE_BEN                              CASEMAST
           10 CS-151               PIC S9999999V USAGE COMP-3.          CASEMAST
      *                       EXPOSURE_LAST_YR                          CASEMAST
           10 CS-152               PIC S99999V99 USAGE COMP-3.          CASEMAST
      *                       EXPOSURE_2YRS_AGO                         CASEMAST
           10 CS-153               PIC S99999V99 USAGE COMP-3.          CASEMAST
      *                       DUNS_NUM                                  CASEMAST
           10 CS-154               PIC S999999999V USAGE COMP-3.        CASEMAST
      *                       UNCOLL_HCC_CHARGES                        CASEMAST
           10 CS-155               PIC S9999999V99 USAGE COMP-3.        CASEMAST
      *                       LIFE_DUE_UNP_ADV                          CASEMAST
           10 CS-156               PIC S9999999V99 USAGE COMP-3.        CASEMAST
      *                       DLIFE_DUE_UNP_ADV                         CASEMAST
           10 CS-157               PIC S9999999V99 USAGE COMP-3.        CASEMAST
      *                       OTHER_DUE_UNP_ADV                         CASEMAST
           10 CS-158               PIC S9999999V99 USAGE COMP-3.        CASEMAST
      *                       DISAB_DUE_UNP_ADV                         CASEMAST
           10 CS-159               PIC S9999999V99 USAGE COMP-3.        CASEMAST
      *                       LTD_DUE_UNP_ADV                           CASEMAST
           10 CS-160               PIC S9999999V99 USAGE COMP-3.        CASEMAST
      *                       HLTH_DUE_UNP_ADV                          CASEMAST
           10 CS-161               PIC S9999999V99 USAGE COMP-3.        CASEMAST
      *                       DENTAL_DUE_UNP_ADV                        CASEMAST
           10 CS-162               PIC S9999999V99 USAGE COMP-3.        CASEMAST
      *                       PCS_DUE_UNP_ADV                           CASEMAST
           10 CS-163               PIC S9999999V99 USAGE COMP-3.        CASEMAST
      *                       BANK_ACCT_NUM                             CASEMAST
           10 CS-164               PIC S9999999999V USAGE COMP-3.       CASEMAST
      *                       NSF_INCR_CHKS_CNT                         CASEMAST
           10 CS-165               PIC S9V USAGE COMP-3.                CASEMAST
      *                       PREMIUM_REFUND_IND                        CASEMAST
           10 CS-166               PIC X.                               CASEMAST
      *                       PRODUCT_LINE                              CASEMAST
           10 CS-167               PIC X(3).                            CASEMAST
      *                       PREV_UNIQUE-NUM                             CASEMA
           10 CS-168               PIC X(6).                            CASEMAST
      *                       PRODUCT_NUMBER                            CASEMAST
           10 CS-169               PIC X(4).                            CASEMAST
      *                       UW_OR_IND                                 CASEMAST
           10 CS-170               PIC X(1).                            CASEMAST
      *                       UW_OR_DATE                                CASEMAST
           10 CS-171               PIC X(10).                           CASEMAST
      *                       HOW_BILLED                                CASEMAST
           10 CS-172               PIC X(2).                            CASEMAST
      *                       CREDCARD_CODE                             CASEMAST
           10 CS-173               PIC X(1).                            CASEMAST
      *                       CREDCARD_NUMBER                           CASEMAST
           10 CS-174               PIC X(20).                           CASEMAST
      *                       CREDCARD_EXP_DATE                         CASEMAST
           10 CS-175               PIC X(10).                           CASEMAST
      *                       LTD_TRUST_CODE                            CASEMAST
           10 CS-176               PIC X(02).                           CASEMAST
      *                       LIST_BILL_CIM                             CASEMAST
           10 CS-177               PIC X(08).                           CASEMAST
      *                       BILL_ADDRESS_CIM                          CASEMAST
           10 CS-178               PIC X(08).                           CASEMAST
      *                       LAST-GUAR-PREM                            CASEMAST
           10 CS-179               PIC S99999V99 USAGE COMP-3.          CASEMAST
      *                       LAST-GUAR-FLG                             CASEMAST
           10 CS-180               PIC X(1).                            CASEMAST
      *                       CEDED-IND                                 CASEMAST
           10 CS-181               PIC X(1).                            CASEMAST
      *                       PREPAID_VENDOR                            CASEMAST
           10 CS-182               PIC X(2).                            CASEMAST
      *                       ASSOCIATION_ID                            CASEMAST
           10 CS-183               PIC X(6).                            CASEMAST
      *                       GURNTEE_ISS_IND                           CASEMAST
           10 CS-184               PIC X(1).                            CASEMAST
      *                       GURNTEE_ISS_DATE                          CASEMAST
           10 CS-185               PIC X(10).                           CASEMAST
      *                       HLTH_TAKEOVER_IND                         CASEMAST
           10 CS-186               PIC X(1).                            CASEMAST
      *                       DENT_TAKEOVER_IND                         CASEMAST
           10 CS-187               PIC X(1).                            CASEMAST
      *                       MAT_TAKEOVER_IND                          CASEMAST
           10 CS-188               PIC X(1).                            CASEMAST
      *                       STD_TAKEOVER_IND                          CASEMAST
           10 CS-189               PIC X(1).                            CASEMAST
      *                       VIS_TAKEOVER_IND                          CASEMAST
           10 CS-190               PIC X(1).                            CASEMAST
      *                       MINIMUM_HOURS                             CASEMAST
           10 CS-191               PIC S99V99 USAGE COMP-3.             CASEMAST
      *                       LOB_CODE                                  CASEMAST
           10 CS-192               PIC X(3).                            CASEMAST
      *                       OPENENROL_BEGIN_DT                        CASEMAST
           10 CS-193               PIC X(4).                            CASEMAST
      *                       OPENENROL_EFF_DT                          CASEMAST
           10 CS-194               PIC X(4).                            CASEMAST
      *                       OPENENROL_END_DT                          CASEMAST
           10 CS-195               PIC X(4).                            CASEMAST
      *                       SERVICE_AREA                              CASEMAST
           10 CS-196               PIC X(4).                            CASEMAST
      *                       STUDENT_AGE_RULE                          CASEMAST
           10 CS-197               PIC X(4).                            CASEMAST
      *                       COUNT_ME_FLAG                             CASEMAST
           10 CS-198               PIC X(1).                            CASEMAST
      *                       CLM_OFFICE_PAYPNT1                        CASEMAST
           10 CS-199               PIC X(3).                            CASEMAST
      *                       CLM_OFFICE_PAYPNT2                        CASEMAST
           10 CS-200               PIC X(3).                            CASEMAST
      *                       AGENT_REP_ID                              CASEMAST
           10 CS-201               PIC X(5).                            CASEMAST
      *                       ANNIVERSARY_DATE                          CASEMAST
           10 CS-202               PIC X(4).                            CASEMAST
      *                       LAPSE_RULE                                CASEMAST
           10 CS-203               PIC X(3).                            CASEMAST
      *                       LAPSE_DATE                                CASEMAST
           10 CS-204               PIC X(10).                           CASEMAST
      *                       LAPSE_COUNTER                             CASEMAST
           10 CS-205               PIC S99 USAGE COMP-3.                CASEMAST
      *                       SUSPEND_LAPSE                             CASEMAST
           10 CS-206               PIC X(1).                            CASEMAST
      *                       CARRIER_POLICY_NUM                        CASEMAST
           10 CS-207               PIC X(20).                           CASEMAST
      *                       INSURER_POLICY_NUM                        CASEMAST
           10 CS-208               PIC X(10).                           CASEMAST
      *                       FIELD_SALES_REP                           CASEMAST
           10 CS-209               PIC X(3).                            CASEMAST
      *                       ROLLOVER_DATE                             CASEMAST
           10 CS-210               PIC X(10).                           CASEMAST
      *                       ROLLOVER_FLAG                             CASEMAST
           10 CS-211               PIC X(1).                            CASEMAST
      *                       PRODUCT_EFF_DATE                          CASEMAST
           10 CS-212               PIC X(10).                           CASEMAST
R01873*                       ORIGIN_CODE                               CASEMAST
R01873     10 CS-213               PIC X(2).                            CASEMAST
R01873*                       PRE_BILL_INDICATOR                        CASEMAST
R01873     10 CS-214               PIC X(1).                            CASEMAST
R02194*                       INITIAL_PYMT_TYPE                         CASEMAST
R02194     10 CS-215               PIC X(1).                            CASEMAST
R04004*                       OUTPUT_FLAG                               CASEMAST
R04004     10 CS-216               PIC X(1).                            CASEMAST
R03896*                       CUST_DEFINED_INFO                         CASEMAST
R03896     10 CS-217               PIC X(20).                           CASEMAST
R04004*                       OUTPUT_RECD_DATE                          CASEMAST
R04004     10 CS-218               PIC X(10).                           CASEMAST
R04045*                       COVERED_DEPS                              CASEMAST
R04045     10 CS-219               PIC S99999V USAGE COMP-3.            CASEMAST
R04045*                       ASSOCIATE_ID                              CASEMAST
R04045     10 CS-220               PIC X(40).                           CASEMAST
R04045*                       EMPLOYER_ID                               CASEMAST
R04045     10 CS-221               PIC X(15).                           CASEMAST
R04045*                       ENROLLMENT_FLAG                           CASEMAST
R04045     10 CS-222               PIC X(1).                            CASEMAST
R04045*                       EXCHANGE-SUBSCRIBER-ID                    CASEMAST
11269      10 CS-223               PIC X(50).                           CASEMAST
R04045*                       CARRIER-SUBSCRIBER-ID                     CASEMAST
11269      10 CS-224               PIC X(50).                           CASEMAST
R04045*                       CASE-OWNING-CARRIER                       CASEMAST
11269      10 CS-225               PIC X(02).                           CASEMAST
R04045*                       CASE-PROD-OWNING-CARRIER                  CASEMAST
11269      10 CS-226               PIC X(02).                           CASEMAST
      ******************************************************************CASEMAST
R04045* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 222     *CASEMAST
      ******************************************************************CASEMAST
      * INDICATOR FIELDS FOR TABLE CASE_MASTER                         *CASEMAST
      ******************************************************************CASEMAST
       01  CASEMAST-INDICATOR-ARRAY.                                    CASEMAST
R04045     05  CASEMAST-INDICATORS  OCCURS 222 TIMES  PIC S9(04) COMP.  CASEMAST
       01  FILLER REDEFINES CASEMAST-INDICATOR-ARRAY.                   CASEMAST
           05  CASE-CASENAME-IDNTY-IND             PIC S9(04)    COMP.    CASEMA
           05  CASE-UNIQUE-NUM-IND                 PIC S9(04)    COMP.    CASEMA
           05  CASE-FEE-CODE-IND                 PIC S9(04)    COMP.    CASEMAST
           05  CASE-BILL-SORT-ORDER-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-CARRIER-IND                  PIC S9(04)    COMP.    CASEMAST
           05  CASE-AFFILIATION-CODE-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-ADMIN-SITE-CODE-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-BILL-FREQUENCY-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-EMPS-AT-INCEPTION-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-ANNUAL-COMM-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-RESIDENT-IND-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-UNDERWRITTEN-IND-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-COV-IND-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-WAIT-CODE-IND                PIC S9(04)    COMP.    CASEMAST
           05  CASE-WAIT-MONTHS-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-OVER20-EMPS-IND-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-CLASS-RESTRICTION-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-AGENT-CODE-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-EASYFLEX-3-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-HANDLE-BILL-IND-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-LIFE-OPTION-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-INFORCE-AGING-CODE-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-REINSTATE-FLAG-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-RECOMPOSITE-IND-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-COMPOSITE-SWITCH-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-CHECK-DIGIT-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-USER-HANDLE-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-LATE-CHARGE-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-BELOW-MINIMUM-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-TRUST-CODE-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-CO-CARRIER-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-RATE-AREA-IND                PIC S9(04)    COMP.    CASEMAST
           05  CASE-RATE-LEVEL-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-ACTIVE-CODE-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-NEXT-HLTH-LEVEL-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-AREA-CHANGE-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-BILL-SITE-CODE-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-COUNTY-CODE-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-SIC-CODE-IND                 PIC S9(04)    COMP.    CASEMAST
           05  CASE-SPECIAL-INFO-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-GENERAL-INFO-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-TIER-NUMBER-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-MONTHS-BELOW-MIN-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-BILLING-MONTH-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-ROOM-CHARGE-IND              PIC S9(04)    COMP.    CASEMAST
           05  UNIQUE-NUM-OF-DAYS-IND              PIC S9(04)    COMP.    CASEMA
           05  CASE-HLTH-NUM-OF-EMPS-IND         PIC S9(04)    COMP.    CASEMAST
           05  UNIQUE-NUM-OF-ADDITIONS-IND         PIC S9(04)    COMP.    CASEMA
           05  UNIQUE-NUM-OF-TERMS-IND             PIC S9(04)    COMP.    CASEMA
           05  CASE-PREV-FEE-IND                 PIC S9(04)    COMP.    CASEMAST
           05  CASE-CO-CARR-UNIQUE-NUM-IND         PIC S9(04)    COMP.    CASEMA
           05  CASE-MAGIC-PROPOSAL-NUM-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-EMP-ADJUST-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-TERM-ADJUSTMENT-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREM-COLLECTED-YTD-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREM-COLLECTED-ITD-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-CLAIMS-PAID-YTD-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-CLAIMS-PAID-ITD-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-CYCLE-CURR-BAL-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-CYCLE-30DAY-BAL-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-CYCLE-CASH-RECVD-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-CYCLE-ADJUSTMENTS-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-CYCLE-PREMIUM-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-CURR-BAL-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-30DAY-BAL-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-CASH-RECVD-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-ADJUSTMENTS-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-LAST-BILL-TOT-PREM-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-ACTUAL-TOT-PREM-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-ADDED-DATE-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-ADDED-LOGON-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-CHANGE-DATE-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-CHANGE-LOGON-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-EFT-ACCT-TYPE-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-PRENOTE-IND-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-ACCT-RECONCILED-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-EFT-FLAG-IND                 PIC S9(04)    COMP.    CASEMAST
           05  CASE-CONTRIB-FACTOR-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-MENTAL-DIS-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-ICU-MULTIPLIER-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-ADJ-TRANS-IND                PIC S9(04)    COMP.    CASEMAST
           05  CASE-ALCOHOL-DIS-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-WELL-CHILD-IND-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-DENTAL-COV-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-BUSINESS-IND-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-LIFE-ROUND-OPT-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-CARRIER-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-POLICY-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-BANK-ID-CODE-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-ALERT-CODE-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-AGREEMENT-NUMBER-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-HMO-PLAN-FOR-COBRA-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-ALT-UNIQUE-NUM-IND             PIC S9(04)    COMP.    CASEMA
           05  CASE-TRANSIT-NUMBER-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-EFT-ACCT-NUM-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-CONTACT-NAME-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-INCEPTION-DATE-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-ELIGIBLE-DATE-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-STAT-DATE-IND                PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-TERM-IND                PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-INCEPTION-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-REINST-PREV-TERM-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-TERM-DATE-IND                PIC S9(04)    COMP.    CASEMAST
           05  CASE-REENTRY-DATE-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-BILL-REINSTATE-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-LAST-BILL-DATE-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-NEXT-BILL-DATE-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-CLAIMS-PAID-THRU-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-NEW-SITE-EFF-DATE-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-RATE-INCREASE-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-DATE-OFF-IND                 PIC S9(04)    COMP.    CASEMAST
           05  CASE-ADM-FEE-COLL-THRU-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-JOINDER-COLL-THRU-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-ASSOCFEE-PAID-THRU-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-CASH-THRU-DATE-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-RATE-REFERENCE-DTE-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-NSF-CHECKS-COUNT-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-QUARTER-IND                  PIC S9(04)    COMP.    CASEMAST
           05  CASE-ALCOHOL-DAYS-IN-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-ALCOHOL-DAYS-OUT-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-DEP-COV-AGE1-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-DEP-COV-AGE2-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-FAMILY-LIMIT-DED-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-MENTAL-DAYS-IN-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-MENTAL-PCT-COI-OUT-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-WDI-ACCIDENT-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-WDI-SICKNESS-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-SUB-STD-FACTOR-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-COINSURANCE-PCT-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-LOAD-FACTOR-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-IND-LIFE-FACTOR-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-CASE-SIZE-DISCOUNT-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-ALERT-FACTOR-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-DEDUCTIBLE-AMT-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-MENTAL-MAX-AMT-OUT-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-SUPPACC-BEN-AMT-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-LTD-MAX-IND                  PIC S9(04)    COMP.    CASEMAST
           05  CASE-CLAIMANTS-LAST-YR-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-CLAIMANTS-2YRS-AGO-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-UNCOLL-EMPADD-CHRG-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-RATE-FACTOR-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-LIFE-EXP-ADJ-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-WI-EXP-ADJ-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-MED-EXP-ADJ-IND              PIC S9(04)    COMP.    CASEMAST
           05  CASE-DENT-EXP-ADJ-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-TREND-FACTOR-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-COINSURANCE-MAX-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-COINSURANCE-LIMIT-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-LIFETIME-MAX-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-MIN-LIFE-BEN-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-MAX-LIFE-BEN-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-EXPOSURE-LAST-YR-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-EXPOSURE-2YRS-AGO-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-DUNS-NUM-IND                 PIC S9(04)    COMP.    CASEMAST
           05  CASE-UNCOLL-HCC-CHARGES-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-LIFE-DUE-UNP-ADV-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-DLIFE-DUE-UNP-ADV-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-OTHER-DUE-UNP-ADV-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-DISAB-DUE-UNP-ADV-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-LTD-DUE-UNP-ADV-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-MATER-DUE-UNP-ADV-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-DENTAL-DUE-UNP-ADV-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-PCS-DUE-UNP-ADV-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-BANK-ACCT-NUMBER-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-NSF-INCR-CHKS-CNT-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREMIUM-REFUND-IND-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-PRODUCT-LINE-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREV-UNIQUE-NUM-IND            PIC S9(04)    COMP.    CASEMA
           05  CASE-PRODUCT-NUMBER-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-UW-OR-IND-IND                PIC S9(04)    COMP.    CASEMAST
           05  CASE-UW-OR-DATE-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-HOW-BILLED-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-CREDCARD-CODE-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-CREDCARD-NUMBER-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-CREDCARD-EXP-DATE-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-LTD-TRUST-CODE-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-LIST-BILL-IDNTY-IND            PIC S9(04)    COMP.    CASEMA
           05  CASE-BILL-ADDRESS-IDNTY-IND         PIC S9(04)    COMP.    CASEMA
           05  CASE-LAST-GUAR-PREM-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-LAST-GUAR-FLG-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-CEDED-IND-IND                PIC S9(04)    COMP.    CASEMAST
           05  CASE-PREPAID-VENDOR-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-ASSOCIATION-ID-IND           PIC S9(04)    COMP.    CASEMAST
           05  CASE-GURNTEE-ISS-IND-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-GURNTEE-ISS-DATE-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-HLTH-TAKEOVER-IND-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-DENT-TAKEOVER-IND-IND        PIC S9(04)    COMP.    CASEMAST
           05  CASE-MAT-TAKEOVER-IND-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-STD-TAKEOVER-IND-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-VIS-TAKEOVER-IND-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-MINIMUM-HOURS-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-LOB-CODE-IND                 PIC S9(04)    COMP.    CASEMAST
           05  CASE-OPENENROL-BEGIN-DT-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-OPENENROL-EFF-DT-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-OPENENROL-END-DT-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-SERVICE-AREA-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-STUDENT-AGE-RULE-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-COUNT-ME-FLAG-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-CLM-OFFICE-PAYPNT1-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-CLM-OFFICE-PAYPNT2-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-AGENT-REP-ID-IND             PIC S9(04)    COMP.    CASEMAST
           05  CASE-ANNIVERSARY-DATE-IND         PIC S9(04)    COMP.    CASEMAST
           05  CASE-LAPSE-RULE-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-LAPSE-DATE-IND               PIC S9(04)    COMP.    CASEMAST
           05  CASE-LAPSE-COUNTER-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-SUSPEND-LAPSE-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-CARRIER-POLICY-NUM-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-INSURER-POLICY-NUM-IND       PIC S9(04)    COMP.    CASEMAST
           05  CASE-FIELD-SALES-REP-IND          PIC S9(04)    COMP.    CASEMAST
           05  CASE-ROLLOVER-DATE-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-ROLLOVER-FLAG-IND            PIC S9(04)    COMP.    CASEMAST
           05  CASE-PRODUCT-EFF-DATE-IND         PIC S9(04)    COMP.    CASEMAST
R01873     05  CASE-ORIGIN-CODE-IND              PIC S9(04)    COMP.    CASEMAST
R01873     05  CASE-PRE-BILL-INDICATOR-IND       PIC S9(04)    COMP.    CASEMAST
R02194     05  CASE-INITIAL-PYMT-TYPE-IND        PIC S9(04)    COMP.    CASEMAST
R04004     05  CASE-OUTPUT-FLAG-IND              PIC S9(04)    COMP.    CASEMAST
R03896     05  CASE-CUST-DEFINED-INFO-IND        PIC S9(04)    COMP.    CASEMAST
R04004     05  CASE-OUTPUT-RECD-DATE-IND         PIC S9(04)    COMP.    CASEMAST
R04045     05  CASE-COVERED-DEPS-IND             PIC S9(04)    COMP.    CASEMAST
R04045     05  CASE-ASSOCIATE-ID-IND             PIC S9(04)    COMP.    CASEMAST
R04045     05  CASE-EMPLOYER-ID-IND              PIC S9(04)    COMP.    CASEMAST
R04045     05  CASE-ENROLLMENT-FLAG-IND          PIC S9(04)    COMP.    CASEMAST
11269      05  CASE-EX-SUBSCRIBER-ID-IND         PIC S9(04)    COMP.
11269      05  CASE-CARR-SUBSCRIBER-ID-IND       PIC S9(04)    COMP.
11269      05  CASE-OWNING-CARRIER-IND           PIC S9(04)    COMP.
11269      05  CASE-PROD-OWNING-CARRIER-IND      PIC S9(04)    COMP.
      ******************************************************************CASEMAST
      * COBOL DECLARATION FOR TABLE CASE_MASTER                        *CASEMAST
      ******************************************************************CASEMAST
       01  CASE-MASTER-RECORD.                                          CASEMAST
           05  CASE-CASENAME-IDNTY        PIC  X(8).                      CASEMA
           05  CASE-UNIQUE-NUM            PIC  X(6).                      CASEMA
           05  CASE-FEE-CODE            PIC  X(1).                      CASEMAST
           05  CASE-BILL-SORT-ORDER     PIC  X(1).                      CASEMAST
           05  CASE-CARRIER-CODE        PIC  X(2).                      CASEMAST
           05  CASE-CARR-AFFL-CODE      PIC  X(2).                      CASEMAST
           05  CASE-ADMIN-SITE-CODE     PIC  X(2).                      CASEMAST
           05  CASE-BILL-FREQUENCY      PIC S9(3)V      COMP-3.         CASEMAST
           05  CASE-EMPS-AT-INCEPTION   PIC S9(5)V      COMP-3.         CASEMAST
           05  CASE-ANNUAL-COMM         PIC  X(1).                      CASEMAST
           05  CASE-RESIDENT-IND        PIC  X(1).                      CASEMAST
           05  CASE-UNDERWRITTEN-IND    PIC  X(1).                      CASEMAST
           05  CASE-PREV-COV-IND        PIC  X(1).                      CASEMAST
           05  CASE-WAIT-CODE           PIC  X(1).                      CASEMAST
           05  CASE-WAIT-MONTHS         PIC  X(1).                      CASEMAST
           05  CASE-OVER20-EMPS-IND     PIC  X(1).                      CASEMAST
           05  CASE-CLASS-RESTRICTION   PIC  X(1).                      CASEMAST
           05  CASE-AGENT-CODE          PIC  X(1).                      CASEMAST
           05  CASE-EASYFLEX-3          PIC  X(1).                      CASEMAST
           05  CASE-HANDLE-BILL-IND     PIC  X(1).                      CASEMAST
           05  CASE-LIFE-OPTION         PIC  X(1).                      CASEMAST
           05  CASE-INFORCE-AGING-CODE  PIC  X(1).                      CASEMAST
           05  CASE-REINSTATE-FLAG      PIC  X(1).                      CASEMAST
           05  CASE-RECOMPOSITE-IND     PIC  X(1).                      CASEMAST
           05  CASE-COMPOSITE-SWITCH    PIC  X(1).                      CASEMAST
           05  CASE-CHECK-DIGIT         PIC  X(1).                      CASEMAST
           05  CASE-USER-HANDLE         PIC  X(1).                      CASEMAST
           05  CASE-LATE-CHARGE         PIC  X(1).                      CASEMAST
           05  CASE-BELOW-MINIMUM       PIC  X(1).                      CASEMAST
           05  CASE-TRUST-CODE          PIC  X(2).                      CASEMAST
           05  CASE-CO-CARRIER          PIC  X(2).                      CASEMAST
           05  CASE-RATE-AREA           PIC  X(2).                      CASEMAST
           05  CASE-RATE-LEVEL          PIC  X(2).                      CASEMAST
           05  CASE-ACTIVE-CODE         PIC  X(2).                      CASEMAST
           05  CASE-NEXT-HLTH-LEVEL     PIC  X(2).                      CASEMAST
           05  CASE-AREA-CHANGE         PIC  X(2).                      CASEMAST
           05  CASE-BILL-SITE-CODE      PIC  X(2).                      CASEMAST
           05  CASE-COUNTY-CODE         PIC  X(3).                      CASEMAST
           05  CASE-SIC-CODE            PIC  X(4).                      CASEMAST
           05  CASE-SPECIAL-INFO        PIC  X(8).                      CASEMAST
           05  CASE-GENERAL-INFO        PIC  X(15).                     CASEMAST
           05  CASE-TIER-NUM            PIC S9(1)V      COMP-3.         CASEMAST
           05  CASE-MONTHS-BELOW-MIN    PIC S9(1)V      COMP-3.         CASEMAST
           05  CASE-BILLING-MONTH       PIC S9(2)V      COMP-3.         CASEMAST
           05  CASE-ROOM-CHARGE         PIC S9(3)V      COMP-3.         CASEMAST
           05  UNIQUE-NUM-OF-DAYS         PIC S9(3)V      COMP-3.         CASEMA
           05  CASE-HLTH-NUM-OF-EMPS    PIC S9(5)V      COMP-3.         CASEMAST
           05  UNIQUE-NUM-OF-ADDITIONS    PIC S9(5)V      COMP-3.         CASEMA
           05  UNIQUE-NUM-OF-TERMS        PIC S9(5)V      COMP-3.         CASEMA
           05  CASE-PREV-FEE            PIC S9(5)V      COMP-3.         CASEMAST
           05  CASE-CO-CARR-UNIQUE-NUM    PIC  X(6).                      CASEMA
           05  CASE-MAGIC-PROPOSAL-NUM  PIC S9(7)V      COMP-3.         CASEMAST
           05  CASE-PREV-EMP-ADJUST     PIC S9(5)V99    COMP-3.         CASEMAST
           05  CASE-TERM-ADJUSTMENT     PIC S9(7)V99    COMP-3.         CASEMAST
           05  CASE-PREM-COLLECTED-YTD  PIC S9(9)V      COMP-3.         CASEMAST
           05  CASE-PREM-COLLECTED-ITD  PIC S9(9)V      COMP-3.         CASEMAST
           05  CASE-CLAIMS-PAID-YTD     PIC S9(9)V      COMP-3.         CASEMAST
           05  CASE-CLAIMS-PAID-ITD     PIC S9(9)V      COMP-3.         CASEMAST
           05  CASE-CYCLE-CURR-BAL      PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-CYCLE-30DAY-BAL     PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-CYCLE-CASH-RECVD    PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-CYCLE-ADJUSTMENTS   PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-CYCLE-PREMIUM       PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-PREV-CURR-BAL       PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-PREV-30DAY-BAL      PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-PREV-CASH-RECVD     PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-PREV-ADJUSTMENTS    PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-LAST-BILL-TOT-PREM  PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-ACTUAL-TOT-PREM     PIC S9(9)V99    COMP-3.         CASEMAST
           05  CASE-ADDED-DATE          PIC  X(26).                     CASEMAST
           05  CASE-ADDED-LOGON         PIC  X(8).                      CASEMAST
           05  CASE-CHANGE-DATE         PIC  X(26).                     CASEMAST
           05  CASE-CHANGE-LOGON        PIC  X(8).                      CASEMAST
           05  CASE-EFT-ACCT-TYPE       PIC  X(1).                      CASEMAST
           05  CASE-PRENOTE-IND         PIC  X(1).                      CASEMAST
           05  CASE-ACCT-RECONCILED     PIC  X(1).                      CASEMAST
           05  CASE-EFT-FLAG            PIC  X(1).                      CASEMAST
           05  CASE-CONTRIB-FACTOR      PIC  X(1).                      CASEMAST
           05  CASE-MENTAL-DIS          PIC  X(1).                      CASEMAST
           05  CASE-ICU-MULTIPLIER      PIC  X(1).                      CASEMAST
           05  CASE-ADJ-TRANS           PIC  X(1).                      CASEMAST
           05  CASE-ALCOHOL-DIS         PIC  X(1).                      CASEMAST
           05  CASE-WELL-CHILD-IND      PIC  X(1).                      CASEMAST
           05  CASE-PREV-DENTAL-COV     PIC  X(1).                      CASEMAST
           05  CASE-BUSINESS-IND        PIC  X(1).                      CASEMAST
           05  CASE-LIFE-ROUND-OPT      PIC  X(1).                      CASEMAST
           05  CASE-PREV-CARRIER        PIC  X(2).                      CASEMAST
           05  CASE-PREV-POLICY         PIC  X(2).                      CASEMAST
           05  CASE-BANK-ID-CODE        PIC  X(2).                      CASEMAST
           05  CASE-ALERT-CODE          PIC  X(2).                      CASEMAST
           05  CASE-AGREEMENT-NUM       PIC  X(3).                      CASEMAST
           05  CASE-HMO-PLAN-FOR-COBRA  PIC  X(4).                      CASEMAST
           05  CASE-ALT-UNIQUE-NUM        PIC  X(8).                      CASEMA
           05  CASE-TRANSIT-NUM         PIC  X(9).                      CASEMAST
           05  CASE-EFT-ACCT-NUM        PIC  X(18).                     CASEMAST
           05  CASE-CONTACT-NAME        PIC  X(22).                     CASEMAST
           05  CASE-INCEPTION-DATE      PIC  X(10).                     CASEMAST
           05  CASE-ELIGIBLE-DATE       PIC  X(10).                     CASEMAST
           05  CASE-STAT-DATE           PIC  X(10).                     CASEMAST
           05  CASE-PREV-TERM           PIC  X(10).                     CASEMAST
           05  CASE-PREV-INCEPTION      PIC  X(10).                     CASEMAST
           05  CASE-REINST-PREV-TERM    PIC  X(10).                     CASEMAST
           05  CASE-TERM-DATE           PIC  X(10).                     CASEMAST
           05  CASE-REENTRY-DATE        PIC  X(10).                     CASEMAST
           05  CASE-BILL-REINSTATE      PIC  X(10).                     CASEMAST
           05  CASE-LAST-BILL-DATE      PIC  X(10).                     CASEMAST
           05  CASE-NEXT-BILL-DATE      PIC  X(10).                     CASEMAST
           05  CASE-CLAIMS-PAID-THRU    PIC  X(10).                     CASEMAST
           05  CASE-NEW-SITE-EFF-DATE   PIC  X(10).                     CASEMAST
           05  CASE-RATE-INCREASE       PIC  X(10).                     CASEMAST
           05  CASE-DATE-OFF            PIC  X(10).                     CASEMAST
           05  CASE-ADM-FEE-COLL-THRU   PIC  X(10).                     CASEMAST
           05  CASE-JOINDER-COLL-THRU   PIC  X(10).                     CASEMAST
           05  CASE-ASSOCFEE-PAID-THRU  PIC  X(10).                     CASEMAST
           05  CASE-CASH-THRU-DATE      PIC  X(10).                     CASEMAST
           05  CASE-RATE-REFERENCE-DTE  PIC  X(10).                     CASEMAST
           05  CASE-NSF-CHECKS-COUNT    PIC S9(1)V      COMP-3.         CASEMAST
           05  CASE-QUARTER             PIC S9(2)V      COMP-3.         CASEMAST
           05  CASE-ALCOHOL-DAYS-IN     PIC S9(2)V      COMP-3.         CASEMAST
           05  CASE-ALCOHOL-DAYS-OUT    PIC S9(2)V      COMP-3.         CASEMAST
           05  CASE-DEP-COV-AGE1        PIC S9(2)V      COMP-3.         CASEMAST
           05  CASE-DEP-COV-AGE2        PIC S9(2)V      COMP-3.         CASEMAST
           05  CASE-FAMILY-LIMIT-DED    PIC S9(1)V9     COMP-3.         CASEMAST
           05  CASE-MENTAL-DAYS-IN      PIC S9(3)V      COMP-3.         CASEMAST
           05  CASE-MENTAL-PCT-COI-OUT  PIC S9(3)V      COMP-3.         CASEMAST
           05  CASE-WDI-ACCIDENT        PIC S9(3)V      COMP-3.         CASEMAST
           05  CASE-WDI-SICKNESS        PIC S9(3)V      COMP-3.         CASEMAST
           05  CASE-SUB-STD-FACTOR      PIC S9(1)V99    COMP-3.         CASEMAST
           05  CASE-COINSURANCE-PCT     PIC S9(1)V99    COMP-3.         CASEMAST
           05  CASE-LOAD-FACTOR         PIC S9(1)V99    COMP-3.         CASEMAST
           05  CASE-IND-LIFE-FACTOR     PIC S9(1)V99    COMP-3.         CASEMAST
           05  CASE-CASE-SIZE-DISCOUNT  PIC S9(1)V99    COMP-3.         CASEMAST
           05  CASE-ALERT-FACTOR        PIC S9(1)V99    COMP-3.         CASEMAST
           05  CASE-DEDUCTIBLE-AMT      PIC S9(5)V      COMP-3.         CASEMAST
           05  CASE-MENTAL-MAX-AMT-OUT  PIC S9(5)V      COMP-3.         CASEMAST
           05  CASE-SUPPACC-BEN-AMT     PIC S9(5)V      COMP-3.         CASEMAST
           05  CASE-LTD-MAX             PIC S9(5)V      COMP-3.         CASEMAST
           05  CASE-CLAIMANTS-LAST-YR   PIC S9(5)V      COMP-3.         CASEMAST
           05  CASE-CLAIMANTS-2YRS-AGO  PIC S9(5)V      COMP-3.         CASEMAST
           05  CASE-UNCOLL-EMPADD-CHRG  PIC S9(3)V99    COMP-3.         CASEMAST
           05  CASE-RATE-FACTOR         PIC S9(3)V99    COMP-3.         CASEMAST
           05  CASE-LIFE-EXP-ADJ        PIC S9(1)V9999  COMP-3.         CASEMAST
           05  CASE-WI-EXP-ADJ          PIC S9(1)V9999  COMP-3.         CASEMAST
           05  CASE-MED-EXP-ADJ         PIC S9(1)V9999  COMP-3.         CASEMAST
           05  CASE-DENT-EXP-ADJ        PIC S9(1)V9999  COMP-3.         CASEMAST
           05  CASE-TREND-FACTOR        PIC S9(1)V9999  COMP-3.         CASEMAST
           05  CASE-COINSURANCE-MAX     PIC S9(7)V      COMP-3.         CASEMAST
           05  CASE-COINSURANCE-LIMIT   PIC S9(7)V      COMP-3.         CASEMAST
           05  CASE-LIFETIME-MAX        PIC S9(7)V      COMP-3.         CASEMAST
           05  CASE-MIN-LIFE-BEN        PIC S9(7)V      COMP-3.         CASEMAST
           05  CASE-MAX-LIFE-BEN        PIC S9(7)V      COMP-3.         CASEMAST
           05  CASE-EXPOSURE-LAST-YR    PIC S9(5)V99    COMP-3.         CASEMAST
           05  CASE-EXPOSURE-2YRS-AGO   PIC S9(5)V99    COMP-3.         CASEMAST
           05  CASE-DUNS-NUM            PIC S9(9)V      COMP-3.         CASEMAST
           05  CASE-UNCOLL-HCC-CHARGES  PIC S9(7)V99    COMP-3.         CASEMAST
           05  CASE-LIFE-DUE-UNP-ADV    PIC S9(7)V99    COMP-3.         CASEMAST
           05  CASE-DLIFE-DUE-UNP-ADV   PIC S9(7)V99    COMP-3.         CASEMAST
           05  CASE-OTHER-DUE-UNP-ADV   PIC S9(7)V99    COMP-3.         CASEMAST
           05  CASE-DISAB-DUE-UNP-ADV   PIC S9(7)V99    COMP-3.         CASEMAST
           05  CASE-LTD-DUE-UNP-ADV     PIC S9(7)V99    COMP-3.         CASEMAST
           05  CASE-HLTH-DUE-UNP-ADV    PIC S9(7)V99    COMP-3.         CASEMAST
           05  CASE-DENTAL-DUE-UNP-ADV  PIC S9(7)V99    COMP-3.         CASEMAST
           05  CASE-PCS-DUE-UNP-ADV     PIC S9(7)V99    COMP-3.         CASEMAST
           05  CASE-BANK-ACCT-NUM       PIC S9(10)V     COMP-3.         CASEMAST
           05  CASE-NSF-INCR-CHKS-CNT   PIC S9(1)V      COMP-3.         CASEMAST
           05  CASE-PREMIUM-REFUND-IND  PIC X.                          CASEMAST
           05  CASE-MAST-PRODUCT-LINE   PIC X(3).                       CASEMAST
           05  CASE-PREV-UNIQUE-NUM     PIC  X(6).                        CASEMA
           05  CASE-MAST-PRODUCT-NUMBER PIC X(4).                       CASEMAST
           05  CASE-UW-OR-IND           PIC X(1).                       CASEMAST
           05  CASE-UW-OR-DATE          PIC X(10).                      CASEMAST
           05  CASE-HOW-BILLED          PIC X(02).                      CASEMAST
           05  CASE-CREDCARD-CODE       PIC X(01).                      CASEMAST
           05  CASE-CREDCARD-NUMBER     PIC X(20).                      CASEMAST
           05  CASE-CREDCARD-EXP-DATE   PIC X(10).                      CASEMAST
           05  CASE-LTD-TRUST-CODE      PIC X(02).                      CASEMAST
           05  CASE-LIST-BILL-IDNTY       PIC X(08).                      CASEMA
           05  CASE-BILL-ADDRESS-IDNTY    PIC X(08).                      CASEMA
           05  CASE-LAST-GUAR-PREM      PIC S9(5)V99    COMP-3.         CASEMAST
           05  CASE-LAST-GUAR-FLG       PIC X(01).                      CASEMAST
           05  CASE-CEDED-IND           PIC X(01).                      CASEMAST
           05  CASE-PREPAID-VENDOR      PIC X(02).                      CASEMAST
           05  CASE-ASSOCIATION-ID      PIC X(06).                      CASEMAST
           05  CASE-GURNTEE-ISS-IND     PIC X(01).                      CASEMAST
           05  CASE-GURNTEE-ISS-DATE    PIC X(10).                      CASEMAST
           05  CASE-HLTH-TAKEOVER-IND   PIC X(01).                      CASEMAST
           05  CASE-DENT-TAKEOVER-IND   PIC X(01).                      CASEMAST
           05  CASE-MAT-TAKEOVER-IND    PIC X(01).                      CASEMAST
           05  CASE-STD-TAKEOVER-IND    PIC X(01).                      CASEMAST
           05  CASE-VIS-TAKEOVER-IND    PIC X(01).                      CASEMAST
           05  CASE-MINIMUM-HOURS       PIC S9(2)V99    COMP-3.         CASEMAST
           05  CASE-LOB-CODE            PIC X(03).                      CASEMAST
           05  CASE-OPENENROL-BEGIN-DT  PIC X(04).                      CASEMAST
           05  CASE-OPENENROL-EFF-DT    PIC X(04).                      CASEMAST
           05  CASE-OPENENROL-END-DT    PIC X(04).                      CASEMAST
           05  CASE-SERVICE-AREA        PIC X(04).                      CASEMAST
           05  CASE-STUDENT-AGE-RULE    PIC X(04).                      CASEMAST
           05  CASE-COUNT-ME-FLAG       PIC X(01).                      CASEMAST
           05  CASE-CLM-OFFICE-PAYPNT1  PIC X(03).                      CASEMAST
           05  CASE-CLM-OFFICE-PAYPNT2  PIC X(03).                      CASEMAST
           05  CASE-AGENT-REP-ID        PIC X(05).                      CASEMAST
           05  CASE-ANNIVERSARY-DATE    PIC X(04).                      CASEMAST
           05  CASE-LAPSE-RULE          PIC X(03).                      CASEMAST
           05  CASE-LAPSE-DATE          PIC X(10).                      CASEMAST
           05  CASE-LAPSE-COUNTER       PIC S99         COMP-3.         CASEMAST
           05  CASE-SUSPEND-LAPSE       PIC X(01).                      CASEMAST
           05  CASE-CARRIER-POLICY-NUM  PIC X(20).                      CASEMAST
           05  CASE-INSURER-POLICY-NUM  PIC X(10).                      CASEMAST
           05  CASE-FIELD-SALES-REP     PIC X(03).                      CASEMAST
           05  CASE-ROLLOVER-DATE       PIC X(10).                      CASEMAST
           05  CASE-ROLLOVER-FLAG       PIC X(01).                      CASEMAST
           05  CASE-PRODUCT-EFF-DATE    PIC X(10).                      CASEMAST
R01873     05  CASE-ORIGIN-CODE         PIC X(2).                       CASEMAST
R01873     05  CASE-PRE-BILL-INDICATOR  PIC X(1).                       CASEMAST
R02194     05  CASE-INITIAL-PYMT-TYPE   PIC X(1).                       CASEMAST
R04004     05  CASE-OUTPUT-FLAG         PIC X(1).                       CASEMAST
R03896     05  CASE-CUST-DEFINED-INFO   PIC X(20).                      CASEMAST
R04004     05  CASE-OUTPUT-RECD-DATE    PIC X(10).                      CASEMAST
R04045     05  CASE-COVERED-DEPS        PIC S9(5)V      COMP-3.         CASEMAST
R04045     05  CASE-ASSOCIATE-ID        PIC X(40).                      CASEMAST
R04045     05  CASE-EMPLOYER-ID         PIC X(15).                      CASEMAST
R04045     05  CASE-ENROLLMENT-FLAG     PIC X(1).                       CASEMAST
           05  CASE-EXCHANGE-SUBSCRIBER-ID.
              49 CASE-EX-SUBSCRIBER-ID-LEN
                 PIC S9(4) USAGE COMP.
              49 CASE-EX-SUBSCRIBER-ID-TEXT
                 PIC X(50).
           05 CASE-CARRIER-SUBSCRIBER-ID.
              49 CASE-CARR-SUBSCRIBER-ID-LEN
                 PIC S9(4) USAGE COMP.
              49 CASE-CARR-SUBSCRIBER-ID-TEXT
                 PIC X(50).
           05 CASE-OWNING-CARRIER       PIC X(2).
           05 CASE-PROD-OWNING-CARRIER  PIC X(2).
