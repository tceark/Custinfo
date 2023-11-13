
           EXEC SQL DECLARE BROKER TABLE                                BROKER
           ( BROKER_ID                      CHAR(9) NOT NULL,           BROKER
             AGNTNAME#IDNTY                   CHAR(8) NOT NULL,           BROKER

             TYPE_CODE                      CHAR(1) NOT NULL,           BROKER
             PSI_STATUS_CODE                CHAR(1) NOT NULL,           BROKER
             CARR_AFFL_CODE                 CHAR(2) NOT NULL,           BROKER
             FULLTIME_CARR_CODE             CHAR(2) NOT NULL,           BROKER
             AGENCY_NUM                     CHAR(5),                    BROKER
             AGENT_NUM                      CHAR(7),                    BROKER
             GENERAL_AGENT_NUM              CHAR(7),                    BROKER
             SELLING_AGENT_NUM              CHAR(6),                    BROKER
             AGENT_CODE                     CHAR(2),                    BROKER
             AGENT_STATE_CODE               CHAR(4),                    BROKER
             ACCOUNT_CODE                   CHAR(6),                    BROKER
             CLASS_CODE                     CHAR(2),                    BROKER
             CONTRACT_CODE                  CHAR(3),                    BROKER
             EMPLOYEE_CODE                  CHAR(5),                    BROKER
             MANAGER_CODE                   CHAR(5),                    BROKER
             SUPERVISOR_CODE                CHAR(5),                    BROKER
             BRANCH_NUM                     CHAR(3),                    BROKER
             DEBIT_NUM                      CHAR(2),                    BROKER
             DISTRICT_NUM                   CHAR(4),                    BROKER
             OFFICE_NUM                     CHAR(5),                    BROKER
             SEQUENCE_NUM                   CHAR(4),                    BROKER
             CARR_BROKER_SSN                CHAR(9),                    BROKER
             DSO_NUM                        CHAR(3),                    BROKER
             FOAB_NUM                       CHAR(1),                    BROKER
             REGION_NUM                     CHAR(4),                    BROKER
             RANK_CODE                      CHAR(1),                    BROKER
             CARR_STATUS_CODE               CHAR(2),                    BROKER
             UNIT_NUM                       CHAR(2),                    BROKER
             WITHHOLD_COMMISS               CHAR(1) NOT NULL,           BROKER
             REVISED_DATE                   DATE NOT NULL,              BROKER
             EXTN_AGENT_NUMBER              CHAR(12),                   BROKER
             AGENCY_PRODUCERID              CHAR(8),                    BROKER
             START_DATE                     DATE NOT NULL,              BROKER
             END_DATE                       DATE,                       BROKER
             REVISED_LOGON                  CHAR(8),                    BROKER
             CUSTOM_LINK                    CHAR(10),                   BROKER
             FO_EXTN_AGENT_NUM              CHAR(12),                   BROKER
             VESTING_PERCENT                DECIMAL(4, 1),              BROKER
             VESTING_END_DATE               DATE,                       BROKER
             ADV_PERCENT                    DECIMAL(5, 2),              BROKER
             RECOUP_PERCENT                 DECIMAL(5, 2),              BROKER
P11812       GA_COMM_PERCENTAGE             DECIMAL(5, 2) NOT NULL,     BROKER
P11812       HIX_AGENT_NUMBER               VARCHAR(25) NOT NULL,
P11812       AGENT_FFM_ID                   VARCHAR(25) NOT NULL
           ) END-EXEC.                                                  BROKER
      ******************************************************************BROKER
      * COBOL DECLARATION FOR TABLE BROKER                             *BROKER
      ******************************************************************BROKER
       01  DCLBROKER.                                                   BROKER
           10 BROKER-ID            PIC X(9).                            BROKER
           10 AGNTNAME-IDNTY         PIC X(8).                            BROKER

           10 TYPE-CODE            PIC X(1).                            BROKER
           10 PSI-STATUS-CODE      PIC X(1).                            BROKER
           10 CARR-AFFL-CODE       PIC X(2).                            BROKER
           10 FULLTIME-CARR-CODE   PIC X(2).                            BROKER
           10 AGENCY-NUM           PIC X(5).                            BROKER
           10 AGENT-NUM            PIC X(7).                            BROKER
           10 GENERAL-AGENT-NUM    PIC X(7).                            BROKER
           10 SELLING-AGENT-NUM    PIC X(6).                            BROKER
           10 AGENT-CODE           PIC X(2).                            BROKER
           10 AGENT-STATE-CODE     PIC X(4).                            BROKER
           10 ACCOUNT-CODE         PIC X(6).                            BROKER
           10 CLASS-CODE           PIC X(2).                            BROKER
           10 CONTRACT-CODE        PIC X(3).                            BROKER
           10 EMPLOYEE-CODE        PIC X(5).                            BROKER
           10 MANAGER-CODE         PIC X(5).                            BROKER
           10 SUPERVISOR-CODE      PIC X(5).                            BROKER
           10 BRANCH-NUM           PIC X(3).                            BROKER
           10 DEBIT-NUM            PIC X(2).                            BROKER
           10 DISTRICT-NUM         PIC X(4).                            BROKER
           10 OFFICE-NUM           PIC X(5).                            BROKER
           10 SEQUENCE-NUM         PIC X(4).                            BROKER
           10 CARR-BROKER-SSN      PIC X(9).                            BROKER
           10 DSO-NUM              PIC X(3).                            BROKER
           10 FOAB-NUM             PIC X(1).                            BROKER
           10 REGION-NUM           PIC X(4).                            BROKER
           10 RANK-CODE            PIC X(1).                            BROKER
           10 CARR-STATUS-CODE     PIC X(2).                            BROKER
           10 UNIT-NUM             PIC X(2).                            BROKER
           10 WITHHOLD-COMMISS     PIC X(1).                            BROKER
           10 REVISED-DATE         PIC X(10).                           BROKER
           10 EXTN-AGENT-NUMBER    PIC X(12).                           BROKER
           10 AGENCY-PRODUCERID    PIC X(8).                            BROKER
           10 START-DATE           PIC X(10).                           BROKER
           10 END-DATE             PIC X(10).                           BROKER
           10 REVISED-LOGON        PIC X(8).                            BROKER
           10 CUSTOM-LINK          PIC X(10).                           BROKER
           10 FO-EXTN-AGENT-NUM    PIC X(12).                           BROKER
           10 VESTING-PERCENT      PIC S9(3)V9(1) USAGE COMP-3.         BROKER
           10 VESTING-END-DATE     PIC X(10).                           BROKER
           10 ADV-PERCENT          PIC S9(3)V9(2) USAGE COMP-3.         BROKER
           10 RECOUP-PERCENT       PIC S9(3)V9(2) USAGE COMP-3.         BROKER
           10 GA-COMM-PERCENTAGE   PIC S9(3)V9(2) USAGE COMP-3.         BROKER
P11812     10 HIX-AGENT-NUMBER.
P11812        49 HIX-AGENT-NUMBER-LEN    PIC S9(4) USAGE COMP.
P11812        49 HIX-AGENT-NUMBER-TEXT   PIC X(25).
P11812     10 AGENT-FFM-ID.
P11812        49 AGENT-FFM-ID-LEN        PIC S9(4) USAGE COMP.
P11812        49 AGENT-FFM-ID-TEXT       PIC X(25).
      *---------------------------------------------------------------* BROKER
      * INDICATOR VARIABLES ARE USED TO CHECK FOR NULL VALUES. IF A   * BROKER
      * NULL VALUE IS RETURNED THE INDICATOR IS SET TO A NEGATIVE     * BROKER
      * VALUE.  USING NULLABLE FIELDS WITHOUT CHECKING THE INDICATOR  * BROKER
      * FOR THAT FIELDS CAN RAISE AN ERROR CONDITION OR YIELD         * BROKER
      * UNPREDICTABLE OR WRONG RESULTS.                               * BROKER
      *---------------------------------------------------------------* BROKER
       01  BROKER-INDICATOR-AREA.                                       BROKER
P11812     10  BROKER-INDICATORS   PIC S9(4) COMP OCCURS 46 TIMES.
      ******************************************************************BROKER
P11812* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 46
      ******************************************************************BROKER
       01  BROKER-INDICATOR-AREA-TOO.                                   BROKER
           10 IND-BROKER-ID                 PIC S9(4) COMP.             BROKER
           10 IND-AGNTNAME-IDNTY              PIC S9(4) COMP.             BROKER

           10 IND-TYPE-CODE                 PIC S9(4) COMP.             BROKER
           10 IND-PSI-STATUS-CODE           PIC S9(4) COMP.             BROKER
           10 IND-CARR-AFFL-CODE            PIC S9(4) COMP.             BROKER
           10 IND-FULLTIME-CARR-CODE        PIC S9(4) COMP.             BROKER
           10 IND-AGENCY-NUM                PIC S9(4) COMP.             BROKER
           10 IND-AGENT-NUM                 PIC S9(4) COMP.             BROKER
           10 IND-GENERAL-AGENT-NUM         PIC S9(4) COMP.             BROKER
           10 IND-SELLING-AGENT-NUM         PIC S9(4) COMP.             BROKER
           10 IND-AGENT-CODE                PIC S9(4) COMP.             BROKER
           10 IND-AGENT-STATE-CODE          PIC S9(4) COMP.             BROKER
           10 IND-ACCOUNT-CODE              PIC S9(4) COMP.             BROKER
           10 IND-CLASS-CODE                PIC S9(4) COMP.             BROKER
           10 IND-CONTRACT-CODE             PIC S9(4) COMP.             BROKER
           10 IND-EMPLOYEE-CODE             PIC S9(4) COMP.             BROKER
           10 IND-MANAGER-CODE              PIC S9(4) COMP.             BROKER
           10 IND-SUPERVISOR-CODE           PIC S9(4) COMP.             BROKER
           10 IND-BRANCH-NUM                PIC S9(4) COMP.             BROKER
           10 IND-DEBIT-NUM                 PIC S9(4) COMP.             BROKER
           10 IND-DISTRICT-NUM              PIC S9(4) COMP.             BROKER
           10 IND-OFFICE-NUM                PIC S9(4) COMP.             BROKER
           10 IND-SEQUENCE-NUM              PIC S9(4) COMP.             BROKER
           10 IND-CARR-BROKER-SSN           PIC S9(4) COMP.             BROKER
           10 IND-DSO-NUM                   PIC S9(4) COMP.             BROKER
           10 IND-FOAB-NUM                  PIC S9(4) COMP.             BROKER
           10 IND-REGION-NUM                PIC S9(4) COMP.             BROKER
           10 IND-RANK-CODE                 PIC S9(4) COMP.             BROKER
           10 IND-CARR-STATUS-CODE          PIC S9(4) COMP.             BROKER
           10 IND-UNIT-NUM                  PIC S9(4) COMP.             BROKER
           10 IND-WITHHOLD-COMMISS          PIC S9(4) COMP.             BROKER
           10 IND-REVISED-DATE              PIC S9(4) COMP.             BROKER
           10 IND-EXTN-AGENT-NUMBER         PIC S9(4) COMP.             BROKER
           10 IND-AGENCY-PRODUCERID         PIC S9(4) COMP.             BROKER
           10 IND-START-DATE                PIC S9(4) COMP.             BROKER
           10 IND-END-DATE                  PIC S9(4) COMP.             BROKER
           10 IND-REVISED-LOGON             PIC S9(4) COMP.             BROKER
           10 IND-CUSTOM-LINK               PIC S9(4) COMP.             BROKER
           10 IND-FO-EXTN-AGENT-NUM         PIC S9(4) COMP.             BROKER
           10 IND-VESTING-PERCENT           PIC S9(4) COMP.             BROKER
           10 IND-VESTING-END-DATE          PIC S9(4) COMP.             BROKER
           10 IND-ADV-PERCENT               PIC S9(4) COMP.             BROKER
           10 IND-RECOUP-PERCENT            PIC S9(4) COMP.             BROKER
           10 IND-GA-COMM-PERCENTAGE        PIC S9(4) COMP.             BROKER
P11812     10 IND-HIX-AGENT-NUMBER          PIC S9(4) COMP.
P11812     10 IND-AGENT-FFM-ID              PIC S9(4) COMP.
      ******************************************************************BROKER
      * FIELD PREFIXED COBOL DECLARATION FOR BROKER.                   *BROKER
      ******************************************************************BROKER
       01  WR-BROKER-RECORD.                                            BROKER
           10 BR-BROKER-ID            PIC X(9).                         BROKER
           10 BR-AGNTNAME-IDNTY         PIC X(8).                         BROKER

           10 BR-TYPE-CODE            PIC X(1).                         BROKER
           10 BR-PSI-STATUS-CODE      PIC X(1).                         BROKER
           10 BR-CARR-AFFL-CODE       PIC X(2).                         BROKER
           10 BR-FULLTIME-CARR-CODE   PIC X(2).                         BROKER
           10 BR-AGENCY-NUM           PIC X(5).                         BROKER
           10 BR-AGENT-NUM            PIC X(7).                         BROKER
           10 BR-GENERAL-AGENT-NUM    PIC X(7).                         BROKER
           10 BR-SELLING-AGENT-NUM    PIC X(6).                         BROKER
           10 BR-AGENT-CODE           PIC X(2).                         BROKER
           10 BR-AGENT-STATE-CODE     PIC X(4).                         BROKER
           10 BR-ACCOUNT-CODE         PIC X(6).                         BROKER
           10 BR-CLASS-CODE           PIC X(2).                         BROKER
           10 BR-CONTRACT-CODE        PIC X(3).                         BROKER
           10 BR-EMPLOYEE-CODE        PIC X(5).                         BROKER
           10 BR-MANAGER-CODE         PIC X(5).                         BROKER
           10 BR-SUPERVISOR-CODE      PIC X(5).                         BROKER
           10 BR-BRANCH-NUM           PIC X(3).                         BROKER
           10 BR-DEBIT-NUM            PIC X(2).                         BROKER
           10 BR-DISTRICT-NUM         PIC X(4).                         BROKER
           10 BR-OFFICE-NUM           PIC X(5).                         BROKER
           10 BR-SEQUENCE-NUM         PIC X(4).                         BROKER
           10 BR-CARR-BROKER-SSN      PIC X(9).                         BROKER
           10 BR-DSO-NUM              PIC X(3).                         BROKER
           10 BR-FOAB-NUM             PIC X(1).                         BROKER
           10 BR-REGION-NUM           PIC X(4).                         BROKER
           10 BR-RANK-CODE            PIC X(1).                         BROKER
           10 BR-CARR-STATUS-CODE     PIC X(2).                         BROKER
           10 BR-UNIT-NUM             PIC X(2).                         BROKER
           10 BR-WITHHOLD-COMMISS     PIC X(1).                         BROKER
           10 BR-REVISED-DATE         PIC X(10).                        BROKER
           10 BR-EXTN-AGENT-NUMBER    PIC X(12).                        BROKER
           10 BR-AGENCY-PRODUCERID    PIC X(8).                         BROKER
           10 BR-START-DATE           PIC X(10).                        BROKER
           10 BR-END-DATE             PIC X(10).                        BROKER
           10 BR-REVISED-LOGON        PIC X(8).                         BROKER
           10 BR-CUSTOM-LINK          PIC X(10).                        BROKER
           10 BR-FO-EXTN-AGENT-NUM    PIC X(12).                        BROKER
           10 BR-VESTING-PERCENT      PIC S9(3)V9(1) USAGE COMP-3.      BROKER
           10 BR-VESTING-END-DATE     PIC X(10).                        BROKER
           10 BR-ADV-PERCENT          PIC S9(3)V9(2) USAGE COMP-3.      BROKER
           10 BR-RECOUP-PERCENT       PIC S9(3)V9(2) USAGE COMP-3.      BROKER
           10 BR-GA-COMM-PERCENTAGE   PIC S9(3)V9(2) USAGE COMP-3.      BROKER
P11812     10 BR-HIX-AGENT-NUMBER.
P11812        49 BR-HIX-AGENT-NUMBER-LEN    PIC S9(4) USAGE COMP.
P11812        49 BR-HIX-AGENT-NUMBER-TEXT   PIC X(25).
P11812     10 BR-AGENT-FFM-ID.
P11812        49 BR-AGENT-FFM-ID-LEN        PIC S9(4) USAGE COMP.
P11812        49 BR-AGENT-FFM-ID-TEXT       PIC X(25).
