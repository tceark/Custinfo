           EXEC SQL DECLARE AGNTNAME TABLE                              00000100
           ( IDNTITY                        CHAR(8) NOT NULL,           00000200
             LAST_NAME                      CHAR(20),                   00000300
             FIRST_NAME                     CHAR(15),                   00000400
             MIDDLE_NAME                    CHAR(15),                   00000500
             PREFIX                         CHAR(4),                    00000600
             SUFFIX1                        CHAR(4),                    00000700
             SUFFIX2                        CHAR(4),                    00000800
             COMPANY_IND                    CHAR(1) NOT NULL,           00000900
             COMPANY_IN_ADDRESS             CHAR(1) NOT NULL,           00001000
             COMPANY_NAME                   CHAR(30),                   00001100
             DISPLAY_NAME                   CHAR(30),                   00001200
             NICKNAME                       CHAR(10),                   00001300
             ADDRESS1                       CHAR(30) NOT NULL,          00001400
             ADDRESS2                       CHAR(30),                   00001500
             CITY                           CHAR(30) NOT NULL,          00001600
             STATE                          CHAR(2) NOT NULL,           00001700
             ZIP                            CHAR(5) NOT NULL,           00001800
             ZIP_PLUS4                      CHAR(4) NOT NULL,           00001900
             COUNTY_CODE                    CHAR(5) NOT NULL,           00002000
             AREA_CODE                      CHAR(3) NOT NULL,           00002100
             PHONE                          CHAR(7) NOT NULL,           00002200
             PHONE_EXTENSION                CHAR(4) NOT NULL,           00002300
             SSN                            CHAR(9) NOT NULL,           00002400
             SEX                            CHAR(1) NOT NULL,           00002500
             BIRTH_DATE                     DATE,                       00002600
             FINALST_REAS_CODE              CHAR(3) NOT NULL,           00002700
             FINALST_OVRD_IND               CHAR(1) NOT NULL,           00002800
             DUP_ADDR_OVRD_IND              CHAR(1) NOT NULL,           00002900
             EFFECTIVE_DATE                 DATE NOT NULL,              00003000
             CHANGE_DATE                    TIMESTAMP NOT NULL,         00003100
             CHANGE_LOGON                   CHAR(8) NOT NULL,           00003200
             ENTITY_TYPE                    CHAR(2) NOT NULL,           00003300
             RECORD_STATUS                  CHAR(1) NOT NULL,           00003400
             ALT_ADDRESS_IND                CHAR(1) NOT NULL,           00003500
             FUTURE_ADDRESS_IND             CHAR(1) NOT NULL,           00003600
             RECORD_ORIGIN                  CHAR(1) NOT NULL,           00003700
             COMBINED_STATUS                CHAR(2) NOT NULL,           00003800
             SITE_CODE                      CHAR(2) NOT NULL,           00003900
             NAME_KEY1                      CHAR(8) NOT NULL,           00004000
             NAME_KEY2                      CHAR(8) NOT NULL,           00004100
             NAME_KEY3                      CHAR(2) NOT NULL,           00004200
             ADDRESS_KEY1                   CHAR(24) NOT NULL,          00004300
             ASSOCIATION1                   CHAR(5),                    00004400
             ASSOCIATION2                   CHAR(5),                    00004500
             ASSOCIATION3                   CHAR(5),                    00004600
             FAX_AREA_CODE                  CHAR(3) NOT NULL,           00004700
             FAX_PHONE                      CHAR(7) NOT NULL,           00004800
             OFF_PRODUCERID                 CHAR(8) NOT NULL,           00004900
             EXTN_AGENCY_NUMBER             VARCHAR(12),                00005000
             PRIMRY_AFFL_CODE               CHAR(2),                    00005100
             SERVFEE_AGREE_IND              CHAR(1),                    00005200
             SERVFEE_AGREE_DATE             DATE,                       00005300
             EMAIL1                         CHAR(50),                   00005400
             EMAIL2                         CHAR(50),                   00005500
             PASSWORD                       CHAR(12),                   00005600
             CELLULAR_AREA_CODE             CHAR(3),                    00005700
             CELLULAR_PHONE                 CHAR(7),                    00005800
             INTEREST_CD                    CHAR(3),                    00005900
             PRIMRY_BUSINESS_CD             CHAR(4),                    00006000
             PRIMRY_BUS_REVS_DT             DATE,                       00006100
             SELLING_SM_GRP_YR              CHAR(4),                    00006200
             IN_INS_SINCE_YEAR              CHAR(4),                    00006300
             COUNTRY                        VARCHAR(30),                00006400
             FOREIGN_FLG                    CHAR(1),                    00006500
             RANKING_CD                     CHAR(2),                    00006600
             DO_NO_UPDATE                   CHAR(1),                    00006700
             TIN                            CHAR(9),                    00006800
             DOING_BUS_AS_NAME              CHAR(30),                   00006900
             CERTIFIED_IND                  CHAR(1),                    00007000
             EFT_ACCT_NUM                   CHAR(18),                   00007100
             TRANSIT_NUM                    CHAR(9),                    00007200
             EFT_ACCT_TYPE                  CHAR(1),                    00007300
             PAYEE_IND                      CHAR(1),                    00007400
             PAYMENT_IND                    CHAR(1)                     00007500
           ) END-EXEC.                                                  00007600
      ******************************************************************00007700
      * COBOL DECLARATION FOR TABLE AGNTNV05                           *00007800
      ******************************************************************00007900
       01  DCLAGNTNAME.                                                 00008000
           10 IDNTITY              PIC X(8).                            00008100
           10 LAST-NAME            PIC X(20).                           00008200
           10 FIRST-NAME           PIC X(15).                           00008300
           10 MIDDLE-NAME          PIC X(15).                           00008400
           10 PREFIX               PIC X(4).                            00008500
           10 SUFFIX1              PIC X(4).                            00008600
           10 SUFFIX2              PIC X(4).                            00008700
           10 COMPANY-IND          PIC X(1).                            00008800
           10 COMPANY-IN-ADDRESS   PIC X(1).                            00008900
           10 COMPANY-NAME         PIC X(30).                           00009000
           10 DISPLAY-NAME         PIC X(30).                           00009100
           10 NICKNAME             PIC X(10).                           00009200
           10 ADDRESS1             PIC X(30).                           00009300
           10 ADDRESS2             PIC X(30).                           00009400
           10 CITY                 PIC X(30).                           00009500
           10 STATE                PIC X(2).                            00009600
           10 ZIP                  PIC X(5).                            00009700
           10 ZIP-PLUS4            PIC X(4).                            00009800
           10 COUNTY-CODE          PIC X(5).                            00009900
           10 AREA-CODE            PIC X(3).                            00010000
           10 PHONE                PIC X(7).                            00010100
           10 PHONE-EXTENSION      PIC X(4).                            00010200
           10 SSN-NUM              PIC X(9).                            00010300
           10 SEX                  PIC X(1).                            00010400
           10 BIRTH-DATE           PIC X(10).                           00010500
           10 FINALST-REAS-CODE    PIC X(3).                            00010600
           10 FINALST-OVRD-IND     PIC X(1).                            00010700
           10 DUP-ADDR-OVRD-IND    PIC X(1).                            00010800
           10 EFFECTIVE-DATE       PIC X(10).                           00010900
           10 CHANGE-DATE          PIC X(26).                           00011000
           10 CHANGE-LOGON         PIC X(8).                            00011100
           10 ENTITY-TYPE          PIC X(2).                            00011200
           10 RECORD-STATUS        PIC X(1).                            00011300
           10 ALT-ADDRESS-IND      PIC X(1).                            00011400
           10 FUTURE-ADDRESS-IND   PIC X(1).                            00011500
           10 RECORD-ORIGIN        PIC X(1).                            00011600
           10 COMBINED-STATUS      PIC X(2).                            00011700
           10 SITE-CODE            PIC X(2).                            00011800
           10 NAME-KEY1            PIC X(8).                            00011900
           10 NAME-KEY2            PIC X(8).                            00012000
           10 NAME-KEY3            PIC X(2).                            00012100
           10 ADDRESS-KEY1         PIC X(24).                           00012200
           10 ASSOCIATION1         PIC X(5).                            00012300
           10 ASSOCIATION2         PIC X(5).                            00012400
           10 ASSOCIATION3         PIC X(5).                            00012500
           10 FAX-AREA-CODE        PIC X(3).                            00012600
           10 FAX-PHONE            PIC X(7).                            00012700
           10 OFF-PRODUCERID       PIC X(8).                            00012800
           10 EXTN-AGENCY-NUMBER   PIC X(12).                           00012900
           10 PRIMRY-AFFL-CODE     PIC X(2).                            00013000
           10 SERVFEE-AGREE-IND    PIC X(1).                            00013100
           10 SERVFEE-AGREE-DATE   PIC X(10).                           00013200
           10 EMAIL1               PIC X(50).                           00013300
           10 EMAIL2               PIC X(50).                           00013400
           10 PASS-WORD            PIC X(12).                           00013500
           10 CELLULAR-AREA-CODE   PIC X(03).                           00013600
           10 CELLULAR-PHONE       PIC X(07).                           00013700
           10 INTEREST-CD          PIC X(03).                           00013800
           10 PRIMRY-BUSINESS-CD   PIC X(04).                           00013900
           10 PRIMRY-BUS-REVS-DT   PIC X(10).                           00014000
           10 SELLING-SM-GRP-YR    PIC X(04).                           00014100
           10 IN-INS-SINCE-YEAR    PIC X(04).                           00014200
           10 COUNTRY              PIC X(30).                           00014300
           10 FOREIGN-FLG          PIC X(01).                           00014400
           10 RANKING-CD           PIC X(02).                           00014500
           10 DO-NO-UPDATE         PIC X(01).                           00014600
           10 TIN                  PIC X(09).                           00014700
           10 DOING-BUS-AS-NAME    PIC X(30).                           00014800
           10 CERTIFIED-IND        PIC X(01).                           00014900
           10 EFT-ACCT-NUM         PIC X(18).                           00015000
           10 TRANSIT-NUM          PIC X(9).                            00015100
           10 EFT-ACCT-TYPE        PIC X(1).                            00015200
           10 PAYEE-IND            PIC X(1).                            00015300
           10 PAYMENT-IND          PIC X(1).                            00015400
      ******************************************************************00015500
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 74      *00015600
      ******************************************************************00015700
      *---------------------------------------------------------------* 00015800
      * INDICATOR VARIABLES ARE USED TO CHECK FOR NULL VALUES. IF A   * 00015900
      * NULL VALUE IS RETURNED THE INDICATOR IS SET TO A NEGATIVE     * 00016000
      * VALUE.  USING NULLABLE FIELDS WITHOUT CHECKING THE INDICATOR  * 00016100
      * FOR THAT FIELDS CAN RAISE AN ERROR CONDITION OR YIELD         * 00016200
      * UNPREDICTABLE OR WRONG RESULTS.                               * 00016300
      *---------------------------------------------------------------* 00016400
       01  AGNTNAME-INDICATOR-AREA.                                     00016500
           10 AGNTNAME-INDICATORS  PIC S9(4) COMP OCCURS 74 TIMES.      00016600
       01  AGNTNAME-NULL-INDS REDEFINES AGNTNAME-INDICATOR-AREA.        00016700
           10 IND-IDNTITY                   PIC S9(4) COMP.             00016800
           10 IND-LAST-NAME                 PIC S9(4) COMP.             00016900
           10 IND-FIRST-NAME                PIC S9(4) COMP.             00017000
           10 IND-MIDDLE-NAME               PIC S9(4) COMP.             00017100
           10 IND-PREFIX                    PIC S9(4) COMP.             00017200
           10 IND-SUFFIX1                   PIC S9(4) COMP.             00017300
           10 IND-SUFFIX2                   PIC S9(4) COMP.             00017400
           10 IND-COMPANY-IND               PIC S9(4) COMP.             00017500
           10 IND-COMPANY-IN-ADDRESS        PIC S9(4) COMP.             00017600
           10 IND-COMPANY-NAME              PIC S9(4) COMP.             00017700
           10 IND-DISPLAY-NAME              PIC S9(4) COMP.             00017800
           10 IND-NICKNAME                  PIC S9(4) COMP.             00017900
           10 IND-ADDRESS1                  PIC S9(4) COMP.             00018000
           10 IND-ADDRESS2                  PIC S9(4) COMP.             00018100
           10 IND-CITY                      PIC S9(4) COMP.             00018200
           10 IND-STATE                     PIC S9(4) COMP.             00018300
           10 IND-ZIP                       PIC S9(4) COMP.             00018400
           10 IND-ZIP-PLUS4                 PIC S9(4) COMP.             00018500
           10 IND-COUNTY-CODE               PIC S9(4) COMP.             00018600
           10 IND-AREA-CODE                 PIC S9(4) COMP.             00018700
           10 IND-PHONE                     PIC S9(4) COMP.             00018800
           10 IND-PHONE-EXTENSION           PIC S9(4) COMP.             00018900
           10 IND-SSN                       PIC S9(4) COMP.             00019000
           10 IND-SEX                       PIC S9(4) COMP.             00019100
           10 IND-BIRTH-DATE                PIC S9(4) COMP.             00019200
           10 IND-FINALST-REAS-CODE         PIC S9(4) COMP.             00019300
           10 IND-FINALST-OVRD-IND          PIC S9(4) COMP.             00019400
           10 IND-DUP-ADDR-OVRD-IND         PIC S9(4) COMP.             00019500
           10 IND-EFFECTIVE-DATE            PIC S9(4) COMP.             00019600
           10 IND-CHANGE-DATE               PIC S9(4) COMP.             00019700
           10 IND-CHANGE-LOGON              PIC S9(4) COMP.             00019800
           10 IND-ENTITY-TYPE               PIC S9(4) COMP.             00019900
           10 IND-RECORD-STATUS             PIC S9(4) COMP.             00020000
           10 IND-ALT-ADDRESS-IND           PIC S9(4) COMP.             00020100
           10 IND-FUTURE-ADDRESS-IND        PIC S9(4) COMP.             00020200
           10 IND-RECORD-ORIGIN             PIC S9(4) COMP.             00020300
           10 IND-COMBINED-STATUS           PIC S9(4) COMP.             00020400
           10 IND-SITE-CODE                 PIC S9(4) COMP.             00020500
           10 IND-NAME-KEY1                 PIC S9(4) COMP.             00020600
           10 IND-NAME-KEY2                 PIC S9(4) COMP.             00020700
           10 IND-NAME-KEY3                 PIC S9(4) COMP.             00020800
           10 IND-ADDRESS-KEY1              PIC S9(4) COMP.             00020900
           10 IND-ASSOCIATION1              PIC S9(4) COMP.             00021000
           10 IND-ASSOCIATION2              PIC S9(4) COMP.             00021100
           10 IND-ASSOCIATION3              PIC S9(4) COMP.             00021200
           10 IND-FAX-AREA-CODE             PIC S9(4) COMP.             00021300
           10 IND-FAX-PHONE                 PIC S9(4) COMP.             00021400
           10 IND-OFF-PRODUCERID            PIC S9(4) COMP.             00021500
           10 IND-EXTN-AGENCY-NUMBER        PIC S9(4) COMP.             00021600
           10 IND-PRIMRY-AFFL-CODE          PIC S9(4) COMP.             00021700
           10 IND-SERVFEE-AGREE-IND         PIC S9(4) COMP.             00021800
           10 IND-SERVFEE-AGREE-DATE        PIC S9(4) COMP.             00021900
           10 IND-EMAIL1                    PIC S9(4) COMP.             00022000
           10 IND-EMAIL2                    PIC S9(4) COMP.             00022100
           10 IND-PASSWORD                  PIC S9(4) COMP.             00022200
           10 IND-CELLULAR-AREA-CODE        PIC S9(4) COMP.             00022300
           10 IND-CELLULAR-PHONE            PIC S9(4) COMP.             00022400
           10 IND-INTEREST-CD               PIC S9(4) COMP.             00022500
           10 IND-PRIMRY-BUSINESS-CD        PIC S9(4) COMP.             00022600
           10 IND-PRIMRY-BUS-REVS-DT        PIC S9(4) COMP.             00022700
           10 IND-SELLING-SM-GRP-YR         PIC S9(4) COMP.             00022800
           10 IND-IN-INS-SINCE-YEAR         PIC S9(4) COMP.             00022900
           10 IND-COUNTRY                   PIC S9(4) COMP.             00023000
           10 IND-FOREIGN-FLG               PIC S9(4) COMP.             00023100
           10 IND-RANKING-CD                PIC S9(4) COMP.             00023200
           10 IND-DO-NO-UPDATE              PIC S9(4) COMP.             00023300
           10 IND-TIN                       PIC S9(4) COMP.             00023400
           10 IND-DOING-BUS-AS-NAME         PIC S9(4) COMP.             00023500
           10 IND-CERTIFIED-IND             PIC S9(4) COMP.             00023600
           10 IND-EFT-ACCT-NUM              PIC S9(4) COMP.             00023700
           10 IND-TRANSIT-NUM               PIC S9(4) COMP.             00023800
           10 IND-EFT-ACCT-TYPE             PIC S9(4) COMP.             00023900
           10 IND-PAYEE-IND                 PIC S9(4) COMP.             00024000
           10 IND-PAYMENT-IND               PIC S9(4) COMP.             00024100
      ******************************************************************00024200
      * FIELD PREFIXED COBOL DECLARATION FOR AGNTNAME.                 *00024300
      ******************************************************************00024400
       01  WR-AGNTNAME-RECORD.                                          00024500
           10 WR-AGNT-IDNTITY              PIC X(8).                    00024600
           10 WR-AGNT-LAST-NAME            PIC X(20).                   00024700
           10 WR-AGNT-FIRST-NAME           PIC X(15).                   00024800
           10 WR-AGNT-MIDDLE-NAME          PIC X(15).                   00024900
           10 WR-AGNT-PREFIX               PIC X(4).                    00025000
           10 WR-AGNT-SUFFIX1              PIC X(4).                    00025100
           10 WR-AGNT-SUFFIX2              PIC X(4).                    00025200
           10 WR-AGNT-COMPANY-IND          PIC X(1).                    00025300
           10 WR-AGNT-COMPANY-IN-ADDRESS   PIC X(1).                    00025400
           10 WR-AGNT-COMPANY-NAME         PIC X(30).                   00025500
           10 WR-AGNT-DISPLAY-NAME         PIC X(30).                   00025600
           10 WR-AGNT-NICKNAME             PIC X(10).                   00025700
           10 WR-AGNT-ADDRESS1             PIC X(30).                   00025800
           10 WR-AGNT-ADDRESS2             PIC X(30).                   00025900
           10 WR-AGNT-CITY                 PIC X(30).                   00026000
           10 WR-AGNT-STATE                PIC X(2).                    00026100
           10 WR-AGNT-ZIP                  PIC X(5).                    00026200
           10 WR-AGNT-ZIP-PLUS4            PIC X(4).                    00026300
           10 WR-AGNT-COUNTY-CODE          PIC X(5).                    00026400
           10 WR-AGNT-AREA-CODE            PIC X(3).                    00026500
           10 WR-AGNT-PHONE                PIC X(7).                    00026600
           10 WR-AGNT-PHONE-EXTENSION      PIC X(4).                    00026700
           10 WR-AGNT-SSN                  PIC X(9).                    00026800
           10 WR-AGNT-SEX                  PIC X(1).                    00026900
           10 WR-AGNT-BIRTH-DATE           PIC X(10).                   00027000
           10 WR-AGNT-FINALST-REAS-CODE    PIC X(3).                    00027100
           10 WR-AGNT-FINALST-OVRD-IND     PIC X(1).                    00027200
           10 WR-AGNT-DUP-ADDR-OVRD-IND    PIC X(1).                    00027300
           10 WR-AGNT-EFFECTIVE-DATE       PIC X(10).                   00027400
           10 WR-AGNT-CHANGE-DATE          PIC X(26).                   00027500
           10 WR-AGNT-CHANGE-LOGON         PIC X(8).                    00027600
           10 WR-AGNT-ENTITY-TYPE          PIC X(2).                    00027700
           10 WR-AGNT-RECORD-STATUS        PIC X(1).                    00027800
           10 WR-AGNT-ALT-ADDRESS-IND      PIC X(1).                    00027900
           10 WR-AGNT-FUTURE-ADDRESS-IND   PIC X(1).                    00028000
           10 WR-AGNT-RECORD-ORIGIN        PIC X(1).                    00028100
           10 WR-AGNT-COMBINED-STATUS      PIC X(2).                    00028200
           10 WR-AGNT-SITE-CODE            PIC X(2).                    00028300
           10 WR-AGNT-NAME-KEY1            PIC X(8).                    00028400
           10 WR-AGNT-NAME-KEY2            PIC X(8).                    00028500
           10 WR-AGNT-NAME-KEY3            PIC X(2).                    00028600
           10 WR-AGNT-ADDRESS-KEY1         PIC X(24).                   00028700
           10 WR-AGNT-ASSOCIATION1         PIC X(5).                    00028800
           10 WR-AGNT-ASSOCIATION2         PIC X(5).                    00028900
           10 WR-AGNT-ASSOCIATION3         PIC X(5).                    00029000
           10 WR-AGNT-FAX-AREA-CODE        PIC X(3).                    00029100
           10 WR-AGNT-FAX-PHONE            PIC X(7).                    00029200
           10 WR-AGNT-OFF-PRODUCERID       PIC X(8).                    00029300
           10 WR-AGNT-EXTN-AGENCY-NUMBER   PIC X(12).                   00029400
           10 WR-AGNT-PRIMRY-AFFL-CODE     PIC X(2).                    00029500
           10 WR-AGNT-SERVFEE-AGREE-IND    PIC X(1).                    00029600
           10 WR-AGNT-SERVFEE-AGREE-DATE   PIC X(10).                   00029700
           10 WR-AGNT-EMAIL1               PIC X(50).                   00029800
           10 WR-AGNT-EMAIL2               PIC X(50).                   00029900
           10 WR-AGNT-PASSWORD             PIC X(12).                   00030000
           10 WR-AGNT-CELLULAR-AREA-CODE   PIC X(03).                   00030100
           10 WR-AGNT-CELLULAR-PHONE       PIC X(07).                   00030200
           10 WR-AGNT-INTEREST-CD          PIC X(03).                   00030300
           10 WR-AGNT-PRIMRY-BUSINESS-CD   PIC X(04).                   00030400
           10 WR-AGNT-PRIMRY-BUS-REVS-DT   PIC X(10).                   00030500
           10 WR-AGNT-SELLING-SM-GRP-YR    PIC X(04).                   00030600
           10 WR-AGNT-IN-INS-SINCE-YEAR    PIC X(04).                   00030700
           10 WR-AGNT-COUNTRY              PIC X(30).                   00030800
           10 WR-AGNT-FOREIGN-FLG          PIC X(01).                   00030900
           10 WR-AGNT-RANKING-CD           PIC X(02).                   00031000
           10 WR-AGNT-DO-NO-UPDATE         PIC X(01).                   00031100
           10 WR-AGNT-TIN                  PIC X(09).                   00031200
           10 WR-AGNT-DOING-BUS-AS-NAME    PIC X(30).                   00031300
           10 WR-AGNT-CERTIFIED-IND        PIC X(01).                   00031400
           10 WR-EFT-ACCT-NUM              PIC X(18).                   00031500
           10 WR-TRANSIT-NUM               PIC X(09).                   00031600
           10 WR-EFT-ACCT-TYPE             PIC X(01).                   00031700
           10 WR-PAYEE-IND                 PIC X(01).                   00031800
           10 WR-PAYMENT-IND               PIC X(01).                   00031900
