   CBL DATA(24)                                                         00000100
900526*---------------------------------------------------------------* 00000200
900526* DATA(24) OPTION IS FOR ABOVE-TO-BELOW-THE-LINE DYNAMIC CALLS  * 00000300
900526*---------------------------------------------------------------* 00000400
       IDENTIFICATION DIVISION.                                         00000500
       PROGRAM-ID. NA340.                                               00000600
 INFO *---------------START OF INFORMATION BLOCK----------------------* 00000700
 INFO *---------------------------------------------------------------* 00000800
 INFO * PLACE AN 'X' NEXT TO THE OPTION THAT MATCHES YOUR NEEDS FOR   * 00000900
 INFO * THIS PROGRAM.                                                 * 00001000
 INFO *---------------------------------------------------------------* 00001100
 INFO *               COMPILE INFORMATION BLOCK                       * 00001200
 INFO *                                                               * 00001300
 INFO *   BATCH                                                       * 00001400
 INFO * _ COBOL II BATCH                                              * 00001500
 INFO * _ HLASM    BATCH                                              * 00001600
 INFO *                                                               * 00001700
 INFO *   DB2                                                         * 00001800
 INFO * _ COBOL II DB2 BATCH                                          * 00001900
 INFO * X COBOL II DB2 CICS                                           * 00002000
 INFO * _ COBOL II DB2 EXCI                                           * 00002100
 INFO *                                                               * 00002200
 INFO *   PKLIST                                                      * 00002300
 INFO * X ADMIN      X ONLINE  _ BATCH                                * 00002400
 INFO * _ BILLING    _ ONLINE  _ BATCH                                * 00002500
 INFO * _ CLAIMS     _ ONLINE  _ BATCH                                * 00002600
 INFO * _ FINANCE    _ ONLINE  _ BATCH                                * 00002700
 INFO * _ RATING     _ ONLINE  _ BATCH                                * 00002800
 INFO * _ SYBASE     _ ONLINE  _ BATCH                                * 00002900
 INFO * _ TECHGRP    _ ONLINE  _ BATCH                                * 00003000
 INFO * _ TEST       _ ONLINE  _ BATCH                                * 00003100
 INFO *                                                               * 00003200
 INFO *   CICS                                                        * 00003300
 INFO * _ COBOL II CICS                                               * 00003400
 INFO * _ COBOL II EXCI                                               * 00003500
 INFO * _ HLASM    CICS                                               * 00003600
 INFO *                                                               * 00003700
 INFO *------------------END OF INFORMATION BLOCK---------------------* 00003800
           EJECT                                                        00003900
       DATE-WRITTEN. 03/22/88.                                          00004000
      *REMARKS. NA340 IS THE CUSTOMER INFORMATION UPDATE PROGRAM. THIS  00004100
      *         PROGRAM UPDATES NAME AND ADDRESS ENTRIES ON THE AGNTNAME00004200
      *         TABLE.                                                  00004300
      *                                                                 00004400
      *------------------------PROGRAM PURPOSE-------------------------*00004500
      *                                                                *00004600
      *  PROGRAM TITLE: NA340                                          *00004700
      *  PROGRAM TEXT:                                                 *00004800
      *                                                                *00004900
      *------------------------PROGRAM CHANGES-------------------------*00005000
      *                                                                *00005100
      *  DATE      BY     CHANGE DESCRIPTION                   RSA/SIR *00005200
      * --------  ----   -----------------------------------   ------- *00005300
DFJ   * 11/07/88  DFJ    UPDATE CAM TABLES (STATE,ZIP,STATUS)  870630  *00005400
      * 12/29/88  SPC    RECOMPILE FOR JOURNALING              881562  *00005500
      * 01/06/89  SPC    ADJUST COMMAREA LENGTH FOR BROKER ADD 89020005*00005600
      * 02/15/89  SPC    PREVENT ADDRESS FROM BEING SPACES     89010382*00005700
      * 05/05/89  SPC    FORCE AT LEAST ONE CHECK OF FINALIST  890117  *00005800
      *                  AND LINK TO NA203 INSTEAD OF NA202    890117  *00005900
      * 05/10/89  EMG    REMOVE ATTRB SETS AFTER FIXING SCREEN 890499  *00006000
      * 05/23/89  GRB    STOP THE SPACING OUT OF ADDRESS-KEY1  890533  *00006100
      * 09/07/89  JDK    ADD EDITS FOR CUSTOMER STATUS         890885  *00006200
      * 09/21/89  JAR    FIX FOR STATE CHG (CAMASV01 UPDATE)   89090315*00006300
      * 11/08/89  CRF    ADD EDITS FOR CUSTOMER TYPE           891018  *00006400
      * 02/13/89  GRB    ADDED BRIDGE OVER TO VSAM CASE SYSTEM 890533  *00006500
      * 05/18/90  SPC    PROTECT YERKE AGENTS                  000000  *00006600
      * 06/04/90  JAR    CHANGE CATBVIEW LOOK UP TO CASEXV01   90060045*00006700
      * 06/20/90  RCG    UNPROTECT ASSOC CODE 3 FOR YERKE AGTS 90060378*00006800
      * 07/02/90  RCG    PROTECT ASSOC CODE 3 FOR YERKE AGENTS   N/A   *00006900
      * 07/18/90  JAR    UPDTE CATMASTR W/CURR DTE WHEN APPRO. 900678  *00007000
      * 09/20/90  CWL    FAX PHONE NUMBER                      900837  *00007100
      * 03/04/91  MAB    CONVERT TO ABOVE THE LINE, AND        900526  *00007200
      *                  VERSION 3.1 TUTORIAL.                    "    *00007300
      * 08/26/91  LMB    APPLY STATE/ZIP CHANGES TO EMPLOYEES  890989  *00007400
      * 08/27/91  LMB    CORRECT CASE NAME ACCEPTABLE INDICATOR91080227*00007500
      * 09/26/91  LMB    MAP TUTORIAL FOR CASENAME TO MSTR CHG 91090363*00007600
      * 09/26/91  LMB    UPDT EMP STATE/ZIP IF SPACES          91090333*00007700
      * 10/02/91  LMB    HANDLE NEW TYPE 'LB' LIST BILL        910412  *00007800
      * 10/10/91  LMB    WRITE 'EG' AUDIT CODE FOR EMP ADDRESS 91100170*00007900
      * 12/11/91  MAB    ADD 1 BYTE TO THE WS-BROKER-FIELDS    91120086*00008000
      *   "        "     AND WS-BROKER-LENGTH                     "    *00008100
      * 12/12/91  LMB    MODS FOR FINALIST RELEASE 6.0         910687  *00008200
      * 01/14/92  LMB    REMOVE YERKE/NBRL EDITS             92010123  *00008300
      * 01/28/92  KAW    ADD NEW BILL ADDR (BA) ENTITY TYPE    920124  *00008400
      * 02/25/92  CWL    REMOVE PROTECTION OF ASSOC. 'FREDM'   920124***00008500
      * 01/11/93  GRB    DISCONTINUE AGNTNAME UPDATE 'PREQUOTE 930403***00008600
      * 04/22/93  LCP    ROLLBACK DB2 WHEN CASE FILE CLOSED  93040154***00008700
      * 07/12/96  SYK    ADD LEAD ACTION TYPE FOR STAPLES      960530***00008800
      * 10/10/96  SYK    ADD REP  ACTION TYPE FOR ADP          961037***00008900
      * 11/07/97  GRB    CASE NUMBER CONVERSION                970614  *00009000
      * 07/16/98  MCS    ADD MODULE RQ09999 FOR BILL REQUESTS  980714  *00009100
      * 01/15/99  TDA    ADD SYNCPOINT AFTER DB2 UPDATES       990115  *00009200
990420* 04/20/99  KXL    CHANGE WS-EMP-NUM TO EMP-EMPLOYEE-NUM 990420  *00009300
990420*                  LINE 004447.                          990420  *00009400
      *----------------------------------------------------------------*00009500
                                                                        00009600
Y2KIMR******************************************************************00009700
Y2KIMR* PROJECT NUMBER: SE-NI-HP-3811-02-03                            *00009800
Y2KIMR* PROJECT NAME: HPS DAGGER PROJECT PHASE I                       *00009900
Y2KIMR* PROGRAMMER: IMR011 + IMR051                                    *00010000
Y2KIMR*                                                                *00010100
Y2KIMR* 11/1998 - CHANGES MADE FOR YEAR 2000 COMPLIANCE - IMR          *00010200
Y2KIMR*                                                                *00010300
Y2KIMR* 1. DATA DIVISION CHANGES -                                     *00010400
Y2KIMR*    Y2K-WK-DATE1-9; Y2K-WK-DATE2-9; Y2K-WK-CUTOFF-YR-X          *00010500
Y2KIMR*    Y2K-WK-DATE1-X; Y2K-WK-CUTOFF-YR-X WS-DB2-CC-R              *00010600
Y2KIMR*    Y2K-WK-VARIABLE1 Y2K-WK-VARIABLE3 Y2K-WK-DATE-TEN           *00010700
Y2KIMR*                                                                *00010800
Y2KIMR*    COM-BIRTH-DATE, WSQ-BIRTH-DATE EXPANDED                     *00010900
Y2KIMR*                                                                *00011000
Y2KIMR* 2. PROCEDURE DIVISION CHANGES -                                *00011100
Y2KIMR*    IMR CHANGE 1 IN EDIT ASSOCIATION CODE1 FOR VALIDITY         *00011200
Y2KIMR*    WS-COMPARE-DATE; WT-C0043-ASSOC-TERM-DATE                   *00011300
Y2KIMR*    IMR CHANGE 2 IN EDIT ASSOCIATION CODE2 FOR VALIDITY         *00011400
Y2KIMR*    WS-COMPARE-DATE; WT-C0043-ASSOC-TERM-DATE                   *00011500
Y2KIMR*    IMR CHANGE 3 IN EDIT ASSOCIATION CODE3 FOR VALIDITY         *00011600
Y2KIMR*    WS-COMPARE-DATE; WT-C0043-ASSOC-TERM-DATE                   *00011700
Y2KIMR*    IMR CHANGE 4 IN 0230-MOVE-COMMAREA-TO-TABLE                 *00011800
Y2KIMR*    EF-CC; WS-YY-A; WS-MM-A; WS-DD-A                            *00011900
Y2KIMR*    IMR CHANGE 5 IN 0230-MOVE-COMMAREA-TO-TABLE                 *00012000
Y2KIMR*    BR-CC                                                       *00012100
Y2KIMR*    IMR CHANGE 6 IN 0120-UPDATE-CIM-MAP-FIELDS                  *00012200
Y2KIMR*    Y2K-WK-VARIABLE1                                            *00012300
Y2KIMR*    IMR CHANGE 7 IN 0280-MOVE-SPACES-TO-MAP                     *00012400
Y2KIMR*    BIRTH-DATE MAP-BIRTH-DATEO                                  *00012500
Y2KIMR*    IMR CHANGE 8 IN 0290-MOVE-OUT-MAP-FIELDS                    *00012600
Y2KIMR*    COM-BIRTH-DATE MAP-BIRTH-DATEO                              *00012700
Y2KIMR******************************************************************00012800
Y2KIMR* -------------------------------------------------------------- *00012900
Y2KIMR*  12/1998 - Y2K COMPLIANCE: CHANGES TO CALL Y2K COMPLIANT DATE  *00013000
Y2KIMR*            ROUTINES - IMRGLOBAL                                *00013100
Y2KIMR* -------------------------------------------------------------- *00013200
Y2KIMR*                                                                *00013300
Y2KIMR* PROJECT ID: SE-NI-HP-3811-02-03                                *00013400
Y2KIMR* PROJECT NAME: HPS DAGGER PROJECT PHASE I                       *00013500
Y2KIMR* PROGRAMMER: IMR042                                             *00013600
Y2KIMR*                                                                *00013700
Y2KIMR* NAME OF DATE ROUTINE CHANGED - DATEPRG2                        *00013800
Y2KIMR* NAME OF Y2K COMPLIANT ROUTINE - DTEPROG2                       *00013900
Y2KIMR* NAME OF WS VARIABLES CHANGED - WS-DATEPRG2-PARAMETERS          *00014000
Y2KIMR* NAME OF WS VARIABLES CHANGED - COB2-DATEPRG2                   *00014100
Y2KIMR* NAME OF WS COPYBOOKS CHANGED - DATEPARM                        *00014200
Y2KIMR* PROCEDURE CHANGES - REF ISSUE 142                              *00014300
Y2KIMR*                     0120-UPDATE-CIM-MAP-FIELDS                 *00014400
Y2KIMR*                                                                *00014500
990428* MCS  UNPACK COM-BIRTH-DATE BEFORE CALL TO DTEPROG2              00014600
990507* MCS  ADD '1111-11-11' TO DATE CHECK FOR Y2K                     00014700
990513* MCS  REMOVE DATE CHECK AND CHECK FOR ENTITY TYPE                00014800
COBOLU* IMR  IOMOD CALL CHANGES                                         00014900
990721* IMR  COBOL/CICS UPGRADE                                         00015000
      * 11/23/99  TDA  ADD CODE TO USE GROUP1 FOR VALIDATE ADDR  COBOLU*00015100
COBOLU* 12/15/99 MCS  COBOL/CICS UPGRADE                               *00015200
COBOLU* 12/17/99 MCS  FIX WS-ERROR-FIELDS                              *00015300
COBOLU* 12/17/99 MCS  FIX INVALID CUSTOMER TYPE ERROR                  *00015400
      * 12/30/99  TDA ATTEMPT TO DEBUG INVALID CUST TYPE ERROR   C08784*00015500
R00946* 06/23/00 MCW EXPAND MKT CODE                                    00015600
R00946* 07/21/00 MCW RECOMPILE FOR NEW NAGROUP1                         00015700
R00700* 09/06/00 KXL ADD EMAIL TO THE SCREEN AND TEST                   00015800
R00700* 09/06/00 KXL ADD IND-PREFIX                                     00015900
C23311* 09/26/00 KXL ADD IND-PREFIX                                     00016000
R01142* 12/10/00 JXV RECOMPILE FOR MAP CHANGE. ALLOW ONLY X(22)         00016100
R01142*              FOR ADDR 1 AND ADDR 2 BECAUSE CASE VSAM USES X(22).00016200
R01142*              ALSO ADD A ':' BEFORE HOST VARIABLES DCLAGNTNAME   00016300
R01142*              AND CAT-MASTER-REC FOR DB2 VERSION 6 REQUIREMENT.  00016400
R01142* 12/12/00 JXV IF NAGROUP1 DEFAULTS VALUES IN ADDR 1 OR ADDR2     00016500
R01142*              THAT EXTENDS PAST THE 22 BYTE LIMIT THEN           00016600
R01142*              SEND A WARNING TO THE USER SO THEY WILL KNOW.      00016700
R01142*              ADD PF6 SAME AS PF18.                              00016800
001220* 12/20/00 JXV HANDLE DUP LOGONID, JUST DISPLAY LOGON ID          00016900
001220*              INSTEAD OF CICS ERROR SCREEN.                      00017000
R01213* 02/01/01 JXE ADD LOGIC TO OVERRIDE ADDRESS CHECKER WHEN THE     00017100
R01213*              RETURNED ADDRESS IS DIFFERENT/TRUNCATED BUT VALID  00017200
R01213*              (NOT GETTING REASON CODE 9)                        00017300
R01717* 12/12/01 JXV ADD CARRIER SECURITY USING SECCHECK                00017400
R01873* 07/29/02 LLM ALLOW PLATYPUS CARRIER '9D' LISTBILLS TO UPDT VSAM 00017500
R01717* 09/04/02 JXJ PASS CASE NUMBER TO AUDIT PROGRAM.                 00017600
R01873* 09/10/02 LLM ALLOW CICSHADU TO LINK TO NAGROUP1                 00017700
R02539* 05/09/03 MXH ADD COUNTRY_CODE AND POSTAL_CODE TO ALLOW FOREIGN  00017800
R02539*              ADDRESS.                                           00017900
R03179* 03/22/04 JXV EMAIL ADDRESS ON CASE MUST BE PROTECTED BECAUSE IT 00018000
R03179*              CAN NOW ONLY BE CHANGED VIA THE CEDAR SYSTEM.      00018100
R78581* 03/30/04 HXM TEMP COMMENT OUT CHANGES R03179. AS PER TAMIKO     00018200
R78581*              CHANGES SHOULD NOT BE IN EFFECT YET.               00018300
R78581*              ALL LINE WITH R03179 MARKER WILL BE COMMENTED      00018400
R78581*              PLEASE REMOVE COMMENTS TO RESTORE CODE.            00018500
R03179* 04/21/04 JXV RESTORE R03179 TO PROTECT EMAIL ADDRESS ON CASE    00018600
T3384 * 05/26/04 HXM ADD PREFIX TO NAME PROCESS AVMA REQUIREMENT        00018700
R03749* 02/17/05 JXV 'HU' PRODUCT NUMBER/ZIP EDIT - FOR FLOA NETWORK    00018800
R03749*               PREVENT ADDRESS CHANGE IF ZIP GOES TO  NON-FLOA.  00018900
R03749*               ADD CALL TO EDIT MODULE GE01600 FOR TURBO 222,    00019000
R03749*               AND RETRIEVE DATA TO PASS DEPENDING ON WHETHER    00019100
R03749*               THIS IS A CAM OR A CASE ADDRESS CHANGE.           00019200
R04023* 06/09/05 RJR ADD ISSUE STATE FOR AIG                            00019300
R04254* 08/19/05 SXA ADD CARRIER 'SV' - AIG VISION                      00019400
R04281* 08/19/05 SXA ADD CARRIER 'MN' - AIG MINI MED                    00019500
R04280* 08/30/05 SXA ADD CARRIER 'SP' - AIG GROUP CRITICAL ILLNESS      00019600
RA4254* 09/01/05 SXA EDIT ISSUE STATE FOR 'SV'                          00019700
107987* 10/18/05 PXJ ZIP CODE EDIT                                      00019800
R04472* 10/30/05 JZG ADD CARRIER 'IG' - AIG FRAUD SAFEGUARD             00019900
RA3749* 11/11/05 LLA USE GE01600 ONLY FOR NA401 (109129)                00020000
110611* 11/30/05 LLA CHANGE CURSOR UPDT-CASE-CUR TO FETCH INTO CORRECT  00020100
110611*              RECORD                                             00020200
R04209* 05/12/06 JXV ADD AIG-AG CARRIERS LIKE 'SH'.                     00020300
R04209*              ALSO DONT GENERATE MR/MS ON DISPLAY-NAME FOR AIG-AG00020400
R04209*              EVEN THOUGH SEX IS STORED.                         00020500
R04209*              ALSO WHEN COMPANY-NAME IS BLANK USE DISPLAY-NAME   00020600
R04209*              WHEN UPDATING CASE VSAM.                           00020700
R04342* 05/15/06 SXP ADD ACMISSUE STATE TO AUDIT RECORD                 00020800
R04832* 06/21/06 JXV NEW CIGNA CARRIERS                                 00020900
R04934* 09/21/06  AXS   ADD AG NY WORK-SITE CARRIERS ('NO' 'NU' 'NX')  *00021000
R8285A* 06/15/09  EMV   PREVENT STATE CHANGE OUT OF IL IF PRODUCT# 0009*00021100
R08986* 07/10/10  JRD   CHANGES FOR 10.7 HUMANA RELEASE PHP REFRESH     00021200
R08986*                 PHASE 2 AND TEXAS MANDATEAND TO                 00021300
R08986*                 CHANGE FOR 10.7 HUMANA RELEASE PHP REFRESH      00021400
R08986*                 PHASE 2 TO EDIT FOR MATERNITY HEALTH OPTION 6   00021500
R08986*                 WITH CHANGE OF ADDRESS                          00021600
R9031A* 09/18/10  TRT   HUMANA 10.09 PWR1054 RESET EMAIL STATUS WHEN   *00021700
R9031A*                 EMAIL1 ADDRESS HAS BEEN ENTERED.               *00021800
R9470A* 01/22/11  TRT   HUMANA 11.01 PWR1039 #9470 - NEW FL PHP-R      *00021900
R9470A*                 NETWORKS 0012 & 0013.                          *00022000
R9470A*                 DISPLAY A WARNING MESSAGE WHEN NEW FL PHP-R    *00022100
R9470A*                 POLICIES (IN NETWORK 0013 ONLY) TRY AND MOVE   *00022200
R9470A*                 OUT OF THE DESIGNATED ZIP CODES FOR            *00022300
R9470A*                 NETWORK 0013.                                  *00022400
167949* 11/01/11  JXI   CHANGE CALL TO NAGROUP1 TO CADDR, add CICSHADL *00022500
R11386* 04/04/13  KNA  ADDED NEW HUMANA'S CICS REGIONS TO CALL CADDR   *00022600
R11386* 04/25/13  RCH  PASSING THE CIM NUMBER WHILE CALLING GE01600    *00022700
11735A* 09/18/13  RAI  PSCR-11735 USE BASE TABLE NAMES INSTEAD OF VIEWS*00022800
11735A* 09/18/13       ALSO CHANGE 'SELECT *' TO USE ACTUAL COLUMN NAME*00022900
00504A* 10/29/13  RAI  SIT DEFECT-504 CHANGES TO AVOID SQLCODE -305    *00023000
289843* 02/11/13  RAI  IMS289843-code change to avoid -304             *00023100
      *****LAST CHANGE**************************************************00023200
       ENVIRONMENT DIVISION.                                            00023300
       DATA DIVISION.                                                   00023400
       WORKING-STORAGE SECTION.                                         00023500
R9031A 01  WS-EMAIL-STATUS             PIC X(01) VALUE SPACE.           00023600
COBOLU 01  WS-ISEIB-ROUTINE            PIC X(8) VALUE 'ISEIB   '.       00023700
COBOLU 01  WS-PSIGSFC                  PIC X(8) VALUE 'PSIGSFC '.       00023800
R08986 01  WS-PROGRAM-NAME             PIC X(8) VALUE 'NA340   '.       00023900
Y2KIMR*                                                                 00024000
Y2KIMR* IMR CHANGE BEGIN                                                00024100
Y2KIMR*                                                                 00024200
Y2KIMR 01  Y2K-WK-CUTOFF-YR-X               PIC X(2) VALUE '50'.        00024300
Y2KIMR*                                                                 00024400
Y2KIMR 01  Y2K-WK-DATE1-X.                                              00024500
Y2KIMR     05  Y2K-WK-DATE1-X-CC            PIC X(2).                   00024600
Y2KIMR     05  Y2K-WK-DATE1-X-YYMMDD        PIC X(6).                   00024700
Y2KIMR     05  Y2K-WK-DATE1R-X-YYMMDD REDEFINES Y2K-WK-DATE1-X-YYMMDD.  00024800
Y2KIMR         10  Y2K-WK-DATE1R-X-YY       PIC X(2).                   00024900
Y2KIMR         10  Y2K-WK-DATE1R-X-MM       PIC X(2).                   00025000
Y2KIMR         10  Y2K-WK-DATE1R-X-DD       PIC X(2).                   00025100
Y2KIMR*                                                                 00025200
Y2KIMR 01  Y2K-WK-DATE1RR-X REDEFINES Y2K-WK-DATE1-X.                   00025300
Y2KIMR     05  Y2K-WK-DATE1RR-X-CCYY        PIC X(4).                   00025400
Y2KIMR     05  Y2K-WK-DATE1RR-X-MMDD        PIC X(4).                   00025500
Y2KIMR*                                                                 00025600
Y2KIMR 01  Y2K-WK-DATE1RRR-X REDEFINES Y2K-WK-DATE1-X PIC X(8).         00025700
Y2KIMR*                                                                 00025800
Y2KIMR 01  Y2K-WK-CUTOFF-YR-9               PIC 9(2) VALUE 50.          00025900
Y2KIMR*                                                                 00026000
Y2KIMR 01  Y2K-WK-DATE1-9.                                              00026100
Y2KIMR     05  Y2K-WK-DATE1-9-CC            PIC 9(2).                   00026200
Y2KIMR     05  Y2K-WK-DATE1-9-YYMMDD        PIC 9(6).                   00026300
Y2KIMR     05  Y2K-WK-DATE1R-9-YYMMDD REDEFINES Y2K-WK-DATE1-9-YYMMDD.  00026400
Y2KIMR         10  Y2K-WK-DATE1R-9-YY       PIC 9(2).                   00026500
Y2KIMR         10  Y2K-WK-DATE1R-9-MM       PIC 9(2).                   00026600
Y2KIMR         10  Y2K-WK-DATE1R-9-DD       PIC 9(2).                   00026700
Y2KIMR*                                                                 00026800
Y2KIMR 01  Y2K-WK-DATE1RR-9 REDEFINES Y2K-WK-DATE1-9.                   00026900
Y2KIMR     05  Y2K-WK-DATE1RR-9-CCYY        PIC 9(4).                   00027000
Y2KIMR     05  Y2K-WK-DATE1RR-9-MMDD        PIC 9(4).                   00027100
Y2KIMR*                                                                 00027200
Y2KIMR 01  Y2K-WK-DATE1RRR-9 REDEFINES Y2K-WK-DATE1-9 PIC 9(8).         00027300
Y2KIMR*                                                                 00027400
Y2KIMR 01  Y2K-WK-DATE2-9.                                              00027500
Y2KIMR     05  Y2K-WK-DATE2-9-CC            PIC 9(2).                   00027600
Y2KIMR     05  Y2K-WK-DATE2-9-YYMMDD        PIC 9(6).                   00027700
Y2KIMR     05  Y2K-WK-DATE2R-9-YYMMDD REDEFINES Y2K-WK-DATE2-9-YYMMDD.  00027800
Y2KIMR         10  Y2K-WK-DATE2R-9-YY       PIC 9(2).                   00027900
Y2KIMR         10  Y2K-WK-DATE2R-9-MM       PIC 9(2).                   00028000
Y2KIMR         10  Y2K-WK-DATE2R-9-DD       PIC 9(2).                   00028100
Y2KIMR*                                                                 00028200
Y2KIMR 01  Y2K-WK-DATE2RR-9 REDEFINES Y2K-WK-DATE2-9.                   00028300
Y2KIMR     05  Y2K-WK-DATE2RR-9-CCYY        PIC 9(4).                   00028400
Y2KIMR     05  Y2K-WK-DATE2RR-9-MMDD        PIC 9(4).                   00028500
Y2KIMR*                                                                 00028600
Y2KIMR 01  Y2K-WK-DATE2RRR-9 REDEFINES Y2K-WK-DATE2-9 PIC 9(8).         00028700
Y2KIMR*                                                                 00028800
Y2KIMR 01 Y2K-WK-VARIABLE1.                                             00028900
Y2KIMR    05 FILLER                         PIC X(02).                  00029000
Y2KIMR    05 Y2K-WK-VARIABLE2               PIC 9(08).                  00029100
Y2KIMR    05 Y2K-WK-VARIABLER  REDEFINES  Y2K-WK-VARIABLE2.             00029200
Y2KIMR       10 Y2K-WK-VARIABLE2-MM         PIC 9(02).                  00029300
Y2KIMR       10 Y2K-WK-VARIABLE2-DD         PIC 9(02).                  00029400
Y2KIMR       10 Y2K-WK-VARIABLE2-CC         PIC 9(02).                  00029500
Y2KIMR       10 Y2K-WK-VARIABLE2-YY         PIC 9(02).                  00029600
Y2KIMR*                                                                 00029700
Y2KIMR 01 Y2K-WK-VARIABLE3.                                             00029800
Y2KIMR    05 Y2K-WK-VARIABLE3-AA            PIC 9(02).                  00029900
Y2KIMR    05 Y2K-WK-VARIABLE3-BB.                                       00030000
Y2KIMR       10 Y2K-WK-VARIABLE3-BB-MM      PIC 9(02).                  00030100
Y2KIMR       10 Y2K-WK-VARIABLE3-BB-DD      PIC 9(02).                  00030200
Y2KIMR       10 Y2K-WK-VARIABLE3-BB-YY      PIC 9(02).                  00030300
Y2KIMR*                                                                 00030400
Y2KIMR     05  Y2K-WK-DATE-TEN.                                         00030500
Y2KIMR         10  Y2K-WK-MONTH                 PIC 99     VALUE ZERO.  00030600
Y2KIMR         10  Y2K-WK-SLASH-1               PIC X.                  00030700
Y2KIMR         10  Y2K-WK-DAY                   PIC 99     VALUE ZERO.  00030800
Y2KIMR         10  Y2K-WK-SLASH-2               PIC X.                  00030900
Y2KIMR         10  Y2K-WK-CENT                  PIC 99     VALUE ZERO.  00031000
Y2KIMR         10  Y2K-WK-YEAR                  PIC 99     VALUE ZERO.  00031100
Y2KIMR*                                                                 00031200
Y2KIMR* IMR CHANGE END                                                  00031300
Y2KIMR*                                                                 00031400
       77  WS-LINK-LENGTH                   PIC S9(8) VALUE +0    COMP. 00031500
       77  WS-ZERO-LENGTH                   PIC S9(4) VALUE +0    COMP. 00031600
       77  WS-COMM-LENGTH                   PIC S9(4) VALUE +600  COMP. 00031700
900837 77  WS-COMM-DB2-LENGTH               PIC S9(4) VALUE +0    COMP. 00031800
       77  WS-AUD-LENGTH                    PIC S9(4) VALUE +3800 COMP. 00031900
910687 77  WS-FIN-LENGTH                    PIC S9(4) VALUE +1024 COMP. 00032000
       77  WS-TUTOR-COMM-LENGTH             PIC S9(4) VALUE +906  COMP. 00032100
MAB    77  WS-BROKER-LENGTH                 PIC S9(4) VALUE +723  COMP. 00032200
       77  WS-AG-KEY-LENGTH                 PIC S9(4) VALUE +8    COMP. 00032300
       77  IS020-LEN                        PIC S9(4) VALUE +1569 COMP. 00032400
       77  DISPLAY-COUNT                    PIC S9(4) VALUE ZERO  COMP. 00032500
                                                                        00032600
       01  CASE-MASTER-RECORD.                                          00032700
           05  CASEX-CARR-AFFL-CODE      PIC  X(2).                     00032800
           05  CASEX-CARRIER-CODE        PIC  X(2).                     00032900
           05  CASEX-CARRIER-IND         PIC S9(04)    COMP.            00033000
           05  CASEX-CASE-NUM            PIC  X(6).                     00033100
           05  CASEX-PREV-CARRIER        PIC  X(2).                     00033200
           COPY AUDCICS.                                                00033300
           COPY CAWSINC.                                                00033400
           COPY DEMOCOMM.                                               00033500
           COPY TURB9999.                                               00033600
           COPY TURB8991.                                               00033700
           COPY TURB8997.                                               00033800
           COPY TUB8997A.                                               00033900
           COPY TURB2006.                                               00034000
           COPY TURB0705.                                               00034100
           COPY TURB0153.                                               00034200
           COPY TURB0043.                                               00034300
RA4254     COPY TURB0106.                                               00034400
           COPY TURB0161.                                               00034500
R02539     COPY TURB0525.                                               00034600
R02539     COPY TURB0091.                                               00034700
      *---------------------------------------------------------------* 00034800
900526*    COPY TUTORCOM.                                             * 00034900
      *---------------------------------------------------------------* 00035000
900526     COPY TU003COM.                                               00035100
           COPY CICSWS.                                                 00035200
           COPY TURBINC.                                                00035300
           COPY TURBDATA.                                               00035400
           COPY IS020COM.                                               00035500
                                                                        00035600
R8285A     COPY COVREWS.                                                00035700
Y2KIMR     COPY EDITCODE.                                               00035800
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE W/S REFERENCES BEGIN              00035900
Y2KIMR*                                                                 00036000
Y2KIMR*01  WS-DATEPRG2-PARAMETERS.                                      00036100
Y2KIMR*    COPY DATEPARM.                                               00036200
Y2KIMR*                                                                 00036300
Y2KIMR 01  WS-DATEPRG2-PARAMETERS.                                      00036400
Y2KIMR     COPY DTEPARM2.                                               00036500
Y2KIMR*                                                                 00036600
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE W/S REFERENCES END                00036700
Y2KIMR*                                                                 00036800
       01  AUDIT-COMM-AREA.                                             00036900
           COPY AUDCOMM.                                                00037000
       01  IO-COMM-AREA.                                                00037100
cobolu*    COPY IOAREA.                                                 00037200
cobolu     COPY IOAREA2K.                                               00037300
                                                                        00037400
R03749     COPY EDITCOMM.                                               00037500
       01  WS-WORK-AREA.                                                00037600
Y2KIMR*                                                                 00037700
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE W/S REFERENCES BEGIN              00037800
Y2KIMR*                                                                 00037900
Y2KIMR*    05  COB2-DATEPRG2                PIC X(8)  VALUE 'DATEPRG2'. 00038000
Y2KIMR*                                                                 00038100
Y2KIMR     05  COB2-DATEPRG2                PIC X(8)  VALUE 'DTEPROG2'. 00038200
                                                                        00038300
R03749     05  CASE-GE01600-PRODUCT-NUMBER  PIC X(4).                   00038400
R03749     05  CASE-GE01600-PRODUCT-LINE    PIC X(3).                   00038500
R03749     05  WS-HLTH-POLICY          PIC XX.                          00038600
R03749     05  WS-PLAN-CODE.                                            00038700
R03749         10  WS-PLAN-1           PIC X.                           00038800
R03749         10  WS-PLAN-2           PIC X.                           00038900
R03749     05  WS-DENTAL-PLAN          PIC X(2) VALUE SPACES.           00039000
R03749     05  HOLD-SQL-DATE.                                           00039100
R03749         10  HOLD-YYYY.                                           00039200
R03749             15  HOLD-19           PIC X(02).                     00039300
R03749             15  HOLD-SQL-YY       PIC 99.                        00039400
R03749         10  HOLD-DASH2            PIC X(01) VALUE '-'.           00039500
R03749         10  HOLD-SQL-MM           PIC 99.                        00039600
R03749             88  VALID-MM          VALUE 1 THRU 12.               00039700
R03749         10  HOLD-DASH1            PIC X(01) VALUE '-'.           00039800
R03749         10  HOLD-SQL-DD           PIC 99.                        00039900
R03749             88  VALID-DD          VALUE 1 THRU 31.               00040000
R03749     05  WS-INCEPTION-DATE   REDEFINES HOLD-SQL-DATE PIC X(10).   00040100
R03749     05  WS-HOLD-HLTH-OPTION     PIC X(02)      VALUE SPACES.     00040200
901057     05  FIELD-EDIT-MODULE                 PIC X(8).              00040300
R03749     05  WS-GE01600-FILLER-FIELDS.                                00040400
R03749         10  WS-GE01600-FLD2-NOT-NEEDED    PIC X(2).              00040500
R03749         10  WS-GE01600-FLD3-NOT-NEEDED    PIC X(2).              00040600
R03749         10  WS-GE01600-FLD4-NOT-NEEDED    PIC X(2).              00040700
R03749         10  WS-GE01600-FLD6-NOT-NEEDED    PIC X(10).             00040800
R03749         10  WS-GE01600-FLD7-NOT-NEEDED    PIC X(2).              00040900
R03749         10  WS-GE01600-FLD8-NOT-NEEDED    PIC X(2).              00041000
R03749         10  WS-GE01600-FLD18-NOT-NEEDED   PIC S9(5) COMP-3.      00041100
R03749         10  WS-GE01600-FLD19-NOT-NEEDED   PIC X(10).             00041200
                                                                        00041300
           05  CASE-FETCH-SW                PIC X.                      00041400
           05  ERROR-STATUS                 PIC S9(8) COMP VALUE +0.    00041500
           05  WS-GASET                     PIC S9(9) COMP.             00041600
           05  WS-GALENGTH                  PIC S9(4) COMP.             00041700
           05  WS-CICS-RESP                 PIC S9(4) COMP.             00041800
           05  WS-CUSTOMER-STATUS           PIC X     VALUE SPACES.     00041900
               88  INVALID-CUSTOMER-STATUS            VALUE 'D' 'C'.    00042000
           05  WS-TS-QUEUE-NAME.                                        00042100
               10  WS-TS-QUEUE-TRANID       PIC X(4).                   00042200
               10  WS-TS-QUEUE-TERMID       PIC X(4).                   00042300
900526     05  WS-REQID-NAME.                                           00042400
900526         10  WS-R-TERMID                   PIC X(4).              00042500
900526         10  FILLER                        PIC X(4)  VALUE 'BRSE'.00042600
           05  WS-TS-ITEM                   PIC S9(4) VALUE +1   COMP.  00042700
           05  WS-HEX80                     PIC S9(4) VALUE +128 COMP.  00042800
           05  WS-HEX80-REDEF REDEFINES WS-HEX80.                       00042900
               10  FILLER                   PIC X.                      00043000
               10  HEX80                    PIC X.                      00043100
           05  WS-NULL-FIELD                PIC X(72).                  00043200
           05  WS-ERROR-FIELDS.                                         00043300
               10  WS-C9999-ERROR-CODE      PIC X(5).                   00043400
               10  WS-C9999-SEVERITY-CODE   PIC X.                      00043500
COBOLU**       10  FILLER                   PIC X.                      00043600
COBOLU         10  WS-C9999-FILLER          PIC X.                      00043700
               10  WS-C9999-ERROR-MESSAGE   PIC X(30).                  00043800
           05  WS-SUBCRIPT COMP.                                        00043900
               10  ACTION-SUB               PIC S99.                    00044000
           05  WS-HOLD-MESSAGE              PIC X(5)   VALUE SPACE.     00044100
MANJU      05  WS-DB2I-MESSAGE              PIC ZZZZ9.                  00044200
           05  WS-MESSAGE-NUMBER1           PIC X(5)   VALUE SPACE.     00044300
           05  WS-MESSAGE-NUMBER2           PIC X(5)   VALUE SPACE.     00044400
           05  WS-MESSAGE-NUMBER3           PIC X(5)   VALUE SPACE.     00044500
           05  WS-MESSAGE-NUMBER4           PIC X(5)   VALUE SPACE.     00044600
           05  LOWER-CASE                   PIC X(26)                   00044700
                                   VALUE 'abcdefghijklmnopqrstuvwxyz'.  00044800
           05  UPPER-CASE                   PIC X(26)                   00044900
                                   VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.  00045000
           05  WS-SYSTEM-CODE.                                          00045100
               10  WS-SYSTEM-CODE-2BYTES    PIC X(2).                   00045200
               10  FILLER                   PIC X(2).                   00045300
           05  WS-ACTION-CODE.                                          00045400
               10  WS-ACTION-CODE-5BYTES    PIC X(5).                   00045500
               10  FILLER                   PIC X.                      00045600
           05  WS-DATE-R                    PIC 9(6)   VALUE ZERO.      00045700
           05  WS-DATE.                                                 00045800
               10  WS-MM                    PIC 99     VALUE ZERO.      00045900
               10  WS-DD                    PIC 99     VALUE ZERO.      00046000
               10  WS-YY                    PIC 99     VALUE ZERO.      00046100
           05  WS-DATE-A REDEFINES WS-DATE.                             00046200
               10  WS-MM-A                  PIC XX.                     00046300
               10  WS-DD-A                  PIC XX.                     00046400
               10  WS-YY-A                  PIC XX.                     00046500
           05  WS-BIRTH-DATE.                                           00046600
               10  BR-CC                    PIC XX.                     00046700
               10  BR-YY                    PIC XX.                     00046800
               10  FILLER                   PIC X      VALUE '-'.       00046900
               10  BR-MM                    PIC XX.                     00047000
               10  FILLER                   PIC X      VALUE '-'.       00047100
               10  BR-DD                    PIC XX.                     00047200
           05  WS-EFFECTIVE-DATE.                                       00047300
               10  EF-CC                    PIC XX.                     00047400
               10  EF-YY                    PIC XX.                     00047500
               10  FILLER                   PIC X      VALUE '-'.       00047600
               10  EF-MM                    PIC XX.                     00047700
               10  FILLER                   PIC X      VALUE '-'.       00047800
               10  EF-DD                    PIC XX.                     00047900
           05  WS-CHANGE-DATE.                                          00048000
               10  CO-CC                    PIC XX.                     00048100
               10  CO-YY                    PIC XX.                     00048200
               10  FILLER                   PIC X      VALUE '-'.       00048300
               10  CO-MM                    PIC XX.                     00048400
               10  FILLER                   PIC X      VALUE '-'.       00048500
               10  CO-DD                    PIC XX.                     00048600
           05  WS-DATE-EIGHT.                                           00048700
               10  WS-MONTH                 PIC 99     VALUE ZERO.      00048800
               10  SLASH-1                  PIC X.                      00048900
               10  WS-DAY                   PIC 99     VALUE ZERO.      00049000
               10  SLASH-2                  PIC X.                      00049100
               10  WS-YEAR                  PIC 99     VALUE ZERO.      00049200
           05  WS-COMPARE-DATE.                                         00049300
               10  WS-COMPARE-YY            PIC 99     VALUE ZERO.      00049400
               10  WS-COMPARE-MM            PIC 99     VALUE ZERO.      00049500
               10  WS-COMPARE-DD            PIC 99     VALUE ZERO.      00049600
           05  HOLD-NEXT-CIM                PIC 9(8) VALUE ZERO.        00049700
           05  HOLD-NEXT-CIM-R REDEFINES HOLD-NEXT-CIM                  00049800
                                            PIC X(8).                   00049900
                                                                        00050000
           05  FWAC-ADDR                         PIC S9(08)  COMP.      00050100
           05  FWAC-PNTR REDEFINES FWAC-ADDR USAGE IS POINTER.          00050200
DFJ        05  STATUS-CHANGED-SW            PIC X      VALUE 'N'.       00050300
DFJ        05  STATE-CHANGED-SW             PIC X      VALUE 'N'.       00050400
DFJ        05  ZIP-CHANGED-SW               PIC X      VALUE 'N'.       00050500
890989     05  ZIP-PLUS4-CHANGED-SW         PIC X      VALUE 'N'.       00050600
           05  PHONE-CHANGED-SW             PIC X.                      00050700
           05  DISPLAY-CHANGED-SW           PIC X.                      00050800
COBOLU     05  WS-APPLID                    PIC X(08).                  00050900
890989     05  WS-HOLD-CASE-STATE           PIC X(02).                  00051000
890989     05  WS-HOLD-CASE-ZIP             PIC 9(05).                  00051100
890989     05  WS-HOLD-CASE-ZIP-PLUS4       PIC 9(04).                  00051200
890989     05  WS-HOLD-CASE-NUM             PIC X(06).                  00051300
890989     05  WS-HOLD-CSN-STATE            PIC X(02).                  00051400
890989     05  WS-HOLD-CSN-ZIP              PIC X(05).                  00051500
890989     05  WS-HOLD-CSN-ZIP-PLUS4        PIC X(04).                  00051600
890989     05  WS-CASE-EMP-NUM.                                         00051700
890989         10  WS-CASE-NUM         PIC X(6).                        00051800
890989         10  WS-EMP-NUM          PIC 9(5).                        00051900
890989     05  WS-EMP-EOF                   PIC X(1) VALUE 'N'.         00052000
890989     05  WS-EMP-UPDATE                PIC X(1) VALUE 'N'.         00052100
           05  WS-CASE-AREA-PHONE.                                      00052200
               10  WS-CASE-AREA             PIC X(3).                   00052300
               10  WS-CASE-PHONE            PIC X(7).                   00052400
           05  WS-PHONE.                                                00052500
               10  WS-PHONE3                PIC X(3).                   00052600
               10  WS-PHONE4                PIC X(4).                   00052700
           05  WS-MAP-DATE.                                             00052800
               10  FILLER                   PIC 99.                     00052900
               10  WS-MAP-BIF               PIC 9(6).                   00053000
           05  WS-CIM-PHONE.                                            00053100
               10  WS-CIM-PHONE-AREA        PIC X(3).                   00053200
               10  WS-DASH1                 PIC X.                      00053300
               10  WS-CIM-PHONE-EXCH        PIC X(3).                   00053400
               10  WS-DASH2                 PIC X.                      00053500
               10  WS-CIM-PHONE-NUMB        PIC X(4).                   00053600
           05  WS-TIME-FIELD                PIC S9(8) COMP.             00053700
           05  WC-TODAYS-DATE.                                          00053800
               10  WC-TODAYS-MM             PIC XX.                     00053900
               10  FILLER                   PIC X.                      00054000
               10  WC-TODAYS-DD             PIC XX.                     00054100
               10  FILLER                   PIC X.                      00054200
               10  WC-TODAYS-YY             PIC XX.                     00054300
           05  WS-FINALST-REAS-CODE.                                    00054400
               10  WS-FINALST-BYTE1         PIC X.                      00054500
               10  FILLER                   PIC XX.                     00054600
           05  WS-CIM-NUMBER                PIC X(8).                   00054700
           05  WS-EDIT-SW                   PIC X.                      00054800
           05  WS-DUPLICATE-SW              PIC X.                      00054900
           05  OPER-ID                      PIC X(3).                   00055000
           05  WS-NUMERIC-CHECK.                                        00055100
               10 WS-NUMERIC-CHECK-BYTE OCCURS 20 TIMES                 00055200
                                            PIC X.                      00055300
           05  WS-NUMERIC-SW                PIC X.                      00055400
R04023     05  ORIGINAL-STATE               PIC XX.                     00055500
R02539     05  POSTAL-CODE                  PIC X(9).                   00055600
R02539     05  COUNTRY-CODE                 PIC X(3).                   00055700
R02539     05  IND-POSTAL-CODE              PIC S9(4) COMP.             00055800
R02539     05  IND-COUNTRY-CODE             PIC S9(4) COMP.             00055900
           05  WS-BROKER-COMMAREA.                                      00056000
               10  WS-AGENT-COMMAREA        PIC X(600).                 00056100
MAB            10  WS-BROKER-FIELDS         PIC X(121).                 00056200
               10  WS-SYSTEM-ADD-TYPE       PIC XX.                     00056300
           05  WS-STATE-ZIP.                                            00056400
               10  WS-STATE                 PIC X(2).                   00056500
               10  FILLER                   PIC X VALUE SPACE.          00056600
               10  WS-ZIP                   PIC X(5).                   00056700
           05  WS-DB2-DATE.                                             00056800
               10  WS-DB2-CC                PIC X(2).                   00056900
Y2KIMR*                                                                 00057000
Y2KIMR* IMR CHANGE BEGIN                                                00057100
Y2KIMR*                                                                 00057200
Y2KIMR         10  WS-DB2-CC-R REDEFINES WS-DB2-CC                      00057300
Y2KIMR                                      PIC 99.                     00057400
Y2KIMR*                                                                 00057500
Y2KIMR* IMR CHANGE END                                                  00057600
Y2KIMR*                                                                 00057700
               10  WS-DB2-YY                PIC X(2).                   00057800
               10  WS-DB2-YY-R REDEFINES WS-DB2-YY                      00057900
                                            PIC 99.                     00058000
               10  FILLER                   PIC X.                      00058100
               10  WS-DB2-MM                PIC X(2).                   00058200
               10  WS-DB2-MM-R REDEFINES WS-DB2-MM                      00058300
                                            PIC 99.                     00058400
               10  FILLER                   PIC X.                      00058500
               10  WS-DB2-DD                PIC X(2).                   00058600
               10  WS-DB2-DD-R REDEFINES WS-DB2-DD                      00058700
                                            PIC 99.                     00058800
           05  WS-DB2-TIMESTAMP.                                        00058900
               10  WS-TS-CC                 PIC X(2).                   00059000
               10  WS-TS-YY                 PIC X(2).                   00059100
               10  WS-TS-YY-R REDEFINES WS-TS-YY                        00059200
                                            PIC 99.                     00059300
               10  FILLER                   PIC X.                      00059400
               10  WS-TS-MM                 PIC X(2).                   00059500
               10  WS-TS-MM-R REDEFINES WS-TS-MM                        00059600
                                            PIC 99.                     00059700
               10  FILLER                   PIC X.                      00059800
               10  WS-TS-DD                 PIC X(2).                   00059900
               10  WS-TS-DD-R REDEFINES WS-TS-DD                        00060000
                                            PIC 99.                     00060100
               10  FILLER                   PIC X(16).                  00060200
           05  WS-RECORD-NOTFND             PIC X.                      00060300
           05  WS-COMPANY-NAME.                                         00060400
               10  WS-COMPANY-NAME-HIGH-ORDR   PIC X(22) VALUE SPACES.  00060500
               10  WS-COMPANY-NAME-LOW-ORDR    PIC X(08) VALUE SPACES.  00060600
           05  WS-ENTITY-LITERAL.                                       00060700
               10  WS-LITERAL-HIGH-ORDR        PIC X(02) VALUE SPACES.  00060800
C08784****     10  WS-LITERAL-LOW-ORDR         PIC X(13) VALUE SPACES.  00060900
C08784         10  WS-LITERAL-LOW-ORDR         PIC X(08) VALUE SPACES.  00061000
           05  WS-C                         PIC X(1) VALUE 'C'.         00061100
           05  WS-Y                         PIC X(1) VALUE 'Y'.         00061200
           05  WS-SPACE                     PIC X(1) VALUE SPACE.       00061300
           05  WS-SPACES                    PIC X(2) VALUE SPACES.      00061400
           05  WS-ZEROES-5                  PIC X(5) VALUE '00000'.     00061500
           05  WS-ZEROES-4                  PIC X(5) VALUE '0000'.      00061600
                                                                        00061700
RA4254 01 WS-GETT0106                      PIC X(08) VALUE 'GETT0106'.  00061800
                                                                        00061900
R01142 01  WS-ADDRESS1.                                                 00062000
R01142     05  WS-ADDRESS1-22          PIC X(22).                       00062100
R01142     05  WS-ADDRESS1-REMAINDER   PIC X(08).                       00062200
R01142 01  WS-ADDRESS2.                                                 00062300
R01142     05  WS-ADDRESS2-22          PIC X(22).                       00062400
R01142     05  WS-ADDRESS2-REMAINDER   PIC X(08).                       00062500
                                                                        00062600
900526 01  TCTUAL                      PIC S9(4) COMP.                  00062700
900526     88  INVALID-TCTUAL                    VALUE 0 THRU 135.      00062800
                                                                        00062900
980714 01  BILLING-REQUEST-RECORDX.                                     00063000
980714     COPY BILLREQ.                                                00063100
GRB    01  CASE-RECORD.                                                 00063200
GRB        COPY CASECOBQ.                                               00063300
890989 01  EMPLOYEE-RECORD.                                             00063400
890989     COPY EMPMSTQ.                                                00063500
890989 01  OLD-EMP-REC.                                                 00063600
890989     05  FILLER                       PIC X(555).                 00063700
                                                                        00063800
       01  WS-SCZ-COUNTY-CODE               PIC X(5).                   00063900
       01  WS-SCZ-COUNTY-CODE-N REDEFINES                               00064000
           WS-SCZ-COUNTY-CODE               PIC 9(5).                   00064100
                                                                        00064200
       01  WS-SCZ-ZIP                       PIC 9(5).                   00064300
       01  WS-SCZ-ZIP-C         REDEFINES                               00064400
           WS-SCZ-ZIP                       PIC X(5).                   00064500
                                                                        00064600
       01  WSQ-COMMAREA.                                                00064700
           02  WSQ-COMM-FIELDS.                                         00064800
               05  WSQ-CICS-COMMAREA-LENGTH PIC S9(4) COMP VALUE +600.  00064900
               05  WSQ-SYSTEM-CODE          PIC X(02).                  00065000
               05  WSQ-ACTION-CODE          PIC X(05).                  00065100
               05  WSQ-CUSTOMER-INFO        PIC X(40).                  00065200
               05  WSQ-CUST-TOKEN-COUNT     PIC S9.                     00065300
               05  WSQ-CUSTOMER-INFO1       PIC X(40).                  00065400
               05  WSQ-CUSTOMER-INFO2       PIC X(30).                  00065500
               05  WSQ-CUSTOMER-INFO3       PIC X(30).                  00065600
               05  WSQ-CUSTOMER-INFO4       PIC X(30).                  00065700
               05  WSQ-CUSTOMER-INFO5       PIC X(30).                  00065800
               05  WSQ-CUSTOMER-INFO6       PIC X(30).                  00065900
               05  FILLER                   PIC X(46).                  00066000
               05  WSQ-PREVIOUS-TRANID      PIC X(4).                   00066100
               05  WSQ-CMDLINE-CHANGED      PIC X(1).                   00066200
               05  WSQ-KEY-CHANGED          PIC X(1).                   00066300
               05  WSQ-CIM-NUMBER           PIC X(8).                   00066400
               05  WSQ-MAIL-LINE-COUNT      PIC 9.                      00066500
               05  WSQ-MAIL-LINE            PIC X(30) OCCURS 5 TIMES.   00066600
               05  WSQ-LAST-ITEM-NUM-BROWSED PIC S9(3) COMP-3.          00066700
               05  WSQ-CUST-TYPE            PIC X(2).                   00066800
               05  WSQ-CUST-STATUS          PIC X.                      00066900
               05  WSQ-CUST-PHONE.                                      00067000
                   10  WSQ-CUST-PHONE-AREA  PIC X(3).                   00067100
                   10  WSQ-CUST-PHONE-EXCH  PIC X(3).                   00067200
                   10  WSQ-CUST-PHONE-NUMB  PIC X(4).                   00067300
               05  FILLER                   PIC X(25).                  00067400
               05  WSQ-MSG-COUNT            PIC 9.                      00067500
               05  WSQ-MSG-ID               PIC X(5) OCCURS 4 TIMES.    00067600
               05  WSQ-MSG-MAX-SEVERITY     PIC X(1).                   00067700
               05  WSQ-NEXT-FUNCTION        PIC X(2).                   00067800
               05  WSQ-CURSOR-POSN          PIC S9(4) COMP.             00067900
               05  FILLER                   PIC X(83).                  00068000
           02  WSQ-AGNTNAME.                                            00068100
               05 WSQ-CIM                   PIC X(8).                   00068200
               05 WSQ-LAST-NAME             PIC X(20).                  00068300
               05 WSQ-FIRST-NAME            PIC X(15).                  00068400
               05 WSQ-MIDDLE-NAME           PIC X(15).                  00068500
               05 WSQ-PREFIX                PIC X(4).                   00068600
               05 WSQ-SUFFIX1               PIC X(4).                   00068700
               05 WSQ-SUFFIX2               PIC X(4).                   00068800
               05 WSQ-COMPANY-IND           PIC X(1).                   00068900
               05 WSQ-COMPANY-IN-ADDRESS    PIC X(1).                   00069000
               05 WSQ-COMPANY-NAME          PIC X(30).                  00069100
               05 WSQ-DISPLAY-NAME          PIC X(30).                  00069200
               05 WSQ-NICKNAME              PIC X(10).                  00069300
R04023         05 WSQ-ISSUE-STATE           PIC X(02).                  00069400
               05 WSQ-ADDRESS1              PIC X(30).                  00069500
               05 WSQ-ADDRESS2              PIC X(30).                  00069600
               05 WSQ-CITY                  PIC X(30).                  00069700
               05 WSQ-STATE                 PIC X(2).                   00069800
               05 WSQ-ZIP                   PIC X(5).                   00069900
               05 WSQ-ZIP-PLUS4             PIC X(4).                   00070000
               05 WSQ-COUNTY-CODE           PIC X(5).                   00070100
               05 WSQ-AREA-CODE             PIC X(3).                   00070200
               05 WSQ-PHONE                 PIC X(7).                   00070300
               05 WSQ-PHONE-EXTENSION       PIC X(4).                   00070400
               05 WSQ-SSN                   PIC X(9).                   00070500
               05 WSQ-SEX                   PIC X(1).                   00070600
Y2KIMR***      05 WSQ-BIRTH-DATE            PIC S9(6)    COMP-3.        00070700
Y2KIMR         05 WSQ-BIRTH-DATE            PIC S9(8)    COMP-3.        00070800
               05 WSQ-FINALST-REAS-CODE     PIC X(3).                   00070900
               05 WSQ-FINALST-OVRD-IND      PIC X(1).                   00071000
               05 WSQ-DUP-ADDR-OVRD-IND     PIC X(1).                   00071100
               05 WSQ-EFFECTIVE-DATE        PIC S9(6)    COMP-3.        00071200
               05 WSQ-CHANGE-DATE           PIC S9(6)    COMP-3.        00071300
               05 WSQ-CHANGE-LOGON          PIC X(8).                   00071400
               05 WSQ-ENTITY-TYPE           PIC X(2).                   00071500
               05 WSQ-RECORD-STATUS         PIC X(1).                   00071600
               05 WSQ-ALT-ADDRESS-IND       PIC X(1).                   00071700
               05 WSQ-FUTURE-ADDRESS-IND    PIC X(1).                   00071800
               05 WSQ-RECORD-ORIGIN         PIC X(1).                   00071900
               05 WSQ-COMBINED-STATUS       PIC X(2).                   00072000
               05 WSQ-SITE-CODE             PIC X(2).                   00072100
               05 WSQ-NAME-KEY1             PIC X(8).                   00072200
               05 WSQ-NAME-KEY2             PIC X(8).                   00072300
               05 WSQ-NAME-KEY3             PIC X(2).                   00072400
               05 WSQ-ADDRESS-KEY1          PIC X(10).                  00072500
               05 WSQ-ASSOCIATION1          PIC X(5).                   00072600
               05 WSQ-ASSOCIATION2          PIC X(5).                   00072700
               05 WSQ-ASSOCIATION3          PIC X(5).                   00072800
               05 WSQ-ENTITY-LITERAL        PIC X(10).                  00072900
               05 WSQ-CASE-SW               PIC X.                      00073000
               05 WSQ-DISP-IND              PIC X.                      00073100
               05 WSQ-FINALIST-SW           PIC X.                      00073200
900837         05 WSQ-FAX-AREA-CODE         PIC X(3).                   00073300
900837         05 WSQ-FAX-PHONE             PIC X(7).                   00073400
R00700         05 WSQ-EMAIL1                PIC X(50).                  00073500
R04023         05 WSQ-POSTAL-CODE           PIC X(09).                  00073600
R04023         05 WSQ-COUNTRY-CODE          PIC X(03).                  00073700
R04023         05 WSQ-CARRIER-CODE          PIC XX.                     00073800
R04023         05 WSQ-PREV-CARRIER          PIC XX.                     00073900
                                                                        00074000
       01  WS-TS-QUEUE.                                                 00074100
           COPY NA200C01.                                               00074200
           02  COM-AGNTNAME.                                            00074300
               05 COM-CIM                   PIC X(8).                   00074400
               05 COM-LAST-NAME             PIC X(20).                  00074500
               05 COM-FIRST-NAME            PIC X(15).                  00074600
               05 COM-MIDDLE-NAME           PIC X(15).                  00074700
               05 COM-PREFIX                PIC X(4).                   00074800
               05 COM-SUFFIX1               PIC X(4).                   00074900
               05 COM-SUFFIX2               PIC X(4).                   00075000
               05 COM-COMPANY-IND           PIC X(1).                   00075100
               05 COM-COMPANY-IN-ADDRESS    PIC X(1).                   00075200
               05 COM-COMPANY-NAME          PIC X(30).                  00075300
               05 COM-DISPLAY-NAME          PIC X(30).                  00075400
               05 COM-NICKNAME              PIC X(10).                  00075500
R04023         05 COM-ISSUE-STATE           PIC X(02).                  00075600
               05 COM-ADDRESS1              PIC X(30).                  00075700
               05 COM-ADDRESS2              PIC X(30).                  00075800
               05 COM-CITY                  PIC X(30).                  00075900
               05 COM-STATE                 PIC X(2).                   00076000
               05 COM-ZIP                   PIC X(5).                   00076100
               05 COM-ZIP-PLUS4             PIC X(4).                   00076200
               05 COM-COUNTY-CODE           PIC X(5).                   00076300
               05 COM-AREA-CODE             PIC X(3).                   00076400
               05 COM-PHONE                 PIC X(7).                   00076500
               05 COM-PHONE-EXTENSION       PIC X(4).                   00076600
               05 COM-SSN                   PIC X(9).                   00076700
               05 COM-SEX                   PIC X(1).                   00076800
Y2KIMR****     05 COM-BIRTH-DATE            PIC S9(6)    COMP-3.        00076900
Y2KIMR         05 COM-BIRTH-DATE            PIC S9(8)    COMP-3.        00077000
               05 COM-FINALST-REAS-CODE     PIC X(3).                   00077100
               05 COM-FINALST-OVRD-IND      PIC X(1).                   00077200
               05 COM-DUP-ADDR-OVRD-IND     PIC X(1).                   00077300
               05 COM-EFFECTIVE-DATE        PIC S9(6)    COMP-3.        00077400
               05 COM-CHANGE-DATE           PIC S9(6)    COMP-3.        00077500
               05 COM-CHANGE-LOGON          PIC X(8).                   00077600
               05 COM-ENTITY-TYPE           PIC X(2).                   00077700
               05 COM-RECORD-STATUS         PIC X(1).                   00077800
               05 COM-ALT-ADDRESS-IND       PIC X(1).                   00077900
               05 COM-FUTURE-ADDRESS-IND    PIC X(1).                   00078000
               05 COM-RECORD-ORIGIN         PIC X(1).                   00078100
               05 COM-COMBINED-STATUS       PIC X(2).                   00078200
               05 COM-SITE-CODE             PIC X(2).                   00078300
               05 COM-NAME-KEY1             PIC X(8).                   00078400
               05 COM-NAME-KEY2             PIC X(8).                   00078500
               05 COM-NAME-KEY3             PIC X(2).                   00078600
               05 COM-ADDRESS-KEY1          PIC X(10).                  00078700
               05 COM-ASSOCIATION1          PIC X(5).                   00078800
               05 COM-ASSOCIATION2          PIC X(5).                   00078900
               05 COM-ASSOCIATION3          PIC X(5).                   00079000
               05 COM-ENTITY-LITERAL        PIC X(10).                  00079100
               05 COM-CASE-SW               PIC X.                      00079200
               05 COM-DISP-IND              PIC X.                      00079300
               05 COM-FINALIST-SW           PIC X.                      00079400
900837         05 COM-FAX-AREA-CODE         PIC X(3).                   00079500
900837         05 COM-FAX-PHONE             PIC X(7).                   00079600
R00700         05 COM-EMAIL1                PIC X(50).                  00079700
R02539         05 COM-POSTAL-CODE           PIC X(09).                  00079800
R02539         05 COM-COUNTRY-CODE          PIC X(03).                  00079900
R04023         05 COM-CARRIER-CODE          PIC XX.                     00080000
R04023         05 COM-PREV-CARRIER          PIC XX.                     00080100
                                                                        00080200
11735A*    COPY BROKRV01.                                               00080300
11735A*    COPY BROKER.                                                 00080400
900837*    COPY AGNTNV03.                                               00080500
R00700     COPY AGNTNV05.                                               00080600
900837     COPY CASNVW3.                                                00080700
DFJ   *    COPY CAMASV01.                                               00080800
11735A     COPY CATMASTE.                                               00080900
900600*    COPY CASEXV01.                                               00081000
MANJU *    COPY CASEMAST  REPLACING CASE- BY CASEX-.                    00081100
890989*    COPY EMPLV03.                                                00081200
11735A*    COPY EMPLOYEE.                                               00081300
R03749*    COPY COVERAGE.                                               00081400
                                                                        00081500
R8285A     COPY COVEDITS.                                               00081600
                                                                        00081700
R08986     COPY RULEWS1.                                                00081800
           COPY STATECON.                                               00081900
R08986                                                                  00082000
           EXEC SQL                                                     00082100
              INCLUDE SQLCA                                             00082200
           END-EXEC.                                                    00082300
           05  WS-SQL-ERROR-MESSAGE         PIC X(78).                  00082400
                                                                        00082500
      *    EXEC SQL DECLARE CONTCUR CURSOR FOR                          00082600
      *        SELECT COUNT(*)                                          00082700
11735A*        FROM BROKRV01                                            00082800
11735A*        FROM BROKER                                              00082900
      *        WHERE AGNTNAME#IDNTY = :WS-CIM-NUMBER                    00083000
      *    END-EXEC.                                                    00083100
                                                                        00083200
           EXEC SQL DECLARE AGNTCUR CURSOR FOR                          00083300
11735A*        SELECT *                                                 00083400
11735A         SELECT                                                   00083500
                    IDNTITY,                                            00083600
                    LAST_NAME,                                          00083700
                    FIRST_NAME,                                         00083800
                    MIDDLE_NAME,                                        00083900
                    PREFIX,                                             00084000
                    SUFFIX1,                                            00084100
                    SUFFIX2,                                            00084200
                    COMPANY_IND,                                        00084300
                    COMPANY_IN_ADDRESS,                                 00084400
                    COMPANY_NAME,                                       00084500
                    DISPLAY_NAME,                                       00084600
                    NICKNAME,                                           00084700
                    ADDRESS1,                                           00084800
                    ADDRESS2,                                           00084900
                    CITY,                                               00085000
                    STATE,                                              00085100
289843              ZIP,                                                00085200
                    ZIP_PLUS4,                                          00085300
                    COUNTY_CODE,                                        00085400
                    AREA_CODE,                                          00085500
                    PHONE,                                              00085600
                    PHONE_EXTENSION,                                    00085700
                    SSN,                                                00085800
                    SEX,                                                00085900
                    BIRTH_DATE,                                         00086000
                    FINALST_REAS_CODE,                                  00086100
                    FINALST_OVRD_IND,                                   00086200
                    DUP_ADDR_OVRD_IND,                                  00086300
                    EFFECTIVE_DATE,                                     00086400
                    CHANGE_DATE,                                        00086500
                    CHANGE_LOGON,                                       00086600
                    ENTITY_TYPE,                                        00086700
                    RECORD_STATUS,                                      00086800
                    ALT_ADDRESS_IND,                                    00086900
                    FUTURE_ADDRESS_IND,                                 00087000
                    RECORD_ORIGIN,                                      00087100
                    COMBINED_STATUS,                                    00087200
                    SITE_CODE,                                          00087300
                    NAME_KEY1,                                          00087400
                    NAME_KEY2,                                          00087500
                    NAME_KEY3,                                          00087600
                    ADDRESS_KEY1,                                       00087700
                    ASSOCIATION1,                                       00087800
                    ASSOCIATION2,                                       00087900
                    ASSOCIATION3                                        00088000
900837*        FROM AGNTNV03                                            00088100
R00700         FROM AGNTNAME                                            00088200
               WHERE IDNTITY = :WS-CIM-NUMBER                           00088300
           END-EXEC.                                                    00088400
                                                                        00088500
           EXEC SQL DECLARE UPDT-AGNT-CUR CURSOR FOR                    00088600
11735A*        SELECT *                                                 00088700
11735A         SELECT                                                   00088800
                    IDNTITY,                                            00088900
                    LAST_NAME,                                          00089000
                    FIRST_NAME,                                         00090000
                    MIDDLE_NAME,                                        00091000
                    PREFIX,                                             00092000
                    SUFFIX1,                                            00093000
                    SUFFIX2,                                            00093100
                    COMPANY_IND,                                        00093200
                    COMPANY_IN_ADDRESS,                                 00093300
                    COMPANY_NAME,                                       00093400
                    DISPLAY_NAME,                                       00093500
                    NICKNAME,                                           00093600
                    ADDRESS1,                                           00093700
                    ADDRESS2,                                           00093800
                    CITY,                                               00093900
                    STATE,                                              00094000
289843              ZIP,                                                00094100
                    ZIP_PLUS4,                                          00094200
                    COUNTY_CODE,                                        00094300
                    AREA_CODE,                                          00094400
                    PHONE,                                              00094500
                    PHONE_EXTENSION,                                    00094600
                    SSN,                                                00094700
                    SEX,                                                00094800
                    BIRTH_DATE,                                         00094900
                    FINALST_REAS_CODE,                                  00095000
                    FINALST_OVRD_IND,                                   00095100
                    DUP_ADDR_OVRD_IND,                                  00095200
                    EFFECTIVE_DATE,                                     00095300
                    CHANGE_DATE,                                        00095400
                    CHANGE_LOGON,                                       00095500
                    ENTITY_TYPE,                                        00095600
                    RECORD_STATUS,                                      00095700
                    ALT_ADDRESS_IND,                                    00095800
                    FUTURE_ADDRESS_IND,                                 00095900
                    RECORD_ORIGIN,                                      00096000
                    COMBINED_STATUS,                                    00096100
                    SITE_CODE,                                          00096200
                    NAME_KEY1,                                          00096300
                    NAME_KEY2,                                          00096400
                    NAME_KEY3,                                          00096500
                    ADDRESS_KEY1,                                       00096600
                    ASSOCIATION1,                                       00096700
                    ASSOCIATION2,                                       00096800
                    ASSOCIATION3                                        00096900
R00700             FROM AGNTNAME                                        00097000
                   WHERE IDNTITY = :WS-CIM-NUMBER                       00097100
               FOR UPDATE OF LAST_NAME, FIRST_NAME, MIDDLE_NAME,        00097200
                   PREFIX, SUFFIX1, SUFFIX2, COMPANY_IND,               00097300
                   COMPANY_IN_ADDRESS, COMPANY_NAME, DISPLAY_NAME,      00097400
                   NICKNAME, ADDRESS1, ADDRESS2, CITY, STATE, ZIP,      00097500
                   ZIP_PLUS4, COUNTY_CODE, AREA_CODE, PHONE,            00097600
                   PHONE_EXTENSION, SSN, SEX, BIRTH_DATE,               00097700
                   FINALST_REAS_CODE, FINALST_OVRD_IND,                 00097800
                   DUP_ADDR_OVRD_IND, EFFECTIVE_DATE,                   00097900
                   CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,              00098000
                   RECORD_STATUS, ALT_ADDRESS_IND,                      00098100
                   FUTURE_ADDRESS_IND, RECORD_ORIGIN,                   00098200
                   COMBINED_STATUS, SITE_CODE, NAME_KEY1,               00098300
                   NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,                  00098400
                   ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,            00098500
900837             FAX_AREA_CODE, FAX_PHONE                             00098600
           END-EXEC.                                                    00098700
                                                                        00098800
KPL        EXEC SQL DECLARE CASECUR CURSOR FOR                          00098900
R00700*        SELECT *                                                 00099000
900837*        FROM CASENV03                                            00099100
R00700         SELECT                                                   00099200
R00700             IDNTITY, LAST_NAME, FIRST_NAME, MIDDLE_NAME,         00099300
R00700             PREFIX, SUFFIX1, SUFFIX2, COMPANY_IND,               00099400
R00700             COMPANY_IN_ADDRESS, COMPANY_NAME, DISPLAY_NAME,      00099500
R00700             NICKNAME, ADDRESS1, ADDRESS2, CITY, STATE, ZIP,      00099600
R00700             ZIP_PLUS4, COUNTY_CODE, AREA_CODE, PHONE,            00099700
R00700             PHONE_EXTENSION, SSN, SEX, BIRTH_DATE,               00099800
R00700             FINALST_REAS_CODE, FINALST_OVRD_IND,                 00099900
R00700             DUP_ADDR_OVRD_IND, EFFECTIVE_DATE,                   00100000
R00700             CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,              00100100
R00700             RECORD_STATUS, ALT_ADDRESS_IND,                      00100200
R00700             FUTURE_ADDRESS_IND, RECORD_ORIGIN,                   00100300
R00700             COMBINED_STATUS, SITE_CODE, NAME_KEY1,               00100400
R00700             NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,                  00100500
R00700             ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,            00100600
R00700***          FAX_AREA_CODE, FAX_PHONE, EMAIL,                     00100700
R04023             FAX_AREA_CODE, FAX_PHONE, ORIGINAL_STATE, EMAIL,     00100800
R02539             COUNTRY_CODE, POSTAL_CODE                            00100900
R00700         FROM CASENAME                                            00101000
               WHERE IDNTITY = :WS-CIM-NUMBER                           00101100
           END-EXEC.                                                    00101200
                                                                        00101300
           EXEC SQL DECLARE UPDT-CASE-CUR CURSOR FOR                    00101400
110611         SELECT IDNTITY, LAST_NAME, FIRST_NAME, MIDDLE_NAME,      00101500
110611             PREFIX, SUFFIX1, SUFFIX2, COMPANY_IND,               00101600
110611             COMPANY_IN_ADDRESS, COMPANY_NAME, DISPLAY_NAME,      00101700
110611             NICKNAME, ADDRESS1, ADDRESS2, CITY, STATE, ZIP,      00101800
110611             ZIP_PLUS4, COUNTY_CODE, AREA_CODE, PHONE,            00101900
110611             PHONE_EXTENSION, SSN, SEX, BIRTH_DATE,               00102000
110611             FINALST_REAS_CODE, FINALST_OVRD_IND,                 00102100
110611             DUP_ADDR_OVRD_IND, EFFECTIVE_DATE,                   00102200
110611             CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,              00102300
110611             RECORD_STATUS, ALT_ADDRESS_IND,                      00102400
110611             FUTURE_ADDRESS_IND, RECORD_ORIGIN,                   00102500
110611             COMBINED_STATUS, SITE_CODE, NAME_KEY1,               00102600
110611             NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,                  00102700
110611             ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,            00102800
110611             FAX_AREA_CODE, FAX_PHONE, ORIGINAL_STATE, EMAIL,     00102900
R9031A             COUNTRY_CODE, POSTAL_CODE, EMAIL_STATUS              00103000
R00700             FROM CASENAME                                        00103100
                   WHERE IDNTITY = :WS-CIM-NUMBER                       00103200
               FOR UPDATE OF LAST_NAME, FIRST_NAME, MIDDLE_NAME,        00103300
                   PREFIX, SUFFIX1, SUFFIX2, COMPANY_IND,               00103400
                   COMPANY_IN_ADDRESS, COMPANY_NAME, DISPLAY_NAME,      00103500
                   NICKNAME, ADDRESS1, ADDRESS2, CITY, STATE, ZIP,      00103600
                   ZIP_PLUS4, COUNTY_CODE, AREA_CODE, PHONE,            00103700
                   PHONE_EXTENSION, SSN, SEX, BIRTH_DATE,               00103800
                   FINALST_REAS_CODE, FINALST_OVRD_IND,                 00103900
                   DUP_ADDR_OVRD_IND, EFFECTIVE_DATE,                   00104000
                   CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,              00104100
                   RECORD_STATUS, ALT_ADDRESS_IND,                      00104200
                   FUTURE_ADDRESS_IND, RECORD_ORIGIN,                   00104300
                   COMBINED_STATUS, SITE_CODE, NAME_KEY1,               00104400
                   NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,                  00104500
                   ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,            00104600
R04023             FAX_AREA_CODE, FAX_PHONE, ORIGINAL_STATE, EMAIL,     00104700
R9031A             EMAIL_STATUS, COUNTRY_CODE, POSTAL_CODE              00104800
           END-EXEC.                                                    00104900
                                                                        00105000
       01  WS-NAME-ADDRESS-DETAIL.                                      00105100
           COPY NA200C02.                                               00105200
       01  SECURITY-AREA.                                               00105300
           COPY SECCOMMC.                                               00105400
           COPY SYSBUSY.                                                00105500
           COPY EI100C01.                                               00105600
           COPY NA340M1.                                                00105700
           COPY DFHAID.                                                 00105800
           COPY ATTRB.                                                  00105900
                                                                        00106000
                                                                        00106100
       LINKAGE SECTION.                                                 00106200
       01  DFHCOMMAREA                      PIC X.                      00106300
                                                                        00106400
       01  WS-LINK-STORAGE.                                             00106500
           02  WS-LINK-SIX-HUNDRED          PIC X(600).                 00106600
           02  WS-LINK-STUFF      OCCURS 0 TO 3496 TIMES                00106700
                                  DEPENDING ON WS-LINK-LENGTH           00106800
                                            PIC X.                      00106900
       01  I-O-AREA.                                                    00107000
           05  I-O-CASE-REC                 PIC X(766).                 00107100
                                                                        00107200
900526 01  TCTUAR.                                                      00107300
900526     05  FILLER                  PIC X(36).                       00107400
900526     05  TCTUA-DEMO-DATA.                                         00107500
900526         10  FILLER              PIC S9(4) COMP.                  00107600
900526             88  INVALID-DEMO-DATA               VALUE 0.         00107700
900526         10  FILLER              PIC X(98).                       00107800
                                                                        00107900
      ******************************************************************00108000
                                                                        00108100
       PROCEDURE DIVISION.                                              00108200
                                                                        00108300
       0010-BEGIN-PROGRAM.                                              00108400
           EXEC CICS HANDLE CONDITION ERROR(9999-CICS-ERROR)            00108500
                                      END-EXEC.                         00108600
                                                                        00108700
           MOVE LOW-VALUES TO NA340M1O SYSBUSYO.                        00108800
C08784     INITIALIZE WS-C9999-FILLER IO-RETURN WS-ENTITY-LITERAL.      00108900
COBOLU     INITIALIZE  NA-COMM-DISPLAY-NAME                             00109000
T3384                  NA-COMM-TITLE                                    00109100
COBOLU                 NA-COMM-RECORD-NUMBER                            00109200
COBOLU                 NA-COMM-SEARCH-FIELD                             00109300
COBOLU                 NA-COMM-PERSONAL-NAME                            00109400
COBOLU                 NA-COMM-COMPANY-NAME                             00109500
COBOLU                 NA-COMM-COMPANY-MAIL                             00109600
COBOLU                 NA-COMM-BIRTH-MONTH                              00109700
COBOLU                 NA-COMM-SSN                                      00109800
COBOLU                 NA-COMM-ADDRESS-1                                00109900
COBOLU                 NA-COMM-ADDRESS-2                                00110000
COBOLU                 NA-COMM-ADDRESS-3                                00110100
COBOLU                 NA-COMM-CITY                                     00110200
COBOLU                 NA-COMM-STATE                                    00110300
COBOLU                 NA-COMM-ZIP                                      00110400
COBOLU                 NA-COMM-ZIP-PLUS4                                00110500
COBOLU                 NA-COMM-PHONE-NUMBER                             00110600
COBOLU                 NA-COMM-POSTAL-CODE                              00110700
COBOLU                 NA-COMM-COUNTY-CODE                              00110800
COBOLU                 NA-COMM-ASSOC-CODE                               00110900
COBOLU                 NA-COMM-SITE-CODE                                00111000
COBOLU                 NA-COMM-SEX                                      00111100
COBOLU                 NA-COMM-CREATION-DATE                            00111200
COBOLU                 NA-COMM-EFFECTIVE-DATE                           00111300
COBOLU                 NA-COMM-LAST-CHG-DATE                            00111400
COBOLU                 NA-COMM-STATUS                                   00111500
COBOLU                 NA-COMM-MAIL-LINE1                               00111600
COBOLU                 NA-COMM-MAIL-LINE2                               00111700
COBOLU                 NA-COMM-MAIL-LINE3                               00111800
COBOLU                 NA-COMM-MAIL-LINE4                               00111900
COBOLU                 NA-COMM-MAIL-LINE5                               00112000
COBOLU                 NA-COMM-MAIL-LINE6                               00112100
COBOLU                 NA-COMM-LINE(1)                                  00112200
COBOLU                 NA-COMM-LINE(2)                                  00112300
COBOLU                 NA-COMM-LINE(3)                                  00112400
COBOLU                 NA-COMM-LINE(4)                                  00112500
COBOLU                 NA-COMM-LINE(5)                                  00112600
COBOLU                 NA-COMM-LINE(6).                                 00112700
                                                                        00112800
                                                                        00112900
       0020-DB2-SQL-PREPARATION.                                        00113000
      *---------------------------------------------------------------* 00113100
      *    CHECK TO SEE IF THE DB2 ATTACH FACILITY IS ACTIVE.         * 00113200
      *---------------------------------------------------------------* 00113300
MANJU *    EXEC CICS EXTRACT EXIT                                       00113400
      *        PROGRAM ('DSNCEXT1')                                     00113500
      *        ENTRYNAME ('DSNCSQL')                                    00113600
      *        GASET     (WS-GASET)                                     00113700
      *        GALENGTH  (WS-GALENGTH)                                  00113800
      *        RESP      (WS-CICS-RESP)                                 00113900
      *    END-EXEC.                                                    00114000
                                                                        00114100
      *    IF WS-CICS-RESP = DFHRESP(INVEXITREQ)                        00114200
      *        MOVE 'PGM: NA340, PARA 0005- DATABASE NOT AVAILABLE'     00114300
      *            TO WS-SQL-ERROR-MESSAGE                              00114400
      *        MOVE +0000 TO SQLCODE                                    00114500
      *        GO TO 0420-DB2-ERROR.                                    00114600
                                                                        00114700
       0030-WRITE-COST-JOURNAL.                                         00114800
      *---------------------------------------------------------------* 00114900
      * THIS IS THE COPY BOOK FOR THE COST JOURNAL USED TO DETERMINE  * 00115000
      * CUSTOMER INFORMATION MANAGEMENT CHARGES BY CARRIER.           * 00115100
      *---------------------------------------------------------------* 00115200
      *   MOVE SPACES TO WS-COST-CARR-CODE.                             00115300
           COPY CAPRCINC.                                               00115400
                                                                        00115500
       0040-READ-NAQ1-TS-QUEUE.                                         00115600
                                                                        00115700
      *---------------------------------------------------------------* 00115800
      *    THIS IS THE TEMPORARY STORAGE QUEUE THAT IS MAINTAINED     * 00115900
      *    THROUGH OUT THE ENTIRE CIM SYSTEM. THE QUEUE IS            * 00116000
      *    ESTABLISHED THE FIRST TIME THE CUSTOMER SIGNS ON           * 00116100
      *    AND IS ONLY DELETED IF CICS COMES DOWN.                    * 00116200
      *---------------------------------------------------------------* 00116300
                                                                        00116400
           MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.           00116500
           MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.           00116600
           EXEC CICS READQ TS QUEUE  (WS-TS-QUEUE-NAME)                 00116700
                              SET    (ADDRESS OF WS-LINK-STORAGE)       00116800
                              LENGTH (WS-ZERO-LENGTH)                   00116900
                              ITEM   (WS-TS-ITEM)                       00117000
                              RESP   (WS-CICS-RESP)                     00117100
                              END-EXEC.                                 00117200
                                                                        00117300
           IF WS-CICS-RESP = DFHRESP(QIDERR)                            00117400
               PERFORM 0350-WRITE-NAQ1-QUEUE                            00117500
           ELSE                                                         00117600
              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                     00117700
                  GO TO 9999-CICS-ERROR.                                00117800
                                                                        00117900
COBOLU     INITIALIZE COM-AGNTNAME.                                     00118000
C08784     INITIALIZE WSQ-AGNTNAME.                                     00118100
                                                                        00118200
           MOVE WS-ZERO-LENGTH TO WS-ZERO-LENGTH.                       00118300
           COMPUTE WS-LINK-LENGTH = (WS-ZERO-LENGTH - +600).            00118400
           MOVE WS-LINK-STORAGE TO WS-TS-QUEUE.                         00118500
           MOVE COMM-IDNTITY TO WS-CIM-NUMBER.                          00118600
                                                                        00118700
      *---------------------------------------------------------------* 00118800
      *0050-CHECK-SECURITY-CLEARANCE.                                 * 00118900
      *                                                               * 00119000
      *************************************************               * 00119100
      *   LINK TO SECURITY MODULE - FOR RESOURCE CHECK                * 00119200
      *************************************************               * 00119300
      *    MOVE 'NAMENU  '         TO SEC-RESOURCE-NAME.              * 00119400
      *    MOVE 'Y'                TO SEC-CHK-RESOURCE.               * 00119500
      *    MOVE SPACE              TO SEC-RETURN-CODE.                * 00119600
      *    MOVE 'A'                TO SEC-FUNCTION-CODE.              * 00119700
      *                                                               * 00119800
      *    EXEC CICS LINK                                             * 00119900
      *              PROGRAM('SECCHECK')                              * 00120000
      *              COMMAREA(SECURITY-COMM-AREA)                     * 00120100
      *              LENGTH(31)                                       * 00120200
      *    END-EXEC.                                                  * 00120300
      *                                                               * 00120400
      *    IF  SEC-RETURN-CODE = 'B'                                  * 00120500
      *        MOVE 100 TO SQLCODE                                    * 00120600
      *        MOVE 'SC002' TO WS-MESSAGE-NUMBER1                     * 00120700
      *        GO TO 0310-INITIATE-ROUTER                             * 00120800
      *    ELSE                                                       * 00120900
      *    IF  SEC-RETURN-CODE = 'C'                                  * 00121000
      *        MOVE 100 TO SQLCODE                                    * 00121100
      *        MOVE 'SC003' TO WS-MESSAGE-NUMBER1                     * 00121200
      *        GO TO 0310-INITIATE-ROUTER                             * 00121300
      *    ELSE                                                       * 00121400
      *    IF  SEC-RETURN-CODE = 'D'                                  * 00121500
      *        MOVE 100 TO SQLCODE                                    * 00121600
      *        MOVE 'SC004' TO WS-MESSAGE-NUMBER1                     * 00121700
      *        GO TO 0310-INITIATE-ROUTER                             * 00121800
      *    ELSE                                                       * 00121900
      *    IF  SEC-RETURN-CODE = 'E'                                  * 00122000
      *        MOVE 100 TO SQLCODE                                    * 00122100
      *        MOVE 'SC005' TO WS-MESSAGE-NUMBER1                     * 00122200
      *        GO TO 0310-INITIATE-ROUTER                             * 00122300
      *    ELSE                                                       * 00122400
      *    IF  SEC-RETURN-CODE = 'F' OR 'G' OR 'H' OR 'I'             * 00122500
      *        MOVE 100 TO SQLCODE                                    * 00122600
      *        MOVE 'SC006' TO WS-MESSAGE-NUMBER1                     * 00122700
      *        GO TO 0310-INITIATE-ROUTER                             * 00122800
      *    ELSE                                                       * 00122900
      *    IF  SEC-RETURN-CODE NOT = 'A'                              * 00123000
      *        MOVE 100 TO SQLCODE                                    * 00123100
      *        MOVE 'SC007' TO WS-MESSAGE-NUMBER1                     * 00123200
      *        GO TO 0310-INITIATE-ROUTER.                            * 00123300
      *                                                               * 00123400
      *---------------------------------------------------------------* 00123500
                                                                        00123600
       0060-PROCESS-MENU-SCREEN.                                        00123700
MANJU       DISPLAY 'INSIDE UPDATE NA340 PROCESS MENU PARA'             00123800
MANJU       DISPLAY 'WS-CIM-NUMBER' WS-CIM-NUMBER                       00123900
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00124000
           OR  EIBTRNID = 'HCS2'                                        00124100
               MOVE COMM-MSG-ID(1) TO WS-MESSAGE-NUMBER1                00124200
               MOVE COMM-MSG-ID(2) TO WS-MESSAGE-NUMBER2                00124300
               MOVE COMM-MSG-ID(3) TO WS-MESSAGE-NUMBER3                00124400
               MOVE COMM-MSG-ID(4) TO WS-MESSAGE-NUMBER4                00124500
               GO TO 0200-SEND-NA340M1-MAP                              00124600
      *    ELSE                                                         00124700
LMB   *    IF  COMM-PREVIOUS-TRANID = 'NAT0'                            00124800
LMB   *        AND COMM-CUST-TYPE = 'CA'                                00124900
LMB   *        GO TO 0070-EDIT-COMMAND-LINE                             00125000
           ELSE                                                         00125100
      *---------------------------------------------------------------* 00125200
900526*    IF  EIBTRNID = 'TU01'                                      * 00125300
900526*        GO TO 0330-RETURN-FROM-TUTORIAL                        * 00125400
900526*    ELSE                                                       * 00125500
      *---------------------------------------------------------------* 00125600
           IF  COMM-NEXT-FUNCTION = '00'                                00125700
               GO TO 0080-RETURN-FROM-ROUTER                            00125800
           ELSE                                                         00125900
           IF  EIBAID = DFHENTER                                        00126000
               GO TO 0070-EDIT-COMMAND-LINE                             00126100
           ELSE                                                         00126200
           IF  EIBAID = DFHCLEAR                                        00126300
               GO TO 0340-PROCESS-SCREEN-CLEAR                          00126400
      *    ELSE                                                         00126500
      *    IF  EIBAID = DFHPF18                                         00126600
R01181*              OR DFHPF6                                          00126700
      *        GO TO 0320-PROCESS-TUTORIAL-REQUEST                      00126800
      *    ELSE                                                         00126900
      *    IF  EIBAID = DFHPA1                                          00127000
      *        GO TO 0370-RETURN-TO-MT-BILLION                          00127100
           ELSE                                                         00127200
           IF  EIBAID = DFHPF2                                          00127300
               GO TO 0090-EXECUTE-STACK-FUNCTION                        00127400
           ELSE                                                         00127500
           IF  EIBAID = DFHPF3                                          00127600
               GO TO 0100-RETURN-TO-BROWSE                              00127700
           ELSE                                                         00127800
               GO TO 0360-WRONG-KEY-HIT.                                00127900
                                                                        00128000
       0070-EDIT-COMMAND-LINE.                                          00128100
                                                                        00128200
COBOLU     MOVE LOW-VALUES TO NA340M1I.                                 00128300
                                                                        00128400
           EXEC CICS RECEIVE MAP    ('NA340M1')                         00128500
                             RESP   (WS-CICS-RESP)                      00128600
                             END-EXEC.                                  00128700
                                                                        00128800
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00128900
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00129000
                GO TO 9999-CICS-ERROR.                                  00129100
                                                                        00129200
           PERFORM 0110-UPDATE-COMMAND-LINE.                            00129300
                                                                        00129400
      *---------------------------------------------------------------* 00129500
      * IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE = 'Y' THEN SOMETHING* 00129600
      * ON THE COMMAND LINE CHANGED AND THE ROUTER SHOULD BE STARTED. * 00129700
      *---------------------------------------------------------------* 00129800
                                                                        00129900
                                                                        00130000
           IF COMM-NEXT-FUNCTION = '01'                                 00130100
                   PERFORM 0120-UPDATE-CIM-MAP-FIELDS                   00130200
                   PERFORM 0130-EDIT-NA340M1-MAP                        00130300
                   GO TO 0200-SEND-NA340M1-MAP.                         00130400
                                                                        00130500
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED  = 'Y'     00130600
               GO TO 0310-INITIATE-ROUTER.                              00130700
                                                                        00130800
961037     IF  ENTITY-TYPE = 'RP'                                       00130900
961037         MOVE ATTRB-PROT-ASKIP TO MAP-ENTITY-TYPEA                00131000
961037     END-IF.                                                      00131100
R03179     IF ENTITY-TYPE = 'CA'                                        00131200
R03179         MOVE ATTRB-PROT-ASKIP        TO MAP-EMAIL1A MAP-EMAIL2A  00131300
R03179     END-IF.                                                      00131400
                                                                        00131500
      *---------------------------------------------------------------* 00131600
      * IF COMM-NEXT-FUNCTION = '01' THEN THE CUSTOMER IS RE-ENTERING * 00131700
      * THE PROGRAM AND MUST CHECK FOR FOR ERRORS THEN ADD THE RECORD * 00131800
      *---------------------------------------------------------------* 00131900
                                                                        00132000
                                                                        00132100
       0080-RETURN-FROM-ROUTER.                                         00132200
      *---------------------------------------------------------------* 00132300
      *  IF COMM-NEXT-FUNCTION = '00' SEND OUT THE SYSTEM SELECTION   * 00132400
      *  MAP                                                          * 00132500
      *---------------------------------------------------------------* 00132600
                                                                        00132700
           GO TO 0190-SEND-NA340M1-FIRST-TIME.                          00132800
                                                                        00132900
       0090-EXECUTE-STACK-FUNCTION.                                     00133000
                                                                        00133100
      *---------------------------------------------------------------* 00133200
      * THIS FUNCTION TELLS THE ROUTER THAT THE CUSTOMER WISHES TO    * 00133300
      * EXECUTE A SERIES OF COMMAND LINE CHANGES THAT ARE STORED      * 00133400
      * IN THE STACK COMMAND DATASET (THIS IS A FUTURE FUNCTION)      * 00133500
      *---------------------------------------------------------------* 00133600
                                                                        00133700
      *    PERFORM 0415-UNLOCK-IOMOD.                                   00133800
                                                                        00133900
           MOVE 'GO'   TO COMM-SYSTEM-CODE.                             00134000
           MOVE SPACES TO COMM-ACTION-CODE.                             00134100
           MOVE 'Y'    TO COMM-CMDLINE-CHANGED.                         00134200
           GO TO 0310-INITIATE-ROUTER.                                  00134300
                                                                        00134400
       0100-RETURN-TO-BROWSE.                                           00134700
                                                                        00134800
           MOVE SPACES   TO COMM-SYSTEM-CODE.                           00134900
           MOVE SPACES   TO COMM-ACTION-CODE.                           00135000
           MOVE SPACES   TO COMM-CUSTOMER-INFO.                         00135100
                                                                        00135200
           MOVE 'NAU0'            TO COMM-PREVIOUS-TRANID.              00135300
           MOVE '00'              TO COMM-NEXT-FUNCTION.                00135400
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT               00135500
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.              00135600
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00135700
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00135800
                                                                        00135900
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00136000
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00136100
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME.                00136200
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00136300
                               FROM   (WS-TS-QUEUE)                     00136400
                               LENGTH (WS-ZERO-LENGTH)                  00136500
                               ITEM   (WS-TS-ITEM)                      00136600
                               RESP   (WS-CICS-RESP)                    00136700
                               REWRITE                                  00136800
                               MAIN                                     00136900
                               END-EXEC.                                00137000
                                                                        00137100
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00137200
                GO TO 9999-CICS-ERROR.                                  00137300
                                                                        00137400
           EXEC CICS START TRANSID('NAM1')                              00137500
                           TERMID (EIBTRMID)                            00137600
                           RESP   (WS-CICS-RESP)                        00137700
                           END-EXEC                                     00137800
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00137900
                GO TO 9999-CICS-ERROR.                                  00138000
                                                                        00138100
           EXEC CICS RETURN                                             00138200
                     END-EXEC.                                          00138300
                                                                        00138400
       0110-UPDATE-COMMAND-LINE.                                        00138500
                                                                        00138600
           IF MAP-SYSTEM-CDF = HEX80                                    00138700
               MOVE SPACES TO COMM-SYSTEM-CODE                          00138800
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED                      00138900
           ELSE                                                         00139000
               IF MAP-SYSTEM-CDL > ZERO                                 00139100
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED           00139200
                  MOVE MAP-SYSTEM-CDI TO COMM-SYSTEM-CODE.              00139300
                                                                        00139400
           IF MAP-ACTION-CDF = HEX80                                    00139500
               MOVE SPACES TO COMM-ACTION-CODE                          00139600
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED                      00139700
           ELSE                                                         00139800
               IF MAP-ACTION-CDL > ZERO                                 00139900
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED           00140000
                  MOVE MAP-ACTION-CDI TO COMM-ACTION-CODE.              00140100
                                                                        00140200
           IF MAP-CUST-INFOF = HEX80                                    00140300
               MOVE SPACES TO COMM-CUSTOMER-INFO                        00140400
               MOVE 'Y'            TO COMM-KEY-CHANGED                  00140500
           ELSE                                                         00140600
               IF MAP-CUST-INFOL > ZERO                                 00140700
                  MOVE 'Y'            TO COMM-KEY-CHANGED               00140800
                  MOVE MAP-CUST-INFOI TO COMM-CUSTOMER-INFO.            00140900
                                                                        00141000
                                                                        00141100
       0120-UPDATE-CIM-MAP-FIELDS.                                      00141200
           DISPLAY '0120-UPDATE-CIM-MAP-FIELDS PARA'                    00141300
           IF MAP-FIRST-NAMEF = HEX80                                   00141400
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00141500
               MOVE SPACES TO COM-FIRST-NAME                            00141600
           ELSE                                                         00141700
               IF MAP-FIRST-NAMEL > ZERO                                00141800
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00141900
                  MOVE MAP-FIRST-NAMEI TO COM-FIRST-NAME.               00142000
                                                                        00142100
           IF MAP-MIDDLE-NAMEF = HEX80                                  00142200
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00142300
               MOVE SPACES TO COM-MIDDLE-NAME                           00142400
           ELSE                                                         00142500
               IF MAP-MIDDLE-NAMEL > ZERO                               00142600
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00142700
                  MOVE MAP-MIDDLE-NAMEI TO COM-MIDDLE-NAME.             00142800
                                                                        00142900
           IF MAP-LAST-NAMEF = HEX80                                    00143000
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00143100
               MOVE SPACES TO COM-LAST-NAME                             00143200
           ELSE                                                         00143300
               IF MAP-LAST-NAMEL > ZERO                                 00143400
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00143500
                  MOVE MAP-LAST-NAMEI TO COM-LAST-NAME.                 00143600
                                                                        00143700
           IF MAP-SUFFIX1F = HEX80                                      00143800
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00143900
               MOVE SPACES TO COM-SUFFIX1                               00144000
           ELSE                                                         00144100
               IF MAP-SUFFIX1L > ZERO                                   00144200
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00144300
                  MOVE MAP-SUFFIX1I TO COM-SUFFIX1.                     00144400
                                                                        00144500
           IF MAP-SUFFIX2F = HEX80                                      00144600
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00144700
               MOVE SPACES TO COM-SUFFIX2                               00144800
           ELSE                                                         00144900
               IF MAP-SUFFIX2L > ZERO                                   00145000
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00145100
                  MOVE MAP-SUFFIX2I TO COM-SUFFIX2.                     00145200
                                                                        00145300
           IF MAP-NICKNAMEF = HEX80                                     00145400
               MOVE SPACES TO COM-NICKNAME                              00145500
           ELSE                                                         00145600
               IF MAP-NICKNAMEL > ZERO                                  00145700
                  MOVE MAP-NICKNAMEI TO COM-NICKNAME.                   00145800
                                                                        00145900
R04023     IF MAP-ISSUE-STATEF = HEX80                                  00146000
R04023         MOVE SPACES TO COM-ISSUE-STATE                           00146100
R04023     ELSE                                                         00146200
R04023         IF MAP-ISSUE-STATEL > ZERO                               00146300
R04023            MOVE MAP-ISSUE-STATEI TO COM-ISSUE-STATE.             00146400
                                                                        00146500
      *---------------------------------------------------------------* 00146600
      *    SPECIAL CODING TO INSURE THE COMPANY IN ADDRESS FLAG IS    * 00146700
      *    SET TO YES IF THE COMPANY NAME WAS BLANK PRIOR TO THE      * 00146800
      *    UPDATE.                                                    * 00146900
      *---------------------------------------------------------------* 00147000
                                                                        00147100
      *    IF COM-COMPANY-NAME = SPACES AND MAP-COMP-NAMEI > SPACE      00147200
      *        AND MAP-COMP-IN-ADDL NOT > ZERO                          00147300
      *             MOVE 'Y' TO COM-COMPANY-IN-ADDRESS.                 00147400
                                                                        00147500
           IF MAP-COMP-NAMEF = HEX80                                    00147600
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00147700
               MOVE SPACES TO COM-COMPANY-NAME                          00147800
           ELSE                                                         00147900
               IF MAP-COMP-NAMEL > ZERO                                 00148000
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00148100
                  MOVE MAP-COMP-NAMEI TO COM-COMPANY-NAME               00148200
                                         WS-COMPANY-NAME.               00148300
                                                                        00148400
      *---------------------------------------------------------------* 00148500
      *    ALTERED SEX CODE CAN CHANGE DEFAULT PREFIX ON DISPLAY NAME * 00148600
      *---------------------------------------------------------------* 00148700
           IF MAP-SEXF = HEX80                                          00148800
               MOVE SPACES TO COM-SEX                                   00148900
           ELSE                                                         00149000
               IF MAP-SEXL > ZERO                                       00149100
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00149200
                  MOVE MAP-SEXI TO COM-SEX.                             00149300
                                                                        00149400
                                                                        00149500
T3384      IF MAP-PREFIXF = HEX80                                       00149600
T3384          MOVE SPACES TO COM-PREFIX                                00149700
T3384      ELSE                                                         00149800
T3384          IF MAP-PREFIXL > ZERO                                    00149900
T3384             MOVE 'Y' TO DISPLAY-CHANGED-SW                        00150000
T3384             MOVE MAP-PREFIXI TO COM-PREFIX.                       00150100
                                                                        00150200
           IF DISPLAY-CHANGED-SW = 'Y'                                  00150300
               PERFORM 0300-FORMAT-DISPLAY-NAME.                        00150400
                                                                        00150500
           IF MAP-ADDRESS1F = HEX80                                     00150600
               MOVE SPACES TO COM-ADDRESS1                              00150700
           ELSE                                                         00150800
               IF MAP-ADDRESS1L > ZERO                                  00150900
                  MOVE MAP-ADDRESS1I TO COM-ADDRESS1.                   00151000
                                                                        00151100
           IF MAP-ADDRESS2F = HEX80                                     00151200
               MOVE SPACES TO COM-ADDRESS2                              00151300
           ELSE                                                         00151400
               IF MAP-ADDRESS2L > ZERO                                  00151500
                  MOVE MAP-ADDRESS2I TO COM-ADDRESS2.                   00151600
                                                                        00151700
           IF MAP-BIRTH-DATEF = HEX80                                   00151800
               MOVE ZEROS TO COM-BIRTH-DATE                             00151900
Y2KIMR*                                                                 00152000
Y2KIMR* IMR CHANGE IMR6 BEGIN                                           00152100
Y2KIMR*                                                                 00152200
Y2KIMR      MOVE ZEROS TO Y2K-WK-VARIABLE1                              00152300
Y2KIMR*                                                                 00152400
Y2KIMR* IMR CHANGE IMR6 END                                             00152500
Y2KIMR*                                                                 00152600
           ELSE                                                         00152700
               IF MAP-BIRTH-DATEL > ZERO                                00152800
                  INSPECT MAP-BIRTH-DATEI REPLACING ALL '-' BY 'A'      00152900
Y2KIMR*                                                                 00153000
Y2KIMR* IMR DATE ROUTINE CHANGE BEGIN                                   00153100
Y2KIMR*                                                                 00153200
Y2KIMR*           EXEC CICS BIF DEEDIT FIELD(MAP-BIRTH-DATEI)           00153300
Y2KIMR*                                LENGTH(8)                        00153400
Y2KIMR*                                END-EXEC                         00153500
Y2KIMR*           MOVE MAP-BIRTH-DATEI TO WS-MAP-DATE                   00153600
Y2KIMR*           MOVE WS-MAP-BIF      TO COM-BIRTH-DATE.               00153700
Y2KIMR*                                                                 00153800
MANJU             EXEC CICS BIF DEEDIT FIELD(MAP-BIRTH-DATEI)           00153900
Y2KIMR                                 LENGTH(10)                       00154000
Y2KIMR                                 END-EXEC                         00154100
                  DISPLAY 'MAP-BIRTH-DATEI' MAP-BIRTH-DATEI.            00154200
Y2KIMR            MOVE MAP-BIRTH-DATEI TO Y2K-WK-VARIABLE1              00154300
Y2KIMR*           MOVE Y2K-WK-VARIABLE2-MM TO Y2K-WK-VARIABLE3-BB-MM    00154400
Y2KIMR*           MOVE Y2K-WK-VARIABLE2-DD TO Y2K-WK-VARIABLE3-BB-DD    00154500
Y2KIMR*           MOVE Y2K-WK-VARIABLE2-YY TO Y2K-WK-VARIABLE3-BB-YY    00154600
Y2KIMR*           MOVE Y2K-WK-VARIABLE3 TO WS-MAP-DATE                  00154700
Y2KIMR            MOVE Y2K-WK-VARIABLE2 TO COM-BIRTH-DATE .             00154800
Y2KIMR*                                                                 00154900
Y2KIMR* IMR DATE ROUTINE CHANGE END                                     00155000
Y2KIMR*                                                                 00155100
                                                                        00155200
           IF MAP-SSNF = HEX80                                          00155300
               MOVE SPACES TO COM-SSN                                   00155400
           ELSE                                                         00155500
               IF MAP-SSNL > ZERO                                       00155600
                  MOVE MAP-SSNI TO COM-SSN.                             00155700
                                                                        00155800
           IF MAP-CITYF = HEX80                                         00155900
               MOVE SPACES TO COM-CITY                                  00156000
           ELSE                                                         00156100
               IF MAP-CITYL > ZERO                                      00156200
                  MOVE MAP-CITYI TO COM-CITY.                           00156300
                                                                        00156400
           IF MAP-COMP-IN-ADDF = HEX80                                  00156500
           OR MAP-COMP-IN-ADDI = SPACES                                 00156600
               MOVE 'Y' TO COM-COMPANY-IN-ADDRESS                       00156700
           ELSE                                                         00156800
               IF MAP-COMP-IN-ADDL > ZERO                               00156900
                  MOVE MAP-COMP-IN-ADDI TO COM-COMPANY-IN-ADDRESS.      00157000
                                                                        00157100
           IF MAP-STATEF = HEX80                                        00157200
DFJ            MOVE 'Y' TO STATE-CHANGED-SW                             00157300
               MOVE SPACES TO COM-STATE                                 00157400
           ELSE                                                         00157500
               IF MAP-STATEL > ZERO                                     00157600
DFJ               MOVE 'Y' TO STATE-CHANGED-SW                          00157700
                  MOVE MAP-STATEI TO COM-STATE.                         00157800
                                                                        00157900
           DISPLAY 'COM-ZIP:' COM-ZIP                                   00158076
           IF MAP-ZIPF = HEX80                                          00158100
DFJ            MOVE 'Y' TO ZIP-CHANGED-SW                               00158200
               MOVE SPACES TO COM-ZIP                                   00158300
           ELSE                                                         00158400
               IF MAP-ZIPL > ZERO                                       00158500
DFJ               MOVE 'Y' TO ZIP-CHANGED-SW                            00158600
                  MOVE MAP-ZIPI TO COM-ZIP.                             00158700
           DISPLAY 'COM-ZIP:' COM-ZIP                                   00158876
                                                                        00158900
           IF MAP-ZIP-PLUS4F = HEX80                                    00159000
890989         MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW                         00159100
               MOVE SPACES TO COM-ZIP-PLUS4                             00159200
           ELSE                                                         00159300
               IF MAP-ZIP-PLUS4L > ZERO                                 00159400
890989            MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW                      00159500
                  MOVE MAP-ZIP-PLUS4I TO COM-ZIP-PLUS4.                 00159600
                                                                        00159700
           IF MAP-ADDR-OVRIDEF = HEX80                                  00159800
               MOVE SPACES TO COM-FINALST-OVRD-IND                      00159900
           ELSE                                                         00160000
               IF MAP-ADDR-OVRIDEL > ZERO                               00160100
                  MOVE MAP-ADDR-OVRIDEI TO COM-FINALST-OVRD-IND.        00160200
                                                                        00160300
           MOVE 'N' TO COM-DUP-ADDR-OVRD-IND.                           00160400
                                                                        00160500
           IF MAP-AREA-CODEF = HEX80                                    00160600
               MOVE SPACES TO COM-AREA-CODE                             00160700
           ELSE                                                         00160800
               IF MAP-AREA-CODEL > ZERO                                 00160900
                  MOVE MAP-AREA-CODEI TO COM-AREA-CODE.                 00161000
                                                                        00161100
900837     MOVE 'N' TO PHONE-CHANGED-SW.                                00161200
           MOVE COM-PHONE TO WS-PHONE.                                  00161300
                                                                        00161400
           IF MAP-PHONE3F = HEX80                                       00161500
               MOVE 'Y' TO PHONE-CHANGED-SW                             00161600
               MOVE SPACES TO WS-PHONE3                                 00161700
           ELSE                                                         00161800
               IF MAP-PHONE3L > ZERO                                    00161900
                  MOVE 'Y' TO PHONE-CHANGED-SW                          00162000
                  MOVE MAP-PHONE3I TO WS-PHONE3.                        00162100
                                                                        00162200
           IF MAP-PHONE4F = HEX80                                       00162300
               MOVE 'Y' TO PHONE-CHANGED-SW                             00162400
               MOVE SPACES TO WS-PHONE4                                 00162500
           ELSE                                                         00162600
               IF MAP-PHONE4L > ZERO                                    00162700
                  MOVE 'Y' TO PHONE-CHANGED-SW                          00162800
                  MOVE MAP-PHONE4I TO WS-PHONE4.                        00162900
                                                                        00163000
           IF PHONE-CHANGED-SW = 'Y'                                    00163100
               MOVE WS-PHONE TO COM-PHONE.                              00163200
                                                                        00163300
           IF MAP-PHONE-EXTF = HEX80                                    00163400
               MOVE SPACES TO COM-PHONE-EXTENSION                       00163500
           ELSE                                                         00163600
               IF MAP-PHONE-EXTL > ZERO                                 00163700
                  MOVE MAP-PHONE-EXTI TO COM-PHONE-EXTENSION.           00163800
                                                                        00163900
900837     IF MAP-FAX-AREA-CDF = HEX80                                  00164000
900837         MOVE SPACES TO COM-FAX-AREA-CODE                         00164100
900837     ELSE                                                         00164200
900837         IF MAP-FAX-AREA-CDL > ZERO                               00164300
900837            MOVE MAP-FAX-AREA-CDI TO COM-FAX-AREA-CODE.           00164400
900837                                                                  00164500
900837     MOVE 'N' TO PHONE-CHANGED-SW.                                00164600
900837     MOVE COM-FAX-PHONE TO WS-PHONE.                              00164700
900837                                                                  00164800
900837     IF MAP-FAX-PHONE3F = HEX80                                   00164900
900837         MOVE 'Y' TO PHONE-CHANGED-SW                             00165000
900837         MOVE SPACES TO WS-PHONE3                                 00165100
900837     ELSE                                                         00165200
900837         IF MAP-FAX-PHONE3L > ZERO                                00165300
900837            MOVE 'Y' TO PHONE-CHANGED-SW                          00165400
900837            MOVE MAP-FAX-PHONE3I TO WS-PHONE3.                    00165500
900837                                                                  00165600
900837     IF MAP-FAX-PHONE4F = HEX80                                   00165700
900837         MOVE 'Y' TO PHONE-CHANGED-SW                             00165800
900837         MOVE SPACES TO WS-PHONE4                                 00165900
900837     ELSE                                                         00166000
900837         IF MAP-FAX-PHONE4L > ZERO                                00166100
900837            MOVE 'Y' TO PHONE-CHANGED-SW                          00166200
900837            MOVE MAP-FAX-PHONE4I TO WS-PHONE4.                    00166300
900837                                                                  00166400
900837     IF PHONE-CHANGED-SW = 'Y'                                    00166500
900837         MOVE WS-PHONE TO COM-FAX-PHONE.                          00166600
                                                                        00166700
           IF MAP-ALT-ADDRF = HEX80                                     00166800
               MOVE SPACES TO COM-ALT-ADDRESS-IND                       00166900
           ELSE                                                         00167000
               IF MAP-ALT-ADDRL  > ZERO                                 00167100
                  MOVE MAP-ALT-ADDRI  TO COM-ALT-ADDRESS-IND.           00167200
                                                                        00167300
           IF MAP-ENTITY-TYPEF = HEX80                                  00167400
               MOVE SPACES TO COM-ENTITY-TYPE COM-ENTITY-LITERAL        00167500
           ELSE                                                         00167600
               IF MAP-ENTITY-TYPEL  > ZERO                              00167700
                  MOVE MAP-ENTITY-TYPEI  TO COM-ENTITY-TYPE             00167800
                                            COM-ENTITY-LITERAL.         00167900
MANJU2      DISPLAY 'COM-ENTITY-TYPE' COM-ENTITY-TYPE.                  00168000
R00700     IF MAP-EMAIL1F = HEX80                                       00168100
R00700         MOVE SPACES TO COM-EMAIL1                                00168200
R00700     ELSE                                                         00168300
R00700         IF MAP-EMAIL1L  > ZERO                                   00168400
R00700            MOVE MAP-EMAIL1I  TO COM-EMAIL1.                      00168500
                                                                        00168600
                                                                        00168700
           IF MAP-DIS-CHG-INDF = HEX80                                  00168800
               MOVE SPACES TO COM-DISP-IND                              00168900
           ELSE                                                         00169000
               IF MAP-DIS-CHG-INDL  > ZERO                              00169100
                  MOVE MAP-DIS-CHG-INDI  TO COM-DISP-IND                00169200
                                                                        00169300
           IF MAP-FUTURE-ADDRF = HEX80                                  00169400
               MOVE SPACES TO COM-FUTURE-ADDRESS-IND                    00169500
           ELSE                                                         00169600
               IF MAP-FUTURE-ADDRL  > ZERO                              00169700
                  MOVE MAP-FUTURE-ADDRI  TO COM-FUTURE-ADDRESS-IND.     00169800
                                                                        00169900
           IF MAP-CUST-STATUSF = HEX80                                  00170000
               MOVE SPACES TO COM-RECORD-STATUS                         00170100
           ELSE                                                         00170200
               IF MAP-CUST-STATUSL  > ZERO                              00170300
                  MOVE MAP-CUST-STATUSI TO COM-RECORD-STATUS            00170400
DFJ               IF COM-RECORD-STATUS = 'C'                            00170500
DFJ                   MOVE 'Y' TO STATUS-CHANGED-SW.                    00170600
                                                                        00170700
           IF MAP-EFF-DATEF = HEX80                                     00170800
               MOVE ZEROS TO COM-EFFECTIVE-DATE                         00170900
           ELSE                                                         00171000
               IF MAP-EFF-DATEL  > ZERO                                 00171100
                  INSPECT MAP-EFF-DATEI REPLACING ALL '-' BY 'A'        00171200
      *           EXEC CICS BIF DEEDIT FIELD(MAP-EFF-DATEI)             00171300
      *                                LENGTH(8)                        00171400
      *                                END-EXEC                         00171500
                  MOVE MAP-EFF-DATEI TO WS-MAP-DATE                     00171600
                  MOVE WS-MAP-BIF    TO COM-EFFECTIVE-DATE.             00171700
                                                                        00171800
                                                                        00171900
           IF MAP-ASSOC1F = HEX80                                       00172000
               MOVE SPACES TO COM-ASSOCIATION1                          00172100
           ELSE                                                         00172200
               IF MAP-ASSOC1L  > ZERO                                   00172300
                  MOVE MAP-ASSOC1I  TO COM-ASSOCIATION1.                00172400
                                                                        00172500
           IF MAP-ASSOC2F = HEX80                                       00172600
               MOVE SPACES TO COM-ASSOCIATION2                          00172700
           ELSE                                                         00172800
               IF MAP-ASSOC2L  > ZERO                                   00172900
                  MOVE MAP-ASSOC2I  TO COM-ASSOCIATION2.                00173000
                                                                        00173100
           IF MAP-ASSOC3F = HEX80                                       00173200
               MOVE SPACES TO COM-ASSOCIATION3                          00173300
           ELSE                                                         00173400
               IF MAP-ASSOC3L  > ZERO                                   00173500
                  MOVE MAP-ASSOC3I  TO COM-ASSOCIATION3.                00173600
                                                                        00173700
R02539     IF MAP-COUNTRY-CDF = HEX80                                   00173800
R02539         MOVE SPACES TO COM-COUNTRY-CODE                          00173900
R02539     ELSE                                                         00174000
R02539         IF MAP-COUNTRY-CDL > ZERO                                00174100
R02539            MOVE MAP-COUNTRY-CDI TO COM-COUNTRY-CODE.             00174200
                                                                        00174300
R02539     IF MAP-POSTAL-CDF = HEX80                                    00174400
R02539         MOVE SPACES TO COM-POSTAL-CODE                           00174500
R02539     ELSE                                                         00174600
R02539         IF MAP-POSTAL-CDL > ZERO                                 00174700
R02539            MOVE MAP-POSTAL-CDI TO COM-POSTAL-CODE.               00174800
           DISPLAY 'COM-COUNTRY-CODE' COM-COUNTRY-CODE.                 00174900
           DISPLAY 'COM-POSTAL-CODEE' COM-POSTAL-CODE.                  00175000
           EXEC CICS ASKTIME ABSTIME (WS-TIME-FIELD)                    00175100
                             RESP    (WS-CICS-RESP)                     00175200
                             END-EXEC.                                  00175300
                                                                        00175400
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00175500
               GO TO 9999-CICS-ERROR.                                   00175600
                                                                        00175700
           EXEC CICS FORMATTIME ABSTIME (WS-TIME-FIELD)                 00175800
                                MMDDYY  (WS-MAP-BIF)                    00175900
                                RESP    (WS-CICS-RESP)                  00176000
                                END-EXEC.                               00176100
                                                                        00176200
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00176300
               GO TO 9999-CICS-ERROR.                                   00176400
                                                                        00176500
           MOVE WS-MAP-BIF       TO COM-CHANGE-DATE.                    00176600
                                                                        00176700
                                                                        00176800
       0130-EDIT-NA340M1-MAP.                                           00176900
           DISPLAY '0130-EDIT-NA340M1-MAP PARA'                         00177000
           MOVE 'N' TO WS-EDIT-SW.                                      00177100
           DISPLAY 'MAP-COUNTRY-CDO:' COM-COUNTRY-CODE.                 00177200
                                                                        00177300
R08986*    PERFORM R07063-BUILD-PROG-RULES-TABLE THRU                   00177400
R08986*            R07063-BUILD-PROG-RULES-EXIT.                        00177500
      *---------------------------------------------------------------* 00177600
      * EDIT CUSTOMER TYPE FOR VALIDITY                               * 00177700
      *---------------------------------------------------------------* 00177800
           DISPLAY 'COM-ENTITY-LITERAL' COM-ENTITY-LITERAL              00177900
           INSPECT COM-ENTITY-LITERAL                                   00178000
               REPLACING ALL 'a' BY 'A'                                 00178100
                             'b' BY 'B'                                 00178200
                             'c' BY 'C'                                 00178300
                             'd' BY 'D'                                 00178400
                             'e' BY 'E'                                 00178500
                             'f' BY 'F'                                 00178600
                             'g' BY 'G'                                 00178700
                             'h' BY 'H'                                 00178800
                             'i' BY 'I'                                 00178900
                             'j' BY 'J'                                 00179000
                             'k' BY 'K'                                 00179100
                             'l' BY 'L'                                 00179200
                             'm' BY 'M'                                 00179300
                             'n' BY 'N'                                 00179400
                             'o' BY 'O'.                                00179500
                                                                        00179600
           INSPECT COM-ENTITY-LITERAL                                   00179700
              REPLACING  ALL 'p' BY 'P'                                 00179800
                             'q' BY 'Q'                                 00179900
                             'r' BY 'R'                                 00180000
                             's' BY 'S'                                 00180100
                             't' BY 'T'                                 00180200
                             'u' BY 'U'                                 00180300
                             'v' BY 'V'                                 00180400
                             'w' BY 'W'                                 00180500
                             'x' BY 'X'                                 00180600
                             'y' BY 'Y'                                 00180700
                             'z' BY 'Z'.                                00180800
      *---------------------------------------------------------------* 00180900
      *   NOTE MAKE SURE WT-C8997A-SYSTEM-DESC IS IN UPPER CASE ON    * 00181000
      *   THE TURBO SYSTEM.                                           * 00181100
      *---------------------------------------------------------------* 00181200
                                                                        00181300
           MOVE COM-ENTITY-LITERAL TO WS-ENTITY-LITERAL.                00181400
                                                                        00181500
           IF  WS-LITERAL-LOW-ORDR  >  ' '                              00181600
               MOVE COM-ENTITY-LITERAL TO WT-C8997A-KEY                 00181700
               MOVE WT-CNTL8997A       TO TURBO-CNTL-AREA               00181800
               PERFORM 10000-CALL-GSF                                   00181900
               MOVE TURBO-CNTL-AREA    TO WT-CNTL8997A                  00182000
               MOVE ZERO TO WT-C8997A-RETURN                            00182100
               IF  WT-C8997A-RETURN = ZERO                              00182200
                   MOVE ZERO TO WT-C8997-RETURN                         00182300
      *            MOVE WT-C8997A-SYSTEM-CD TO COM-ENTITY-TYPE          00182400
               ELSE                                                     00182500
                   MOVE 'Y'      TO WS-EDIT-SW                          00182600
                   MOVE -1       TO MAP-ENTITY-TYPEL                    00182700
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ENTITY-TYPEA        00182800
C08784***          MOVE 'NA004'  TO WS-HOLD-MESSAGE                     00182900
C08784             MOVE 'NA400'  TO WS-HOLD-MESSAGE                     00183000
                   PERFORM 0170-SLIDE-ERROR-MESSAGES                    00183100
           ELSE                                                         00183200
               MOVE COM-ENTITY-TYPE    TO WT-C8997-SYSTEM-CD            00183300
               MOVE WT-CNTL8997        TO TURBO-CNTL-AREA               00183400
               PERFORM 10000-CALL-GSF                                   00183500
               MOVE TURBO-CNTL-AREA    TO WT-CNTL8997                   00183600
               MOVE ZERO TO WT-C8997-RETURN                             00183700
               IF  WT-C8997-RETURN NOT = ZERO                           00183800
                   MOVE 'Y'      TO WS-EDIT-SW                          00183900
                   MOVE -1       TO MAP-ENTITY-TYPEL                    00184000
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ENTITY-TYPEA        00184100
                   MOVE 'NA004'  TO WS-HOLD-MESSAGE                     00184200
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00184300
                                                                        00184400
      *---------------------------------------------------------------* 00184500
      * DETERMINE IF THERE IS A BROKER ON THE BROKER TABLE IF THERE   * 00184600
      * IS THE CUSTOMER MAY NOT CHANGE THE ENTITY TYPE WITHOUT        * 00184700
      * DELETING THE BROKER FROM THE SYSTEM.                          * 00184800
      *---------------------------------------------------------------* 00184900
                                                                        00185000
      *    IF COMM-CUST-TYPE  = 'BR' AND COM-ENTITY-TYPE NOT = 'BR'     00185100
      *        EXEC SQL                                                 00185200
      *            OPEN CONTCUR                                         00185300
      *        END-EXEC                                                 00185400
      *        PERFORM 0140-SQL-ERROR-CHECK                             00185500
                                                                        00185600
      *        EXEC SQL FETCH CONTCUR                                   00185700
      *            INTO :DISPLAY-COUNT                                  00185800
      *        END-EXEC                                                 00185900
      *        PERFORM 0140-SQL-ERROR-CHECK                             00186000
                                                                        00186100
      *        EXEC SQL                                                 00186200
      *            CLOSE CONTCUR                                        00186300
      *        END-EXEC                                                 00186400
      *        PERFORM 0140-SQL-ERROR-CHECK                             00186500
               MOVE ZERO TO DISPLAY-COUNT                               00186600
               IF DISPLAY-COUNT > ZERO                                  00186700
                   MOVE 'Y'      TO WS-EDIT-SW                          00186800
                   MOVE -1       TO MAP-ENTITY-TYPEL                    00186900
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ENTITY-TYPEA        00187000
                   MOVE 'NA160'  TO WS-HOLD-MESSAGE                     00187100
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00187200
                                                                        00187300
      *---------------------------------------------------------------* 00187400
      * EDIT CUSTOMER TYPE                                            * 00187500
      *---------------------------------------------------------------* 00187600
                                                                        00187700
           IF  COMM-CUST-TYPE   = 'CA'                                  00187800
               IF  WS-COMPANY-NAME-LOW-ORDR    >  ' '                   00187900
                   MOVE 'Y'      TO WS-EDIT-SW                          00188000
                   MOVE -1       TO MAP-COMP-NAMEL                      00188100
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-COMP-NAMEA          00188200
                   MOVE 'NA185'  TO WS-HOLD-MESSAGE                     00188300
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00188400
                                                                        00188500
      *---------------------------------------------------------------* 00188600
      * EDIT CUSTOMER TYPE                                            * 00188700
      *---------------------------------------------------------------* 00188800
                                                                        00188900
           IF COM-ENTITY-LITERAL = 'CA' OR 'CASE'                       00189000
                   MOVE 'Y'      TO WS-EDIT-SW                          00189100
                   MOVE -1       TO MAP-ENTITY-TYPEL                    00189200
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ENTITY-TYPEA        00189300
                   MOVE 'NA165'  TO WS-HOLD-MESSAGE                     00189400
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00189500
                                                                        00189600
      *---------------------------------------------------------------* 00189700
      * EDIT CUSTOMER TYPE FOR CHANGE (CAM, CASE) TO (BROKER, MAIL)   * 00189800
      *---------------------------------------------------------------* 00189900
                                                                        00190000
           IF COM-CASE-SW = 'Y' AND (COM-ENTITY-TYPE = 'ML' OR          00190100
                   COM-ENTITY-TYPE = 'BR')                              00190200
                   MOVE 'Y'      TO WS-EDIT-SW                          00190300
                   MOVE -1       TO MAP-ENTITY-TYPEL                    00190400
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ENTITY-TYPEA        00190500
                   MOVE 'NA165'  TO WS-HOLD-MESSAGE                     00190600
                   PERFORM 0170-SLIDE-ERROR-MESSAGES                    00190700
           ELSE                                                         00190800
           IF COM-CASE-SW NOT = 'Y' AND (COM-ENTITY-TYPE = 'CA' OR      00190900
                   COM-ENTITY-TYPE = 'AM')                              00191000
                   MOVE 'Y'      TO WS-EDIT-SW                          00191100
                   MOVE -1       TO MAP-ENTITY-TYPEL                    00191200
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ENTITY-TYPEA        00191300
                   MOVE 'NA165'  TO WS-HOLD-MESSAGE                     00191400
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00191500
                                                                        00191600
      *---------------------------------------------------------------* 00191700
      * EDIT CUSTOMER STATUS FOR VALIDITY                             * 00191800
      *---------------------------------------------------------------* 00191900
           MOVE COM-RECORD-STATUS  TO WT-C2006-STATUS-CD.               00192000
           MOVE WT-CNTL2006        TO TURBO-CNTL-AREA.                  00192100
           PERFORM 10000-CALL-GSF.                                      00192200
           MOVE TURBO-CNTL-AREA    TO WT-CNTL2006.                      00192300
           MOVE ZERO TO WT-C2006-RETURN                                 00192400
           IF  WT-C2006-RETURN NOT = ZERO                               00192500
                   MOVE 'Y'      TO WS-EDIT-SW                          00192600
                   MOVE -1       TO MAP-CUST-STATUSL                    00192700
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-CUST-STATUSA        00192800
                   MOVE 'NA005'  TO WS-HOLD-MESSAGE                     00192900
                   PERFORM 0170-SLIDE-ERROR-MESSAGES                    00193000
           ELSE                                                         00193100
              IF COM-ENTITY-TYPE = 'BR' OR 'ML'                         00193200
                 MOVE COM-RECORD-STATUS  TO WS-CUSTOMER-STATUS          00193300
                 IF  INVALID-CUSTOMER-STATUS                            00193400
                     MOVE 'Y'      TO WS-EDIT-SW                        00193500
                     MOVE -1       TO MAP-CUST-STATUSL                  00193600
                     MOVE ATTRB-UNPROT-BRT-PEN TO MAP-CUST-STATUSA      00193700
                     MOVE 'NA005'  TO WS-HOLD-MESSAGE                   00193800
                     PERFORM 0170-SLIDE-ERROR-MESSAGES                  00193900
                 END-IF                                                 00194000
              END-IF                                                    00194100
           END-IF.                                                      00194200
                                                                        00194300
                                                                        00194400
      *---------------------------------------------------------------* 00194500
      * EDIT CUSTOMER SSN STATUS FOR VALIDITY                         * 00194600
      *---------------------------------------------------------------* 00194700
           IF COM-SSN NOT = SPACES AND COM-SSN NOT = LOW-VALUES         00194800
               IF COM-SSN NOT NUMERIC                                   00194900
                   MOVE 'Y'      TO WS-EDIT-SW                          00195000
                   MOVE -1       TO MAP-SSNL                            00195100
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-SSNA                00195200
                   MOVE 'NA031'  TO WS-HOLD-MESSAGE                     00195300
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00195400
                                                                        00195500
      *---------------------------------------------------------------* 00195600
      * EDIT RE-ASSEMBLE NAME CHANGE INDICATOR                        * 00195700
      *---------------------------------------------------------------* 00195800
                                                                        00195900
           IF COM-DISP-IND  NOT = 'Y' AND                               00196000
               COM-DISP-IND  NOT = 'N'                                  00196100
                   MOVE 'Y'      TO WS-EDIT-SW                          00196200
                   MOVE -1       TO MAP-DIS-CHG-INDL                    00196300
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-DIS-CHG-INDA        00196400
                   MOVE 'NA180'  TO WS-HOLD-MESSAGE                     00196500
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00196600
                                                                        00196700
      *---------------------------------------------------------------* 00196800
      * EDIT CUSTOMER PHONE NUMBER FOR VALIDITY                       * 00196900
      *---------------------------------------------------------------* 00197000
           IF COM-AREA-CODE NOT = SPACES AND                            00197100
               COM-AREA-CODE NOT = LOW-VALUES                           00197200
               IF COM-AREA-CODE NOT NUMERIC                             00197300
                   MOVE 'Y'      TO WS-EDIT-SW                          00197400
                   MOVE -1       TO MAP-AREA-CODEL                      00197500
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-AREA-CODEA          00197600
                   MOVE 'NA032'  TO WS-HOLD-MESSAGE                     00197700
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00197800
                                                                        00197900
           IF COM-PHONE NOT = SPACES AND                                00198000
               COM-PHONE NOT = LOW-VALUES                               00198100
               IF COM-PHONE NOT NUMERIC                                 00198200
                   MOVE 'Y'      TO WS-EDIT-SW                          00198300
                   MOVE -1       TO MAP-PHONE3L                         00198400
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-PHONE3A             00198500
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-PHONE4A             00198600
                   MOVE 'NA033'  TO WS-HOLD-MESSAGE                     00198700
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00198800
                                                                        00198900
           IF COM-PHONE-EXTENSION NOT = SPACES AND                      00199000
               COM-PHONE-EXTENSION NOT = LOW-VALUES                     00199100
               IF COM-PHONE-EXTENSION NOT NUMERIC                       00199200
                   MOVE 'Y'      TO WS-EDIT-SW                          00199300
                   MOVE -1       TO MAP-PHONE-EXTL                      00199400
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-PHONE-EXTA          00199500
                   MOVE 'NA034'  TO WS-HOLD-MESSAGE                     00199600
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00199700
                                                                        00199800
900837*---------------------------------------------------------------* 00199900
900837* EDIT FAX PHONE NUMBER FOR VALIDITY                            * 00200000
900837*---------------------------------------------------------------* 00200100
900837     IF COM-FAX-AREA-CODE NOT = SPACES AND                        00200200
900837         COM-FAX-AREA-CODE NOT = LOW-VALUES                       00200300
900837         IF COM-FAX-AREA-CODE NOT NUMERIC                         00200400
900837             MOVE 'Y'      TO WS-EDIT-SW                          00200500
900837             MOVE -1       TO MAP-FAX-AREA-CDL                    00200600
900837             MOVE ATTRB-UNPROT-BRT-PEN TO MAP-FAX-AREA-CDA        00200700
900837             MOVE 'NA186'  TO WS-HOLD-MESSAGE                     00200800
900837             PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00200900
900837                                                                  00201000
900837     IF COM-FAX-PHONE NOT = SPACES AND                            00201100
900837         COM-FAX-PHONE NOT = LOW-VALUES                           00201200
900837         IF COM-FAX-PHONE NOT NUMERIC                             00201300
900837             MOVE 'Y'      TO WS-EDIT-SW                          00201400
900837             MOVE -1       TO MAP-FAX-PHONE3L                     00201500
900837             MOVE ATTRB-UNPROT-BRT-PEN TO MAP-FAX-PHONE3A         00201600
900837                                          MAP-FAX-PHONE4A         00201700
900837             MOVE 'NA187'  TO WS-HOLD-MESSAGE                     00201800
900837             PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00201900
                                                                        00202000
      *---------------------------------------------------------------* 00202100
      * EDIT COMPANY IN ADDRESS FIELD FOR VALIDITY                    * 00202200
      *---------------------------------------------------------------* 00202300
      *    IF COM-COMPANY-NAME = SPACES OR COM-COMPANY-NAME = LOW-VALUES00202400
      *        MOVE 'Y' TO COM-COMPANY-IN-ADDRESS.                      00202500
      *    IF COM-COMPANY-IN-ADDRESS  NOT = SPACES AND                  00202600
      *        COM-COMPANY-IN-ADDRESS  NOT = LOW-VALUES                 00202700
           IF  COM-COMPANY-IN-ADDRESS NOT = 'Y' AND                     00202800
               COM-COMPANY-IN-ADDRESS NOT = 'N'                         00202900
               MOVE 'Y'      TO WS-EDIT-SW                              00203000
               MOVE -1       TO MAP-COMP-IN-ADDL                        00203100
               MOVE ATTRB-UNPROT-BRT-PEN TO MAP-COMP-IN-ADDA            00203200
               MOVE 'NA035'  TO WS-HOLD-MESSAGE                         00203300
               PERFORM 0170-SLIDE-ERROR-MESSAGES.                       00203400
                                                                        00203500
      *---------------------------------------------------------------* 00203600
      * EDIT ADDRESS OVERRIDE INDICATOR FOR VALIDITY                  * 00203700
      *---------------------------------------------------------------* 00203800
           IF COM-FINALST-OVRD-IND NOT = 'N' AND                        00203900
              COM-FINALST-OVRD-IND NOT = 'Y'                            00204000
                   MOVE 'Y'      TO WS-EDIT-SW                          00204100
                   MOVE -1       TO MAP-ADDR-OVRIDEL                    00204200
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDR-OVRIDEA        00204300
                   MOVE 'NA036'  TO WS-HOLD-MESSAGE                     00204400
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00204500
                                                                        00204600
      *---------------------------------------------------------------* 00204700
      * CALL NA205 INTERFACE TO FINALIST TO VERIFY/CORRECT ADDRESS.   * 00204800
      * IF ADDRESS CORRECTION HAS BEEN OVERRIDEN THEN SKIP.           * 00204900
      *---------------------------------------------------------------* 00205000
107987     IF COM-ZIP NOT =  SPACES                                     00205100
107987        AND COM-ZIP NOT NUMERIC                                   00205200
107987        MOVE 'NA194'  TO WS-HOLD-MESSAGE                          00205300
107987        MOVE 'N' TO COM-FINALIST-SW                               00205400
107987        PERFORM 0170-SLIDE-ERROR-MESSAGES                         00205500
107987     END-IF                                                       00205600
107987     IF COM-ZIP EQUAL SPACES OR ZEROS                             00205700
107987        AND COM-FINALST-OVRD-IND = 'Y'                            00205800
107987        MOVE 'NA194'  TO WS-HOLD-MESSAGE                          00205900
107987        MOVE 'N' TO COM-FINALIST-SW                               00206000
107987        PERFORM 0170-SLIDE-ERROR-MESSAGES                         00206100
107987     END-IF                                                       00206200
           IF COM-FINALST-OVRD-IND = 'N' AND                            00206300
R02539        COM-ZIP NOT EQUAL '00100'                                 00206400
               PERFORM 0150-FINALST-ADDRESS-CHECK                       00206500
           ELSE                                                         00206600
             IF COM-FINALST-OVRD-IND = 'Y'                              00206700
               IF COM-FINALIST-SW NOT = 'Y' AND                         00206800
R02539            COM-ZIP NOT EQUAL '00100'                             00206900
               MOVE 'Y' TO  COM-FINALIST-SW                             00207000
               MOVE 'N' TO COM-FINALST-OVRD-IND                         00207100
               PERFORM 0150-FINALST-ADDRESS-CHECK.                      00207200
                                                                        00207300
      *---------------------------------------------------------------* 00207400
      *    NA205 WILL RETURN A '9' IN FIRST BYTE OF RETURN CODE IF    * 00207500
      *    SOMETHING WAS WRONG WITH THE ADDRESS.                      * 00207600
      *---------------------------------------------------------------* 00207700
           IF WS-FINALST-BYTE1 = '9' AND COM-FINALST-OVRD-IND = 'N'     00207800
                   MOVE 'Y'      TO WS-EDIT-SW                          00207900
                   MOVE -1       TO MAP-ADDRESS1L                       00208000
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A           00208100
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS2A           00208200
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-CITYA               00208300
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-STATEA              00208400
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ZIPA                00208500
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ZIP-PLUS4A          00208600
                   MOVE 'NA037'  TO WS-HOLD-MESSAGE                     00208700
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00208800
                                                                        00208900
      *---------------------------------------------------------------* 00209000
      * EDIT ADDRESS FIELDS FOR SPACES                                * 00209100
      *---------------------------------------------------------------* 00209200
           IF COM-ADDRESS1 = SPACES AND COM-ADDRESS2 = SPACES           00209300
                   MOVE 'Y'      TO WS-EDIT-SW                          00209400
                   MOVE -1       TO MAP-ADDRESS1L                       00209500
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A           00209600
                   MOVE 'NA182'  TO WS-HOLD-MESSAGE                     00209700
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00209800
                                                                        00209900
      *---------------------------------------------------------------* 00210000
      * EDIT CITY FIELD FOR SPACES                                    * 00210100
      *---------------------------------------------------------------* 00210200
           IF COM-CITY = SPACES                                         00210300
                   MOVE 'Y'      TO WS-EDIT-SW                          00210400
                   MOVE -1       TO MAP-CITYL                           00210500
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-CITYA               00210600
                   MOVE 'NA183'  TO WS-HOLD-MESSAGE                     00210700
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00210800
                                                                        00210900
      *---------------------------------------------------------------* 00211000
      * EDIT STATE FIELD FOR SPACES                                   * 00211100
      *---------------------------------------------------------------* 00211200
           IF COM-STATE = SPACES                                        00211300
                   MOVE 'Y'      TO WS-EDIT-SW                          00211400
                   MOVE -1       TO MAP-STATEL                          00211500
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-STATEA              00211600
                   MOVE 'NA184'  TO WS-HOLD-MESSAGE                     00211700
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00211800
                                                                        00211900
      *---------------------------------------------------------------* 00212000
      * EDIT EFFECTIVE DATE FOR VALIDITY                              * 00212100
      *---------------------------------------------------------------* 00212200
                                                                        00212300
           MOVE COM-EFFECTIVE-DATE TO WS-DATE-R.                        00212400
           MOVE WS-DATE-R          TO WS-DATE.                          00212500
           MOVE WS-MM              TO INPUT-GREGORIAN-MM.               00212600
           MOVE WS-DD              TO INPUT-GREGORIAN-DD.               00212700
           MOVE WS-YY              TO INPUT-GREGORIAN-YY.               00212800
           MOVE 'VG'               TO OPERATION-TO-PERFORM.             00212900
           MOVE 'G'                TO INPUT-DATE-FORMAT-CODE.           00213000
MANJU *    CALL COB2-DATEPRG2 USING WS-DATEPRG2-PARAMETERS.             00213100
      *    MOVE ZEROS              TO WS-DATE-R.                        00213200
      *    IF ERRORS                                                    00213300
      *            MOVE 'Y'      TO WS-EDIT-SW                          00213400
      *            MOVE -1       TO MAP-EFF-DATEL                       00213500
      *            MOVE ATTRB-UNPROT-BRT-PEN TO MAP-EFF-DATEA           00213600
      *            MOVE 'NA006'  TO WS-HOLD-MESSAGE                     00213700
      *            PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00213800
                                                                        00213900
      *---------------------------------------------------------------* 00214000
      * EDIT BIRTH DATE FOR VALIDITY                                  * 00214100
      *---------------------------------------------------------------* 00214200
                                                                        00214300
Y2KIMR*                                                                 00214400
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE BEGIN                             00214500
Y2KIMR*                                                                 00214600
Y2KIMR*    IF COM-BIRTH-DATE NOT = ZEROES                               00214700
Y2KIMR*            MOVE COM-BIRTH-DATE  TO WS-DATE-R                    00214800
Y2KIMR*            MOVE WS-DATE-R       TO WS-DATE                      00214900
Y2KIMR*            MOVE WS-MM           TO INPUT-GREGORIAN-MM           00215000
Y2KIMR*            MOVE WS-DD           TO INPUT-GREGORIAN-DD           00215100
Y2KIMR*            MOVE WS-YY           TO INPUT-GREGORIAN-YY           00215200
Y2KIMR*            MOVE 'VG'            TO OPERATION-TO-PERFORM         00215300
Y2KIMR*            MOVE 'G'             TO INPUT-DATE-FORMAT-CODE       00215400
Y2KIMR*            CALL COB2-DATEPRG2 USING WS-DATEPRG2-PARAMETERS      00215500
Y2KIMR*            MOVE ZEROS           TO WS-DATE-R.                   00215600
Y2KIMR*                                                                 00215700
990513**   IF BIRTH-DATE  EQUAL '1901-01-01' OR  '0001-01-01'           00215800
990513**                  OR    '1900-01-01'                            00215900
990513**                  OR    '1111-11-11'                            00216000
990513     IF COM-ENTITY-TYPE NOT = 'BR' AND 'ML'                       00216100
Y2KIMR        NEXT SENTENCE                                             00216200
Y2KIMR     ELSE                                                         00216300
Y2KIMR     IF COM-BIRTH-DATE NOT = ZEROES                               00216400
Y2KIMR     AND  COM-BIRTH-DATE IS NUMERIC                               00216500
Y2KIMR*            MOVE COM-BIRTH-DATE  TO WS-DATE-R                    00216600
Y2KIMR*            MOVE WS-DATE-R       TO WS-DATE                      00216700
Y2KIMR*            MOVE WS-MM           TO INPUT-GREGORIAN-MM           00216800
Y2KIMR*            MOVE WS-DD           TO INPUT-GREGORIAN-DD           00216900
Y2KIMR*            MOVE WS-YY           TO INPUT-GREGORIAN-YY           00217000
Y2KIMR*            MOVE 'VG'            TO OPERATION-TO-PERFORM         00217100
Y2KIMR*                                                                 00217200
990428             MOVE COM-BIRTH-DATE TO Y2K-WK-VARIABLE2              00217300
Y2KIMR             MOVE Y2K-WK-VARIABLE2 TO INPUT-4-GREG-DATE           00217400
Y2KIMR**           MOVE COM-BIRTH-DATE  TO INPUT-4-GREG-DATE            00217500
Y2KIMR             MOVE 'FG'            TO OPERATION-TO-PERFORM         00217600
Y2KIMR             MOVE 'G'             TO INPUT-DATE-FORMAT-CODE       00217700
Y2KIMR*            CALL COB2-DATEPRG2 USING WS-DATEPRG2-PARAMETERS      00217800
Y2KIMR             MOVE ZEROS           TO WS-DATE-R.                   00217900
Y2KIMR*                                                                 00218000
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE END                               00218100
Y2KIMR*                                                                 00218200
                                                                        00218300
990513**   IF BIRTH-DATE  EQUAL '1901-01-01' OR  '0001-01-01'           00218400
990513**                  OR    '1900-01-01'                            00218500
990513**                  OR    '1111-11-11'                            00218600
990513     IF COM-ENTITY-TYPE NOT = 'BR' AND 'ML'                       00218700
Y2KIMR        NEXT SENTENCE                                             00218800
Y2KIMR     ELSE                                                         00218900
           IF COM-BIRTH-DATE NOT = ZEROES                               00219000
Y2KIMR     AND  COM-BIRTH-DATE IS NUMERIC                               00219100
                   IF ERRORS                                            00219200
                       MOVE 'Y'      TO WS-EDIT-SW                      00219300
                       MOVE -1       TO MAP-BIRTH-DATEL                 00219400
                       MOVE ATTRB-UNPROT-BRT-PEN TO MAP-BIRTH-DATEA     00219500
                       MOVE 'NA038'  TO WS-HOLD-MESSAGE                 00219600
                       PERFORM 0170-SLIDE-ERROR-MESSAGES.               00219700
                                                                        00219800
      *---------------------------------------------------------------* 00219900
      * EDIT FIRST MIDDLE LAST AND COMPANY FOR ENTRY                  * 00220000
      *---------------------------------------------------------------* 00220100
                                                                        00220200
           IF COM-LAST-NAME = SPACES AND COM-FIRST-NAME = SPACES AND    00220300
               COM-MIDDLE-NAME = SPACES AND COM-COMPANY-NAME = SPACES   00220400
                   MOVE 'Y'      TO WS-EDIT-SW                          00220500
                   MOVE -1       TO MAP-FIRST-NAMEL                     00220600
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-FIRST-NAMEA         00220700
                   MOVE 'NA027'  TO WS-HOLD-MESSAGE                     00220800
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00220900
                                                                        00221000
      *---------------------------------------------------------------* 00221100
      * EDIT LAST NAME SPACES WHEN FIRST AND MIDDLE ENTERED           * 00221200
      *---------------------------------------------------------------* 00221300
                                                                        00221400
           IF COM-FIRST-NAME NOT = SPACES AND                           00221500
               COM-MIDDLE-NAME NOT = SPACES AND COM-LAST-NAME = SPACES  00221600
               OR COM-FIRST-NAME NOT = SPACES AND COM-LAST-NAME = SPACES00221700
               OR COM-MIDDLE-NAME NOT = SPACES AND COM-LAST-NAME = SPACE00221800
                   MOVE 'Y'      TO WS-EDIT-SW                          00221900
                   MOVE -1       TO MAP-LAST-NAMEL                      00222000
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-LAST-NAMEA          00222100
                   MOVE 'NA161'  TO WS-HOLD-MESSAGE                     00222200
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00222300
                                                                        00222400
      *---------------------------------------------------------------* 00222500
      * EDIT FIRST NAME WHEN LAST NAME ENTERED                        * 00222600
      *---------------------------------------------------------------* 00222700
                                                                        00222800
           IF COM-LAST-NAME NOT = SPACES AND COM-FIRST-NAME = SPACE     00222900
                   MOVE 'Y'      TO WS-EDIT-SW                          00223000
                   MOVE -1       TO MAP-FIRST-NAMEL                     00223100
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-FIRST-NAMEA         00223200
                   MOVE 'NA161'  TO WS-HOLD-MESSAGE                     00223300
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00223400
                                                                        00223500
                                                                        00223600
      *---------------------------------------------------------------* 00223700
      * DECIDE IF RECORD BELONGS TO A COMPANY OR AGENT                * 00223800
      *---------------------------------------------------------------* 00223900
           DISPLAY 'COMM-SYSTEM-CODE' COMM-SYSTEM-CODE.                 00224000
           DISPLAY 'COMM-ACTION-CODE' COMM-ACTION-CODE.                 00224100
           IF (COM-LAST-NAME = SPACES AND COM-FIRST-NAME = SPACES AND   00224200
               COM-MIDDLE-NAME = SPACES) OR                             00224300
               COM-COMPANY-NAME NOT = SPACES                            00224400
      *      (COMM-SYSTEM-CODE = 'NA' AND                               00224500
      *       COMM-ACTION-CODE(1:1) = 'U')                              00224600
                   MOVE 'Y' TO COM-COMPANY-IND                          00224700
                   DISPLAY 'COM-COMPANY-IND' COM-COMPANY-IND            00224800
                   UNSTRING COM-COMPANY-NAME                            00224900
                       DELIMITED BY ALL ' ' OR ','                      00225000
                           INTO COM-NAME-KEY1                           00225100
                                COM-NAME-KEY2                           00225200
                                COM-NAME-KEY3                           00225300
           ELSE                                                         00225400
              MOVE 'N' TO COM-COMPANY-IND                               00225500
              MOVE COM-LAST-NAME   TO COM-NAME-KEY1                     00225600
              MOVE COM-FIRST-NAME  TO COM-NAME-KEY2                     00225700
              MOVE COM-MIDDLE-NAME TO COM-NAME-KEY3                     00225800
R04023*       IF (COM-CARRIER-CODE = 'SH')                              00225900
R04254*       IF (COM-CARRIER-CODE = 'SH' OR 'SV')                      00226000
R04281*       IF (COM-CARRIER-CODE = 'SH' OR 'SV' OR 'MN')              00226100
R04280        IF (COM-CARRIER-CODE = 'SH' OR 'SV' OR 'MN' OR 'SP'       00226200
R04472                                    OR 'IG'                       00226300
R04209             OR 'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'              00226400
R04934             OR 'NO' OR 'NU' OR 'NX'                              00226500
R04209             OR 'LD' OR 'LI' OR 'MI'                              00226600
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'      00226700
R04209             OR 'NQ' OR 'NS'                                      00226800
R04209             OR 'UD' OR 'UT'                                      00226900
R04209             OR 'VI' OR 'WF'                                      00227000
R04832** NEW CIGNA CARRIERS                                             00227100
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'      00227200
R04832             OR 'JH'                                              00227300
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'      00227400
R04832             )                                                    00227500
R04023          OR                                                      00227600
R04023           (COM-CARRIER-CODE = '9D'                               00227700
R04023          AND                                                     00227800
R04023*           COM-PREV-CARRIER = 'SH')                              00227900
R04254*           COM-PREV-CARRIER = 'SH' OR 'SV')                      00228000
R04281*           COM-PREV-CARRIER = 'SH' OR 'SV' OR 'MN')              00228100
R04280            COM-PREV-CARRIER = 'SH' OR 'SV' OR 'MN' OR 'SP'       00228200
R04472*                                   OR 'IG'                       00228300
R04342                                    OR 'IG' OR 'MV' OR 'MB'       00228400
R04209                                    OR 'JJ'                       00228500
R04832                                    OR 'JA')                      00228600
R04023            MOVE COM-FIRST-NAME   TO COM-NAME-KEY1                00228700
R04023            MOVE COM-LAST-NAME  TO COM-NAME-KEY2.                 00228800
      *---------------------------------------------------------------* 00228900
      * EDIT CUSTOMER SEX STATUS FOR VALIDITY                         * 00229000
      *---------------------------------------------------------------* 00229100
      *  AIG DOES NOT UPDATE GENDER/SEX ON CASENAME TABLE             * 00229200
      *  SEX FIELD IS NOT SHOWN ON SCREEN FOR AIG                     * 00229300
      *---------------------------------------------------------------* 00229400
R04023*    IF (COM-CARRIER-CODE = 'SH')                                 00229500
R04254*    IF (COM-CARRIER-CODE = 'SH' OR 'SV')                         00229600
R04281*    IF (COM-CARRIER-CODE = 'SH' OR 'SV' OR 'MN')                 00229700
R04280*    IF (COM-CARRIER-CODE = 'SH' OR 'SV' OR 'MN' OR 'SP')         00229800
R04472     IF (COM-CARRIER-CODE = 'SH' OR 'SV' OR 'MN' OR 'SP' OR 'IG'  00229900
R04342             OR 'MV' OR 'MB'                                      00230000
R04209             OR 'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'              00230100
R04934             OR 'NO' OR 'NU' OR 'NX'                              00230200
R04209             OR 'LD' OR 'LI' OR 'MI'                              00230300
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'      00230400
R04209             OR 'NQ' OR 'NS'                                      00230500
R04209             OR 'UD' OR 'UT'                                      00230600
R04209             OR 'VI' OR 'WF'                                      00230700
R04832** NEW CIGNA CARRIERS                                             00230800
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'      00230900
R04832             OR 'JH'                                              00231000
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'      00231100
R04832             )                                                    00231200
R04023         OR                                                       00231300
R04023        (COM-CARRIER-CODE = '9D'                                  00231400
R04023         AND                                                      00231500
R04023*        COM-PREV-CARRIER = 'SH')                                 00231600
R04254*        COM-PREV-CARRIER = ('SH' OR 'SV'))                       00231700
R04281*        COM-PREV-CARRIER = ('SH' OR 'SV' OR 'MN'))               00231800
R04280         COM-PREV-CARRIER = ('SH' OR 'SV' OR 'MN' OR 'SP'         00231900
R04209                                  OR 'JJ'                         00232000
R04832                                  OR 'JA'                         00232100
R04472                                  OR 'IG'))                       00232200
R04023         NEXT SENTENCE                                            00232300
R04023     ELSE                                                         00232400
           IF COM-COMPANY-IND NOT = 'Y'                                 00232500
               IF COM-SEX NOT = 'M' AND COM-SEX NOT = 'F'               00232600
                   MOVE 'Y'      TO WS-EDIT-SW                          00232700
                   MOVE -1       TO MAP-SEXL                            00232800
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-SEXA                00232900
                   MOVE 'NA154'  TO WS-HOLD-MESSAGE                     00233000
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00233100
                                                                        00233200
           IF COM-COMPANY-IND = 'Y'                                     00233300
                MOVE SPACE TO COM-SEX.                                  00233400
                                                                        00233500
      *---------------------------------------------------------------* 00233600
      * RECORD ORIGIN IS AN ONLINE ADD = 'O'                          * 00233700
      *---------------------------------------------------------------* 00233800
                                                                        00233900
           MOVE 'O' TO COM-RECORD-ORIGIN.                               00234000
                                                                        00234100
      *---------------------------------------------------------------* 00234200
      * EDIT LAST-NAME FOR NUMERICS                                   * 00234300
      *---------------------------------------------------------------* 00234400
                                                                        00234500
           MOVE COM-LAST-NAME TO WS-NUMERIC-CHECK.                      00234600
           PERFORM 0160-NUMERIC-CHECK                                   00234700
               VARYING ACTION-SUB FROM 1 BY 1                           00234800
                   UNTIL ACTION-SUB > 20.                               00234900
                                                                        00235000
           IF WS-NUMERIC-SW = 'Y'                                       00235100
                   MOVE 'Y'      TO WS-EDIT-SW                          00235200
                   MOVE -1       TO MAP-LAST-NAMEL                      00235300
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-LAST-NAMEA          00235400
                   MOVE 'NA022'  TO WS-HOLD-MESSAGE                     00235500
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00235600
                                                                        00235700
      *---------------------------------------------------------------* 00235800
      * EDIT FIRST NAME FOR NUMERICS                                  * 00235900
      *---------------------------------------------------------------* 00236000
                                                                        00236100
           MOVE 'N'            TO WS-NUMERIC-SW.                        00236200
           MOVE COM-FIRST-NAME TO WS-NUMERIC-CHECK.                     00236300
           PERFORM 0160-NUMERIC-CHECK                                   00236400
               VARYING ACTION-SUB FROM 1 BY 1                           00236500
                   UNTIL ACTION-SUB > 15.                               00236600
                                                                        00236700
           IF WS-NUMERIC-SW = 'Y'                                       00236800
                   MOVE 'Y'      TO WS-EDIT-SW                          00236900
                   MOVE -1       TO MAP-FIRST-NAMEL                     00237000
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-FIRST-NAMEA         00237100
                   MOVE 'NA040'  TO WS-HOLD-MESSAGE                     00237200
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00237300
                                                                        00237400
      *---------------------------------------------------------------* 00237500
      * EDIT MIDDLE NAME FOR NUMERICS                                 * 00237600
      *---------------------------------------------------------------* 00237700
                                                                        00237800
           MOVE 'N'             TO WS-NUMERIC-SW.                       00237900
           MOVE COM-MIDDLE-NAME TO WS-NUMERIC-CHECK.                    00238000
           PERFORM 0160-NUMERIC-CHECK                                   00238100
               VARYING ACTION-SUB FROM 1 BY 1                           00238200
                   UNTIL ACTION-SUB > 10.                               00238300
                                                                        00238400
           IF WS-NUMERIC-SW = 'Y'                                       00238500
                   MOVE 'Y'      TO WS-EDIT-SW                          00238600
                   MOVE -1       TO MAP-MIDDLE-NAMEL                    00238700
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-MIDDLE-NAMEA        00238800
                   MOVE 'NA041'  TO WS-HOLD-MESSAGE                     00238900
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00239000
                                                                        00239100
           DISPLAY ' CM427 COM-ZIP:'          COM-ZIP                   00239200
           DISPLAY ' CM427 COM-COUNTRY-CODE:' COM-COUNTRY-CODE          00239300
R02539*---------------------------------------------------------------* 00239400
R02539* EDIT COUNTRY CODE FOR VALIDITY                                * 00239500
R02539*---------------------------------------------------------------* 00239600
R02539     IF COM-ZIP = '00100'                                         00239700
              DISPLAY 'ZIP VALDN'                                       00239877
R02539        EVALUATE TRUE                                             00239900
R02539           WHEN COM-COUNTRY-CODE = SPACES                         00240000
                   DISPLAY '1'                                          00240177
R02539             MOVE 'Y'      TO WS-EDIT-SW                          00240200
R02539             MOVE -1       TO MAP-COUNTRY-CDL                     00240300
R02539             MOVE ATTRB-UNPROT-BRT-PEN TO MAP-COUNTRY-CDA         00240400
R02539             MOVE 'CM427'  TO WS-HOLD-MESSAGE                     00240500
R02539             PERFORM 0170-SLIDE-ERROR-MESSAGES                    00240600
R02539           WHEN COM-COUNTRY-CODE = 'US'                           00240700
                   DISPLAY '2'                                          00240877
R02539             MOVE 'Y'      TO WS-EDIT-SW                          00240900
R02539             MOVE -1       TO MAP-COUNTRY-CDL                     00241000
R02539             MOVE ATTRB-UNPROT-BRT-PEN TO MAP-COUNTRY-CDA         00241100
R02539             MOVE 'CM428'  TO WS-HOLD-MESSAGE                     00241200
R02539             PERFORM 0170-SLIDE-ERROR-MESSAGES                    00241300
R02539           WHEN OTHER                                             00241400
                   DISPLAY '3'                                          00241577
R02539             MOVE COM-COUNTRY-CODE     TO WT-C0525-COUNTRY        00241600
R02539             MOVE WT-CNTL0525 TO TURBO-CNTL-AREA                  00241700
R02539             PERFORM 10000-CALL-GSF                               00241800
R02539             MOVE TURBO-CNTL-AREA TO WT-CNTL0525                  00241900
                   MOVE ZERO TO  WT-C0525-RETURN                        00242000
R02539             IF  WT-C0525-RETURN = 0                              00242100
R02539                 CONTINUE                                         00242200
R02539             ELSE                                                 00242300
R02539                 IF WT-C0525-RETURN =  4                          00242400
R02539                    MOVE 'Y'      TO WS-EDIT-SW                   00242500
R02539                    MOVE -1       TO MAP-COUNTRY-CDL              00242600
R02539                    MOVE ATTRB-UNPROT-BRT-PEN TO MAP-COUNTRY-CDA  00242700
R02539                    MOVE 'CM428'  TO WS-HOLD-MESSAGE              00242800
R02539                    PERFORM 0170-SLIDE-ERROR-MESSAGES             00242900
R02539                    MOVE 'CM428'  TO WS-HOLD-MESSAGE              00243000
R02539                 END-IF                                           00243100
R02539             END-IF                                               00243200
R02539        END-EVALUATE                                              00243300
R02539     ELSE                                                         00243400
R02539        IF COM-ZIP NOT EQUAL '00100'                              00243500
R02539           EVALUATE TRUE                                          00243600
R02539              WHEN COM-COUNTRY-CODE = SPACES                      00243700
R02539                 CONTINUE                                         00243800
R02539              WHEN COM-COUNTRY-CODE NOT EQUAL SPACES              00243900
R02539                 MOVE 'Y'      TO WS-EDIT-SW                      00244000
R02539                 MOVE -1       TO MAP-COUNTRY-CDL                 00244100
R02539                 MOVE ATTRB-UNPROT-BRT-PEN TO MAP-COUNTRY-CDA     00244200
R02539                 MOVE 'CM428'  TO WS-HOLD-MESSAGE                 00244300
R02539                 PERFORM 0170-SLIDE-ERROR-MESSAGES                00244400
R02539              END-EVALUATE                                        00244500
R02539        END-IF                                                    00244600
R02539     END-IF.                                                      00244700
R02539                                                                  00244800
R02539                                                                  00244900
R02539*---------------------------------------------------------------* 00245000
R02539* EDIT POSTAL CODE FOR VALIDITY                                 * 00245100
R02539*---------------------------------------------------------------* 00245200
R02539     IF COM-ZIP = '00100'                                         00245300
R02539       IF COM-COUNTRY-CODE = 'CA'                                 00245400
R02539         EVALUATE TRUE ALSO TRUE                                  00245500
R02539           WHEN COM-STATE = 'AB' ALSO COM-POSTAL-CODE (1:1) = 'T' 00245600
R02539             CONTINUE                                             00245700
R02539           WHEN COM-STATE = 'BC' ALSO COM-POSTAL-CODE (1:1) = 'V' 00245800
R02539             CONTINUE                                             00245900
R02539           WHEN COM-STATE = 'LB' ALSO COM-POSTAL-CODE (1:1) = 'A' 00246000
R02539             CONTINUE                                             00246100
R02539           WHEN COM-STATE = 'MB' ALSO COM-POSTAL-CODE (1:1) = 'R' 00246200
R02539             CONTINUE                                             00246300
R02539           WHEN COM-STATE = 'NB' ALSO COM-POSTAL-CODE (1:1) = 'E' 00246400
R02539             CONTINUE                                             00246500
R02539           WHEN COM-STATE = 'NF' ALSO COM-POSTAL-CODE (1:1) = 'A' 00246600
R02539             CONTINUE                                             00246700
R02539           WHEN COM-STATE = 'NA' ALSO COM-POSTAL-CODE (1:1) = 'X' 00246800
R02539             CONTINUE                                             00246900
R02539           WHEN COM-STATE = 'NS' ALSO COM-POSTAL-CODE (1:1) = 'B' 00247000
R02539             CONTINUE                                             00247100
R02539           WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'L' 00247200
R02539           WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'N' 00247300
R02539           WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'K' 00247400
R02539           WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'M' 00247500
R02539           WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'P' 00247600
R02539             CONTINUE                                             00247700
R02539           WHEN COM-STATE = 'PE' ALSO COM-POSTAL-CODE (1:1) = 'C' 00247800
R02539             CONTINUE                                             00247900
R02539           WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'H' 00248000
R02539           WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'J' 00248100
R02539           WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'G' 00248200
R02539             CONTINUE                                             00248300
R02539           WHEN COM-STATE = 'SK' ALSO COM-POSTAL-CODE (1:1) = 'S' 00248400
R02539             CONTINUE                                             00248500
R02539           WHEN COM-STATE = 'YT' ALSO COM-POSTAL-CODE (1:1) = 'Y' 00248600
R02539             CONTINUE                                             00248700
R02539           WHEN OTHER                                             00248800
R02539               MOVE 'Y'      TO WS-EDIT-SW                        00248900
R02539               MOVE -1       TO MAP-POSTAL-CDL                    00249000
R02539               MOVE ATTRB-UNPROT-BRT-PEN TO MAP-POSTAL-CDA        00249100
R02539               MOVE 'CM120'  TO WS-HOLD-MESSAGE                   00249200
R02539               PERFORM 0170-SLIDE-ERROR-MESSAGES                  00249300
R02539           END-EVALUATE                                           00249400
R02539       END-IF                                                     00249500
R02539     ELSE                                                         00249600
R02539       IF COM-POSTAL-CODE NOT EQUAL SPACES                        00249700
R02539          MOVE 'Y'      TO WS-EDIT-SW                             00249800
R02539          MOVE -1       TO MAP-POSTAL-CDL                         00249900
R02539          MOVE ATTRB-UNPROT-BRT-PEN TO MAP-POSTAL-CDA             00250000
R02539          MOVE 'CM120'  TO WS-HOLD-MESSAGE                        00250100
R02539          PERFORM 0170-SLIDE-ERROR-MESSAGES                       00250200
R02539       END-IF                                                     00250300
R02539     END-IF.                                                      00250400
R02539                                                                  00250500
R02539*---------------------------------------------------------------* 00250600
R02539* EDIT STATE CODE FOR VALIDITY                                  * 00250700
R02539*---------------------------------------------------------------* 00250800
R02539     IF COM-STATE = SPACES OR                                     00250900
R02539        COM-STATE = 'XX'                                          00251000
R02539        EVALUATE TRUE                                             00251100
R02539           WHEN COM-COUNTRY-CODE = SPACES                         00251200
R02539           WHEN COM-COUNTRY-CODE = 'CA'                           00251300
R02539             MOVE 'Y'      TO WS-EDIT-SW                          00251400
R02539             MOVE -1       TO MAP-STATEL                          00251500
R02539             MOVE ATTRB-UNPROT-BRT-PEN TO MAP-STATEA              00251600
R02539             MOVE 'CM016'  TO WS-HOLD-MESSAGE                     00251700
R02539             PERFORM 0170-SLIDE-ERROR-MESSAGES                    00251800
R02539        END-EVALUATE                                              00251900
R02539     ELSE                                                         00252000
R02539        MOVE COM-STATE   TO WT-C0091-STATE                        00252100
R02539        MOVE WT-CNTL0091 TO TURBO-CNTL-AREA                       00252200
R02539        PERFORM 10000-CALL-GSF                                    00252300
R02539        MOVE TURBO-CNTL-AREA TO WT-CNTL0091                       00252400
R02539     END-IF.                                                      00252500
R02539                                                                  00252600
R02539     EVALUATE WT-C0091-RETURN                                     00252700
R02539         WHEN   +0                                                00252800
R02539             CONTINUE                                             00252900
R02539         WHEN   +4                                                00253000
R02539             MOVE 'CM016'  TO WS-HOLD-MESSAGE                     00253100
R02539             PERFORM 0170-SLIDE-ERROR-MESSAGES                    00253200
R02539     END-EVALUATE.                                                00253300
RA4254                                                                  00253400
RA4254*---------------------------------------------------------------* 00253500
RA4254* EDIT ISSUE STATE FOR VALIDITY - AIG CARRIERS ONLY             * 00253600
RA4254*---------------------------------------------------------------* 00253700
RA4254                                                                  00253800
RA4254     IF COM-CARRIER-CODE = ('SV' OR 'MN' OR 'SP')                 00253900
RA4254             CONTINUE                                             00254000
RA4254     ELSE                                                         00254100
RA4254             NEXT SENTENCE                                        00254200
RA4254     END-IF                                                       00254300
RA4254                                                                  00254400
RA4254     MOVE 'G'                TO WT-C0106-REQUEST                  00254500
RA4254                                                                  00254600
RA4254     INITIALIZE                 WT-C0106-GENERIC-KEY              00254700
RA4254     MOVE COM-CARRIER-CODE   TO WT-C0106-CARRIER                  00254800
RA4254     MOVE COM-ISSUE-STATE    TO WT-C0106-STATE                    00254900
RA4254     EVALUATE COM-CARRIER-CODE                                    00255000
RA4254         WHEN 'SV'                                                00255100
RA4254             MOVE 'V'        TO WT-C0106-PRODUCT-TYPE             00255200
RA4254         WHEN 'MN'                                                00255300
RA4254             MOVE 'M'        TO WT-C0106-PRODUCT-TYPE             00255400
RA4254         WHEN 'SP'                                                00255500
RA4254             MOVE 'E'        TO WT-C0106-PRODUCT-TYPE             00255600
RA4254     END-EVALUATE                                                 00255700
RA4254     MOVE COM-CHANGE-DATE    TO WS-DATE-R                         00255800
RA4254     MOVE WS-DATE-R          TO WS-DATE                           00255900
RA4254     MOVE WS-YY              TO WT-C0106-EFF-DATE-YY              00256000
RA4254     MOVE WS-MM              TO WT-C0106-EFF-DATE-MM              00256100
RA4254     MOVE WS-DD              TO WT-C0106-EFF-DATE-DD              00256200
RA4254     IF WS-YY > 50                                                00256300
RA4254         MOVE 19             TO WT-C0106-EFF-DATE-CC              00256400
RA4254     ELSE                                                         00256500
RA4254         MOVE 20             TO WT-C0106-EFF-DATE-CC              00256600
RA4254     END-IF                                                       00256700
RA4254                                                                  00256800
RA4254*    CALL WS-GETT0106 USING WT-CNTL0106                           00256900
RA4254*    IF WT-C0106-RETURN = +4                                      00257000
RA4254*       MOVE 'Y'     TO WS-EDIT-SW                                00257100
RA4254*       MOVE -1      TO MAP-ISSUE-STATEL                          00257200
RA4254*       MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ISSUE-STATEA             00257300
RA4254*       MOVE 'AM207' TO WS-HOLD-MESSAGE                           00257400
RA4254*       PERFORM 0170-SLIDE-ERROR-MESSAGES                         00257500
RA4254*    END-IF.                                                      00257600
                                                                        00257700
      *---------------------------------------------------------------* 00257800
      * EDIT ASSOCIATION CODE1 FOR VALIDITY                           * 00257900
      *---------------------------------------------------------------* 00258000
                                                                        00258100
           IF COM-ASSOCIATION1 NOT = SPACES                             00258200
               MOVE COM-ASSOCIATION1   TO WT-C0043-ASSOC-CODE-KEY       00258300
               MOVE WT-CNTL0043        TO TURBO-CNTL-AREA               00258400
               PERFORM 10000-CALL-GSF                                   00258500
               MOVE TURBO-CNTL-AREA    TO WT-CNTL0043                   00258600
               MOVE ZERO TO WT-C0043-RETURN                             00258700
               IF  WT-C0043-RETURN NOT = ZERO                           00258800
                   MOVE 'Y'      TO WS-EDIT-SW                          00258900
                   MOVE -1       TO MAP-ASSOC1L                         00259000
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ASSOC1A             00259100
                   MOVE 'NA155'  TO WS-HOLD-MESSAGE                     00259200
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00259300
                                                                        00259400
           IF COM-ASSOCIATION1 NOT = SPACES                             00259500
               MOVE COM-CHANGE-DATE TO WS-DATE-R                        00259600
               MOVE WS-DATE-R       TO WS-DATE                          00259700
               MOVE WS-YY           TO WS-COMPARE-YY                    00259800
               MOVE WS-MM           TO WS-COMPARE-MM                    00259900
               MOVE WS-DD           TO WS-COMPARE-DD.                   00260000
                                                                        00260100
           IF COM-ASSOCIATION1 NOT = SPACES                             00260200
Y2KIMR*                                                                 00260300
Y2KIMR* IMR CHANGE IMR1 BEGIN                                           00260400
Y2KIMR*                                                                 00260500
Y2KIMR*        IF  WT-C0043-RETURN  = ZERO AND                          00260600
Y2KIMR*             WS-COMPARE-DATE > WT-C0043-ASSOC-TERM-DATE          00260700
Y2KIMR*                                                                 00260800
Y2KIMR             COPY IMRG9DT1                                        00260900
Y2KIMR             REPLACING DATE-9 BY WS-COMPARE-DATE.                 00261000
Y2KIMR*                                                                 00261100
Y2KIMR             COPY IMRG9DT2                                        00261200
Y2KIMR             REPLACING DATE-9 BY WT-C0043-ASSOC-TERM-DATE.        00261300
Y2KIMR         MOVE ZERO TO WT-C0043-RETURN                             00261400
Y2KIMR         IF WT-C0043-RETURN = ZERO AND                            00261500
Y2KIMR             Y2K-WK-DATE1-9 > Y2K-WK-DATE2-9                      00261600
Y2KIMR*                                                                 00261700
Y2KIMR* IMR CHANGE IMR1 END                                             00261800
Y2KIMR*                                                                 00261900
                    MOVE 'Y'      TO WS-EDIT-SW                         00262000
                    MOVE -1       TO MAP-ASSOC1L                        00262100
                    MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ASSOC1A            00262200
                    MOVE 'NA158'  TO WS-HOLD-MESSAGE                    00262300
                    PERFORM 0170-SLIDE-ERROR-MESSAGES.                  00262400
                                                                        00262500
      *---------------------------------------------------------------* 00262600
      * EDIT ASSOCIATION CODE2 FOR VALIDITY                           * 00262700
      *---------------------------------------------------------------* 00262800
           IF COM-ASSOCIATION2 NOT = SPACES                             00262900
               MOVE COM-ASSOCIATION2   TO WT-C0043-ASSOC-CODE-KEY       00263000
               MOVE WT-CNTL0043        TO TURBO-CNTL-AREA               00263100
               PERFORM 10000-CALL-GSF                                   00263200
               MOVE TURBO-CNTL-AREA    TO WT-CNTL0043                   00263300
               MOVE ZERO TO WT-C0043-RETURN                             00263400
               IF  WT-C0043-RETURN NOT = ZERO                           00263500
                   MOVE 'Y'      TO WS-EDIT-SW                          00263600
                   MOVE -1       TO MAP-ASSOC2L                         00263700
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ASSOC2A             00263800
                   MOVE 'NA156'  TO WS-HOLD-MESSAGE                     00263900
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00264000
                                                                        00264100
           IF COM-ASSOCIATION2 NOT = SPACES                             00264200
               MOVE COM-CHANGE-DATE TO WS-DATE-R                        00264300
               MOVE WS-DATE-R       TO WS-DATE                          00264400
               MOVE WS-YY           TO WS-COMPARE-YY                    00264500
               MOVE WS-MM           TO WS-COMPARE-MM                    00264600
               MOVE WS-DD           TO WS-COMPARE-DD.                   00264700
                                                                        00264800
           IF COM-ASSOCIATION2 NOT = SPACES                             00264900
Y2KIMR*                                                                 00265000
Y2KIMR* IMR CHANGE IMR2 BEGIN                                           00265100
Y2KIMR*                                                                 00265200
Y2KIMR*        IF  WT-C0043-RETURN  = ZERO AND                          00265300
Y2KIMR*            WS-COMPARE-DATE > WT-C0043-ASSOC-TERM-DATE           00265400
Y2KIMR*                                                                 00265500
Y2KIMR             COPY IMRG9DT1                                        00265600
Y2KIMR             REPLACING DATE-9 BY WS-COMPARE-DATE.                 00265700
Y2KIMR*                                                                 00265800
Y2KIMR             COPY IMRG9DT2                                        00265900
Y2KIMR             REPLACING DATE-9 BY WT-C0043-ASSOC-TERM-DATE.        00266000
Y2KIMR*                                                                 00266100
Y2KIMR         IF  WT-C0043-RETURN = ZERO AND                           00266200
Y2KIMR             Y2K-WK-DATE1-9 > Y2K-WK-DATE2-9                      00266300
Y2KIMR*                                                                 00266400
Y2KIMR* IMR CHANGE IMR2 END                                             00266500
Y2KIMR*                                                                 00266600
                   MOVE 'Y'      TO WS-EDIT-SW                          00266700
                   MOVE -1       TO MAP-ASSOC2L                         00266800
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ASSOC2A             00266900
                   MOVE 'NA158'  TO WS-HOLD-MESSAGE                     00267000
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00267100
                                                                        00267200
      *---------------------------------------------------------------* 00267300
      * EDIT ASSOCIATION CODE3 FOR VALIDITY                           * 00267400
      *---------------------------------------------------------------* 00267500
           IF COM-ASSOCIATION3 NOT = SPACES                             00267600
               MOVE COM-ASSOCIATION3   TO WT-C0043-ASSOC-CODE-KEY       00267700
               MOVE WT-CNTL0043        TO TURBO-CNTL-AREA               00267800
               PERFORM 10000-CALL-GSF                                   00267900
               MOVE TURBO-CNTL-AREA    TO WT-CNTL0043                   00268000
               MOVE ZERO TO WT-C0043-RETURN                             00268100
               IF  WT-C0043-RETURN NOT = ZERO                           00268200
                   MOVE 'Y'      TO WS-EDIT-SW                          00268300
                   MOVE -1       TO MAP-ASSOC3L                         00268400
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ASSOC3A             00268500
                   MOVE 'NA157'  TO WS-HOLD-MESSAGE                     00268600
                   PERFORM 0170-SLIDE-ERROR-MESSAGES.                   00268700
                                                                        00268800
           IF COM-ASSOCIATION3 NOT = SPACES                             00268900
               MOVE COM-CHANGE-DATE TO WS-DATE-R                        00269000
               MOVE WS-DATE-R       TO WS-DATE                          00269100
               MOVE WS-YY           TO WS-COMPARE-YY                    00269200
               MOVE WS-MM           TO WS-COMPARE-MM                    00269300
               MOVE WS-DD           TO WS-COMPARE-DD.                   00269400
                                                                        00269500
           IF COM-ASSOCIATION3 NOT = SPACES                             00269600
Y2KIMR*                                                                 00269700
Y2KIMR* IMR CHANGE IMR3 BEGIN                                           00269800
Y2KIMR*                                                                 00269900
Y2KIMR*        IF  WT-C0043-RETURN  = ZERO AND                          00270000
Y2KIMR*            WS-COMPARE-DATE > WT-C0043-ASSOC-TERM-DATE           00270100
Y2KIMR*                                                                 00270200
Y2KIMR             COPY IMRG9DT1                                        00270300
Y2KIMR             REPLACING DATE-9 BY WS-COMPARE-DATE.                 00270400
Y2KIMR*                                                                 00270500
Y2KIMR             COPY IMRG9DT2                                        00270600
Y2KIMR             REPLACING DATE-9 BY WT-C0043-ASSOC-TERM-DATE.        00270700
Y2KIMR         MOVE ZERO TO WT-C0043-RETURN                             00270800
Y2KIMR         IF  WT-C0043-RETURN = ZERO AND                           00270900
Y2KIMR             Y2K-WK-DATE1-9 > Y2K-WK-DATE2-9                      00271000
Y2KIMR*                                                                 00271100
Y2KIMR* IMR CHANGE IMR3 END                                             00271200
Y2KIMR*                                                                 00271300
                   MOVE 'Y'      TO WS-EDIT-SW                          00271400
                   MOVE -1       TO MAP-ASSOC3L                         00271500
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ASSOC3A             00271600
                   MOVE 'NA158'  TO WS-HOLD-MESSAGE                     00271700
                   PERFORM 0170-SLIDE-ERROR-MESSAGES                    00271800
               END-IF.                                                  00271900
                                                                        00272000
R03749*** HUMANA CARRIER LOGIC -                                        00272100
R03749*** CHECKING FLOA NETWORK - MAKE SURE ADDRESS CHANGE WILL STILL   00272200
R03749*** BE IN FLOA NETWORK, OTHERWISE GIVE ERROR.                     00272300
R8285A*** SAME VALIDATION IS NEEDED FOR ILLINOIS CHOICE POS NETWORK     00272400
R03749*    IF COMM-CUST-TYPE = 'CA'                                     00272500
R03749*                     OR 'AM'                                     00272600
R03749*       EXEC SQL SELECT CARRIER_CODE                              00272700
R03749*                  INTO :CASEX-CARRIER-CODE  :CASEX-CARRIER-IND   00272800
R03749*                  FROM CASEXV01                                  00272900
11735A*                  FROM CASE_MASTER                               00273000
R03749*                 WHERE CASENAME#IDNTITY = :COMM-IDNTITY          00273100
R03749*       END-EXEC                                                  00273200
                                                                        00273300
R03749*       IF CASEX-CARRIER-CODE = 'HU'                              00273400
R03749** GET DATA FROM VSAM, IF THIS IS A CASE                          00273500
R03749*          IF COMM-CUST-TYPE = 'CA'                               00273600
R03749*             PERFORM 9960-READ-CASE-DATA THRU 9960-EXIT          00273700
R08986*             PERFORM 9961-READ-CASE-NAME THRU 9961-EXIT          00273800
R08986*             MOVE 'HU'                   TO WS-TEST-CARRIER      00273900
R08986*             MOVE 'ATTR5'                TO WS-TEST-RULE         00274000
R08986*             MOVE 'PRODTYPE'             TO WS-TEST-ENTITY       00274100
R08986*             MOVE '  '                   TO WS-TEST-QUALIFIER    00274200
R08986*             MOVE WS-HOLD-HLTH-OPTION    TO WS-TEST-VALUE        00274300
R08986*             PERFORM R07063-TEST-RULE-VALUE-TBL THRU             00274400
R08986*                     R07063-TEST-RULE-VALUE-EXIT                 00274500
R08986*             IF RULE-SWITCH-PASSED                               00274600
R08986*                IF CASE-HLTH-MATERNITY = 'N'                     00274700
R08986*                   CONTINUE                                      00274800
R08986*                ELSE                                             00274900
R08986*                   IF ORIGINAL-STATE NOT = COM-STATE             00275000
R08986*                      MOVE 'MG844'       TO WS-HOLD-MESSAGE      00275100
R08986*                      PERFORM 0170-SLIDE-ERROR-MESSAGES          00275200
R08986*                   END-IF                                        00275300
R08986*                END-IF                                           00275400
R08986*             END-IF                                              00275500
R03749*          END-IF                                                 00275600
                                                                        00275700
R03749** GET DATA FROM DB2, IF THIS IS A CAM                            00275800
R03749*          IF COMM-CUST-TYPE = 'AM'                               00275900
R03749*             MOVE 'HEALTH  ' TO COV-TYPE                         00276000
R03749*             PERFORM 9970-CAM-COVERAGE THRU 9970-EXIT            00276100
R03749*             MOVE COV-PLAN-CODE TO WS-HLTH-POLICY                00276200
R03749*             MOVE 'HEALTHOP' TO COV-TYPE                         00276300
R03749*             PERFORM 9970-CAM-COVERAGE THRU 9970-EXIT            00276400
R03749*             MOVE COV-PLAN-CODE TO WS-HOLD-HLTH-OPTION           00276500
R03749*             PERFORM 9980-READ-CASEMASTER  THRU 9980-EXIT        00276600
R03749*          END-IF                                                 00276700
                                                                        00276800
RA3749** IF MESSAGE NA401 HAS NEVER BEEN DISPLAYED YET AND              00276900
RA3749** THERE IS NO OTHER MESSAGE TO DISPLAY.                          00277000
R8285A** INCLUDE PROD NUM 0009 IN STATEMENT BELOW THROUGH TABLE DRIVEN. 00277100
                                                                        00277200
R9470A*** JUST CREATED A PARAGRAPH FOR PRODUCT NUMBER VALIDATION        00277300
R9470A*          PERFORM 0135-CHECK-PROD-NUMBER THRU 0135-EXIT          00277400
R9470A*       END-IF                                                    00277500
R9470A*    END-IF.                                                      00277600
MANJU2     DISPLAY ' 130 PARA COM-ENTITY-TYPE ' COM-ENTITY-TYPE.        00277700
           IF WS-EDIT-SW NOT = 'Y'                                      00277800
920124        IF (COM-ENTITY-TYPE = 'AM' OR 'CA' OR 'LB' OR 'BA'        00277900
MANJU                            OR 'LD' OR 'CAM')                      00278000
                  PERFORM 0220-UPDATE-CASENAME-ROW                      00278100
              ELSE                                                      00278200
                  PERFORM 0210-UPDATE-AGNTNAME-ROW                      00278300
              END-IF                                                    00278400
           END-IF.                                                      00278500
                                                                        00278600
R9470A 0135-CHECK-PROD-NUMBER.                                          00278700
                                                                        00278800
R8285A      INITIALIZE DCLCOVERAGE-EDITS.                               00278900
R8285A      MOVE  CASEX-CARRIER-CODE  TO CE-CARRIER.                    00279000
R8285A      MOVE  SPACES              TO CE-PROD-TYPE                   00279100
R8285A                                   CE-COV-QUALIFIER.              00279200
R8285A      MOVE 'PRODUCTNUM'         TO CE-COV-ENTITY.                 00279300
R8285A      MOVE CASE-PRODUCT-NUMBER OF CASE-RECORD TO CE-COV-VALUE.    00279400
R8285A      MOVE 'C'                  TO CE-CURRENT-IND.                00279500
R8285A      PERFORM R08041-CHECK-COVERAGE-ROW                           00279600
R8285A         THRU R08041-CHECK-COVERAGE-ROW-EXIT.                     00279700
                                                                        00279800
R8285A      IF COVREDT-ROW-FOUND AND                                    00279900
RA3749        (WS-MESSAGE-NUMBER1 = SPACES  AND                         00280000
RA3749         WS-MESSAGE-NUMBER2 = SPACES  AND                         00280100
RA3749         WS-MESSAGE-NUMBER3 = SPACES  AND                         00280200
RA3749         WS-MESSAGE-NUMBER4 = SPACES) AND                         00280300
RA3749        (COMM-MSG-ID(1) NOT = 'NA401' AND                         00280400
RA3749         COMM-MSG-ID(2) NOT = 'NA401' AND                         00280500
RA3749         COMM-MSG-ID(3) NOT = 'NA401' AND                         00280600
RA3749         COMM-MSG-ID(4) NOT = 'NA401')                            00280700
                                                                        00280800
R03749** COM-ZIP IS THE REVISED ZIP, ZIP IS WHAT YOU STARTED WITH       00280900
R03749         IF COM-ZIP NOT = ZIP                                     00281000
                                                                        00281100
R03749*** SAVE CURRENT CASE-PRODUCT-NUMBER, CASE-PRODUCT-LINE           00281200
R03749            MOVE CASE-PRODUCT-NUMBER OF CASE-RECORD               00281300
R03749              TO CASE-GE01600-PRODUCT-NUMBER                      00281400
R03749            MOVE CASE-PRODUCT-LINE OF CASE-RECORD                 00281500
R03749              TO CASE-GE01600-PRODUCT-LINE                        00281600
                                                                        00281700
R03749*           PERFORM 9955-EDIT-PROD-LINE-NUM THRU 9955-EXIT        00281800
                                                                        00281900
R03749*** GIVE ERROR IF NETWORK IS CHANGING - DUE TO DIFFERENT ZIP CODE 00282000
R03749*** OR IF ERROR-CODE RETURNED FROM GE01600 BECAUSE                00282100
R03749*** THE CURRENT FLOA NETWORK DID NOT VALIDATE WITH THE NEW ZIP    00282200
MANJU             MOVE '5' TO CASE-HLTH-OPTION                          00282300
MANJU             MOVE 'IL' TO COM-ISSUE-STATE                          00282400
R8285B            IF (CASE-HLTH-OPTION = '5' AND                        00282500
R8285B               (CASE-PRODUCT-NUMBER OF CASE-RECORD = '0009' OR    00282600
R8285B                COM-ISSUE-STATE = 'IL'))                          00282700
R8285B                CONTINUE                                          00282800
R8285B            ELSE                                                  00282900
R03749               IF (ERROR-CODE > SPACES) OR                        00283000
R03749                  (CASE-GE01600-PRODUCT-NUMBER  NOT =             00283100
R03749                    CASE-PRODUCT-NUMBER OF CASE-RECORD)           00283200
R03749               OR (CASE-GE01600-PRODUCT-LINE NOT =                00283300
R03749                    CASE-PRODUCT-LINE OF CASE-RECORD)             00283400
R03749                   MOVE 'Y'                   TO WS-EDIT-SW       00283500
R03749                   MOVE -1                    TO MAP-ADDRESS1L    00283600
R03749                   MOVE ATTRB-UNPROT-BRT-PEN  TO MAP-ZIPA         00283700
R03749                   MOVE ATTRB-UNPROT-BRT-PEN  TO MAP-ZIP-PLUS4A   00283800
R08986                   MOVE CASEX-CARRIER-CODE    TO WS-TEST-CARRIER  00283900
R08986                   MOVE 'INVALID NETWORK'     TO WS-TEST-RULE     00284000
R08986                   MOVE CASE-HLTH-OPTION      TO WS-TEST-ENTITY   00284100
R08986                   MOVE CASE-HLTH-POLICY      TO WS-TEST-QUALIFIER00284200
R08986                   MOVE CASE-PRODUCT-NUMBER   OF CASE-RECORD      00284300
                                                    TO WS-TEST-VALUE    00284400
R08986                   PERFORM R07063-TEST-RULE-VALUE-TBL             00284500
R08986                      THRU R07063-TEST-RULE-VALUE-EXIT            00284600
                                                                        00284700
R08986                   IF RULE-SWITCH-PASSED                          00284800
R08986                      MOVE 'MG841'            TO WS-HOLD-MESSAGE  00284900
R08986                   ELSE                                           00285000
R9470A                      PERFORM 0137-CHK-PROD-NUM-CHG               00285100
R9470A                         THRU 0137-EXIT                           00285200
R08986                   END-IF                                         00285300
R9470A                   IF WS-EDIT-SW = 'Y'                            00285400
R03749                      PERFORM 0170-SLIDE-ERROR-MESSAGES           00285500
R9470A                   END-IF                                         00285600
R9470A               END-IF                                             00285700
R9470A            END-IF                                                00285800
R9470A         END-IF                                                   00285900
R9470A      END-IF.                                                     00286000
                                                                        00286100
       0135-EXIT. EXIT.                                                 00286200
                                                                        00286300
R9470A 0137-CHK-PROD-NUM-CHG.                                           00286400
                                                                        00286500
R9470A*** DETERMINE IF FL PHP-R HMO (NETWORK 0013).                     00286600
R9470A      MOVE CASEX-CARRIER-CODE TO CE-CARRIER.                      00286700
R9470A      MOVE CASE-HLTH-OPTION   TO CE-PROD-TYPE.                    00286800
R9470A      MOVE 'LOB'              TO CE-COV-QUALIFIER.                00286900
R9470A      MOVE 'HEALTH'           TO CE-COV-ENTITY.                   00287000
R9470A      MOVE 'IHM'              TO CE-COV-VALUE.                    00287100
                                                                        00287200
R9470A      IF COM-ISSUE-STATE > '  '                                   00287300
R9470A         MOVE COM-ISSUE-STATE TO CE-STATE                         00287400
R9470A      ELSE                                                        00287500
R9470A         MOVE ORIGINAL-STATE  TO CE-STATE                         00287600
R9470A      END-IF.                                                     00287700
                                                                        00287800
R9470A      MOVE 'C'                TO CE-CURRENT-IND.                  00287900
                                                                        00288000
R9470A      PERFORM R08041-CHECK-COVERAGE-ROW                           00288100
R9470A         THRU R08041-CHECK-COVERAGE-ROW-EXIT.                     00288200
                                                                        00288300
R9470A*** DISPLAY A WARNING MESSAGE WHEN THE POLICY IS A FLORIDA PHP-R  00288400
R9470A*** AND THE NETWORK IS 0013 AND THE NETWORK IS CHANGING DUE TO    00288500
R9470A*** A DIFFERENT ZIP CODE.                                         00288600
R9470A      IF COVREDT-ROW-FOUND AND                                    00288700
R9470A         CASE-PRODUCT-NUMBER OF CASE-RECORD = CE-XREF-VALUE       00288800
R9470A         IF COMM-MSG-ID(1) NOT = 'NA402' AND                      00288900
R9470A            COMM-MSG-ID(2) NOT = 'NA402' AND                      00289000
R9470A            COMM-MSG-ID(3) NOT = 'NA402' AND                      00289100
R9470A            COMM-MSG-ID(4) NOT = 'NA402'                          00289200
R9470A            MOVE 'NA402'          TO WS-HOLD-MESSAGE              00289300
R9470A         ELSE                                                     00289400
R9470A            MOVE 'N'              TO WS-EDIT-SW                   00289500
R9470A***         MOVE -1               TO MAP-ADDRESS1L                00289600
R9470A            MOVE ATTRB-UNPROT     TO MAP-ZIPA                     00289700
R9470A            MOVE ATTRB-UNPROT     TO MAP-ZIP-PLUS4A               00289800
R9470A         END-IF                                                   00289900
R9470A      ELSE                                                        00290000
R03749         MOVE 'NA401'             TO WS-HOLD-MESSAGE              00290100
R9470A      END-IF.                                                     00290200
                                                                        00290300
R9470A 0137-EXIT. EXIT.                                                 00290400
                                                                        00290500
       0140-SQL-ERROR-CHECK.                                            00290600
                                                                        00290700
           IF SQLCODE NOT = ZERO                                        00290800
               GO TO 0420-DB2-ERROR.                                    00290900
                                                                        00291000
       0150-FINALST-ADDRESS-CHECK.                                      00291100
                                                                        00291200
           MOVE COM-ADDRESS1 TO NA-COMM-ADDRESS-1.                      00291300
           MOVE COM-ADDRESS2 TO NA-COMM-ADDRESS-2.                      00291400
           MOVE COM-CITY     TO NA-COMM-CITY.                           00291500
           MOVE COM-STATE    TO NA-COMM-STATE.                          00291600
           MOVE COM-ZIP      TO NA-COMM-ZIP-CODE.                       00291700
890989     MOVE COM-ZIP-PLUS4 TO NA-COMM-ZIP-PLUS4.                     00291800
                                                                        00291900
COBOLU     EXEC CICS ASSIGN APPLID(WS-APPLID) END-EXEC.                 00292000
                                                                        00292100
R01873*    IF WS-APPLID = 'CICSHADT' OR 'CICSHADP' OR 'CICSHADU'        00292200
167949*                   OR 'CICSHADL'                                 00292300
R11386*                   OR 'CICSHADF' OR 'CICSADHD' OR 'CICSHUAE'     00292400
R11386*                   OR 'CICSADHQ' OR 'CICSADHS' OR 'CICSADHU'     00292500
R11386*                   OR 'CICSADHT' OR 'CICSADHB' OR 'CICSHADY'     00292600
COBOLU*        EXEC CICS LINK                                           00292700
167949*          PROGRAM  ('CADDR')                                     00292800
COBOLU*          COMMAREA (NAME-AND-ADDRESS-COMMAREA)                   00292900
COBOLU*          LENGTH   (LENGTH OF NAME-AND-ADDRESS-COMMAREA)         00293000
COBOLU*          RESP     (WS-CICS-RESP)                                00293100
COBOLU*        END-EXEC                                                 00293200
COBOLU*    ELSE                                                         00293300
      *        EXEC CICS LINK                                           00293400
      *          PROGRAM  ('NA205')                                     00293500
      *          COMMAREA (NAME-AND-ADDRESS-COMMAREA)                   00293600
990721**         LENGTH   (WS-FIN-LENGTH)                               00293700
990721*          LENGTH   (LENGTH OF NAME-AND-ADDRESS-COMMAREA)         00293800
      *          RESP     (WS-CICS-RESP)                                00293900
      *        END-EXEC.                                                00294000
                                                                        00294100
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00294200
      *        GO TO 9999-CICS-ERROR.                                   00294300
      *                                                                 00294400
      *    MOVE NA-COMM-FINALIST-REASON TO COM-FINALST-REAS-CODE.       00294500
      *    MOVE NA-COMM-FINALIST-REASON TO WS-FINALST-REAS-CODE.        00294600
                                                                        00294700
R01142*    IF WS-FINALST-BYTE1 NOT = '9'                                00294800
R01142*       IF NA-COMM-ADDRESS-1 = COM-ADDRESS1 AND                   00294900
R01142*          NA-COMM-ADDRESS-2 = COM-ADDRESS2                       00295000
R01142*            NEXT SENTENCE                                        00295100
R01142*       ELSE                                                      00295200
R01142*         MOVE NA-COMM-ADDRESS-1 TO WS-ADDRESS1                   00295300
R01142*         MOVE NA-COMM-ADDRESS-2 TO WS-ADDRESS2                   00295400
R01142*         IF WS-ADDRESS1-REMAINDER > SPACES OR                    00295500
R01142*            WS-ADDRESS2-REMAINDER > SPACES                       00295600
R01142*** THE SOFTWARE HAS DEFAULTED/REFORMATTED AN ADDRESS LINE AND    00295700
R01142*** IT EXCEEDS 22 BYTES SO WARN THE USER                          00295800
R01142*              MOVE 'Y'      TO WS-EDIT-SW                        00295900
R01142*                               COM-FINALIST-SW                   00296000
R01142*              MOVE -1       TO MAP-ADDRESS1L                     00296100
R01142*              MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A         00296200
R01142*              MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS2A         00296300
R01142*              MOVE 'NA047'  TO WS-HOLD-MESSAGE                   00296400
R01142*              PERFORM 0170-SLIDE-ERROR-MESSAGES                  00296500
R01142*              MOVE 'NA048'  TO WS-HOLD-MESSAGE                   00296600
R01142*              PERFORM 0170-SLIDE-ERROR-MESSAGES.                 00296700
                                                                        00296800
R11386********************************************************          00296900
R11386**ADDING FUNCTIONALITY TO GET THE COUNTY CODE FROM *              00297000
R11386**STATE_COUNTY IF CADDR FAILS TO RETURN THE COUNTY CODE*          00297100
R11386********************************************************          00297200
R11386     IF WS-FINALST-BYTE1 = '9'                                    00297300
R11386        MOVE NA-COMM-ZIP        TO WS-SCZ-ZIP                     00297400
R11386        PERFORM 1300-STATE-COUNTY-ZIP THRU 1300-EXIT              00297500
R11386     END-IF                                                       00297600
R01213     IF WS-FINALST-BYTE1 NOT = '9'                                00297700
R01213        IF (NA-COMM-ADDRESS-1 = COM-ADDRESS1 AND                  00297800
R01213           NA-COMM-ADDRESS-2 = COM-ADDRESS2)                      00297900
R01213         MOVE NA-COMM-ADDRESS-1 TO COM-ADDRESS1                   00298000
R01213         MOVE NA-COMM-ADDRESS-2 TO COM-ADDRESS2                   00298100
R01213         MOVE NA-COMM-CITY      TO COM-CITY                       00298200
R01213         MOVE NA-COMM-STATE     TO COM-STATE                      00298300
R01213         MOVE NA-COMM-ZIP-CODE  TO COM-ZIP                        00298400
R01213         MOVE NA-COMM-ZIP-PLUS4 TO COM-ZIP-PLUS4                  00298500
R01213         MOVE NA-COMM-COUNTY-CODE TO COM-COUNTY-CODE              00298600
R01213        ELSE                                                      00298700
R01213          MOVE NA-COMM-ADDRESS-1 TO WS-ADDRESS1                   00298800
R01213          MOVE NA-COMM-ADDRESS-2 TO WS-ADDRESS2                   00298900
R01213          IF WS-ADDRESS1-REMAINDER > SPACES OR                    00299000
R01213             WS-ADDRESS2-REMAINDER > SPACES                       00299100
R01213             MOVE COM-ADDRESS1  TO NA-COMM-ADDRESS-1              00299200
R01213             MOVE COM-ADDRESS2  TO NA-COMM-ADDRESS-2              00299300
R01213*** RETURNED ADDRESS FROM THE DATABASE THAT WAS TRUNCATED         00299400
R01213*** THE SOFTWARE HAS DEFAULTED/REFORMATTED AN ADDRESS LINE AND    00299500
R01213*** IT EXCEEDS 22 BYTES SO WARN THE USER                          00299600
R01213               MOVE 'Y'      TO WS-EDIT-SW                        00299700
R01213                                COM-FINALIST-SW                   00299800
R01213               MOVE -1       TO MAP-ADDRESS1L                     00299900
R01213               MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A         00300000
R01213               MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS2A         00300100
R01213               MOVE 'NA047'  TO WS-HOLD-MESSAGE                   00300200
R01213               PERFORM 0170-SLIDE-ERROR-MESSAGES                  00300300
R01213               MOVE 'NA048'  TO WS-HOLD-MESSAGE                   00300400
R01213               PERFORM 0170-SLIDE-ERROR-MESSAGES                  00300500
R01213         ELSE                                                     00300600
R01213**** NO TRUNCATION                                                00300700
R01213               MOVE NA-COMM-ADDRESS-1 TO COM-ADDRESS1             00300800
R01213               MOVE NA-COMM-ADDRESS-2 TO COM-ADDRESS2.            00300900
                                                                        00301000
           IF WS-FINALST-BYTE1 NOT = '9'                                00301100
R01213*        MOVE NA-COMM-ADDRESS-1 TO COM-ADDRESS1                   00301200
R01213*        MOVE NA-COMM-ADDRESS-2 TO COM-ADDRESS2                   00301300
               MOVE NA-COMM-CITY      TO COM-CITY                       00301400
               MOVE NA-COMM-STATE     TO COM-STATE                      00301500
               MOVE NA-COMM-ZIP-CODE  TO COM-ZIP                        00301600
               MOVE NA-COMM-ZIP-PLUS4 TO COM-ZIP-PLUS4                  00301700
KPL            MOVE NA-COMM-COUNTY-CODE TO COM-COUNTY-CODE              00301800
920124         IF (COMM-CUST-TYPE = 'CA' OR 'AM' OR 'LB' OR 'BA'        00301900
960530                            OR 'LD')                              00302000
890989         EXEC SQL                                                 00302100
890989             OPEN CASECUR                                         00302200
890989         END-EXEC                                                 00302300
890989         IF SQLCODE NOT = ZERO                                    00302400
890989             GO TO 0420-DB2-ERROR                                 00302500
890989         ELSE                                                     00302600
890989             EXEC SQL FETCH CASECUR                               00302700
890989*                INTO DCLAGNTNAME:AGNTNAME-INDICATORS             00302800
R00700                INTO  :IDNTITY, :LAST-NAME :IND-LAST-NAME,        00302900
C23311                      :FIRST-NAME :IND-FIRST-NAME,                00303000
C23311                      :MIDDLE-NAME :IND-MIDDLE-NAME,              00303100
R00700                      :PREFIX :IND-PREFIX, :SUFFIX1 :IND-SUFFIX1, 00303200
R00700                      :SUFFIX2 :IND-SUFFIX2,                      00303300
R00700                      :COMPANY-IND, :COMPANY-IN-ADDRESS,          00303400
C23311                      :COMPANY-NAME :IND-COMPANY-NAME,            00303500
C23311                      :DISPLAY-NAME :IND-DISPLAY-NAME,            00303600
R00700                      :NICKNAME :IND-NICKNAME,                    00303700
R00700                      :ADDRESS1, :ADDRESS2 :IND-ADDRESS2,         00303800
R00700                      :CITY, :STATE, :ZIP, :ZIP-PLUS4,            00303900
R00700                      :COUNTY-CODE, :AREA-CODE,                   00304000
R00700                      :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,   00304100
R00700                      :BIRTH-DATE :IND-BIRTH-DATE,                00304200
R00700                      :FINALST-REAS-CODE, :FINALST-OVRD-IND,      00304300
R00700                      :DUP-ADDR-OVRD-IND,                         00304400
R00700                      :EFFECTIVE-DATE, :CHANGE-DATE,              00304500
R00700                      :CHANGE-LOGON, :ENTITY-TYPE,                00304600
R00700                      :RECORD-STATUS, :ALT-ADDRESS-IND,           00304700
R00700                      :FUTURE-ADDRESS-IND,                        00304800
R00700                      :RECORD-ORIGIN, :COMBINED-STATUS,           00304900
R00700                      :SITE-CODE, :NAME-KEY1,                     00305000
R00700                      :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,      00305100
R00700                      :ASSOCIATION1 :IND-ASSOCIATION1,            00305200
R00700                      :ASSOCIATION2 :IND-ASSOCIATION2,            00305300
R00700                      :ASSOCIATION3 :IND-ASSOCIATION3,            00305400
R00700                      :FAX-AREA-CODE, :FAX-PHONE,                 00305500
R04023                      :ORIGINAL-STATE,                            00305600
R00700                      :EMAIL1 :IND-EMAIL1,                        00305700
R02539                      :PASS-WORD :IND-PASSWORD,                   00305800
R02539                      :COUNTRY-CODE :IND-COUNTRY-CODE,            00305900
R02539                      :POSTAL-CODE :IND-POSTAL-CODE               00306000
890989             END-EXEC                                             00306100
890989             IF SQLCODE NOT = ZERO                                00306200
890989                 GO TO 0420-DB2-ERROR                             00306300
890989             ELSE                                                 00306400
890989                 EXEC SQL                                         00306500
890989                     CLOSE CASECUR                                00306600
890989                 END-EXEC                                         00306700
890989                 IF SQLCODE NOT = ZERO                            00306800
890989                    GO TO 0420-DB2-ERROR                          00306900
890989                 ELSE                                             00307000
890989         EVALUATE TRUE ALSO TRUE                                  00307100
890989             WHEN ZIP NOT EQUAL COM-ZIP                           00307200
890989             ALSO STATE NOT EQUAL COM-STATE                       00307300
890989                 MOVE 'Y' TO ZIP-CHANGED-SW                       00307400
890989                 MOVE 'Y' TO STATE-CHANGED-SW                     00307500
890989                 MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW                 00307600
890989             WHEN ZIP NOT EQUAL COM-ZIP                           00307700
890989             ALSO ZIP-PLUS4 NOT EQUAL COM-ZIP-PLUS4               00307800
890989                 MOVE 'Y' TO ZIP-CHANGED-SW                       00307900
890989                 MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW                 00308000
890989             WHEN OTHER                                           00308100
890989                 CONTINUE                                         00308200
890989         END-EVALUATE                                             00308300
890989         EVALUATE TRUE                                            00308400
890989             WHEN ZIP-PLUS4 NOT EQUAL COM-ZIP-PLUS4               00308500
890989                 MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW                 00308600
890989             WHEN ZIP NOT EQUAL COM-ZIP                           00308700
890989                 MOVE 'Y' TO ZIP-CHANGED-SW                       00308800
890989             WHEN OTHER                                           00308900
890989                 CONTINUE                                         00309000
890989         END-EVALUATE.                                            00309100
R11386****************************************************              00309200
R11386* START OF COUNTY CODE UPDATE FROM STATE_COUNTY_ZIP*              00309300
R11386****************************************************              00309400
R11386 1300-STATE-COUNTY-ZIP.                                           00309500
R11386                                                                  00309600
R11386                                                                  00309700
R11386        EXEC SQL                                                  00309800
R11386           SELECT STATE_NUMBER||COUNTY_NUMBER AS COUNTY_CODE      00309900
R11386             INTO :WS-SCZ-COUNTY-CODE                             00310000
R11386             FROM STATE_COUNTY_ZIP                                00310100
R11386            WHERE STATE_CD    = :NA-COMM-STATE                    00310200
R11386            AND ZIP_CODE      = :WS-SCZ-ZIP-C                     00310300
R11386            FETCH FIRST ROW ONLY                                  00310400
R11386        END-EXEC                                                  00310500
R11386                                                                  00310600
R11386         EVALUATE SQLCODE                                         00310700
R11386           WHEN  0                                                00310800
R11386             MOVE WS-SCZ-COUNTY-CODE-N TO NA-COMM-COUNTY-CODE     00310900
R11386             MOVE SPACES TO NA-COMM-FINALIST-REASON               00311000
R11386           WHEN 100                                               00311100
R11386             CONTINUE                                             00311200
R11386           WHEN OTHER                                             00311300
R11386             GO TO 0420-DB2-ERROR                                 00311400
R11386         END-EVALUATE                                             00311500
R11386           MOVE NA-COMM-FINALIST-REASON TO WS-FINALST-REAS-CODE.  00311600
R11386                                                                  00311700
R11386 1300-EXIT.                                                       00311800
R11386      EXIT.                                                       00311900
R11386**************************************************                00312000
R11386* END OF COUNTY CODE UPDATE FROM STATE_COUNTY_ZIP*                00312100
R11386**************************************************                00312200
       0160-NUMERIC-CHECK.                                              00312300
                                                                        00312400
           IF WS-NUMERIC-CHECK-BYTE (ACTION-SUB) NUMERIC                00312500
               MOVE 'Y' TO WS-NUMERIC-SW.                               00312600
                                                                        00312700
                                                                        00312800
       0170-SLIDE-ERROR-MESSAGES.                                       00312900
                                                                        00313000
           IF WS-MESSAGE-NUMBER1 = SPACES OR LOW-VALUES                 00313100
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER1               00313200
           ELSE                                                         00313300
           IF WS-MESSAGE-NUMBER2 = SPACES OR LOW-VALUES                 00313400
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER2               00313500
           ELSE                                                         00313600
           IF WS-MESSAGE-NUMBER3 = SPACES OR LOW-VALUES                 00313700
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER3               00313800
           ELSE                                                         00313900
           IF WS-MESSAGE-NUMBER4 = SPACES OR LOW-VALUES                 00314000
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER4.              00314100
                                                                        00314200
       0190-SEND-NA340M1-FIRST-TIME.                                    00314300
           DISPLAY '0190-SEND-NA340M1-FIRST-TIME PARA'                  00314400
           PERFORM 0390-CHECK-ERROR-MESSAGES                            00314500
               VARYING ACTION-SUB FROM 1 BY 1                           00314600
                   UNTIL ACTION-SUB > 4.                                00314700
                                                                        00314800
R01717** FOR CASE OR CAM DATA, CHECK CARRIER SECURITY                   00314900
R01717*    IF COMM-CUST-TYPE = 'AM' OR 'CA'                             00315000
R01717*       PERFORM 0590-CHECK-SECURITY THRU 0590-EXIT                00315100
R01717*       IF WS-MESSAGE-NUMBER1 > SPACES                            00315200
R01717*         MOVE +100 TO SQLCODE                                    00315300
R01717*         GO TO 0310-INITIATE-ROUTER.                             00315400
                                                                        00315500
           PERFORM 0250-PROCESS-DB2-REQUESTS.                           00315600
           DISPLAY '0250 WS-RECORD-NOTFND' WS-RECORD-NOTFND             00315700
           IF WS-RECORD-NOTFND NOT = 'Y'                                00315800
               PERFORM 0280-MOVE-SPACES-TO-MAP                          00315900
           ELSE                                                         00316000
           IF WS-RECORD-NOTFND = 'Y'  AND                               00316100
900837        WS-ZERO-LENGTH = LENGTH OF WSQ-COMMAREA                   00316200
                 MOVE COM-AGNTNAME TO WSQ-AGNTNAME                      00316300
                 PERFORM 0290-MOVE-OUT-MAP-FIELDS.                      00316400
                                                                        00316500
           MOVE SPACES       TO COMM-MSG-MAX-SEVERITY.                  00316600
           MOVE -1           TO MAP-SYSTEM-CDL.                         00316700
                                                                        00316800
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00316900
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00317000
               MOVE SPACES  TO COMM-SYSTEM-CODE                         00317100
           ELSE                                                         00317200
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00317300
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00317400
               MOVE ALL '_' TO MAP-ACTION-CDO                           00317500
               MOVE SPACES  TO COMM-ACTION-CODE                         00317600
           ELSE                                                         00317700
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00317800
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00317900
               MOVE ALL '_' TO MAP-CUST-INFOO                           00318000
               MOVE SPACES  TO COMM-CUSTOMER-INFO                       00318100
           ELSE                                                         00318200
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00318300
                                                                        00318400
961037     IF ENTITY-TYPE = 'RP'                                        00318500
961037         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA         00318600
961037     END-IF.                                                      00318700
                                                                        00318800
R03179     IF ENTITY-TYPE = 'CA'                                        00318900
R03179         MOVE ATTRB-PROT-ASKIP        TO MAP-EMAIL1A MAP-EMAIL2A  00319000
R03179     END-IF.                                                      00319100
                                                                        00319200
           EXEC CICS SEND MAP    ('NA340M1')                            00319300
                          CURSOR                                        00319400
                          ERASE                                         00319500
                          RESP (WS-CICS-RESP)                           00319600
                          END-EXEC.                                     00319700
                                                                        00319800
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00319900
                GO TO 9999-CICS-ERROR.                                  00320000
                                                                        00320100
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00320200
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00320300
                                                                        00320400
           MOVE 'NAU0'             TO COMM-PREVIOUS-TRANID.             00320500
           MOVE '01'               TO COMM-NEXT-FUNCTION.               00320600
           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                  00320700
900837     MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.           00320800
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00320900
                               FROM   (WSQ-COMMAREA)                    00321000
                               LENGTH (WS-COMM-DB2-LENGTH)              00321100
                               ITEM   (WS-TS-ITEM)                      00321200
                               RESP   (WS-CICS-RESP)                    00321300
                               REWRITE                                  00321400
                               MAIN                                     00321500
                               END-EXEC.                                00321600
                                                                        00321700
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00321800
                GO TO 9999-CICS-ERROR.                                  00321900
                                                                        00322000
           EXEC CICS RETURN TRANSID ('NAU0')                            00322100
                            END-EXEC.                                   00322200
                                                                        00322300
       0200-SEND-NA340M1-MAP.                                           00322400
           DISPLAY '0200-SEND-NA340M1-MAP  PARA'.                       00322500
           MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1).                   00322600
           MOVE WS-MESSAGE-NUMBER2 TO COMM-MSG-ID(2).                   00322700
           MOVE WS-MESSAGE-NUMBER3 TO COMM-MSG-ID(3).                   00322800
           MOVE WS-MESSAGE-NUMBER4 TO COMM-MSG-ID(4).                   00322900
                                                                        00323000
           PERFORM 0390-CHECK-ERROR-MESSAGES                            00323100
               VARYING ACTION-SUB FROM 1 BY 1                           00323200
                   UNTIL ACTION-SUB > 4.                                00323300
                                                                        00323400
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00323500
               MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                    00323600
                                                                        00323700
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00323800
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00323900
               MOVE SPACES  TO COMM-SYSTEM-CODE                         00324000
           ELSE                                                         00324100
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00324200
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00324300
               MOVE ALL '_' TO MAP-ACTION-CDO                           00324400
               MOVE SPACES  TO COMM-ACTION-CODE                         00324500
           ELSE                                                         00324600
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00324700
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00324800
               MOVE ALL '_' TO MAP-CUST-INFOO                           00324900
               MOVE SPACES  TO COMM-CUSTOMER-INFO                       00325000
           ELSE                                                         00325100
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00325200
                                                                        00325300
                                                                        00325400
           PERFORM 0290-MOVE-OUT-MAP-FIELDS.                            00325500
900837     IF WS-EDIT-SW = 'Y'                                          00325600
900837         EXEC CICS SEND CONTROL ALARM END-EXEC                    00325700
900837     ELSE                                                         00325800
               MOVE -1 TO MAP-SYSTEM-CDL.                               00325900
                                                                        00326000
           EXEC CICS SEND MAP    ('NA340M1')                            00326100
                          CURSOR                                        00326200
                          ERASE                                         00326300
                          RESP (WS-CICS-RESP)                           00326400
                          END-EXEC.                                     00326500
                                                                        00326600
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00326700
                GO TO 9999-CICS-ERROR.                                  00326800
                                                                        00326900
                                                                        00327000
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00327100
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00327200
                                                                        00327300
           MOVE 'NAU0'             TO COMM-PREVIOUS-TRANID.             00327400
           MOVE '01'               TO COMM-NEXT-FUNCTION.               00327500
           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                  00327600
           MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                     00327700
900837     MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.           00327800
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00327900
                               FROM   (WSQ-COMMAREA)                    00328000
                               LENGTH (WS-COMM-DB2-LENGTH)              00328100
                               ITEM   (WS-TS-ITEM)                      00328200
                               RESP   (WS-CICS-RESP)                    00328300
                               REWRITE                                  00328400
                               MAIN                                     00328500
                               END-EXEC.                                00328600
                                                                        00328700
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00328800
                GO TO 9999-CICS-ERROR.                                  00328900
                                                                        00329000
           EXEC CICS RETURN TRANSID ('NAU0')                            00329100
                            END-EXEC.                                   00329200
                                                                        00329300
                                                                        00329400
       0210-UPDATE-AGNTNAME-ROW.                                        00329500
           DISPLAY '0210-UPDATE-AGNTNAME-ROW PARA'                      00329600
           EXEC SQL                                                     00329700
               OPEN AGNTCUR                                             00329800
           END-EXEC.                                                    00329900
           IF SQLCODE NOT = ZERO                                        00330000
               GO TO 0420-DB2-ERROR.                                    00330100
                                                                        00330200
           EXEC SQL FETCH AGNTCUR                                       00330300
R01142         INTO :DCLAGNTNAME:AGNTNAME-INDICATORS                    00330400
           END-EXEC.                                                    00330500
                                                                        00330600
           IF SQLCODE NOT = ZERO                                        00330700
               GO TO 0420-DB2-ERROR.                                    00330800
                                                                        00330900
           IF IND-LAST-NAME IS NEGATIVE                                 00331000
               MOVE SPACES TO LAST-NAME.                                00331100
           IF IND-FIRST-NAME IS NEGATIVE                                00331200
               MOVE SPACES TO FIRST-NAME.                               00331300
           IF IND-MIDDLE-NAME IS NEGATIVE                               00331400
               MOVE SPACES TO MIDDLE-NAME.                              00331500
           IF IND-PREFIX IS NEGATIVE                                    00331600
               MOVE SPACES TO PREFIX.                                   00331700
           IF IND-SUFFIX1 IS NEGATIVE                                   00331800
               MOVE SPACES TO SUFFIX1.                                  00331900
           IF IND-SUFFIX2 IS NEGATIVE                                   00332000
               MOVE SPACES TO SUFFIX2.                                  00332100
           IF IND-COMPANY-NAME IS NEGATIVE                              00332200
               MOVE SPACES TO COMPANY-NAME.                             00332300
           IF IND-BIRTH-DATE IS NEGATIVE                                00332400
               MOVE ZEROES TO BIRTH-DATE.                               00332500
           IF IND-DISPLAY-NAME IS NEGATIVE                              00332600
               MOVE SPACES TO DISPLAY-NAME.                             00332700
           IF IND-NICKNAME IS NEGATIVE                                  00332800
               MOVE SPACES TO NICKNAME.                                 00332900
           IF IND-ADDRESS2 IS NEGATIVE                                  00333000
               MOVE SPACES TO ADDRESS2.                                 00333100
           IF IND-ASSOCIATION1 IS NEGATIVE                              00333200
               MOVE SPACES TO ASSOCIATION1.                             00333300
           IF IND-ASSOCIATION2 IS NEGATIVE                              00333400
               MOVE SPACES TO ASSOCIATION2.                             00333500
           IF IND-ASSOCIATION3 IS NEGATIVE                              00333600
               MOVE SPACES TO ASSOCIATION3.                             00333700
                                                                        00333800
           MOVE SPACES TO AUDIT-COMM-AREA.                              00333900
           MOVE DCLAGNTNAME TO AC-PREV-RECORD.                          00334000
                                                                        00334100
           EXEC SQL                                                     00334200
               CLOSE AGNTCUR                                            00334300
           END-EXEC.                                                    00334400
                                                                        00334500
           IF SQLCODE NOT = ZERO                                        00334600
               GO TO 0420-DB2-ERROR.                                    00334700
                                                                        00334800
      *---------------------------------------------------------------* 00334900
      *    THIS CODE IS NEEDED TO UPDATE THE AUDIT RECORD WITH        * 00335000
      *    THE IMAGE OF THE RECORD BEFORE IT WAS CHANGED. DB2         * 00335100
      *    DOES NOT MAKE THE RECORD AVAILABLE IN THE REDEFINES        * 00335200
      *    AREA WHEN IT IS FETCED FOR UPDATE.                         * 00335300
      *---------------------------------------------------------------* 00335400
                                                                        00335500
                                                                        00335600
                                                                        00335700
           EXEC SQL                                                     00335800
               OPEN UPDT-AGNT-CUR                                       00335900
           END-EXEC.                                                    00336000
                                                                        00336100
           IF SQLCODE NOT = ZERO                                        00336200
               GO TO 0420-DB2-ERROR.                                    00336300
                                                                        00336400
           EXEC SQL FETCH UPDT-AGNT-CUR                                 00336500
R01142         INTO :DCLAGNTNAME:AGNTNAME-INDICATORS                    00336600
           END-EXEC.                                                    00336700
           DISPLAY 'SQLCODE OF FETCH UPDT CUR' SQLCODE                  00336800
           IF SQLCODE = 100 AND COMM-PREVIOUS-TRANID NOT = 'NAU0'       00336900
               MOVE 'NA020'  TO WS-MESSAGE-NUMBER1                      00337000
               GO TO 0310-INITIATE-ROUTER                               00337100
           ELSE                                                         00337200
              IF SQLCODE = 100                                          00337300
                  MOVE 'NA020'           TO WS-MESSAGE-NUMBER1          00337400
                  PERFORM 0380-BUMP-ERROR-MESSAGES                      00337500
              ELSE                                                      00337600
                 IF SQLCODE NOT = ZERO                                  00337700
                     GO TO 0420-DB2-ERROR.                              00337800
                                                                        00337900
                                                                        00338000
                                                                        00338100
           IF IND-LAST-NAME IS NEGATIVE                                 00338200
               MOVE SPACES TO LAST-NAME.                                00338300
           IF IND-FIRST-NAME IS NEGATIVE                                00338400
               MOVE SPACES TO FIRST-NAME.                               00338500
           IF IND-MIDDLE-NAME IS NEGATIVE                               00338600
               MOVE SPACES TO MIDDLE-NAME.                              00338700
           IF IND-PREFIX IS NEGATIVE                                    00338800
               MOVE SPACES TO PREFIX.                                   00338900
           IF IND-SUFFIX1 IS NEGATIVE                                   00339000
               MOVE SPACES TO SUFFIX1.                                  00339100
           IF IND-SUFFIX2 IS NEGATIVE                                   00339200
               MOVE SPACES TO SUFFIX2.                                  00339300
           IF IND-COMPANY-NAME IS NEGATIVE                              00339400
               MOVE SPACES TO COMPANY-NAME.                             00339500
           IF IND-BIRTH-DATE IS NEGATIVE                                00339600
               MOVE ZEROES TO BIRTH-DATE.                               00339700
           IF IND-DISPLAY-NAME IS NEGATIVE                              00339800
               MOVE SPACES TO DISPLAY-NAME.                             00339900
           IF IND-NICKNAME IS NEGATIVE                                  00340000
               MOVE SPACES TO NICKNAME.                                 00340100
           IF IND-ADDRESS2 IS NEGATIVE                                  00340200
               MOVE SPACES TO ADDRESS2.                                 00340300
           IF IND-ASSOCIATION1 IS NEGATIVE                              00340400
               MOVE SPACES TO ASSOCIATION1.                             00340500
           IF IND-ASSOCIATION2 IS NEGATIVE                              00340600
               MOVE SPACES TO ASSOCIATION2.                             00340700
           IF IND-ASSOCIATION3 IS NEGATIVE                              00340800
               MOVE SPACES TO ASSOCIATION3.                             00340900
                                                                        00341000
           PERFORM  0230-MOVE-COMMAREA-TO-TABLE.                        00341100
MANJU2     DISPLAY 'ENTITY-TYPE IN 210 UPDT PARA' ENTITY-TYPE.          00341200
           EXEC SQL                                                     00341300
900837*       UPDATE AGNTNV03                                           00341400
R00700        UPDATE AGNTNAME                                           00341500
                  SET FIRST_NAME = :FIRST-NAME,                         00341600
                      MIDDLE_NAME = :MIDDLE-NAME,                       00341700
                      LAST_NAME = :LAST-NAME,                           00341800
                      PREFIX = :PREFIX, SUFFIX1 = :SUFFIX1,             00341900
                      SUFFIX2 = :SUFFIX2, COMPANY_IND = :COMPANY-IND,   00342000
                      COMPANY_IN_ADDRESS = :COMPANY-IN-ADDRESS,         00342100
                      COMPANY_NAME = :COMPANY-NAME,                     00342200
                      DISPLAY_NAME = :DISPLAY-NAME,                     00342300
                      NICKNAME = :NICKNAME, ADDRESS1 = :ADDRESS1,       00342400
                      ADDRESS2 = :ADDRESS2, CITY = :CITY,               00342500
                      STATE = :STATE, ZIP=:ZIP,                         00342600
                      ZIP_PLUS4 = :ZIP-PLUS4,                           00342700
                      COUNTY_CODE = :COUNTY-CODE,                       00342800
                      AREA_CODE = :AREA-CODE, PHONE = :PHONE,           00342900
                      PHONE_EXTENSION = :PHONE-EXTENSION,               00343000
                      SSN = :SSN-NUM,                                   00343100
                      SEX = :SEX,                                       00343200
                      BIRTH_DATE = :BIRTH-DATE:IND-BIRTH-DATE,          00343300
                      FINALST_REAS_CODE = :FINALST-REAS-CODE,           00343400
                      FINALST_OVRD_IND = :FINALST-OVRD-IND,             00343500
                      DUP_ADDR_OVRD_IND = :DUP-ADDR-OVRD-IND,           00343600
                    EFFECTIVE_DATE = :EFFECTIVE-DATE:IND-EFFECTIVE-DATE,00343700
                      CHANGE_DATE = CURRENT TIMESTAMP,                  00343800
                      CHANGE_LOGON = :CHANGE-LOGON,                     00343900
                      ENTITY_TYPE = :ENTITY-TYPE,                       00344000
                      RECORD_STATUS = :RECORD-STATUS,                   00344100
                      ALT_ADDRESS_IND = :ALT-ADDRESS-IND,               00344200
                      FUTURE_ADDRESS_IND = :FUTURE-ADDRESS-IND,         00344300
                      RECORD_ORIGIN = :RECORD-ORIGIN,                   00344400
                      COMBINED_STATUS = :COMBINED-STATUS,               00344500
                      SITE_CODE = :SITE-CODE,                           00344600
                      NAME_KEY1 = :NAME-KEY1,                           00344700
                      NAME_KEY2 = :NAME-KEY2,                           00344800
                      NAME_KEY3 = :NAME-KEY3,                           00344900
                      ADDRESS_KEY1 = :ADDRESS-KEY1,                     00345000
                      ASSOCIATION1 = :ASSOCIATION1,                     00345100
                      ASSOCIATION2 = :ASSOCIATION2,                     00345200
                      ASSOCIATION3 = :ASSOCIATION3,                     00345300
900837                FAX_AREA_CODE = :FAX-AREA-CODE,                   00345400
900837                FAX_PHONE = :FAX-PHONE                            00345500
              WHERE CURRENT OF UPDT-AGNT-CUR                            00345600
           END-EXEC.                                                    00345700
                                                                        00345800
           IF SQLCODE = -508                                            00345900
               MOVE SPACES TO WS-MESSAGE-NUMBER2                        00346000
                              COMM-MSG-ID(2)                            00346100
                              MAP-ERROR-MSGO(2)                         00346200
               GO TO 0200-SEND-NA340M1-MAP                              00346300
           ELSE                                                         00346400
           IF SQLCODE NOT = ZERO                                        00346500
              GO TO 0420-DB2-ERROR                                      00346600
           ELSE                                                         00346700
              MOVE 'NA159'           TO WS-MESSAGE-NUMBER1.             00346800
                                                                        00346900
                                                                        00347000
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00347100
               GO TO 9999-CICS-ERROR.                                   00347200
                                                                        00347300
      *---------------------------------------------------------------* 00347400
      *   WRITE AUDIT RECORD TO REFLECT THAT A CHANGE HAS TAKEN PLACE * 00347500
      *---------------------------------------------------------------* 00347600
           EXEC CICS ASSIGN OPID(OPER-ID)                               00347700
                            RESP   (WS-CICS-RESP)                       00347800
                            END-EXEC.                                   00347900
                                                                        00348000
           MOVE OPER-ID      TO INITIALS.                               00348100
           MOVE IDNTITY      TO AC-ID-NUMBER.                           00348200
           MOVE DCLAGNTNAME  TO AC-CURR-RECORD.                         00348300
           MOVE 'NAU0'       TO AC-TRANID.                              00348400
           MOVE 'NC'         TO AUDIT-CODE.                             00348500
900837     COMPUTE AC-REMAINDER-LENGTH = 2 * LENGTH OF DCLAGNTNAME.     00348600
                                                                        00348700
MANJU *    EXEC CICS LINK PROGRAM ('AUDPRG2')                           00348800
      *                   COMMAREA (AUDIT-COMM-AREA)                    00348900
990721**                  LENGTH (WS-AUD-LENGTH)                        00349000
990721*                   LENGTH (LENGTH OF AUDIT-COMM-AREA)            00349100
      *                   END-EXEC.                                     00349200
                                                                        00349300
      *---------------------------------------------------------------* 00349400
      *   THIS CODE IS USED TO CHECK FOR A DISABLED OR NOT OPEN       * 00349500
      *   CONDITION ON THE WRITE OF THE AUDIT PROGRAM.                * 00349600
      *---------------------------------------------------------------* 00349700
      *                                                               * 00349800
      *    IF CICS-ERROR-SW = 'Y'                                     * 00349900
      *        EXEC CICS RETURN                                       * 00350000
      *                  END-EXEC.                                    * 00350100
      *                                                               * 00350200
      *---------------------------------------------------------------* 00350300
                                                                        00350400
           EXEC SQL                                                     00350500
               CLOSE UPDT-AGNT-CUR                                      00350600
           END-EXEC.                                                    00350700
                                                                        00350800
      *---------------------------------------------------------------* 00350900
      * IF THE CUSTOMER DECIDES TO OVERRIDE THE FINALST EDITS AND     * 00351000
      * ADD THE ADDRESS AS ENTERED, THIS PARAGRAPH WRITES OUT A       * 00351100
      * COPY OF THE RECORD FOR THE CIM DEPARTMENT. THE CIM DEPARTMENT * 00351200
      * USES THIS RECORD TO DECIDE IF THE ADDRESS ADDED  WAS VALID    * 00351300
      * IF THEY DETERMINE IT IS INVALID THE CUSTOMER IS CALLED AND    * 00351400
      * ADVISED THAT THE RECORD SHOULD BE CHANGED.                    * 00351500
      *---------------------------------------------------------------* 00351600
                                                                        00351700
      *---------------------------------------------------------------* 00351800
      *    IF COM-FINALST-OVRD-IND = 'Y'                              * 00351900
      *        PERFORM 0240-WRITE-ADDR-OVRD-TABLE.                    * 00352000
      *---------------------------------------------------------------* 00352100
                                                                        00352200
      *    IF COMM-CUST-TYPE NOT = 'BR' AND COM-ENTITY-TYPE = 'BR'      00352300
      *        PERFORM 0260-COMPLETE-BROKER-ADD.                        00352400
                                                                        00352500
MANJU *    IF COM-DISP-IND = 'N'                                        00352600
      *        PERFORM 0270-REASSEMBLE-NAME-CHANGE.                     00352700
                                                                        00352800
                                                                        00352900
       0220-UPDATE-CASENAME-ROW.                                        00353000
                                                                        00353100
           EXEC SQL                                                     00353200
               OPEN CASECUR                                             00353300
           END-EXEC.                                                    00353400
                                                                        00353500
           IF SQLCODE NOT = ZERO                                        00353600
               GO TO 0420-DB2-ERROR.                                    00353700
                                                                        00353800
KPL        EXEC SQL FETCH CASECUR                                       00353900
R00700*        INTO DCLAGNTNAME:AGNTNAME-INDICATORS                     00354000
C23311     INTO  :IDNTITY, :LAST-NAME :IND-LAST-NAME,                   00354100
C23311       :FIRST-NAME :IND-FIRST-NAME,                               00354200
R00700       :MIDDLE-NAME :IND-MIDDLE-NAME,                             00354300
R00700       :PREFIX :IND-PREFIX, :SUFFIX1 :IND-SUFFIX1,                00354400
R00700       :SUFFIX2 :IND-SUFFIX2,                                     00354500
R00700       :COMPANY-IND, :COMPANY-IN-ADDRESS,                         00354600
C23311       :COMPANY-NAME :IND-COMPANY-NAME,                           00354700
C23311       :DISPLAY-NAME :IND-DISPLAY-NAME,                           00354800
R00700       :NICKNAME :IND-NICKNAME,                                   00354900
C23311       :ADDRESS1, :ADDRESS2 :IND-ADDRESS2,                        00355000
R00700       :CITY, :STATE, :ZIP, :ZIP-PLUS4, :COUNTY-CODE, :AREA-CODE, 00355100
R00700       :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,                  00355200
R00700       :BIRTH-DATE :IND-BIRTH-DATE,                               00355300
R00700       :FINALST-REAS-CODE, :FINALST-OVRD-IND, :DUP-ADDR-OVRD-IND, 00355400
R00700       :EFFECTIVE-DATE, :CHANGE-DATE,                             00355500
R00700       :CHANGE-LOGON, :ENTITY-TYPE,                               00355600
R00700       :RECORD-STATUS, :ALT-ADDRESS-IND, :FUTURE-ADDRESS-IND,     00355700
R00700       :RECORD-ORIGIN, :COMBINED-STATUS, :SITE-CODE, :NAME-KEY1,  00355800
R00700       :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,                     00355900
R00700       :ASSOCIATION1 :IND-ASSOCIATION1,                           00356000
R00700       :ASSOCIATION2 :IND-ASSOCIATION2,                           00356100
R00700       :ASSOCIATION3 :IND-ASSOCIATION3,                           00356200
R00700       :FAX-AREA-CODE, :FAX-PHONE,                                00356300
R04023       :ORIGINAL-STATE,                                           00356400
R02539       :EMAIL1 :IND-EMAIL1, :PASS-WORD :IND-PASSWORD,             00356500
R02539       :COUNTRY-CODE :IND-COUNTRY-CODE,                           00356600
R02539       :POSTAL-CODE :IND-POSTAL-CODE                              00356700
           END-EXEC.                                                    00356800
                                                                        00356900
           IF SQLCODE NOT = ZERO                                        00357000
               GO TO 0420-DB2-ERROR.                                    00357100
                                                                        00357200
           IF IND-LAST-NAME IS NEGATIVE                                 00357300
               MOVE SPACES TO LAST-NAME.                                00357400
           IF IND-FIRST-NAME IS NEGATIVE                                00357500
               MOVE SPACES TO FIRST-NAME.                               00357600
           IF IND-MIDDLE-NAME IS NEGATIVE                               00357700
               MOVE SPACES TO MIDDLE-NAME.                              00357800
           IF IND-PREFIX IS NEGATIVE                                    00357900
               MOVE SPACES TO PREFIX.                                   00358000
           IF IND-SUFFIX1 IS NEGATIVE                                   00358100
               MOVE SPACES TO SUFFIX1.                                  00358200
           IF IND-SUFFIX2 IS NEGATIVE                                   00358300
               MOVE SPACES TO SUFFIX2.                                  00358400
           IF IND-COMPANY-NAME IS NEGATIVE                              00358500
               MOVE SPACES TO COMPANY-NAME.                             00358600
           IF IND-BIRTH-DATE IS NEGATIVE                                00358700
               MOVE ZEROES TO BIRTH-DATE.                               00358800
           IF IND-DISPLAY-NAME IS NEGATIVE                              00358900
               MOVE SPACES TO DISPLAY-NAME.                             00359000
           IF IND-NICKNAME IS NEGATIVE                                  00359100
               MOVE SPACES TO NICKNAME.                                 00359200
           IF IND-ADDRESS2 IS NEGATIVE                                  00359300
               MOVE SPACES TO ADDRESS2.                                 00359400
           IF IND-ASSOCIATION1 IS NEGATIVE                              00359500
               MOVE SPACES TO ASSOCIATION1.                             00359600
           IF IND-ASSOCIATION2 IS NEGATIVE                              00359700
               MOVE SPACES TO ASSOCIATION2.                             00359800
           IF IND-ASSOCIATION3 IS NEGATIVE                              00359900
               MOVE SPACES TO ASSOCIATION3.                             00360000
           IF IND-FAX-PHONE IS NEGATIVE                                 00360100
               MOVE SPACES TO FAX-PHONE.                                00360200
           IF IND-EMAIL1 IS NEGATIVE                                    00360300
               MOVE SPACES TO EMAIL1.                                   00360400
R02539     IF IND-PASSWORD IS NEGATIVE                                  00360500
R02539         MOVE SPACES TO PASS-WORD.                                00360600
R02539     IF IND-POSTAL-CODE IS NEGATIVE                               00360700
R02539         MOVE SPACES TO POSTAL-CODE.                              00360800
R02539     IF IND-COUNTRY-CODE IS NEGATIVE                              00360900
R02539         MOVE SPACES TO COUNTRY-CODE.                             00361000
                                                                        00361100
           MOVE DCLAGNTNAME TO AC-PREV-RECORD.                          00361200
                                                                        00361300
           EXEC SQL                                                     00361400
               CLOSE CASECUR                                            00361500
           END-EXEC.                                                    00361600
                                                                        00361700
           IF SQLCODE NOT = ZERO                                        00361800
               GO TO 0420-DB2-ERROR.                                    00361900
                                                                        00362000
      *---------------------------------------------------------------* 00362100
      *    THIS CODE IS NEEDED TO UPDATE THE AUDIT RECORD WITH        * 00362200
      *    THE IMAGE OF THE RECORD BEFORE IT WAS CHANGED. DB2         * 00362300
      *    DOES NOT MAKE THE RECORD AVAILABLE IN THE REDEFINES        * 00362400
      *    AREA WHEN IT IS FETCHED FOR UPDATE.                         *00362500
      *---------------------------------------------------------------* 00362600
           EXEC SQL                                                     00362700
               OPEN UPDT-CASE-CUR                                       00362800
           END-EXEC.                                                    00362900
           IF SQLCODE NOT = ZERO                                        00363000
               GO TO 0420-DB2-ERROR.                                    00363100
                                                                        00363200
           EXEC SQL FETCH UPDT-CASE-CUR                                 00363300
110611*        INTO :DCLCASENAME:CASENAME-INDICATORS                    00363400
110611                INTO  :IDNTITY, :LAST-NAME :IND-LAST-NAME,        00363500
110611                      :FIRST-NAME :IND-FIRST-NAME,                00363600
110611                      :MIDDLE-NAME :IND-MIDDLE-NAME,              00363700
110611                      :PREFIX :IND-PREFIX, :SUFFIX1 :IND-SUFFIX1, 00363800
110611                      :SUFFIX2 :IND-SUFFIX2,                      00363900
110611                      :COMPANY-IND, :COMPANY-IN-ADDRESS,          00364000
110611                      :COMPANY-NAME :IND-COMPANY-NAME,            00364100
110611                      :DISPLAY-NAME :IND-DISPLAY-NAME,            00364200
110611                      :NICKNAME :IND-NICKNAME,                    00364300
110611                      :ADDRESS1, :ADDRESS2 :IND-ADDRESS2,         00364400
110611                      :CITY, :STATE, :ZIP, :ZIP-PLUS4,            00364500
110611                      :COUNTY-CODE, :AREA-CODE,                   00364600
110611                      :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,   00364700
110611                      :BIRTH-DATE :IND-BIRTH-DATE,                00364800
110611                      :FINALST-REAS-CODE, :FINALST-OVRD-IND,      00364900
110611                      :DUP-ADDR-OVRD-IND,                         00365000
110611                      :EFFECTIVE-DATE, :CHANGE-DATE,              00365100
110611                      :CHANGE-LOGON, :ENTITY-TYPE,                00365200
110611                      :RECORD-STATUS, :ALT-ADDRESS-IND,           00365300
110611                      :FUTURE-ADDRESS-IND,                        00365400
110611                      :RECORD-ORIGIN, :COMBINED-STATUS,           00365500
110611                      :SITE-CODE, :NAME-KEY1,                     00365600
110611                      :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,      00365700
110611                      :ASSOCIATION1 :IND-ASSOCIATION1,            00365800
110611                      :ASSOCIATION2 :IND-ASSOCIATION2,            00365900
110611                      :ASSOCIATION3 :IND-ASSOCIATION3,            00366000
110611                      :FAX-AREA-CODE, :FAX-PHONE,                 00366100
110611                      :ORIGINAL-STATE,                            00366200
110611                      :EMAIL1 :IND-EMAIL1,                        00366300
R9031A****                  :PASS-WORD :IND-PASSWORD,                   00366400
110611                      :COUNTRY-CODE :IND-COUNTRY-CODE,            00366500
110611                      :POSTAL-CODE :IND-POSTAL-CODE,              00366600
R9031A                      :WS-EMAIL-STATUS                            00366700
           END-EXEC.                                                    00366800
                                                                        00366900
           IF SQLCODE = 100 AND COMM-PREVIOUS-TRANID NOT = 'NAU0'       00367000
               MOVE 'NA020'    TO WS-MESSAGE-NUMBER1                    00367100
               GO TO 0310-INITIATE-ROUTER                               00367200
           ELSE                                                         00367300
              IF SQLCODE = 100                                          00367400
                  MOVE 'NA020' TO WS-MESSAGE-NUMBER1                    00367500
                  PERFORM 0380-BUMP-ERROR-MESSAGES                      00367600
              ELSE                                                      00367700
                 IF SQLCODE NOT = ZERO                                  00367800
                     GO TO 0420-DB2-ERROR.                              00367900
           IF IND-LAST-NAME IS NEGATIVE                                 00368000
               MOVE SPACES TO LAST-NAME.                                00368100
           IF IND-FIRST-NAME IS NEGATIVE                                00368200
               MOVE SPACES TO FIRST-NAME.                               00368300
           IF IND-MIDDLE-NAME IS NEGATIVE                               00368400
               MOVE SPACES TO MIDDLE-NAME.                              00368500
           IF IND-PREFIX IS NEGATIVE                                    00368600
               MOVE SPACES TO PREFIX.                                   00368700
           IF IND-SUFFIX1 IS NEGATIVE                                   00368800
               MOVE SPACES TO SUFFIX1.                                  00368900
           IF IND-SUFFIX2 IS NEGATIVE                                   00369000
               MOVE SPACES TO SUFFIX2.                                  00369100
           IF IND-COMPANY-NAME IS NEGATIVE                              00369200
               MOVE SPACES TO COMPANY-NAME.                             00369300
           IF IND-BIRTH-DATE IS NEGATIVE                                00369400
               MOVE ZEROES TO BIRTH-DATE.                               00369500
           IF IND-DISPLAY-NAME IS NEGATIVE                              00369600
               MOVE SPACES TO DISPLAY-NAME.                             00369700
           IF IND-NICKNAME IS NEGATIVE                                  00369800
               MOVE SPACES TO NICKNAME.                                 00369900
           IF IND-ADDRESS2 IS NEGATIVE                                  00370000
               MOVE SPACES TO ADDRESS2.                                 00370100
           IF IND-ASSOCIATION1 IS NEGATIVE                              00370200
               MOVE SPACES TO ASSOCIATION1.                             00370300
           IF IND-ASSOCIATION2 IS NEGATIVE                              00370400
               MOVE SPACES TO ASSOCIATION2.                             00370500
           IF IND-ASSOCIATION3 IS NEGATIVE                              00370600
               MOVE SPACES TO ASSOCIATION3.                             00370700
R02539     IF IND-POSTAL-CODE IS NEGATIVE                               00370800
R02539         MOVE SPACES TO POSTAL-CODE.                              00370900
R02539     IF IND-COUNTRY-CODE IS NEGATIVE                              00371000
R02539         MOVE SPACES TO COUNTRY-CODE.                             00371100
                                                                        00371200
R9031A     IF WS-EMAIL-STATUS = 'I' AND                                 00371300
R9031A        EMAIL1 > SPACES                                           00371400
R9031A        MOVE ' ' TO WS-EMAIL-STATUS                               00371500
R9031A     END-IF.                                                      00371600
                                                                        00371700
890989     IF STATE-CHANGED-SW = 'Y'                                    00371800
890989     OR ZIP-CHANGED-SW = 'Y'                                      00371900
890989     OR ZIP-PLUS4-CHANGED-SW = 'Y'                                00372000
890989     OR (COM-FINALST-OVRD-IND = 'Y' AND                           00372100
890989         COM-FINALIST-SW      = 'Y')                              00372200
890989         MOVE STATE     TO WS-HOLD-CSN-STATE                      00372300
890989         MOVE ZIP       TO WS-HOLD-CSN-ZIP                        00372400
890989         MOVE ZIP-PLUS4 TO WS-HOLD-CSN-ZIP-PLUS4.                 00372500
           MOVE SPACES TO AUDIT-COMM-AREA.                              00372600
           MOVE DCLAGNTNAME TO AC-PREV-RECORD.                          00372700
           PERFORM 0230-MOVE-COMMAREA-TO-TABLE.                         00372800
                                                                        00372900
           EXEC SQL                                                     00373000
900837*       UPDATE CASENV03                                           00373100
R00700        UPDATE CASENAME                                           00373200
                  SET FIRST_NAME = :FIRST-NAME,                         00373300
                      MIDDLE_NAME = :MIDDLE-NAME,                       00373400
                      LAST_NAME = :LAST-NAME,                           00373500
                      PREFIX = :PREFIX, SUFFIX1 = :SUFFIX1,             00373600
                      SUFFIX2 = :SUFFIX2, COMPANY_IND = :COMPANY-IND,   00373700
                      COMPANY_IN_ADDRESS = :COMPANY-IN-ADDRESS,         00373800
                      COMPANY_NAME = :COMPANY-NAME,                     00373900
                      DISPLAY_NAME = :DISPLAY-NAME,                     00374000
                      NICKNAME = :NICKNAME, ADDRESS1 = :ADDRESS1,       00374100
                      ADDRESS2 = :ADDRESS2, CITY = :CITY,               00374200
                      STATE = :STATE, ZIP=:ZIP,                         00374300
                      ZIP_PLUS4 = :ZIP-PLUS4,                           00374400
                      COUNTY_CODE = :COUNTY-CODE,                       00374500
                      AREA_CODE = :AREA-CODE, PHONE = :PHONE,           00374600
                      PHONE_EXTENSION = :PHONE-EXTENSION,               00374700
                      SSN = :SSN-NUM,                                   00374800
                      SEX = :SEX,                                       00374900
                      BIRTH_DATE = :BIRTH-DATE:IND-BIRTH-DATE,          00375000
                      FINALST_REAS_CODE = :FINALST-REAS-CODE,           00375100
                      FINALST_OVRD_IND = :FINALST-OVRD-IND,             00375200
                      DUP_ADDR_OVRD_IND = :DUP-ADDR-OVRD-IND,           00375300
                    EFFECTIVE_DATE = :EFFECTIVE-DATE:IND-EFFECTIVE-DATE,00375400
                      CHANGE_DATE = CURRENT TIMESTAMP,                  00375500
                      CHANGE_LOGON = :CHANGE-LOGON,                     00375600
                      ENTITY_TYPE = :ENTITY-TYPE,                       00375700
                      RECORD_STATUS = :RECORD-STATUS,                   00375800
                      ALT_ADDRESS_IND = :ALT-ADDRESS-IND,               00375900
                      FUTURE_ADDRESS_IND = :FUTURE-ADDRESS-IND,         00376000
                      RECORD_ORIGIN = :RECORD-ORIGIN,                   00376100
                      COMBINED_STATUS = :COMBINED-STATUS,               00376200
                      SITE_CODE = :SITE-CODE,                           00376300
                      NAME_KEY1 = :NAME-KEY1,                           00376400
                      NAME_KEY2 = :NAME-KEY2,                           00376500
                      NAME_KEY3 = :NAME-KEY3,                           00376600
                      ADDRESS_KEY1 = :ADDRESS-KEY1,                     00376700
                      ASSOCIATION1 = :ASSOCIATION1,                     00376800
                      ASSOCIATION2 = :ASSOCIATION2,                     00376900
                      ASSOCIATION3 = :ASSOCIATION3,                     00377000
900837                FAX_AREA_CODE = :FAX-AREA-CODE,                   00377100
900837                FAX_PHONE = :FAX-PHONE,                           00377200
R04023                ORIGINAL_STATE = :ORIGINAL-STATE,                 00377300
R00700                EMAIL     = :EMAIL1,                              00377400
R02539                COUNTRY_CODE = :COUNTRY-CODE,                     00377500
R02539                POSTAL_CODE = :POSTAL-CODE,                       00377600
R9031A                EMAIL_STATUS = :WS-EMAIL-STATUS                   00377700
              WHERE CURRENT OF UPDT-CASE-CUR                            00377800
           END-EXEC.                                                    00377900
                                                                        00378000
           IF SQLCODE = -508                                            00378100
               MOVE SPACES TO WS-MESSAGE-NUMBER2                        00378200
                              COMM-MSG-ID(2)                            00378300
                              MAP-ERROR-MSGO(2)                         00378400
               GO TO 0200-SEND-NA340M1-MAP                              00378500
             ELSE                                                       00378600
               IF SQLCODE NOT = ZERO                                    00378700
                   GO TO 0420-DB2-ERROR                                 00378800
                 ELSE                                                   00378900
                   MOVE 'NA159'                  TO WS-MESSAGE-NUMBER1. 00379000
                                                                        00379100
890989*    IF (ZIP-PLUS4-CHANGED-SW = 'Y'  OR                           00379200
890989*        STATE-CHANGED-SW = 'Y'  OR                               00379300
890989*        ZIP-CHANGED-SW = 'Y')                                    00379400
890989*        OR (COM-FINALST-OVRD-IND = 'Y' AND                       00379500
890989*        COM-FINALIST-SW      = 'Y')                              00379600
MANJU *        PERFORM 9950-UPDATE-EMPS THRU 9950-EXIT.                 00379700
890989                                                                  00379800
      *---------------------------------------------------------------* 00379900
      *   WRITE AUDIT RECORD TO REFLECT THAT A CHANGE HAS TAKEN PLACE * 00380000
      *---------------------------------------------------------------* 00380100
           EXEC CICS ASSIGN OPID(OPER-ID)                               00380200
                            RESP   (WS-CICS-RESP)                       00380300
                            END-EXEC.                                   00380400
                                                                        00380500
R01717*-----------                                                      00380600
R01717*- GET CASE NUMBER FOR AUDIT PROGRAM 'AUDPRG2'                    00380700
R01717*-----------                                                      00380800
R01717*    IF COMM-CUST-TYPE = 'CA'                                     00380900
R01717*      EXEC SQL SELECT CASE_NUM                                   00381000
R01717*          INTO :CASEX-CASE-NUM                                   00381100
R01717*          FROM CASEXV01                                          00381200
11735A*          FROM CASE_MASTER                                       00381300
R01717*         WHERE CASENAME#IDNTITY = :WS-CIM-NUMBER                 00381400
R01717*      END-EXEC                                                   00381500
                                                                        00381600
R01717*      IF  SQLCODE  = +0                                          00381700
R01717*          MOVE CASEX-CASE-NUM TO UNIQUENUM                       00381800
R01717*      ELSE                                                       00381900
R01717*      IF SQLCODE = 100 AND COMM-PREVIOUS-TRANID NOT = 'NAU0'     00382000
R01717*          MOVE 'AM004'  TO WS-MESSAGE-NUMBER1                    00382100
R01717*          GO TO 0310-INITIATE-ROUTER                             00382200
R01717*      ELSE                                                       00382300
R01717*          MOVE 'AM004'       TO WS-MESSAGE-NUMBER1               00382400
R01717*          MOVE 'Y'           TO WS-RECORD-NOTFND                 00382500
R01717*          PERFORM 0380-BUMP-ERROR-MESSAGES                       00382600
R01717*          GO TO 0220-EXIT                                        00382700
R01717*      END-IF                                                     00382800
R01717*      END-IF                                                     00382900
R01717*      ELSE                                                       00383000
R01717*         MOVE '000000' TO UNIQUENUM                              00383100
R01717*    END-IF                                                       00383200
                                                                        00383300
           MOVE OPER-ID      TO INITIALS.                               00383400
           MOVE IDNTITY          TO AC-ID-NUMBER.                       00383500
           MOVE DCLAGNTNAME  TO AC-CURR-RECORD.                         00383600
           MOVE 'NAU0'       TO AC-TRANID.                              00383700
           MOVE 'CN'         TO AUDIT-CODE.                             00383800
900837     COMPUTE AC-REMAINDER-LENGTH = 2 * LENGTH OF DCLAGNTNAME.     00383900
                                                                        00384000
      *    EXEC CICS LINK PROGRAM ('AUDPRG2')                           00384100
      *                   COMMAREA (AUDIT-COMM-AREA)                    00384200
990721**                  LENGTH (WS-AUD-LENGTH)                        00384300
990721*                   LENGTH (LENGTH OF AUDIT-COMM-AREA)            00384400
      *                   END-EXEC.                                     00384500
                                                                        00384600
                                                                        00384700
890903*    IF (COM-FINALST-OVRD-IND = 'Y' AND                           00384800
890903*        COM-FINALIST-SW      = 'Y')                              00384900
890903*     OR                                                          00385000
DFJ   *       (STATUS-CHANGED-SW = 'Y'  OR                              00385100
DFJ   *        STATE-CHANGED-SW = 'Y'  OR                               00385200
DFJ   *        ZIP-CHANGED-SW = 'Y')                                    00385300
910412*     AND COM-ENTITY-TYPE NOT = 'LB'                              00385400
920124*     AND COM-ENTITY-TYPE NOT = 'BA'                              00385500
960530*     AND COM-ENTITY-TYPE NOT = 'LD'                              00385600
DFJ   *           PERFORM 9910-UPDATE-CAM-TABLES.                       00385700
                                                                        00385800
                                                                        00385900
      **   IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00386000
      *        GO TO 9999-CICS-ERROR.                                   00386100
                                                                        00386200
           EXEC SQL                                                     00386300
               CLOSE UPDT-CASE-CUR                                      00386400
           END-EXEC.                                                    00386500
                                                                        00386600
      *---------------------------------------------------------------* 00386700
      * IF THE CUSTOMER DECIDES TO OVERRIDE THE FINALST EDITS AND     * 00386800
      * ADD THE ADDRESS AS ENTERED, THIS PARAGRAPH WRITES OUT A       * 00386900
      * COPY OF THE RECORD FOR THE CIM DEPARTMENT. THE CIM DEPARTMENT * 00387000
      * USES THIS RECORD TO DECIDE IF THE ADDRESS ADDED  WAS VALID    * 00387100
      * IF THEY DETERMINE IT IS INVALID THE CUSTOMER IS CALLED AND    * 00387200
      * ADVISED THAT THE RECORD SHOULD BE CHANGED.                    * 00387300
      * --------------------------------------------------------------* 00387400
                                                                        00387500
      * --------------------------------------------------------------* 00387600
      *    IF COM-FINALST-OVRD-IND = 'Y'                              * 00387700
      *        PERFORM 0240-WRITE-ADDR-OVRD-TABLE.                    * 00387800
      * --------------------------------------------------------------* 00387900
                                                                        00388000
                                                                        00388100
      *    IF   COMM-CUST-TYPE = 'CA'                                   00388200
R01873*      OR COMM-CUST-TYPE = 'LB'                                   00388300
      *        PERFORM  0225-UPDATE-IOMOD THRU 0225-EXIT.               00388400
                                                                        00388500
MANJU *    IF COM-DISP-IND = 'N'                                        00388600
      *        PERFORM 0270-REASSEMBLE-NAME-CHANGE.                     00388700
                                                                        00388800
       0220-EXIT.                                                       00388900
           EXIT.                                                        00389000
                                                                        00389100
      *---------------------------------------------------------------* 00389200
      * THIS CODE WAS ADDED TO FORM A BRIDGE BETWEEN THE CASENAME     * 00389300
      * TABLE AND THE VSAM CASE-MASTER FILE FOR THE PURPOSE OF        * 00389400
      * KEEPING BOTH NAME AND ADDRESSING AREAS IN SYNC.  GRB          * 00389500
      *---------------------------------------------------------------* 00389600
      *0225-UPDATE-IOMOD.                                               00389700
      *                                                                 00389800
900600*    EXEC SQL SELECT CASE_NUM                                     00389900
900600*        INTO :CASEX-CASE-NUM                                     00390000
900600*        FROM CASEXV01                                            00390100
11735A*        FROM CASE_MASTER                                         00390200
900600*       WHERE CASENAME#IDNTITY  = :WS-CIM-NUMBER                  00390300
900600*    END-EXEC.                                                    00390400
      *                                                                 00390500
      *    IF  SQLCODE  = +0                                            00390600
      *        NEXT SENTENCE                                            00390700
      *    ELSE                                                         00390800
      *    IF SQLCODE = 100 AND COMM-PREVIOUS-TRANID NOT = 'NAU0'       00390900
      *        MOVE 'AM004'  TO WS-MESSAGE-NUMBER1                      00391000
      *        GO TO 0310-INITIATE-ROUTER                               00391100
      *    ELSE                                                         00391200
      *        MOVE 'AM004'       TO WS-MESSAGE-NUMBER1                 00391300
      *        MOVE 'Y'           TO WS-RECORD-NOTFND                   00391400
      *        PERFORM 0380-BUMP-ERROR-MESSAGES                         00391500
      *        GO TO 0225-EXIT.                                         00391600
      *                                                                 00391700
900600*    MOVE CASEX-CASE-NUM    TO CASE-CASE-NUM.                     00391800
      *    IF  CASE-CASE-NUM = '000000'                                 00391900
      *        GO TO 0225-EXIT.                                         00392000
      *                                                                 00392100
      *    MOVE CASE-CASE-NUM     TO IO-KEY.                            00392200
      *    MOVE 'CASE'            TO IO-FILE-NAME.                      00392300
      *    MOVE 'U'               TO IO-FUNCTION.                       00392400
      *                                                                 00392500
      *    EXEC CICS LINK PROGRAM ('IOMOD')                             00392600
      *                   COMMAREA (IO-COMM-AREA)                       00392700
      *                   LENGTH (50)                                   00392800
      *                   END-EXEC.                                     00392900
      *                                                                 00393000
      *    IF  IO-RETURN   = ' ' OR 'U'                                 00393100
      *        MOVE IO-ADDR            TO FWAC-ADDR                     00393200
      *        SET ADDRESS OF I-O-AREA TO FWAC-PNTR                     00393300
      *        MOVE I-O-CASE-REC       TO CASE-RECORD                   00393400
      *    ELSE                                                         00393500
      *    IF  COMM-PREVIOUS-TRANID NOT = 'NAU0'                        00393600
      *        MOVE 'BL021'  TO WS-MESSAGE-NUMBER1                      00393700
      *        MOVE  100     TO SQLCODE                                 00393800
      *        GO TO 0310-INITIATE-ROUTER                               00393900
      *    ELSE                                                         00394000
      *        MOVE 'BL021'           TO WS-MESSAGE-NUMBER1             00394100
      *        MOVE 'Y'               TO WS-RECORD-NOTFND               00394200
      *        PERFORM 0380-BUMP-ERROR-MESSAGES                         00394300
      *        GO TO 0225-EXIT.                                         00394400
      *---------------------------------------------------------------* 00394500
      *    LINK TO IOMOD TO GENERICALLY LOCK ALL RECORDS              * 00394600
      *---------------------------------------------------------------* 00394700
COBOLU**   MOVE 'C-A-F-E '             TO IO-FILE-NAME.                 00394800
COBOLU**   MOVE CASE-CASE-NUM          TO IO-KEY.                       00394900
COBOLU**   MOVE 'G'                    TO IO-FUNCTION.                  00395000
                                                                        00395100
COBOLU*    EXEC CICS LINK PROGRAM('IOMOD')                              00395200
COBOLU*                   COMMAREA(IO-COMM-AREA)                        00395300
COBOLU*                   LENGTH(50)                                    00395400
COBOLU*                   END-EXEC.                                     00395500
                                                                        00395600
COBOLU**   EXEC CICS LINK PROGRAM('IOMOD2K')                            00395700
COBOLU**                  COMMAREA(IO-COMM-AREA)                        00395800
COBOLU**                  LENGTH(58)                                    00395900
COBOLU**                  END-EXEC.                                     00396000
                                                                        00396100
COBOLU**   IF IO-GOOD                                                   00396200
COBOLU**       NEXT SENTENCE                                            00396300
COBOLU**   ELSE                                                         00396400
COBOLU**   IF IO-OUT-FOR-UPDATE                                         00396500
COBOLU**       MOVE 'BL021'  TO WS-MESSAGE-NUMBER1                      00396600
COBOLU**       MOVE 'Y'      TO WS-RECORD-NOTFND                        00396700
COBOLU**       PERFORM 0380-BUMP-ERROR-MESSAGES                         00396800
COBOLU**       GO TO 0225-EXIT                                          00396900
COBOLU**   ELSE                                                         00397000
COBOLU**   IF COMM-PREVIOUS-TRANID NOT = 'NAU0'                         00397100
COBOLU**       MOVE 'BL021'  TO WS-MESSAGE-NUMBER1                      00397200
COBOLU**       MOVE  100     TO SQLCODE                                 00397300
COBOLU**       GO TO 0310-INITIATE-ROUTER                               00397400
COBOLU**   ELSE                                                         00397500
COBOLU**       MOVE 'BL021'  TO WS-MESSAGE-NUMBER1                      00397600
COBOLU**       MOVE 'Y'      TO WS-RECORD-NOTFND                        00397700
COBOLU**       PERFORM 0380-BUMP-ERROR-MESSAGES                         00397800
COBOLU**       GO TO 0225-EXIT.                                         00397900
      *                                                                 00398000
      *    EXEC CICS READ                                               00398100
      *              DATASET ('CASE')                                   00398200
      *              INTO    (CASE-RECORD)                              00398300
      *              RIDFLD  (CASE-CASE-NUM)                            00398400
990721*              LENGTH  (LENGTH OF CASE-RECORD)                    00398500
      *              UPDATE                                             00398600
      *              RESP    (ERROR-STATUS)                             00398700
      *    END-EXEC.                                                    00398800
      *                                                                 00398900
      *    IF  ERROR-STATUS = DFHRESP(NORMAL)                           00399000
      *        NEXT SENTENCE                                            00399100
      *    ELSE                                                         00399200
040154*    IF  ERROR-STATUS = DFHRESP(NOTFND)                           00399300
      *        MOVE 'BL010' TO  WS-MESSAGE-NUMBER1                      00399400
      *        PERFORM 0380-BUMP-ERROR-MESSAGES                         00399500
      *        GO TO 0225-EXIT                                          00399600
      *    ELSE                                                         00399700
      *        PERFORM 0415-UNLOCK-IOMOD                                00399800
040154*        EXEC CICS SYNCPOINT ROLLBACK END-EXEC                    00399900
      *        GO TO 9999-CICS-ERROR.                                   00400000
      *                                                                 00400100
      *    MOVE SPACES TO AUDIT-COMM-AREA.                              00400200
870486*    MOVE 'NA340'           TO IS020CA-PROGRAM-NAME.              00400300
870486*    MOVE 'NA340M1'         TO IS020CA-MAP-NAME.                  00400400
870486*    MOVE CASE-CASE-NUM     TO IS020CA-CASE-NUMBER.               00400500
870486*    MOVE ZERO              TO IS020CA-EMPLOYEE-NUMBER.           00400600
870486*    MOVE CASE-HLTH-CARRIER TO IS020CA-CARRIER.                   00400700
870486*    MOVE 'CM'              TO IS020CA-FILE-PREFIX.               00400800
870486*    EXEC CICS ASKTIME END-EXEC.                                  00400900
870486*    MOVE EIBTIME           TO IS020CA-TIME-CASE-UPDATED.         00401000
870486*    MOVE CASE-RECORD       TO IS020CA-PREVIOUS-RECORD            00401100
      *                              AC-PREV-RECORD.                    00401200
      *                                                                 00401300
      *    MOVE COMPANY-NAME       TO CASE-CASE-NAME.                   00401400
      *                                                                 00401500
R04209* DONT MOVE A BLANK CASE NAME TO VSAM                             00401600
R04209* AMERICAN GENERAL DID NOT STORE COMPANY-NAME ON THE LEVEL 4/5    00401700
R04209* SO WHEN BLANK, USE DISPLAY-NAME                                 00401800
      *                                                                 00401900
R04209*    IF CASE-CASE-NAME = SPACES                                   00402000
R04209*       MOVE FUNCTION UPPER-CASE (COM-DISPLAY-NAME)               00402100
R04209*                              TO CASE-CASE-NAME                  00402200
R04209*    END-IF.                                                      00402300
      *                                                                 00402400
      *    MOVE ADDRESS1           TO CASE-ADDRESS-1.                   00402500
      *    MOVE ADDRESS2           TO CASE-ADDRESS-2.                   00402600
      *    MOVE CITY               TO CASE-CITY-AND-STATE.              00402700
890989*    MOVE CASE-STATE-CODE    TO WS-HOLD-CASE-STATE.               00402800
      *    MOVE STATE              TO CASE-STATE-CODE.                  00402900
890989*    MOVE CASE-ZIP-CODE      TO WS-HOLD-CASE-ZIP.                 00403000
      *    MOVE ZIP                TO CASE-ZIP-CODE.                    00403100
890989*    MOVE CASE-ZIP-PLUS-4    TO WS-HOLD-CASE-ZIP-PLUS4.           00403200
      *    MOVE ZIP-PLUS4          TO CASE-ZIP-PLUS-4.                  00403300
      *    MOVE AREA-CODE          TO WS-CASE-AREA.                     00403400
      *    MOVE PHONE              TO WS-CASE-PHONE.                    00403500
      *    MOVE WS-CASE-AREA-PHONE TO CASE-PHONE-NUM.                   00403600
870486*    MOVE CASE-RECORD        TO IS020CA-CURRENT-RECORD            00403700
      *                               AC-CURR-RECORD.                   00403800
      *                                                                 00403900
      *    MOVE CASE-CASE-NUM     TO IO-KEY.                            00404000
      *    MOVE CASE-RECORD       TO I-O-CASE-REC.                      00404100
      *    MOVE 'CASE'            TO IO-FILE-NAME.                      00404200
      *    MOVE 'R'               TO IO-FUNCTION.                       00404300
      *                                                                 00404400
      *    EXEC CICS LINK PROGRAM ('IOMOD')                             00404500
      *                   COMMAREA (IO-COMM-AREA)                       00404600
      *                   LENGTH (50)                                   00404700
      *                   END-EXEC.                                     00404800
      *                                                                 00404900
      *    EXEC CICS                                                    00405000
      *         REWRITE                                                 00405100
      *         DATASET('CASE')                                         00405200
      *         FROM(CASE-RECORD)                                       00405300
990721*         LENGTH(LENGTH OF CASE-RECORD)                           00405400
      *         RESP(ERROR-STATUS)                                      00405500
      *         END-EXEC.                                               00405600
      *                                                                 00405700
      *    IF  ERROR-STATUS = DFHRESP(NORMAL)                           00405800
      *        NEXT SENTENCE                                            00405900
      *    ELSE                                                         00406000
      *    IF  ERROR-STATUS = DFHRESP(NOTFND)                           00406100
      *        MOVE 'BL020' TO  WS-MESSAGE-NUMBER1                      00406200
      *        MOVE 'Y'     TO  WS-RECORD-NOTFND                        00406300
      *        PERFORM 0380-BUMP-ERROR-MESSAGES                         00406400
      *    ELSE                                                         00406500
      *    IF  COMM-PREVIOUS-TRANID NOT = 'NAU0'                        00406600
      *        MOVE 'BL021'  TO WS-MESSAGE-NUMBER1                      00406700
      *        MOVE  100     TO SQLCODE                                 00406800
      *        GO TO 0310-INITIATE-ROUTER                               00406900
      *    ELSE                                                         00407000
      *        PERFORM 0415-UNLOCK-IOMOD                                00407100
      *        GO TO 9999-CICS-ERROR.                                   00407200
      *                                                                 00407300
      *    PERFORM 0335-PROCESS-IS020-CALL.                             00407400
      *                                                                 00407500
990115*    EXEC CICS                                                    00407600
990115*         SYNCPOINT                                               00407700
990115*    END-EXEC.                                                    00407800
      *                                                                 00407900
980714*    PERFORM 0336-ADD-BILL-REQUEST THRU 0336-EXIT.                00408000
                                                                        00408100
      *0225-EXIT.                                                       00408200
      *    EXIT.                                                        00408300
      *---------------------------------------------------------------* 00408400
      * END OF ADDED CODE.    GRB                                     * 00408500
      *---------------------------------------------------------------* 00408600
                                                                        00408700
       0230-MOVE-COMMAREA-TO-TABLE.                                     00408800
                                                                        00408900
           MOVE COM-CIM                TO IDNTITY.                      00409000
           MOVE COM-LAST-NAME          TO LAST-NAME.                    00409100
           MOVE COM-FIRST-NAME         TO FIRST-NAME.                   00409200
           MOVE COM-MIDDLE-NAME        TO MIDDLE-NAME.                  00409300
           MOVE COM-PREFIX             TO PREFIX.                       00409400
           MOVE COM-SUFFIX1            TO SUFFIX1.                      00409500
           MOVE COM-SUFFIX2            TO SUFFIX2.                      00409600
           MOVE COM-COMPANY-IND        TO COMPANY-IND.                  00409700
           MOVE COM-COMPANY-IN-ADDRESS TO COMPANY-IN-ADDRESS.           00409800
           MOVE COM-COMPANY-NAME       TO COMPANY-NAME.                 00409900
           MOVE COM-DISPLAY-NAME       TO DISPLAY-NAME.                 00410000
           MOVE COM-NICKNAME           TO NICKNAME.                     00410100
           MOVE COM-ADDRESS1           TO ADDRESS1.                     00410200
           MOVE COM-ADDRESS2           TO ADDRESS2.                     00410300
           MOVE COM-CITY               TO CITY.                         00410400
           MOVE COM-STATE              TO STATE.                        00410500
           MOVE COM-ZIP                TO ZIP.                          00410600
           MOVE COM-ZIP-PLUS4          TO ZIP-PLUS4.                    00410700
           MOVE COM-COUNTY-CODE        TO COUNTY-CODE.                  00410800
           MOVE COM-AREA-CODE          TO AREA-CODE.                    00410900
           MOVE COM-PHONE              TO PHONE.                        00411000
           MOVE COM-PHONE-EXTENSION    TO PHONE-EXTENSION.              00411100
900837     MOVE COM-FAX-AREA-CODE      TO FAX-AREA-CODE.                00411200
900837     MOVE COM-FAX-PHONE          TO FAX-PHONE.                    00411300
           MOVE COM-SSN                TO SSN-NUM.                      00411400
           MOVE COM-SEX                TO SEX.                          00411500
           MOVE COM-FINALST-REAS-CODE  TO FINALST-REAS-CODE.            00411600
           MOVE COM-FINALST-OVRD-IND   TO FINALST-OVRD-IND.             00411700
           MOVE COM-DUP-ADDR-OVRD-IND  TO DUP-ADDR-OVRD-IND.            00411800
R04023** ISSUE DATE IS ONLY UPDATED FOR AIG, SHOULD NOT CHANGE          00411900
R04023** OTHER CLIENTS ISSUE STATE                                      00412000
R04023     IF COM-ISSUE-STATE NOT = SPACES                              00412100
R04023        MOVE COM-ISSUE-STATE        TO ORIGINAL-STATE.            00412200
R00700     MOVE COM-EMAIL1             TO EMAIL1.                       00412300
R02539     MOVE COM-POSTAL-CODE        TO POSTAL-CODE.                  00412400
R02539     MOVE COM-COUNTRY-CODE       TO COUNTRY-CODE.                 00412500
           DISPLAY ' 230 PARA POSTAL-CODE' POSTAL-CODE.                 00412600
           DISPLAY '230 PARA COUNTRY-CODE' COUNTRY-CODE.                00412700
900526*    EXEC CICS ASSIGN TCTUALENG(TCTUAL) END-EXEC.                 00412800
900526*    EXEC CICS ADDRESS TCTUA(ADDRESS OF TCTUAR) END-EXEC.         00412900
900526     MOVE TCTUA-DEMO-DATA TO WS-DEMOGRAPHIC-COMM-AREA.            00413000
                                                                        00413100
MANJU1     MOVE 'WIPRO123' TO  WS-DEMO-LOGONID                          00413200
MANJU1     MOVE '01'       TO  WS-DEMO-SITE-CODE                        00413300
           MOVE WS-DEMO-LOGONID        TO COM-CHANGE-LOGON              00413400
                                          CHANGE-LOGON.                 00413500
           MOVE WS-DEMO-SITE-CODE      TO COM-SITE-CODE                 00413600
                                          SITE-CODE.                    00413700
                                                                        00413800
           MOVE COM-RECORD-STATUS      TO RECORD-STATUS.                00413900
           MOVE COM-ALT-ADDRESS-IND    TO ALT-ADDRESS-IND.              00414000
           MOVE COM-FUTURE-ADDRESS-IND TO FUTURE-ADDRESS-IND.           00414100
           MOVE COM-RECORD-ORIGIN      TO RECORD-ORIGIN.                00414200
           MOVE COM-COMBINED-STATUS    TO COMBINED-STATUS.              00414300
           MOVE COM-NAME-KEY1          TO NAME-KEY1.                    00414400
           MOVE COM-NAME-KEY2          TO NAME-KEY2.                    00414500
           MOVE COM-NAME-KEY3          TO NAME-KEY3.                    00414600
           MOVE COM-ADDRESS-KEY1       TO ADDRESS-KEY1.                 00414700
           MOVE COM-ASSOCIATION1       TO ASSOCIATION1.                 00414800
           MOVE COM-ASSOCIATION2       TO ASSOCIATION2.                 00414900
           MOVE COM-ASSOCIATION3       TO ASSOCIATION3.                 00415000
           MOVE COM-EFFECTIVE-DATE     TO WS-DATE-R.                    00415100
           MOVE ZERO                   TO IND-EFFECTIVE-DATE.           00415200
           MOVE WS-DATE-R              TO WS-DATE.                      00415300
Y2KIMR*                                                                 00415400
Y2KIMR* IMR CHANGE IMR4 BEGIN                                           00415500
Y2KIMR*                                                                 00415600
Y2KIMR*    MOVE '19'                   TO EF-CC.                        00415700
Y2KIMR*                                                                 00415800
Y2KIMR     MOVE WS-YY-A TO Y2K-WK-DATE1R-X-YY                           00415900
Y2KIMR     MOVE WS-MM-A TO Y2K-WK-DATE1R-X-MM                           00416000
Y2KIMR     MOVE WS-DD-A TO Y2K-WK-DATE1R-X-DD                           00416100
Y2KIMR                                                                  00416200
Y2KIMR     IF Y2K-WK-DATE1R-X-YY LESS THAN Y2K-WK-CUTOFF-YR-X           00416300
Y2KIMR        MOVE '20' TO Y2K-WK-DATE1-X-CC                            00416400
Y2KIMR     ELSE                                                         00416500
Y2KIMR        MOVE '19' TO Y2K-WK-DATE1-X-CC                            00416600
Y2KIMR     END-IF                                                       00416700
Y2KIMR                                                                  00416800
Y2KIMR     MOVE Y2K-WK-DATE1-X-CC TO EF-CC                              00416900
Y2KIMR*                                                                 00417000
Y2KIMR* IMR CHANGE IMR4 END                                             00417100
Y2KIMR*                                                                 00417200
           MOVE WS-YY-A                TO EF-YY.                        00417300
           MOVE WS-MM-A                TO EF-MM.                        00417400
           MOVE WS-DD-A                TO EF-DD.                        00417500
           MOVE WS-EFFECTIVE-DATE      TO EFFECTIVE-DATE.               00417600
           IF COMM-CUST-TYPE NOT = 'BR' AND COM-ENTITY-TYPE = 'BR'      00417700
                MOVE 'ML' TO ENTITY-TYPE                                00417800
           ELSE                                                         00417900
              MOVE COM-ENTITY-TYPE        TO ENTITY-TYPE.               00418000
                                                                        00418100
           IF COM-BIRTH-DATE NOT =  ZEROS                               00418200
Y2KIMR*                                                                 00418300
Y2KIMR* IMR CHANGE IMR5 BEGIN                                           00418400
Y2KIMR*                                                                 00418500
Y2KIMR*        MOVE COM-BIRTH-DATE TO WS-DATE-R                         00418600
Y2KIMR*        MOVE WS-DATE-R      TO WS-DATE                           00418700
Y2KIMR*        MOVE '19'           TO BR-CC                             00418800
Y2KIMR*        MOVE WS-YY-A        TO BR-YY                             00418900
Y2KIMR*        MOVE WS-MM-A        TO BR-MM                             00419000
Y2KIMR*        MOVE WS-DD-A        TO BR-DD                             00419100
Y2KIMR*                                                                 00419200
Y2KIMR         MOVE COM-BIRTH-DATE TO Y2K-WK-VARIABLE2                  00419300
Y2KIMR         MOVE Y2K-WK-VARIABLE2-CC TO BR-CC                        00419400
Y2KIMR         MOVE Y2K-WK-VARIABLE2-YY TO BR-YY                        00419500
Y2KIMR         MOVE Y2K-WK-VARIABLE2-MM TO BR-MM                        00419600
Y2KIMR         MOVE Y2K-WK-VARIABLE2-DD TO BR-DD                        00419700
Y2KIMR*                                                                 00419800
Y2KIMR* IMR CHANGE IMR5 END                                             00419900
Y2KIMR         DISPLAY 'WS-BIRTH-DATE' WS-BIRTH-DATE                    00420000
               MOVE WS-BIRTH-DATE  TO BIRTH-DATE                        00420100
               MOVE ZERO           TO IND-BIRTH-DATE                    00420200
           ELSE                                                         00420300
               MOVE -1 TO IND-BIRTH-DATE.                               00420400
                                                                        00420500
      * --------------------------------------------------------------* 00420600
      *0240-WRITE-ADDR-OVRD-TABLE.                                    * 00420700
      * --------------------------------------------------------------* 00420800
                                                                        00420900
                                                                        00421000
       0250-PROCESS-DB2-REQUESTS.                                       00421100
                                                                        00421200
           DISPLAY 'WS-CIM-NUMBER'         WS-CIM-NUMBER.               00421300
           EXEC SQL                                                     00421400
               OPEN AGNTCUR                                             00421500
           END-EXEC.                                                    00421600
                                                                        00421700
           IF SQLCODE NOT = ZERO                                        00421800
               GO TO 0420-DB2-ERROR.                                    00421900
                                                                        00422000
           EXEC SQL FETCH AGNTCUR                                       00422100
R01142         INTO :DCLAGNTNAME:AGNTNAME-INDICATORS                    00422200
           END-EXEC.                                                    00422300
           DISPLAY 'AGNT TABLE SQLCODE ' SQLCODE                        00422400
           IF SQLCODE = 100                                             00422500
               EXEC SQL OPEN CASECUR END-EXEC                           00422600
               PERFORM 0140-SQL-ERROR-CHECK                             00422700
               MOVE 'Y' TO CASE-FETCH-SW WSQ-CASE-SW                    00422800
KPL            EXEC SQL FETCH CASECUR                                   00422900
R00700*            INTO DCLAGNTNAME:AGNTNAME-INDICATORS                 00423000
R00700             INTO  :IDNTITY, :LAST-NAME :IND-LAST-NAME,           00423100
C23311                   :FIRST-NAME :IND-FIRST-NAME,                   00423200
C23311                   :MIDDLE-NAME :IND-MIDDLE-NAME,                 00423300
R00700                   :PREFIX :IND-PREFIX, :SUFFIX1 :IND-SUFFIX1,    00423400
R00700                   :SUFFIX2 :IND-SUFFIX2,                         00423500
R00700                   :COMPANY-IND, :COMPANY-IN-ADDRESS,             00423600
C23311                   :COMPANY-NAME :IND-COMPANY-NAME,               00423700
C23311                   :DISPLAY-NAME :IND-DISPLAY-NAME,               00423800
R00700                   :NICKNAME :IND-NICKNAME,                       00423900
C23311                   :ADDRESS1, :ADDRESS2 :IND-ADDRESS2,            00424000
R00700                   :CITY, :STATE, :ZIP, :ZIP-PLUS4,               00424100
R00700                   :COUNTY-CODE, :AREA-CODE,                      00424200
R00700                   :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,      00424300
R00700                   :BIRTH-DATE :IND-BIRTH-DATE,                   00424400
R00700                   :FINALST-REAS-CODE, :FINALST-OVRD-IND,         00424500
R00700                   :DUP-ADDR-OVRD-IND,                            00424600
R00700                   :EFFECTIVE-DATE, :CHANGE-DATE,                 00424700
R00700                   :CHANGE-LOGON, :ENTITY-TYPE,                   00424800
R00700                   :RECORD-STATUS, :ALT-ADDRESS-IND,              00424900
R00700                   :FUTURE-ADDRESS-IND,                           00425000
R00700                   :RECORD-ORIGIN, :COMBINED-STATUS,              00425100
R00700                   :SITE-CODE, :NAME-KEY1,                        00425200
R00700                   :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,         00425300
R00700                   :ASSOCIATION1 :IND-ASSOCIATION1,               00425400
R00700                   :ASSOCIATION2 :IND-ASSOCIATION2,               00425500
R00700                   :ASSOCIATION3 :IND-ASSOCIATION3,               00425600
R00700                   :FAX-AREA-CODE, :FAX-PHONE,                    00425700
R04023                   :ORIGINAL-STATE,                               00425800
R00700                   :EMAIL1 :IND-EMAIL1,                           00425900
R02539                   :COUNTRY-CODE :IND-COUNTRY-CODE,               00426000
R02539                   :POSTAL-CODE :IND-POSTAL-CODE                  00426100
               END-EXEC.                                                00426200
           DISPLAY 'CASE SQLCODE         ' SQLCODE                      00426300
           DISPLAY 'WS-CIM-NUMBER'         WS-CIM-NUMBER                00426400
           DISPLAY 'COMM-CUST-TYPE'        COMM-CUST-TYPE               00426500
           IF  SQLCODE = 100 AND COMM-PREVIOUS-TRANID NOT = 'NAU0'      00426600
               MOVE 'NA020'  TO WS-MESSAGE-NUMBER1                      00426700
               GO TO 0310-INITIATE-ROUTER                               00426800
           ELSE                                                         00426900
           IF  SQLCODE = 100                                            00427000
               MOVE 'NA020'           TO WS-MESSAGE-NUMBER1             00427100
               MOVE 'Y'               TO WS-RECORD-NOTFND               00427200
               PERFORM 0380-BUMP-ERROR-MESSAGES                         00427300
           ELSE                                                         00427400
           IF  SQLCODE NOT = ZERO                                       00427500
               GO TO 0420-DB2-ERROR                                     00427600
920403     ELSE                                                         00427700
920403     IF ENTITY-TYPE NOT = 'CA' AND 'AM' AND 'LB' AND 'BA'         00427800
960530                              AND 'LD' AND 'ML'                   00427900
MANJU                               AND 'RP' AND 'BR' AND 'CAM'         00428000
920403        MOVE +100    TO  SQLCODE                                  00428100
920403        MOVE 'NA189'  TO WS-MESSAGE-NUMBER1                       00428200
920403        IF COMM-PREVIOUS-TRANID NOT = 'NAU0'                      00428300
920403            GO TO 0310-INITIATE-ROUTER                            00428400
920403        ELSE                                                      00428500
920403            MOVE 'Y'    TO WS-RECORD-NOTFND                       00428600
920403            PERFORM 0380-BUMP-ERROR-MESSAGES.                     00428700
                                                                        00428800
           IF IND-LAST-NAME IS NEGATIVE                                 00428900
               MOVE SPACES TO LAST-NAME.                                00429000
           IF IND-FIRST-NAME IS NEGATIVE                                00429100
               MOVE SPACES TO FIRST-NAME.                               00429200
           IF IND-MIDDLE-NAME IS NEGATIVE                               00429300
               MOVE SPACES TO MIDDLE-NAME.                              00429400
           IF IND-PREFIX IS NEGATIVE                                    00429500
               MOVE SPACES TO PREFIX.                                   00429600
           IF IND-SUFFIX1 IS NEGATIVE                                   00429700
               MOVE SPACES TO SUFFIX1.                                  00429800
           IF IND-SUFFIX2 IS NEGATIVE                                   00429900
               MOVE SPACES TO SUFFIX2.                                  00430000
           IF IND-COMPANY-NAME IS NEGATIVE                              00430100
               MOVE SPACES TO COMPANY-NAME.                             00430200
           IF IND-BIRTH-DATE IS NEGATIVE                                00430300
               MOVE ZEROES TO BIRTH-DATE.                               00430400
           IF IND-DISPLAY-NAME IS NEGATIVE                              00430500
               MOVE SPACES TO DISPLAY-NAME.                             00430600
           IF IND-NICKNAME IS NEGATIVE                                  00430700
               MOVE SPACES TO NICKNAME.                                 00430800
           IF IND-ADDRESS2 IS NEGATIVE                                  00430900
               MOVE SPACES TO ADDRESS2.                                 00431000
           IF IND-ASSOCIATION1 IS NEGATIVE                              00431100
               MOVE SPACES TO ASSOCIATION1.                             00431200
           IF IND-ASSOCIATION2 IS NEGATIVE                              00431300
               MOVE SPACES TO ASSOCIATION2.                             00431400
           IF IND-ASSOCIATION3 IS NEGATIVE                              00431500
               MOVE SPACES TO ASSOCIATION3.                             00431600
           IF IND-FAX-PHONE IS NEGATIVE                                 00431700
               MOVE SPACES TO FAX-PHONE.                                00431800
           IF IND-EMAIL1 IS NEGATIVE                                    00431900
               MOVE SPACES TO EMAIL1.                                   00432000
R02539     IF IND-POSTAL-CODE IS NEGATIVE                               00432100
R02539         MOVE SPACES TO POSTAL-CODE.                              00432200
R02539     IF IND-COUNTRY-CODE IS NEGATIVE                              00432300
R02539         MOVE SPACES TO COUNTRY-CODE.                             00432400
                                                                        00432500
           EXEC SQL                                                     00432600
               CLOSE AGNTCUR                                            00432700
           END-EXEC.                                                    00432800
                                                                        00432900
           IF CASE-FETCH-SW = 'Y'                                       00433000
               EXEC SQL CLOSE CASECUR END-EXEC.                         00433100
                                                                        00433200
      *    IF  COMM-CUST-TYPE = 'CA'                                    00433300
      *        PERFORM  0255-LOCK-IOMOD.                                00433400
                                                                        00433500
      *0255-LOCK-IOMOD.                                                 00433600
      *                                                                 00433700
900600*    EXEC SQL SELECT CASE_NUM                                     00433800
900600*        INTO :CASEX-CASE-NUM                                     00433900
900600*        FROM CASEXV01                                            00434000
11735A*        FROM CASE_MASTER                                         00434100
900600*       WHERE CASENAME#IDNTITY  = :WS-CIM-NUMBER                  00434200
900600*    END-EXEC.                                                    00434300
      *                                                                 00434400
      *    IF SQLCODE = 100 AND COMM-PREVIOUS-TRANID NOT = 'NAU0'       00434500
      *        MOVE 'AM004'  TO WS-MESSAGE-NUMBER1                      00434600
      *        GO TO 0310-INITIATE-ROUTER                               00434700
      *    ELSE                                                         00434800
      *       IF SQLCODE = 100                                          00434900
      *           MOVE 'AM004'    TO WS-MESSAGE-NUMBER1                 00435000
      *           MOVE 'Y'        TO WS-RECORD-NOTFND                   00435100
      *           PERFORM 0380-BUMP-ERROR-MESSAGES                      00435200
      *           GO TO 0255-EXIT                                       00435300
      *       ELSE                                                      00435400
      *          IF SQLCODE NOT = ZERO                                  00435500
      *              GO TO 0420-DB2-ERROR.                              00435600
      *                                                                 00435700
900600*    MOVE CASEX-CASE-NUM    TO CASE-CASE-NUM.                     00435800
      *    IF  CASE-CASE-NUM = '000000'                                 00435900
      *        GO TO 0255-EXIT.                                         00436000
      *                                                                 00436100
      *    MOVE CASE-CASE-NUM     TO IO-KEY.                            00436200
      *    MOVE 'CASE'            TO IO-FILE-NAME.                      00436300
      *    MOVE 'U'               TO IO-FUNCTION.                       00436400
      *                                                                 00436500
COBOLU*    EXEC CICS LINK PROGRAM ('IOMOD')                             00436600
COBOLU*                   COMMAREA (IO-COMM-AREA)                       00436700
COBOLU*                   LENGTH (50)                                   00436800
COBOLU*                   END-EXEC.                                     00436900
      *                                                                 00437000
COBOLU*    EXEC CICS LINK PROGRAM ('IOMOD2K')                           00437100
COBOLU*                   COMMAREA (IO-COMM-AREA)                       00437200
COBOLU*                   LENGTH (58)                                   00437300
COBOLU*                   END-EXEC.                                     00437400
                                                                        00437500
      *    IF IO-GOOD                                                   00437600
      *        MOVE IO-ADDR            TO FWAC-ADDR                     00437700
      *        SET ADDRESS OF I-O-AREA TO FWAC-PNTR                     00437800
      *        MOVE I-O-CASE-REC       TO CASE-RECORD                   00437900
      *    ELSE                                                         00438000
      *    IF  COMM-PREVIOUS-TRANID NOT = 'NAU0'                        00438100
      *        MOVE 'BL021'  TO WS-MESSAGE-NUMBER1                      00438200
      *        MOVE  100     TO SQLCODE                                 00438300
      *        GO TO 0310-INITIATE-ROUTER                               00438400
      *    ELSE                                                         00438500
      *        MOVE 'BL021'           TO WS-MESSAGE-NUMBER1             00438600
      *        MOVE 'Y'               TO WS-RECORD-NOTFND               00438700
      *        PERFORM 0380-BUMP-ERROR-MESSAGES.                        00438800
                                                                        00438900
      *0255-EXIT.                                                       00439000
      *    EXIT.                                                        00439100
                                                                        00439200
       0260-COMPLETE-BROKER-ADD.                                        00439300
                                                                        00439400
           MOVE '00'              TO COMM-NEXT-FUNCTION.                00439500
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT               00439600
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.              00439700
                                                                        00439800
           MOVE SPACES             TO COMM-MSG-ID(1) COMM-MSG-ID(2)     00439900
                                      COMM-MSG-ID(3) COMM-MSG-ID(4).    00440000
           MOVE 'NAU0'             TO COMM-PREVIOUS-TRANID.             00440100
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00440200
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00440300
                                                                        00440400
      * --------------------------------------------------------------* 00440500
      *    ADD FUTURE ADD TYPE COMMAREA'S AT THIS LOCATION            * 00440600
      *    INCLUDING CASES AND MAGIC'S                                * 00440700
      * --------------------------------------------------------------* 00440800
                                                                        00440900
           IF COM-ENTITY-TYPE = 'BR'                                    00441000
               MOVE 'BR'              TO COMM-SYSTEM-CODE               00441100
               MOVE 'A_____'          TO COMM-ACTION-CODE               00441200
               MOVE IDNTITY               TO COMM-IDNTITY               00441300
               MOVE 'NA042'           TO COMM-MSG-ID(1)                 00441400
               MOVE COM-DISPLAY-NAME  TO COMM-MAIL-LINE(1)              00441500
               MOVE COM-ADDRESS1      TO COMM-MAIL-LINE(2)              00441600
               MOVE COM-CITY          TO COMM-MAIL-LINE(3)              00441700
               MOVE COM-STATE         TO WS-STATE                       00441800
               MOVE COM-ZIP           TO WS-ZIP                         00441900
               MOVE WS-STATE-ZIP      TO COMM-MAIL-LINE(4)              00442000
               MOVE COM-AREA-CODE     TO COMM-CUST-PHONE-AREA           00442100
               MOVE COM-PHONE         TO WS-PHONE                       00442200
               MOVE WS-PHONE3         TO COMM-CUST-PHONE-EXCH           00442300
               MOVE WS-PHONE4         TO COMM-CUST-PHONE-NUMB           00442400
               MOVE COM-RECORD-STATUS TO COMM-CUST-STATUS               00442500
               MOVE COM-ENTITY-TYPE   TO COMM-CUST-TYPE                 00442600
               MOVE NA-COMMAREA       TO WS-AGENT-COMMAREA              00442700
               MOVE SPACES            TO WS-BROKER-FIELDS               00442800
               MOVE COM-ENTITY-TYPE   TO WS-SYSTEM-ADD-TYPE.            00442900
                                                                        00443000
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00443100
                               FROM   (WS-BROKER-COMMAREA)              00443200
                               LENGTH (WS-BROKER-LENGTH)                00443300
                               ITEM   (WS-TS-ITEM)                      00443400
                               RESP   (WS-CICS-RESP)                    00443500
                               REWRITE                                  00443600
                               MAIN                                     00443700
                               END-EXEC.                                00443800
                                                                        00443900
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00444000
                GO TO 9999-CICS-ERROR.                                  00444100
                                                                        00444200
MANJU *    EXEC CICS SEND MAP ('SYSBUSY')                               00444300
      *                   RESP (WS-CICS-RESP)                           00444400
      *                   END-EXEC.                                     00444500
                                                                        00444600
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00444700
      *         GO TO 9999-CICS-ERROR.                                  00444800
                                                                        00444900
      *   EXEC CICS START TRANSID('BRA0')                               00445000
      *                   TERMID (EIBTRMID)                             00445100
      *                   RESP   (WS-CICS-RESP)                         00445200
      *                    END-EXEC.                                    00445300
                                                                        00445400
      *   IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                         00445500
      *        GO TO 9999-CICS-ERROR.                                   00445600
                                                                        00445700
           EXEC CICS RETURN                                             00445800
                     END-EXEC.                                          00445900
                                                                        00446000
       0270-REASSEMBLE-NAME-CHANGE.                                     00446100
                                                                        00446200
           MOVE '00' TO COMM-NEXT-FUNCTION.                             00446300
           MOVE ZERO TO COMM-CURSOR-POSN     COMM-MSG-COUNT             00446400
                        COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.     00446500
                                                                        00446600
           MOVE SPACES             TO COMM-MSG-ID(1) COMM-MSG-ID(2)     00446700
                                      COMM-MSG-ID(3) COMM-MSG-ID(4).    00446800
           MOVE 'NAU0'             TO COMM-PREVIOUS-TRANID.             00446900
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00447000
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00447100
                                                                        00447200
      * --------------------------------------------------------------* 00447300
      *    ADD FUTURE ADD TYPE COMMAREA'S AT THIS LOCATION            * 00447400
      *    INCLUDING CASES AND MAGIC'S                                * 00447500
      * --------------------------------------------------------------* 00447600
                                                                        00447700
           IF COM-DISP-IND    = 'N'                                     00447800
               MOVE 'NA'              TO COMM-SYSTEM-CODE               00447900
               MOVE 'T_____'          TO COMM-ACTION-CODE               00448000
               MOVE IDNTITY              TO COMM-IDNTITY                00448100
               MOVE COM-DISPLAY-NAME  TO COMM-MAIL-LINE(1)              00448200
               MOVE COM-ADDRESS1      TO COMM-MAIL-LINE(2)              00448300
               MOVE COM-CITY          TO COMM-MAIL-LINE(3)              00448400
               MOVE COM-STATE         TO WS-STATE                       00448500
               MOVE COM-ZIP           TO WS-ZIP                         00448600
               MOVE WS-STATE-ZIP      TO COMM-MAIL-LINE(4)              00448700
               MOVE COM-AREA-CODE     TO COMM-CUST-PHONE-AREA           00448800
               MOVE COM-PHONE         TO WS-PHONE                       00448900
               MOVE WS-PHONE3         TO COMM-CUST-PHONE-EXCH           00449000
               MOVE WS-PHONE4         TO COMM-CUST-PHONE-NUMB           00449100
               MOVE COM-RECORD-STATUS TO COMM-CUST-STATUS               00449200
               MOVE COM-ENTITY-TYPE   TO COMM-CUST-TYPE                 00449300
                                                                        00449400
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00449500
                               FROM   (NA-COMMAREA)                     00449600
                               LENGTH (WS-COMM-LENGTH)                  00449700
                               ITEM   (WS-TS-ITEM)                      00449800
                               RESP   (WS-CICS-RESP)                    00449900
                               REWRITE                                  00450000
                               MAIN                                     00450100
                               END-EXEC.                                00450200
                                                                        00450300
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00450400
                GO TO 9999-CICS-ERROR.                                  00450500
                                                                        00450600
MANJU *    EXEC CICS SEND MAP ('SYSBUSY')                               00450700
      *                  RESP (WS-CICS-RESP)                            00450800
      *                  END-EXEC.                                      00450900
                                                                        00451000
      *   IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                         00451100
      *        GO TO 9999-CICS-ERROR.                                   00451200
                                                                        00451300
      *   EXEC CICS START TRANSID('NAT0')                               00451400
      *                   TERMID (EIBTRMID)                             00451500
      *                   RESP   (WS-CICS-RESP)                         00451600
      *                   END-EXEC.                                     00451700
                                                                        00451800
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00451900
                GO TO 9999-CICS-ERROR.                                  00452000
                                                                        00452100
           EXEC CICS RETURN                                             00452200
                     END-EXEC.                                          00452300
                                                                        00452400
       0280-MOVE-SPACES-TO-MAP.                                         00452500
           DISPLAY '0280-MOVE-SPACES-TO-MAP PARA'                       00452600
R00700     MOVE EMAIL1             TO MAP-EMAIL1O      WSQ-EMAIL1.      00452700
           MOVE IDNTITY                TO MAP-CIMO         WSQ-CIM.     00452800
           MOVE LAST-NAME          TO MAP-LAST-NAMEO   WSQ-LAST-NAME.   00452900
T3384      MOVE PREFIX             TO MAP-PREFIXO      WSQ-PREFIX.      00453000
           MOVE FIRST-NAME         TO MAP-FIRST-NAMEO  WSQ-FIRST-NAME.  00453100
           MOVE MIDDLE-NAME        TO MAP-MIDDLE-NAMEO WSQ-MIDDLE-NAME. 00453200
           MOVE SUFFIX1            TO MAP-SUFFIX1O     WSQ-SUFFIX1.     00453300
           MOVE SUFFIX2            TO MAP-SUFFIX2O     WSQ-SUFFIX2.     00453400
           MOVE COMPANY-IN-ADDRESS TO MAP-COMP-IN-ADDO                  00453500
                                      WSQ-COMPANY-IN-ADDRESS.           00453600
           MOVE COMPANY-NAME       TO MAP-COMP-NAMEO   WSQ-COMPANY-NAME.00453700
           MOVE DISPLAY-NAME       TO MAP-DIS-NAMEO    WSQ-DISPLAY-NAME.00453800
           MOVE NICKNAME           TO MAP-NICKNAMEO    WSQ-NICKNAME.    00453900
           MOVE ADDRESS1           TO MAP-ADDRESS1O    WSQ-ADDRESS1.    00454000
           MOVE ADDRESS2           TO MAP-ADDRESS2O    WSQ-ADDRESS2.    00454100
           MOVE CITY               TO MAP-CITYO        WSQ-CITY.        00454200
           MOVE STATE              TO MAP-STATEO       WSQ-STATE.       00454300
           MOVE ZIP                TO MAP-ZIPO         WSQ-ZIP.         00454400
           MOVE ZIP-PLUS4          TO MAP-ZIP-PLUS4O   WSQ-ZIP-PLUS4.   00454500
R02539     MOVE COUNTRY-CODE       TO MAP-COUNTRY-CDO WSQ-COUNTRY-CODE. 00454600
R02539     MOVE POSTAL-CODE        TO MAP-POSTAL-CDO   WSQ-POSTAL-CODE. 00454700
           MOVE AREA-CODE          TO MAP-AREA-CODEO   WSQ-AREA-CODE.   00454800
           MOVE PHONE              TO WS-PHONE         WSQ-PHONE.       00454900
           MOVE WS-PHONE3          TO MAP-PHONE3O.                      00455000
           MOVE WS-PHONE4          TO MAP-PHONE4O.                      00455100
           MOVE PHONE-EXTENSION    TO MAP-PHONE-EXTO                    00455200
                                      WSQ-PHONE-EXTENSION.              00455300
           DISPLAY 'MAP-COUNTRY-CDO' MAP-COUNTRY-CDO.                   00455400
           DISPLAY 'MAP-POSTAL-CDO' MAP-POSTAL-CDO.                     00455500
900837     MOVE FAX-AREA-CODE      TO MAP-FAX-AREA-CDO                  00455600
900837                                WSQ-FAX-AREA-CODE.                00455700
900837     MOVE FAX-PHONE          TO WS-PHONE         WSQ-FAX-PHONE.   00455800
900837     MOVE WS-PHONE3          TO MAP-FAX-PHONE3O.                  00455900
900837     MOVE WS-PHONE4          TO MAP-FAX-PHONE4O.                  00456000
R04023     MOVE ORIGINAL-STATE     TO MAP-ISSUE-STATEO                  00456100
R04023                                WSQ-ISSUE-STATE.                  00456200
           MOVE SSN-NUM            TO MAP-SSNO         WSQ-SSN.         00456300
           MOVE SEX                TO MAP-SEXO         WSQ-SEX.         00456400
           MOVE 'Y'                TO MAP-DIS-CHG-INDO WSQ-DISP-IND.    00456500
           MOVE FINALST-REAS-CODE  TO MAP-FINALST-CDO                   00456600
                                      WSQ-FINALST-REAS-CODE.            00456700
           MOVE FINALST-OVRD-IND  TO  MAP-ADDR-OVRIDEO                  00456800
                                      WSQ-FINALST-OVRD-IND.             00456900
           MOVE CHANGE-LOGON       TO WSQ-CHANGE-LOGON.                 00457000
MANJU1     MOVE CHANGE-LOGON       TO MAP-LOGONO.                       00457100
MANJU *    IF  CHANGE-LOGON NOT EQUAL SPACES AND                        00457200
      *        CHANGE-LOGON NOT = LOW-VALUES                            00457300
      *            MOVE CHANGE-LOGON TO WR-EMP-PSINUM                   00457400
      *            EXEC CICS READ DATASET('EMPLID')                     00457500
      *                           INTO   (WR-EMPLOYEE-RECORD)           00457600
      *                           RIDFLD (WR-EMP-PSINUM)                00457700
990721*                           LENGTH (LENGTH OF WR-EMPLOYEE-RECORD) 00457800
      *                           RESP   (WS-CICS-RESP)                 00457900
      *                           END-EXEC                              00458000
      *            MOVE WR-EMP-NAME TO MAP-LOGONO                       00458100
001220** HANDLE DUP LOGONID- JUST DISPLAY LOGONID INSTEAD OF CICS ERROR 00458200
001220*            IF WS-CICS-RESP = DFHRESP(DUPKEY)                    00458300
001220*               MOVE WR-EMP-PSINUM TO MAP-LOGONO                  00458400
001220*            ELSE                                                 00458500
      *            IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND            00458600
      *                WS-CICS-RESP NOT = DFHRESP(NOTFND)               00458700
      *                    GO TO 9999-CICS-ERROR.                       00458800
                                                                        00458900
           MOVE RECORD-STATUS      TO WSQ-RECORD-STATUS.                00459000
           MOVE RECORD-STATUS      TO WT-C2006-STATUS-CD.               00459100
           MOVE WT-CNTL2006        TO TURBO-CNTL-AREA.                  00459200
           PERFORM 10000-CALL-GSF.                                      00459300
           MOVE TURBO-CNTL-AREA    TO WT-CNTL2006.                      00459400
      *    MOVE ZERO TO WT-C2006-RETURN                                 00459500
           IF  WT-C2006-RETURN EQUAL ZEROES                             00459600
               MOVE WT-C2006-STAT-DESC TO MAP-CUST-STATUSO              00459700
           ELSE                                                         00459800
               MOVE RECORD-STATUS  TO MAP-CUST-STATUSO.                 00459900
MANJU      MOVE RECORD-STATUS  TO MAP-CUST-STATUSO.                     00460000
                                                                        00460100
           MOVE ALT-ADDRESS-IND    TO MAP-ALT-ADDRO                     00460200
                                      WSQ-ALT-ADDRESS-IND.              00460300
           MOVE FUTURE-ADDRESS-IND TO MAP-FUTURE-ADDRO                  00460400
                                      WSQ-FUTURE-ADDRESS-IND.           00460500
           MOVE SITE-CODE          TO MAP-SITE-CODEO   WSQ-SITE-CODE.   00460600
           MOVE SITE-CODE          TO WT-C0161-SITE.                    00460700
           MOVE WT-CNTL0161        TO TURBO-CNTL-AREA.                  00460800
           PERFORM 10000-CALL-GSF.                                      00460900
           MOVE TURBO-CNTL-AREA    TO WT-CNTL0161.                      00461000
           IF  WT-C0161-RETURN EQUAL ZEROES                             00461100
               MOVE WT-C0161-COMPANY-NAME TO MAP-SITE-DESCO             00461200
           ELSE                                                         00461300
               MOVE SPACES         TO MAP-SITE-DESCO.                   00461400
                                                                        00461500
GRB        MOVE COMBINED-STATUS    TO WSQ-COMBINED-STATUS.              00461600
GRB        MOVE ADDRESS-KEY1       TO WSQ-ADDRESS-KEY1.                 00461700
                                                                        00461800
           MOVE ASSOCIATION1       TO MAP-ASSOC1O      WSQ-ASSOCIATION1.00461900
           MOVE ASSOCIATION2       TO MAP-ASSOC2O      WSQ-ASSOCIATION2.00462000
           MOVE ASSOCIATION3       TO MAP-ASSOC3O      WSQ-ASSOCIATION3.00462100
SPC   ***  IF ASSOCIATION1 = 'YERKE'                                    00462200
SPC   ***    AND ASSOCIATION2 = 'NBRL'                                  00462300
SPC   ***          MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A                 00462400
SPC   ***                                   MAP-ASSOC2A.                00462500
LMB   ***                                   MAP-ASSOC3A.                00462600
                                                                        00462700
920124***  IF  ASSOCIATION1 = 'FREDM'                                   00462800
920124***  AND ASSOCIATION2 = SPACES                                    00462900
920124***  AND ASSOCIATION3 = SPACES                                    00463000
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A                     00463100
920124***                               MAP-ASSOC2A                     00463200
920124***                               MAP-ASSOC3A.                    00463300
920124***  IF  ASSOCIATION1 = 'FREDM'                                   00463400
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A.                    00463500
920124***  IF  ASSOCIATION2 = 'FREDM'                                   00463600
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC2A.                    00463700
920124***  IF  ASSOCIATION3 = 'FREDM'                                   00463800
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC3A.                    00463900
                                                                        00464000
           MOVE ENTITY-TYPE        TO WSQ-ENTITY-TYPE.                  00464100
           MOVE ENTITY-TYPE        TO WT-C8997-SYSTEM-CD.               00464200
           MOVE WT-CNTL8997        TO TURBO-CNTL-AREA.                  00464300
           PERFORM 10000-CALL-GSF.                                      00464400
           MOVE TURBO-CNTL-AREA    TO WT-CNTL8997.                      00464500
           IF  WT-C8997-RETURN EQUAL ZEROES                             00464600
               MOVE WT-C8997-SYS-TYPE  TO MAP-ENTITY-TYPEO              00464700
           ELSE                                                         00464800
               MOVE ENTITY-TYPE        TO MAP-ENTITY-TYPEO.             00464900
           MOVE ENTITY-TYPE        TO MAP-ENTITY-TYPEO.                 00465000
                                                                        00465100
           MOVE BIRTH-DATE         TO WS-DB2-DATE.                      00465200
Y2KIMR*                                                                 00465300
Y2KIMR* IMR CHANGE IMR7  BEGIN                                          00465400
Y2KIMR*                                                                 00465500
Y2KIMR*    MOVE WS-DB2-DD-R        TO WS-DAY.                           00465600
Y2KIMR*    MOVE WS-DB2-MM-R        TO WS-MONTH.                         00465700
Y2KIMR*    MOVE WS-DB2-YY-R        TO WS-YEAR.                          00465800
Y2KIMR*    MOVE '/'                TO SLASH-1 SLASH-2.                  00465900
Y2KIMR*    MOVE WS-DATE-EIGHT      TO MAP-BIRTH-DATEO.                  00466000
Y2KIMR*                                                                 00466100
Y2KIMR     IF    WS-DB2-DATE  EQUAL  ZEROS                              00466200
Y2KIMR        MOVE ZEROS    TO  Y2K-WK-DATE-TEN                         00466300
Y2KIMR                          MAP-BIRTH-DATEO                         00466400
Y2KIMR     ELSE                                                         00466500
Y2KIMR      MOVE WS-DB2-MM-R         TO Y2K-WK-MONTH                    00466600
Y2KIMR      MOVE WS-DB2-DD-R         TO Y2K-WK-DAY                      00466700
Y2KIMR      MOVE WS-DB2-CC-R         TO Y2K-WK-CENT                     00466800
Y2KIMR      MOVE WS-DB2-YY-R         TO Y2K-WK-YEAR                     00466900
Y2KIMR      MOVE '-'                 TO Y2K-WK-SLASH-1 Y2K-WK-SLASH-2   00467000
Y2KIMR      MOVE Y2K-WK-DATE-TEN TO  MAP-BIRTH-DATEO .                  00467100
Y2KIMR*                                                                 00467200
Y2KIMR* IMR CHANGE IMR7 END                                             00467300
Y2KIMR*                                                                 00467400
                                                                        00467500
                                                                        00467600
Y2KIMR*                                                                 00467700
Y2KIMR* IMR CHANGE IMR7A BEGIN                                          00467800
Y2KIMR*                                                                 00467900
Y2KIMR*    MOVE BR-CC              TO WS-YY-A.                          00468000
Y2KIMR*    MOVE BR-YY              TO WS-YY-A.                          00468100
Y2KIMR*    MOVE BR-MM              TO WS-MM-A.                          00468200
Y2KIMR*    MOVE BR-DD              TO WS-DD-A.                          00468300
Y2KIMR*    MOVE WS-DATE            TO WS-DATE-R.                        00468400
Y2KIMR*    MOVE WS-DATE-R          TO WSQ-BIRTH-DATE.                   00468500
Y2KIMR*                                                                 00468600
Y2KIMR     MOVE BIRTH-DATE         TO WS-BIRTH-DATE.                    00468700
Y2KIMR     MOVE BR-CC              TO Y2K-WK-VARIABLE2-CC.              00468800
Y2KIMR     MOVE BR-YY              TO Y2K-WK-VARIABLE2-YY.              00468900
Y2KIMR     MOVE BR-MM              TO Y2K-WK-VARIABLE2-MM.              00469000
Y2KIMR     MOVE BR-DD              TO Y2K-WK-VARIABLE2-DD.              00469100
Y2KIMR     MOVE Y2K-WK-VARIABLE2   TO WSQ-BIRTH-DATE.                   00469200
Y2KIMR*                                                                 00469300
Y2KIMR* IMR CHANGE IMR7A END                                            00469400
Y2KIMR*                                                                 00469500
                                                                        00469600
           MOVE EFFECTIVE-DATE     TO WS-DB2-DATE.                      00469700
           MOVE WS-DB2-DD-R        TO WS-DAY.                           00469800
           MOVE WS-DB2-MM-R        TO WS-MONTH.                         00469900
           MOVE WS-DB2-YY-R        TO WS-YEAR.                          00470000
           MOVE '/'                TO SLASH-1 SLASH-2.                  00470100
           MOVE WS-DATE-EIGHT      TO MAP-EFF-DATEO.                    00470200
                                                                        00470300
           MOVE EFFECTIVE-DATE     TO WS-EFFECTIVE-DATE                 00470400
           MOVE EF-YY              TO WS-YY-A.                          00470500
           MOVE EF-MM              TO WS-MM-A.                          00470600
           MOVE EF-DD              TO WS-DD-A.                          00470700
           MOVE WS-DATE            TO WS-DATE-R.                        00470800
           MOVE WS-DATE-R          TO WSQ-EFFECTIVE-DATE.               00470900
                                                                        00471000
           MOVE CHANGE-DATE        TO WS-DB2-TIMESTAMP.                 00471100
           MOVE WS-TS-DD-R         TO WS-DAY.                           00471200
           MOVE WS-TS-MM-R         TO WS-MONTH.                         00471300
           MOVE WS-TS-YY-R         TO WS-YEAR.                          00471400
           MOVE '/'                TO SLASH-1 SLASH-2.                  00471500
           MOVE WS-DATE-EIGHT      TO MAP-CHANGE-DATEO.                 00471600
                                                                        00471700
           MOVE CHANGE-DATE        TO WS-CHANGE-DATE.                   00471800
           MOVE CO-YY              TO WS-YY-A.                          00471900
           MOVE CO-MM              TO WS-MM-A.                          00472000
           MOVE CO-DD              TO WS-DD-A.                          00472100
           MOVE WS-DATE            TO WS-DATE-R.                        00472200
           MOVE WS-DATE-R          TO WSQ-CHANGE-DATE.                  00472300
                                                                        00472400
MANJU *    EXEC SQL SELECT CARRIER_CODE,                                00472500
R04023*                    PREV_CARRIER                                 00472600
R04023*    INTO :CASEX-CARRIER-CODE,                                    00472700
R04023*         :CASEX-PREV-CARRIER                                     00472800
R04023*    FROM CASEXV01                                                00472900
11735A*    FROM CASE_MASTER                                             00473000
R04023*    WHERE CASENAME#IDNTITY  = :COMM-IDNTITY                      00473100
R04023*    END-EXEC.                                                    00473200
MANJU      MOVE SPACES TO CASEX-CARRIER-CODE CASEX-PREV-CARRIER.        00473300
R04023     MOVE CASEX-CARRIER-CODE  TO WSQ-CARRIER-CODE.                00473400
R04023     MOVE CASEX-PREV-CARRIER  TO WSQ-PREV-CARRIER.                00473500
R04023**************************************************                00473600
R04023* DISPLAY ISSUE STATE ONLY FOR AIG                                00473700
R04342* DISPLAY ISSUE STATE ONLY FOR AIG AND ACM                        00473800
R04023**************************************************                00473900
R04023     IF (CASEX-CARRIER-CODE  = '9D'                               00474000
R04023        AND                                                       00474100
R04023*       CASEX-PREV-CARRIER   = 'SH')                              00474200
R04254*       CASEX-PREV-CARRIER   = ('SH' OR 'SV'))                    00474300
R04281*       CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN'))            00474400
R04280        CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN' OR 'SP'      00474500
R04209                                     OR 'JJ'                      00474600
R04832                                     OR 'JA'                      00474700
R04472                                     OR 'IG'))                    00474800
R04023        OR                                                        00474900
R04023*       (CASEX-CARRIER-CODE  = 'SH')                              00475000
R04254*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV')                      00475100
R04281*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN')              00475200
R04280        (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN' OR 'SP'       00475300
R04472*                                   OR 'IG'                       00475400
R04342                                    OR 'IG' OR 'MV' OR 'MB'       00475500
R04209             OR 'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'              00475600
R04934             OR 'NO' OR 'NU' OR 'NX'                              00475700
R04209             OR 'LD' OR 'LI' OR 'MI'                              00475800
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'      00475900
R04209             OR 'NQ' OR 'NS'                                      00476000
R04209             OR 'UD' OR 'UT'                                      00476100
R04209             OR 'VI' OR 'WF'                                      00476200
R04832** NEW CIGNA CARRIERS                                             00476300
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'      00476400
R04832             OR 'JH'                                              00476500
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'      00476600
R04832             )                                                    00476700
R04023         MOVE ATTRB-PROT-ASKIP-DRK                                00476800
R04023                     TO MAP-LITERAL8A    MAP-LITERAL6A            00476900
R04023                        MAP-LITERAL5A    MAP-SUFFIX1A             00477000
R04023                        MAP-SUFFIX2A     MAP-NICKNAMEA            00477100
R04023                        MAP-LITERAL4A    MAP-SEXA                 00477200
R04023         MOVE SPACES TO MAP-LITERAL8O    MAP-LITERAL6O            00477300
R04023                        MAP-LITERAL5O    MAP-SUFFIX1O             00477400
R04023                        MAP-SUFFIX2O     MAP-NICKNAMEO            00477500
R04023                        MAP-LITERAL4O    MAP-SEXO                 00477600
R04023     ELSE                                                         00477700
960530*    IF (ENTITY-TYPE = 'AM' OR 'CA' OR 'LD')                      00477800
960530     IF (ENTITY-TYPE = 'AM' OR 'LD')                              00477900
900837         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA         00478000
               MOVE ATTRB-PROT-ASKIP-DRK                                00478100
                           TO MAP-LITERAL1A    MAP-LITERAL2A            00478200
                              MAP-LITERAL3A    MAP-LITERAL4A            00478300
                              MAP-LITERAL5A    MAP-LITERAL6A            00478400
                              MAP-LITERAL7A    MAP-LITERAL8A            00478500
                              MAP-LITERAL9A    MAP-LITERAL10A           00478600
                              MAP-FIRST-NAMEA  MAP-MIDDLE-NAMEA         00478700
                              MAP-LAST-NAMEA   MAP-SUFFIX1A             00478800
                              MAP-SUFFIX2A     MAP-NICKNAMEA            00478900
                              MAP-SEXA         MAP-BIRTH-DATEA          00479000
                              MAP-COMP-IN-ADDA                          00479100
R04023                        MAP-ISSUE-STATEA                          00479200
R04023                        MAP-ISS-LITERALA                          00479300
R04023                        MAP-ST-LITERALA                           00479400
               MOVE SPACES TO MAP-FIRST-NAMEO  MAP-MIDDLE-NAMEO         00479500
                              MAP-LAST-NAMEO   MAP-SUFFIX1O             00479600
                              MAP-SUFFIX2O     MAP-NICKNAMEO            00479700
                              MAP-SEXO         MAP-BIRTH-DATEO          00479800
                              MAP-COMP-IN-ADDO                          00479900
R04023                        MAP-ISSUE-STATEO                          00480000
                              WSQ-FIRST-NAME   WSQ-MIDDLE-NAME          00480100
                              WSQ-LAST-NAME    WSQ-SUFFIX1              00480200
                              WSQ-SUFFIX2      WSQ-NICKNAME             00480300
R04023                        WSQ-ISSUE-STATE                           00480400
                              WSQ-SEX                                   00480500
R04023     ELSE                                                         00480600
R04023          MOVE ATTRB-PROT-ASKIP-DRK TO                            00480700
R04023                        MAP-ISSUE-STATEA                          00480800
R04023                        MAP-ISS-LITERALA                          00480900
R04023                        MAP-ST-LITERALA                           00481000
R04023          MOVE SPACES TO WSQ-ISSUE-STATE                          00481100
R04023                         MAP-ISSUE-STATEO.                        00481200
                                                                        00481300
       0290-MOVE-OUT-MAP-FIELDS.                                        00481400
           DISPLAY '290 PARA FIELDS' COM-SEX                            00481500
MANJU1     MOVE 'WIPRO123'             TO MAP-LOGONO.                   00481600
           MOVE COM-FIRST-NAME         TO MAP-FIRST-NAMEO.              00481700
           MOVE COM-MIDDLE-NAME        TO MAP-MIDDLE-NAMEO.             00481800
           MOVE COM-LAST-NAME          TO MAP-LAST-NAMEO.               00481900
T3384      MOVE COM-PREFIX             TO MAP-PREFIXO.                  00482000
           MOVE COM-SUFFIX1            TO MAP-SUFFIX1O.                 00482100
           MOVE COM-SUFFIX2            TO MAP-SUFFIX2O.                 00482200
           MOVE COM-NICKNAME           TO MAP-NICKNAMEO.                00482300
           MOVE COM-COMPANY-NAME       TO MAP-COMP-NAMEO.               00482400
           MOVE COM-DISPLAY-NAME       TO MAP-DIS-NAMEO.                00482500
           MOVE COM-ADDRESS1           TO MAP-ADDRESS1O.                00482600
           MOVE COM-SEX                TO MAP-SEXO.                     00482700
           MOVE COM-ADDRESS2           TO MAP-ADDRESS2O.                00482800
           MOVE COM-CITY               TO MAP-CITYO.                    00482900
           MOVE COM-COMPANY-IN-ADDRESS TO MAP-COMP-IN-ADDO.             00483000
           MOVE COM-STATE              TO MAP-STATEO.                   00483100
           MOVE COM-ZIP                TO MAP-ZIPO.                     00483200
           MOVE COM-ZIP-PLUS4          TO MAP-ZIP-PLUS4O.               00483300
           MOVE COM-FINALST-OVRD-IND   TO MAP-ADDR-OVRIDEO.             00483400
           MOVE COM-AREA-CODE          TO MAP-AREA-CODEO.               00483500
           MOVE COM-PHONE              TO WS-PHONE.                     00483600
           MOVE WS-PHONE3              TO MAP-PHONE3O.                  00483700
           MOVE WS-PHONE4              TO MAP-PHONE4O.                  00483800
           MOVE COM-SSN                TO MAP-SSNO.                     00483900
           MOVE COM-PHONE-EXTENSION    TO MAP-PHONE-EXTO.               00484000
900837     MOVE COM-FAX-AREA-CODE      TO MAP-FAX-AREA-CDO.             00484100
900837     MOVE COM-FAX-PHONE          TO WS-PHONE.                     00484200
900837     MOVE WS-PHONE3              TO MAP-FAX-PHONE3O.              00484300
900837     MOVE WS-PHONE4              TO MAP-FAX-PHONE4O.              00484400
R04023     MOVE COM-ISSUE-STATE        TO MAP-ISSUE-STATEO.             00484500
R9031A     MOVE COM-EMAIL1             TO MAP-EMAIL1O.                  00484600
           MOVE COM-ALT-ADDRESS-IND    TO MAP-ALT-ADDRO.                00484700
MANJU2     DISPLAY '290 PARA COM-ENTITY-TYPE' COM-ENTITY-TYPE.          00484800
           MOVE COM-ENTITY-TYPE        TO MAP-ENTITY-TYPEO.             00484900
           MOVE COM-DISP-IND           TO MAP-DIS-CHG-INDO.             00485000
           MOVE COM-FUTURE-ADDRESS-IND TO MAP-FUTURE-ADDRO.             00485100
           MOVE COM-RECORD-STATUS      TO MAP-CUST-STATUSO.             00485200
           MOVE COM-SITE-CODE          TO MAP-SITE-CODEO.               00485300
           MOVE COM-ASSOCIATION1       TO MAP-ASSOC1O.                  00485400
           MOVE COM-ASSOCIATION2       TO MAP-ASSOC2O.                  00485500
           MOVE COM-ASSOCIATION3       TO MAP-ASSOC3O.                  00485600
R02539     MOVE COM-POSTAL-CODE        TO MAP-POSTAL-CDO.               00485700
R02539     MOVE COM-COUNTRY-CODE       TO MAP-COUNTRY-CDO.              00485800
            DISPLAY ' 29O PARA MAP-POSTAL-CDO' MAP-POSTAL-CDO.          00485900
            DISPLAY ' 29O PARA MAP-COUNTRY-CDO' MAP-COUNTRY-CDO.        00486000
SPC   ***  IF COM-ASSOCIATION1 = 'YERKE'                                00486100
SPC   ***    AND COM-ASSOCIATION2 = 'NBRL'                              00486200
SPC   ***          MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A                 00486300
SPC   ***                                   MAP-ASSOC2A.                00486400
LMB   ***                                   MAP-ASSOC3A.                00486500
                                                                        00486600
920124***  IF  COM-ASSOCIATION1 = 'FREDM'                               00486700
920124***  AND COM-ASSOCIATION2 = SPACES                                00486800
920124***  AND COM-ASSOCIATION3 = SPACES                                00486900
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A                     00487000
920124***                               MAP-ASSOC2A                     00487100
920124***                               MAP-ASSOC3A.                    00487200
920124***  IF  COM-ASSOCIATION1 = 'FREDM'                               00487300
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A.                    00487400
920124***  IF  COM-ASSOCIATION2 = 'FREDM'                               00487500
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC2A.                    00487600
920124***  IF  COM-ASSOCIATION3 = 'FREDM'                               00487700
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC3A.                    00487800
                                                                        00487900
961037     IF  ENTITY-TYPE = 'RP'                                       00488000
961037         MOVE ATTRB-PROT-ASKIP TO MAP-ENTITY-TYPEA                00488100
961037     END-IF.                                                      00488200
                                                                        00488300
           MOVE COM-CIM                TO MAP-CIMO.                     00488400
           MOVE COM-FINALST-REAS-CODE  TO MAP-FINALST-CDO.              00488500
      *    MOVE WS-DEMO-USER-NAME      TO MAP-LOGONO.                   00488600
      *    IF  COM-CHANGE-LOGON NOT EQUAL SPACES AND                    00488700
      *        COM-CHANGE-LOGON NOT = LOW-VALUES                        00488800
      *            MOVE COM-CHANGE-LOGON TO WR-EMP-PSINUM               00488900
      *            EXEC CICS READ DATASET('EMPLID')                     00489000
      *                           INTO   (WR-EMPLOYEE-RECORD)           00489100
      *                           RIDFLD (WR-EMP-PSINUM)                00489200
990721*                           LENGTH (LENGTH OF WR-EMPLOYEE-RECORD) 00489300
      *                           RESP   (WS-CICS-RESP)                 00489400
      *                           END-EXEC                              00489500
      *            MOVE WR-EMP-NAME TO MAP-LOGONO                       00489600
001220** HANDLE DUP LOGONID- JUST DISPLAY LOGONID INSTEAD OF CICS ERROR 00489700
001220*            IF WS-CICS-RESP = DFHRESP(DUPKEY)                    00489800
001220*               MOVE WR-EMP-PSINUM TO MAP-LOGONO                  00489900
001220*            ELSE                                                 00490000
      *            IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND            00490100
      *                WS-CICS-RESP NOT = DFHRESP(NOTFND)               00490200
      *                    GO TO 9999-CICS-ERROR.                       00490300
                                                                        00490400
                                                                        00490500
           MOVE COM-RECORD-STATUS  TO WT-C2006-STATUS-CD.               00490600
           MOVE WT-CNTL2006        TO TURBO-CNTL-AREA.                  00490700
           PERFORM 10000-CALL-GSF.                                      00490800
           MOVE TURBO-CNTL-AREA    TO WT-CNTL2006.                      00490900
      *    IF  WT-C2006-RETURN EQUAL ZEROES                             00491000
      *        MOVE WT-C2006-STAT-DESC TO MAP-CUST-STATUSO              00491100
      *    ELSE                                                         00491200
               MOVE COM-RECORD-STATUS  TO MAP-CUST-STATUSO.             00491300
                                                                        00491400
           MOVE COM-SITE-CODE          TO WT-C0161-SITE.                00491500
           MOVE WT-CNTL0161            TO TURBO-CNTL-AREA.              00491600
           PERFORM 10000-CALL-GSF.                                      00491700
           MOVE TURBO-CNTL-AREA    TO WT-CNTL0161.                      00491800
           IF  WT-C0161-RETURN EQUAL ZEROES                             00491900
               MOVE WT-C0161-COMPANY-NAME TO MAP-SITE-DESCO             00492000
           ELSE                                                         00492100
               MOVE SPACES         TO MAP-SITE-DESCO.                   00492200
                                                                        00492300
           MOVE COM-ENTITY-TYPE    TO WT-C8997-SYSTEM-CD.               00492400
           MOVE WT-CNTL8997        TO TURBO-CNTL-AREA.                  00492500
           PERFORM 10000-CALL-GSF.                                      00492600
           MOVE TURBO-CNTL-AREA    TO WT-CNTL8997.                      00492700
MANJU3*    IF  WT-C8997-RETURN EQUAL ZEROES                             00492800
      *        MOVE WT-C8997-SYS-TYPE  TO MAP-ENTITY-TYPEO              00492900
      *    ELSE                                                         00493000
               MOVE COM-ENTITY-TYPE        TO MAP-ENTITY-TYPEO.         00493100
                                                                        00493200
      *    IF DISPLAY-CHANGED-SW = 'Y'                                  00493300
      *        PERFORM 0300-FORMAT-DISPLAY-NAME.                        00493400
                                                                        00493500
Y2KIMR*                                                                 00493600
Y2KIMR* IMR CHANGE IMR8 BEGIN                                           00493700
Y2KIMR*                                                                 00493800
Y2KIMR*    MOVE COM-BIRTH-DATE TO WS-DATE-R.                            00493900
Y2KIMR*    MOVE WS-DATE-R      TO WS-DATE.                              00494000
Y2KIMR*    MOVE WS-MM          TO WS-MONTH.                             00494100
Y2KIMR*    MOVE WS-DD          TO WS-DAY.                               00494200
Y2KIMR*    MOVE WS-YY          TO WS-YEAR.                              00494300
Y2KIMR*    MOVE '/'            TO SLASH-1 SLASH-2.                      00494400
Y2KIMR*    MOVE WS-DATE-EIGHT  TO MAP-BIRTH-DATEO.                      00494500
Y2KIMR*                                                                 00494600
Y2KIMR      MOVE COM-BIRTH-DATE TO Y2K-WK-VARIABLE2.                    00494700
Y2KIMR      MOVE Y2K-WK-VARIABLE2-MM TO Y2K-WK-MONTH.                   00494800
Y2KIMR      MOVE Y2K-WK-VARIABLE2-DD TO Y2K-WK-DAY .                    00494900
Y2KIMR      MOVE Y2K-WK-VARIABLE2-CC TO Y2K-WK-CENT.                    00495000
Y2KIMR      MOVE Y2K-WK-VARIABLE2-YY TO Y2K-WK-YEAR .                   00495100
Y2KIMR      MOVE '-'                 TO Y2K-WK-SLASH-1 Y2K-WK-SLASH-2.  00495200
Y2KIMR      MOVE Y2K-WK-DATE-TEN TO MAP-BIRTH-DATEO.                    00495300
Y2KIMR*                                                                 00495400
Y2KIMR* IMR CHANGE IMR8 END                                             00495500
Y2KIMR*                                                                 00495600
                                                                        00495700
           MOVE COM-CHANGE-DATE TO WS-DATE-R                            00495800
           MOVE WS-DATE-R       TO WS-DATE.                             00495900
           MOVE WS-MM           TO WS-MONTH.                            00496000
           MOVE WS-DD           TO WS-DAY.                              00496100
           MOVE WS-YY           TO WS-YEAR.                             00496200
           MOVE '/'             TO SLASH-1 SLASH-2.                     00496300
           MOVE WS-DATE-EIGHT   TO MAP-CHANGE-DATEO.                    00496400
                                                                        00496500
           MOVE COM-EFFECTIVE-DATE TO WS-DATE-R.                        00496600
           MOVE WS-DATE-R          TO WS-DATE.                          00496700
           MOVE WS-MM              TO WS-MONTH.                         00496800
           MOVE WS-DD              TO WS-DAY.                           00496900
           MOVE WS-YY              TO WS-YEAR.                          00497000
           MOVE '/'                TO SLASH-1 SLASH-2.                  00497100
           MOVE WS-DATE-EIGHT      TO MAP-EFF-DATEO.                    00497200
                                                                        00497300
R04023*    EXEC SQL SELECT CARRIER_CODE,                                00497400
R04023*                    PREV_CARRIER                                 00497500
R04023*    INTO :CASEX-CARRIER-CODE,                                    00497600
R04023*         :CASEX-PREV-CARRIER                                     00497700
R04023*    FROM CASEXV01                                                00497800
11735A*    FROM CASE_MASTER                                             00497900
R04023*    WHERE CASENAME#IDNTITY= :COMM-IDNTITY                        00498000
R04023*    END-EXEC.                                                    00498100
MANJU      MOVE SPACES TO CASEX-CARRIER-CODE CASEX-PREV-CARRIER.        00498200
R04023     MOVE CASEX-CARRIER-CODE  TO WSQ-CARRIER-CODE.                00498300
R04023     MOVE CASEX-PREV-CARRIER  TO WSQ-PREV-CARRIER.                00498400
R04023**************************************************                00498500
R04023* DISPLAY ISSUE STATE ONLY FOR AIG                                00498600
R04023**************************************************                00498700
R04023     IF (CASEX-CARRIER-CODE  = '9D'                               00498800
R04023        AND                                                       00498900
R04023*       CASEX-PREV-CARRIER   = 'SH')                              00499000
R04254*       CASEX-PREV-CARRIER   = ('SH' OR 'SV'))                    00499100
R04281*       CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN'))            00499200
R04280        CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN' OR 'SP'      00499300
R04209                                     OR 'JJ'                      00499400
R04832                                     OR 'JA'                      00499500
R04472                                     OR 'IG'))                    00499600
R04023        OR                                                        00499700
R04023*       (CASEX-CARRIER-CODE  = 'SH')                              00499800
R04254*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV')                      00499900
R04281*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN')              00500000
R04280        (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN' OR 'SP'       00500100
R04472                                    OR 'IG'                       00500200
R04209             OR 'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'              00500300
R04934             OR 'NO' OR 'NU' OR 'NX'                              00500400
R04209             OR 'LD' OR 'LI' OR 'MI'                              00500500
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'      00500600
R04209             OR 'NQ' OR 'NS'                                      00500700
R04209             OR 'UD' OR 'UT'                                      00500800
R04209             OR 'VI' OR 'WF'                                      00500900
R04832** NEW CIGNA CARRIERS                                             00501000
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'      00501100
R04832             OR 'JH'                                              00501200
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'      00501300
R04832             )                                                    00501400
R04023         MOVE ATTRB-PROT-ASKIP-DRK                                00501500
R04023                     TO MAP-LITERAL8A    MAP-LITERAL6A            00501600
R04023                        MAP-LITERAL5A    MAP-SUFFIX1A             00501700
R04023                        MAP-SUFFIX2A     MAP-NICKNAMEA            00501800
R04023                        MAP-LITERAL4A    MAP-SEXA                 00501900
R04023         MOVE SPACES TO MAP-LITERAL8O    MAP-LITERAL6O            00502000
R04023                        MAP-LITERAL5O    MAP-SUFFIX1O             00502100
R04023                        MAP-SUFFIX2O     MAP-NICKNAMEO            00502200
R04023                        MAP-LITERAL4O    MAP-SEXO                 00502300
R04023     ELSE                                                         00502400
           IF COM-CASE-SW = 'Y'  AND                                    00502500
960530       (COM-ENTITY-TYPE = 'AM' OR 'LD')                           00502600
900837         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA         00502700
               MOVE ATTRB-PROT-ASKIP-DRK                                00502800
                           TO MAP-LITERAL1A    MAP-LITERAL2A            00502900
                              MAP-LITERAL3A    MAP-LITERAL4A            00503000
                              MAP-LITERAL5A    MAP-LITERAL6A            00503100
                              MAP-LITERAL7A    MAP-LITERAL8A            00503200
                              MAP-LITERAL9A    MAP-LITERAL10A           00503300
                              MAP-FIRST-NAMEA  MAP-MIDDLE-NAMEA         00503400
                              MAP-LAST-NAMEA   MAP-SUFFIX1A             00503500
                              MAP-SUFFIX2A     MAP-NICKNAMEA            00503600
                              MAP-SEXA         MAP-BIRTH-DATEA          00503700
                              MAP-COMP-IN-ADDA                          00503800
               MOVE SPACES TO MAP-FIRST-NAMEO  MAP-MIDDLE-NAMEO         00503900
                              MAP-LAST-NAMEO   MAP-SUFFIX1O             00504000
                              MAP-SUFFIX2O     MAP-NICKNAMEO            00504100
                              MAP-SEXO         MAP-BIRTH-DATEO          00504200
                              MAP-COMP-IN-ADDO                          00504300
R04023                        MAP-ISSUE-STATEO                          00504400
R04023     ELSE                                                         00504500
R04023         MOVE ATTRB-PROT-ASKIP-DRK TO                             00504600
R04023                        MAP-ISSUE-STATEA                          00504700
R04023                        MAP-ISS-LITERALA                          00504800
R04023                        MAP-ST-LITERALA                           00504900
R04023         MOVE SPACES TO MAP-ISSUE-STATEO.                         00505000
R04023                                                                  00505100
961037     IF COM-ENTITY-TYPE = 'RP'                                    00505200
961037         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA.        00505300
R03179     IF COM-ENTITY-TYPE = 'CA'                                    00505400
R03179         MOVE ATTRB-PROT-ASKIP        TO MAP-EMAIL1A MAP-EMAIL2A. 00505500
                                                                        00505600
       0300-FORMAT-DISPLAY-NAME.                                        00505700
                                                                        00505800
      * --------------------------------------------------------------* 00505900
      *    GET FORMATED VERSION OF NAME AND ADDRESS                   * 00506000
      * --------------------------------------------------------------* 00506100
           MOVE ZEROS                 TO NA-COMM-RECORD-NUMBER.         00506200
           MOVE COM-FIRST-NAME        TO NA-COMM-FIRST-NAME.            00506300
           MOVE COM-MIDDLE-NAME       TO NA-COMM-MIDDLE-NAME.           00506400
           MOVE COM-LAST-NAME         TO NA-COMM-LAST-NAME.             00506500
T3384      MOVE COM-PREFIX            TO NA-COMM-TITLE.                 00506600
           MOVE COM-SUFFIX1           TO NA-COMM-SUFFIX1.               00506700
           MOVE COM-SUFFIX2           TO NA-COMM-SUFFIX2.               00506800
           MOVE COM-COMPANY-NAME      TO NA-COMM-COMPANY-NAME.          00506900
           MOVE COM-COMPANY-IND       TO NA-COMM-COMPANY-MAIL.          00507000
           MOVE COM-COMPANY-NAME      TO NA-COMM-COMPANY-NAME.          00507100
           MOVE COM-ADDRESS1          TO NA-COMM-ADDRESS-1.             00507200
           MOVE COM-ADDRESS2          TO NA-COMM-ADDRESS-2.             00507300
           MOVE COM-CITY              TO NA-COMM-CITY.                  00507400
           MOVE COM-STATE             TO NA-COMM-STATE.                 00507500
           MOVE COM-ZIP               TO NA-COMM-ZIP.                   00507600
           MOVE COM-SEX               TO NA-COMM-SEX.                   00507700
           MOVE COM-ZIP-PLUS4         TO NA-COMM-ZIP-PLUS4.             00507800
           MOVE COM-ENTITY-TYPE       TO NA-COMM-SYSTEM-TYPE.           00507900
           MOVE COM-RECORD-STATUS     TO NA-COMM-RECORD-STATUS.         00508000
                                                                        00508100
           IF COM-AREA-CODE NUMERIC                                     00508200
               MOVE COM-AREA-CODE TO NA-COMM-PHONE-AREA                 00508300
           ELSE                                                         00508400
               MOVE ZEROES TO NA-COMM-PHONE-AREA.                       00508500
                                                                        00508600
           MOVE COM-PHONE TO WS-PHONE.                                  00508700
           IF WS-PHONE3 NUMERIC                                         00508800
               MOVE WS-PHONE3 TO NA-COMM-PHONE-3                        00508900
           ELSE                                                         00509000
               MOVE ZEROES TO NA-COMM-PHONE-3.                          00509100
                                                                        00509200
           IF WS-PHONE4 NUMERIC                                         00509300
               MOVE WS-PHONE4 TO NA-COMM-PHONE-4                        00509400
           ELSE                                                         00509500
               MOVE ZEROES TO NA-COMM-PHONE-4.                          00509600
                                                                        00509700
           MOVE 'Y'  TO NA-COMM-FORMAT-SWITCH.                          00509800
           MOVE 'Y'  TO NA-COMM-CASE-SWITCH.                            00509900
           MOVE 'FM' TO NA-COMM-REQUEST-CODE.                           00510000
           MOVE ZERO TO NA-COMM-RETURN-CODE.                            00510100
                                                                        00510200
R04209** IF AMERICAN GENERAL USERS CHANGE NAME ON TAS                   00510300
R04209** DON'T PASS THE SEX FOR FORMATTING THE DISPLAY NAME             00510400
R04209** SO THAT IT WON'T USE MR. MS.                                   00510500
R04209     IF (COM-ENTITY-TYPE = 'AM' OR 'CA' OR 'LB')                  00510600
R04209        IF (COM-CARRIER-CODE  = '9D'                              00510700
R04209            AND COM-PREV-CARRIER   = 'JJ'                         00510800
R04832                                 OR  'JA')                        00510900
R04209          OR (COM-CARRIER-CODE =                                  00511000
R04209                'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'              00511100
R04934             OR 'NO' OR 'NU' OR 'NX'                              00511200
R04209             OR 'LD' OR 'LI' OR 'MI'                              00511300
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'      00511400
R04209             OR 'NQ' OR 'NS'                                      00511500
R04209             OR 'UD' OR 'UT'                                      00511600
R04209             OR 'VI' OR 'WF'                                      00511700
R04832** NEW CIGNA CARRIERS                                             00511800
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'      00511900
R04832             OR 'JH'                                              00512000
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'      00512100
R04832             )                                                    00512200
R04209          MOVE SPACES TO NA-COMM-SEX                              00512300
R04209        END-IF                                                    00512400
R04209     END-IF.                                                      00512500
                                                                        00512600
      *    EXEC CICS LINK PROGRAM ('NA203')                             00512700
      *                   COMMAREA(NAME-AND-ADDRESS-COMMAREA)           00512800
990721**                  LENGTH  (NA-COMMAREA-LENGTH)                  00512900
990721*                   LENGTH  (LENGTH OF NAME-AND-ADDRESS-COMMAREA) 00513000
      *                   RESP    (WS-CICS-RESP)                        00513100
      *                   END-EXEC.                                     00513200
                                                                        00513300
      *    IF  WS-CICS-RESP NOT = DFHRESP(NORMAL)                       00513400
      *        GO TO 9999-CICS-ERROR.                                   00513500
                                                                        00513600
      *---------------------------------------------------------------* 00513700
      *    IF THERE WAS AN SQL ERROR OR MISC CICS ERROR IN NA202      * 00513800
      *    IT HAS ALREADY PLACED AN ERROR DIAGNOSTIC MAP ON THE       * 00513900
      *    TERMINAL, WE WANT TO STOP THIS PROGRAM HERE SO THAT        * 00514000
      *    THE DIAGNOSTIC IS NOT OVERLAYED.                           * 00514100
      *---------------------------------------------------------------* 00514200
      *    IF NA-SQL-FAILED OR NA-MISC-CICS-ERROR                       00514300
      *        EXEC CICS SEND CONTROL ALARM END-EXEC                    00514400
      *        EXEC CICS RETURN END-EXEC.                               00514500
                                                                        00514600
           IF NA-SUCCESSFUL                                             00514700
COBOLU         MOVE SPACES TO COM-DISPLAY-NAME                          00514800
               MOVE NA-COMM-MAIL-LINE1 TO COM-DISPLAY-NAME              00514900
               MOVE COM-DISPLAY-NAME   TO MAP-DIS-NAMEO.                00515000
                                                                        00515100
       0310-INITIATE-ROUTER.                                            00515200
           DISPLAY 'INSIDE 0310-INITIATE-ROUTER PARA'                   00515300
      *    PERFORM 0415-UNLOCK-IOMOD.                                   00515400
                                                                        00515500
           MOVE '00'              TO COMM-NEXT-FUNCTION.                00515600
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT               00515700
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.              00515800
      * --------------------------------------------------------------* 00515900
      * NOT FOUND ERROR MESSAGE ON DB2 LOOKUP                         * 00516000
      * --------------------------------------------------------------* 00516100
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED  = 'Y'     00516200
               MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)           00516300
               COMM-MSG-ID (3) COMM-MSG-ID (4)                          00516400
               MOVE 'NAU0'            TO COMM-PREVIOUS-TRANID           00516500
           ELSE                                                         00516600
              IF SQLCODE = 100                                          00516700
                  MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1)             00516800
                  MOVE 'E' TO COMM-MSG-MAX-SEVERITY                     00516900
              ELSE                                                      00517000
                 MOVE 'NAU0'            TO COMM-PREVIOUS-TRANID.        00517100
                                                                        00517200
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00517300
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00517400
                                                                        00517500
           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                  00517600
           MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                     00517700
                                                                        00517800
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00517900
               MOVE NA-COMMAREA TO WS-LINK-SIX-HUNDRED                  00518000
               EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)            00518100
                                   FROM   (WS-LINK-STORAGE)             00518200
                                   LENGTH (WS-ZERO-LENGTH)              00518300
                                   ITEM   (WS-TS-ITEM)                  00518400
                                   RESP   (WS-CICS-RESP)                00518500
                                   REWRITE                              00518600
                                   MAIN                                 00518700
                                   END-EXEC                             00518800
           ELSE                                                         00518900
900837         MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH        00519000
               EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)            00519100
                                   FROM   (WSQ-COMMAREA)                00519200
                                   LENGTH (WS-COMM-DB2-LENGTH)          00519300
                                   ITEM   (WS-TS-ITEM)                  00519400
                                   RESP   (WS-CICS-RESP)                00519500
                                   REWRITE                              00519600
                                   MAIN                                 00519700
                                   END-EXEC.                            00519800
                                                                        00519900
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00520000
                GO TO 9999-CICS-ERROR.                                  00520100
                                                                        00520200
      *    EXEC CICS SEND MAP ('SYSBUSY')                               00520300
      *                   RESP (WS-CICS-RESP)                           00520400
      *                   END-EXEC.                                     00520500
                                                                        00520600
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00520700
      *         GO TO 9999-CICS-ERROR.                                  00520800
                                                                        00520900
MANJU *    EXEC CICS START TRANSID('NAU0')                              00521000
      *                    TERMID (EIBTRMID)                            00521100
      *                    RESP   (WS-CICS-RESP)                        00521200
      *                    END-EXEC.                                    00521300
      *                                                                 00521400
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00521500
      *         GO TO 9999-CICS-ERROR.                                  00521600
                                                                        00521700
           EXEC CICS RETURN                                             00521800
                     END-EXEC.                                          00521900
                                                                        00522000
      * --------------------------------------------------------------* 00522100
      *   THE FOLLOWING CODE EXECUTES THE PSI TUTORIAL OR ONLINE HELP * 00522200
      *   FUNCTION  IT STORES ANY CHANGES MADE ON THE MAP IN A        * 00522300
      *   TEMPORARY STORAGE QUEUE, THEN IT INITIATES THE TUTORIAL     * 00522400
      *   COMMAREA AND XCNTL'S TO THE TUTORIAL PROGRAM                * 00522500
      * --------------------------------------------------------------* 00522600
                                                                        00522700
                                                                        00522800
       0320-PROCESS-TUTORIAL-REQUEST.                                   00522900
                                                                        00523000
900526     MOVE 'NA340M1' TO T3-SCREEN-NAME                             00523100
900526     MOVE 'Y'       TO T3-COMMAND-LINE                            00523200
900526     MOVE SPACES    TO T3-CARRIER                                 00523300
900526     EXEC CICS LINK PROGRAM  ('TU003')                            00523400
900526                    COMMAREA (T3-COMMAREA)                        00523500
990721                    LENGTH   (LENGTH OF T3-COMMAREA)              00523600
900526                    END-EXEC.                                     00523700
900526                                                                  00523800
900526     EXEC CICS RETURN TRANSID ('NAU0') END-EXEC.                  00523900
                                                                        00524000
      *---------------------------------------------------------------* 00524100
900526*    CODE NO LONGER NEEDED FOR VER. 3.1 TUTORIAL                * 00524200
900526*---------------------------------------------------------------* 00524300
900526*    EXEC CICS RECEIVE MAP    ('NA340M1')                       * 00524400
900526*                      RESP   (WS-CICS-RESP)                    * 00524500
900526*                      END-EXEC.                                * 00524600
900526*                                                               * 00524700
900526*    IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                  * 00524800
900526*         WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                   * 00524900
900526*             GO TO 9999-CICS-ERROR.                            * 00525000
900526*                                                               * 00525100
900526*    PERFORM 0110-UPDATE-COMMAND-LINE.                          * 00525200
900526*    PERFORM 0120-UPDATE-CIM-MAP-FIELDS.                        * 00525300
900526*                                                               * 00525400
900526*                                                               * 00525500
900526*                                                               * 00525600
900526*    MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.             * 00525700
900526*    MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.             * 00525800
900526*                                                               * 00525900
900526*    MOVE 'NA340M1'          TO TC-SCREEN-NAME.                 * 00526000
900526*    MOVE 'NAU0'             TO TC-TRANSACTION-ID.              * 00526100
900526*    MOVE  1                 TO TC-PAGE-NUMBER.                 * 00526200
900526*    MOVE 'Y'                TO TC-COMMAND-LINE.                * 00526300
900526*    MOVE 'NA340'            TO TC-PROGRAM-NAME.                * 00526400
900526*    MOVE EIBCPOSN           TO TC-CURSOR-POSITION              * 00526500
900526*                               COMM-CURSOR-POSN.               * 00526600
900526*    MOVE SPACE              TO TC-KEY-DATA.                    * 00526700
900526*    MOVE COMM-MSG-ID(1)     TO TC-MESSAGE(1).                  * 00526800
900526*    MOVE COMM-MSG-ID(2)     TO TC-MESSAGE(2).                  * 00526900
900526*    MOVE COMM-MSG-ID(3)     TO TC-MESSAGE(3).                  * 00527000
900526*    MOVE COMM-MSG-ID(4)     TO TC-MESSAGE(4).                  * 00527100
900526*                                                               * 00527200
900526*                                                               * 00527300
900526*    IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED = 'Y'    * 00527400
900526*        MOVE 'Y' TO COMM-KEY-CHANGED                           * 00527500
900526*        MOVE 'Y' TO COMM-CMDLINE-CHANGED.                      * 00527600
900526*                                                               * 00527700
900526*                                                               * 00527800
900526*    MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                * 00527900
900526*    MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                   * 00528000
900526*    MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.         * 00528100
900526*    EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)              * 00528200
900526*                        FROM   (WSQ-COMMAREA)                  * 00528300
900526*                        LENGTH (WS-COMM-DB2-LENGTH)            * 00528400
900526*                        ITEM   (WS-TS-ITEM)                    * 00528500
900526*                        RESP   (WS-CICS-RESP)                  * 00528600
900526*                        REWRITE                                * 00528700
900526*                        MAIN                                   * 00528800
900526*                        END-EXEC.                              * 00528900
900526*                                                               * 00529000
900526*    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                      * 00529100
900526*         GO TO 9999-CICS-ERROR.                                * 00529200
900526*                                                               * 00529300
900526*    EXEC CICS XCTL PROGRAM  ('TU001')                          * 00529400
900526*                   COMMAREA (TUTORIAL-COMM-AREA)               * 00529500
900526*                   LENGTH   (WS-TUTOR-COMM-LENGTH)             * 00529600
900526*                   END-EXEC.                                   * 00529700
900526*                                                               * 00529800
900526*                                                               * 00529900
900526*0330-RETURN-FROM-TUTORIAL.                                     * 00530000
900526* --------------------------------------------------------------* 00530100
900526*  CONTROL WAS RETURNED FROM THE TUTORIAL PROGRAM. READ THE     * 00530200
900526*  'HELP' TS QUEUE AND DISPLAY THE MAP AS IT APPEARED PRIOR     * 00530300
900526*  TO GOING TO THE HELP FACILITY.                               * 00530400
900526* --------------------------------------------------------------* 00530500
900526*                                                               * 00530600
900526*                                                               * 00530700
900526*    MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.         * 00530800
900526*    MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.         * 00530900
900526*    IF  EIBAID = DFHCLEAR                                      * 00531000
900526*        PERFORM 0340-PROCESS-SCREEN-CLEAR.                     * 00531100
900526*                                                               * 00531200
900526*                                                               * 00531300
900526*    IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                 * 00531400
900526*        MOVE ALL '_' TO MAP-SYSTEM-CDO                         * 00531500
900526*    ELSE                                                       * 00531600
900526*       MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.              * 00531700
900526*    IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                 * 00531800
900526*        MOVE ALL '_' TO MAP-ACTION-CDO                         * 00531900
900526*    ELSE                                                       * 00532000
900526*       MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.              * 00532100
900526*    IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES               * 00532200
900526*        MOVE ALL '_' TO MAP-CUST-INFOO                         * 00532300
900526*    ELSE                                                       * 00532400
900526*       MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.              * 00532500
900526*                                                               * 00532600
900526*    MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                * 00532700
900526*    MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                   * 00532800
900526*    MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.         * 00532900
900526*    EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)              * 00533000
900526*                        FROM   (WSQ-COMMAREA)                  * 00533100
900526*                        LENGTH (WS-COMM-DB2-LENGTH)            * 00533200
900526*                        ITEM   (WS-TS-ITEM)                    * 00533300
900526*                        RESP   (WS-CICS-RESP)                  * 00533400
900526*                        REWRITE                                * 00533500
900526*                        MAIN                                   * 00533600
900526*                        END-EXEC.                              * 00533700
900526*                                                               * 00533800
900526*    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                      * 00533900
900526*         GO TO 9999-CICS-ERROR.                                * 00534000
900526*                                                               * 00534100
900526*    PERFORM 0390-CHECK-ERROR-MESSAGES                          * 00534200
900526*        VARYING ACTION-SUB FROM 1 BY 1                         * 00534300
900526*            UNTIL ACTION-SUB > 4.                              * 00534400
900526*                                                               * 00534500
900526*    PERFORM 0290-MOVE-OUT-MAP-FIELDS.                          * 00534600
900526*                                                               * 00534700
900526*    EXEC CICS SEND MAP    ('NA340M1')                          * 00534800
900526*                   CURSOR (COMM-CURSOR-POSN)                   * 00534900
900526*                   ERASE                                       * 00535000
900526*                   RESP (WS-CICS-RESP)                         * 00535100
900526*                   END-EXEC.                                   * 00535200
900526*                                                               * 00535300
900526*    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                      * 00535400
900526*         GO TO 9999-CICS-ERROR.                                * 00535500
900526*                                                               * 00535600
900526*    EXEC CICS RETURN TRANSID ('NAU0')                          * 00535700
900526*                     END-EXEC.                                 * 00535800
900526*                                                               * 00535900
900526*    GOBACK.                                                    * 00536000
      *---------------------------------------------------------------* 00536100
                                                                        00536200
       0335-PROCESS-IS020-CALL.                                         00536300
                                                                        00536400
           EXEC CICS ASSIGN OPID(OPER-ID) END-EXEC.                     00536500
R04342     IF COM-ISSUE-STATE NOT = SPACES                              00536600
R04342        MOVE COM-ISSUE-STATE     TO AC-PREV-STATE                 00536700
R04342     END-IF.                                                      00536800
           MOVE OPER-ID                TO INITIALS.                     00536900
           MOVE CASE-CASE-NUM          TO UNIQUENUM.                    00537000
           MOVE CASE-HLTH-CARRIER      TO CARRIER.                      00537100
           MOVE CASE-HLTH-POLICY       TO CURR-HEALTH.                  00537200
           MOVE CASE-STATE-CODE        TO AC-CURR-STATE.                00537300
           MOVE 'NAU0'                 TO AC-TRANID.                    00537400
           MOVE 'CX'                   TO AUDIT-CODE.                   00537500
           MOVE +1532                  TO AC-REMAINDER-LENGTH.          00537600
                                                                        00537700
MANJU *    EXEC CICS LINK PROGRAM ('AUDPRG2')                           00537800
      *                   COMMAREA (AUDIT-COMM-AREA)                    00537900
990721*                   LENGTH (WS-AUD-LENGTH)                        00538000
990721*                   LENGTH (LENGTH OF AUDIT-COMM-AREA)            00538100
      *                   END-EXEC.                                     00538200
                                                                        00538300
890989     IF STATE-CHANGED-SW       = 'Y' OR                           00538400
890989          ZIP-CHANGED-SW       = 'Y' OR                           00538500
890989          ZIP-PLUS4-CHANGED-SW = 'Y'                              00538600
890989     OR (COM-FINALST-OVRD-IND = 'Y' AND                           00538700
890989         COM-FINALIST-SW      = 'Y')                              00538800
890989         PERFORM 9920-READ-EMPMSTR THRU 9920-EXIT.                00538900
                                                                        00539000
                                                                        00539100
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00539200
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00539300
                                                                        00539400
           MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1).                   00539500
           MOVE WS-MESSAGE-NUMBER2 TO COMM-MSG-ID(2).                   00539600
           MOVE WS-MESSAGE-NUMBER3 TO COMM-MSG-ID(3).                   00539700
           MOVE WS-MESSAGE-NUMBER4 TO COMM-MSG-ID(4).                   00539800
                                                                        00539900
           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                  00540000
           MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                     00540100
           MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.           00540200
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00540300
                               FROM   (WSQ-COMMAREA)                    00540400
                               LENGTH (WS-COMM-DB2-LENGTH)              00540500
                               ITEM   (WS-TS-ITEM)                      00540600
                               RESP   (WS-CICS-RESP)                    00540700
                               REWRITE                                  00540800
                               MAIN                                     00540900
                               END-EXEC.                                00541000
                                                                        00541100
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00541200
                GO TO 9999-CICS-ERROR.                                  00541300
                                                                        00541400
      *    EXEC CICS SEND MAP ('SYSBUSY')                               00541500
      *                   RESP (WS-CICS-RESP)                           00541600
      *                   END-EXEC.                                     00541700
      *                                                                 00541800
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00541900
      *         GO TO 9999-CICS-ERROR.                                  00542000
      *                                                                 00542100
      *    EXEC CICS XCTL PROGRAM  ('IS020')                            00542200
      *                   COMMAREA (IS020-COMMAREA)                     00542300
      *                   LENGTH   (IS020-LEN)                          00542400
      *                   END-EXEC.                                     00542500
                                                                        00542600
                                                                        00542700
980714*0336-ADD-BILL-REQUEST.                                           00542800
                                                                        00542900
R00946*    INITIALIZE CASEX-CARR-AFFL-CODE.                             00543000
R00946*    MOVE CASE-CASE-NUM TO CASEX-CASE-NUM.                        00543100
                                                                        00543200
R00946*    EXEC SQL                                                     00543300
R00946*        SELECT CARR_AFFL_CODE                                    00543400
R00946*          INTO :CASEX-CARR-AFFL-CODE                             00543500
R00946*          FROM CASEXV01                                          00543600
11735A*          FROM CASE_MASTER                                       00543700
R00946*         WHERE CASE_NUM = :CASEX-CASE-NUM                        00543800
R00946*    END-EXEC.                                                    00543900
                                                                        00544000
R00946*    IF SQLCODE NOT = 0                                           00544100
      *       GO TO 0420-DB2-ERROR.                                     00544200
                                                                        00544300
980714*    IF CASE-ACTIVE-CODE OF CASE-MASTER-RECORD NOT = 'CM'         00544400
980714*        GO TO 0336-EXIT.                                         00544500
                                                                        00544600
980714*    INITIALIZE BILLING-REQUEST-RECORDX.                          00544700
                                                                        00544800
980714*    MOVE CASE-CASE-NUM     TO BRR-CASE-NUMBER.                   00544900
980714*    MOVE CASE-HLTH-CARRIER TO BRR-CARRIER-CODE.                  00545000
980714*    MOVE CASE-SITE-CODE    TO BRR-SITE-CODE-1, BRR-SITE-CODE-2.  00545100
980714*    MOVE WS-DEMO-LOGONID   TO BRR-REQUEST-LOGONID.               00545200
980714*    MOVE WS-DEMO-DIVISION  TO BRR-DIVISION-NUMBER.               00545300
980714*    MOVE WS-DEMO-BRANCH    TO BRR-BRANCH-NUMBER.                 00545400
980714*    MOVE 'NA340   '        TO BRR-SOURCE-PROGRAM.                00545500
980714*    MOVE ZEROS             TO BRR-EMPLOYEE-NUMBER,               00545600
980714*                              BRR-DEPENDENT-NUMBER.              00545700
R00946**   MOVE CASE-MKT-CODE     TO BRR-AFFL-CODE.                     00545800
R00946*    MOVE CASEX-CARR-AFFL-CODE TO BRR-AFFL-CODE.                  00545900
980714*    MOVE CASE-NEXT-BILL-YY   TO BRR-NEXT-BILL-YY.                00546000
980714*    MOVE CASE-NEXT-BILL-MM   TO BRR-NEXT-BILL-MM.                00546100
980714*    MOVE CASE-NEXT-BILL-DD   TO BRR-NEXT-BILL-DD.                00546200
980714*    MOVE CASE-HOW-BILLED     OF CASE-MASTER-RECORD               00546300
      *                             TO BRR-HOW-BILLED.                  00546400
980714*    EXEC CICS LINK                                               00546500
980714*        PROGRAM('RQ09999')                                       00546600
980714*        COMMAREA(BILLING-REQUEST-RECORDX)                        00546700
980714*        LENGTH(LENGTH OF BILLING-REQUEST-RECORDX)                00546800
980714*    END-EXEC.                                                    00546900
                                                                        00547000
980714*    IF BRR-ERROR-CODE NOT = SPACES                               00547100
980714*        MOVE BRR-ERROR-CODE TO WS-MESSAGE-NUMBER1                00547200
980714*        PERFORM 0380-BUMP-ERROR-MESSAGES.                        00547300
                                                                        00547400
980714*0336-EXIT.                                                       00547500
980714*    EXIT.                                                        00547600
                                                                        00547700
       0340-PROCESS-SCREEN-CLEAR.                                       00547800
                                                                        00547900
           PERFORM 0415-UNLOCK-IOMOD.                                   00548000
                                                                        00548100
           MOVE 'NAU0' TO COMM-PREVIOUS-TRANID.                         00548200
           MOVE '00'   TO COMM-NEXT-FUNCTION.                           00548300
           MOVE ZERO   TO COMM-CURSOR-POSN     COMM-MSG-COUNT           00548400
                          COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.   00548500
           MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                        00548600
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00548700
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00548800
                                                                        00548900
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00549000
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00549100
                                                                        00549200
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00549300
                               FROM   (WS-TS-QUEUE)                     00549400
                               LENGTH (WS-ZERO-LENGTH)                  00549500
                               ITEM   (WS-TS-ITEM)                      00549600
                               RESP   (WS-CICS-RESP)                    00549700
                               REWRITE                                  00549800
                               MAIN                                     00549900
                               END-EXEC.                                00550000
                                                                        00550100
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00550200
                GO TO 9999-CICS-ERROR.                                  00550300
                                                                        00550400
           MOVE SPACES TO WS-NULL-FIELD.                                00550500
           EXEC CICS SEND FROM (WS-NULL-FIELD)                          00550600
                          RESP (WS-CICS-RESP)                           00550700
                          ERASE                                         00550800
                          LENGTH(72)                                    00550900
                          END-EXEC.                                     00551000
                                                                        00551100
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00551200
                GO TO 9999-CICS-ERROR.                                  00551300
                                                                        00551400
           EXEC CICS RETURN                                             00551500
                     END-EXEC.                                          00551600
                                                                        00551700
                                                                        00551800
       0350-WRITE-NAQ1-QUEUE.                                           00551900
                                                                        00552000
           MOVE SPACES TO WSQ-COMMAREA.                                 00552100
           MOVE ZERO   TO WSQ-CURSOR-POSN     WSQ-MSG-COUNT             00552200
                          WSQ-MAIL-LINE-COUNT WSQ-CUST-TOKEN-COUNT.     00552300
           MOVE '00'   TO WSQ-NEXT-FUNCTION.                            00552400
                                                                        00552500
900837     MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.           00552600
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00552700
                               FROM   (WSQ-COMMAREA)                    00552800
                               LENGTH (WS-COMM-DB2-LENGTH)              00552900
                               ITEM   (WS-TS-ITEM)                      00553000
                               RESP   (WS-CICS-RESP)                    00553100
                               MAIN                                     00553200
                               END-EXEC.                                00553300
                                                                        00553400
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00553500
                GO TO 9999-CICS-ERROR.                                  00553600
                                                                        00553700
           MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.           00553800
           MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.           00553900
           EXEC CICS READQ TS QUEUE  (WS-TS-QUEUE-NAME)                 00554000
                              SET    (ADDRESS OF WS-LINK-STORAGE)       00554100
                              LENGTH (WS-ZERO-LENGTH)                   00554200
                              ITEM   (WS-TS-ITEM)                       00554300
                              RESP   (WS-CICS-RESP)                     00554400
                              END-EXEC.                                 00554500
                                                                        00554600
                                                                        00554700
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00554800
                GO TO 9999-CICS-ERROR.                                  00554900
                                                                        00555000
           MOVE WS-ZERO-LENGTH TO WS-ZERO-LENGTH.                       00555100
           COMPUTE WS-LINK-LENGTH = (WS-ZERO-LENGTH - +600).            00555200
           MOVE WS-LINK-STORAGE TO WS-TS-QUEUE.                         00555300
                                                                        00555400
       0360-WRONG-KEY-HIT.                                              00555500
                                                                        00555600
           EXEC CICS RECEIVE MAP    ('NA340M1')                         00555700
                             RESP   (WS-CICS-RESP)                      00555800
                             END-EXEC.                                  00555900
                                                                        00556000
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00556100
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00556200
                GO TO 9999-CICS-ERROR.                                  00556300
                                                                        00556400
           PERFORM 0110-UPDATE-COMMAND-LINE.                            00556500
           PERFORM 0120-UPDATE-CIM-MAP-FIELDS.                          00556600
                                                                        00556700
                                                                        00556800
                                                                        00556900
      * --------------------------------------------------------------* 00557000
      * THIS CODE KEEPS THE UPDATE MESSAGE BR152 FROM BEING DISPLAYED * 00557100
      *  IF AN UPDATE HAD BEEN COMMPLETED PRIOR AND THEN THE USER     * 00557200
      *  HIST AN INVALID KEY.                                         * 00557300
      * --------------------------------------------------------------* 00557400
                                                                        00557500
           IF COMM-MSG-ID (1) = 'NA159'                                 00557600
               MOVE SPACES TO COMM-MSG-ID (1)                           00557700
           ELSE                                                         00557800
           IF COMM-MSG-ID (2) = 'NA159'                                 00557900
               MOVE SPACES TO COMM-MSG-ID (2)                           00558000
           ELSE                                                         00558100
           IF COMM-MSG-ID (3) = 'NA159'                                 00558200
               MOVE SPACES TO COMM-MSG-ID (3)                           00558300
           ELSE                                                         00558400
           IF COMM-MSG-ID (4) = 'NA159'                                 00558500
               MOVE SPACES TO COMM-MSG-ID (4).                          00558600
                                                                        00558700
           MOVE 'NA030'           TO WS-MESSAGE-NUMBER1.                00558800
           PERFORM 0380-BUMP-ERROR-MESSAGES.                            00558900
           MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(1).                    00559000
           MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(2).                    00559100
           MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(3).                    00559200
           MOVE WSQ-MSG-ID(4)     TO COMM-MSG-ID(4).                    00559300
                                                                        00559400
           MOVE EIBCPOSN           TO COMM-CURSOR-POSN.                 00559500
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00559600
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00559700
                                                                        00559800
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED = 'Y'      00559900
               MOVE 'Y' TO COMM-KEY-CHANGED                             00560000
               MOVE 'Y' TO COMM-CMDLINE-CHANGED.                        00560100
                                                                        00560200
           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.                  00560300
           MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.                     00560400
900837     MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.           00560500
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00560600
                               FROM   (WSQ-COMMAREA)                    00560700
                               LENGTH (WS-COMM-DB2-LENGTH)              00560800
                               ITEM   (WS-TS-ITEM)                      00560900
                               RESP   (WS-CICS-RESP)                    00561000
                               REWRITE                                  00561100
                               MAIN                                     00561200
                               END-EXEC.                                00561300
                                                                        00561400
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00561500
                GO TO 9999-CICS-ERROR.                                  00561600
                                                                        00561700
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00561800
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00561900
           ELSE                                                         00562000
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00562100
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00562200
               MOVE ALL '_' TO MAP-ACTION-CDO                           00562300
           ELSE                                                         00562400
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00562500
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00562600
               MOVE ALL '_' TO MAP-CUST-INFOO                           00562700
           ELSE                                                         00562800
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00562900
                                                                        00563000
                                                                        00563100
           PERFORM 0290-MOVE-OUT-MAP-FIELDS.                            00563200
                                                                        00563300
           EXEC CICS SEND MAP    ('NA340M1')                            00563400
                          CURSOR (COMM-CURSOR-POSN)                     00563500
                          ERASE                                         00563600
                          RESP (WS-CICS-RESP)                           00563700
                          END-EXEC.                                     00563800
                                                                        00563900
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00564000
                GO TO 9999-CICS-ERROR.                                  00564100
                                                                        00564200
           EXEC CICS RETURN TRANSID ('NAU0')                            00564300
                            END-EXEC.                                   00564400
                                                                        00564500
       0370-RETURN-TO-MT-BILLION.                                       00564600
                                                                        00564700
           PERFORM 0415-UNLOCK-IOMOD.                                   00564800
                                                                        00564900
           MOVE 'NAU0' TO COMM-PREVIOUS-TRANID.                         00565000
           MOVE '00'   TO COMM-NEXT-FUNCTION.                           00565100
           MOVE ZERO   TO COMM-CURSOR-POSN     COMM-MSG-COUNT           00565200
                          COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.   00565300
           MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                        00565400
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)               00565500
                          COMM-MSG-ID (3) COMM-MSG-ID (4).              00565600
                                                                        00565700
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.               00565800
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.               00565900
                                                                        00566000
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)                00566100
                               FROM   (WS-TS-QUEUE)                     00566200
                               LENGTH (WS-ZERO-LENGTH)                  00566300
                               ITEM   (WS-TS-ITEM)                      00566400
                               RESP   (WS-CICS-RESP)                    00566500
                               REWRITE                                  00566600
                               MAIN                                     00566700
                               END-EXEC.                                00566800
                                                                        00566900
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00567000
                GO TO 9999-CICS-ERROR.                                  00567100
                                                                        00567200
MANJU *    EXEC CICS SEND MAP  ('SYSBUSY')                              00567300
      *                   RESP (WS-CICS-RESP)                           00567400
      *                   END-EXEC.                                     00567500
      *                                                                 00567600
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00567700
      *         GO TO 9999-CICS-ERROR.                                  00567800
      *                                                                 00567900
      *    EXEC CICS START TRANSID('PA3')                               00568000
      *                    RESP (WS-CICS-RESP)                          00568100
      *                    TERMID (EIBTRMID)                            00568200
      *                    END-EXEC.                                    00568300
      *                                                                 00568400
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00568500
      *         GO TO 9999-CICS-ERROR.                                  00568600
      *                                                                 00568700
           EXEC CICS RETURN                                             00568800
                     END-EXEC.                                          00568900
                                                                        00569000
       0380-BUMP-ERROR-MESSAGES.                                        00569100
                                                                        00569200
           MOVE COMM-MSG-ID(1)    TO WSQ-MSG-ID(1).                     00569300
           MOVE COMM-MSG-ID(2)    TO WSQ-MSG-ID(2).                     00569400
           MOVE COMM-MSG-ID(3)    TO WSQ-MSG-ID(3).                     00569500
           MOVE COMM-MSG-ID(4)    TO WSQ-MSG-ID(4).                     00569600
           MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1).                   00569700
           MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(2).                    00569800
           MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(3).                    00569900
           MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(4).                    00570000
           PERFORM 0390-CHECK-ERROR-MESSAGES                            00570100
               VARYING ACTION-SUB FROM 1 BY 1                           00570200
                   UNTIL ACTION-SUB > 4.                                00570300
                                                                        00570400
       0390-CHECK-ERROR-MESSAGES.                                       00570500
           DISPLAY '0390-CHECK-ERROR-MESSAGES PARA'                     00570600
           IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND                 00570700
               COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES                00570800
               MOVE COMM-MSG-ID (ACTION-SUB) TO WT-C9999-ERROR-CODE     00570900
               PERFORM 0410-GET-ERROR-MESSAGE                           00571000
               PERFORM 0400-CHECK-RETURN-CODE.                          00571100
                                                                        00571200
       0400-CHECK-RETURN-CODE.                                          00571300
               DISPLAY 'SQLCODE' SQLCODE.                               00571400
               IF SQLCODE = ZERO                                        00571500
                   MOVE WT-C9999-ERROR-CODE TO WS-C9999-ERROR-CODE      00571600
      *            MOVE WT-C9999-SEVERITY-CODE TO WS-C9999-SEVERITY-CODE00571700
                   MOVE EDIT-DESC TO WS-C9999-ERROR-MESSAGE             00571800
                   MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (ACTION-SUB)  00571900
               ELSE                                                     00572000
                  IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND          00572100
                      COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES         00572200
                      MOVE '** INVALID ERROR MESSAGE ** ' TO            00572300
                           MAP-ERROR-MSGO (ACTION-SUB).                 00572400
                                                                        00572500
       0410-GET-ERROR-MESSAGE.                                          00572600
           DISPLAY 'EDIT WT-C9999-ERROR-CODE' WT-C9999-ERROR-CODE.      00572700
           EXEC SQL                                                     00572800
           SELECT EDIT_DESC                                             00572900
              INTO :EDIT-DESC                                           00573000
             FROM EDITCODE                                              00573100
            WHERE EDIT_CD = :WT-C9999-ERROR-CODE                        00573200
           END-EXEC.                                                    00573300
                                                                        00573400
       0415-UNLOCK-IOMOD.                                               00573500
           MOVE 'L'       TO IO-FUNCTION.                               00573600
           MOVE EIBTRMID  TO IO-TERM.                                   00573700
COBOLU*    EXEC CICS LINK PROGRAM ('IOMOD')                             00573800
COBOLU*                   COMMAREA (IO-COMM-AREA)                       00573900
COBOLU*                   LENGTH (50)                                   00574000
COBOLU*                   END-EXEC.                                     00574100
                                                                        00574200
COBOLU*    EXEC CICS LINK PROGRAM ('IOMOD2K')                           00574300
COBOLU*                   COMMAREA (IO-COMM-AREA)                       00574400
COBOLU*                   LENGTH (58)                                   00574500
COBOLU*                   END-EXEC.                                     00574600
                                                                        00574700
       0420-DB2-ERROR.                                                  00574800
                                                                        00574900
           MOVE SQLCODE TO WS-DB2I-MESSAGE.                             00575000
           MOVE WS-DB2I-MESSAGE TO WS-C9999-ERROR-CODE.                 00575100
           MOVE "DB2 SQL ERROR" TO WS-C9999-ERROR-MESSAGE.              00575200
           MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (1).                  00575300
           PERFORM 0290-MOVE-OUT-MAP-FIELDS.                            00575400
           EXEC CICS SEND MAP    ('NA340M1')                            00575500
                          CURSOR                                        00575600
                          ERASE                                         00575700
                          RESP (WS-CICS-RESP)                           00575800
                          END-EXEC.                                     00575900
           EXEC CICS RETURN TRANSID ('NAU0')                            00576000
                     END-EXEC.                                          00576100
           EXEC CICS RETURN                                             00576200
                     END-EXEC.                                          00576300
      * --------------------------------------------------------------* 00576400
      * THIS ROUTINE TRANSFERS CONTROL TO AN ONLINE ERROR PROGRAM     * 00576500
      * WHICH DISPLAYS THE ERRORS ON THE SCREEN SUPPLYING INFORMATION * 00576600
      * FOR DB2 ERRORS.                                               * 00576700
      * --------------------------------------------------------------* 00576800
      *    EXEC CICS XCTL PROGRAM('SQLERRTN')                           00576900
      *                   COMMAREA (SQLCA)                              00577000
990721*                   LENGTH (214)                                  00577100
990721*                   LENGTH (LENGTH OF SQLCA)                      00577200
      *                   END-EXEC.                                     00577300
                                                                        00577400
      * --------------------------------------------------------------* 00577500
      * THIS ROUTINE TRANSFERS CONTROL TO AN ONLINE ERROR PROGRAM     * 00577600
      * WHICH DISPLAYS THE ERRORS ON THE SCREEN SUPPLYING INFORMATION * 00577700
      * FOR THE HELP-DESK  TO AID PROGRAMMERS IN DEBUGGING.           * 00577800
      * --------------------------------------------------------------* 00577900
                                                                        00578000
           COPY CICSERR.                                                00578100
                                                                        00578200
R01717*0590-CHECK-SECURITY.                                             00578300
      *                                                                 00578400
R01717** GET CARRIER TO CHECK SECURITY                                  00578500
R01717*    EXEC SQL SELECT CARRIER_CODE                                 00578600
R01717*                INTO :CASEX-CARRIER-CODE  :CASEX-CARRIER-IND     00578700
R01717*                FROM CASEXV01                                    00578800
11735A*                FROM CASE_MASTER                                 00578900
R01717*                WHERE CASENAME#IDNTITY= :COMM-IDNTITY            00579000
R01717*    END-EXEC                                                     00579100
R01717*    IF SQLCODE NOT = 0                                           00579200
R01717*       GO TO 0590-EXIT.                                          00579300
R01717*    MOVE 'N' TO SEC-CHK-RESOURCE.                                00579400
R01717*    MOVE 'I' TO SEC-FUNCTION-CODE.                               00579500
R01717*    MOVE '       ' TO SEC-RESOURCE-NAME.                         00579600
R01717*    MOVE CASEX-CARRIER-CODE TO SEC-CARRIER-CODE.                 00579700
R01717*    MOVE 'Y' TO SEC-CHK-CARRIER.                                 00579800
R01717*    EXEC CICS LINK                                               00579900
R01717*              PROGRAM  ('SECCHECK')                              00580000
R01717*              COMMAREA (SECURITY-COMM-AREA)                      00580100
R01717*              LENGTH   (LENGTH OF SECURITY-COMM-AREA)            00580200
R01717*    END-EXEC.                                                    00580300
      *                                                                 00580400
R01717*    IF SEC-RETURN-CODE = 'A'                                     00580500
R01717*        GO TO 0590-EXIT.                                         00580600
      *                                                                 00580700
R01717*    IF SEC-RETURN-CODE NOT = 'A'                                 00580800
R01717*        MOVE 'SC007' TO WS-MESSAGE-NUMBER1.                      00580900
                                                                        00581000
R01717*    IF SEC-RETURN-CODE = 'B'                                     00581100
R01717*        MOVE 'SC002' TO WS-MESSAGE-NUMBER1.                      00581200
      *                                                                 00581300
R01717*    IF SEC-RETURN-CODE = 'C'                                     00581400
R01717*        MOVE 'SC003' TO WS-MESSAGE-NUMBER1.                      00581500
      *                                                                 00581600
R01717*    IF SEC-RETURN-CODE = 'D'                                     00581700
R01717*        MOVE 'SC004' TO WS-MESSAGE-NUMBER1.                      00581800
      *                                                                 00581900
R01717*    IF SEC-RETURN-CODE = 'E'                                     00582000
R01717*        MOVE 'SC005' TO WS-MESSAGE-NUMBER1.                      00582100
      *                                                                 00582200
R01717*    IF SEC-RETURN-CODE = 'F' OR 'G' OR 'H' OR 'I'                00582300
R01717*        MOVE 'SC006' TO WS-MESSAGE-NUMBER1.                      00582400
                                                                        00582500
R01717*0590-EXIT. EXIT.                                                 00582600
                                                                        00582700
DFJ   * --------------------------------------------------------------* 00582800
DFJ   * THIS SECTION WILL UPDATE THE CATMASTER TABLE IF ANY OF THE    * 00582900
DFJ   * FOLLOWING OCCURS: 1) THE STATUS IS CHANGED TO 'CANCELLED'.    * 00583000
DFJ   * 2) THE STATE CODE CHANGES. 3) THE ZIP CODE CHANGES.           * 00583100
DFJ   * --------------------------------------------------------------* 00583200
                                                                        00583300
DFJ    9910-UPDATE-CAM-TABLES SECTION.                                  00583400
DFJ        EXEC SQL                                                     00583500
DFJ   *11735A   SELECT *                                                00583600
11735A          SELECT                                                  00583700
                     CAT_IDNTITY_NUMBER,                                00583800
                     CAT_VERSION_QUAL,                                  00583900
                     CAT_DIST_CHANNEL,                                  00584000
                     CAT_FIELD_FORCE,                                   00584100
                     CAT_ALT_FF,                                        00584200
                     CAT_PRODUCT_CODE,                                  00584300
                     CAT_ACTIVE_IND,                                    00584400
                     CAT_UNDERWRITER,                                   00584500
                     CAT_RECEIVED_DATE,                                 00584600
                     CAT_PREMIUM_AMOUNT,                                00584700
                     CAT_DECLINE_REASON,                                00584800
                     CAT_DECLINED_DATE,                                 00584900
                     CAT_STATE_CODE,                                    00585000
                     CAT_MKT_SITE,                                      00585100
                     CAT_LICENSE_STATUS,                                00585200
                     CAT_LICENSE_LID,                                   00585300
                     CAT_LICENSE_DATE,                                  00585400
                     CAT_UNDER_STATUS,                                  00585500
                     CAT_UNDER_LID,                                     00585600
                     CAT_UNDER_DATE,                                    00585700
                     CAT_VERIF_STATUS,                                  00585800
                     CAT_VERIF_LID,                                     00585900
                     CAT_VERIF_DATE,                                    00586000
                     CAT_CERTIF_STATUS,                                 00586100
                     CAT_CERTIF_LID,                                    00586200
                     CAT_CERTIF_DATE,                                   00586300
                     CAT_ISSUE_STATUS,                                  00586400
                     CAT_ISSUE_LID,                                     00586500
                     CAT_ISSUE_DATE,                                    00586600
                     CAT_ZIP_CODE,                                      00586700
                     CAT_UNDERWRTN_DATE,                                00586800
                     CAT_COCARR_CIMNUM,                                 00586900
                     CAT_UPDATE_DATE,                                   00587000
                     CAT_UPDATE_LID,                                    00587100
                     CAT_OVERRIDE_FLAG,                                 00587200
                     CAT_FPP_FLAG,                                      00587300
                     CAT_SALES_REP_LID,                                 00587400
                     CAT_CLOSEOUT_DATE,                                 00587500
                     RECEIVED_DATE,                                     00587600
                     DECLINED_DATE,                                     00587700
                     LICENSE_DATE,                                      00587800
                     UNDER_DATE,                                        00587900
                     VERIF_DATE,                                        00588000
                     CERTIF_DATE,                                       00588100
                     ISSUE_DATE,                                        00588200
                     UNDERWRTN_DATE,                                    00588300
                     CAT_UNDER_REC_DATE,                                00588400
                     TVU_STATUS,                                        00588500
                     TVU_DATE,                                          00588600
                     ADV_COMM_STATUS,                                   00588700
                     ADV_COMM_DATE,                                     00588800
                     CAT_COM_PREPAY_FLG,                                00588900
                     CAT_COM_PREPAY_AMT,                                00589000
                     CAT_COM_PREPAY_DTE,                                00589100
                     APP_SIGNED_DATE,                                   00589200
                     SCAN_DATE,                                         00589300
                     UW_COMPLETE_DATE,                                  00589400
                     CAT_DECLINE_ID,                                    00589500
                     UW_DECISION,                                       00589600
                     HOLD_DATE,                                         00589700
                     APPL_SCRUB_DT,                                     00589800
                     APPL_SCRUB_COMP_DT                                 00589900
R01142*00504A   INTO :CAT-MASTER-REC                                    00590000
00504A          INTO :CAT-MASTER-REC:CAT-INDICATOR                      00590100
DFJ   *         FROM CAMASV01                                           00590200
11735A          FROM CATMASTER                                          00590300
DFJ             WHERE CAT_IDNTITY_NUMBER = :IDNTITY                     00590400
DFJ        END-EXEC.                                                    00590500
DFJ                                                                     00590600
DFJ        IF SQLCODE = 0                                               00590700
DFJ            MOVE CAT-MASTER-REC TO CAT-MASTER-RECORD                 00590800
DFJ          ELSE                                                       00590900
DFJ            IF SQLCODE = +100                                        00591000
DFJ                GO TO 9910-EXIT                                      00591100
DFJ              ELSE                                                   00591200
DFJ                GO TO 0420-DB2-ERROR.                                00591300
DFJ                                                                     00591400
DFJ        IF RECORD-STATUS = 'C'                                       00591500
DFJ            EXEC SQL                                                 00591600
DFJ   *             UPDATE CAMASV01                                     00591700
11735A              UPDATE CATMASTER                                    00591800
DFJ                    SET CAT_ACTIVE_IND   = :RECORD-STATUS            00591900
DFJ                      , CAT_STATE_CODE   = :STATE                    00592000
DFJ                      , CAT_ZIP_CODE     = :ZIP                      00592100
900678                   , CAT_UPDATE_DATE  = CURRENT DATE              00592200
900678                   , CAT_UPDATE_LID   = :CHANGE-LOGON             00592300
DFJ                  WHERE CAT_IDNTITY_NUMBER = :CAT-IDNTY-NUMBER       00592400
DFJ                    AND CAT_VERSION_QUAL = :CAT-VERSION-QUAL         00592500
DFJ            END-EXEC                                                 00592600
DFJ          ELSE                                                       00592700
DFJ            EXEC SQL                                                 00592800
DFJ   *             UPDATE CAMASV01                                     00592900
11735A              UPDATE CATMASTER                                    00593000
DFJ                    SET CAT_STATE_CODE   = :STATE                    00593100
DFJ                      , CAT_ZIP_CODE     = :ZIP                      00593200
DFJ                  WHERE CAT_IDNTITY_NUMBER = :CAT-IDNTY-NUMBER       00593300
DFJ            END-EXEC.                                                00593400
DFJ                                                                     00593500
DFJ        IF SQLCODE NOT = (0  AND  +100)                              00593600
DFJ            GO TO 0420-DB2-ERROR.                                    00593700
                                                                        00593800
DFJ    9910-EXIT.  EXIT.                                                00593900
                                                                        00594000
890989* --------------------------------------------------------------* 00594100
890989* THIS SECTION WILL START THE VSAM EMPLOYEE FILE AND PERFORM    * 00594200
890989* A READ LOOP, WHICH UPDATES THE APPROPRIATE EMPLOYEES WITH     * 00594300
890989* CHANGES MADE TO STATE AND ZIP AT THE CASE LEVEL.              * 00594400
890989* --------------------------------------------------------------* 00594500
890989 9920-READ-EMPMSTR SECTION.                                       00594600
                                                                        00594700
890989     MOVE CASE-KEY TO EMP-IDNTY-NUM WS-HOLD-CASE-NUM WS-CASE-NUM. 00594800
890989     MOVE ZEROES TO WS-EMP-NUM.                                   00594900
890989     MOVE 'N' TO WS-EMP-EOF.                                      00595000
890989     PERFORM 9930-READ-NEXT THRU 9930-EXIT                        00595100
890989           UNTIL WS-EMP-EOF = 'Y'.                                00595200
890989                                                                  00595300
890989 9920-EXIT.  EXIT.                                                00595400
890989 9930-READ-NEXT    SECTION.                                       00595500
890989                                                                  00595600
           ADD  1                TO WS-EMP-NUM.                         00595700
           MOVE WS-CASE-EMP-NUM  TO EMP-KEY.                            00595800
                                                                        00595900
           EXEC CICS STARTBR DATASET('EMPMSTR')                         00596000
                             RIDFLD(EMP-KEY)                            00596100
                             END-EXEC.                                  00596200
                                                                        00596300
           EXEC CICS READNEXT DATASET('EMPMSTR')                        00596400
                              INTO(EMPMSTR-REC)                         00596500
                              RIDFLD(EMP-KEY)                           00596600
990721                        LENGTH(LENGTH OF EMPMSTR-REC)             00596700
                              END-EXEC.                                 00596800
                                                                        00596900
           EXEC CICS ENDBR DATASET('EMPMSTR') END-EXEC.                 00597000
                                                                        00597100
           IF EMP-IDNTY-NUM NOT = CASE-CASE-NUM                         00597200
               MOVE 'Y'   TO WS-EMP-EOF                                 00597300
               GO TO 9930-EXIT.                                         00597400
                                                                        00597500
                                                                        00597600
890989     MOVE EMPMSTR-REC     TO AC-PREV-RECORD.                      00597700
890989     MOVE 'N' TO WS-EMP-UPDATE.                                   00597800
890989                                                                  00597900
890989     IF ZIP-CHANGED-SW = 'Y'                                      00598000
890989     OR (COM-FINALST-OVRD-IND = 'Y' AND                           00598100
890989         COM-FINALIST-SW      = 'Y')                              00598200
890989         IF  (EMP-RATING-ZIP-CODE = WS-HOLD-CASE-ZIP              00598300
890989         OR   EMP-RATING-ZIP-CODE < ZEROES)                       00598400
890989         AND (EMP-ZIP-PLUS-4      = WS-HOLD-CASE-ZIP-PLUS4        00598500
890989         OR   EMP-ZIP-PLUS-4      < ZEROES)                       00598600
890989             MOVE 'Y'           TO WS-EMP-UPDATE                  00598700
890989             MOVE COM-STATE     TO EMP-STATE-CODE                 00598800
890989             MOVE COM-ZIP       TO EMP-RATING-ZIP-CODE            00598900
890989             MOVE COM-ZIP-PLUS4 TO EMP-ZIP-PLUS-4.                00599000
890989                                                                  00599100
890989     IF ZIP-PLUS4-CHANGED-SW = 'Y'                                00599200
890989         IF  (EMP-ZIP-PLUS-4 = WS-HOLD-CASE-ZIP-PLUS4             00599300
890989         OR   EMP-ZIP-PLUS-4      < ZEROES)                       00599400
890989         AND (EMP-RATING-ZIP-CODE = WS-HOLD-CASE-ZIP              00599500
890989         OR   EMP-RATING-ZIP-CODE < ZEROES)                       00599600
890989             MOVE 'Y'           TO WS-EMP-UPDATE                  00599700
890989             MOVE COM-STATE     TO EMP-STATE-CODE                 00599800
890989             MOVE COM-ZIP       TO EMP-RATING-ZIP-CODE            00599900
890989             MOVE COM-ZIP-PLUS4 TO EMP-ZIP-PLUS-4.                00600000
890989                                                                  00600100
890989     IF STATE-CHANGED-SW = 'Y'                                    00600200
890989         IF  (EMP-STATE-CODE = WS-HOLD-CASE-STATE                 00600300
890989         OR   EMP-STATE-CODE      < SPACES)                       00600400
890989         AND (EMP-RATING-ZIP-CODE = WS-HOLD-CASE-ZIP              00600500
890989         OR   EMP-RATING-ZIP-CODE < ZEROES)                       00600600
890989         AND (EMP-ZIP-PLUS-4 = WS-HOLD-CASE-ZIP-PLUS4             00600700
890989         OR   EMP-ZIP-PLUS-4      < ZEROES)                       00600800
890989             MOVE 'Y'           TO WS-EMP-UPDATE                  00600900
890989             MOVE COM-STATE     TO EMP-STATE-CODE                 00601000
890989             MOVE COM-ZIP       TO EMP-RATING-ZIP-CODE            00601100
890989             MOVE COM-ZIP-PLUS4 TO EMP-ZIP-PLUS-4.                00601200
890989                                                                  00601300
890989     IF WS-EMP-UPDATE = 'Y'                                       00601400
890989         EXEC CICS READ                                           00601500
890989               DATASET('EMPMSTR')                                 00601600
890989               INTO(OLD-EMP-REC)                                  00601700
890989               RIDFLD(EMP-KEY)                                    00601800
990721**             KEYLENGTH(11)                                      00601900
990721               KEYLENGTH(LENGTH OF EMP-KEY)                       00602000
990721               LENGTH(LENGTH OF OLD-EMP-REC)                      00602100
890989               UPDATE                                             00602200
890989               RESP(ERROR-STATUS)                                 00602300
890989         END-EXEC                                                 00602400
890989         IF ERROR-STATUS  = DFHRESP(NOTFND)                       00602500
890989             MOVE 'BL023' TO WS-MESSAGE-NUMBER1                   00602600
890989             MOVE 'Y'     TO WS-RECORD-NOTFND                     00602700
890989             PERFORM 0380-BUMP-ERROR-MESSAGES                     00602800
890989             GO TO 9930-EXIT                                      00602900
               ELSE                                                     00603000
890989         IF ERROR-STATUS NOT = DFHRESP(NORMAL)                    00603100
890989             PERFORM 0415-UNLOCK-IOMOD                            00603200
890989             GO TO 9999-CICS-ERROR                                00603300
890989         ELSE                                                     00603400
890989         EXEC CICS REWRITE DATASET('EMPMSTR')                     00603500
890989                   FROM(EMPMSTR-REC)                              00603600
990721                   LENGTH(LENGTH OF EMPMSTR-REC)                  00603700
890989                   RESP(ERROR-STATUS)                             00603800
890989         END-EXEC                                                 00603900
890989         IF ERROR-STATUS NOT = DFHRESP(NORMAL)                    00604000
890989             PERFORM 0415-UNLOCK-IOMOD                            00604100
890989             GO TO 9999-CICS-ERROR                                00604200
890989         ELSE                                                     00604300
890989             PERFORM 9940-WRITE-AUDIT THRU 9940-EXIT.             00604400
                                                                        00604500
890989 9930-EXIT.  EXIT.                                                00604600
890989                                                                  00604700
890989 9940-WRITE-AUDIT  SECTION.                                       00604800
890989                                                                  00604900
890989     EXEC CICS ASSIGN OPID(OPER-ID) END-EXEC.                     00605000
890989     MOVE OPER-ID                TO INITIALS.                     00605100
890989     MOVE WS-HOLD-CASE-STATE     TO AC-PREV-STATE.                00605200
890989     MOVE STATE                  TO AC-CURR-STATE.                00605300
990420     MOVE EMP-EMPLOYEE-NUM       TO EMPNO.                        00605400
890989     MOVE WS-CASE-NUM            TO UNIQUENUM.                    00605500
890989     MOVE 'EG'                   TO AUDIT-CODE.                   00605600
890989     MOVE EMPMSTR-REC            TO AC-CURR-RECORD.               00605700
890989     MOVE +1110                  TO AC-REMAINDER-LENGTH.          00605800
           MOVE CASE-HLTH-AREA        TO AC-CURR-AREA.                  00605900
           MOVE CASE-HLTH-LEVEL       TO AC-CURR-LEVEL.                 00606000
           MOVE CASE-HLTH-OPTION      TO AC-CURR-CASE-OPTION.           00606100
           MOVE CASE-HLTH-CARRIER     TO CARRIER.                       00606200
           MOVE CASE-BILL-SITE-CODE   OF CASE-RECORD                    00606300
                                      TO AC-SITE-CODE.                  00606400
           MOVE +1110                 TO AC-REMAINDER-LENGTH.           00606500
                                                                        00606600
      *    EXEC CICS LINK PROGRAM('AUDPRG2')                            00606700
      *                   COMMAREA(AUDIT-COMM-AREA)                     00606800
990721**                  LENGTH(3805)                                  00606900
990721*                   LENGTH(LENGTH OF AUDIT-COMM-AREA)             00607000
      *                   END-EXEC.                                     00607100
890989******************************************************************00607200
890989*      GO CREATE CHANGE RECORDS                                   00607300
890989******************************************************************00607400
890989*    MOVE 'NA340' TO IS020CA-PROGRAM-NAME.                        00607500
890989*    MOVE 'NA340M1' TO IS020CA-MAP-NAME.                          00607600
890989*    MOVE EMP-IDNTY-NUM TO IS020CA-CASE-NUMBER.                   00607700
890989*    MOVE EMP-EMPLOYEE-NUM TO IS020CA-EMPLOYEE-NUMBER.            00607800
890989*    MOVE CASE-HLTH-CARRIER TO IS020CA-CARRIER.                   00607900
890989*    MOVE 'EM' TO IS020CA-FILE-PREFIX.                            00608000
890989*    EXEC CICS ASKTIME END-EXEC.                                  00608100
890989*    MOVE EIBTIME TO IS020CA-TIME-CASE-UPDATED.                   00608200
890989*    MOVE AC-PREV-RECORD TO IS020CA-PREVIOUS-RECORD.              00608300
890989*    MOVE AC-CURR-RECORD TO IS020CA-CURRENT-RECORD.               00608400
890989*    EXEC CICS XCTL                                               00608500
890989*              PROGRAM('IS020')                                   00608600
890989*              COMMAREA(IS020-COMMAREA)                           00608700
890989*              LENGTH(3452)                                       00608800
890989*    END-EXEC.                                                    00608900
890989                                                                  00609000
890989 9940-EXIT.  EXIT.                                                00609100
890989* --------------------------------------------------------------* 00609200
890989* THIS SECTION WILL UPDATE THE DB2 EMPLOYEE FILE WHEN THE STATE * 00609300
890989* OR ZIP OF THE CATMASTER CHANGES AND MATCHES THAT OF THE EMPL. * 00610000
R03749* DETERMINE NETWORK PRODUCT NUMBER ASSOCIATED WITH NEW ZIP, CAUSE 00613800
R03749* HUMANA MUST NOT ALLOW ADDRESS CHANGE IF IT CAUSES FLOA NETWORK  00613900
R03749* TO CHANGE TO NON-FLOA NETWORK. THE REP WOULD HAVE TO CHANGE THE 00614000
R03749* PLAN/NETWORK FIRST BEFORE APPLYING THE ADDRESS CHANGE.          00614100
R03749 9955-EDIT-PROD-LINE-NUM.                                         00614200
R03749*----------------------------------------------------------------*00614300
R03749*   CALL MODULE GE01600 TO CHECK PRODUCT NUMBER FOR ZIP          *00614400
R03749*----------------------------------------------------------------*00614500
R03749                                                                  00614600
R03749     MOVE 'GE01600' TO FIELD-EDIT-MODULE.                         00614700
R03749     CALL FIELD-EDIT-MODULE                                       00614800
R03749         USING DFHEIBLK DFHCOMMAREA                               00614900
R03749         BY REFERENCE EDIT-AREA                                   00615000
R03749                      CASE-SITE-CODE                              00615100
R03749                      WS-GE01600-FLD2-NOT-NEEDED                  00615200
R03749                      WS-GE01600-FLD3-NOT-NEEDED                  00615300
R03749                      WS-GE01600-FLD4-NOT-NEEDED                  00615400
R03749                      CASEX-CARRIER-CODE                          00615500
R03749                      WS-GE01600-FLD6-NOT-NEEDED                  00615600
R03749                      WS-GE01600-FLD7-NOT-NEEDED                  00615700
R03749                      WS-GE01600-FLD8-NOT-NEEDED                  00615800
R03749** PASS THE NEW STATE AND ZIP (FROM THE NA U SCREEN)              00615900
R03749** SO WE CAN FIND OUT IF THE PROD NUM NETWORK IS VALID/OR CHANGING00616000
R03749                      COM-STATE                                   00616100
R03749                      COM-ZIP                                     00616200
R03749                      WS-HLTH-POLICY                              00616300
R03749** PASS THE CURRENT PRODUCT LINE/ PRODUCT NUMBER THAT             00616400
R03749** GE01600 WILL NEED TO CHECK TO SEE IF THEY ARE VALID FOR ZIP    00616500
R03749                      CASE-GE01600-PRODUCT-LINE                   00616600
R03749                      CASE-GE01600-PRODUCT-NUMBER                 00616700
R03749                      CASE-BILL-FREQUENCY OF CASE-RECORD          00616800
R03749                      CASEX-CARR-AFFL-CODE                        00616900
R03749                      CASE-MICKEYM-AGENT-CODE                     00617000
R03749                      WS-DENTAL-PLAN                              00617100
R03749                      WS-GE01600-FLD18-NOT-NEEDED                 00617200
R03749                      WS-GE01600-FLD19-NOT-NEEDED                 00617300
R03749                      WS-INCEPTION-DATE                           00617400
R03749                      CASE-COUNTY-CODE OF CASE-RECORD             00617500
R03749                      WS-HOLD-HLTH-OPTION                         00617600
R11386                      COMM-IDNTITY.                               00617700
R03749                                                                  00617800
R03749 9955-EXIT.                                                       00617900
R03749     EXIT.                                                        00618000
R03989                                                                  00624000
R08986****************************************************************  00624100
R08986*** SINCE THE ORIGINAL-STATE IS NOT AVAILABLE AT THIS POINT       00624200
R08986*** WE NEED TO ACCESS THE CASENAME TABLE TO IN ORDER TO COMPARE   00624300
R08986*** THE ORIGINAL ISSUE STATE WITH WHAT IN THE NEW CASE STATE      00624400
R08986****************************************************************  00624500
R08986 9961-READ-CASE-NAME.                                             00624600
R08986     INITIALIZE ORIGINAL-STATE                                    00624700
R08986     EXEC SQL SELECT ORIGINAL_STATE                               00624800
R08986         INTO :ORIGINAL-STATE                                     00624900
R08986         FROM CASENAME                                            00625000
R08986        WHERE IDNTITY = :WS-CIM-NUMBER                            00625100
R08986     END-EXEC.                                                    00625200
R08986                                                                  00625300
R08986     IF  SQLCODE  = +0                                            00625400
R08986         NEXT SENTENCE                                            00625500
R08986     ELSE                                                         00625600
R08986         IF SQLCODE = 100                                         00625700
R08986             MOVE 'AM007'       TO WS-MESSAGE-NUMBER1             00625800
R08986             MOVE 'Y'           TO WS-RECORD-NOTFND               00625900
R08986             PERFORM 0380-BUMP-ERROR-MESSAGES                     00626000
R08986          END-IF                                                  00626100
R08986      END-IF.                                                     00626200
R08986                                                                  00626300
R08986 9961-EXIT.                                                       00626400
R08986     EXIT.                                                        00626500
R08749                                                                  00626600
R8285A***** R08041-COVERAGE EDITS TABLE ROW CHECKING *****              00631600
R8285A     COPY COVRECHK REPLACING 9999-DB2-ERROR BY                    00631700
R8285A                             0420-DB2-ERROR.                      00631800
R08986                                                                  00631900
R08492                                                                  00632000
R08986 R07063-RULEPCB SECTION.                                          00632100
R08986 COPY RULEPCB REPLACING 9999-DB2-ERROR BY                         00632200
R08986                        0420-DB2-ERROR.                           00632300
R08986 R07063-RULEPCB-EXIT.                                             00632400
R08986     EXIT.                                                        00632500
R08986                                                                  00632600
      * --------------------------------------------------------------* 00632700
      * THIS ROUTINE TRANSFERS CONTROL TO THE TURBO TABLES FOR EDIT   * 00632800
      * VERIFICATION                                                  * 00632900
      * --------------------------------------------------------------* 00633000
                                                                        00633100
           COPY TURBOCAL.                                               00634000
                                                                        00640000
