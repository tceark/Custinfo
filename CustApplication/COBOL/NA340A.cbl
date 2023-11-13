   CBL DATA(24)                                                         00000100
900526*---------------------------------------------------------------* 00000200
900526* DATA(24) OPTION IS FOR ABOVE-TO-BELOW-THE-LINE DYNAMIC CALLS  * 00000300
900526*---------------------------------------------------------------* 00000400
       IDENTIFICATION DIVISION.                                         00000500
       PROGRAM-ID. NA340A.                                              00000601
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
      *  PROGRAM TITLE: NA340A                                         *00004701
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
R08986 01  WS-PROGRAM-NAME             PIC X(8) VALUE 'NA340A  '.       00023901
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
Y2KIMR     COPY EDITCODE.                                               00035801
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
           05  WS-CICS-RESP2                PIC S9(4) COMP.             00041901
           05  WS-CUSTOMER-STATUS           PIC X     VALUE SPACES.     00042001
               88  INVALID-CUSTOMER-STATUS            VALUE 'D' 'C'.    00042101
           05  WS-TS-QUEUE-NAME.                                        00042201
               10  WS-TS-QUEUE-TRANID       PIC X(4).                   00042301
               10  WS-TS-QUEUE-TERMID       PIC X(4).                   00042401
900526     05  WS-REQID-NAME.                                           00042501
900526         10  WS-R-TERMID                   PIC X(4).              00042601
900526         10  FILLER                        PIC X(4)  VALUE 'BRSE'.00042701
           05  WS-TS-ITEM                   PIC S9(4) VALUE +1   COMP.  00042801
           05  WS-HEX80                     PIC S9(4) VALUE +128 COMP.  00042901
           05  WS-HEX80-REDEF REDEFINES WS-HEX80.                       00043001
               10  FILLER                   PIC X.                      00043101
               10  HEX80                    PIC X.                      00043201
           05  WS-NULL-FIELD                PIC X(72).                  00043301
           05  WS-ERROR-FIELDS.                                         00043401
               10  WS-C9999-ERROR-CODE      PIC X(5).                   00043501
               10  WS-C9999-SEVERITY-CODE   PIC X.                      00043601
COBOLU**       10  FILLER                   PIC X.                      00043701
COBOLU         10  WS-C9999-FILLER          PIC X.                      00043801
               10  WS-C9999-ERROR-MESSAGE   PIC X(30).                  00043901
           05  WS-SUBCRIPT COMP.                                        00044001
               10  ACTION-SUB               PIC S99.                    00044101
           05  WS-HOLD-MESSAGE              PIC X(5)   VALUE SPACE.     00044201
MANJU      05  WS-DB2I-MESSAGE              PIC ZZZZ9.                  00044301
           05  LOWER-CASE                   PIC X(26)                   00044801
                                   VALUE 'abcdefghijklmnopqrstuvwxyz'.  00044901
           05  UPPER-CASE                   PIC X(26)                   00045001
                                   VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.  00045101
           05  WS-SYSTEM-CODE.                                          00045201
               10  WS-SYSTEM-CODE-2BYTES    PIC X(2).                   00045301
               10  FILLER                   PIC X(2).                   00045401
           05  WS-ACTION-CODE.                                          00045501
               10  WS-ACTION-CODE-5BYTES    PIC X(5).                   00045601
               10  FILLER                   PIC X.                      00045701
           05  WS-DATE-R                    PIC 9(6)   VALUE ZERO.      00045801
           05  WS-DATE.                                                 00045901
               10  WS-MM                    PIC 99     VALUE ZERO.      00046001
               10  WS-DD                    PIC 99     VALUE ZERO.      00046101
               10  WS-YY                    PIC 99     VALUE ZERO.      00046201
           05  WS-DATE-A REDEFINES WS-DATE.                             00046301
               10  WS-MM-A                  PIC XX.                     00046401
               10  WS-DD-A                  PIC XX.                     00046501
               10  WS-YY-A                  PIC XX.                     00046601
           05  WS-BIRTH-DATE.                                           00046701
               10  BR-CC                    PIC XX.                     00046801
               10  BR-YY                    PIC XX.                     00046901
               10  FILLER                   PIC X      VALUE '-'.       00047001
               10  BR-MM                    PIC XX.                     00047101
               10  FILLER                   PIC X      VALUE '-'.       00047201
               10  BR-DD                    PIC XX.                     00047301
           05  WS-EFFECTIVE-DATE.                                       00047401
               10  EF-CC                    PIC XX.                     00047501
               10  EF-YY                    PIC XX.                     00047601
               10  FILLER                   PIC X      VALUE '-'.       00047701
               10  EF-MM                    PIC XX.                     00047801
               10  FILLER                   PIC X      VALUE '-'.       00047901
               10  EF-DD                    PIC XX.                     00048001
           05  WS-CHANGE-DATE.                                          00048101
               10  CO-CC                    PIC XX.                     00048201
               10  CO-YY                    PIC XX.                     00048301
               10  FILLER                   PIC X      VALUE '-'.       00048401
               10  CO-MM                    PIC XX.                     00048501
               10  FILLER                   PIC X      VALUE '-'.       00048601
               10  CO-DD                    PIC XX.                     00048701
           05  WS-DATE-EIGHT.                                           00048801
               10  WS-MONTH                 PIC 99     VALUE ZERO.      00048901
               10  SLASH-1                  PIC X.                      00049001
               10  WS-DAY                   PIC 99     VALUE ZERO.      00049101
               10  SLASH-2                  PIC X.                      00049201
               10  WS-YEAR                  PIC 99     VALUE ZERO.      00049301
           05  WS-COMPARE-DATE.                                         00049401
               10  WS-COMPARE-YY            PIC 99     VALUE ZERO.      00049501
               10  WS-COMPARE-MM            PIC 99     VALUE ZERO.      00049601
               10  WS-COMPARE-DD            PIC 99     VALUE ZERO.      00049701
           05  HOLD-NEXT-CIM                PIC 9(8) VALUE ZERO.        00049801
           05  HOLD-NEXT-CIM-R REDEFINES HOLD-NEXT-CIM                  00049901
                                            PIC X(8).                   00050001
                                                                        00050101
           05  FWAC-ADDR                         PIC S9(08)  COMP.      00050201
           05  FWAC-PNTR REDEFINES FWAC-ADDR USAGE IS POINTER.          00050301
DFJ        05  STATUS-CHANGED-SW            PIC X      VALUE 'N'.       00050401
DFJ        05  STATE-CHANGED-SW             PIC X      VALUE 'N'.       00050501
DFJ        05  ZIP-CHANGED-SW               PIC X      VALUE 'N'.       00050601
890989     05  ZIP-PLUS4-CHANGED-SW         PIC X      VALUE 'N'.       00050701
           05  PHONE-CHANGED-SW             PIC X.                      00050801
           05  DISPLAY-CHANGED-SW           PIC X.                      00050901
COBOLU     05  WS-APPLID                    PIC X(08).                  00051001
890989     05  WS-HOLD-CASE-STATE           PIC X(02).                  00051101
890989     05  WS-HOLD-CASE-ZIP             PIC 9(05).                  00051201
890989     05  WS-HOLD-CASE-ZIP-PLUS4       PIC 9(04).                  00051301
890989     05  WS-HOLD-CASE-NUM             PIC X(06).                  00051401
890989     05  WS-HOLD-CSN-STATE            PIC X(02).                  00051501
890989     05  WS-HOLD-CSN-ZIP              PIC X(05).                  00051601
890989     05  WS-HOLD-CSN-ZIP-PLUS4        PIC X(04).                  00051701
890989     05  WS-CASE-EMP-NUM.                                         00051801
890989         10  WS-CASE-NUM         PIC X(6).                        00051901
890989         10  WS-EMP-NUM          PIC 9(5).                        00052001
890989     05  WS-EMP-EOF                   PIC X(1) VALUE 'N'.         00052101
890989     05  WS-EMP-UPDATE                PIC X(1) VALUE 'N'.         00052201
           05  WS-CASE-AREA-PHONE.                                      00052301
               10  WS-CASE-AREA             PIC X(3).                   00052401
               10  WS-CASE-PHONE            PIC X(7).                   00052501
           05  WS-PHONE.                                                00052601
               10  WS-PHONE3                PIC X(3).                   00052701
               10  WS-PHONE4                PIC X(4).                   00052801
           05  WS-MAP-DATE.                                             00052901
               10  FILLER                   PIC 99.                     00053001
               10  WS-MAP-BIF               PIC 9(6).                   00053101
           05  WS-CIM-PHONE.                                            00053201
               10  WS-CIM-PHONE-AREA        PIC X(3).                   00053301
               10  WS-DASH1                 PIC X.                      00053401
               10  WS-CIM-PHONE-EXCH        PIC X(3).                   00053501
               10  WS-DASH2                 PIC X.                      00053601
               10  WS-CIM-PHONE-NUMB        PIC X(4).                   00053701
           05  WS-TIME-FIELD                PIC S9(8) COMP.             00053801
           05  WC-TODAYS-DATE.                                          00053901
               10  WC-TODAYS-MM             PIC XX.                     00054001
               10  FILLER                   PIC X.                      00054101
               10  WC-TODAYS-DD             PIC XX.                     00054201
               10  FILLER                   PIC X.                      00054301
               10  WC-TODAYS-YY             PIC XX.                     00054401
           05  WS-FINALST-REAS-CODE.                                    00054501
               10  WS-FINALST-BYTE1         PIC X.                      00054601
               10  FILLER                   PIC XX.                     00054701
           05  WS-DUPLICATE-SW              PIC X.                      00054901
           05  OPER-ID                      PIC X(3).                   00055001
           05  WS-NUMERIC-CHECK.                                        00055101
               10 WS-NUMERIC-CHECK-BYTE OCCURS 20 TIMES                 00055201
                                            PIC X.                      00055301
           05  WS-NUMERIC-SW                PIC X.                      00055401
R04023     05  ORIGINAL-STATE               PIC XX.                     00055501
R02539     05  POSTAL-CODE                  PIC X(9).                   00055601
R02539     05  COUNTRY-CODE                 PIC X(3).                   00055701
R02539     05  IND-POSTAL-CODE              PIC S9(4) COMP.             00055801
R02539     05  IND-COUNTRY-CODE             PIC S9(4) COMP.             00055901
           05  WS-BROKER-COMMAREA.                                      00056001
               10  WS-AGENT-COMMAREA        PIC X(600).                 00056101
MAB            10  WS-BROKER-FIELDS         PIC X(121).                 00056201
               10  WS-SYSTEM-ADD-TYPE       PIC XX.                     00056301
           05  WS-STATE-ZIP.                                            00056401
               10  WS-STATE                 PIC X(2).                   00056501
               10  FILLER                   PIC X VALUE SPACE.          00056601
               10  WS-ZIP                   PIC X(5).                   00056701
           05  WS-DB2-DATE.                                             00056801
               10  WS-DB2-CC                PIC X(2).                   00056901
Y2KIMR*                                                                 00057001
Y2KIMR* IMR CHANGE BEGIN                                                00057101
Y2KIMR*                                                                 00057201
Y2KIMR         10  WS-DB2-CC-R REDEFINES WS-DB2-CC                      00057301
Y2KIMR                                      PIC 99.                     00057401
Y2KIMR*                                                                 00057501
Y2KIMR* IMR CHANGE END                                                  00057601
Y2KIMR*                                                                 00057701
               10  WS-DB2-YY                PIC X(2).                   00057801
               10  WS-DB2-YY-R REDEFINES WS-DB2-YY                      00057901
                                            PIC 99.                     00058001
               10  FILLER                   PIC X.                      00058101
               10  WS-DB2-MM                PIC X(2).                   00058201
               10  WS-DB2-MM-R REDEFINES WS-DB2-MM                      00058301
                                            PIC 99.                     00058401
               10  FILLER                   PIC X.                      00058501
               10  WS-DB2-DD                PIC X(2).                   00058601
               10  WS-DB2-DD-R REDEFINES WS-DB2-DD                      00058701
                                            PIC 99.                     00058801
           05  WS-DB2-TIMESTAMP.                                        00058901
               10  WS-TS-CC                 PIC X(2).                   00059001
               10  WS-TS-YY                 PIC X(2).                   00059101
               10  WS-TS-YY-R REDEFINES WS-TS-YY                        00059201
                                            PIC 99.                     00059301
               10  FILLER                   PIC X.                      00059401
               10  WS-TS-MM                 PIC X(2).                   00059501
               10  WS-TS-MM-R REDEFINES WS-TS-MM                        00059601
                                            PIC 99.                     00059701
               10  FILLER                   PIC X.                      00059801
               10  WS-TS-DD                 PIC X(2).                   00059901
               10  WS-TS-DD-R REDEFINES WS-TS-DD                        00060001
                                            PIC 99.                     00060101
               10  FILLER                   PIC X(16).                  00060201
           05  WS-COMPANY-NAME.                                         00060401
               10  WS-COMPANY-NAME-HIGH-ORDR   PIC X(22) VALUE SPACES.  00060501
               10  WS-COMPANY-NAME-LOW-ORDR    PIC X(08) VALUE SPACES.  00060601
           05  WS-ENTITY-LITERAL.                                       00060701
               10  WS-LITERAL-HIGH-ORDR        PIC X(02) VALUE SPACES.  00060801
C08784****     10  WS-LITERAL-LOW-ORDR         PIC X(13) VALUE SPACES.  00060901
C08784         10  WS-LITERAL-LOW-ORDR         PIC X(08) VALUE SPACES.  00061001
           05  WS-C                         PIC X(1) VALUE 'C'.         00061101
           05  WS-Y                         PIC X(1) VALUE 'Y'.         00061201
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
                                                                        00080200
       01  WS-INQ-COMMAREA.                                             00080301
           COPY INQCOMA.                                                00080401
                                                                        00080501
       01  LINK-INQ-COMM-LENGTH PIC S9(4) COMP VALUE +763.              00080601
                                                                        00080701
       01  WS-UPD-COMMAREA.                                             00080801
           COPY UPDCOMA.                                                00080901
                                                                        00081001
       01  LINK-UPD-COMM-LENGTH PIC S9(4) COMP VALUE +681.              00081101
                                                                        00081201
11735A*    COPY BROKRV01.                                               00081301
11735A*    COPY BROKER.                                                 00081401
900837*    COPY AGNTNV03.                                               00081501
R00700     COPY AGNTNV05.                                               00081601
900837     COPY CASNVW3.                                                00081701
DFJ   *    COPY CAMASV01.                                               00081801
11735A     COPY CATMASTE.                                               00081901
900600*    COPY CASEXV01.                                               00082001
MANJU *    COPY CASEMAST  REPLACING CASE- BY CASEX-.                    00082101
890989*    COPY EMPLV03.                                                00082201
11735A*    COPY EMPLOYEE.                                               00082301
R03749*    COPY COVERAGE.                                               00082401
                                                                        00082501
R8285A     COPY COVEDITS.                                               00082601
                                                                        00082701
R08986     COPY RULEWS1.                                                00082802
           COPY STATECON.                                               00082901
R08986                                                                  00083001
           EXEC SQL                                                     00083101
              INCLUDE SQLCA                                             00083201
           END-EXEC.                                                    00083301
           05  WS-SQL-ERROR-MESSAGE         PIC X(78).                  00083401
                                                                        00105000
       01  WS-NAME-ADDRESS-DETAIL.                                      00105100
           COPY NA200C02.                                               00105200
       01  SECURITY-AREA.                                               00105300
           COPY SECCOMMC.                                               00105400
           COPY SYSBUSY.                                                00105500
           COPY EI100C01.                                               00105600
           COPY NA340M2.                                                00105701
           COPY DFHAID.                                                 00105800
           COPY ATTRB.                                                  00105900
                                                                        00106000
                                                                        00106100
       LINKAGE SECTION.                                                 00106200
                                                                        00106301
       01  DFHCOMMAREA                      PIC X.                      00106400
                                                                        00106500
      ******************************************************************00108000
                                                                        00108100
       PROCEDURE DIVISION.                                              00108200
                                                                        00108300
       0010-BEGIN-PROGRAM.                                              00108400
           DISPLAY 'NA340A 0010-BEGIN-PROGRAM'.                         00108501
                                                                        00108601
           EXEC CICS HANDLE CONDITION ERROR(9999-CICS-ERROR)            00108700
                                      END-EXEC.                         00108800
                                                                        00108900
           MOVE LOW-VALUES TO NA340M2O SYSBUSYO.                        00109001
C08784     INITIALIZE WS-C9999-FILLER IO-RETURN WS-ENTITY-LITERAL.      00109100
COBOLU     INITIALIZE  NA-COMM-DISPLAY-NAME                             00109200
T3384                  NA-COMM-TITLE                                    00109300
COBOLU                 NA-COMM-RECORD-NUMBER                            00109400
COBOLU                 NA-COMM-SEARCH-FIELD                             00109500
COBOLU                 NA-COMM-PERSONAL-NAME                            00109600
COBOLU                 NA-COMM-COMPANY-NAME                             00109700
COBOLU                 NA-COMM-COMPANY-MAIL                             00109800
COBOLU                 NA-COMM-BIRTH-MONTH                              00109900
COBOLU                 NA-COMM-SSN                                      00110000
COBOLU                 NA-COMM-ADDRESS-1                                00110100
COBOLU                 NA-COMM-ADDRESS-2                                00110200
COBOLU                 NA-COMM-ADDRESS-3                                00110300
COBOLU                 NA-COMM-CITY                                     00110400
COBOLU                 NA-COMM-STATE                                    00110500
COBOLU                 NA-COMM-ZIP                                      00110600
COBOLU                 NA-COMM-ZIP-PLUS4                                00110700
COBOLU                 NA-COMM-PHONE-NUMBER                             00110800
COBOLU                 NA-COMM-POSTAL-CODE                              00110900
COBOLU                 NA-COMM-COUNTY-CODE                              00111000
COBOLU                 NA-COMM-ASSOC-CODE                               00111100
COBOLU                 NA-COMM-SITE-CODE                                00111200
COBOLU                 NA-COMM-SEX                                      00111300
COBOLU                 NA-COMM-CREATION-DATE                            00111400
COBOLU                 NA-COMM-EFFECTIVE-DATE                           00111500
COBOLU                 NA-COMM-LAST-CHG-DATE                            00111600
COBOLU                 NA-COMM-STATUS                                   00111700
COBOLU                 NA-COMM-MAIL-LINE1                               00111800
COBOLU                 NA-COMM-MAIL-LINE2                               00111900
COBOLU                 NA-COMM-MAIL-LINE3                               00112000
COBOLU                 NA-COMM-MAIL-LINE4                               00112100
COBOLU                 NA-COMM-MAIL-LINE5                               00112200
COBOLU                 NA-COMM-MAIL-LINE6                               00112300
COBOLU                 NA-COMM-LINE(1)                                  00112400
COBOLU                 NA-COMM-LINE(2)                                  00112500
COBOLU                 NA-COMM-LINE(3)                                  00112600
COBOLU                 NA-COMM-LINE(4)                                  00112700
COBOLU                 NA-COMM-LINE(5)                                  00112800
COBOLU                 NA-COMM-LINE(6).                                 00112900
                                                                        00113000
COBOLU     INITIALIZE COM-AGNTNAME.                                     00118200
                                                                        00118900
           MOVE 'NA'      TO   COMM-SYSTEM-CODE.                        00119001
           MOVE 'U'       TO   COMM-ACTION-CODE.                        00119101
                                                                        00119201
       0060-PROCESS-MENU-SCREEN.                                        00124100
MANJU       DISPLAY 'INSIDE UPDATE NA340 PROCESS MENU PARA'             00124200
           DISPLAY 'EIBCALEN:'  EIBCALEN.                               00124301
                                                                        00124601
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00124701
           OR  EIBTRNID = 'HCS2'                                        00124801
               MOVE COMM-MSG-ID(1) TO WS-MESSAGE-NUMBER1                00124901
               MOVE COMM-MSG-ID(2) TO WS-MESSAGE-NUMBER2                00125001
               MOVE COMM-MSG-ID(3) TO WS-MESSAGE-NUMBER3                00125101
               MOVE COMM-MSG-ID(4) TO WS-MESSAGE-NUMBER4                00125201
               GO TO 0200-SEND-NA340M2-MAP                              00125301
           ELSE                                                         00125500
           IF EIBCALEN = ZERO                                           00126101
               DISPLAY 'INITIAL SCREEN:'                                00126201
               GO TO 0180-SEND-NA340M2-FIRST-TIME                       00126301
           ELSE                                                         00126401
           IF  EIBAID = DFHENTER                                        00126901
               DISPLAY 'INITIAL ENTERN:'                                00127001
               GO TO 0070-EDIT-COMMAND-LINE                             00127301
           ELSE                                                         00127701
           IF  EIBAID = DFHCLEAR                                        00127801
               DISPLAY 'INITIAL CLEAR:'                                 00127901
               GO TO 0340-PROCESS-SCREEN-CLEAR                          00128001
           ELSE                                                         00128101
           IF  EIBAID = DFHPF3                                          00128501
               DISPLAY 'INITIAL PF3:'                                   00128601
               GO TO 0100-RETURN-TO-BROWSE                              00128701
           ELSE                                                         00128801
               DISPLAY 'WRONG KEY'                                      00128901
               GO TO 0360-WRONG-KEY-HIT.                                00129001
                                                                        00129101
       0070-EDIT-COMMAND-LINE.                                          00129201
                                                                        00129301
COBOLU     MOVE LOW-VALUES TO NA340M2I.                                 00129401
                                                                        00129501
           EXEC CICS RECEIVE MAP    ('NA340M2')                         00129601
                             RESP   (WS-CICS-RESP)                      00129701
                             END-EXEC.                                  00129801
                                                                        00129901
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00130001
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00130101
                GO TO 9999-CICS-ERROR.                                  00130201
                                                                        00130301
           DISPLAY 'MAP-SITE-CODEL:' MAP-SITE-CODEL                     00130401
           DISPLAY 'MAP-SITE-CODEI:' MAP-SITE-CODEI                     00130501
           DISPLAY 'MAP-CUST-INFOL:' MAP-CUST-INFOL                     00130601
           DISPLAY 'MAP-CIML:'       MAP-CIML                           00130701
           DISPLAY 'MAP-CIMI:'       MAP-CIMI                           00130801
                                                                        00130901
      *    PERFORM 0110-UPDATE-COMMAND-LINE.                            00131001
           IF MAP-CUST-INFOL > 0                                        00131101
              MOVE MAP-CUST-INFOI(1:8)    TO WS-CIM-NUMBER              00131201
              GO TO 0190-INQUIRE-PARA                                   00131301
           ELSE                                                         00131401
             IF MAP-CIMI > 0                                            00131501
                MOVE MAP-CIMI             TO WS-CIM-NUMBER              00131801
                                             COM-CIM                    00131901
                MOVE ALL '_'              TO COMM-CUSTOMER-INFO         00132001
                MOVE WS-CIM-NUMBER        TO COMM-CUSTOMER-INFO(1:8)    00132101
                PERFORM 0120-UPDATE-CIM-MAP-FIELDS                      00132201
                PERFORM 0130-EDIT-NA340M2-MAP                           00132301
                GO TO 0200-SEND-NA340M2-MAP                             00132401
             END-IF                                                     00132501
           END-IF.                                                      00132601
                                                                        00133001
       0100-RETURN-TO-BROWSE.                                           00135401
                                                                        00138101
           EXEC CICS START TRANSID('NAM1')                              00138201
                           TERMID (EIBTRMID)                            00138301
                           RESP   (WS-CICS-RESP)                        00138401
                           END-EXEC                                     00138501
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00138601
                GO TO 9999-CICS-ERROR.                                  00138701
                                                                        00138801
           EXEC CICS RETURN                                             00138901
                     END-EXEC.                                          00139001
                                                                        00139101
       0110-UPDATE-COMMAND-LINE.                                        00139201
                                                                        00139301
           IF MAP-SYSTEM-CDF = HEX80                                    00139401
               MOVE SPACES TO COMM-SYSTEM-CODE                          00139501
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED                      00139601
           ELSE                                                         00139701
               IF MAP-SYSTEM-CDL > ZERO                                 00139801
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED           00139901
                  MOVE MAP-SYSTEM-CDI TO COMM-SYSTEM-CODE.              00140001
                                                                        00140101
           IF MAP-ACTION-CDF = HEX80                                    00140201
               MOVE SPACES TO COMM-ACTION-CODE                          00140301
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED                      00140401
           ELSE                                                         00140501
               IF MAP-ACTION-CDL > ZERO                                 00140601
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED           00140701
                  MOVE MAP-ACTION-CDI TO COMM-ACTION-CODE.              00140801
                                                                        00140901
           IF MAP-CUST-INFOF = HEX80                                    00141001
               MOVE SPACES TO COMM-CUSTOMER-INFO                        00141101
               MOVE 'Y'            TO COMM-KEY-CHANGED                  00141201
           ELSE                                                         00141301
               IF MAP-CUST-INFOL > ZERO                                 00141401
                  MOVE 'Y'            TO COMM-KEY-CHANGED               00141501
                  MOVE MAP-CUST-INFOI TO COMM-CUSTOMER-INFO.            00141601
                                                                        00141701
                                                                        00141801
       0120-UPDATE-CIM-MAP-FIELDS.                                      00141901
           DISPLAY '0120-UPDATE-CIM-MAP-FIELDS PARA'                    00142001
           IF MAP-FIRST-NAMEF = HEX80                                   00142101
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00142201
               MOVE SPACES TO COM-FIRST-NAME                            00142301
           ELSE                                                         00142401
               IF MAP-FIRST-NAMEL > ZERO                                00142501
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00142601
                  MOVE MAP-FIRST-NAMEI TO COM-FIRST-NAME.               00142701
                                                                        00142801
           IF MAP-MIDDLE-NAMEF = HEX80                                  00142901
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00143001
               MOVE SPACES TO COM-MIDDLE-NAME                           00143101
           ELSE                                                         00143201
               IF MAP-MIDDLE-NAMEL > ZERO                               00143301
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00143401
                  MOVE MAP-MIDDLE-NAMEI TO COM-MIDDLE-NAME.             00143501
                                                                        00143601
           IF MAP-LAST-NAMEF = HEX80                                    00143701
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00143801
               MOVE SPACES TO COM-LAST-NAME                             00143901
           ELSE                                                         00144001
               IF MAP-LAST-NAMEL > ZERO                                 00144101
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00144201
                  MOVE MAP-LAST-NAMEI TO COM-LAST-NAME.                 00144301
                                                                        00144401
           IF MAP-SUFFIX1F = HEX80                                      00144501
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00144601
               MOVE SPACES TO COM-SUFFIX1                               00144701
           ELSE                                                         00144801
               IF MAP-SUFFIX1L > ZERO                                   00144901
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00145001
                  MOVE MAP-SUFFIX1I TO COM-SUFFIX1.                     00145101
                                                                        00145201
           IF MAP-SUFFIX2F = HEX80                                      00145301
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00145401
               MOVE SPACES TO COM-SUFFIX2                               00145501
           ELSE                                                         00145601
               IF MAP-SUFFIX2L > ZERO                                   00145701
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00145801
                  MOVE MAP-SUFFIX2I TO COM-SUFFIX2.                     00145901
                                                                        00146001
           IF MAP-NICKNAMEF = HEX80                                     00146101
               MOVE SPACES TO COM-NICKNAME                              00146201
           ELSE                                                         00146301
               IF MAP-NICKNAMEL > ZERO                                  00146401
                  MOVE MAP-NICKNAMEI TO COM-NICKNAME.                   00146501
                                                                        00146601
R04023     IF MAP-ISSUE-STATEF = HEX80                                  00146701
R04023         MOVE SPACES TO COM-ISSUE-STATE                           00146801
R04023     ELSE                                                         00146901
R04023         IF MAP-ISSUE-STATEL > ZERO                               00147001
R04023            MOVE MAP-ISSUE-STATEI TO COM-ISSUE-STATE.             00147101
                                                                        00147201
      *---------------------------------------------------------------* 00147301
      *    SPECIAL CODING TO INSURE THE COMPANY IN ADDRESS FLAG IS    * 00147401
      *    SET TO YES IF THE COMPANY NAME WAS BLANK PRIOR TO THE      * 00147501
      *    UPDATE.                                                    * 00147601
      *---------------------------------------------------------------* 00147701
                                                                        00147801
      *    IF COM-COMPANY-NAME = SPACES AND MAP-COMP-NAMEI > SPACE      00147901
      *        AND MAP-COMP-IN-ADDL NOT > ZERO                          00148001
      *             MOVE 'Y' TO COM-COMPANY-IN-ADDRESS.                 00148101
                                                                        00148201
           IF MAP-COMP-NAMEF = HEX80                                    00148301
               MOVE 'Y' TO DISPLAY-CHANGED-SW                           00148401
               MOVE SPACES TO COM-COMPANY-NAME                          00148501
           ELSE                                                         00148601
               IF MAP-COMP-NAMEL > ZERO                                 00148701
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00148801
                  MOVE MAP-COMP-NAMEI TO COM-COMPANY-NAME               00148901
                                         WS-COMPANY-NAME.               00149001
                                                                        00149101
      *---------------------------------------------------------------* 00149201
      *    ALTERED SEX CODE CAN CHANGE DEFAULT PREFIX ON DISPLAY NAME * 00149301
      *---------------------------------------------------------------* 00149401
           IF MAP-SEXF = HEX80                                          00149501
               MOVE SPACES TO COM-SEX                                   00149601
           ELSE                                                         00149701
               IF MAP-SEXL > ZERO                                       00149801
                  MOVE 'Y' TO DISPLAY-CHANGED-SW                        00149901
                  MOVE MAP-SEXI TO COM-SEX.                             00150001
                                                                        00150101
                                                                        00150201
T3384      IF MAP-PREFIXF = HEX80                                       00150301
T3384          MOVE SPACES TO COM-PREFIX                                00150401
T3384      ELSE                                                         00150501
T3384          IF MAP-PREFIXL > ZERO                                    00150601
T3384             MOVE 'Y' TO DISPLAY-CHANGED-SW                        00150701
T3384             MOVE MAP-PREFIXI TO COM-PREFIX.                       00150801
                                                                        00150901
           IF DISPLAY-CHANGED-SW = 'Y'                                  00151001
               PERFORM 0300-FORMAT-DISPLAY-NAME.                        00151101
                                                                        00151201
           IF MAP-ADDRESS1F = HEX80                                     00151301
               MOVE SPACES TO COM-ADDRESS1                              00151401
           ELSE                                                         00151501
               IF MAP-ADDRESS1L > ZERO                                  00151601
                  MOVE MAP-ADDRESS1I TO COM-ADDRESS1.                   00151701
                                                                        00151801
           IF MAP-ADDRESS2F = HEX80                                     00151901
               MOVE SPACES TO COM-ADDRESS2                              00152001
           ELSE                                                         00152101
               IF MAP-ADDRESS2L > ZERO                                  00152201
                  MOVE MAP-ADDRESS2I TO COM-ADDRESS2.                   00152301
                                                                        00152401
           IF MAP-BIRTH-DATEF = HEX80                                   00152501
               MOVE ZEROS TO COM-BIRTH-DATE                             00152601
Y2KIMR*                                                                 00152701
Y2KIMR* IMR CHANGE IMR6 BEGIN                                           00152801
Y2KIMR*                                                                 00152901
Y2KIMR      MOVE ZEROS TO Y2K-WK-VARIABLE1                              00153001
Y2KIMR*                                                                 00153101
Y2KIMR* IMR CHANGE IMR6 END                                             00153201
Y2KIMR*                                                                 00153301
           ELSE                                                         00153401
               IF MAP-BIRTH-DATEL > ZERO                                00153501
                  INSPECT MAP-BIRTH-DATEI REPLACING ALL '-' BY 'A'      00153601
Y2KIMR*                                                                 00153701
Y2KIMR* IMR DATE ROUTINE CHANGE BEGIN                                   00153801
Y2KIMR*                                                                 00153901
Y2KIMR*           EXEC CICS BIF DEEDIT FIELD(MAP-BIRTH-DATEI)           00154001
Y2KIMR*                                LENGTH(8)                        00154101
Y2KIMR*                                END-EXEC                         00154201
Y2KIMR*           MOVE MAP-BIRTH-DATEI TO WS-MAP-DATE                   00154301
Y2KIMR*           MOVE WS-MAP-BIF      TO COM-BIRTH-DATE.               00154401
Y2KIMR*                                                                 00154501
MANJU             EXEC CICS BIF DEEDIT FIELD(MAP-BIRTH-DATEI)           00154601
Y2KIMR                                 LENGTH(10)                       00154701
Y2KIMR                                 END-EXEC                         00154801
                  DISPLAY 'MAP-BIRTH-DATEI' MAP-BIRTH-DATEI.            00154901
Y2KIMR            MOVE MAP-BIRTH-DATEI TO Y2K-WK-VARIABLE1              00155001
Y2KIMR*           MOVE Y2K-WK-VARIABLE2-MM TO Y2K-WK-VARIABLE3-BB-MM    00155101
Y2KIMR*           MOVE Y2K-WK-VARIABLE2-DD TO Y2K-WK-VARIABLE3-BB-DD    00155201
Y2KIMR*           MOVE Y2K-WK-VARIABLE2-YY TO Y2K-WK-VARIABLE3-BB-YY    00155301
Y2KIMR*           MOVE Y2K-WK-VARIABLE3 TO WS-MAP-DATE                  00155401
Y2KIMR            MOVE Y2K-WK-VARIABLE2 TO COM-BIRTH-DATE .             00155501
Y2KIMR*                                                                 00155601
Y2KIMR* IMR DATE ROUTINE CHANGE END                                     00155701
Y2KIMR*                                                                 00155801
                                                                        00155901
           IF MAP-SSNF = HEX80                                          00156001
               MOVE SPACES TO COM-SSN                                   00156101
           ELSE                                                         00156201
               IF MAP-SSNL > ZERO                                       00156301
                  MOVE MAP-SSNI TO COM-SSN.                             00156401
                                                                        00156501
           IF MAP-CITYF = HEX80                                         00156601
               MOVE SPACES TO COM-CITY                                  00156701
           ELSE                                                         00156801
               IF MAP-CITYL > ZERO                                      00156901
                  MOVE MAP-CITYI TO COM-CITY.                           00157001
                                                                        00157101
           IF MAP-COMP-IN-ADDF = HEX80                                  00157201
           OR MAP-COMP-IN-ADDI = SPACES                                 00157301
               MOVE 'Y' TO COM-COMPANY-IN-ADDRESS                       00157401
           ELSE                                                         00157501
               IF MAP-COMP-IN-ADDL > ZERO                               00157601
                  MOVE MAP-COMP-IN-ADDI TO COM-COMPANY-IN-ADDRESS       00157701
               ELSE                                                     00157801
                  MOVE 'Y' TO COM-COMPANY-IN-ADDRESS.                   00157901
                                                                        00158001
           IF MAP-STATEF = HEX80                                        00158101
DFJ            MOVE 'Y' TO STATE-CHANGED-SW                             00158201
               MOVE SPACES TO COM-STATE                                 00158301
           ELSE                                                         00158401
               IF MAP-STATEL > ZERO                                     00158501
DFJ               MOVE 'Y' TO STATE-CHANGED-SW                          00158601
                  MOVE MAP-STATEI TO COM-STATE.                         00158701
                                                                        00158801
           IF MAP-ZIPF = HEX80                                          00158901
DFJ            MOVE 'Y' TO ZIP-CHANGED-SW                               00159001
               MOVE SPACES TO COM-ZIP                                   00159101
           ELSE                                                         00159201
               IF MAP-ZIPL > ZERO                                       00159301
DFJ               MOVE 'Y' TO ZIP-CHANGED-SW                            00159401
                  MOVE MAP-ZIPI TO COM-ZIP.                             00159501
                                                                        00159601
           IF MAP-ZIP-PLUS4F = HEX80                                    00159701
890989         MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW                         00159801
               MOVE SPACES TO COM-ZIP-PLUS4                             00159901
           ELSE                                                         00160001
               IF MAP-ZIP-PLUS4L > ZERO                                 00160101
890989            MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW                      00160201
                  MOVE MAP-ZIP-PLUS4I TO COM-ZIP-PLUS4.                 00160301
                                                                        00160401
           IF MAP-ADDR-OVRIDEF = HEX80                                  00160501
               MOVE SPACES TO COM-FINALST-OVRD-IND                      00160601
           ELSE                                                         00160701
               IF MAP-ADDR-OVRIDEL > ZERO                               00160801
                  MOVE MAP-ADDR-OVRIDEI TO COM-FINALST-OVRD-IND.        00160901
                                                                        00161001
           MOVE 'N' TO COM-DUP-ADDR-OVRD-IND.                           00161501
                                                                        00161601
           IF MAP-AREA-CODEF = HEX80                                    00161701
               MOVE SPACES TO COM-AREA-CODE                             00161801
           ELSE                                                         00161901
               IF MAP-AREA-CODEL > ZERO                                 00162001
                  MOVE MAP-AREA-CODEI TO COM-AREA-CODE.                 00162101
                                                                        00162201
900837     MOVE 'N' TO PHONE-CHANGED-SW.                                00162301
           MOVE COM-PHONE TO WS-PHONE.                                  00162401
                                                                        00162501
           IF MAP-PHONE3F = HEX80                                       00162601
               MOVE 'Y' TO PHONE-CHANGED-SW                             00162701
               MOVE SPACES TO WS-PHONE3                                 00162801
           ELSE                                                         00162901
               IF MAP-PHONE3L > ZERO                                    00163001
                  MOVE 'Y' TO PHONE-CHANGED-SW                          00163101
                  MOVE MAP-PHONE3I TO WS-PHONE3.                        00163201
                                                                        00163301
           IF MAP-PHONE4F = HEX80                                       00163401
               MOVE 'Y' TO PHONE-CHANGED-SW                             00163501
               MOVE SPACES TO WS-PHONE4                                 00163601
           ELSE                                                         00163701
               IF MAP-PHONE4L > ZERO                                    00163801
                  MOVE 'Y' TO PHONE-CHANGED-SW                          00163901
                  MOVE MAP-PHONE4I TO WS-PHONE4.                        00164001
                                                                        00164101
           IF PHONE-CHANGED-SW = 'Y'                                    00164201
               MOVE WS-PHONE TO COM-PHONE.                              00164301
                                                                        00164401
           IF MAP-PHONE-EXTF = HEX80                                    00164501
               MOVE SPACES TO COM-PHONE-EXTENSION                       00164601
           ELSE                                                         00164701
               IF MAP-PHONE-EXTL > ZERO                                 00164801
                  MOVE MAP-PHONE-EXTI TO COM-PHONE-EXTENSION.           00164901
                                                                        00165001
900837     IF MAP-FAX-AREA-CDF = HEX80                                  00165101
900837         MOVE SPACES TO COM-FAX-AREA-CODE                         00165201
900837     ELSE                                                         00165301
900837         IF MAP-FAX-AREA-CDL > ZERO                               00165401
900837            MOVE MAP-FAX-AREA-CDI TO COM-FAX-AREA-CODE.           00165501
900837                                                                  00165601
900837     MOVE 'N' TO PHONE-CHANGED-SW.                                00165701
900837     MOVE COM-FAX-PHONE TO WS-PHONE.                              00165801
900837                                                                  00165901
900837     IF MAP-FAX-PHONE3F = HEX80                                   00166001
900837         MOVE 'Y' TO PHONE-CHANGED-SW                             00166101
900837         MOVE SPACES TO WS-PHONE3                                 00166201
900837     ELSE                                                         00166301
900837         IF MAP-FAX-PHONE3L > ZERO                                00166401
900837            MOVE 'Y' TO PHONE-CHANGED-SW                          00166501
900837            MOVE MAP-FAX-PHONE3I TO WS-PHONE3.                    00166601
900837                                                                  00166701
900837     IF MAP-FAX-PHONE4F = HEX80                                   00166801
900837         MOVE 'Y' TO PHONE-CHANGED-SW                             00166901
900837         MOVE SPACES TO WS-PHONE4                                 00167001
900837     ELSE                                                         00167101
900837         IF MAP-FAX-PHONE4L > ZERO                                00167201
900837            MOVE 'Y' TO PHONE-CHANGED-SW                          00167301
900837            MOVE MAP-FAX-PHONE4I TO WS-PHONE4.                    00167401
900837                                                                  00167501
900837     IF PHONE-CHANGED-SW = 'Y'                                    00167601
900837         MOVE WS-PHONE TO COM-FAX-PHONE.                          00167701
                                                                        00167801
           IF MAP-ALT-ADDRF = HEX80                                     00167901
               MOVE SPACES TO COM-ALT-ADDRESS-IND                       00168001
           ELSE                                                         00168101
               IF MAP-ALT-ADDRL  > ZERO                                 00168201
                  MOVE MAP-ALT-ADDRI  TO COM-ALT-ADDRESS-IND.           00168301
                                                                        00168401
           IF MAP-ENTITY-TYPEF = HEX80                                  00168501
               MOVE SPACES TO COM-ENTITY-TYPE COM-ENTITY-LITERAL        00168601
           ELSE                                                         00168701
               IF MAP-ENTITY-TYPEL  > ZERO                              00168801
                  MOVE MAP-ENTITY-TYPEI  TO COM-ENTITY-TYPE             00168901
                                            COM-ENTITY-LITERAL.         00169001
MANJU2      DISPLAY 'COM-ENTITY-TYPE' COM-ENTITY-TYPE.                  00169101
R00700     IF MAP-EMAIL1F = HEX80                                       00169201
R00700         MOVE SPACES TO COM-EMAIL1                                00169301
R00700     ELSE                                                         00169401
R00700         IF MAP-EMAIL1L  > ZERO                                   00169501
R00700            MOVE MAP-EMAIL1I  TO COM-EMAIL1.                      00169601
                                                                        00169701
                                                                        00169801
           IF MAP-DIS-CHG-INDF = HEX80                                  00169901
               MOVE SPACES TO COM-DISP-IND                              00170001
           ELSE                                                         00170101
               IF MAP-DIS-CHG-INDL  > ZERO                              00170201
                  MOVE MAP-DIS-CHG-INDI  TO COM-DISP-IND                00170301
               ELSE                                                     00170401
                  MOVE 'Y'               TO COM-DISP-IND.               00170501
                                                                        00170601
           IF MAP-FUTURE-ADDRF = HEX80                                  00170701
               MOVE SPACES TO COM-FUTURE-ADDRESS-IND                    00170801
           ELSE                                                         00170901
               IF MAP-FUTURE-ADDRL  > ZERO                              00171001
                  MOVE MAP-FUTURE-ADDRI  TO COM-FUTURE-ADDRESS-IND.     00171101
                                                                        00171201
           IF MAP-CUST-STATUSF = HEX80                                  00171301
               MOVE SPACES TO COM-RECORD-STATUS                         00171401
           ELSE                                                         00171501
               IF MAP-CUST-STATUSL  > ZERO                              00171601
                  MOVE MAP-CUST-STATUSI TO COM-RECORD-STATUS            00171701
DFJ               IF COM-RECORD-STATUS = 'C'                            00171801
DFJ                   MOVE 'Y' TO STATUS-CHANGED-SW.                    00171901
                                                                        00172001
           IF MAP-EFF-DATEF = HEX80                                     00172101
               MOVE ZEROS TO COM-EFFECTIVE-DATE                         00172201
           ELSE                                                         00172301
               IF MAP-EFF-DATEL  > ZERO                                 00172401
                  INSPECT MAP-EFF-DATEI REPLACING ALL '-' BY 'A'        00172501
                  EXEC CICS BIF DEEDIT FIELD(MAP-EFF-DATEI)             00172601
                                       LENGTH(8)                        00172701
                                       END-EXEC                         00172801
                  MOVE MAP-EFF-DATEI TO WS-MAP-DATE                     00172901
                  MOVE WS-MAP-BIF    TO COM-EFFECTIVE-DATE.             00173001
                                                                        00173101
                                                                        00173201
           IF MAP-ASSOC1F = HEX80                                       00173301
               MOVE SPACES TO COM-ASSOCIATION1                          00173401
           ELSE                                                         00173501
               IF MAP-ASSOC1L  > ZERO                                   00173601
                  MOVE MAP-ASSOC1I  TO COM-ASSOCIATION1.                00173701
                                                                        00173801
           IF MAP-ASSOC2F = HEX80                                       00173901
               MOVE SPACES TO COM-ASSOCIATION2                          00174001
           ELSE                                                         00174101
               IF MAP-ASSOC2L  > ZERO                                   00174201
                  MOVE MAP-ASSOC2I  TO COM-ASSOCIATION2.                00174301
                                                                        00174401
           IF MAP-ASSOC3F = HEX80                                       00174501
               MOVE SPACES TO COM-ASSOCIATION3                          00174601
           ELSE                                                         00174701
               IF MAP-ASSOC3L  > ZERO                                   00174801
                  MOVE MAP-ASSOC3I  TO COM-ASSOCIATION3.                00174901
                                                                        00175001
R02539     IF MAP-COUNTRY-CDF = HEX80                                   00175101
R02539         MOVE SPACES TO COM-COUNTRY-CODE                          00175201
R02539     ELSE                                                         00175301
R02539         IF MAP-COUNTRY-CDL > ZERO                                00175401
R02539            MOVE MAP-COUNTRY-CDI TO COM-COUNTRY-CODE.             00175501
                                                                        00175601
           IF COM-COUNTRY-CODE = SPACES                                 00175701
              MOVE LOW-VALUES TO COM-COUNTRY-CODE                       00175801
           END-IF                                                       00175901
                                                                        00176001
R02539     IF MAP-POSTAL-CDF = HEX80                                    00176101
R02539         MOVE SPACES TO COM-POSTAL-CODE                           00176201
R02539     ELSE                                                         00176301
R02539         IF MAP-POSTAL-CDL > ZERO                                 00176401
R02539            MOVE MAP-POSTAL-CDI TO COM-POSTAL-CODE.               00176501
           DISPLAY 'COM-COUNTRY-CODE' COM-COUNTRY-CODE.                 00176601
           DISPLAY 'COM-POSTAL-CODEE' COM-POSTAL-CODE.                  00176701
           EXEC CICS ASKTIME ABSTIME (WS-TIME-FIELD)                    00176801
                             RESP    (WS-CICS-RESP)                     00176901
                             END-EXEC.                                  00177001
                                                                        00177101
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00177201
               GO TO 9999-CICS-ERROR.                                   00177301
                                                                        00177401
           EXEC CICS FORMATTIME ABSTIME (WS-TIME-FIELD)                 00177501
                                MMDDYY  (WS-MAP-BIF)                    00177601
                                RESP    (WS-CICS-RESP)                  00177701
                                END-EXEC.                               00177801
                                                                        00177901
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00178001
               GO TO 9999-CICS-ERROR.                                   00178101
                                                                        00178201
           MOVE WS-MAP-BIF       TO COM-CHANGE-DATE.                    00178301
                                                                        00178401
                                                                        00178501
       0130-EDIT-NA340M2-MAP.                                           00178601
           DISPLAY '0130-EDIT-NA340M2-MAP PARA'                         00178701
           MOVE 'N' TO WS-EDIT-SW                                       00178801
                      WS-SQL-ERROR.                                     00178901
           MOVE WS-CIM-NUMBER   TO  WS-UPD-CIM-NUMBER.                  00179001
           DISPLAY 'WS-UPD-CIM-NUMBER:' WS-UPD-CIM-NUMBER               00179101
                                                                        00179201
           EXEC CICS LINK PROGRAM('NA340B')                             00180001
                          COMMAREA(WS-UPD-COMMAREA)                     00190001
                          DATALENGTH(LINK-UPD-COMM-LENGTH)              00200001
                          RESP(WS-CICS-RESP)                            00210001
                          RESP2(WS-CICS-RESP2)                          00220001
           END-EXEC                                                     00230001
                                                                        00240001
           DISPLAY 'WS-CICS-RESP:' WS-CICS-RESP.                        00241001
                                                                        00242001
           IF WS-EDIT-SW = 'Y'                                          00250001
              EVALUATE TRUE                                             00260001
                WHEN WS-ERR-FIELD1 = 'MAP-ENTITY-TYPE'                  00270001
                WHEN WS-ERR-FIELD2 = 'MAP-ENTITY-TYPE'                  00280001
                WHEN WS-ERR-FIELD3 = 'MAP-ENTITY-TYPE'                  00290001
                WHEN WS-ERR-FIELD4 = 'MAP-ENTITY-TYPE'                  00290101
                   MOVE -1       TO MAP-ENTITY-TYPEL                    00290201
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ENTITY-TYPEA        00290301
                WHEN WS-ERR-FIELD1 = 'MAP-CUST-STATUS'                  00290401
                WHEN WS-ERR-FIELD2 = 'MAP-CUST-STATUS'                  00290501
                WHEN WS-ERR-FIELD3 = 'MAP-CUST-STATUS'                  00290601
                WHEN WS-ERR-FIELD4 = 'MAP-CUST-STATUS'                  00290701
                   MOVE -1       TO MAP-CUST-STATUSL                    00290801
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-CUST-STATUSA        00290901
                WHEN WS-ERR-FIELD1 = 'MAP-SSN'                          00291001
                WHEN WS-ERR-FIELD2 = 'MAP-SSN'                          00291101
                WHEN WS-ERR-FIELD3 = 'MAP-SSN'                          00291201
                WHEN WS-ERR-FIELD4 = 'MAP-SSN'                          00291301
                   MOVE -1       TO MAP-SSNL                            00291401
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-SSNA                00291501
                WHEN WS-ERR-FIELD1 = 'MAP-DISP-IND'                     00291601
                WHEN WS-ERR-FIELD2 = 'MAP-DISP-IND'                     00291701
                WHEN WS-ERR-FIELD3 = 'MAP-DISP-IND'                     00291801
                WHEN WS-ERR-FIELD4 = 'MAP-DISP-IND'                     00291901
                   MOVE -1       TO MAP-DIS-CHG-INDL                    00292001
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-DIS-CHG-INDA        00292101
                WHEN WS-ERR-FIELD1 = 'MAP-AREA-CODE'                    00292201
                WHEN WS-ERR-FIELD2 = 'MAP-AREA-CODE'                    00292301
                WHEN WS-ERR-FIELD3 = 'MAP-AREA-CODE'                    00292401
                WHEN WS-ERR-FIELD4 = 'MAP-AREA-CODE'                    00292501
                   MOVE -1       TO MAP-AREA-CODEL                      00292601
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-AREA-CODEA          00292701
                WHEN WS-ERR-FIELD1 = 'MAP-PHONE3'                       00292801
                WHEN WS-ERR-FIELD2 = 'MAP-PHONE3'                       00292901
                WHEN WS-ERR-FIELD3 = 'MAP-PHONE3'                       00293001
                WHEN WS-ERR-FIELD4 = 'MAP-PHONE3'                       00293101
                   MOVE -1       TO MAP-PHONE3L                         00293201
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-PHONE3A             00293301
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-PHONE4A             00293401
                WHEN WS-ERR-FIELD1 = 'MAP-PHONE-EXT'                    00293501
                WHEN WS-ERR-FIELD2 = 'MAP-PHONE-EXT'                    00293601
                WHEN WS-ERR-FIELD3 = 'MAP-PHONE-EXT'                    00293701
                WHEN WS-ERR-FIELD4 = 'MAP-PHONE-EXT'                    00293801
                   MOVE -1       TO MAP-PHONE-EXTL                      00293901
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-PHONE-EXTA          00294001
                WHEN WS-ERR-FIELD1 = 'MAP-FAX-AREA-CD'                  00294101
                WHEN WS-ERR-FIELD2 = 'MAP-FAX-AREA-CD'                  00294201
                WHEN WS-ERR-FIELD3 = 'MAP-FAX-AREA-CD'                  00294301
                WHEN WS-ERR-FIELD4 = 'MAP-FAX-AREA-CD'                  00294401
                   MOVE -1       TO MAP-FAX-AREA-CDL                    00294501
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-FAX-AREA-CDA        00294601
                WHEN WS-ERR-FIELD1 = 'MAP-FAX-PHONE3'                   00294701
                WHEN WS-ERR-FIELD2 = 'MAP-FAX-PHONE3'                   00294801
                WHEN WS-ERR-FIELD3 = 'MAP-FAX-PHONE3'                   00294901
                WHEN WS-ERR-FIELD4 = 'MAP-FAX-PHONE3'                   00295001
                   MOVE -1       TO MAP-FAX-PHONE3L                     00295101
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-FAX-PHONE3A         00295201
                                                MAP-FAX-PHONE4A         00295301
                WHEN WS-ERR-FIELD1 = 'MAP-COMP-IN-ADD'                  00295401
                WHEN WS-ERR-FIELD2 = 'MAP-COMP-IN-ADD'                  00295501
                WHEN WS-ERR-FIELD3 = 'MAP-COMP-IN-ADD'                  00295601
                WHEN WS-ERR-FIELD4 = 'MAP-COMP-IN-ADD'                  00295701
                   MOVE -1       TO MAP-COMP-IN-ADDL                    00295801
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-COMP-IN-ADDA        00295901
                WHEN WS-ERR-FIELD1 = 'MAP-ADDR-OVRIDE'                  00296001
                WHEN WS-ERR-FIELD2 = 'MAP-ADDR-OVRIDE'                  00296101
                WHEN WS-ERR-FIELD3 = 'MAP-ADDR-OVRIDE'                  00296201
                WHEN WS-ERR-FIELD4 = 'MAP-ADDR-OVRIDE'                  00296301
                   MOVE -1       TO MAP-ADDR-OVRIDEL                    00296401
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDR-OVRIDEA        00296501
                WHEN WS-ERR-FIELD1 = 'MAP-ZIP'                          00296601
                WHEN WS-ERR-FIELD2 = 'MAP-ZIP'                          00296701
                WHEN WS-ERR-FIELD3 = 'MAP-ZIP'                          00296801
                WHEN WS-ERR-FIELD4 = 'MAP-ZIP'                          00296901
                   MOVE -1       TO MAP-ZIPL                            00297001
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ZIPA                00297101
                WHEN WS-ERR-FIELD1 = 'MAP-ADDRESS1'                     00297201
                WHEN WS-ERR-FIELD2 = 'MAP-ADDRESS1'                     00297301
                WHEN WS-ERR-FIELD3 = 'MAP-ADDRESS1'                     00297401
                WHEN WS-ERR-FIELD4 = 'MAP-ADDRESS1'                     00297501
                   MOVE -1       TO MAP-ADDRESS1L                       00297601
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A           00297701
                WHEN WS-ERR-FIELD1 = 'MAP-CITY'                         00297801
                WHEN WS-ERR-FIELD2 = 'MAP-CITY'                         00297901
                WHEN WS-ERR-FIELD3 = 'MAP-CITY'                         00298001
                WHEN WS-ERR-FIELD4 = 'MAP-CITY'                         00298101
                   MOVE -1       TO MAP-CITYL                           00298201
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-CITYA               00298301
                WHEN WS-ERR-FIELD1 = 'MAP-STATE'                        00298401
                WHEN WS-ERR-FIELD2 = 'MAP-STATE'                        00298501
                WHEN WS-ERR-FIELD3 = 'MAP-STATE'                        00298601
                WHEN WS-ERR-FIELD4 = 'MAP-STATE'                        00298701
                   MOVE -1       TO MAP-STATEL                          00298801
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-STATEA              00298901
                WHEN WS-ERR-FIELD1 = 'MAP-FIRST-NAME'                   00299001
                WHEN WS-ERR-FIELD2 = 'MAP-FIRST-NAME'                   00299101
                WHEN WS-ERR-FIELD3 = 'MAP-FIRST-NAME'                   00299201
                WHEN WS-ERR-FIELD4 = 'MAP-FIRST-NAME'                   00299301
                   MOVE -1  TO MAP-FIRST-NAMEL                          00299401
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-FIRST-NAMEA         00299501
                WHEN WS-ERR-FIELD1 = 'MAP-LAST-NAME'                    00299601
                WHEN WS-ERR-FIELD2 = 'MAP-LAST-NAME'                    00299701
                WHEN WS-ERR-FIELD3 = 'MAP-LAST-NAME'                    00299801
                WHEN WS-ERR-FIELD4 = 'MAP-LAST-NAME'                    00299901
                   MOVE -1       TO MAP-LAST-NAMEL                      00300001
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-LAST-NAMEA          00300101
                WHEN WS-ERR-FIELD1 = 'MAP-SEX'                          00300201
                WHEN WS-ERR-FIELD2 = 'MAP-SEX'                          00300301
                WHEN WS-ERR-FIELD3 = 'MAP-SEX'                          00300401
                WHEN WS-ERR-FIELD4 = 'MAP-SEX'                          00300501
                   MOVE -1       TO MAP-SEXL                            00300601
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-SEXA                00300701
                WHEN WS-ERR-FIELD1 = 'MAP-MIDDLE-NAME'                  00300801
                WHEN WS-ERR-FIELD2 = 'MAP-MIDDLE-NAME'                  00300901
                WHEN WS-ERR-FIELD3 = 'MAP-MIDDLE-NAME'                  00301001
                WHEN WS-ERR-FIELD4 = 'MAP-MIDDLE-NAME'                  00301101
                   MOVE -1       TO MAP-MIDDLE-NAMEL                    00301201
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-MIDDLE-NAMEA        00301301
                WHEN WS-ERR-FIELD1 = 'MAP-COUNTRY-CD'                   00301401
                WHEN WS-ERR-FIELD2 = 'MAP-COUNTRY-CD'                   00301501
                WHEN WS-ERR-FIELD3 = 'MAP-COUNTRY-CD'                   00301601
                WHEN WS-ERR-FIELD4 = 'MAP-COUNTRY-CD'                   00301701
                   MOVE -1       TO MAP-COUNTRY-CDL                     00301801
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-COUNTRY-CDA         00301901
                WHEN WS-ERR-FIELD1 = 'MAP-POSTAL-CD'                    00302001
                WHEN WS-ERR-FIELD2 = 'MAP-POSTAL-CD'                    00302101
                WHEN WS-ERR-FIELD3 = 'MAP-POSTAL-CD'                    00302201
                WHEN WS-ERR-FIELD4 = 'MAP-POSTAL-CD'                    00302301
                   MOVE -1       TO MAP-POSTAL-CDL                      00302401
                   MOVE ATTRB-UNPROT-BRT-PEN TO MAP-POSTAL-CDA          00302501
              END-EVALUATE                                              00302601
           ELSE                                                         00302701
              IF WS-UPD-SQL-ERROR = 'Y'                                 00302801
               MOVE WS-UPD-SQLCODE   TO  SQLCODE                        00302901
               IF WS-UPD-SQLCODE = 100                                  00303001
                 PERFORM 0380-BUMP-ERROR-MESSAGES                       00303301
               ELSE                                                     00303401
                 GO TO 0420-DB2-ERROR                                   00303501
               END-IF                                                   00303601
              ELSE                                                      00303801
                 DISPLAY 'RECORD UPDATED SUCCESSFULLY:' COM-CIM         00303901
              END-IF                                                    00304001
           END-IF.                                                      00304101
                                                                        00326601
       0180-SEND-NA340M2-FIRST-TIME.                                    00328101
                                                                        00328201
           DISPLAY '0180-SEND-NA340M2-FIRST-TIME'                       00328301
                                                                        00328401
           MOVE 'NA'    TO MAP-SYSTEM-CDO.                              00328501
                                                                        00328601
           MOVE 'U'     TO MAP-ACTION-CDO.                              00328701
                                                                        00328801
           MOVE ALL '_' TO MAP-CUST-INFOO.                              00328901
                                                                        00329001
           MOVE -1      TO MAP-CUST-INFOL.                              00329101
                                                                        00329201
           PERFORM 0275-MOVE-SPACES-TO-MAP.                             00329301
                                                                        00329401
           EXEC CICS SEND MAP    ('NA340M2')                            00329501
                          CURSOR                                        00329601
                          ERASE                                         00329701
                          RESP (WS-CICS-RESP)                           00329801
                          END-EXEC.                                     00329901
                                                                        00330001
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00330101
                GO TO 9999-CICS-ERROR.                                  00330201
                                                                        00330301
           DISPLAY 'HERE'                                               00330401
                                                                        00330501
           EXEC CICS RETURN TRANSID ('NAU1')                            00330601
                COMMAREA (NA340M2O)                                     00330701
                LENGTH    (32)                                          00330801
             END-EXEC.                                                  00330901
                                                                        00331001
       0190-INQUIRE-PARA.                                               00331101
                                                                        00331201
           DISPLAY '0190-INQUIRE-PARA'                                  00331301
                                                                        00331401
           DISPLAY 'MAP-CUST-INFOL:' MAP-CUST-INFOL                     00331501
                                                                        00331601
           MOVE MAP-CUST-INFOI         TO COMM-CUSTOMER-INFO            00331701
           MOVE MAP-CUST-INFOI(1:8)    TO WS-CIM-NUMBER                 00331801
           DISPLAY 'WS-CIM-NUMBER:' WS-CIM-NUMBER                       00331901
                                                                        00332001
           MOVE 'N' TO WS-RECORD-NOTFND                                 00332101
                       WS-SQL-ERROR                                     00332201
                                                                        00332301
           EXEC CICS LINK PROGRAM('NA320C')                             00332401
                          COMMAREA(WS-INQ-COMMAREA)                     00332501
                          DATALENGTH(LINK-INQ-COMM-LENGTH)              00332601
                          RESP(WS-CICS-RESP)                            00332701
                          RESP2(WS-CICS-RESP2)                          00332801
           END-EXEC                                                     00332901
                                                                        00333001
           DISPLAY 'WS-CICS-RESP:' WS-CICS-RESP                         00333101
           DISPLAY 'WS-CICS-RESP:' WS-CICS-RESP.                        00333201
                                                                        00333301
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00333401
               GO TO 9999-CICS-ERROR                                    00333501
           END-IF                                                       00333601
                                                                        00333701
           DISPLAY '0250 WS-RECORD-NOTFND' WS-RECORD-NOTFND             00333801
           DISPLAY '0250 WS-SQL-ERROR' WS-SQL-ERROR.                    00333901
                                                                        00334001
           IF WS-SQL-ERROR = 'Y'                                        00334101
                 MOVE WS-SQLCODE  TO  SQLCODE                           00334201
                 GO TO  0420-DB2-ERROR                                  00334301
           END-IF.                                                      00334401
                                                                        00334501
           IF WS-RECORD-NOTFND = 'Y'                                    00334601
              MOVE 'NA020'           TO WS-MESSAGE-NUMBER1              00334701
              PERFORM 0380-BUMP-ERROR-MESSAGES                          00334801
           END-IF.                                                      00334901
                                                                        00335001
           IF WS-RECORD-NOTFND NOT = 'Y'                                00335101
              MOVE LS-TABLE-DATA TO DCLAGNTNAME                         00335201
              MOVE LS-POSTAL-CODE TO POSTAL-CODE                        00335301
              MOVE LS-COUNTRY-CODE TO COUNTRY-CODE                      00335401
              PERFORM 0280-POPULATE-MAP                                 00335501
           ELSE                                                         00335601
             IF WS-RECORD-NOTFND = 'Y'                                  00335701
                DISPLAY 'RECORD NOT FOUND'                              00335801
                MOVE MAP-CUST-INFOI      TO MAP-CUST-INFOO              00335901
                PERFORM 0275-MOVE-SPACES-TO-MAP                         00336001
      *         MOVE COM-AGNTNAME TO WSQ-AGNTNAME                       00336101
      *         PERFORM 0290-MOVE-OUT-MAP-FIELDS                        00336201
             END-IF                                                     00336301
            END-IF                                                      00336401
                                                                        00336501
           MOVE SPACES       TO COMM-MSG-MAX-SEVERITY.                  00336601
           MOVE -1           TO MAP-CUST-INFOL.                         00336701
                                                                        00336801
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00336901
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00337001
               MOVE SPACES  TO COMM-SYSTEM-CODE                         00337101
           ELSE                                                         00337201
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00337301
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00337401
               MOVE ALL '_' TO MAP-ACTION-CDO                           00337501
               MOVE SPACES  TO COMM-ACTION-CODE                         00337601
           ELSE                                                         00337701
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00337801
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00337901
               MOVE ALL '_' TO MAP-CUST-INFOO                           00338001
               MOVE SPACES  TO COMM-CUSTOMER-INFO                       00338101
           ELSE                                                         00338201
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00338301
                                                                        00338401
961037     IF ENTITY-TYPE = 'RP'                                        00338501
961037         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA         00338601
961037     END-IF.                                                      00338701
                                                                        00338801
R03179     IF ENTITY-TYPE = 'CA'                                        00338901
R03179         MOVE ATTRB-PROT-ASKIP        TO MAP-EMAIL1A MAP-EMAIL2A  00339001
R03179     END-IF.                                                      00339101
                                                                        00339201
           EXEC CICS SEND MAP    ('NA340M2')                            00339301
                          CURSOR                                        00339401
                          ERASE                                         00339501
                          RESP (WS-CICS-RESP)                           00339601
                          END-EXEC.                                     00339701
                                                                        00339801
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00339901
                GO TO 9999-CICS-ERROR.                                  00340001
                                                                        00340101
                                                                        00340201
           EXEC CICS RETURN TRANSID ('NAU1')                            00340301
                COMMAREA (NA340M2O)                                     00340401
                LENGTH    (32)                                          00340501
              END-EXEC.                                                 00340601
                                                                        00340701
       0200-SEND-NA340M2-MAP.                                           00340801
           DISPLAY '0200-SEND-NA340M2-MAP  PARA'.                       00340901
           MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1).                   00341001
           MOVE WS-MESSAGE-NUMBER2 TO COMM-MSG-ID(2).                   00341101
           MOVE WS-MESSAGE-NUMBER3 TO COMM-MSG-ID(3).                   00341201
           MOVE WS-MESSAGE-NUMBER4 TO COMM-MSG-ID(4).                   00341301
                                                                        00341401
           PERFORM 0390-CHECK-ERROR-MESSAGES                            00341501
               VARYING ACTION-SUB FROM 1 BY 1                           00341601
                   UNTIL ACTION-SUB > 4.                                00341701
                                                                        00341801
           IF COMM-MSG-MAX-SEVERITY = 'E'                               00341901
               MOVE SPACES TO COMM-MSG-MAX-SEVERITY.                    00342001
                                                                        00342101
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00342201
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00342301
               MOVE SPACES  TO COMM-SYSTEM-CODE                         00342401
           ELSE                                                         00342501
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00342601
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00342701
               MOVE ALL '_' TO MAP-ACTION-CDO                           00342801
               MOVE SPACES  TO COMM-ACTION-CODE                         00342901
           ELSE                                                         00343001
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00343101
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00343201
               MOVE ALL '_' TO MAP-CUST-INFOO                           00343301
               MOVE SPACES  TO COMM-CUSTOMER-INFO                       00343401
           ELSE                                                         00343501
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00343601
                                                                        00343701
                                                                        00343801
           PERFORM 0290-MOVE-OUT-MAP-FIELDS.                            00343901
900837     IF WS-EDIT-SW = 'Y'                                          00344001
900837         EXEC CICS SEND CONTROL ALARM END-EXEC                    00344101
900837     ELSE                                                         00344201
               MOVE -1 TO MAP-CUST-INFOL.                               00344301
                                                                        00344401
           EXEC CICS SEND MAP    ('NA340M2')                            00344501
                          CURSOR                                        00344601
                          ERASE                                         00344701
                          RESP (WS-CICS-RESP)                           00344801
                          END-EXEC.                                     00344901
                                                                        00345001
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00345101
                GO TO 9999-CICS-ERROR.                                  00345201
                                                                        00345301
           EXEC CICS RETURN TRANSID ('NAU1')                            00345601
                COMMAREA (NA340M2O)                                     00345701
                LENGTH    (32)                                          00345801
                            END-EXEC.                                   00345901
                                                                        00346001
       0275-MOVE-SPACES-TO-MAP.                                         00452301
                                                                        00452401
           DISPLAY '0275-MOVE-SPACES-TO-MAP'                            00452501
           MOVE SPACES TO MAP-FIRST-NAMEO MAP-MIDDLE-NAMEO              00452601
               MAP-LAST-NAMEO MAP-SUFFIX1O MAP-SUFFIX2O                 00452701
               MAP-NICKNAMEO MAP-COMP-NAMEO MAP-DIS-NAMEO               00452801
               MAP-ADDRESS1O MAP-BIRTH-DATEO MAP-SEXO                   00452901
               MAP-ADDRESS2O MAP-SSNO  MAP-EFF-DATEO                    00453001
               MAP-PREFIXO MAP-COUNTRY-CDO MAP-POSTAL-CDO               00453101
               MAP-CITYO MAP-COMP-IN-ADDO                               00453201
               MAP-STATEO MAP-ZIPO MAP-ZIP-PLUS4O MAP-ADDR-OVRIDEO      00453301
               MAP-AREA-CODEO MAP-PHONE3O MAP-PHONE4O                   00453401
               MAP-FAX-AREA-CDO MAP-FAX-PHONE3O MAP-FAX-PHONE4O         00453501
               MAP-PHONE-EXTO MAP-ALT-ADDRO MAP-ENTITY-TYPEO            00453601
               MAP-FUTURE-ADDRO MAP-CUST-STATUSO                        00453701
               MAP-SITE-CODEO MAP-SITE-DESCO MAP-CHANGE-DATEO           00453801
               MAP-LOGONO MAP-ASSOC1O MAP-ASSOC2O MAP-ASSOC3O           00453901
               MAP-FINALST-CDO MAP-CIMO                                 00454001
               MAP-EMAIL1O.                                             00454101
                                                                        00454401
       0280-POPULATE-MAP.                                               00457301
           DISPLAY '0280-POPULATE-MAP'.                                 00457401
R00700     MOVE EMAIL1             TO MAP-EMAIL1O      WSQ-EMAIL1.      00457501
           MOVE IDNTITY                TO MAP-CIMO         WSQ-CIM.     00457601
           MOVE LAST-NAME          TO MAP-LAST-NAMEO   WSQ-LAST-NAME.   00457701
T3384      MOVE PREFIX             TO MAP-PREFIXO      WSQ-PREFIX.      00457801
           MOVE FIRST-NAME         TO MAP-FIRST-NAMEO  WSQ-FIRST-NAME.  00457901
           MOVE MIDDLE-NAME        TO MAP-MIDDLE-NAMEO WSQ-MIDDLE-NAME. 00458001
           MOVE SUFFIX1            TO MAP-SUFFIX1O     WSQ-SUFFIX1.     00458101
           MOVE SUFFIX2            TO MAP-SUFFIX2O     WSQ-SUFFIX2.     00458201
           MOVE COMPANY-IN-ADDRESS TO MAP-COMP-IN-ADDO                  00458301
                                      WSQ-COMPANY-IN-ADDRESS.           00458401
           MOVE COMPANY-NAME       TO MAP-COMP-NAMEO   WSQ-COMPANY-NAME.00458501
           MOVE DISPLAY-NAME       TO MAP-DIS-NAMEO    WSQ-DISPLAY-NAME.00458601
           MOVE NICKNAME           TO MAP-NICKNAMEO    WSQ-NICKNAME.    00458701
           MOVE ADDRESS1           TO MAP-ADDRESS1O    WSQ-ADDRESS1.    00458801
           MOVE ADDRESS2           TO MAP-ADDRESS2O    WSQ-ADDRESS2.    00458901
           MOVE CITY               TO MAP-CITYO        WSQ-CITY.        00459001
           MOVE STATE              TO MAP-STATEO       WSQ-STATE.       00459101
           MOVE ZIP                TO MAP-ZIPO         WSQ-ZIP.         00459201
           MOVE ZIP-PLUS4          TO MAP-ZIP-PLUS4O   WSQ-ZIP-PLUS4.   00459301
R02539     MOVE COUNTRY-CODE       TO MAP-COUNTRY-CDO WSQ-COUNTRY-CODE. 00459401
R02539     MOVE POSTAL-CODE        TO MAP-POSTAL-CDO   WSQ-POSTAL-CODE. 00459501
           MOVE AREA-CODE          TO MAP-AREA-CODEO   WSQ-AREA-CODE.   00459601
           MOVE PHONE              TO WS-PHONE         WSQ-PHONE.       00459701
           MOVE WS-PHONE3          TO MAP-PHONE3O.                      00459801
           MOVE WS-PHONE4          TO MAP-PHONE4O.                      00459901
           MOVE PHONE-EXTENSION    TO MAP-PHONE-EXTO                    00460001
                                      WSQ-PHONE-EXTENSION.              00460101
           DISPLAY 'MAP-COUNTRY-CDO' MAP-COUNTRY-CDO.                   00460201
           DISPLAY 'MAP-POSTAL-CDO' MAP-POSTAL-CDO.                     00460301
900837     MOVE FAX-AREA-CODE      TO MAP-FAX-AREA-CDO                  00460401
900837                                WSQ-FAX-AREA-CODE.                00460501
900837     MOVE FAX-PHONE          TO WS-PHONE         WSQ-FAX-PHONE.   00460601
900837     MOVE WS-PHONE3          TO MAP-FAX-PHONE3O.                  00460701
900837     MOVE WS-PHONE4          TO MAP-FAX-PHONE4O.                  00460801
R04023     MOVE ORIGINAL-STATE     TO MAP-ISSUE-STATEO                  00460901
R04023                                WSQ-ISSUE-STATE.                  00461001
           MOVE SSN-NUM            TO MAP-SSNO         WSQ-SSN.         00461101
           MOVE SEX                TO MAP-SEXO         WSQ-SEX.         00461201
           MOVE 'Y'                TO MAP-DIS-CHG-INDO WSQ-DISP-IND.    00461301
           MOVE FINALST-REAS-CODE  TO MAP-FINALST-CDO                   00461401
                                      WSQ-FINALST-REAS-CODE.            00461501
           MOVE FINALST-OVRD-IND  TO  MAP-ADDR-OVRIDEO                  00461601
                                      WSQ-FINALST-OVRD-IND.             00461701
           MOVE CHANGE-LOGON       TO WSQ-CHANGE-LOGON.                 00461801
MANJU1     MOVE CHANGE-LOGON       TO MAP-LOGONO.                       00461901
                                                                        00463701
MANJU      MOVE RECORD-STATUS  TO MAP-CUST-STATUSO.                     00464801
                                                                        00464901
           MOVE ALT-ADDRESS-IND    TO MAP-ALT-ADDRO                     00465001
                                      WSQ-ALT-ADDRESS-IND.              00465101
           MOVE FUTURE-ADDRESS-IND TO MAP-FUTURE-ADDRO                  00465201
                                      WSQ-FUTURE-ADDRESS-IND.           00465301
           MOVE SITE-CODE          TO MAP-SITE-CODEO   WSQ-SITE-CODE.   00465401
           MOVE SPACES             TO MAP-SITE-DESCO.                   00466201
                                                                        00466301
GRB        MOVE COMBINED-STATUS    TO WSQ-COMBINED-STATUS.              00466401
GRB        MOVE ADDRESS-KEY1       TO WSQ-ADDRESS-KEY1.                 00466501
                                                                        00466601
           MOVE ASSOCIATION1       TO MAP-ASSOC1O      WSQ-ASSOCIATION1.00466701
           MOVE ASSOCIATION2       TO MAP-ASSOC2O      WSQ-ASSOCIATION2.00466801
           MOVE ASSOCIATION3       TO MAP-ASSOC3O      WSQ-ASSOCIATION3.00466901
SPC   ***  IF ASSOCIATION1 = 'YERKE'                                    00467001
SPC   ***    AND ASSOCIATION2 = 'NBRL'                                  00467101
SPC   ***          MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A                 00467201
SPC   ***                                   MAP-ASSOC2A.                00467301
LMB   ***                                   MAP-ASSOC3A.                00467401
                                                                        00467501
920124***  IF  ASSOCIATION1 = 'FREDM'                                   00467601
920124***  AND ASSOCIATION2 = SPACES                                    00467701
920124***  AND ASSOCIATION3 = SPACES                                    00467801
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A                     00467901
920124***                               MAP-ASSOC2A                     00468001
920124***                               MAP-ASSOC3A.                    00468101
920124***  IF  ASSOCIATION1 = 'FREDM'                                   00468201
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A.                    00468301
920124***  IF  ASSOCIATION2 = 'FREDM'                                   00468401
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC2A.                    00468501
920124***  IF  ASSOCIATION3 = 'FREDM'                                   00468601
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC3A.                    00468701
                                                                        00468801
           MOVE ENTITY-TYPE        TO MAP-ENTITY-TYPEO.                 00469801
                                                                        00469901
           MOVE BIRTH-DATE         TO WS-DB2-DATE.                      00470001
Y2KIMR*                                                                 00470101
Y2KIMR* IMR CHANGE IMR7  BEGIN                                          00470201
Y2KIMR*                                                                 00470301
Y2KIMR*    MOVE WS-DB2-DD-R        TO WS-DAY.                           00470401
Y2KIMR*    MOVE WS-DB2-MM-R        TO WS-MONTH.                         00470501
Y2KIMR*    MOVE WS-DB2-YY-R        TO WS-YEAR.                          00470601
Y2KIMR*    MOVE '/'                TO SLASH-1 SLASH-2.                  00470701
Y2KIMR*    MOVE WS-DATE-EIGHT      TO MAP-BIRTH-DATEO.                  00470801
Y2KIMR*                                                                 00470901
Y2KIMR     IF    WS-DB2-DATE  EQUAL  ZEROS                              00471001
Y2KIMR        MOVE ZEROS    TO  Y2K-WK-DATE-TEN                         00471101
Y2KIMR                          MAP-BIRTH-DATEO                         00471201
Y2KIMR     ELSE                                                         00471301
Y2KIMR      MOVE WS-DB2-MM-R         TO Y2K-WK-MONTH                    00471401
Y2KIMR      MOVE WS-DB2-DD-R         TO Y2K-WK-DAY                      00471501
Y2KIMR      MOVE WS-DB2-CC-R         TO Y2K-WK-CENT                     00471601
Y2KIMR      MOVE WS-DB2-YY-R         TO Y2K-WK-YEAR                     00471701
Y2KIMR      MOVE '-'                 TO Y2K-WK-SLASH-1 Y2K-WK-SLASH-2   00471801
Y2KIMR      MOVE Y2K-WK-DATE-TEN TO  MAP-BIRTH-DATEO .                  00471901
Y2KIMR*                                                                 00472001
Y2KIMR* IMR CHANGE IMR7 END                                             00472101
Y2KIMR*                                                                 00472201
                                                                        00472301
                                                                        00472401
Y2KIMR*                                                                 00472501
Y2KIMR* IMR CHANGE IMR7A BEGIN                                          00472601
Y2KIMR*                                                                 00472701
Y2KIMR*    MOVE BR-CC              TO WS-YY-A.                          00472801
Y2KIMR*    MOVE BR-YY              TO WS-YY-A.                          00472901
Y2KIMR*    MOVE BR-MM              TO WS-MM-A.                          00473001
Y2KIMR*    MOVE BR-DD              TO WS-DD-A.                          00473101
Y2KIMR*    MOVE WS-DATE            TO WS-DATE-R.                        00473201
Y2KIMR*    MOVE WS-DATE-R          TO WSQ-BIRTH-DATE.                   00473301
Y2KIMR*                                                                 00473401
Y2KIMR     MOVE BIRTH-DATE         TO WS-BIRTH-DATE.                    00473501
Y2KIMR     MOVE BR-CC              TO Y2K-WK-VARIABLE2-CC.              00473601
Y2KIMR     MOVE BR-YY              TO Y2K-WK-VARIABLE2-YY.              00473701
Y2KIMR     MOVE BR-MM              TO Y2K-WK-VARIABLE2-MM.              00473801
Y2KIMR     MOVE BR-DD              TO Y2K-WK-VARIABLE2-DD.              00473901
Y2KIMR     MOVE Y2K-WK-VARIABLE2   TO WSQ-BIRTH-DATE.                   00474001
Y2KIMR*                                                                 00474101
Y2KIMR* IMR CHANGE IMR7A END                                            00474201
Y2KIMR*                                                                 00474301
                                                                        00474401
           MOVE EFFECTIVE-DATE     TO WS-DB2-DATE.                      00474501
           MOVE WS-DB2-DD-R        TO WS-DAY.                           00474601
           MOVE WS-DB2-MM-R        TO WS-MONTH.                         00474701
           MOVE WS-DB2-YY-R        TO WS-YEAR.                          00474801
           MOVE '/'                TO SLASH-1 SLASH-2.                  00474901
           MOVE WS-DATE-EIGHT      TO MAP-EFF-DATEO.                    00475001
                                                                        00475101
           MOVE EFFECTIVE-DATE     TO WS-EFFECTIVE-DATE                 00475201
           MOVE EF-YY              TO WS-YY-A.                          00475301
           MOVE EF-MM              TO WS-MM-A.                          00475401
           MOVE EF-DD              TO WS-DD-A.                          00475501
           MOVE WS-DATE            TO WS-DATE-R.                        00475601
           MOVE WS-DATE-R          TO WSQ-EFFECTIVE-DATE.               00475701
                                                                        00475801
           MOVE CHANGE-DATE        TO WS-DB2-TIMESTAMP.                 00475901
           MOVE WS-TS-DD-R         TO WS-DAY.                           00476001
           MOVE WS-TS-MM-R         TO WS-MONTH.                         00476101
           MOVE WS-TS-YY-R         TO WS-YEAR.                          00476201
           MOVE '/'                TO SLASH-1 SLASH-2.                  00476301
           MOVE WS-DATE-EIGHT      TO MAP-CHANGE-DATEO.                 00476401
                                                                        00476501
           MOVE CHANGE-DATE        TO WS-CHANGE-DATE.                   00476601
           MOVE CO-YY              TO WS-YY-A.                          00476701
           MOVE CO-MM              TO WS-MM-A.                          00476801
           MOVE CO-DD              TO WS-DD-A.                          00476901
           MOVE WS-DATE            TO WS-DATE-R.                        00477001
           MOVE WS-DATE-R          TO WSQ-CHANGE-DATE.                  00477101
                                                                        00477201
MANJU *    EXEC SQL SELECT CARRIER_CODE,                                00477301
R04023*                    PREV_CARRIER                                 00477401
R04023*    INTO :CASEX-CARRIER-CODE,                                    00477501
R04023*         :CASEX-PREV-CARRIER                                     00477601
R04023*    FROM CASEXV01                                                00477701
11735A*    FROM CASE_MASTER                                             00477801
R04023*    WHERE CASENAME#IDNTITY  = :COMM-IDNTITY                      00477901
R04023*    END-EXEC.                                                    00478001
MANJU      MOVE SPACES TO CASEX-CARRIER-CODE CASEX-PREV-CARRIER.        00478101
R04023     MOVE CASEX-CARRIER-CODE  TO WSQ-CARRIER-CODE.                00478201
R04023     MOVE CASEX-PREV-CARRIER  TO WSQ-PREV-CARRIER.                00478301
R04023**************************************************                00478401
R04023* DISPLAY ISSUE STATE ONLY FOR AIG                                00478501
R04342* DISPLAY ISSUE STATE ONLY FOR AIG AND ACM                        00478601
R04023**************************************************                00478701
R04023     IF (CASEX-CARRIER-CODE  = '9D'                               00478801
R04023        AND                                                       00478901
R04023*       CASEX-PREV-CARRIER   = 'SH')                              00479001
R04254*       CASEX-PREV-CARRIER   = ('SH' OR 'SV'))                    00479101
R04281*       CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN'))            00479201
R04280        CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN' OR 'SP'      00479301
R04209                                     OR 'JJ'                      00479401
R04832                                     OR 'JA'                      00479501
R04472                                     OR 'IG'))                    00479601
R04023        OR                                                        00479701
R04023*       (CASEX-CARRIER-CODE  = 'SH')                              00479801
R04254*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV')                      00479901
R04281*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN')              00480001
R04280        (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN' OR 'SP'       00480101
R04472*                                   OR 'IG'                       00480201
R04342                                    OR 'IG' OR 'MV' OR 'MB'       00480301
R04209             OR 'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'              00480401
R04934             OR 'NO' OR 'NU' OR 'NX'                              00480501
R04209             OR 'LD' OR 'LI' OR 'MI'                              00480601
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'      00480701
R04209             OR 'NQ' OR 'NS'                                      00480801
R04209             OR 'UD' OR 'UT'                                      00480901
R04209             OR 'VI' OR 'WF'                                      00481001
R04832** NEW CIGNA CARRIERS                                             00481101
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'      00481201
R04832             OR 'JH'                                              00481301
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'      00481401
R04832             )                                                    00481501
R04023         MOVE ATTRB-PROT-ASKIP-DRK                                00481601
R04023                     TO MAP-LITERAL8A    MAP-LITERAL6A            00481701
R04023                        MAP-LITERAL5A    MAP-SUFFIX1A             00481801
R04023                        MAP-SUFFIX2A     MAP-NICKNAMEA            00481901
R04023                        MAP-LITERAL4A    MAP-SEXA                 00482001
R04023         MOVE SPACES TO MAP-LITERAL8O    MAP-LITERAL6O            00482101
R04023                        MAP-LITERAL5O    MAP-SUFFIX1O             00482201
R04023                        MAP-SUFFIX2O     MAP-NICKNAMEO            00482301
R04023                        MAP-LITERAL4O    MAP-SEXO                 00482401
R04023     ELSE                                                         00482501
960530*    IF (ENTITY-TYPE = 'AM' OR 'CA' OR 'LD')                      00482601
960530     IF (ENTITY-TYPE = 'AM' OR 'LD')                              00482701
900837         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA         00482801
               MOVE ATTRB-PROT-ASKIP-DRK                                00482901
                           TO MAP-LITERAL1A    MAP-LITERAL2A            00483001
                              MAP-LITERAL3A    MAP-LITERAL4A            00483101
                              MAP-LITERAL5A    MAP-LITERAL6A            00483201
                              MAP-LITERAL7A    MAP-LITERAL8A            00483301
                              MAP-LITERAL9A    MAP-LITERAL10A           00483401
                              MAP-FIRST-NAMEA  MAP-MIDDLE-NAMEA         00483501
                              MAP-LAST-NAMEA   MAP-SUFFIX1A             00483601
                              MAP-SUFFIX2A     MAP-NICKNAMEA            00483701
                              MAP-SEXA         MAP-BIRTH-DATEA          00483801
                              MAP-COMP-IN-ADDA                          00483901
R04023                        MAP-ISSUE-STATEA                          00484001
R04023                        MAP-ISS-LITERALA                          00484101
R04023                        MAP-ST-LITERALA                           00484201
               MOVE SPACES TO MAP-FIRST-NAMEO  MAP-MIDDLE-NAMEO         00484301
                              MAP-LAST-NAMEO   MAP-SUFFIX1O             00484401
                              MAP-SUFFIX2O     MAP-NICKNAMEO            00484501
                              MAP-SEXO         MAP-BIRTH-DATEO          00484601
                              MAP-COMP-IN-ADDO                          00484701
R04023                        MAP-ISSUE-STATEO                          00484801
                              WSQ-FIRST-NAME   WSQ-MIDDLE-NAME          00484901
                              WSQ-LAST-NAME    WSQ-SUFFIX1              00485001
                              WSQ-SUFFIX2      WSQ-NICKNAME             00485101
R04023                        WSQ-ISSUE-STATE                           00485201
                              WSQ-SEX                                   00485301
R04023     ELSE                                                         00485401
R04023          MOVE ATTRB-PROT-ASKIP-DRK TO                            00485501
R04023                        MAP-ISSUE-STATEA                          00485601
R04023                        MAP-ISS-LITERALA                          00485701
R04023                        MAP-ST-LITERALA                           00485801
R04023          MOVE SPACES TO WSQ-ISSUE-STATE                          00485901
R04023                         MAP-ISSUE-STATEO.                        00486001
                                                                        00486101
       0290-MOVE-OUT-MAP-FIELDS.                                        00486201
           DISPLAY '290 PARA FIELDS' COM-SEX                            00486301
MANJU1     MOVE 'WIPRO123'             TO MAP-LOGONO.                   00486401
           MOVE COM-FIRST-NAME         TO MAP-FIRST-NAMEO.              00486501
           MOVE COM-MIDDLE-NAME        TO MAP-MIDDLE-NAMEO.             00486601
           MOVE COM-LAST-NAME          TO MAP-LAST-NAMEO.               00486701
T3384      MOVE COM-PREFIX             TO MAP-PREFIXO.                  00486801
           MOVE COM-SUFFIX1            TO MAP-SUFFIX1O.                 00486901
           MOVE COM-SUFFIX2            TO MAP-SUFFIX2O.                 00487001
           MOVE COM-NICKNAME           TO MAP-NICKNAMEO.                00487101
           MOVE COM-COMPANY-NAME       TO MAP-COMP-NAMEO.               00487201
           MOVE COM-DISPLAY-NAME       TO MAP-DIS-NAMEO.                00487301
           MOVE COM-ADDRESS1           TO MAP-ADDRESS1O.                00487401
           MOVE COM-SEX                TO MAP-SEXO.                     00487501
           MOVE COM-ADDRESS2           TO MAP-ADDRESS2O.                00487601
           MOVE COM-CITY               TO MAP-CITYO.                    00487701
           MOVE COM-COMPANY-IN-ADDRESS TO MAP-COMP-IN-ADDO.             00487801
           MOVE COM-STATE              TO MAP-STATEO.                   00487901
           MOVE COM-ZIP                TO MAP-ZIPO.                     00488001
           MOVE COM-ZIP-PLUS4          TO MAP-ZIP-PLUS4O.               00488101
           MOVE COM-FINALST-OVRD-IND   TO MAP-ADDR-OVRIDEO.             00488201
           MOVE COM-AREA-CODE          TO MAP-AREA-CODEO.               00488301
           MOVE COM-PHONE              TO WS-PHONE.                     00488401
           MOVE WS-PHONE3              TO MAP-PHONE3O.                  00488501
           MOVE WS-PHONE4              TO MAP-PHONE4O.                  00488601
           MOVE COM-SSN                TO MAP-SSNO.                     00488701
           MOVE COM-PHONE-EXTENSION    TO MAP-PHONE-EXTO.               00488801
900837     MOVE COM-FAX-AREA-CODE      TO MAP-FAX-AREA-CDO.             00488901
900837     MOVE COM-FAX-PHONE          TO WS-PHONE.                     00489001
900837     MOVE WS-PHONE3              TO MAP-FAX-PHONE3O.              00489101
900837     MOVE WS-PHONE4              TO MAP-FAX-PHONE4O.              00489201
R04023     MOVE COM-ISSUE-STATE        TO MAP-ISSUE-STATEO.             00489301
R9031A     MOVE COM-EMAIL1             TO MAP-EMAIL1O.                  00489401
           MOVE COM-ALT-ADDRESS-IND    TO MAP-ALT-ADDRO.                00489501
MANJU2     DISPLAY '290 PARA COM-ENTITY-TYPE' COM-ENTITY-TYPE.          00489601
           MOVE COM-ENTITY-TYPE        TO MAP-ENTITY-TYPEO.             00489701
           MOVE COM-DISP-IND           TO MAP-DIS-CHG-INDO.             00489801
           MOVE COM-FUTURE-ADDRESS-IND TO MAP-FUTURE-ADDRO.             00489901
           MOVE COM-RECORD-STATUS      TO MAP-CUST-STATUSO.             00490001
           MOVE COM-SITE-CODE          TO MAP-SITE-CODEO.               00490101
           MOVE COM-ASSOCIATION1       TO MAP-ASSOC1O.                  00490201
           MOVE COM-ASSOCIATION2       TO MAP-ASSOC2O.                  00490301
           MOVE COM-ASSOCIATION3       TO MAP-ASSOC3O.                  00490401
R02539     MOVE COM-POSTAL-CODE        TO MAP-POSTAL-CDO.               00490501
R02539     MOVE COM-COUNTRY-CODE       TO MAP-COUNTRY-CDO.              00490601
            DISPLAY ' 29O PARA MAP-POSTAL-CDO' MAP-POSTAL-CDO.          00490701
            DISPLAY ' 29O PARA MAP-COUNTRY-CDO' MAP-COUNTRY-CDO.        00490801
SPC   ***  IF COM-ASSOCIATION1 = 'YERKE'                                00490901
SPC   ***    AND COM-ASSOCIATION2 = 'NBRL'                              00491001
SPC   ***          MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A                 00491101
SPC   ***                                   MAP-ASSOC2A.                00491201
LMB   ***                                   MAP-ASSOC3A.                00491301
                                                                        00491401
920124***  IF  COM-ASSOCIATION1 = 'FREDM'                               00491501
920124***  AND COM-ASSOCIATION2 = SPACES                                00491601
920124***  AND COM-ASSOCIATION3 = SPACES                                00491701
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A                     00491801
920124***                               MAP-ASSOC2A                     00491901
920124***                               MAP-ASSOC3A.                    00492001
920124***  IF  COM-ASSOCIATION1 = 'FREDM'                               00492101
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A.                    00492201
920124***  IF  COM-ASSOCIATION2 = 'FREDM'                               00492301
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC2A.                    00492401
920124***  IF  COM-ASSOCIATION3 = 'FREDM'                               00492501
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC3A.                    00492601
                                                                        00492701
961037     IF  ENTITY-TYPE = 'RP'                                       00492801
961037         MOVE ATTRB-PROT-ASKIP TO MAP-ENTITY-TYPEA                00492901
961037     END-IF.                                                      00493001
                                                                        00493101
           MOVE COM-CIM                TO MAP-CIMO.                     00493201
           MOVE COM-FINALST-REAS-CODE  TO MAP-FINALST-CDO.              00493301
      *    MOVE WS-DEMO-USER-NAME      TO MAP-LOGONO.                   00493401
      *    IF  COM-CHANGE-LOGON NOT EQUAL SPACES AND                    00493501
      *        COM-CHANGE-LOGON NOT = LOW-VALUES                        00493601
      *            MOVE COM-CHANGE-LOGON TO WR-EMP-PSINUM               00493701
      *            EXEC CICS READ DATASET('EMPLID')                     00493801
      *                           INTO   (WR-EMPLOYEE-RECORD)           00493901
      *                           RIDFLD (WR-EMP-PSINUM)                00494001
990721*                           LENGTH (LENGTH OF WR-EMPLOYEE-RECORD) 00494101
      *                           RESP   (WS-CICS-RESP)                 00494201
      *                           END-EXEC                              00494301
      *            MOVE WR-EMP-NAME TO MAP-LOGONO                       00494401
001220** HANDLE DUP LOGONID- JUST DISPLAY LOGONID INSTEAD OF CICS ERROR 00494501
001220*            IF WS-CICS-RESP = DFHRESP(DUPKEY)                    00494601
001220*               MOVE WR-EMP-PSINUM TO MAP-LOGONO                  00494701
001220*            ELSE                                                 00494801
      *            IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND            00494901
      *                WS-CICS-RESP NOT = DFHRESP(NOTFND)               00495001
      *                    GO TO 9999-CICS-ERROR.                       00495101
                                                                        00495201
                                                                        00495301
           MOVE COM-RECORD-STATUS  TO MAP-CUST-STATUSO.                 00496101
                                                                        00496201
           MOVE SPACES         TO MAP-SITE-DESCO.                       00497001
                                                                        00497101
           MOVE COM-ENTITY-TYPE        TO MAP-ENTITY-TYPEO.             00497901
                                                                        00498001
      *    IF DISPLAY-CHANGED-SW = 'Y'                                  00498101
      *        PERFORM 0300-FORMAT-DISPLAY-NAME.                        00498201
                                                                        00498301
Y2KIMR*                                                                 00498401
Y2KIMR* IMR CHANGE IMR8 BEGIN                                           00498501
Y2KIMR*                                                                 00498601
Y2KIMR*    MOVE COM-BIRTH-DATE TO WS-DATE-R.                            00498701
Y2KIMR*    MOVE WS-DATE-R      TO WS-DATE.                              00498801
Y2KIMR*    MOVE WS-MM          TO WS-MONTH.                             00498901
Y2KIMR*    MOVE WS-DD          TO WS-DAY.                               00499001
Y2KIMR*    MOVE WS-YY          TO WS-YEAR.                              00499101
Y2KIMR*    MOVE '/'            TO SLASH-1 SLASH-2.                      00499201
Y2KIMR*    MOVE WS-DATE-EIGHT  TO MAP-BIRTH-DATEO.                      00499301
Y2KIMR*                                                                 00499401
Y2KIMR      MOVE COM-BIRTH-DATE TO Y2K-WK-VARIABLE2.                    00499501
Y2KIMR      MOVE Y2K-WK-VARIABLE2-MM TO Y2K-WK-MONTH.                   00499601
Y2KIMR      MOVE Y2K-WK-VARIABLE2-DD TO Y2K-WK-DAY .                    00499701
Y2KIMR      MOVE Y2K-WK-VARIABLE2-CC TO Y2K-WK-CENT.                    00499801
Y2KIMR      MOVE Y2K-WK-VARIABLE2-YY TO Y2K-WK-YEAR .                   00499901
Y2KIMR      MOVE '-'                 TO Y2K-WK-SLASH-1 Y2K-WK-SLASH-2.  00500001
Y2KIMR      MOVE Y2K-WK-DATE-TEN TO MAP-BIRTH-DATEO.                    00500101
Y2KIMR*                                                                 00500201
Y2KIMR* IMR CHANGE IMR8 END                                             00500301
Y2KIMR*                                                                 00500401
                                                                        00500501
           MOVE COM-CHANGE-DATE TO WS-DATE-R                            00500601
           MOVE WS-DATE-R       TO WS-DATE.                             00500701
           MOVE WS-MM           TO WS-MONTH.                            00500801
           MOVE WS-DD           TO WS-DAY.                              00500901
           MOVE WS-YY           TO WS-YEAR.                             00501001
           MOVE '/'             TO SLASH-1 SLASH-2.                     00501101
           MOVE WS-DATE-EIGHT   TO MAP-CHANGE-DATEO.                    00501201
                                                                        00501301
           MOVE COM-EFFECTIVE-DATE TO WS-DATE-R.                        00501401
           MOVE WS-DATE-R          TO WS-DATE.                          00501501
           MOVE WS-MM              TO WS-MONTH.                         00501601
           MOVE WS-DD              TO WS-DAY.                           00501701
           MOVE WS-YY              TO WS-YEAR.                          00501801
           MOVE '/'                TO SLASH-1 SLASH-2.                  00501901
           MOVE WS-DATE-EIGHT      TO MAP-EFF-DATEO.                    00502001
                                                                        00502101
R04023*    EXEC SQL SELECT CARRIER_CODE,                                00502201
R04023*                    PREV_CARRIER                                 00502301
R04023*    INTO :CASEX-CARRIER-CODE,                                    00502401
R04023*         :CASEX-PREV-CARRIER                                     00502501
R04023*    FROM CASEXV01                                                00502601
11735A*    FROM CASE_MASTER                                             00502701
R04023*    WHERE CASENAME#IDNTITY= :COMM-IDNTITY                        00502801
R04023*    END-EXEC.                                                    00502901
MANJU      MOVE SPACES TO CASEX-CARRIER-CODE CASEX-PREV-CARRIER.        00503001
R04023     MOVE CASEX-CARRIER-CODE  TO WSQ-CARRIER-CODE.                00503101
R04023     MOVE CASEX-PREV-CARRIER  TO WSQ-PREV-CARRIER.                00503201
R04023**************************************************                00503301
R04023* DISPLAY ISSUE STATE ONLY FOR AIG                                00503401
R04023**************************************************                00503501
R04023     IF (CASEX-CARRIER-CODE  = '9D'                               00503601
R04023        AND                                                       00503701
R04023*       CASEX-PREV-CARRIER   = 'SH')                              00503801
R04254*       CASEX-PREV-CARRIER   = ('SH' OR 'SV'))                    00503901
R04281*       CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN'))            00504001
R04280        CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN' OR 'SP'      00504101
R04209                                     OR 'JJ'                      00504201
R04832                                     OR 'JA'                      00504301
R04472                                     OR 'IG'))                    00504401
R04023        OR                                                        00504501
R04023*       (CASEX-CARRIER-CODE  = 'SH')                              00504601
R04254*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV')                      00504701
R04281*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN')              00504801
R04280        (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN' OR 'SP'       00504901
R04472                                    OR 'IG'                       00505001
R04209             OR 'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'              00505101
R04934             OR 'NO' OR 'NU' OR 'NX'                              00505201
R04209             OR 'LD' OR 'LI' OR 'MI'                              00505301
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'      00505401
R04209             OR 'NQ' OR 'NS'                                      00505501
R04209             OR 'UD' OR 'UT'                                      00505601
R04209             OR 'VI' OR 'WF'                                      00505701
R04832** NEW CIGNA CARRIERS                                             00505801
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'      00505901
R04832             OR 'JH'                                              00506001
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'      00506101
R04832             )                                                    00506201
R04023         MOVE ATTRB-PROT-ASKIP-DRK                                00506301
R04023                     TO MAP-LITERAL8A    MAP-LITERAL6A            00506401
R04023                        MAP-LITERAL5A    MAP-SUFFIX1A             00506501
R04023                        MAP-SUFFIX2A     MAP-NICKNAMEA            00506601
R04023                        MAP-LITERAL4A    MAP-SEXA                 00506701
R04023         MOVE SPACES TO MAP-LITERAL8O    MAP-LITERAL6O            00506801
R04023                        MAP-LITERAL5O    MAP-SUFFIX1O             00506901
R04023                        MAP-SUFFIX2O     MAP-NICKNAMEO            00507001
R04023                        MAP-LITERAL4O    MAP-SEXO                 00507101
R04023     ELSE                                                         00507201
           IF COM-CASE-SW = 'Y'  AND                                    00507301
960530       (COM-ENTITY-TYPE = 'AM' OR 'LD')                           00507401
900837         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA         00507501
               MOVE ATTRB-PROT-ASKIP-DRK                                00507601
                           TO MAP-LITERAL1A    MAP-LITERAL2A            00507701
                              MAP-LITERAL3A    MAP-LITERAL4A            00507801
                              MAP-LITERAL5A    MAP-LITERAL6A            00507901
                              MAP-LITERAL7A    MAP-LITERAL8A            00508001
                              MAP-LITERAL9A    MAP-LITERAL10A           00508101
                              MAP-FIRST-NAMEA  MAP-MIDDLE-NAMEA         00508201
                              MAP-LAST-NAMEA   MAP-SUFFIX1A             00508301
                              MAP-SUFFIX2A     MAP-NICKNAMEA            00508401
                              MAP-SEXA         MAP-BIRTH-DATEA          00508501
                              MAP-COMP-IN-ADDA                          00508601
               MOVE SPACES TO MAP-FIRST-NAMEO  MAP-MIDDLE-NAMEO         00508701
                              MAP-LAST-NAMEO   MAP-SUFFIX1O             00508801
                              MAP-SUFFIX2O     MAP-NICKNAMEO            00508901
                              MAP-SEXO         MAP-BIRTH-DATEO          00509001
                              MAP-COMP-IN-ADDO                          00509101
R04023                        MAP-ISSUE-STATEO                          00509201
R04023     ELSE                                                         00509301
R04023         MOVE ATTRB-PROT-ASKIP-DRK TO                             00509401
R04023                        MAP-ISSUE-STATEA                          00509501
R04023                        MAP-ISS-LITERALA                          00509601
R04023                        MAP-ST-LITERALA                           00509701
R04023         MOVE SPACES TO MAP-ISSUE-STATEO.                         00509801
R04023                                                                  00509901
961037     IF COM-ENTITY-TYPE = 'RP'                                    00510001
961037         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA.        00510101
R03179     IF COM-ENTITY-TYPE = 'CA'                                    00510201
R03179         MOVE ATTRB-PROT-ASKIP        TO MAP-EMAIL1A MAP-EMAIL2A. 00510301
                                                                        00510401
       0300-FORMAT-DISPLAY-NAME.                                        00510501
                                                                        00510601
      * --------------------------------------------------------------* 00510701
      *    GET FORMATED VERSION OF NAME AND ADDRESS                   * 00510801
      * --------------------------------------------------------------* 00510901
           MOVE ZEROS                 TO NA-COMM-RECORD-NUMBER.         00511001
           MOVE COM-FIRST-NAME        TO NA-COMM-FIRST-NAME.            00511101
           MOVE COM-MIDDLE-NAME       TO NA-COMM-MIDDLE-NAME.           00511201
           MOVE COM-LAST-NAME         TO NA-COMM-LAST-NAME.             00511301
T3384      MOVE COM-PREFIX            TO NA-COMM-TITLE.                 00511401
           MOVE COM-SUFFIX1           TO NA-COMM-SUFFIX1.               00511501
           MOVE COM-SUFFIX2           TO NA-COMM-SUFFIX2.               00511601
           MOVE COM-COMPANY-NAME      TO NA-COMM-COMPANY-NAME.          00511701
           MOVE COM-COMPANY-IND       TO NA-COMM-COMPANY-MAIL.          00511801
           MOVE COM-COMPANY-NAME      TO NA-COMM-COMPANY-NAME.          00511901
           MOVE COM-ADDRESS1          TO NA-COMM-ADDRESS-1.             00512001
           MOVE COM-ADDRESS2          TO NA-COMM-ADDRESS-2.             00512101
           MOVE COM-CITY              TO NA-COMM-CITY.                  00512201
           MOVE COM-STATE             TO NA-COMM-STATE.                 00512301
           MOVE COM-ZIP               TO NA-COMM-ZIP.                   00512401
           MOVE COM-SEX               TO NA-COMM-SEX.                   00512501
           MOVE COM-ZIP-PLUS4         TO NA-COMM-ZIP-PLUS4.             00512601
           MOVE COM-ENTITY-TYPE       TO NA-COMM-SYSTEM-TYPE.           00512701
           MOVE COM-RECORD-STATUS     TO NA-COMM-RECORD-STATUS.         00512801
                                                                        00512901
           IF COM-AREA-CODE NUMERIC                                     00513001
               MOVE COM-AREA-CODE TO NA-COMM-PHONE-AREA                 00513101
           ELSE                                                         00513201
               MOVE ZEROES TO NA-COMM-PHONE-AREA.                       00513301
                                                                        00513401
           MOVE COM-PHONE TO WS-PHONE.                                  00513501
           IF WS-PHONE3 NUMERIC                                         00513601
               MOVE WS-PHONE3 TO NA-COMM-PHONE-3                        00513701
           ELSE                                                         00513801
               MOVE ZEROES TO NA-COMM-PHONE-3.                          00513901
                                                                        00514001
           IF WS-PHONE4 NUMERIC                                         00514101
               MOVE WS-PHONE4 TO NA-COMM-PHONE-4                        00514201
           ELSE                                                         00514301
               MOVE ZEROES TO NA-COMM-PHONE-4.                          00514401
                                                                        00514501
           MOVE 'Y'  TO NA-COMM-FORMAT-SWITCH.                          00514601
           MOVE 'Y'  TO NA-COMM-CASE-SWITCH.                            00514701
           MOVE 'FM' TO NA-COMM-REQUEST-CODE.                           00514801
           MOVE ZERO TO NA-COMM-RETURN-CODE.                            00514901
                                                                        00515001
R04209** IF AMERICAN GENERAL USERS CHANGE NAME ON TAS                   00515101
R04209** DON'T PASS THE SEX FOR FORMATTING THE DISPLAY NAME             00515201
R04209** SO THAT IT WON'T USE MR. MS.                                   00515301
R04209     IF (COM-ENTITY-TYPE = 'AM' OR 'CA' OR 'LB')                  00515401
R04209        IF (COM-CARRIER-CODE  = '9D'                              00515501
R04209            AND COM-PREV-CARRIER   = 'JJ'                         00515601
R04832                                 OR  'JA')                        00515701
R04209          OR (COM-CARRIER-CODE =                                  00515801
R04209                'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'              00515901
R04934             OR 'NO' OR 'NU' OR 'NX'                              00516001
R04209             OR 'LD' OR 'LI' OR 'MI'                              00516101
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'      00516201
R04209             OR 'NQ' OR 'NS'                                      00516301
R04209             OR 'UD' OR 'UT'                                      00516401
R04209             OR 'VI' OR 'WF'                                      00516501
R04832** NEW CIGNA CARRIERS                                             00516601
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'      00516701
R04832             OR 'JH'                                              00516801
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'      00516901
R04832             )                                                    00517001
R04209          MOVE SPACES TO NA-COMM-SEX                              00517101
R04209        END-IF                                                    00517201
R04209     END-IF.                                                      00517301
                                                                        00517401
      *    EXEC CICS LINK PROGRAM ('NA203')                             00517501
      *                   COMMAREA(NAME-AND-ADDRESS-COMMAREA)           00517601
990721**                  LENGTH  (NA-COMMAREA-LENGTH)                  00517701
990721*                   LENGTH  (LENGTH OF NAME-AND-ADDRESS-COMMAREA) 00517801
      *                   RESP    (WS-CICS-RESP)                        00517901
      *                   END-EXEC.                                     00518001
                                                                        00518101
      *    IF  WS-CICS-RESP NOT = DFHRESP(NORMAL)                       00518201
      *        GO TO 9999-CICS-ERROR.                                   00518301
                                                                        00518401
      *---------------------------------------------------------------* 00518501
      *    IF THERE WAS AN SQL ERROR OR MISC CICS ERROR IN NA202      * 00518601
      *    IT HAS ALREADY PLACED AN ERROR DIAGNOSTIC MAP ON THE       * 00518701
      *    TERMINAL, WE WANT TO STOP THIS PROGRAM HERE SO THAT        * 00518801
      *    THE DIAGNOSTIC IS NOT OVERLAYED.                           * 00518901
      *---------------------------------------------------------------* 00519001
      *    IF NA-SQL-FAILED OR NA-MISC-CICS-ERROR                       00519101
      *        EXEC CICS SEND CONTROL ALARM END-EXEC                    00519201
      *        EXEC CICS RETURN END-EXEC.                               00519301
                                                                        00519401
           IF NA-SUCCESSFUL                                             00519501
COBOLU         MOVE SPACES TO COM-DISPLAY-NAME                          00519601
               MOVE NA-COMM-MAIL-LINE1 TO COM-DISPLAY-NAME              00519701
               MOVE COM-DISPLAY-NAME   TO MAP-DIS-NAMEO.                00519801
                                                                        00519901
       0310-INITIATE-ROUTER.                                            00520001
           DISPLAY 'INSIDE 0310-INITIATE-ROUTER PARA'                   00520101
                                                                        00526501
           EXEC CICS RETURN                                             00526601
                     END-EXEC.                                          00526701
                                                                        00526801
       0340-PROCESS-SCREEN-CLEAR.                                       00552901
                                                                        00553001
           MOVE SPACES TO WS-NULL-FIELD.                                00555601
           EXEC CICS SEND FROM (WS-NULL-FIELD)                          00555701
                          RESP (WS-CICS-RESP)                           00555801
                          ERASE                                         00555901
                          LENGTH(72)                                    00556001
                          END-EXEC.                                     00556101
                                                                        00556201
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00556301
                GO TO 9999-CICS-ERROR.                                  00556401
                                                                        00556501
           EXEC CICS RETURN                                             00556601
                     END-EXEC.                                          00556701
                                                                        00556901
       0360-WRONG-KEY-HIT.                                              00560601
                                                                        00560701
           EXEC CICS RECEIVE MAP    ('NA340M2')                         00560801
                             RESP   (WS-CICS-RESP)                      00560901
                             END-EXEC.                                  00561001
                                                                        00561101
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND                    00561201
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)                     00561301
                GO TO 9999-CICS-ERROR.                                  00561401
                                                                        00561501
           PERFORM 0110-UPDATE-COMMAND-LINE.                            00561601
           PERFORM 0120-UPDATE-CIM-MAP-FIELDS.                          00561701
                                                                        00561801
                                                                        00561901
                                                                        00562001
      * --------------------------------------------------------------* 00562101
      * THIS CODE KEEPS THE UPDATE MESSAGE BR152 FROM BEING DISPLAYED * 00562201
      *  IF AN UPDATE HAD BEEN COMMPLETED PRIOR AND THEN THE USER     * 00562301
      *  HIST AN INVALID KEY.                                         * 00562401
      * --------------------------------------------------------------* 00562501
                                                                        00562601
           IF COMM-MSG-ID (1) = 'NA159'                                 00562701
               MOVE SPACES TO COMM-MSG-ID (1)                           00562801
           ELSE                                                         00562901
           IF COMM-MSG-ID (2) = 'NA159'                                 00563001
               MOVE SPACES TO COMM-MSG-ID (2)                           00563101
           ELSE                                                         00563201
           IF COMM-MSG-ID (3) = 'NA159'                                 00563301
               MOVE SPACES TO COMM-MSG-ID (3)                           00563401
           ELSE                                                         00563501
           IF COMM-MSG-ID (4) = 'NA159'                                 00563601
               MOVE SPACES TO COMM-MSG-ID (4).                          00563701
                                                                        00563801
           MOVE 'NA030'           TO WS-MESSAGE-NUMBER1.                00563901
           PERFORM 0380-BUMP-ERROR-MESSAGES.                            00564001
           MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(1).                    00564101
           MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(2).                    00564201
           MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(3).                    00564301
           MOVE WSQ-MSG-ID(4)     TO COMM-MSG-ID(4).                    00564401
                                                                        00566801
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00566901
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00567001
           ELSE                                                         00567101
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00567201
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00567301
               MOVE ALL '_' TO MAP-ACTION-CDO                           00567401
           ELSE                                                         00567501
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00567601
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00567701
               MOVE ALL '_' TO MAP-CUST-INFOO                           00567801
           ELSE                                                         00567901
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00568001
                                                                        00568201
           PERFORM 0290-MOVE-OUT-MAP-FIELDS.                            00568301
                                                                        00568401
           EXEC CICS SEND MAP    ('NA340M2')                            00568501
                          CURSOR (COMM-CURSOR-POSN)                     00568601
                          ERASE                                         00568701
                          RESP (WS-CICS-RESP)                           00568801
                          END-EXEC.                                     00568901
                                                                        00569001
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)                        00569101
                GO TO 9999-CICS-ERROR.                                  00569201
                                                                        00569301
           EXEC CICS RETURN TRANSID ('NAU1')                            00569401
                COMMAREA (NA340M2O)                                     00569501
                LENGTH    (32)                                          00569601
                            END-EXEC.                                   00569701
                                                                        00569801
       0380-BUMP-ERROR-MESSAGES.                                        00569901
                                                                        00570001
           MOVE COMM-MSG-ID(1)    TO WSQ-MSG-ID(1).                     00570101
           MOVE COMM-MSG-ID(2)    TO WSQ-MSG-ID(2).                     00570201
           MOVE COMM-MSG-ID(3)    TO WSQ-MSG-ID(3).                     00570301
           MOVE COMM-MSG-ID(4)    TO WSQ-MSG-ID(4).                     00570401
           MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1).                   00570501
           MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(2).                    00570601
           MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(3).                    00570701
           MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(4).                    00570801
           PERFORM 0390-CHECK-ERROR-MESSAGES                            00570901
               VARYING ACTION-SUB FROM 1 BY 1                           00571001
                   UNTIL ACTION-SUB > 4.                                00571101
                                                                        00571201
       0390-CHECK-ERROR-MESSAGES.                                       00571301
           DISPLAY '0390-CHECK-ERROR-MESSAGES PARA'                     00571401
           IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND                 00571501
               COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES                00571601
               MOVE COMM-MSG-ID (ACTION-SUB) TO WT-C9999-ERROR-CODE     00571701
               PERFORM 0410-GET-ERROR-MESSAGE                           00571801
               PERFORM 0400-CHECK-RETURN-CODE.                          00571901
                                                                        00572001
       0400-CHECK-RETURN-CODE.                                          00572101
               DISPLAY 'SQLCODE' SQLCODE.                               00572201
               DISPLAY 'SQLERRML:' SQLERRML                             00572303
               DISPLAY 'SQLERRMC:' SQLERRMC                             00572403
               IF SQLCODE = ZERO                                        00572501
                   MOVE WT-C9999-ERROR-CODE TO WS-C9999-ERROR-CODE      00572601
      *            MOVE WT-C9999-SEVERITY-CODE TO WS-C9999-SEVERITY-CODE00572701
                   MOVE EDIT-DESC TO WS-C9999-ERROR-MESSAGE             00572801
                   MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (ACTION-SUB)  00572901
               ELSE                                                     00573001
                  IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND          00573101
                      COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES         00573201
                      MOVE '** INVALID ERROR MESSAGE ** ' TO            00573301
                           MAP-ERROR-MSGO (ACTION-SUB).                 00573401
                                                                        00573501
       0410-GET-ERROR-MESSAGE.                                          00573601
           DISPLAY 'EDIT WT-C9999-ERROR-CODE' WT-C9999-ERROR-CODE.      00573701
           EXEC SQL                                                     00573801
           SELECT EDIT_DESC                                             00573901
              INTO :EDIT-DESC                                           00574001
             FROM EDITCODE                                              00574101
            WHERE EDIT_CD = :WT-C9999-ERROR-CODE                        00574201
           END-EXEC.                                                    00574301
                                                                        00574401
       0420-DB2-ERROR.                                                  00575601
                                                                        00575701
           DISPLAY 'SQLCODE 0420-DB2-ERROR:' SQLCODE                    00575801
           MOVE SQLCODE TO WS-DB2I-MESSAGE.                             00575901
           MOVE WS-DB2I-MESSAGE TO WS-C9999-ERROR-CODE.                 00576001
           MOVE "DB2 SQL ERROR" TO WS-C9999-ERROR-MESSAGE.              00576101
           MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (1).                  00576201
           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES                   00576301
               MOVE ALL '_' TO MAP-SYSTEM-CDO                           00576401
               MOVE SPACES  TO COMM-SYSTEM-CODE                         00576501
           ELSE                                                         00576601
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.                00576701
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES                   00576801
               MOVE ALL '_' TO MAP-ACTION-CDO                           00576901
               MOVE SPACES  TO COMM-ACTION-CODE                         00577001
           ELSE                                                         00577101
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.                00577201
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES                 00577301
               MOVE ALL '_' TO MAP-CUST-INFOO                           00577401
               MOVE SPACES  TO COMM-CUSTOMER-INFO                       00577501
           ELSE                                                         00577601
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.                00577701
                                                                        00577801
           PERFORM 0290-MOVE-OUT-MAP-FIELDS.                            00577901
           EXEC CICS SEND MAP    ('NA340M2')                            00578001
                          CURSOR                                        00578101
                          ERASE                                         00578201
                          RESP (WS-CICS-RESP)                           00578301
                          END-EXEC.                                     00578401
           EXEC CICS RETURN TRANSID ('NAU1')                            00578501
                COMMAREA (NA340M2O)                                     00578601
                LENGTH    (32)                                          00578701
                     END-EXEC.                                          00578801
                                                                        00578901
           EXEC CICS RETURN                                             00579001
                     END-EXEC.                                          00579101
      * --------------------------------------------------------------* 00579201
      * THIS ROUTINE TRANSFERS CONTROL TO AN ONLINE ERROR PROGRAM     * 00579301
      * WHICH DISPLAYS THE ERRORS ON THE SCREEN SUPPLYING INFORMATION * 00579401
      * FOR THE HELP-DESK  TO AID PROGRAMMERS IN DEBUGGING.           * 00579501
      * --------------------------------------------------------------* 00579601
                                                                        00579701
           COPY CICSERR.                                                00579801
                                                                        00579901
