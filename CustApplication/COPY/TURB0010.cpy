      **********************PROGRAM PURPOSE**************************   TURB0010
      *                                                             *   TURB0010
      *   COPYBOOK TITLE: TURB0010                                  *   TURB0010
      *   COPYBOOK TEXT: TABLE COPYBOOK FOR CARRIER GENERAL INFO    *   TURB0010
      *                                                             *   TURB0010
      *     THIS TABLE CONTAINS SEVERAL PIECES OF DATA WHICH        *   TURB0010
      *     ARE UNIQUE TO EACH CARRIER SUCH AS FIRST/LAST           *   TURB0010
      *     ISSUE DATE, MINIMUM/MAXIMUM NUMBER OF LIVES, TYPE       *   TURB0010
      *     OF COVERAGE IN CODE3 - CODE6.                           *   TURB0010
      *     THE KEY TO THE RECORD IS 'CARRIER CODE'(2 BYTES).       *   TURB0010
      *                                                             *   TURB0010
      *----------------MODULES USING THIS TABLE---------------------*   TURB0010
      *                                                             *   TURB0010
      *   CFPSIN00                                                  *   TURB0010
      *   ISEDTCAS               IS546                 MG003        *   TURB0010
      *   ISEDTTBL               IS560                 MG050        *   TURB0010
      *   IS010                  IS620                 MG060        *   TURB0010
      *   IS010WS                LC500                 PLPTABLE     *   TURB0010
      *   IS541                  MGCEDITS              RT700        *   TURB0010
      *   IS542                  MG001                 RT701        *   TURB0010
      *   IS545                                                     *   TURB0010
      *                                                             *   TURB0010
      *------------------------REVISIONS----------------------------*   TURB0010
      *                                                             *   TURB0010
      *   DATE     BY         CHANGE DESCRIPTION           RSA/SIR  *   TURB0010
      * --------  ----      ----------------------         -------  *   TURB0010
      * 02/26/85   REL        ADDED DOCUMENTATION            NONE   *   TURB0010
      * 08/13/86   TXD        CARRIER TYPE CHANGE           860791  *   TURB0010
      * 08/14/86   KLS        ADD CF CARRIER TO DESC.       860262  *   TURB0010
      * 10/24/86   DKC        MADE REMITTANCE %, LOCKBOX #, AND BANK*   TURB0010
      *                       ACCOUNT FILLER.                       *   TURB0010
      * 06/02/87   SPC        MODIFY FOR NEW TURBO SYSTEM   870421  *   TURB0010
      * 11/01/89   JAR        ADD COLUMN PREM REFUND IND    890371  *   TURB0010
      * 03/26/90   TDA        ADD BANKCARD 88 LEVEL TO EFT  900233  *   TURB0010
      * 06/18/90   SPC        ADD GL CARRIER CODE PER TONY  890371  *   TURB0010
      * 06/04/91   RXR        ADD COLUMN COMPLIANCE IND     910403  *   TURB0010
      * 11/24/92   CWL        ADD 88 LEVEL FOR SHORT-TERM   920474  *   TURB0010
930358* 07/20/93   BXN        ADD TWO FIELDS FOR BILLING    930358  *   TURB0010
930247* 10/19/93   DER        ADD  DEPENDENT SYSTEM IND     930247  *   TURB0010
960473* 04/24/96   GRB        ADD  HMO SWITCH               960473  *   TURB0010
R00996* 06/22/00   IAM        TABLE 0200 TO 0500 CONVERSION R00996  *   TURB0010
R02795* 08/27/03   FXM        ADD '3' TO USE-PROD-TYPE      R02795  *   TURB0010
R02795*                       WHICH WILL ALSO MEAN THAT THE R02795  *   TURB0010
R02795*                       HEALTH OPTION IS USED OR      R02795  *   TURB0010
R02795*                       INCLUDED AS PART OF THE       R02795  *   TURB0010
R02795*                       HEALTH PLAN CODE.             R02795  *   TURB0010
      *****LAST CHANGE***********************************************   TURB0010
Y2KIMR***********************************************************       TURB0010
Y2KIMR* PROJECT NUMBER: SE-NI-HP-3811-02-03                     *       TURB0010
Y2KIMR* PROJECT NAME: HPS DAGGER PROJECT PHASE I                *       TURB0010
Y2KIMR* PROGRAMMER: IMR062                                      *       TURB0010
Y2KIMR*                                                         *       TURB0010
Y2KIMR* 02/1999 - CHANGES MADE FOR YEAR 2000 COMPLIANCE - IMR   *       TURB0010
Y2KIMR*                                                         *       TURB0010
Y2KIMR* 1. KEY AND DATA PARAMETERS CHANGED- WT-C0010-PARM       *       TURB0010
Y2KIMR***********************************************************       TURB0010
           05  WT-CNTL0010.                                             TURB0010
             10  FILLER                   PIC X(8) VALUE 'GSFRB.10'.    TURB0010
             10  WT-C0010-CONTROL         PIC X(8) VALUE SPACE.         TURB0010
XXXXXX       10  WT-C0010-NAME            PIC X(8) VALUE 'TBV80010'.    TURB0010
             10  WT-C0010-REQUEST         PIC X    VALUE 'S'.           TURB0010
             10  FILLER                   PIC X    VALUE SPACE.         TURB0010
             10  WT-C0010-RETURN          PIC S9(4) COMP VALUE +0.      TURB0010
             10  WT-C0010-KEY-LENGTH      PIC S9(4) COMP VALUE +2.      TURB0010
XXXXXX       10  WT-C0010-ENTRY-LENGTH    PIC S9(4) COMP VALUE +83.     TURB0010
             10  WT-C0010-PARM-LENGTH     PIC S9(4) COMP VALUE +80.     TURB0010
             10  WT-C0010-KEY.                                          TURB0010
                 15 WT-C0010-CARRIER               PIC X(2).            TURB0010
             10  WT-C0010-DATA.                                         TURB0010
                 15 WT-C0010-FIRST-ISSUE-DATE      PIC 9(6).            TURB0010
                 15 WT-C0010-LAST-ISSUE-DATE       PIC 9(6).            TURB0010
                 15 WT-C0010-MAX-LIFE-AMOUNT       PIC 9(7).            TURB0010
                 15 FILLER                         PIC X(5).            TURB0010
                 15 WT-C0010-MIN-NUM-OF-LIFES      PIC 9(3).            TURB0010
                 15 WT-C0010-MAX-NUM-OF-LIFES      PIC 9(3).            TURB0010
                 15 WT-C0010-ROLLOVER-START-DT     PIC 9(6).            TURB0010
                 15 WT-C0010-ROLLOVER-END-DT       PIC 9(6).            TURB0010
                 15 WT-C0010-ROLLOVER-FROM-CR      PIC XX.              TURB0010
                 15 WT-C0010-ROLLOVER-TO-CR        PIC XX.              TURB0010
R00996           15 WT-C0010-USE-PROD-TYPE-IND     PIC X.               TURB0010
R02795              88 WT-C0010-USE-PROD-TYPE        VALUE 'Y' '3'.     TURB0010
R02795              88 WT-C0010-PLAN-CD-INCL-OPT     VALUE '3'.         TURB0010
R00996           15 FILLER                         PIC X(12).           TURB0010
                 15 WT-C0010-CARRIER-TYPE.                              TURB0010
                     20 WT-C0010-RATE-TYPE         PIC 9.               TURB0010
                        88 WT-C0010-TABLE            VALUE 0.           TURB0010
                        88 WT-C0010-COMPOSITE        VALUE 1.           TURB0010
                        88 WT-C0010-GROUP            VALUE 2.           TURB0010
                        88 WT-C0010-TIERED           VALUE 3.           TURB0010
                        88 WT-C0010-TAB-OR-COMP      VALUE 4.           TURB0010
                     20 WT-C0010-ADMIN-GROUP       PIC 9.               TURB0010
                        88 WT-C0010-MET              VALUE 0.           TURB0010
                        88 WT-C0010-MEDRE            VALUE 1.           TURB0010
                        88 WT-C0010-LARGE-GROUP      VALUE 2.           TURB0010
                        88 WT-C0010-NON-PSI          VALUE 3.           TURB0010
                        88 WT-C0010-SHORT-TERM       VALUE 4.           TURB0010
                     20 WT-C0010-EFT-FLAG          PIC 9.               TURB0010
                        88 WT-C0010-NO-EFT           VALUE 0.           TURB0010
                        88 WT-C0010-EFT              VALUE 1.           TURB0010
                        88 WT-C0010-EFT-N-BNKCRD     VALUE 2.           TURB0010
                        88 WT-C0010-BNKCRD-ONLY      VALUE 3.           TURB0010
                     20 WT-C0010-CO-CARRIER        PIC 9.               TURB0010
                        88 WT-C0010-CO-NO            VALUE 0.           TURB0010
                        88 WT-C0010-CO-YES           VALUE 1.           TURB0010
                 15 WT-C0010-CODE3-TYPE            PIC X(2).            TURB0010
                 15 WT-C0010-CODE4-TYPE            PIC X(2).            TURB0010
                 15 WT-C0010-CODE5-TYPE            PIC X(2).            TURB0010
                 15 WT-C0010-CODE6-TYPE            PIC X(2).            TURB0010
                 15 WT-C0010-CF30-CARRIER          PIC X(2).            TURB0010
                 15 WT-C0010-HCA-INDICATOR         PIC X(1).            TURB0010
                 15 WT-C0010-PREMIUM-REFUND-IND    PIC X(1).            TURB0010
                 15 WT-C0010-GL-CARR-CODE          PIC X(2).            TURB0010
                 15 WT-C0010-COMPL-IND             PIC X(1).            TURB0010
                    88 WT-C0010-COMPL-CARR           VALUE 'Y'.         TURB0010
                 15 WT-C0010-COMPL-COMM-IND        PIC X(1).            TURB0010
                    88 WT-C0010-SPEC-COMPL-COMM      VALUE 'Y'.         TURB0010
930358           15 WT-C0010-BYPASS-BILLING-SW     PIC X(1).            TURB0010
930358              88 WT-C0010-BYPASS-BILLING       VALUE 'Y'.         TURB0010
930358           15 WT-C0010-BYPASS-BILL-PRINT-SW  PIC X(1).            TURB0010
930358              88 WT-C0010-BYPASS-BILL-PRINT    VALUE 'Y'.         TURB0010
930247           15 WT-C0010-DEPENDENT-SYSTEM-SW   PIC X(1).            TURB0010
930247              88 WT-C0010-DEPENDENT-SYS-CARR  VALUE 'Y'.          TURB0010
960473           15 WT-C0010-HMO-SW                PIC X(1).            TURB0010
960473              88 WT-C0010-HMO                 VALUE 'Y'.          TURB0010
             10  WT-C0010-PARM.                                         TURB0010
                 15  FILLER                        PIC X(50) VALUE      TURB0010
Y2KIMR*                                                                 TURB0010
Y2KIMR* IMR CHANGE BEGIN                                                TURB0010
Y2KIMR*                                                                 TURB0010
Y2KIMR*        ' CONTROL DD=IS600V02,M=0010,K=(5,2),D=(41,83),S=B,'.    TURB0010
Y2KIMR*          15  FILLER                        PIC X(30) VALUE      TURB0010
Y2KIMR*        'E=(31,1, )'.                                            TURB0010
Y2KIMR*                                                                 TURB0010
Y2KIMR         ' CONTROL DD=IS600V02,M=0010,K=(5,2),D=(51,83),S=B,'.    TURB0010
Y2KIMR           15  FILLER                        PIC X(30) VALUE      TURB0010
Y2KIMR         'E=(41,1, )'.                                            TURB0010
Y2KIMR*                                                                 TURB0010
Y2KIMR* IMR CHANGE END                                                  TURB0010
Y2KIMR*                                                                 TURB0010
             10  FILLER                  PIC X(8) VALUE 'GSFRB.10'.     TURB0010
