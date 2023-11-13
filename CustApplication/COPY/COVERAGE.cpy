      ********************************************************************000001
      * DCLGEN TABLE(COVERAGE)                                         *00000110
      ******************************************************************00000120
           EXEC SQL DECLARE COVERAGE TABLE                              00000130
           ( CASENAME#CIM         CHAR(8)          NOT NULL,              000001
             EMPLOYEE#EMP_NUM   DECIMAL(5)       NOT NULL,              00000150
             TYPE               CHAR(8)          NOT NULL,              00000160
             PLAN_CODE          CHAR(2)          NOT NULL,              00000170
             EXCEPTION          CHAR(1)          NOT NULL WITH DEFAULT, 00000180
             EFF_DATE           DATE,                                   00000190
             TERM_DATE          DATE,                                   00000200
             OVERRIDE           CHAR(1)          NOT NULL WITH DEFAULT, 00000210
             PREMIUM            DECIMAL(7,2)     NOT NULL WITH DEFAULT, 00000220
             INCEPTION_DATE     DATE,                                   00000230
             WAIT_PERIOD        CHAR(2),                                00000240
             PROVIDER           CHAR(9),                                00000250
R04342       EMPLOYER_CNTR_AMT  DECIMAL(7, 2),                          00000260
R04342       EMPLOYER_CNTR_RATE DECIMAL(5, 4)                           00000270
         ) END-EXEC.                                                    00000280
      ******************************************************************00000300
      * COBOL DECLARATION FOR TABLE COVERAGE                           *00000310
      ******************************************************************00000320
       01  COVERAGE-REC.                                                00000330
           10 COVERAGE-1           PIC X(8).                            00000340
           10 COVERAGE-2           PIC S9(5)      USAGE COMP-3.         00000350
           10 COVERAGE-3           PIC X(8).                            00000360
           10 COVERAGE-4           PIC X(2).                            00000370
           10 COVERAGE-5           PIC X(1).                            00000380
           10 COVERAGE-6           PIC X(10).                           00000390
           10 COVERAGE-7           PIC X(10).                           00000400
           10 COVERAGE-8           PIC X(1).                            00000410
           10 COVERAGE-9           PIC S9(5)V99   USAGE COMP-3.         00000420
           10 COVERAGE-10          PIC X(10).                           00000430
           10 COVERAGE-11          PIC X(2).                            00000440
           10 COVERAGE-12          PIC X(09).                           00000450
R04342     10 COVERAGE-13          PIC S9(5)V99   USAGE COMP-3.         00000460
R04342     10 COVERAGE-14          PIC S9(1)V9999 USAGE COMP-3.         00000470
      ******************************************************************00000490
      * INDICATOR FIELDS FOR TABLE COVERAGE                            *00000500
      ******************************************************************00000510
       01  COVERAGE-INDICATOR-ARRAY.                                    00000520
           05  COVERAGE-INDICATORS OCCURS 14 TIMES PIC S9(4) COMP.      00000530
       01  FILLER REDEFINES COVERAGE-INDICATOR-ARRAY.                   00000540
           05  COV-CASENAME-IDNTY-IND         PIC S9(4) COMP.             000005
           05  COV-EMPLOYEE-EMP-NUM-IND     PIC S9(4) COMP.             00000560
           05  COV-TYPE-IND                 PIC S9(4) COMP.             00000570
           05  COV-PLAN-CODE-IND            PIC S9(4) COMP.             00000580
           05  COV-EXCEPTION-IND            PIC S9(4) COMP.             00000590
           05  COV-EFF-DATE-IND             PIC S9(4) COMP.             00000600
           05  COV-TERM-DATE-IND            PIC S9(4) COMP.             00000610
           05  COV-OVERRIDE-IND             PIC S9(4) COMP.             00000620
           05  COV-PREMIUM-IND              PIC S9(4) COMP.             00000630
           05  COV-INCEPTION-DATE-IND       PIC S9(4) COMP.             00000640
           05  COV-WAIT-PERIOD-IND          PIC S9(4) COMP.             00000650
           05  COV-PROVIDER-IND             PIC S9(4) COMP.             00000660
R04342     05  COV-EMPLOYER-CNTR-AMT-IND    PIC S9(4) COMP.             00000670
R04342     05  COV-EMPLOYER-CNTR-RATE-IND   PIC S9(4) COMP.             00000680
      ******************************************************************00000700
      * COBOL RECORD LAYOUT FOR TABLE COVERAGE                         *00000710
      ******************************************************************00000720
IDENT  01  COVERAGE-RECORD.                                             00000730
01010      05  COV-CASENAME-IDNTY      PIC X(8)      VALUE SPACES.        000007
01020      05  COV-EMPLOYEE-EMP-NUM  PIC S9(5)     VALUE ZEROS  COMP-3. 00000750
01030      05  COV-TYPE              PIC X(8)      VALUE SPACES.        00000760
01040      05  COV-PLAN-CODE         PIC X(2)      VALUE SPACES.        00000770
01050      05  COV-EXCEPTION         PIC X(1)      VALUE SPACES.        00000780
01060      05  COV-EFF-DATE          PIC X(10)     VALUE SPACES.        00000790
01070      05  COV-TERM-DATE         PIC X(10)     VALUE SPACES.        00000800
01080      05  COV-OVERRIDE          PIC X(1)      VALUE SPACES.        00000810
01090      05  COV-PREMIUM           PIC S9(5)V99  VALUE ZEROS  COMP-3. 00000820
           05  COV-INCEPTION-DATE    PIC X(10)     VALUE SPACES.        00000830
           05  COV-WAIT-PERIOD       PIC X(2)      VALUE SPACES.        00000840
           05  COV-PROVIDER          PIC X(9)      VALUE SPACES.        00000850
R04342     05  COV-EMPLOYER-CNTR-AMT PIC S9(5)V9(2)                     00000860
R04342                                             VALUE ZEROS  COMP-3. 00000870
R04342     05  COV-EMPLOYER-CNTR-RATE                                   00000880
R04342                               PIC S9(1)V9(4)                     00000890
R04342                                             VALUE ZEROS  COMP-3. 00000900
      ******************************************************************00000920
      * END OF COPYBOOK "COVERAGE"                                     *00000930
      ******************************************************************00000940
