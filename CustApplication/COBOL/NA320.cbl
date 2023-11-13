   CBL DATA(24)
900526*---------------------------------------------------------------*
900526* DATA(24) OPTION IS FOR ABOVE-TO-BELOW-THE-LINE DYNAMIC CALLS  *
900526*---------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NA320.
 INFO *---------------START OF INFORMATION BLOCK----------------------*
 INFO *---------------------------------------------------------------*
 INFO * PLACE AN 'X' NEXT TO THE OPTION THAT MATCHES YOUR NEEDS FOR   *
 INFO * THIS PROGRAM.                                                 *
 INFO *---------------------------------------------------------------*
 INFO *               COMPILE INFORMATION BLOCK                       *
 INFO *                                                               *
 INFO *   BATCH                                                       *
 INFO *   CICS                                                        *
 INFO * _ COBOL II CICS                                               *
 INFO * _ COBOL II EXCI                                               *
 INFO * _ HLASM    CICS                                               *
 INFO *                                                               *
 INFO *------------------END OF INFORMATION BLOCK---------------------*
           EJECT
       DATE-WRITTEN. 03/22/88.
      *REMARKS. NA320 IS THE CUSTOMER INFORMATION INQUIRE PROGRAM. THIS
      *         PROGRAM INQUIRE NAME AND ADDRESS ENTRIES ON THE AGNTNAME
      *         AND CASENAME TABLES.
      *
      *------------------------PROGRAM PURPOSE-------------------------*
      *                                                                *
      *  PROGRAM TITLE: NA320                                          *
      *                                                                *
      *------------------------PROGRAM CHANGES-------------------------*
      *                                                                *
      *  DATE      BY     CHANGE DESCRIPTION                   RSA/SIR *
      * --------  ----   -----------------------------------   ------- *
      *****LAST CHANGE**************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
R9031A 01  WS-EMAIL-STATUS             PIC X(01) VALUE SPACE.
COBOLU 01  WS-ISEIB-ROUTINE            PIC X(8) VALUE 'ISEIB   '.
COBOLU 01  WS-PSIGSFC                  PIC X(8) VALUE 'PSIGSFC '.
R08986 01  WS-PROGRAM-NAME             PIC X(8) VALUE 'NA320   '.
Y2KIMR*
Y2KIMR* IMR CHANGE BEGIN
Y2KIMR*
Y2KIMR 01  Y2K-WK-CUTOFF-YR-X               PIC X(2) VALUE '50'.
Y2KIMR*
Y2KIMR 01  Y2K-WK-DATE1-X.
Y2KIMR     05  Y2K-WK-DATE1-X-CC            PIC X(2).
Y2KIMR     05  Y2K-WK-DATE1-X-YYMMDD        PIC X(6).
Y2KIMR     05  Y2K-WK-DATE1R-X-YYMMDD REDEFINES Y2K-WK-DATE1-X-YYMMDD.
Y2KIMR         10  Y2K-WK-DATE1R-X-YY       PIC X(2).
Y2KIMR         10  Y2K-WK-DATE1R-X-MM       PIC X(2).
Y2KIMR         10  Y2K-WK-DATE1R-X-DD       PIC X(2).
Y2KIMR*
Y2KIMR 01  Y2K-WK-DATE1RR-X REDEFINES Y2K-WK-DATE1-X.
Y2KIMR     05  Y2K-WK-DATE1RR-X-CCYY        PIC X(4).
Y2KIMR     05  Y2K-WK-DATE1RR-X-MMDD        PIC X(4).
Y2KIMR*
Y2KIMR 01  Y2K-WK-DATE1RRR-X REDEFINES Y2K-WK-DATE1-X PIC X(8).
Y2KIMR*
Y2KIMR 01  Y2K-WK-CUTOFF-YR-9               PIC 9(2) VALUE 50.
Y2KIMR*
Y2KIMR 01  Y2K-WK-DATE1-9.
Y2KIMR     05  Y2K-WK-DATE1-9-CC            PIC 9(2).
Y2KIMR     05  Y2K-WK-DATE1-9-YYMMDD        PIC 9(6).
Y2KIMR     05  Y2K-WK-DATE1R-9-YYMMDD REDEFINES Y2K-WK-DATE1-9-YYMMDD.
Y2KIMR         10  Y2K-WK-DATE1R-9-YY       PIC 9(2).
Y2KIMR         10  Y2K-WK-DATE1R-9-MM       PIC 9(2).
Y2KIMR         10  Y2K-WK-DATE1R-9-DD       PIC 9(2).
Y2KIMR*
Y2KIMR 01  Y2K-WK-DATE1RR-9 REDEFINES Y2K-WK-DATE1-9.
Y2KIMR     05  Y2K-WK-DATE1RR-9-CCYY        PIC 9(4).
Y2KIMR     05  Y2K-WK-DATE1RR-9-MMDD        PIC 9(4).
Y2KIMR*
Y2KIMR 01  Y2K-WK-DATE1RRR-9 REDEFINES Y2K-WK-DATE1-9 PIC 9(8).
Y2KIMR*
Y2KIMR 01  Y2K-WK-DATE2-9.
Y2KIMR     05  Y2K-WK-DATE2-9-CC            PIC 9(2).
Y2KIMR     05  Y2K-WK-DATE2-9-YYMMDD        PIC 9(6).
Y2KIMR     05  Y2K-WK-DATE2R-9-YYMMDD REDEFINES Y2K-WK-DATE2-9-YYMMDD.
Y2KIMR         10  Y2K-WK-DATE2R-9-YY       PIC 9(2).
Y2KIMR         10  Y2K-WK-DATE2R-9-MM       PIC 9(2).
Y2KIMR         10  Y2K-WK-DATE2R-9-DD       PIC 9(2).
Y2KIMR*
Y2KIMR 01  Y2K-WK-DATE2RR-9 REDEFINES Y2K-WK-DATE2-9.
Y2KIMR     05  Y2K-WK-DATE2RR-9-CCYY        PIC 9(4).
Y2KIMR     05  Y2K-WK-DATE2RR-9-MMDD        PIC 9(4).
Y2KIMR*
Y2KIMR 01  Y2K-WK-DATE2RRR-9 REDEFINES Y2K-WK-DATE2-9 PIC 9(8).
Y2KIMR*
Y2KIMR 01 Y2K-WK-VARIABLE1.
Y2KIMR    05 FILLER                         PIC X(02).
Y2KIMR    05 Y2K-WK-VARIABLE2               PIC 9(08).
Y2KIMR    05 Y2K-WK-VARIABLER  REDEFINES  Y2K-WK-VARIABLE2.
Y2KIMR       10 Y2K-WK-VARIABLE2-MM         PIC 9(02).
Y2KIMR       10 Y2K-WK-VARIABLE2-DD         PIC 9(02).
Y2KIMR       10 Y2K-WK-VARIABLE2-CC         PIC 9(02).
Y2KIMR       10 Y2K-WK-VARIABLE2-YY         PIC 9(02).
Y2KIMR*
Y2KIMR 01 Y2K-WK-VARIABLE3.
Y2KIMR    05 Y2K-WK-VARIABLE3-AA            PIC 9(02).
Y2KIMR    05 Y2K-WK-VARIABLE3-BB.
Y2KIMR       10 Y2K-WK-VARIABLE3-BB-MM      PIC 9(02).
Y2KIMR       10 Y2K-WK-VARIABLE3-BB-DD      PIC 9(02).
Y2KIMR       10 Y2K-WK-VARIABLE3-BB-YY      PIC 9(02).
Y2KIMR*
Y2KIMR     05  Y2K-WK-DATE-TEN.
Y2KIMR         10  Y2K-WK-MONTH                 PIC 99     VALUE ZERO.
Y2KIMR         10  Y2K-WK-SLASH-1               PIC X.
Y2KIMR         10  Y2K-WK-DAY                   PIC 99     VALUE ZERO.
Y2KIMR         10  Y2K-WK-SLASH-2               PIC X.
Y2KIMR         10  Y2K-WK-CENT                  PIC 99     VALUE ZERO.
Y2KIMR         10  Y2K-WK-YEAR                  PIC 99     VALUE ZERO.
Y2KIMR*
Y2KIMR* IMR CHANGE END
Y2KIMR*
       77  WS-LINK-LENGTH                   PIC S9(8) VALUE +0    COMP.
       77  WS-ZERO-LENGTH                   PIC S9(4) VALUE +0    COMP.
       77  WS-COMM-LENGTH                   PIC S9(4) VALUE +600  COMP.
900837 77  WS-COMM-DB2-LENGTH               PIC S9(4) VALUE +0    COMP.
       77  WS-AUD-LENGTH                    PIC S9(4) VALUE +3800 COMP.
910687 77  WS-FIN-LENGTH                    PIC S9(4) VALUE +1024 COMP.
       77  WS-TUTOR-COMM-LENGTH             PIC S9(4) VALUE +906  COMP.
MAB    77  WS-BROKER-LENGTH                 PIC S9(4) VALUE +723  COMP.
       77  WS-AG-KEY-LENGTH                 PIC S9(4) VALUE +8    COMP.
       77  IS020-LEN                        PIC S9(4) VALUE +1569 COMP.
       77  DISPLAY-COUNT                    PIC S9(4) VALUE ZERO  COMP.

       01  CASE-MASTER-RECORD.
           05  CASEX-CARR-AFFL-CODE      PIC  X(2).
           05  CASEX-CARRIER-CODE        PIC  X(2).
           05  CASEX-CARRIER-IND         PIC S9(04)    COMP.
           05  CASEX-CASE-NUM            PIC  X(6).
           05  CASEX-PREV-CARRIER        PIC  X(2).
           COPY AUDCICS.
           COPY CAWSINC.
           COPY DEMOCOMM.
           COPY TURB9999.
           COPY TURB8991.
           COPY TURB8997.
           COPY TUB8997A.
           COPY TURB2006.
           COPY TURB0705.
           COPY TURB0153.
           COPY TURB0043.
RA4254     COPY TURB0106.
           COPY TURB0161.
R02539     COPY TURB0525.
R02539     COPY TURB0091.
      *---------------------------------------------------------------*
900526*    COPY TUTORCOM.                                             *
      *---------------------------------------------------------------*
900526*    COPY TU003COM.
           COPY CICSWS.
           COPY TURBINC.
           COPY TURBDATA.
      *    COPY IS020COM.

R8285A*    COPY COVREWS.
Y2KIMR     COPY EDITCODE.
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE W/S REFERENCES BEGIN
Y2KIMR*
Y2KIMR*01  WS-DATEPRG2-PARAMETERS.
Y2KIMR*    COPY DATEPARM.
Y2KIMR*
Y2KIMR 01  WS-DATEPRG2-PARAMETERS.
Y2KIMR     COPY DTEPARM2.
Y2KIMR*
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE W/S REFERENCES END
Y2KIMR*
       01  AUDIT-COMM-AREA.
           COPY AUDCOMM.
       01  IO-COMM-AREA.
cobolu*    COPY IOAREA.
cobolu     COPY IOAREA2K.

R03749     COPY EDITCOMM.
       01  WS-WORK-AREA.
Y2KIMR*
Y2KIMR* IMRGLOBAL CHANGE DATE ROUTINE W/S REFERENCES BEGIN
Y2KIMR*
Y2KIMR*    05  COB2-DATEPRG2                PIC X(8)  VALUE 'DATEPRG2'.
Y2KIMR*
Y2KIMR     05  COB2-DATEPRG2                PIC X(8)  VALUE 'DTEPROG2'.

R03749     05  CASE-GE01600-PRODUCT-NUMBER  PIC X(4).
R03749     05  CASE-GE01600-PRODUCT-LINE    PIC X(3).
R03749     05  WS-HLTH-POLICY          PIC XX.
R03749     05  WS-PLAN-CODE.
R03749         10  WS-PLAN-1           PIC X.
R03749         10  WS-PLAN-2           PIC X.
R03749     05  WS-DENTAL-PLAN          PIC X(2) VALUE SPACES.
R03749     05  HOLD-SQL-DATE.
R03749         10  HOLD-YYYY.
R03749             15  HOLD-19           PIC X(02).
R03749             15  HOLD-SQL-YY       PIC 99.
R03749         10  HOLD-DASH2            PIC X(01) VALUE '-'.
R03749         10  HOLD-SQL-MM           PIC 99.
R03749             88  VALID-MM          VALUE 1 THRU 12.
R03749         10  HOLD-DASH1            PIC X(01) VALUE '-'.
R03749         10  HOLD-SQL-DD           PIC 99.
R03749             88  VALID-DD          VALUE 1 THRU 31.
R03749     05  WS-INCEPTION-DATE   REDEFINES HOLD-SQL-DATE PIC X(10).
R03749     05  WS-HOLD-HLTH-OPTION     PIC X(02)      VALUE SPACES.
901057     05  FIELD-EDIT-MODULE                 PIC X(8).
R03749     05  WS-GE01600-FILLER-FIELDS.
R03749         10  WS-GE01600-FLD2-NOT-NEEDED    PIC X(2).
R03749         10  WS-GE01600-FLD3-NOT-NEEDED    PIC X(2).
R03749         10  WS-GE01600-FLD4-NOT-NEEDED    PIC X(2).
R03749         10  WS-GE01600-FLD6-NOT-NEEDED    PIC X(10).
R03749         10  WS-GE01600-FLD7-NOT-NEEDED    PIC X(2).
R03749         10  WS-GE01600-FLD8-NOT-NEEDED    PIC X(2).
R03749         10  WS-GE01600-FLD18-NOT-NEEDED   PIC S9(5) COMP-3.
R03749         10  WS-GE01600-FLD19-NOT-NEEDED   PIC X(10).

           05  CASE-FETCH-SW                PIC X.
           05  ERROR-STATUS                 PIC S9(8) COMP VALUE +0.
           05  WS-GASET                     PIC S9(9) COMP.
           05  WS-GALENGTH                  PIC S9(4) COMP.
           05  WS-CICS-RESP                 PIC S9(4) COMP.
           05  WS-CUSTOMER-STATUS           PIC X     VALUE SPACES.
               88  INVALID-CUSTOMER-STATUS            VALUE 'D' 'C'.
           05  WS-TS-QUEUE-NAME.
               10  WS-TS-QUEUE-TRANID       PIC X(4).
               10  WS-TS-QUEUE-TERMID       PIC X(4).
900526     05  WS-REQID-NAME.
900526         10  WS-R-TERMID                   PIC X(4).
900526         10  FILLER                        PIC X(4)  VALUE 'BRSE'.
           05  WS-TS-ITEM                   PIC S9(4) VALUE +1   COMP.
           05  WS-HEX80                     PIC S9(4) VALUE +128 COMP.
           05  WS-HEX80-REDEF REDEFINES WS-HEX80.
               10  FILLER                   PIC X.
               10  HEX80                    PIC X.
           05  WS-NULL-FIELD                PIC X(72).
           05  WS-ERROR-FIELDS.
               10  WS-C9999-ERROR-CODE      PIC X(5).
               10  WS-C9999-SEVERITY-CODE   PIC X.
COBOLU**       10  FILLER                   PIC X.
COBOLU         10  WS-C9999-FILLER          PIC X.
               10  WS-C9999-ERROR-MESSAGE   PIC X(30).
           05  WS-SUBCRIPT COMP.
               10  ACTION-SUB               PIC S99.
           05  WS-HOLD-MESSAGE              PIC X(5)   VALUE SPACE.
MANJU      05  WS-DB2I-MESSAGE              PIC ZZZZZ9.
           05  WS-MESSAGE-NUMBER1           PIC X(5)   VALUE SPACE.
           05  WS-MESSAGE-NUMBER2           PIC X(5)   VALUE SPACE.
           05  WS-MESSAGE-NUMBER3           PIC X(5)   VALUE SPACE.
           05  WS-MESSAGE-NUMBER4           PIC X(5)   VALUE SPACE.
           05  LOWER-CASE                   PIC X(26)
                                   VALUE 'abcdefghijklmnopqrstuvwxyz'.
           05  UPPER-CASE                   PIC X(26)
                                   VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05  WS-SYSTEM-CODE.
               10  WS-SYSTEM-CODE-2BYTES    PIC X(2).
               10  FILLER                   PIC X(2).
           05  WS-ACTION-CODE.
               10  WS-ACTION-CODE-5BYTES    PIC X(5).
               10  FILLER                   PIC X.
           05  WS-DATE-R                    PIC 9(6)   VALUE ZERO.
           05  WS-DATE.
               10  WS-MM                    PIC 99     VALUE ZERO.
               10  WS-DD                    PIC 99     VALUE ZERO.
               10  WS-YY                    PIC 99     VALUE ZERO.
           05  WS-DATE-A REDEFINES WS-DATE.
               10  WS-MM-A                  PIC XX.
               10  WS-DD-A                  PIC XX.
               10  WS-YY-A                  PIC XX.
           05  WS-BIRTH-DATE.
               10  BR-CC                    PIC XX.
               10  BR-YY                    PIC XX.
               10  FILLER                   PIC X      VALUE '-'.
               10  BR-MM                    PIC XX.
               10  FILLER                   PIC X      VALUE '-'.
               10  BR-DD                    PIC XX.
           05  WS-EFFECTIVE-DATE.
               10  EF-CC                    PIC XX.
               10  EF-YY                    PIC XX.
               10  FILLER                   PIC X      VALUE '-'.
               10  EF-MM                    PIC XX.
               10  FILLER                   PIC X      VALUE '-'.
               10  EF-DD                    PIC XX.
           05  WS-CHANGE-DATE.
               10  CO-CC                    PIC XX.
               10  CO-YY                    PIC XX.
               10  FILLER                   PIC X      VALUE '-'.
               10  CO-MM                    PIC XX.
               10  FILLER                   PIC X      VALUE '-'.
               10  CO-DD                    PIC XX.
           05  WS-DATE-EIGHT.
               10  WS-MONTH                 PIC 99     VALUE ZERO.
               10  SLASH-1                  PIC X.
               10  WS-DAY                   PIC 99     VALUE ZERO.
               10  SLASH-2                  PIC X.
               10  WS-YEAR                  PIC 99     VALUE ZERO.
           05  WS-COMPARE-DATE.
               10  WS-COMPARE-YY            PIC 99     VALUE ZERO.
               10  WS-COMPARE-MM            PIC 99     VALUE ZERO.
               10  WS-COMPARE-DD            PIC 99     VALUE ZERO.
           05  HOLD-NEXT-CIM                PIC 9(8) VALUE ZERO.
           05  HOLD-NEXT-CIM-R REDEFINES HOLD-NEXT-CIM
                                            PIC X(8).

           05  FWAC-ADDR                         PIC S9(08)  COMP.
           05  FWAC-PNTR REDEFINES FWAC-ADDR USAGE IS POINTER.
DFJ        05  STATUS-CHANGED-SW            PIC X      VALUE 'N'.
DFJ        05  STATE-CHANGED-SW             PIC X      VALUE 'N'.
DFJ        05  ZIP-CHANGED-SW               PIC X      VALUE 'N'.
890989     05  ZIP-PLUS4-CHANGED-SW         PIC X      VALUE 'N'.
           05  PHONE-CHANGED-SW             PIC X.
           05  DISPLAY-CHANGED-SW           PIC X.
COBOLU     05  WS-APPLID                    PIC X(08).
890989     05  WS-HOLD-CASE-STATE           PIC X(02).
890989     05  WS-HOLD-CASE-ZIP             PIC 9(05).
890989     05  WS-HOLD-CASE-ZIP-PLUS4       PIC 9(04).
890989     05  WS-HOLD-CASE-NUM             PIC X(06).
890989     05  WS-HOLD-CSN-STATE            PIC X(02).
890989     05  WS-HOLD-CSN-ZIP              PIC X(05).
890989     05  WS-HOLD-CSN-ZIP-PLUS4        PIC X(04).
890989     05  WS-CASE-EMP-NUM.
890989         10  WS-CASE-NUM         PIC X(6).
890989         10  WS-EMP-NUM          PIC 9(5).
890989     05  WS-EMP-EOF                   PIC X(1) VALUE 'N'.
890989     05  WS-EMP-UPDATE                PIC X(1) VALUE 'N'.
           05  WS-CASE-AREA-PHONE.
               10  WS-CASE-AREA             PIC X(3).
               10  WS-CASE-PHONE            PIC X(7).
           05  WS-PHONE.
               10  WS-PHONE3                PIC X(3).
               10  WS-PHONE4                PIC X(4).
           05  WS-MAP-DATE.
               10  FILLER                   PIC 99.
               10  WS-MAP-BIF               PIC 9(6).
           05  WS-CIM-PHONE.
               10  WS-CIM-PHONE-AREA        PIC X(3).
               10  WS-DASH1                 PIC X.
               10  WS-CIM-PHONE-EXCH        PIC X(3).
               10  WS-DASH2                 PIC X.
               10  WS-CIM-PHONE-NUMB        PIC X(4).
           05  WS-TIME-FIELD                PIC S9(8) COMP.
           05  WC-TODAYS-DATE.
               10  WC-TODAYS-MM             PIC XX.
               10  FILLER                   PIC X.
               10  WC-TODAYS-DD             PIC XX.
               10  FILLER                   PIC X.
               10  WC-TODAYS-YY             PIC XX.
           05  WS-FINALST-REAS-CODE.
               10  WS-FINALST-BYTE1         PIC X.
               10  FILLER                   PIC XX.
           05  WS-CIM-NUMBER                PIC X(8).
           05  WS-EDIT-SW                   PIC X.
           05  WS-DUPLICATE-SW              PIC X.
           05  OPER-ID                      PIC X(3).
           05  WS-NUMERIC-CHECK.
               10 WS-NUMERIC-CHECK-BYTE OCCURS 20 TIMES
                                            PIC X.
           05  WS-NUMERIC-SW                PIC X.
R04023     05  ORIGINAL-STATE               PIC XX.
R02539     05  POSTAL-CODE                  PIC X(9).
R02539     05  COUNTRY-CODE                 PIC X(3).
R02539     05  IND-POSTAL-CODE              PIC S9(4) COMP.
R02539     05  IND-COUNTRY-CODE             PIC S9(4) COMP.
           05  WS-BROKER-COMMAREA.
               10  WS-AGENT-COMMAREA        PIC X(600).
MAB            10  WS-BROKER-FIELDS         PIC X(121).
               10  WS-SYSTEM-ADD-TYPE       PIC XX.
           05  WS-STATE-ZIP.
               10  WS-STATE                 PIC X(2).
               10  FILLER                   PIC X VALUE SPACE.
               10  WS-ZIP                   PIC X(5).
           05  WS-DB2-DATE.
               10  WS-DB2-CC                PIC X(2).
Y2KIMR*
Y2KIMR* IMR CHANGE BEGIN
Y2KIMR*
Y2KIMR         10  WS-DB2-CC-R REDEFINES WS-DB2-CC
Y2KIMR                                      PIC 99.
Y2KIMR*
Y2KIMR* IMR CHANGE END
Y2KIMR*
               10  WS-DB2-YY                PIC X(2).
               10  WS-DB2-YY-R REDEFINES WS-DB2-YY
                                            PIC 99.
               10  FILLER                   PIC X.
               10  WS-DB2-MM                PIC X(2).
               10  WS-DB2-MM-R REDEFINES WS-DB2-MM
                                            PIC 99.
               10  FILLER                   PIC X.
               10  WS-DB2-DD                PIC X(2).
               10  WS-DB2-DD-R REDEFINES WS-DB2-DD
                                            PIC 99.
           05  WS-DB2-TIMESTAMP.
               10  WS-TS-CC                 PIC X(2).
               10  WS-TS-YY                 PIC X(2).
               10  WS-TS-YY-R REDEFINES WS-TS-YY
                                            PIC 99.
               10  FILLER                   PIC X.
               10  WS-TS-MM                 PIC X(2).
               10  WS-TS-MM-R REDEFINES WS-TS-MM
                                            PIC 99.
               10  FILLER                   PIC X.
               10  WS-TS-DD                 PIC X(2).
               10  WS-TS-DD-R REDEFINES WS-TS-DD
                                            PIC 99.
               10  FILLER                   PIC X(16).
           05  WS-RECORD-NOTFND             PIC X.
           05  WS-COMPANY-NAME.
               10  WS-COMPANY-NAME-HIGH-ORDR   PIC X(22) VALUE SPACES.
               10  WS-COMPANY-NAME-LOW-ORDR    PIC X(08) VALUE SPACES.
           05  WS-ENTITY-LITERAL.
               10  WS-LITERAL-HIGH-ORDR        PIC X(02) VALUE SPACES.
C08784****     10  WS-LITERAL-LOW-ORDR         PIC X(13) VALUE SPACES.
C08784         10  WS-LITERAL-LOW-ORDR         PIC X(08) VALUE SPACES.
           05  WS-C                         PIC X(1) VALUE 'C'.
           05  WS-Y                         PIC X(1) VALUE 'Y'.
           05  WS-SPACE                     PIC X(1) VALUE SPACE.
           05  WS-SPACES                    PIC X(2) VALUE SPACES.
           05  WS-ZEROES-5                  PIC X(5) VALUE '00000'.
           05  WS-ZEROES-4                  PIC X(5) VALUE '0000'.

RA4254*01 WS-GETT0106                      PIC X(08) VALUE 'GETT0106'.

R01142 01  WS-ADDRESS1.
R01142     05  WS-ADDRESS1-22          PIC X(22).
R01142     05  WS-ADDRESS1-REMAINDER   PIC X(08).
R01142 01  WS-ADDRESS2.
R01142     05  WS-ADDRESS2-22          PIC X(22).
R01142     05  WS-ADDRESS2-REMAINDER   PIC X(08).

900526 01  TCTUAL                      PIC S9(4) COMP.
900526     88  INVALID-TCTUAL                    VALUE 0 THRU 135.

980714*01  BILLING-REQUEST-RECORDX.
980714*    COPY BILLREQ.
GRB   *01  CASE-RECORD.
GRB   *    COPY CASECOBQ.
890989*01  EMPLOYEE-RECORD.
890989*    COPY EMPMSTQ.
890989*01  OLD-EMP-REC.
890989*    05  FILLER                       PIC X(555).

       01  WS-SCZ-COUNTY-CODE               PIC X(5).
       01  WS-SCZ-COUNTY-CODE-N REDEFINES
           WS-SCZ-COUNTY-CODE               PIC 9(5).

       01  WS-SCZ-ZIP                       PIC 9(5).
       01  WS-SCZ-ZIP-C         REDEFINES
           WS-SCZ-ZIP                       PIC X(5).

       01  WSQ-COMMAREA.
           02  WSQ-COMM-FIELDS.
               05  WSQ-CICS-COMMAREA-LENGTH PIC S9(4) COMP VALUE +600.
               05  WSQ-SYSTEM-CODE          PIC X(02).
               05  WSQ-ACTION-CODE          PIC X(05).
               05  WSQ-CUSTOMER-INFO        PIC X(40).
               05  WSQ-CUST-TOKEN-COUNT     PIC S9.
               05  WSQ-CUSTOMER-INFO1       PIC X(40).
               05  WSQ-CUSTOMER-INFO2       PIC X(30).
               05  WSQ-CUSTOMER-INFO3       PIC X(30).
               05  WSQ-CUSTOMER-INFO4       PIC X(30).
               05  WSQ-CUSTOMER-INFO5       PIC X(30).
               05  WSQ-CUSTOMER-INFO6       PIC X(30).
               05  FILLER                   PIC X(46).
               05  WSQ-PREVIOUS-TRANID      PIC X(4).
               05  WSQ-CMDLINE-CHANGED      PIC X(1).
               05  WSQ-KEY-CHANGED          PIC X(1).
               05  WSQ-CIM-NUMBER           PIC X(8).
               05  WSQ-MAIL-LINE-COUNT      PIC 9.
               05  WSQ-MAIL-LINE            PIC X(30) OCCURS 5 TIMES.
               05  WSQ-LAST-ITEM-NUM-BROWSED PIC S9(3) COMP-3.
               05  WSQ-CUST-TYPE            PIC X(2).
               05  WSQ-CUST-STATUS          PIC X.
               05  WSQ-CUST-PHONE.
                   10  WSQ-CUST-PHONE-AREA  PIC X(3).
                   10  WSQ-CUST-PHONE-EXCH  PIC X(3).
                   10  WSQ-CUST-PHONE-NUMB  PIC X(4).
               05  FILLER                   PIC X(25).
               05  WSQ-MSG-COUNT            PIC 9.
               05  WSQ-MSG-ID               PIC X(5) OCCURS 4 TIMES.
               05  WSQ-MSG-MAX-SEVERITY     PIC X(1).
               05  WSQ-NEXT-FUNCTION        PIC X(2).
               05  WSQ-CURSOR-POSN          PIC S9(4) COMP.
               05  FILLER                   PIC X(83).
           02  WSQ-AGNTNAME.
               05 WSQ-CIM                   PIC X(8).
               05 WSQ-LAST-NAME             PIC X(20).
               05 WSQ-FIRST-NAME            PIC X(15).
               05 WSQ-MIDDLE-NAME           PIC X(15).
               05 WSQ-PREFIX                PIC X(4).
               05 WSQ-SUFFIX1               PIC X(4).
               05 WSQ-SUFFIX2               PIC X(4).
               05 WSQ-COMPANY-IND           PIC X(1).
               05 WSQ-COMPANY-IN-ADDRESS    PIC X(1).
               05 WSQ-COMPANY-NAME          PIC X(30).
               05 WSQ-DISPLAY-NAME          PIC X(30).
               05 WSQ-NICKNAME              PIC X(10).
R04023         05 WSQ-ISSUE-STATE           PIC X(02).
               05 WSQ-ADDRESS1              PIC X(30).
               05 WSQ-ADDRESS2              PIC X(30).
               05 WSQ-CITY                  PIC X(30).
               05 WSQ-STATE                 PIC X(2).
               05 WSQ-ZIP                   PIC X(5).
               05 WSQ-ZIP-PLUS4             PIC X(4).
               05 WSQ-COUNTY-CODE           PIC X(5).
               05 WSQ-AREA-CODE             PIC X(3).
               05 WSQ-PHONE                 PIC X(7).
               05 WSQ-PHONE-EXTENSION       PIC X(4).
               05 WSQ-SSN                   PIC X(9).
               05 WSQ-SEX                   PIC X(1).
Y2KIMR***      05 WSQ-BIRTH-DATE            PIC S9(6)    COMP-3.
Y2KIMR         05 WSQ-BIRTH-DATE            PIC S9(8)    COMP-3.
               05 WSQ-FINALST-REAS-CODE     PIC X(3).
               05 WSQ-FINALST-OVRD-IND      PIC X(1).
               05 WSQ-DUP-ADDR-OVRD-IND     PIC X(1).
               05 WSQ-EFFECTIVE-DATE        PIC S9(6)    COMP-3.
               05 WSQ-CHANGE-DATE           PIC S9(6)    COMP-3.
               05 WSQ-CHANGE-LOGON          PIC X(8).
               05 WSQ-ENTITY-TYPE           PIC X(2).
               05 WSQ-RECORD-STATUS         PIC X(1).
               05 WSQ-ALT-ADDRESS-IND       PIC X(1).
               05 WSQ-FUTURE-ADDRESS-IND    PIC X(1).
               05 WSQ-RECORD-ORIGIN         PIC X(1).
               05 WSQ-COMBINED-STATUS       PIC X(2).
               05 WSQ-SITE-CODE             PIC X(2).
               05 WSQ-NAME-KEY1             PIC X(8).
               05 WSQ-NAME-KEY2             PIC X(8).
               05 WSQ-NAME-KEY3             PIC X(2).
               05 WSQ-ADDRESS-KEY1          PIC X(10).
               05 WSQ-ASSOCIATION1          PIC X(5).
               05 WSQ-ASSOCIATION2          PIC X(5).
               05 WSQ-ASSOCIATION3          PIC X(5).
               05 WSQ-ENTITY-LITERAL        PIC X(10).
               05 WSQ-CASE-SW               PIC X.
               05 WSQ-DISP-IND              PIC X.
               05 WSQ-FINALIST-SW           PIC X.
900837         05 WSQ-FAX-AREA-CODE         PIC X(3).
900837         05 WSQ-FAX-PHONE             PIC X(7).
R00700         05 WSQ-EMAIL1                PIC X(50).
R04023         05 WSQ-POSTAL-CODE           PIC X(09).
R04023         05 WSQ-COUNTRY-CODE          PIC X(03).
R04023         05 WSQ-CARRIER-CODE          PIC XX.
R04023         05 WSQ-PREV-CARRIER          PIC XX.

       01  WS-TS-QUEUE.
           COPY NA200C01.
           02  COM-AGNTNAME.
               05 COM-CIM                   PIC X(8).
               05 COM-LAST-NAME             PIC X(20).
               05 COM-FIRST-NAME            PIC X(15).
               05 COM-MIDDLE-NAME           PIC X(15).
               05 COM-PREFIX                PIC X(4).
               05 COM-SUFFIX1               PIC X(4).
               05 COM-SUFFIX2               PIC X(4).
               05 COM-COMPANY-IND           PIC X(1).
               05 COM-COMPANY-IN-ADDRESS    PIC X(1).
               05 COM-COMPANY-NAME          PIC X(30).
               05 COM-DISPLAY-NAME          PIC X(30).
               05 COM-NICKNAME              PIC X(10).
R04023         05 COM-ISSUE-STATE           PIC X(02).
               05 COM-ADDRESS1              PIC X(30).
               05 COM-ADDRESS2              PIC X(30).
               05 COM-CITY                  PIC X(30).
               05 COM-STATE                 PIC X(2).
               05 COM-ZIP                   PIC X(5).
               05 COM-ZIP-PLUS4             PIC X(4).
               05 COM-COUNTY-CODE           PIC X(5).
               05 COM-AREA-CODE             PIC X(3).
               05 COM-PHONE                 PIC X(7).
               05 COM-PHONE-EXTENSION       PIC X(4).
               05 COM-SSN                   PIC X(9).
               05 COM-SEX                   PIC X(1).
Y2KIMR****     05 COM-BIRTH-DATE            PIC S9(6)    COMP-3.
Y2KIMR         05 COM-BIRTH-DATE            PIC S9(8)    COMP-3.
               05 COM-FINALST-REAS-CODE     PIC X(3).
               05 COM-FINALST-OVRD-IND      PIC X(1).
               05 COM-DUP-ADDR-OVRD-IND     PIC X(1).
               05 COM-EFFECTIVE-DATE        PIC S9(6)    COMP-3.
               05 COM-CHANGE-DATE           PIC S9(6)    COMP-3.
               05 COM-CHANGE-LOGON          PIC X(8).
               05 COM-ENTITY-TYPE           PIC X(2).
               05 COM-RECORD-STATUS         PIC X(1).
               05 COM-ALT-ADDRESS-IND       PIC X(1).
               05 COM-FUTURE-ADDRESS-IND    PIC X(1).
               05 COM-RECORD-ORIGIN         PIC X(1).
               05 COM-COMBINED-STATUS       PIC X(2).
               05 COM-SITE-CODE             PIC X(2).
               05 COM-NAME-KEY1             PIC X(8).
               05 COM-NAME-KEY2             PIC X(8).
               05 COM-NAME-KEY3             PIC X(2).
               05 COM-ADDRESS-KEY1          PIC X(10).
               05 COM-ASSOCIATION1          PIC X(5).
               05 COM-ASSOCIATION2          PIC X(5).
               05 COM-ASSOCIATION3          PIC X(5).
               05 COM-ENTITY-LITERAL        PIC X(10).
               05 COM-CASE-SW               PIC X.
               05 COM-DISP-IND              PIC X.
               05 COM-FINALIST-SW           PIC X.
900837         05 COM-FAX-AREA-CODE         PIC X(3).
900837         05 COM-FAX-PHONE             PIC X(7).
R00700         05 COM-EMAIL1                PIC X(50).
R02539         05 COM-POSTAL-CODE           PIC X(09).
R02539         05 COM-COUNTRY-CODE          PIC X(03).
R04023         05 COM-CARRIER-CODE          PIC XX.
R04023         05 COM-PREV-CARRIER          PIC XX.

11735A*    COPY BROKRV01.
11735A*    COPY BROKER.
900837*    COPY AGNTNV03.
R00700     COPY AGNTNV05.
900837     COPY CASNVW3.
DFJ   *    COPY CAMASV01.
11735A*    COPY CATMASTE.
900600*    COPY CASEXV01.
11735A*    COPY CASEMAST  REPLACING CASE- BY CASEX-.
890989*    COPY EMPLV03.
11735A*    COPY EMPLOYEE.
R03749*    COPY COVERAGE.

R8285A*    COPY COVEDITS.

R08986*    COPY RULEWS1.
           COPY STATECON.
R08986
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
           05  WS-SQL-ERROR-MESSAGE         PIC X(78).

           EXEC SQL DECLARE AGNTCUR CURSOR FOR
11735A*        SELECT *
11735A         SELECT
                    IDNTITY,
                    LAST_NAME,
                    FIRST_NAME,
                    MIDDLE_NAME,
                    PREFIX,
                    SUFFIX1,
                    SUFFIX2,
                    COMPANY_IND,
                    COMPANY_IN_ADDRESS,
                    COMPANY_NAME,
                    DISPLAY_NAME,
                    NICKNAME,
                    ADDRESS1,
                    ADDRESS2,
                    CITY,
                    STATE,
289843              ZIP,
                    ZIP_PLUS4,
                    COUNTY_CODE,
                    AREA_CODE,
                    PHONE,
                    PHONE_EXTENSION,
                    SSN,
                    SEX,
                    BIRTH_DATE,
                    FINALST_REAS_CODE,
                    FINALST_OVRD_IND,
                    DUP_ADDR_OVRD_IND,
                    EFFECTIVE_DATE,
                    CHANGE_DATE,
                    CHANGE_LOGON,
                    ENTITY_TYPE,
                    RECORD_STATUS,
                    ALT_ADDRESS_IND,
                    FUTURE_ADDRESS_IND,
                    RECORD_ORIGIN,
                    COMBINED_STATUS,
                    SITE_CODE,
                    NAME_KEY1,
                    NAME_KEY2,
                    NAME_KEY3,
                    ADDRESS_KEY1,
                    ASSOCIATION1,
                    ASSOCIATION2,
                    ASSOCIATION3
900837*        FROM AGNTNV03
R00700         FROM AGNTNAME
               WHERE IDNTITY = :WS-CIM-NUMBER
           END-EXEC.

KPL        EXEC SQL DECLARE CASECUR CURSOR FOR
R00700*        SELECT *
900837*        FROM CASENV03
R00700         SELECT
R00700             IDNTITY, LAST_NAME, FIRST_NAME, MIDDLE_NAME,
R00700             PREFIX, SUFFIX1, SUFFIX2, COMPANY_IND,
R00700             COMPANY_IN_ADDRESS, COMPANY_NAME, DISPLAY_NAME,
R00700             NICKNAME, ADDRESS1, ADDRESS2, CITY, STATE, ZIP,
R00700             ZIP_PLUS4, COUNTY_CODE, AREA_CODE, PHONE,
R00700             PHONE_EXTENSION, SSN, SEX, BIRTH_DATE,
R00700             FINALST_REAS_CODE, FINALST_OVRD_IND,
R00700             DUP_ADDR_OVRD_IND, EFFECTIVE_DATE,
R00700             CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,
R00700             RECORD_STATUS, ALT_ADDRESS_IND,
R00700             FUTURE_ADDRESS_IND, RECORD_ORIGIN,
R00700             COMBINED_STATUS, SITE_CODE, NAME_KEY1,
R00700             NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,
R00700             ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,
R00700***          FAX_AREA_CODE, FAX_PHONE, EMAIL,
R04023             FAX_AREA_CODE, FAX_PHONE, ORIGINAL_STATE, EMAIL,
R02539             COUNTRY_CODE, POSTAL_CODE
R00700         FROM CASENAME
               WHERE IDNTITY = :WS-CIM-NUMBER
           END-EXEC.

       01  WS-NAME-ADDRESS-DETAIL.
           COPY NA200C02.
      *01  SECURITY-AREA.
      *    COPY SECCOMMC.
      *    COPY SYSBUSY.
      *    COPY EI100C01.
           COPY NA320M1.
           COPY DFHAID.
           COPY ATTRB.

       LINKAGE SECTION.
       01  DFHCOMMAREA                      PIC X.

       01  WS-LINK-STORAGE.
           02  WS-LINK-SIX-HUNDRED          PIC X(600).
           02  WS-LINK-STUFF      OCCURS 0 TO 3496 TIMES
                                  DEPENDING ON WS-LINK-LENGTH
                                            PIC X.
       01  I-O-AREA.
           05  I-O-CASE-REC                 PIC X(766).

900526 01  TCTUAR.
900526     05  FILLER                  PIC X(36).
900526     05  TCTUA-DEMO-DATA.
900526         10  FILLER              PIC S9(4) COMP.
900526             88  INVALID-DEMO-DATA               VALUE 0.
900526         10  FILLER              PIC X(98).

      ******************************************************************

       PROCEDURE DIVISION.

       0010-BEGIN-PROGRAM.
           EXEC CICS HANDLE CONDITION ERROR(9999-CICS-ERROR)
                                      END-EXEC.

           MOVE LOW-VALUES TO NA320M1O.
C08784     INITIALIZE WS-C9999-FILLER IO-RETURN WS-ENTITY-LITERAL.
COBOLU     INITIALIZE  NA-COMM-DISPLAY-NAME
T3384                  NA-COMM-TITLE
COBOLU                 NA-COMM-RECORD-NUMBER
COBOLU                 NA-COMM-SEARCH-FIELD
COBOLU                 NA-COMM-PERSONAL-NAME
COBOLU                 NA-COMM-COMPANY-NAME
COBOLU                 NA-COMM-COMPANY-MAIL
COBOLU                 NA-COMM-BIRTH-MONTH
COBOLU                 NA-COMM-SSN
COBOLU                 NA-COMM-ADDRESS-1
COBOLU                 NA-COMM-ADDRESS-2
COBOLU                 NA-COMM-ADDRESS-3
COBOLU                 NA-COMM-CITY
COBOLU                 NA-COMM-STATE
COBOLU                 NA-COMM-ZIP
COBOLU                 NA-COMM-ZIP-PLUS4
COBOLU                 NA-COMM-PHONE-NUMBER
COBOLU                 NA-COMM-POSTAL-CODE
COBOLU                 NA-COMM-COUNTY-CODE
COBOLU                 NA-COMM-ASSOC-CODE
COBOLU                 NA-COMM-SITE-CODE
COBOLU                 NA-COMM-SEX
COBOLU                 NA-COMM-CREATION-DATE
COBOLU                 NA-COMM-EFFECTIVE-DATE
COBOLU                 NA-COMM-LAST-CHG-DATE
COBOLU                 NA-COMM-STATUS
COBOLU                 NA-COMM-MAIL-LINE1
COBOLU                 NA-COMM-MAIL-LINE2
COBOLU                 NA-COMM-MAIL-LINE3
COBOLU                 NA-COMM-MAIL-LINE4
COBOLU                 NA-COMM-MAIL-LINE5
COBOLU                 NA-COMM-MAIL-LINE6
COBOLU                 NA-COMM-LINE(1)
COBOLU                 NA-COMM-LINE(2)
COBOLU                 NA-COMM-LINE(3)
COBOLU                 NA-COMM-LINE(4)
COBOLU                 NA-COMM-LINE(5)
COBOLU                 NA-COMM-LINE(6).

       0030-WRITE-COST-JOURNAL.
      *---------------------------------------------------------------*
      * THIS IS THE COPY BOOK FOR THE COST JOURNAL USED TO DETERMINE  *
      * CUSTOMER INFORMATION MANAGEMENT CHARGES BY CARRIER.           *
      *---------------------------------------------------------------*
      *   MOVE SPACES TO WS-COST-CARR-CODE.
           COPY CAPRCINC.

       0040-READ-NAQ1-TS-QUEUE.

      *---------------------------------------------------------------*
      *    THIS IS THE TEMPORARY STORAGE QUEUE THAT IS MAINTAINED     *
      *    THROUGH OUT THE ENTIRE CIM SYSTEM. THE QUEUE IS            *
      *    ESTABLISHED THE FIRST TIME THE CUSTOMER SIGNS ON           *
      *    AND IS ONLY DELETED IF CICS COMES DOWN.                    *
      *---------------------------------------------------------------*

           MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.
           MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.
           EXEC CICS READQ TS QUEUE  (WS-TS-QUEUE-NAME)
                              SET    (ADDRESS OF WS-LINK-STORAGE)
                              LENGTH (WS-ZERO-LENGTH)
                              ITEM   (WS-TS-ITEM)
                              RESP   (WS-CICS-RESP)
                              END-EXEC.

           IF WS-CICS-RESP = DFHRESP(QIDERR)
               PERFORM 0350-WRITE-NAQ1-QUEUE
           ELSE
              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                  GO TO 9999-CICS-ERROR.

COBOLU     INITIALIZE COM-AGNTNAME.
C08784     INITIALIZE WSQ-AGNTNAME.

           MOVE WS-ZERO-LENGTH TO WS-ZERO-LENGTH.
           COMPUTE WS-LINK-LENGTH = (WS-ZERO-LENGTH - +600).
           MOVE WS-LINK-STORAGE TO WS-TS-QUEUE.
           MOVE COMM-IDNTITY TO WS-CIM-NUMBER.

       0060-PROCESS-MENU-SCREEN.
MANJU       DISPLAY 'INSIDE UPDATE NA320 PROCESS MENU PARA'
MANJU       DISPLAY 'WS-CIM-NUMBER' WS-CIM-NUMBER
           IF COMM-MSG-MAX-SEVERITY = 'E'
           OR  EIBTRNID = 'HCS2'
               MOVE COMM-MSG-ID(1) TO WS-MESSAGE-NUMBER1
               MOVE COMM-MSG-ID(2) TO WS-MESSAGE-NUMBER2
               MOVE COMM-MSG-ID(3) TO WS-MESSAGE-NUMBER3
               MOVE COMM-MSG-ID(4) TO WS-MESSAGE-NUMBER4
               GO TO 0200-SEND-NA320M1-MAP
      *    ELSE
LMB   *    IF  COMM-PREVIOUS-TRANID = 'NAT0'
LMB   *        AND COMM-CUST-TYPE = 'CA'
LMB   *        GO TO 0070-EDIT-COMMAND-LINE
           ELSE
      *---------------------------------------------------------------*
900526*    IF  EIBTRNID = 'TU01'                                      *
900526*        GO TO 0330-RETURN-FROM-TUTORIAL                        *
900526*    ELSE                                                       *
      *---------------------------------------------------------------*
           IF  COMM-NEXT-FUNCTION = '00'
               GO TO 0080-RETURN-FROM-ROUTER
           ELSE
           IF  EIBAID = DFHENTER
               GO TO 0080-RETURN-FROM-ROUTER
           ELSE
           IF  EIBAID = DFHCLEAR
               GO TO 0340-PROCESS-SCREEN-CLEAR
      *    ELSE
      *    IF  EIBAID = DFHPF18
R01181*              OR DFHPF6
      *        GO TO 0320-PROCESS-TUTORIAL-REQUEST
      *    ELSE
      *    IF  EIBAID = DFHPA1
      *        GO TO 0370-RETURN-TO-MT-BILLION
      *    ELSE
      *    IF  EIBAID = DFHPF2
      *        GO TO 0090-EXECUTE-STACK-FUNCTION
           ELSE
           IF  EIBAID = DFHPF3
               GO TO 0100-RETURN-TO-BROWSE
           ELSE
               GO TO 0360-WRONG-KEY-HIT.

       0070-EDIT-COMMAND-LINE.

COBOLU     MOVE LOW-VALUES TO NA320M1I.

           EXEC CICS RECEIVE MAP    ('NA320M1')
                             RESP   (WS-CICS-RESP)
                             END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)
                GO TO 9999-CICS-ERROR.

           PERFORM 0110-UPDATE-COMMAND-LINE.

      *---------------------------------------------------------------*
      * IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE = 'Y' THEN SOMETHING*
      * ON THE COMMAND LINE CHANGED AND THE ROUTER SHOULD BE STARTED. *
      *---------------------------------------------------------------*


           IF COMM-NEXT-FUNCTION = '01'
                   PERFORM 0120-UPDATE-CIM-MAP-FIELDS
      *            PERFORM 0130-EDIT-NA320M1-MAP
                   GO TO 0200-SEND-NA320M1-MAP.

           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED  = 'Y'
               GO TO 0310-INITIATE-ROUTER.

961037     IF  ENTITY-TYPE = 'RP'
961037         MOVE ATTRB-PROT-ASKIP TO MAP-ENTITY-TYPEA
961037     END-IF.
R03179     IF ENTITY-TYPE = 'CA'
R03179         MOVE ATTRB-PROT-ASKIP        TO MAP-EMAIL1A MAP-EMAIL2A
R03179     END-IF.

      *---------------------------------------------------------------*
      * IF COMM-NEXT-FUNCTION = '01' THEN THE CUSTOMER IS RE-ENTERING *
      * THE PROGRAM AND MUST CHECK FOR FOR ERRORS THEN ADD THE RECORD *
      *---------------------------------------------------------------*

       0080-RETURN-FROM-ROUTER.
      *---------------------------------------------------------------*
      *  IF COMM-NEXT-FUNCTION = '00' SEND OUT THE SYSTEM SELECTION   *
      *  MAP                                                          *
      *---------------------------------------------------------------*

           GO TO 0190-SEND-NA320M1-FIRST-TIME.

       0100-RETURN-TO-BROWSE.

           MOVE SPACES   TO COMM-SYSTEM-CODE.
           MOVE SPACES   TO COMM-ACTION-CODE.
           MOVE SPACES   TO COMM-CUSTOMER-INFO.

           MOVE 'NAUI'            TO COMM-PREVIOUS-TRANID.
           MOVE '00'              TO COMM-NEXT-FUNCTION.
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)
                          COMM-MSG-ID (3) COMM-MSG-ID (4).

           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.
           DISPLAY 'WS-TS-QUEUE-NAME:' WS-TS-QUEUE-NAME.
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)
                               FROM   (WS-TS-QUEUE)
                               LENGTH (WS-ZERO-LENGTH)
                               ITEM   (WS-TS-ITEM)
                               RESP   (WS-CICS-RESP)
                               REWRITE
                               MAIN
                               END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           EXEC CICS START TRANSID('NAM1')
                           TERMID (EIBTRMID)
                           RESP   (WS-CICS-RESP)
                           END-EXEC
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           EXEC CICS RETURN
                     END-EXEC.

       0110-UPDATE-COMMAND-LINE.

           IF MAP-SYSTEM-CDF = HEX80
               MOVE SPACES TO COMM-SYSTEM-CODE
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED
           ELSE
               IF MAP-SYSTEM-CDL > ZERO
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED
                  MOVE MAP-SYSTEM-CDI TO COMM-SYSTEM-CODE.

           IF MAP-ACTION-CDF = HEX80
               MOVE SPACES TO COMM-ACTION-CODE
               MOVE 'Y'    TO COMM-CMDLINE-CHANGED
           ELSE
               IF MAP-ACTION-CDL > ZERO
                  MOVE 'Y'            TO COMM-CMDLINE-CHANGED
                  MOVE MAP-ACTION-CDI TO COMM-ACTION-CODE.

           IF MAP-CUST-INFOF = HEX80
               MOVE SPACES TO COMM-CUSTOMER-INFO
               MOVE 'Y'            TO COMM-KEY-CHANGED
           ELSE
               IF MAP-CUST-INFOL > ZERO
                  MOVE 'Y'            TO COMM-KEY-CHANGED
                  MOVE MAP-CUST-INFOI TO COMM-CUSTOMER-INFO.

       0120-UPDATE-CIM-MAP-FIELDS.
           DISPLAY '0120-UPDATE-CIM-MAP-FIELDS PARA'
           IF MAP-FIRST-NAMEF = HEX80
               MOVE 'Y' TO DISPLAY-CHANGED-SW
               MOVE SPACES TO COM-FIRST-NAME
           ELSE
               IF MAP-FIRST-NAMEL > ZERO
                  MOVE 'Y' TO DISPLAY-CHANGED-SW
                  MOVE MAP-FIRST-NAMEI TO COM-FIRST-NAME.

           IF MAP-MIDDLE-NAMEF = HEX80
               MOVE 'Y' TO DISPLAY-CHANGED-SW
               MOVE SPACES TO COM-MIDDLE-NAME
           ELSE
               IF MAP-MIDDLE-NAMEL > ZERO
                  MOVE 'Y' TO DISPLAY-CHANGED-SW
                  MOVE MAP-MIDDLE-NAMEI TO COM-MIDDLE-NAME.

           IF MAP-LAST-NAMEF = HEX80
               MOVE 'Y' TO DISPLAY-CHANGED-SW
               MOVE SPACES TO COM-LAST-NAME
           ELSE
               IF MAP-LAST-NAMEL > ZERO
                  MOVE 'Y' TO DISPLAY-CHANGED-SW
                  MOVE MAP-LAST-NAMEI TO COM-LAST-NAME.

           IF MAP-SUFFIX1F = HEX80
               MOVE 'Y' TO DISPLAY-CHANGED-SW
               MOVE SPACES TO COM-SUFFIX1
           ELSE
               IF MAP-SUFFIX1L > ZERO
                  MOVE 'Y' TO DISPLAY-CHANGED-SW
                  MOVE MAP-SUFFIX1I TO COM-SUFFIX1.

           IF MAP-SUFFIX2F = HEX80
               MOVE 'Y' TO DISPLAY-CHANGED-SW
               MOVE SPACES TO COM-SUFFIX2
           ELSE
               IF MAP-SUFFIX2L > ZERO
                  MOVE 'Y' TO DISPLAY-CHANGED-SW
                  MOVE MAP-SUFFIX2I TO COM-SUFFIX2.

           IF MAP-NICKNAMEF = HEX80
               MOVE SPACES TO COM-NICKNAME
           ELSE
               IF MAP-NICKNAMEL > ZERO
                  MOVE MAP-NICKNAMEI TO COM-NICKNAME.

R04023     IF MAP-ISSUE-STATEF = HEX80
R04023         MOVE SPACES TO COM-ISSUE-STATE
R04023     ELSE
R04023         IF MAP-ISSUE-STATEL > ZERO
R04023            MOVE MAP-ISSUE-STATEI TO COM-ISSUE-STATE.

      *---------------------------------------------------------------*
      *    SPECIAL CODING TO INSURE THE COMPANY IN ADDRESS FLAG IS    *
      *    SET TO YES IF THE COMPANY NAME WAS BLANK PRIOR TO THE      *
      *    UPDATE.                                                    *
      *---------------------------------------------------------------*

      *    IF COM-COMPANY-NAME = SPACES AND MAP-COMP-NAMEI > SPACE
      *        AND MAP-COMP-IN-ADDL NOT > ZERO
      *             MOVE 'Y' TO COM-COMPANY-IN-ADDRESS.

           IF MAP-COMP-NAMEF = HEX80
               MOVE 'Y' TO DISPLAY-CHANGED-SW
               MOVE SPACES TO COM-COMPANY-NAME
           ELSE
               IF MAP-COMP-NAMEL > ZERO
                  MOVE 'Y' TO DISPLAY-CHANGED-SW
                  MOVE MAP-COMP-NAMEI TO COM-COMPANY-NAME
                                         WS-COMPANY-NAME.

      *---------------------------------------------------------------*
      *    ALTERED SEX CODE CAN CHANGE DEFAULT PREFIX ON DISPLAY NAME *
      *---------------------------------------------------------------*
           IF MAP-SEXF = HEX80
               MOVE SPACES TO COM-SEX
           ELSE
               IF MAP-SEXL > ZERO
                  MOVE 'Y' TO DISPLAY-CHANGED-SW
                  MOVE MAP-SEXI TO COM-SEX.


T3384      IF MAP-PREFIXF = HEX80
T3384          MOVE SPACES TO COM-PREFIX
T3384      ELSE
T3384          IF MAP-PREFIXL > ZERO
T3384             MOVE 'Y' TO DISPLAY-CHANGED-SW
T3384             MOVE MAP-PREFIXI TO COM-PREFIX.

      *    IF DISPLAY-CHANGED-SW = 'Y'
      *        PERFORM 0300-FORMAT-DISPLAY-NAME.

           IF MAP-ADDRESS1F = HEX80
               MOVE SPACES TO COM-ADDRESS1
           ELSE
               IF MAP-ADDRESS1L > ZERO
                  MOVE MAP-ADDRESS1I TO COM-ADDRESS1.

           IF MAP-ADDRESS2F = HEX80
               MOVE SPACES TO COM-ADDRESS2
           ELSE
               IF MAP-ADDRESS2L > ZERO
                  MOVE MAP-ADDRESS2I TO COM-ADDRESS2.

           IF MAP-BIRTH-DATEF = HEX80
               MOVE ZEROS TO COM-BIRTH-DATE
Y2KIMR*
Y2KIMR* IMR CHANGE IMR6 BEGIN
Y2KIMR*
Y2KIMR      MOVE ZEROS TO Y2K-WK-VARIABLE1
Y2KIMR*
Y2KIMR* IMR CHANGE IMR6 END
Y2KIMR*
           ELSE
               IF MAP-BIRTH-DATEL > ZERO
                  INSPECT MAP-BIRTH-DATEI REPLACING ALL '-' BY 'A'
Y2KIMR*
Y2KIMR* IMR DATE ROUTINE CHANGE BEGIN
Y2KIMR*
Y2KIMR*           EXEC CICS BIF DEEDIT FIELD(MAP-BIRTH-DATEI)
Y2KIMR*                                LENGTH(8)
Y2KIMR*                                END-EXEC
Y2KIMR*           MOVE MAP-BIRTH-DATEI TO WS-MAP-DATE
Y2KIMR*           MOVE WS-MAP-BIF      TO COM-BIRTH-DATE.
Y2KIMR*
MANJU             EXEC CICS BIF DEEDIT FIELD(MAP-BIRTH-DATEI)
Y2KIMR                                 LENGTH(10)
Y2KIMR                                 END-EXEC
                  DISPLAY 'MAP-BIRTH-DATEI' MAP-BIRTH-DATEI.
Y2KIMR            MOVE MAP-BIRTH-DATEI TO Y2K-WK-VARIABLE1
Y2KIMR*           MOVE Y2K-WK-VARIABLE2-MM TO Y2K-WK-VARIABLE3-BB-MM
Y2KIMR*           MOVE Y2K-WK-VARIABLE2-DD TO Y2K-WK-VARIABLE3-BB-DD
Y2KIMR*           MOVE Y2K-WK-VARIABLE2-YY TO Y2K-WK-VARIABLE3-BB-YY
Y2KIMR*           MOVE Y2K-WK-VARIABLE3 TO WS-MAP-DATE
Y2KIMR            MOVE Y2K-WK-VARIABLE2 TO COM-BIRTH-DATE .
Y2KIMR*
Y2KIMR* IMR DATE ROUTINE CHANGE END
Y2KIMR*

           IF MAP-SSNF = HEX80
               MOVE SPACES TO COM-SSN
           ELSE
               IF MAP-SSNL > ZERO
                  MOVE MAP-SSNI TO COM-SSN.

           IF MAP-CITYF = HEX80
               MOVE SPACES TO COM-CITY
           ELSE
               IF MAP-CITYL > ZERO
                  MOVE MAP-CITYI TO COM-CITY.

           IF MAP-COMP-IN-ADDF = HEX80
           OR MAP-COMP-IN-ADDI = SPACES
               MOVE 'Y' TO COM-COMPANY-IN-ADDRESS
           ELSE
               IF MAP-COMP-IN-ADDL > ZERO
                  MOVE MAP-COMP-IN-ADDI TO COM-COMPANY-IN-ADDRESS.

           IF MAP-STATEF = HEX80
DFJ            MOVE 'Y' TO STATE-CHANGED-SW
               MOVE SPACES TO COM-STATE
           ELSE
               IF MAP-STATEL > ZERO
DFJ               MOVE 'Y' TO STATE-CHANGED-SW
                  MOVE MAP-STATEI TO COM-STATE.

           IF MAP-ZIPF = HEX80
DFJ            MOVE 'Y' TO ZIP-CHANGED-SW
               MOVE SPACES TO COM-ZIP
           ELSE
               IF MAP-ZIPL > ZERO
DFJ               MOVE 'Y' TO ZIP-CHANGED-SW
                  MOVE MAP-ZIPI TO COM-ZIP.

           IF MAP-ZIP-PLUS4F = HEX80
890989         MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW
               MOVE SPACES TO COM-ZIP-PLUS4
           ELSE
               IF MAP-ZIP-PLUS4L > ZERO
890989            MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW
                  MOVE MAP-ZIP-PLUS4I TO COM-ZIP-PLUS4.

           IF MAP-ADDR-OVRIDEF = HEX80
               MOVE SPACES TO COM-FINALST-OVRD-IND
           ELSE
               IF MAP-ADDR-OVRIDEL > ZERO
                  MOVE MAP-ADDR-OVRIDEI TO COM-FINALST-OVRD-IND.

           MOVE 'N' TO COM-DUP-ADDR-OVRD-IND.

           IF MAP-AREA-CODEF = HEX80
               MOVE SPACES TO COM-AREA-CODE
           ELSE
               IF MAP-AREA-CODEL > ZERO
                  MOVE MAP-AREA-CODEI TO COM-AREA-CODE.

900837     MOVE 'N' TO PHONE-CHANGED-SW.
           MOVE COM-PHONE TO WS-PHONE.

           IF MAP-PHONE3F = HEX80
               MOVE 'Y' TO PHONE-CHANGED-SW
               MOVE SPACES TO WS-PHONE3
           ELSE
               IF MAP-PHONE3L > ZERO
                  MOVE 'Y' TO PHONE-CHANGED-SW
                  MOVE MAP-PHONE3I TO WS-PHONE3.

           IF MAP-PHONE4F = HEX80
               MOVE 'Y' TO PHONE-CHANGED-SW
               MOVE SPACES TO WS-PHONE4
           ELSE
               IF MAP-PHONE4L > ZERO
                  MOVE 'Y' TO PHONE-CHANGED-SW
                  MOVE MAP-PHONE4I TO WS-PHONE4.

           IF PHONE-CHANGED-SW = 'Y'
               MOVE WS-PHONE TO COM-PHONE.

           IF MAP-PHONE-EXTF = HEX80
               MOVE SPACES TO COM-PHONE-EXTENSION
           ELSE
               IF MAP-PHONE-EXTL > ZERO
                  MOVE MAP-PHONE-EXTI TO COM-PHONE-EXTENSION.

900837     IF MAP-FAX-AREA-CDF = HEX80
900837         MOVE SPACES TO COM-FAX-AREA-CODE
900837     ELSE
900837         IF MAP-FAX-AREA-CDL > ZERO
900837            MOVE MAP-FAX-AREA-CDI TO COM-FAX-AREA-CODE.
900837
900837     MOVE 'N' TO PHONE-CHANGED-SW.
900837     MOVE COM-FAX-PHONE TO WS-PHONE.
900837
900837     IF MAP-FAX-PHONE3F = HEX80
900837         MOVE 'Y' TO PHONE-CHANGED-SW
900837         MOVE SPACES TO WS-PHONE3
900837     ELSE
900837         IF MAP-FAX-PHONE3L > ZERO
900837            MOVE 'Y' TO PHONE-CHANGED-SW
900837            MOVE MAP-FAX-PHONE3I TO WS-PHONE3.
900837
900837     IF MAP-FAX-PHONE4F = HEX80
900837         MOVE 'Y' TO PHONE-CHANGED-SW
900837         MOVE SPACES TO WS-PHONE4
900837     ELSE
900837         IF MAP-FAX-PHONE4L > ZERO
900837            MOVE 'Y' TO PHONE-CHANGED-SW
900837            MOVE MAP-FAX-PHONE4I TO WS-PHONE4.
900837
900837     IF PHONE-CHANGED-SW = 'Y'
900837         MOVE WS-PHONE TO COM-FAX-PHONE.

           IF MAP-ALT-ADDRF = HEX80
               MOVE SPACES TO COM-ALT-ADDRESS-IND
           ELSE
               IF MAP-ALT-ADDRL  > ZERO
                  MOVE MAP-ALT-ADDRI  TO COM-ALT-ADDRESS-IND.

           IF MAP-ENTITY-TYPEF = HEX80
               MOVE SPACES TO COM-ENTITY-TYPE COM-ENTITY-LITERAL
           ELSE
               IF MAP-ENTITY-TYPEL  > ZERO
                  MOVE MAP-ENTITY-TYPEI  TO COM-ENTITY-TYPE
                                            COM-ENTITY-LITERAL.
R00700     IF MAP-EMAIL1F = HEX80
R00700         MOVE SPACES TO COM-EMAIL1
R00700     ELSE
R00700         IF MAP-EMAIL1L  > ZERO
R00700            MOVE MAP-EMAIL1I  TO COM-EMAIL1.


           IF MAP-DIS-CHG-INDF = HEX80
               MOVE SPACES TO COM-DISP-IND
           ELSE
               IF MAP-DIS-CHG-INDL  > ZERO
                  MOVE MAP-DIS-CHG-INDI  TO COM-DISP-IND

           IF MAP-FUTURE-ADDRF = HEX80
               MOVE SPACES TO COM-FUTURE-ADDRESS-IND
           ELSE
               IF MAP-FUTURE-ADDRL  > ZERO
                  MOVE MAP-FUTURE-ADDRI  TO COM-FUTURE-ADDRESS-IND.

           IF MAP-CUST-STATUSF = HEX80
               MOVE SPACES TO COM-RECORD-STATUS
           ELSE
               IF MAP-CUST-STATUSL  > ZERO
                  MOVE MAP-CUST-STATUSI TO COM-RECORD-STATUS
DFJ               IF COM-RECORD-STATUS = 'C'
DFJ                   MOVE 'Y' TO STATUS-CHANGED-SW.

           IF MAP-EFF-DATEF = HEX80
               MOVE ZEROS TO COM-EFFECTIVE-DATE
           ELSE
               IF MAP-EFF-DATEL  > ZERO
                  INSPECT MAP-EFF-DATEI REPLACING ALL '-' BY 'A'
      *           EXEC CICS BIF DEEDIT FIELD(MAP-EFF-DATEI)
      *                                LENGTH(8)
      *                                END-EXEC
                  MOVE MAP-EFF-DATEI TO WS-MAP-DATE
                  MOVE WS-MAP-BIF    TO COM-EFFECTIVE-DATE.


           IF MAP-ASSOC1F = HEX80
               MOVE SPACES TO COM-ASSOCIATION1
           ELSE
               IF MAP-ASSOC1L  > ZERO
                  MOVE MAP-ASSOC1I  TO COM-ASSOCIATION1.

           IF MAP-ASSOC2F = HEX80
               MOVE SPACES TO COM-ASSOCIATION2
           ELSE
               IF MAP-ASSOC2L  > ZERO
                  MOVE MAP-ASSOC2I  TO COM-ASSOCIATION2.

           IF MAP-ASSOC3F = HEX80
               MOVE SPACES TO COM-ASSOCIATION3
           ELSE
               IF MAP-ASSOC3L  > ZERO
                  MOVE MAP-ASSOC3I  TO COM-ASSOCIATION3.

R02539     IF MAP-COUNTRY-CDF = HEX80
R02539         MOVE SPACES TO COM-COUNTRY-CODE
R02539     ELSE
R02539         IF MAP-COUNTRY-CDL > ZERO
R02539            MOVE MAP-COUNTRY-CDI TO COM-COUNTRY-CODE.

R02539     IF MAP-POSTAL-CDF = HEX80
R02539         MOVE SPACES TO COM-POSTAL-CODE
R02539     ELSE
R02539         IF MAP-POSTAL-CDL > ZERO
R02539            MOVE MAP-POSTAL-CDI TO COM-POSTAL-CODE.
           DISPLAY 'COM-COUNTRY-CODE' COM-COUNTRY-CODE.
           DISPLAY 'COM-POSTAL-CODEE' COM-POSTAL-CODE.
           EXEC CICS ASKTIME ABSTIME (WS-TIME-FIELD)
                             RESP    (WS-CICS-RESP)
                             END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               GO TO 9999-CICS-ERROR.

           EXEC CICS FORMATTIME ABSTIME (WS-TIME-FIELD)
                                MMDDYY  (WS-MAP-BIF)
                                RESP    (WS-CICS-RESP)
                                END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
               GO TO 9999-CICS-ERROR.

           MOVE WS-MAP-BIF       TO COM-CHANGE-DATE.


       0140-SQL-ERROR-CHECK.

           IF SQLCODE NOT = ZERO
               GO TO 0420-DB2-ERROR.

       0150-FINALST-ADDRESS-CHECK.

           MOVE COM-ADDRESS1 TO NA-COMM-ADDRESS-1.
           MOVE COM-ADDRESS2 TO NA-COMM-ADDRESS-2.
           MOVE COM-CITY     TO NA-COMM-CITY.
           MOVE COM-STATE    TO NA-COMM-STATE.
           MOVE COM-ZIP      TO NA-COMM-ZIP-CODE.
890989     MOVE COM-ZIP-PLUS4 TO NA-COMM-ZIP-PLUS4.

COBOLU     EXEC CICS ASSIGN APPLID(WS-APPLID) END-EXEC.

R01873*    IF WS-APPLID = 'CICSHADT' OR 'CICSHADP' OR 'CICSHADU'
167949*                   OR 'CICSHADL'
R11386*                   OR 'CICSHADF' OR 'CICSADHD' OR 'CICSHUAE'
R11386*                   OR 'CICSADHQ' OR 'CICSADHS' OR 'CICSADHU'
R11386*                   OR 'CICSADHT' OR 'CICSADHB' OR 'CICSHADY'
COBOLU*        EXEC CICS LINK
167949*          PROGRAM  ('CADDR')
COBOLU*          COMMAREA (NAME-AND-ADDRESS-COMMAREA)
COBOLU*          LENGTH   (LENGTH OF NAME-AND-ADDRESS-COMMAREA)
COBOLU*          RESP     (WS-CICS-RESP)
COBOLU*        END-EXEC
COBOLU*    ELSE
      *        EXEC CICS LINK
      *          PROGRAM  ('NA205')
      *          COMMAREA (NAME-AND-ADDRESS-COMMAREA)
990721**         LENGTH   (WS-FIN-LENGTH)
990721*          LENGTH   (LENGTH OF NAME-AND-ADDRESS-COMMAREA)
      *          RESP     (WS-CICS-RESP)
      *        END-EXEC.

      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      *        GO TO 9999-CICS-ERROR.
      *
      *    MOVE NA-COMM-FINALIST-REASON TO COM-FINALST-REAS-CODE.
      *    MOVE NA-COMM-FINALIST-REASON TO WS-FINALST-REAS-CODE.

R01142*    IF WS-FINALST-BYTE1 NOT = '9'
R01142*       IF NA-COMM-ADDRESS-1 = COM-ADDRESS1 AND
R01142*          NA-COMM-ADDRESS-2 = COM-ADDRESS2
R01142*            NEXT SENTENCE
R01142*       ELSE
R01142*         MOVE NA-COMM-ADDRESS-1 TO WS-ADDRESS1
R01142*         MOVE NA-COMM-ADDRESS-2 TO WS-ADDRESS2
R01142*         IF WS-ADDRESS1-REMAINDER > SPACES OR
R01142*            WS-ADDRESS2-REMAINDER > SPACES
R01142*** THE SOFTWARE HAS DEFAULTED/REFORMATTED AN ADDRESS LINE AND
R01142*** IT EXCEEDS 22 BYTES SO WARN THE USER
R01142*              MOVE 'Y'      TO WS-EDIT-SW
R01142*                               COM-FINALIST-SW
R01142*              MOVE -1       TO MAP-ADDRESS1L
R01142*              MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A
R01142*              MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS2A
R01142*              MOVE 'NA047'  TO WS-HOLD-MESSAGE
R01142*              PERFORM 0170-SLIDE-ERROR-MESSAGES
R01142*              MOVE 'NA048'  TO WS-HOLD-MESSAGE
R01142*              PERFORM 0170-SLIDE-ERROR-MESSAGES.

R11386********************************************************
R11386**ADDING FUNCTIONALITY TO GET THE COUNTY CODE FROM *
R11386**STATE_COUNTY IF CADDR FAILS TO RETURN THE COUNTY CODE*
R11386********************************************************
R11386     IF WS-FINALST-BYTE1 = '9'
R11386        MOVE NA-COMM-ZIP        TO WS-SCZ-ZIP
R11386        PERFORM 1300-STATE-COUNTY-ZIP THRU 1300-EXIT
R11386     END-IF
R01213     IF WS-FINALST-BYTE1 NOT = '9'
R01213        IF (NA-COMM-ADDRESS-1 = COM-ADDRESS1 AND
R01213           NA-COMM-ADDRESS-2 = COM-ADDRESS2)
R01213         MOVE NA-COMM-ADDRESS-1 TO COM-ADDRESS1
R01213         MOVE NA-COMM-ADDRESS-2 TO COM-ADDRESS2
R01213         MOVE NA-COMM-CITY      TO COM-CITY
R01213         MOVE NA-COMM-STATE     TO COM-STATE
R01213         MOVE NA-COMM-ZIP-CODE  TO COM-ZIP
R01213         MOVE NA-COMM-ZIP-PLUS4 TO COM-ZIP-PLUS4
R01213         MOVE NA-COMM-COUNTY-CODE TO COM-COUNTY-CODE
R01213        ELSE
R01213          MOVE NA-COMM-ADDRESS-1 TO WS-ADDRESS1
R01213          MOVE NA-COMM-ADDRESS-2 TO WS-ADDRESS2
R01213          IF WS-ADDRESS1-REMAINDER > SPACES OR
R01213             WS-ADDRESS2-REMAINDER > SPACES
R01213             MOVE COM-ADDRESS1  TO NA-COMM-ADDRESS-1
R01213             MOVE COM-ADDRESS2  TO NA-COMM-ADDRESS-2
R01213*** RETURNED ADDRESS FROM THE DATABASE THAT WAS TRUNCATED
R01213*** THE SOFTWARE HAS DEFAULTED/REFORMATTED AN ADDRESS LINE AND
R01213*** IT EXCEEDS 22 BYTES SO WARN THE USER
R01213               MOVE 'Y'      TO WS-EDIT-SW
R01213                                COM-FINALIST-SW
R01213               MOVE -1       TO MAP-ADDRESS1L
R01213               MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS1A
R01213               MOVE ATTRB-UNPROT-BRT-PEN TO MAP-ADDRESS2A
R01213               MOVE 'NA047'  TO WS-HOLD-MESSAGE
R01213               PERFORM 0170-SLIDE-ERROR-MESSAGES
R01213               MOVE 'NA048'  TO WS-HOLD-MESSAGE
R01213               PERFORM 0170-SLIDE-ERROR-MESSAGES
R01213         ELSE
R01213**** NO TRUNCATION
R01213               MOVE NA-COMM-ADDRESS-1 TO COM-ADDRESS1
R01213               MOVE NA-COMM-ADDRESS-2 TO COM-ADDRESS2.

           IF WS-FINALST-BYTE1 NOT = '9'
R01213*        MOVE NA-COMM-ADDRESS-1 TO COM-ADDRESS1
R01213*        MOVE NA-COMM-ADDRESS-2 TO COM-ADDRESS2
               MOVE NA-COMM-CITY      TO COM-CITY
               MOVE NA-COMM-STATE     TO COM-STATE
               MOVE NA-COMM-ZIP-CODE  TO COM-ZIP
               MOVE NA-COMM-ZIP-PLUS4 TO COM-ZIP-PLUS4
KPL            MOVE NA-COMM-COUNTY-CODE TO COM-COUNTY-CODE
920124         IF (COMM-CUST-TYPE = 'CA' OR 'AM' OR 'LB' OR 'BA'
960530                            OR 'LD')
890989         EXEC SQL
890989             OPEN CASECUR
890989         END-EXEC
890989         IF SQLCODE NOT = ZERO
890989             GO TO 0420-DB2-ERROR
890989         ELSE
890989             EXEC SQL FETCH CASECUR
890989*                INTO DCLAGNTNAME:AGNTNAME-INDICATORS
R00700                INTO  :IDNTITY, :LAST-NAME :IND-LAST-NAME,
C23311                      :FIRST-NAME :IND-FIRST-NAME,
C23311                      :MIDDLE-NAME :IND-MIDDLE-NAME,
R00700                      :PREFIX :IND-PREFIX, :SUFFIX1 :IND-SUFFIX1,
R00700                      :SUFFIX2 :IND-SUFFIX2,
R00700                      :COMPANY-IND, :COMPANY-IN-ADDRESS,
C23311                      :COMPANY-NAME :IND-COMPANY-NAME,
C23311                      :DISPLAY-NAME :IND-DISPLAY-NAME,
R00700                      :NICKNAME :IND-NICKNAME,
R00700                      :ADDRESS1, :ADDRESS2 :IND-ADDRESS2,
R00700                      :CITY, :STATE, :ZIP, :ZIP-PLUS4,
R00700                      :COUNTY-CODE, :AREA-CODE,
R00700                      :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,
R00700                      :BIRTH-DATE :IND-BIRTH-DATE,
R00700                      :FINALST-REAS-CODE, :FINALST-OVRD-IND,
R00700                      :DUP-ADDR-OVRD-IND,
R00700                      :EFFECTIVE-DATE, :CHANGE-DATE,
R00700                      :CHANGE-LOGON, :ENTITY-TYPE,
R00700                      :RECORD-STATUS, :ALT-ADDRESS-IND,
R00700                      :FUTURE-ADDRESS-IND,
R00700                      :RECORD-ORIGIN, :COMBINED-STATUS,
R00700                      :SITE-CODE, :NAME-KEY1,
R00700                      :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,
R00700                      :ASSOCIATION1 :IND-ASSOCIATION1,
R00700                      :ASSOCIATION2 :IND-ASSOCIATION2,
R00700                      :ASSOCIATION3 :IND-ASSOCIATION3,
R00700                      :FAX-AREA-CODE, :FAX-PHONE,
R04023                      :ORIGINAL-STATE,
R00700                      :EMAIL1 :IND-EMAIL1,
R02539                      :PASS-WORD :IND-PASSWORD,
R02539                      :COUNTRY-CODE :IND-COUNTRY-CODE,
R02539                      :POSTAL-CODE :IND-POSTAL-CODE
890989             END-EXEC
890989             IF SQLCODE NOT = ZERO
890989                 GO TO 0420-DB2-ERROR
890989             ELSE
890989                 EXEC SQL
890989                     CLOSE CASECUR
890989                 END-EXEC
890989                 IF SQLCODE NOT = ZERO
890989                    GO TO 0420-DB2-ERROR
890989                 ELSE
890989         EVALUATE TRUE ALSO TRUE
890989             WHEN ZIP NOT EQUAL COM-ZIP
890989             ALSO STATE NOT EQUAL COM-STATE
890989                 MOVE 'Y' TO ZIP-CHANGED-SW
890989                 MOVE 'Y' TO STATE-CHANGED-SW
890989                 MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW
890989             WHEN ZIP NOT EQUAL COM-ZIP
890989             ALSO ZIP-PLUS4 NOT EQUAL COM-ZIP-PLUS4
890989                 MOVE 'Y' TO ZIP-CHANGED-SW
890989                 MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW
890989             WHEN OTHER
890989                 CONTINUE
890989         END-EVALUATE
890989         EVALUATE TRUE
890989             WHEN ZIP-PLUS4 NOT EQUAL COM-ZIP-PLUS4
890989                 MOVE 'Y' TO ZIP-PLUS4-CHANGED-SW
890989             WHEN ZIP NOT EQUAL COM-ZIP
890989                 MOVE 'Y' TO ZIP-CHANGED-SW
890989             WHEN OTHER
890989                 CONTINUE
890989         END-EVALUATE.
R11386****************************************************
R11386* START OF COUNTY CODE UPDATE FROM STATE_COUNTY_ZIP*
R11386****************************************************
R11386 1300-STATE-COUNTY-ZIP.
R11386
R11386
R11386        EXEC SQL
R11386           SELECT STATE_NUMBER||COUNTY_NUMBER AS COUNTY_CODE
R11386             INTO :WS-SCZ-COUNTY-CODE
R11386             FROM STATE_COUNTY_ZIP
R11386            WHERE STATE_CD    = :NA-COMM-STATE
R11386            AND ZIP_CODE      = :WS-SCZ-ZIP-C
R11386            FETCH FIRST ROW ONLY
R11386        END-EXEC
R11386
R11386         EVALUATE SQLCODE
R11386           WHEN  0
R11386             MOVE WS-SCZ-COUNTY-CODE-N TO NA-COMM-COUNTY-CODE
R11386             MOVE SPACES TO NA-COMM-FINALIST-REASON
R11386           WHEN 100
R11386             CONTINUE
R11386           WHEN OTHER
R11386             GO TO 0420-DB2-ERROR
R11386         END-EVALUATE
R11386           MOVE NA-COMM-FINALIST-REASON TO WS-FINALST-REAS-CODE.
R11386
R11386 1300-EXIT.
R11386      EXIT.
R11386**************************************************
R11386* END OF COUNTY CODE UPDATE FROM STATE_COUNTY_ZIP*
R11386**************************************************
       0160-NUMERIC-CHECK.

           IF WS-NUMERIC-CHECK-BYTE (ACTION-SUB) NUMERIC
               MOVE 'Y' TO WS-NUMERIC-SW.


       0170-SLIDE-ERROR-MESSAGES.

           IF WS-MESSAGE-NUMBER1 = SPACES OR LOW-VALUES
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER1
           ELSE
           IF WS-MESSAGE-NUMBER2 = SPACES OR LOW-VALUES
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER2
           ELSE
           IF WS-MESSAGE-NUMBER3 = SPACES OR LOW-VALUES
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER3
           ELSE
           IF WS-MESSAGE-NUMBER4 = SPACES OR LOW-VALUES
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER4.

       0190-SEND-NA320M1-FIRST-TIME.
           DISPLAY '0190-SEND-NA320M1-FIRST-TIME PARA'
           PERFORM 0390-CHECK-ERROR-MESSAGES
               VARYING ACTION-SUB FROM 1 BY 1
                   UNTIL ACTION-SUB > 4.

R01717** FOR CASE OR CAM DATA, CHECK CARRIER SECURITY
R01717*    IF COMM-CUST-TYPE = 'AM' OR 'CA'
R01717*       PERFORM 0590-CHECK-SECURITY THRU 0590-EXIT
R01717*       IF WS-MESSAGE-NUMBER1 > SPACES
R01717*         MOVE +100 TO SQLCODE
R01717*         GO TO 0310-INITIATE-ROUTER.

           PERFORM 0250-PROCESS-DB2-REQUESTS.
           DISPLAY '0250 WS-RECORD-NOTFND' WS-RECORD-NOTFND
           IF WS-RECORD-NOTFND NOT = 'Y'
               PERFORM 0280-MOVE-SPACES-TO-MAP.
           MOVE SPACES       TO COMM-MSG-MAX-SEVERITY.
           MOVE -1           TO MAP-SYSTEM-CDL.

           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES
               MOVE ALL '_' TO MAP-SYSTEM-CDO
               MOVE SPACES  TO COMM-SYSTEM-CODE
           ELSE
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES
               MOVE ALL '_' TO MAP-ACTION-CDO
               MOVE SPACES  TO COMM-ACTION-CODE
           ELSE
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES
               MOVE ALL '_' TO MAP-CUST-INFOO
               MOVE SPACES  TO COMM-CUSTOMER-INFO
           ELSE
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.

961037     IF ENTITY-TYPE = 'RP'
961037         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA
961037     END-IF.

R03179     IF ENTITY-TYPE = 'CA'
R03179         MOVE ATTRB-PROT-ASKIP        TO MAP-EMAIL1A MAP-EMAIL2A
R03179     END-IF.

           EXEC CICS SEND MAP    ('NA320M1')
                          CURSOR
                          ERASE
                          RESP (WS-CICS-RESP)
                          END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.

           MOVE 'NAI0'             TO COMM-PREVIOUS-TRANID.
           MOVE '01'               TO COMM-NEXT-FUNCTION.
           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.
900837     MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)
                               FROM   (WSQ-COMMAREA)
                               LENGTH (WS-COMM-DB2-LENGTH)
                               ITEM   (WS-TS-ITEM)
                               RESP   (WS-CICS-RESP)
                               REWRITE
                               MAIN
                               END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           EXEC CICS RETURN TRANSID ('NAI0')
                            END-EXEC.

       0200-SEND-NA320M1-MAP.
           DISPLAY '0200-SEND-NA320M1-MAP  PARA'.
           MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1).
           MOVE WS-MESSAGE-NUMBER2 TO COMM-MSG-ID(2).
           MOVE WS-MESSAGE-NUMBER3 TO COMM-MSG-ID(3).
           MOVE WS-MESSAGE-NUMBER4 TO COMM-MSG-ID(4).

           PERFORM 0390-CHECK-ERROR-MESSAGES
               VARYING ACTION-SUB FROM 1 BY 1
                   UNTIL ACTION-SUB > 4.

           IF COMM-MSG-MAX-SEVERITY = 'E'
               MOVE SPACES TO COMM-MSG-MAX-SEVERITY.

           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES
               MOVE ALL '_' TO MAP-SYSTEM-CDO
               MOVE SPACES  TO COMM-SYSTEM-CODE
           ELSE
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES
               MOVE ALL '_' TO MAP-ACTION-CDO
               MOVE SPACES  TO COMM-ACTION-CODE
           ELSE
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES
               MOVE ALL '_' TO MAP-CUST-INFOO
               MOVE SPACES  TO COMM-CUSTOMER-INFO
           ELSE
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.


           PERFORM 0290-MOVE-OUT-MAP-FIELDS.
900837     IF WS-EDIT-SW = 'Y'
900837         EXEC CICS SEND CONTROL ALARM END-EXEC
900837     ELSE
               MOVE -1 TO MAP-SYSTEM-CDL.

           EXEC CICS SEND MAP    ('NA320M1')
                          CURSOR
                          ERASE
                          RESP (WS-CICS-RESP)
                          END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.


           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.

           MOVE 'NAI0'             TO COMM-PREVIOUS-TRANID.
           MOVE '01'               TO COMM-NEXT-FUNCTION.
           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.
           MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.
900837     MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)
                               FROM   (WSQ-COMMAREA)
                               LENGTH (WS-COMM-DB2-LENGTH)
                               ITEM   (WS-TS-ITEM)
                               RESP   (WS-CICS-RESP)
                               REWRITE
                               MAIN
                               END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           EXEC CICS RETURN TRANSID ('NAI0')
                            END-EXEC.




       0250-PROCESS-DB2-REQUESTS.

           DISPLAY 'WS-CIM-NUMBER'         WS-CIM-NUMBER.
           EXEC SQL
               OPEN AGNTCUR
           END-EXEC.

           IF SQLCODE NOT = ZERO
               GO TO 0420-DB2-ERROR.

           EXEC SQL FETCH AGNTCUR
R01142         INTO :DCLAGNTNAME:AGNTNAME-INDICATORS
           END-EXEC.
           DISPLAY 'AGNT TABLE SQLCODE ' SQLCODE
           IF SQLCODE = 100
               EXEC SQL OPEN CASECUR END-EXEC
               PERFORM 0140-SQL-ERROR-CHECK
               MOVE 'Y' TO CASE-FETCH-SW WSQ-CASE-SW
KPL            EXEC SQL FETCH CASECUR
R00700*            INTO DCLAGNTNAME:AGNTNAME-INDICATORS
R00700             INTO  :IDNTITY, :LAST-NAME :IND-LAST-NAME,
C23311                   :FIRST-NAME :IND-FIRST-NAME,
C23311                   :MIDDLE-NAME :IND-MIDDLE-NAME,
R00700                   :PREFIX :IND-PREFIX, :SUFFIX1 :IND-SUFFIX1,
R00700                   :SUFFIX2 :IND-SUFFIX2,
R00700                   :COMPANY-IND, :COMPANY-IN-ADDRESS,
C23311                   :COMPANY-NAME :IND-COMPANY-NAME,
C23311                   :DISPLAY-NAME :IND-DISPLAY-NAME,
R00700                   :NICKNAME :IND-NICKNAME,
C23311                   :ADDRESS1, :ADDRESS2 :IND-ADDRESS2,
R00700                   :CITY, :STATE, :ZIP, :ZIP-PLUS4,
R00700                   :COUNTY-CODE, :AREA-CODE,
R00700                   :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,
R00700                   :BIRTH-DATE :IND-BIRTH-DATE,
R00700                   :FINALST-REAS-CODE, :FINALST-OVRD-IND,
R00700                   :DUP-ADDR-OVRD-IND,
R00700                   :EFFECTIVE-DATE, :CHANGE-DATE,
R00700                   :CHANGE-LOGON, :ENTITY-TYPE,
R00700                   :RECORD-STATUS, :ALT-ADDRESS-IND,
R00700                   :FUTURE-ADDRESS-IND,
R00700                   :RECORD-ORIGIN, :COMBINED-STATUS,
R00700                   :SITE-CODE, :NAME-KEY1,
R00700                   :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,
R00700                   :ASSOCIATION1 :IND-ASSOCIATION1,
R00700                   :ASSOCIATION2 :IND-ASSOCIATION2,
R00700                   :ASSOCIATION3 :IND-ASSOCIATION3,
R00700                   :FAX-AREA-CODE, :FAX-PHONE,
R04023                   :ORIGINAL-STATE,
R00700                   :EMAIL1 :IND-EMAIL1,
R02539                   :COUNTRY-CODE :IND-COUNTRY-CODE,
R02539                   :POSTAL-CODE :IND-POSTAL-CODE
               END-EXEC.
           DISPLAY 'CASE SQLCODE         ' SQLCODE
           DISPLAY 'WS-CIM-NUMBER'         WS-CIM-NUMBER
           DISPLAY 'COMM-CUST-TYPE'        COMM-CUST-TYPE
      *    IF  SQLCODE = 100 AND COMM-PREVIOUS-TRANID NOT = 'NAI0'
      *        MOVE 'NA020'  TO WS-MESSAGE-NUMBER1
      *        GO TO 0310-INITIATE-ROUTER
      *    ELSE
           IF  SQLCODE = 100
               MOVE 'NA020'           TO WS-MESSAGE-NUMBER1
               MOVE 'Y'               TO WS-RECORD-NOTFND
               PERFORM 0380-BUMP-ERROR-MESSAGES
           ELSE
           IF  SQLCODE NOT = ZERO
               GO TO 0420-DB2-ERROR
920403     END-IF.
920403*    IF COMM-CUST-TYPE NOT = 'CA' AND 'AM' AND 'LB' AND 'BA'
960530*                             AND 'LD'
MANJU *                             AND 'RP' AND 'BR' AND 'CAM'
920403*       MOVE +100    TO  SQLCODE
920403*       MOVE 'NA189'  TO WS-MESSAGE-NUMBER1
920403*       IF COMM-PREVIOUS-TRANID NOT = 'NAI0'
920403*           GO TO 0310-INITIATE-ROUTER
920403*       ELSE
920403*           MOVE 'Y'    TO WS-RECORD-NOTFND
920403*           PERFORM 0380-BUMP-ERROR-MESSAGES.

           IF IND-LAST-NAME IS NEGATIVE
               MOVE SPACES TO LAST-NAME.
           IF IND-FIRST-NAME IS NEGATIVE
               MOVE SPACES TO FIRST-NAME.
           IF IND-MIDDLE-NAME IS NEGATIVE
               MOVE SPACES TO MIDDLE-NAME.
           IF IND-PREFIX IS NEGATIVE
               MOVE SPACES TO PREFIX.
           IF IND-SUFFIX1 IS NEGATIVE
               MOVE SPACES TO SUFFIX1.
           IF IND-SUFFIX2 IS NEGATIVE
               MOVE SPACES TO SUFFIX2.
           IF IND-COMPANY-NAME IS NEGATIVE
               MOVE SPACES TO COMPANY-NAME.
           IF IND-BIRTH-DATE IS NEGATIVE
               MOVE ZEROES TO BIRTH-DATE.
           IF IND-DISPLAY-NAME IS NEGATIVE
               MOVE SPACES TO DISPLAY-NAME.
           IF IND-NICKNAME IS NEGATIVE
               MOVE SPACES TO NICKNAME.
           IF IND-ADDRESS2 IS NEGATIVE
               MOVE SPACES TO ADDRESS2.
           IF IND-ASSOCIATION1 IS NEGATIVE
               MOVE SPACES TO ASSOCIATION1.
           IF IND-ASSOCIATION2 IS NEGATIVE
               MOVE SPACES TO ASSOCIATION2.
           IF IND-ASSOCIATION3 IS NEGATIVE
               MOVE SPACES TO ASSOCIATION3.
           IF IND-FAX-PHONE IS NEGATIVE
               MOVE SPACES TO FAX-PHONE.
           IF IND-EMAIL1 IS NEGATIVE
               MOVE SPACES TO EMAIL1.
R02539     IF IND-POSTAL-CODE IS NEGATIVE
R02539         MOVE SPACES TO POSTAL-CODE.
R02539     IF IND-COUNTRY-CODE IS NEGATIVE
R02539         MOVE SPACES TO COUNTRY-CODE.

           EXEC SQL
               CLOSE AGNTCUR
           END-EXEC.

           IF CASE-FETCH-SW = 'Y'
               EXEC SQL CLOSE CASECUR END-EXEC.

      *    IF  COMM-CUST-TYPE = 'CA'
      *        PERFORM  0255-LOCK-IOMOD.

       0280-MOVE-SPACES-TO-MAP.
           DISPLAY '0280-MOVE-SPACES-TO-MAP PARA'
R00700     MOVE EMAIL1             TO MAP-EMAIL1O      WSQ-EMAIL1.
           MOVE IDNTITY                TO MAP-CIMO         WSQ-CIM.
           MOVE LAST-NAME          TO MAP-LAST-NAMEO   WSQ-LAST-NAME.
T3384      MOVE PREFIX             TO MAP-PREFIXO      WSQ-PREFIX.
           MOVE FIRST-NAME         TO MAP-FIRST-NAMEO  WSQ-FIRST-NAME.
           MOVE MIDDLE-NAME        TO MAP-MIDDLE-NAMEO WSQ-MIDDLE-NAME.
           MOVE SUFFIX1            TO MAP-SUFFIX1O     WSQ-SUFFIX1.
           MOVE SUFFIX2            TO MAP-SUFFIX2O     WSQ-SUFFIX2.
           MOVE COMPANY-IN-ADDRESS TO MAP-COMP-IN-ADDO
                                      WSQ-COMPANY-IN-ADDRESS.
           MOVE COMPANY-NAME       TO MAP-COMP-NAMEO   WSQ-COMPANY-NAME.
           MOVE DISPLAY-NAME       TO MAP-DIS-NAMEO    WSQ-DISPLAY-NAME.
           MOVE NICKNAME           TO MAP-NICKNAMEO    WSQ-NICKNAME.
           MOVE ADDRESS1           TO MAP-ADDRESS1O    WSQ-ADDRESS1.
           MOVE ADDRESS2           TO MAP-ADDRESS2O    WSQ-ADDRESS2.
           MOVE CITY               TO MAP-CITYO        WSQ-CITY.
           MOVE STATE              TO MAP-STATEO       WSQ-STATE.
           MOVE ZIP                TO MAP-ZIPO         WSQ-ZIP.
           MOVE ZIP-PLUS4          TO MAP-ZIP-PLUS4O   WSQ-ZIP-PLUS4.
R02539     MOVE COUNTRY-CODE       TO MAP-COUNTRY-CDO WSQ-COUNTRY-CODE.
R02539     MOVE POSTAL-CODE        TO MAP-POSTAL-CDO   WSQ-POSTAL-CODE.
           MOVE AREA-CODE          TO MAP-AREA-CODEO   WSQ-AREA-CODE.
           MOVE PHONE              TO WS-PHONE         WSQ-PHONE.
           MOVE WS-PHONE3          TO MAP-PHONE3O.
           MOVE WS-PHONE4          TO MAP-PHONE4O.
           MOVE PHONE-EXTENSION    TO MAP-PHONE-EXTO
                                      WSQ-PHONE-EXTENSION.
           DISPLAY 'MAP-COUNTRY-CDO' MAP-COUNTRY-CDO.
           DISPLAY 'MAP-POSTAL-CDO' MAP-POSTAL-CDO.
900837     MOVE FAX-AREA-CODE      TO MAP-FAX-AREA-CDO
900837                                WSQ-FAX-AREA-CODE.
900837     MOVE FAX-PHONE          TO WS-PHONE         WSQ-FAX-PHONE.
900837     MOVE WS-PHONE3          TO MAP-FAX-PHONE3O.
900837     MOVE WS-PHONE4          TO MAP-FAX-PHONE4O.
R04023     MOVE ORIGINAL-STATE     TO MAP-ISSUE-STATEO
R04023                                WSQ-ISSUE-STATE.
           MOVE SSN-NUM            TO MAP-SSNO         WSQ-SSN.
           MOVE SEX                TO MAP-SEXO         WSQ-SEX.
           MOVE 'Y'                TO MAP-DIS-CHG-INDO WSQ-DISP-IND.
           MOVE FINALST-REAS-CODE  TO MAP-FINALST-CDO
                                      WSQ-FINALST-REAS-CODE.
           MOVE FINALST-OVRD-IND  TO  MAP-ADDR-OVRIDEO
                                      WSQ-FINALST-OVRD-IND.
           MOVE CHANGE-LOGON       TO WSQ-CHANGE-LOGON.
MANJU1     MOVE CHANGE-LOGON       TO MAP-LOGONO.
MANJU *    IF  CHANGE-LOGON NOT EQUAL SPACES AND
      *        CHANGE-LOGON NOT = LOW-VALUES
      *            MOVE CHANGE-LOGON TO WR-EMP-PSINUM
      *            EXEC CICS READ DATASET('EMPLID')
      *                           INTO   (WR-EMPLOYEE-RECORD)
      *                           RIDFLD (WR-EMP-PSINUM)
990721*                           LENGTH (LENGTH OF WR-EMPLOYEE-RECORD)
      *                           RESP   (WS-CICS-RESP)
      *                           END-EXEC
      *            MOVE WR-EMP-NAME TO MAP-LOGONO
001220** HANDLE DUP LOGONID- JUST DISPLAY LOGONID INSTEAD OF CICS ERROR
001220*            IF WS-CICS-RESP = DFHRESP(DUPKEY)
001220*               MOVE WR-EMP-PSINUM TO MAP-LOGONO
001220*            ELSE
      *            IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND
      *                WS-CICS-RESP NOT = DFHRESP(NOTFND)
      *                    GO TO 9999-CICS-ERROR.

           MOVE RECORD-STATUS      TO WSQ-RECORD-STATUS.
           MOVE RECORD-STATUS      TO WT-C2006-STATUS-CD.
           MOVE WT-CNTL2006        TO TURBO-CNTL-AREA.
           PERFORM 10000-CALL-GSF.
           MOVE TURBO-CNTL-AREA    TO WT-CNTL2006.
      *    MOVE ZERO TO WT-C2006-RETURN
           IF  WT-C2006-RETURN EQUAL ZEROES
               MOVE WT-C2006-STAT-DESC TO MAP-CUST-STATUSO
           ELSE
               MOVE RECORD-STATUS  TO MAP-CUST-STATUSO.
MANJU      MOVE RECORD-STATUS  TO MAP-CUST-STATUSO.

           MOVE ALT-ADDRESS-IND    TO MAP-ALT-ADDRO
                                      WSQ-ALT-ADDRESS-IND.
           MOVE FUTURE-ADDRESS-IND TO MAP-FUTURE-ADDRO
                                      WSQ-FUTURE-ADDRESS-IND.
           MOVE SITE-CODE          TO MAP-SITE-CODEO   WSQ-SITE-CODE.
           MOVE SITE-CODE          TO WT-C0161-SITE.
           MOVE WT-CNTL0161        TO TURBO-CNTL-AREA.
           PERFORM 10000-CALL-GSF.
           MOVE TURBO-CNTL-AREA    TO WT-CNTL0161.
           IF  WT-C0161-RETURN EQUAL ZEROES
               MOVE WT-C0161-COMPANY-NAME TO MAP-SITE-DESCO
           ELSE
               MOVE SPACES         TO MAP-SITE-DESCO.

GRB        MOVE COMBINED-STATUS    TO WSQ-COMBINED-STATUS.
GRB        MOVE ADDRESS-KEY1       TO WSQ-ADDRESS-KEY1.

           MOVE ASSOCIATION1       TO MAP-ASSOC1O      WSQ-ASSOCIATION1.
           MOVE ASSOCIATION2       TO MAP-ASSOC2O      WSQ-ASSOCIATION2.
           MOVE ASSOCIATION3       TO MAP-ASSOC3O      WSQ-ASSOCIATION3.
SPC   ***  IF ASSOCIATION1 = 'YERKE'
SPC   ***    AND ASSOCIATION2 = 'NBRL'
SPC   ***          MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A
SPC   ***                                   MAP-ASSOC2A.
LMB   ***                                   MAP-ASSOC3A.

920124***  IF  ASSOCIATION1 = 'FREDM'
920124***  AND ASSOCIATION2 = SPACES
920124***  AND ASSOCIATION3 = SPACES
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A
920124***                               MAP-ASSOC2A
920124***                               MAP-ASSOC3A.
920124***  IF  ASSOCIATION1 = 'FREDM'
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A.
920124***  IF  ASSOCIATION2 = 'FREDM'
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC2A.
920124***  IF  ASSOCIATION3 = 'FREDM'
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC3A.

           MOVE ENTITY-TYPE        TO WSQ-ENTITY-TYPE.
           MOVE ENTITY-TYPE        TO WT-C8997-SYSTEM-CD.
           MOVE WT-CNTL8997        TO TURBO-CNTL-AREA.
           PERFORM 10000-CALL-GSF.
           MOVE TURBO-CNTL-AREA    TO WT-CNTL8997.
           IF  WT-C8997-RETURN EQUAL ZEROES
               MOVE WT-C8997-SYS-TYPE  TO MAP-ENTITY-TYPEO
           ELSE
               MOVE ENTITY-TYPE        TO MAP-ENTITY-TYPEO.
           MOVE ENTITY-TYPE        TO MAP-ENTITY-TYPEO.

           MOVE BIRTH-DATE         TO WS-DB2-DATE.
Y2KIMR*
Y2KIMR* IMR CHANGE IMR7  BEGIN
Y2KIMR*
Y2KIMR*    MOVE WS-DB2-DD-R        TO WS-DAY.
Y2KIMR*    MOVE WS-DB2-MM-R        TO WS-MONTH.
Y2KIMR*    MOVE WS-DB2-YY-R        TO WS-YEAR.
Y2KIMR*    MOVE '/'                TO SLASH-1 SLASH-2.
Y2KIMR*    MOVE WS-DATE-EIGHT      TO MAP-BIRTH-DATEO.
Y2KIMR*
Y2KIMR     IF    WS-DB2-DATE  EQUAL  ZEROS
Y2KIMR        MOVE ZEROS    TO  Y2K-WK-DATE-TEN
Y2KIMR                          MAP-BIRTH-DATEO
Y2KIMR     ELSE
Y2KIMR      MOVE WS-DB2-MM-R         TO Y2K-WK-MONTH
Y2KIMR      MOVE WS-DB2-DD-R         TO Y2K-WK-DAY
Y2KIMR      MOVE WS-DB2-CC-R         TO Y2K-WK-CENT
Y2KIMR      MOVE WS-DB2-YY-R         TO Y2K-WK-YEAR
Y2KIMR      MOVE '-'                 TO Y2K-WK-SLASH-1 Y2K-WK-SLASH-2
Y2KIMR      MOVE Y2K-WK-DATE-TEN TO  MAP-BIRTH-DATEO .
Y2KIMR*
Y2KIMR* IMR CHANGE IMR7 END
Y2KIMR*


Y2KIMR*
Y2KIMR* IMR CHANGE IMR7A BEGIN
Y2KIMR*
Y2KIMR*    MOVE BR-CC              TO WS-YY-A.
Y2KIMR*    MOVE BR-YY              TO WS-YY-A.
Y2KIMR*    MOVE BR-MM              TO WS-MM-A.
Y2KIMR*    MOVE BR-DD              TO WS-DD-A.
Y2KIMR*    MOVE WS-DATE            TO WS-DATE-R.
Y2KIMR*    MOVE WS-DATE-R          TO WSQ-BIRTH-DATE.
Y2KIMR*
Y2KIMR     MOVE BIRTH-DATE         TO WS-BIRTH-DATE.
Y2KIMR     MOVE BR-CC              TO Y2K-WK-VARIABLE2-CC.
Y2KIMR     MOVE BR-YY              TO Y2K-WK-VARIABLE2-YY.
Y2KIMR     MOVE BR-MM              TO Y2K-WK-VARIABLE2-MM.
Y2KIMR     MOVE BR-DD              TO Y2K-WK-VARIABLE2-DD.
Y2KIMR     MOVE Y2K-WK-VARIABLE2   TO WSQ-BIRTH-DATE.
Y2KIMR*
Y2KIMR* IMR CHANGE IMR7A END
Y2KIMR*

           MOVE EFFECTIVE-DATE     TO WS-DB2-DATE.
           MOVE WS-DB2-DD-R        TO WS-DAY.
           MOVE WS-DB2-MM-R        TO WS-MONTH.
           MOVE WS-DB2-YY-R        TO WS-YEAR.
           MOVE '/'                TO SLASH-1 SLASH-2.
           MOVE WS-DATE-EIGHT      TO MAP-EFF-DATEO.

           MOVE EFFECTIVE-DATE     TO WS-EFFECTIVE-DATE
           MOVE EF-YY              TO WS-YY-A.
           MOVE EF-MM              TO WS-MM-A.
           MOVE EF-DD              TO WS-DD-A.
           MOVE WS-DATE            TO WS-DATE-R.
           MOVE WS-DATE-R          TO WSQ-EFFECTIVE-DATE.

           MOVE CHANGE-DATE        TO WS-DB2-TIMESTAMP.
           MOVE WS-TS-DD-R         TO WS-DAY.
           MOVE WS-TS-MM-R         TO WS-MONTH.
           MOVE WS-TS-YY-R         TO WS-YEAR.
           MOVE '/'                TO SLASH-1 SLASH-2.
           MOVE WS-DATE-EIGHT      TO MAP-CHANGE-DATEO.

           MOVE CHANGE-DATE        TO WS-CHANGE-DATE.
           MOVE CO-YY              TO WS-YY-A.
           MOVE CO-MM              TO WS-MM-A.
           MOVE CO-DD              TO WS-DD-A.
           MOVE WS-DATE            TO WS-DATE-R.
           MOVE WS-DATE-R          TO WSQ-CHANGE-DATE.

MANJU *    EXEC SQL SELECT CARRIER_CODE,
R04023*                    PREV_CARRIER
R04023*    INTO :CASEX-CARRIER-CODE,
R04023*         :CASEX-PREV-CARRIER
R04023*    FROM CASEXV01
11735A*    FROM CASE_MASTER
R04023*    WHERE CASENAME#IDNTITY  = :COMM-IDNTITY
R04023*    END-EXEC.
MANJU      MOVE SPACES TO CASEX-CARRIER-CODE CASEX-PREV-CARRIER.
R04023     MOVE CASEX-CARRIER-CODE  TO WSQ-CARRIER-CODE.
R04023     MOVE CASEX-PREV-CARRIER  TO WSQ-PREV-CARRIER.
R04023**************************************************
R04023* DISPLAY ISSUE STATE ONLY FOR AIG
R04342* DISPLAY ISSUE STATE ONLY FOR AIG AND ACM
R04023**************************************************
R04023     IF (CASEX-CARRIER-CODE  = '9D'
R04023        AND
R04023*       CASEX-PREV-CARRIER   = 'SH')
R04254*       CASEX-PREV-CARRIER   = ('SH' OR 'SV'))
R04281*       CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN'))
R04280        CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN' OR 'SP'
R04209                                     OR 'JJ'
R04832                                     OR 'JA'
R04472                                     OR 'IG'))
R04023        OR
R04023*       (CASEX-CARRIER-CODE  = 'SH')
R04254*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV')
R04281*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN')
R04280        (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN' OR 'SP'
R04472*                                   OR 'IG'
R04342                                    OR 'IG' OR 'MV' OR 'MB'
R04209             OR 'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'
R04934             OR 'NO' OR 'NU' OR 'NX'
R04209             OR 'LD' OR 'LI' OR 'MI'
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'
R04209             OR 'NQ' OR 'NS'
R04209             OR 'UD' OR 'UT'
R04209             OR 'VI' OR 'WF'
R04832** NEW CIGNA CARRIERS
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'
R04832             OR 'JH'
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'
R04832             )
R04023         MOVE ATTRB-PROT-ASKIP-DRK
R04023                     TO MAP-LITERAL8A    MAP-LITERAL6A
R04023                        MAP-LITERAL5A    MAP-SUFFIX1A
R04023                        MAP-SUFFIX2A     MAP-NICKNAMEA
R04023                        MAP-LITERAL4A    MAP-SEXA
R04023         MOVE SPACES TO MAP-LITERAL8O    MAP-LITERAL6O
R04023                        MAP-LITERAL5O    MAP-SUFFIX1O
R04023                        MAP-SUFFIX2O     MAP-NICKNAMEO
R04023                        MAP-LITERAL4O    MAP-SEXO
R04023     ELSE
960530*    IF (ENTITY-TYPE = 'AM' OR 'CA' OR 'LD')
960530     IF (ENTITY-TYPE = 'AM' OR 'LD')
900837         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA
               MOVE ATTRB-PROT-ASKIP-DRK
                           TO MAP-LITERAL1A    MAP-LITERAL2A
                              MAP-LITERAL3A    MAP-LITERAL4A
                              MAP-LITERAL5A    MAP-LITERAL6A
                              MAP-LITERAL7A    MAP-LITERAL8A
                              MAP-LITERAL9A    MAP-LITERAL10A
                              MAP-FIRST-NAMEA  MAP-MIDDLE-NAMEA
                              MAP-LAST-NAMEA   MAP-SUFFIX1A
                              MAP-SUFFIX2A     MAP-NICKNAMEA
                              MAP-SEXA         MAP-BIRTH-DATEA
                              MAP-COMP-IN-ADDA
R04023                        MAP-ISSUE-STATEA
R04023                        MAP-ISS-LITERALA
R04023                        MAP-ST-LITERALA
               MOVE SPACES TO MAP-FIRST-NAMEO  MAP-MIDDLE-NAMEO
                              MAP-LAST-NAMEO   MAP-SUFFIX1O
                              MAP-SUFFIX2O     MAP-NICKNAMEO
                              MAP-SEXO         MAP-BIRTH-DATEO
                              MAP-COMP-IN-ADDO
R04023                        MAP-ISSUE-STATEO
                              WSQ-FIRST-NAME   WSQ-MIDDLE-NAME
                              WSQ-LAST-NAME    WSQ-SUFFIX1
                              WSQ-SUFFIX2      WSQ-NICKNAME
R04023                        WSQ-ISSUE-STATE
                              WSQ-SEX
R04023     ELSE
R04023          MOVE ATTRB-PROT-ASKIP-DRK TO
R04023                        MAP-ISSUE-STATEA
R04023                        MAP-ISS-LITERALA
R04023                        MAP-ST-LITERALA
R04023          MOVE SPACES TO WSQ-ISSUE-STATE
R04023                         MAP-ISSUE-STATEO.

       0290-MOVE-OUT-MAP-FIELDS.

           MOVE COM-FIRST-NAME         TO MAP-FIRST-NAMEO.
           MOVE COM-MIDDLE-NAME        TO MAP-MIDDLE-NAMEO.
           MOVE COM-LAST-NAME          TO MAP-LAST-NAMEO.
T3384      MOVE COM-PREFIX             TO MAP-PREFIXO.
           MOVE COM-SUFFIX1            TO MAP-SUFFIX1O.
           MOVE COM-SUFFIX2            TO MAP-SUFFIX2O.
           MOVE COM-NICKNAME           TO MAP-NICKNAMEO.
           MOVE COM-COMPANY-NAME       TO MAP-COMP-NAMEO.
           MOVE COM-DISPLAY-NAME       TO MAP-DIS-NAMEO.
           MOVE COM-ADDRESS1           TO MAP-ADDRESS1O.
           MOVE COM-SEX                TO MAP-SEXO.
           MOVE COM-ADDRESS2           TO MAP-ADDRESS2O.
           MOVE COM-CITY               TO MAP-CITYO.
           MOVE COM-COMPANY-IN-ADDRESS TO MAP-COMP-IN-ADDO.
           MOVE COM-STATE              TO MAP-STATEO.
           MOVE COM-ZIP                TO MAP-ZIPO.
           MOVE COM-ZIP-PLUS4          TO MAP-ZIP-PLUS4O.
           MOVE COM-FINALST-OVRD-IND   TO MAP-ADDR-OVRIDEO.
           MOVE COM-AREA-CODE          TO MAP-AREA-CODEO.
           MOVE COM-PHONE              TO WS-PHONE.
           MOVE WS-PHONE3              TO MAP-PHONE3O.
           MOVE WS-PHONE4              TO MAP-PHONE4O.
           MOVE COM-SSN                TO MAP-SSNO.
           MOVE COM-PHONE-EXTENSION    TO MAP-PHONE-EXTO.
900837     MOVE COM-FAX-AREA-CODE      TO MAP-FAX-AREA-CDO.
900837     MOVE COM-FAX-PHONE          TO WS-PHONE.
900837     MOVE WS-PHONE3              TO MAP-FAX-PHONE3O.
900837     MOVE WS-PHONE4              TO MAP-FAX-PHONE4O.
R04023     MOVE COM-ISSUE-STATE        TO MAP-ISSUE-STATEO.
R9031A     MOVE COM-EMAIL1             TO MAP-EMAIL1O.
           MOVE COM-ALT-ADDRESS-IND    TO MAP-ALT-ADDRO.
           MOVE COM-ENTITY-TYPE        TO MAP-ENTITY-TYPEO.
           MOVE COM-DISP-IND           TO MAP-DIS-CHG-INDO.
           MOVE COM-FUTURE-ADDRESS-IND TO MAP-FUTURE-ADDRO.
           MOVE COM-RECORD-STATUS      TO MAP-CUST-STATUSO.
           MOVE COM-SITE-CODE          TO MAP-SITE-CODEO.
           MOVE COM-ASSOCIATION1       TO MAP-ASSOC1O.
           MOVE COM-ASSOCIATION2       TO MAP-ASSOC2O.
           MOVE COM-ASSOCIATION3       TO MAP-ASSOC3O.
R02539     MOVE COM-POSTAL-CODE        TO MAP-POSTAL-CDO.
R02539     MOVE COM-COUNTRY-CODE       TO MAP-COUNTRY-CDO.
            DISPLAY ' 29O PARA MAP-POSTAL-CDO' MAP-POSTAL-CDO.
            DISPLAY ' 29O PARA MAP-COUNTRY-CDO' MAP-COUNTRY-CDO.
SPC   ***  IF COM-ASSOCIATION1 = 'YERKE'
SPC   ***    AND COM-ASSOCIATION2 = 'NBRL'
SPC   ***          MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A
SPC   ***                                   MAP-ASSOC2A.
LMB   ***                                   MAP-ASSOC3A.

920124***  IF  COM-ASSOCIATION1 = 'FREDM'
920124***  AND COM-ASSOCIATION2 = SPACES
920124***  AND COM-ASSOCIATION3 = SPACES
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A
920124***                               MAP-ASSOC2A
920124***                               MAP-ASSOC3A.
920124***  IF  COM-ASSOCIATION1 = 'FREDM'
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC1A.
920124***  IF  COM-ASSOCIATION2 = 'FREDM'
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC2A.
920124***  IF  COM-ASSOCIATION3 = 'FREDM'
920124***      MOVE ATTRB-PROT-ASKIP TO MAP-ASSOC3A.

961037     IF  ENTITY-TYPE = 'RP'
961037         MOVE ATTRB-PROT-ASKIP TO MAP-ENTITY-TYPEA
961037     END-IF.

           MOVE COM-CIM                TO MAP-CIMO.
           MOVE COM-FINALST-REAS-CODE  TO MAP-FINALST-CDO.
      *    MOVE WS-DEMO-USER-NAME      TO MAP-LOGONO.
      *    IF  COM-CHANGE-LOGON NOT EQUAL SPACES AND
      *        COM-CHANGE-LOGON NOT = LOW-VALUES
      *            MOVE COM-CHANGE-LOGON TO WR-EMP-PSINUM
      *            EXEC CICS READ DATASET('EMPLID')
      *                           INTO   (WR-EMPLOYEE-RECORD)
      *                           RIDFLD (WR-EMP-PSINUM)
990721*                           LENGTH (LENGTH OF WR-EMPLOYEE-RECORD)
      *                           RESP   (WS-CICS-RESP)
      *                           END-EXEC
      *            MOVE WR-EMP-NAME TO MAP-LOGONO
001220** HANDLE DUP LOGONID- JUST DISPLAY LOGONID INSTEAD OF CICS ERROR
001220*            IF WS-CICS-RESP = DFHRESP(DUPKEY)
001220*               MOVE WR-EMP-PSINUM TO MAP-LOGONO
001220*            ELSE
      *            IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND
      *                WS-CICS-RESP NOT = DFHRESP(NOTFND)
      *                    GO TO 9999-CICS-ERROR.


           MOVE COM-RECORD-STATUS  TO WT-C2006-STATUS-CD.
           MOVE WT-CNTL2006        TO TURBO-CNTL-AREA.
           PERFORM 10000-CALL-GSF.
           MOVE TURBO-CNTL-AREA    TO WT-CNTL2006.
           IF  WT-C2006-RETURN EQUAL ZEROES
               MOVE WT-C2006-STAT-DESC TO MAP-CUST-STATUSO
           ELSE
               MOVE COM-RECORD-STATUS  TO MAP-CUST-STATUSO.

           MOVE COM-SITE-CODE          TO WT-C0161-SITE.
           MOVE WT-CNTL0161            TO TURBO-CNTL-AREA.
           PERFORM 10000-CALL-GSF.
           MOVE TURBO-CNTL-AREA    TO WT-CNTL0161.
           IF  WT-C0161-RETURN EQUAL ZEROES
               MOVE WT-C0161-COMPANY-NAME TO MAP-SITE-DESCO
           ELSE
               MOVE SPACES         TO MAP-SITE-DESCO.

           MOVE COM-ENTITY-TYPE    TO WT-C8997-SYSTEM-CD.
           MOVE WT-CNTL8997        TO TURBO-CNTL-AREA.
           PERFORM 10000-CALL-GSF.
           MOVE TURBO-CNTL-AREA    TO WT-CNTL8997.
           IF  WT-C8997-RETURN EQUAL ZEROES
               MOVE WT-C8997-SYS-TYPE  TO MAP-ENTITY-TYPEO
           ELSE
               MOVE COM-ENTITY-TYPE        TO MAP-ENTITY-TYPEO.

Y2KIMR*
Y2KIMR      MOVE COM-BIRTH-DATE TO Y2K-WK-VARIABLE2.
Y2KIMR      MOVE Y2K-WK-VARIABLE2-MM TO Y2K-WK-MONTH.
Y2KIMR      MOVE Y2K-WK-VARIABLE2-DD TO Y2K-WK-DAY .
Y2KIMR      MOVE Y2K-WK-VARIABLE2-CC TO Y2K-WK-CENT.
Y2KIMR      MOVE Y2K-WK-VARIABLE2-YY TO Y2K-WK-YEAR .
Y2KIMR      MOVE '-'                 TO Y2K-WK-SLASH-1 Y2K-WK-SLASH-2.
Y2KIMR      MOVE Y2K-WK-DATE-TEN TO MAP-BIRTH-DATEO.
Y2KIMR*
Y2KIMR* IMR CHANGE IMR8 END
Y2KIMR*

           MOVE COM-CHANGE-DATE TO WS-DATE-R
           MOVE WS-DATE-R       TO WS-DATE.
           MOVE WS-MM           TO WS-MONTH.
           MOVE WS-DD           TO WS-DAY.
           MOVE WS-YY           TO WS-YEAR.
           MOVE '/'             TO SLASH-1 SLASH-2.
           MOVE WS-DATE-EIGHT   TO MAP-CHANGE-DATEO.

           MOVE COM-EFFECTIVE-DATE TO WS-DATE-R.
           MOVE WS-DATE-R          TO WS-DATE.
           MOVE WS-MM              TO WS-MONTH.
           MOVE WS-DD              TO WS-DAY.
           MOVE WS-YY              TO WS-YEAR.
           MOVE '/'                TO SLASH-1 SLASH-2.
           MOVE WS-DATE-EIGHT      TO MAP-EFF-DATEO.

R04023*    EXEC SQL SELECT CARRIER_CODE,
R04023*                    PREV_CARRIER
R04023*    INTO :CASEX-CARRIER-CODE,
R04023*         :CASEX-PREV-CARRIER
R04023*    FROM CASEXV01
11735A*    FROM CASE_MASTER
R04023*    WHERE CASENAME#IDNTITY= :COMM-IDNTITY
R04023*    END-EXEC.
MANJU      MOVE SPACES TO CASEX-CARRIER-CODE CASEX-PREV-CARRIER.
R04023     MOVE CASEX-CARRIER-CODE  TO WSQ-CARRIER-CODE.
R04023     MOVE CASEX-PREV-CARRIER  TO WSQ-PREV-CARRIER.
R04023**************************************************
R04023* DISPLAY ISSUE STATE ONLY FOR AIG
R04023**************************************************
R04023     IF (CASEX-CARRIER-CODE  = '9D'
R04023        AND
R04023*       CASEX-PREV-CARRIER   = 'SH')
R04254*       CASEX-PREV-CARRIER   = ('SH' OR 'SV'))
R04281*       CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN'))
R04280        CASEX-PREV-CARRIER   = ('SH' OR 'SV' OR 'MN' OR 'SP'
R04209                                     OR 'JJ'
R04832                                     OR 'JA'
R04472                                     OR 'IG'))
R04023        OR
R04023*       (CASEX-CARRIER-CODE  = 'SH')
R04254*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV')
R04281*       (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN')
R04280        (CASEX-CARRIER-CODE  = 'SH' OR 'SV' OR 'MN' OR 'SP'
R04472                                    OR 'IG'
R04209             OR 'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'
R04934             OR 'NO' OR 'NU' OR 'NX'
R04209             OR 'LD' OR 'LI' OR 'MI'
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'
R04209             OR 'NQ' OR 'NS'
R04209             OR 'UD' OR 'UT'
R04209             OR 'VI' OR 'WF'
R04832** NEW CIGNA CARRIERS
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'
R04832             OR 'JH'
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'
R04832             )
R04023         MOVE ATTRB-PROT-ASKIP-DRK
R04023                     TO MAP-LITERAL8A    MAP-LITERAL6A
R04023                        MAP-LITERAL5A    MAP-SUFFIX1A
R04023                        MAP-SUFFIX2A     MAP-NICKNAMEA
R04023                        MAP-LITERAL4A    MAP-SEXA
R04023         MOVE SPACES TO MAP-LITERAL8O    MAP-LITERAL6O
R04023                        MAP-LITERAL5O    MAP-SUFFIX1O
R04023                        MAP-SUFFIX2O     MAP-NICKNAMEO
R04023                        MAP-LITERAL4O    MAP-SEXO
R04023     ELSE
           IF COM-CASE-SW = 'Y'  AND
960530       (COM-ENTITY-TYPE = 'AM' OR 'LD')
900837         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA
               MOVE ATTRB-PROT-ASKIP-DRK
                           TO MAP-LITERAL1A    MAP-LITERAL2A
                              MAP-LITERAL3A    MAP-LITERAL4A
                              MAP-LITERAL5A    MAP-LITERAL6A
                              MAP-LITERAL7A    MAP-LITERAL8A
                              MAP-LITERAL9A    MAP-LITERAL10A
                              MAP-FIRST-NAMEA  MAP-MIDDLE-NAMEA
                              MAP-LAST-NAMEA   MAP-SUFFIX1A
                              MAP-SUFFIX2A     MAP-NICKNAMEA
                              MAP-SEXA         MAP-BIRTH-DATEA
                              MAP-COMP-IN-ADDA
               MOVE SPACES TO MAP-FIRST-NAMEO  MAP-MIDDLE-NAMEO
                              MAP-LAST-NAMEO   MAP-SUFFIX1O
                              MAP-SUFFIX2O     MAP-NICKNAMEO
                              MAP-SEXO         MAP-BIRTH-DATEO
                              MAP-COMP-IN-ADDO
R04023                        MAP-ISSUE-STATEO
R04023     ELSE
R04023         MOVE ATTRB-PROT-ASKIP-DRK TO
R04023                        MAP-ISSUE-STATEA
R04023                        MAP-ISS-LITERALA
R04023                        MAP-ST-LITERALA
R04023         MOVE SPACES TO MAP-ISSUE-STATEO.
R04023
961037     IF COM-ENTITY-TYPE = 'RP'
961037         MOVE ATTRB-PROT-ASKIP        TO MAP-ENTITY-TYPEA.
R03179     IF COM-ENTITY-TYPE = 'CA'
R03179         MOVE ATTRB-PROT-ASKIP        TO MAP-EMAIL1A MAP-EMAIL2A.


       0310-INITIATE-ROUTER.
           DISPLAY 'INSIDE 0310-INITIATE-ROUTER PARA'
      *    PERFORM 0415-UNLOCK-IOMOD.

           MOVE '00'              TO COMM-NEXT-FUNCTION.
           MOVE ZEROES TO COMM-CURSOR-POSN COMM-MSG-COUNT
               COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.
      * --------------------------------------------------------------*
      * NOT FOUND ERROR MESSAGE ON DB2 LOOKUP                         *
      * --------------------------------------------------------------*
           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED  = 'Y'
               MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)
               COMM-MSG-ID (3) COMM-MSG-ID (4)
               MOVE 'NAI0'            TO COMM-PREVIOUS-TRANID
           ELSE
              IF SQLCODE = 100
                  MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1)
                  MOVE 'E' TO COMM-MSG-MAX-SEVERITY
              ELSE
                 MOVE 'NAI0'            TO COMM-PREVIOUS-TRANID.

           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.

           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.
           MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.

           IF COMM-MSG-MAX-SEVERITY = 'E'
               MOVE NA-COMMAREA TO WS-LINK-SIX-HUNDRED
               EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)
                                   FROM   (WS-LINK-STORAGE)
                                   LENGTH (WS-ZERO-LENGTH)
                                   ITEM   (WS-TS-ITEM)
                                   RESP   (WS-CICS-RESP)
                                   REWRITE
                                   MAIN
                                   END-EXEC
           ELSE
900837         MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH
               EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)
                                   FROM   (WSQ-COMMAREA)
                                   LENGTH (WS-COMM-DB2-LENGTH)
                                   ITEM   (WS-TS-ITEM)
                                   RESP   (WS-CICS-RESP)
                                   REWRITE
                                   MAIN
                                   END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

      *    EXEC CICS SEND MAP ('SYSBUSY')
      *                   RESP (WS-CICS-RESP)
      *                   END-EXEC.

      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      *         GO TO 9999-CICS-ERROR.

MANJU *    EXEC CICS START TRANSID('NAI0')
      *                    TERMID (EIBTRMID)
      *                    RESP   (WS-CICS-RESP)
      *                    END-EXEC.
      *
      *    IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
      *         GO TO 9999-CICS-ERROR.

           EXEC CICS RETURN
                     END-EXEC.

      * --------------------------------------------------------------*
      *   THE FOLLOWING CODE EXECUTES THE PSI TUTORIAL OR ONLINE HELP *
      *   FUNCTION  IT STORES ANY CHANGES MADE ON THE MAP IN A        *
      *   TEMPORARY STORAGE QUEUE, THEN IT INITIATES THE TUTORIAL     *
      *   COMMAREA AND XCNTL'S TO THE TUTORIAL PROGRAM                *
      * --------------------------------------------------------------*

       0340-PROCESS-SCREEN-CLEAR.

      *    PERFORM 0415-UNLOCK-IOMOD.

           MOVE 'NAI0' TO COMM-PREVIOUS-TRANID.
           MOVE '00'   TO COMM-NEXT-FUNCTION.
           MOVE ZERO   TO COMM-CURSOR-POSN     COMM-MSG-COUNT
                          COMM-MAIL-LINE-COUNT COMM-CUST-TOKEN-COUNT.
           MOVE SPACES TO COMM-MSG-MAX-SEVERITY.
           MOVE SPACES TO COMM-MSG-ID (1) COMM-MSG-ID (2)
                          COMM-MSG-ID (3) COMM-MSG-ID (4).

           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.

           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)
                               FROM   (WS-TS-QUEUE)
                               LENGTH (WS-ZERO-LENGTH)
                               ITEM   (WS-TS-ITEM)
                               RESP   (WS-CICS-RESP)
                               REWRITE
                               MAIN
                               END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           MOVE SPACES TO WS-NULL-FIELD.
           EXEC CICS SEND FROM (WS-NULL-FIELD)
                          RESP (WS-CICS-RESP)
                          ERASE
                          LENGTH(72)
                          END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           EXEC CICS RETURN
                     END-EXEC.


       0350-WRITE-NAQ1-QUEUE.

           MOVE SPACES TO WSQ-COMMAREA.
           MOVE ZERO   TO WSQ-CURSOR-POSN     WSQ-MSG-COUNT
                          WSQ-MAIL-LINE-COUNT WSQ-CUST-TOKEN-COUNT.
           MOVE '00'   TO WSQ-NEXT-FUNCTION.

900837     MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)
                               FROM   (WSQ-COMMAREA)
                               LENGTH (WS-COMM-DB2-LENGTH)
                               ITEM   (WS-TS-ITEM)
                               RESP   (WS-CICS-RESP)
                               MAIN
                               END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           MOVE 'NAQ1'                 TO WS-TS-QUEUE-TRANID.
           MOVE EIBTRMID               TO WS-TS-QUEUE-TERMID.
           EXEC CICS READQ TS QUEUE  (WS-TS-QUEUE-NAME)
                              SET    (ADDRESS OF WS-LINK-STORAGE)
                              LENGTH (WS-ZERO-LENGTH)
                              ITEM   (WS-TS-ITEM)
                              RESP   (WS-CICS-RESP)
                              END-EXEC.


           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           MOVE WS-ZERO-LENGTH TO WS-ZERO-LENGTH.
           COMPUTE WS-LINK-LENGTH = (WS-ZERO-LENGTH - +600).
           MOVE WS-LINK-STORAGE TO WS-TS-QUEUE.

       0360-WRONG-KEY-HIT.

           EXEC CICS RECEIVE MAP    ('NA320M1')
                             RESP   (WS-CICS-RESP)
                             END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL) AND
                WS-CICS-RESP NOT = DFHRESP(MAPFAIL)
                GO TO 9999-CICS-ERROR.

           PERFORM 0110-UPDATE-COMMAND-LINE.
           PERFORM 0120-UPDATE-CIM-MAP-FIELDS.

           IF COMM-MSG-ID (1) = 'NA159'
               MOVE SPACES TO COMM-MSG-ID (1)
           ELSE
           IF COMM-MSG-ID (2) = 'NA159'
               MOVE SPACES TO COMM-MSG-ID (2)
           ELSE
           IF COMM-MSG-ID (3) = 'NA159'
               MOVE SPACES TO COMM-MSG-ID (3)
           ELSE
           IF COMM-MSG-ID (4) = 'NA159'
               MOVE SPACES TO COMM-MSG-ID (4).

           MOVE 'NA030'           TO WS-MESSAGE-NUMBER1.
           PERFORM 0380-BUMP-ERROR-MESSAGES.
           MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(1).
           MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(2).
           MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(3).
           MOVE WSQ-MSG-ID(4)     TO COMM-MSG-ID(4).

           MOVE EIBCPOSN           TO COMM-CURSOR-POSN.
           MOVE 'NAQ1'             TO WS-TS-QUEUE-TRANID.
           MOVE EIBTRMID           TO WS-TS-QUEUE-TERMID.

           IF COMM-KEY-CHANGED = 'Y' OR COMM-CMDLINE-CHANGED = 'Y'
               MOVE 'Y' TO COMM-KEY-CHANGED
               MOVE 'Y' TO COMM-CMDLINE-CHANGED.

           MOVE NA-COMMAREA        TO WSQ-COMM-FIELDS.
           MOVE COM-AGNTNAME       TO WSQ-AGNTNAME.
900837     MOVE LENGTH OF WSQ-COMMAREA TO WS-COMM-DB2-LENGTH.
           EXEC CICS WRITEQ TS QUEUE  (WS-TS-QUEUE-NAME)
                               FROM   (WSQ-COMMAREA)
                               LENGTH (WS-COMM-DB2-LENGTH)
                               ITEM   (WS-TS-ITEM)
                               RESP   (WS-CICS-RESP)
                               REWRITE
                               MAIN
                               END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           IF COMM-SYSTEM-CODE = SPACES OR LOW-VALUES
               MOVE ALL '_' TO MAP-SYSTEM-CDO
           ELSE
              MOVE COMM-SYSTEM-CODE   TO MAP-SYSTEM-CDO.
           IF COMM-ACTION-CODE = SPACES OR LOW-VALUES
               MOVE ALL '_' TO MAP-ACTION-CDO
           ELSE
              MOVE COMM-ACTION-CODE   TO MAP-ACTION-CDO.
           IF COMM-CUSTOMER-INFO = SPACES OR LOW-VALUES
               MOVE ALL '_' TO MAP-CUST-INFOO
           ELSE
              MOVE COMM-CUSTOMER-INFO TO MAP-CUST-INFOO.


           PERFORM 0290-MOVE-OUT-MAP-FIELDS.

           EXEC CICS SEND MAP    ('NA320M1')
                          CURSOR (COMM-CURSOR-POSN)
                          ERASE
                          RESP (WS-CICS-RESP)
                          END-EXEC.

           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                GO TO 9999-CICS-ERROR.

           EXEC CICS RETURN TRANSID ('NAI0')
                            END-EXEC.


       0380-BUMP-ERROR-MESSAGES.

           MOVE COMM-MSG-ID(1)    TO WSQ-MSG-ID(1).
           MOVE COMM-MSG-ID(2)    TO WSQ-MSG-ID(2).
           MOVE COMM-MSG-ID(3)    TO WSQ-MSG-ID(3).
           MOVE COMM-MSG-ID(4)    TO WSQ-MSG-ID(4).
           MOVE WS-MESSAGE-NUMBER1 TO COMM-MSG-ID(1).
           MOVE WSQ-MSG-ID(1)     TO COMM-MSG-ID(2).
           MOVE WSQ-MSG-ID(2)     TO COMM-MSG-ID(3).
           MOVE WSQ-MSG-ID(3)     TO COMM-MSG-ID(4).
           PERFORM 0390-CHECK-ERROR-MESSAGES
               VARYING ACTION-SUB FROM 1 BY 1
                   UNTIL ACTION-SUB > 4.

       0390-CHECK-ERROR-MESSAGES.
           DISPLAY '0390-CHECK-ERROR-MESSAGES PARA'
           IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND
               COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES
               MOVE COMM-MSG-ID (ACTION-SUB) TO WT-C9999-ERROR-CODE
               PERFORM 0410-GET-ERROR-MESSAGE
               PERFORM 0400-CHECK-RETURN-CODE.

       0400-CHECK-RETURN-CODE.
               DISPLAY 'SQLCODE' SQLCODE.
               IF SQLCODE = ZERO
                   MOVE WT-C9999-ERROR-CODE TO WS-C9999-ERROR-CODE
      *            MOVE WT-C9999-SEVERITY-CODE TO WS-C9999-SEVERITY-CODE
                   MOVE EDIT-DESC TO WS-C9999-ERROR-MESSAGE
                   MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (ACTION-SUB)
               ELSE
                  IF COMM-MSG-ID (ACTION-SUB) NOT = SPACES AND
                      COMM-MSG-ID (ACTION-SUB) NOT = LOW-VALUES
                      MOVE '** INVALID ERROR MESSAGE ** ' TO
                           MAP-ERROR-MSGO (ACTION-SUB).

       0410-GET-ERROR-MESSAGE.
           DISPLAY 'EDIT WT-C9999-ERROR-CODE' WT-C9999-ERROR-CODE.
           EXEC SQL
           SELECT EDIT_DESC
              INTO :EDIT-DESC
             FROM EDITCODE
            WHERE EDIT_CD = :WT-C9999-ERROR-CODE
           END-EXEC.

       0420-DB2-ERROR.

           MOVE SQLCODE TO WS-DB2I-MESSAGE.
           MOVE WS-DB2I-MESSAGE TO WS-C9999-ERROR-CODE.
           MOVE "DB2 SQL ERROR" TO WS-C9999-ERROR-MESSAGE.
           MOVE WS-ERROR-FIELDS TO MAP-ERROR-MSGO (1).
           PERFORM 0290-MOVE-OUT-MAP-FIELDS.
           EXEC CICS SEND MAP    ('NA320M1')
                          CURSOR
                          ERASE
                          RESP (WS-CICS-RESP)
                          END-EXEC.
           EXEC CICS RETURN TRANSID ('NAI0')
                     END-EXEC.
           EXEC CICS RETURN
                     END-EXEC.

           COPY CICSERR.
      * --------------------------------------------------------------*
      * THIS ROUTINE TRANSFERS CONTROL TO THE TURBO TABLES FOR EDIT   *
      * VERIFICATION                                                  *
      * --------------------------------------------------------------*

           COPY TURBOCAL.

