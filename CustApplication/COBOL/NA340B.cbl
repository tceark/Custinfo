   CBL DATA(24)                                                         00000100
       IDENTIFICATION DIVISION.                                         00000500
       PROGRAM-ID. NA340B.                                              00000600
       DATE-WRITTEN. 03/22/88.                                          00004000
      *----------------------------------------------------------------*00004100
      *REMARKS. NA340B - UPDATED INFO TO DB2 TABLE                      00005000
      *------------------------PROGRAM PURPOSE-------------------------*00006000
      *                                                                *00007000
      *  PROGRAM TITLE: NA340B                                         *00008000
      *  PROGRAM TEXT:                                                 *00009000
      *--------------------COMPILATION OPTIONS-------------------------*00010000
      *  COBOL II DB2 CICS                                             *00020000
      *----------------------------------------------------------------*00021000
                                                                        00023000
       ENVIRONMENT DIVISION.                                            00023300
       DATA DIVISION.                                                   00023400
       WORKING-STORAGE SECTION.                                         00023500
R08986 01  WS-PROGRAM-NAME             PIC X(8) VALUE 'NA340B  '.       00023900
                                                                        00032600
       01  WS-WORK-AREA.                                                00037600
           05  WS-EMAIL-STATUS              PIC X(01) VALUE SPACE.      00037700
           05  WS-CUSTOMER-STATUS           PIC X     VALUE SPACES.     00042000
               88  INVALID-CUSTOMER-STATUS            VALUE 'D' 'C'.    00042100
           05  WS-SUBCRIPT COMP.                                        00044000
               10  ACTION-SUB               PIC S99.                    00044100
           05  WS-HOLD-MESSAGE              PIC X(5)   VALUE SPACE.     00044200
           05  WS-ERR-FIELD                 PIC X(40)  VALUE SPACES.    00044300
           05  WS-DATE-R                    PIC 9(6)   VALUE ZERO.      00045800
           05  WS-TRAN                      PIC X(6)   VALUE 'UPDATE'.  00045900
           05  WS-N                         PIC X(1)   VALUE 'N'.       00046100
           05  WS-DATE.                                                 00046200
               10  WS-MM                    PIC 99     VALUE ZERO.      00046300
               10  WS-DD                    PIC 99     VALUE ZERO.      00046400
               10  WS-YY                    PIC 99     VALUE ZERO.      00046500
           05  WS-DATE-A REDEFINES WS-DATE.                             00046600
               10  WS-MM-A                  PIC XX.                     00046700
               10  WS-DD-A                  PIC XX.                     00046800
               10  WS-YY-A                  PIC XX.                     00046900
           05  WS-BIRTH-DATE.                                           00047000
               10  BR-CC                    PIC XX.                     00047100
               10  BR-YY                    PIC XX.                     00047200
               10  FILLER                   PIC X      VALUE '-'.       00047300
               10  BR-MM                    PIC XX.                     00047400
               10  FILLER                   PIC X      VALUE '-'.       00047500
               10  BR-DD                    PIC XX.                     00047600
           05  WS-EFFECTIVE-DATE.                                       00047700
               10  EF-CC                    PIC XX.                     00047800
               10  EF-YY                    PIC XX.                     00047900
               10  FILLER                   PIC X      VALUE '-'.       00048000
               10  EF-MM                    PIC XX.                     00048100
               10  FILLER                   PIC X      VALUE '-'.       00048200
               10  EF-DD                    PIC XX.                     00048300
           05  WS-CHANGE-DATE.                                          00048400
               10  CO-CC                    PIC XX.                     00048500
               10  CO-YY                    PIC XX.                     00048600
               10  FILLER                   PIC X      VALUE '-'.       00048700
               10  CO-MM                    PIC XX.                     00048800
               10  FILLER                   PIC X      VALUE '-'.       00048900
               10  CO-DD                    PIC XX.                     00049000
           05  WS-DATE-EIGHT.                                           00049100
               10  WS-MONTH                 PIC 99     VALUE ZERO.      00049200
               10  SLASH-1                  PIC X.                      00049300
               10  WS-DAY                   PIC 99     VALUE ZERO.      00049400
               10  SLASH-2                  PIC X.                      00049500
               10  WS-YEAR                  PIC 99     VALUE ZERO.      00049600
           05  WS-COMPARE-DATE.                                         00049700
               10  WS-COMPARE-YY            PIC 99     VALUE ZERO.      00049800
               10  WS-COMPARE-MM            PIC 99     VALUE ZERO.      00049900
               10  WS-COMPARE-DD            PIC 99     VALUE ZERO.      00050000
           05  HOLD-NEXT-CIM                PIC 9(8) VALUE ZERO.        00050100
           05  HOLD-NEXT-CIM-R REDEFINES HOLD-NEXT-CIM                  00050200
                                            PIC X(8).                   00050300
                                                                        00050400
           05  WS-NUMERIC-CHECK.                                        00055100
               10 WS-NUMERIC-CHECK-BYTE OCCURS 20 TIMES                 00055200
                                            PIC X.                      00055300
           05  WS-NUMERIC-SW                PIC X.                      00055400
R04023     05  ORIGINAL-STATE               PIC XX.                     00055500
R02539     05  POSTAL-CODE                  PIC X(9).                   00055600
R02539     05  COUNTRY-CODE                 PIC X(3).                   00055700
R02539     05  IND-POSTAL-CODE              PIC S9(4) COMP.             00055800
R02539     05  IND-COUNTRY-CODE             PIC S9(4) COMP.             00055900
           05  WS-COMPANY-NAME.                                         00060400
               10  WS-COMPANY-NAME-HIGH-ORDR   PIC X(22) VALUE SPACES.  00060500
               10  WS-COMPANY-NAME-LOW-ORDR    PIC X(08) VALUE SPACES.  00060600
           05  WS-C                         PIC X(1) VALUE 'C'.         00061100
           05  WS-Y                         PIC X(1) VALUE 'Y'.         00061200
           05  WS-SPACE                     PIC X(1) VALUE SPACE.       00061300
           05  WS-SPACES                    PIC X(2) VALUE SPACES.      00061400
           05  WS-ZEROES-5                  PIC X(5) VALUE '00000'.     00061500
           05  WS-ZEROES-4                  PIC X(5) VALUE '0000'.      00061600
                                                                        00081200
R00700     COPY AGNTNV05.                                               00081600
900837     COPY CASNVW3.                                                00081700
           COPY IDTYHIST.                                               00081814
           COPY IDTYINFO.                                               00081914
R08986                                                                  00083000
           EXEC SQL                                                     00083100
              INCLUDE SQLCA                                             00083200
           END-EXEC.                                                    00083300
                                                                        00084400
           EXEC SQL DECLARE AGNTCUR CURSOR FOR                          00084500
11735A*        SELECT *                                                 00084600
11735A         SELECT                                                   00084700
                    IDNTITY,                                            00084800
                    LAST_NAME,                                          00084900
                    FIRST_NAME,                                         00085000
                    MIDDLE_NAME,                                        00085100
                    PREFIX,                                             00085200
                    SUFFIX1,                                            00085300
                    SUFFIX2,                                            00085400
                    COMPANY_IND,                                        00085500
                    COMPANY_IN_ADDRESS,                                 00085600
                    COMPANY_NAME,                                       00085700
                    DISPLAY_NAME,                                       00085800
                    NICKNAME,                                           00085900
                    ADDRESS1,                                           00086000
                    ADDRESS2,                                           00086100
                    CITY,                                               00086200
                    STATE,                                              00086300
289843              ZIP,                                                00086400
                    ZIP_PLUS4,                                          00086500
                    COUNTY_CODE,                                        00086600
                    AREA_CODE,                                          00086700
                    PHONE,                                              00086800
                    PHONE_EXTENSION,                                    00086900
                    SSN,                                                00087000
                    SEX,                                                00087100
                    BIRTH_DATE,                                         00087200
                    FINALST_REAS_CODE,                                  00087300
                    FINALST_OVRD_IND,                                   00087400
                    DUP_ADDR_OVRD_IND,                                  00087500
                    EFFECTIVE_DATE,                                     00087600
                    CHANGE_DATE,                                        00087700
                    CHANGE_LOGON,                                       00087800
                    ENTITY_TYPE,                                        00087900
                    RECORD_STATUS,                                      00088000
                    ALT_ADDRESS_IND,                                    00088100
                    FUTURE_ADDRESS_IND,                                 00088200
                    RECORD_ORIGIN,                                      00088300
                    COMBINED_STATUS,                                    00088400
                    SITE_CODE,                                          00088500
                    NAME_KEY1,                                          00088600
                    NAME_KEY2,                                          00088700
                    NAME_KEY3,                                          00088800
                    ADDRESS_KEY1,                                       00088900
                    ASSOCIATION1,                                       00089000
                    ASSOCIATION2,                                       00089100
                    ASSOCIATION3                                        00089200
900837*        FROM AGNTNV03                                            00089300
R00700         FROM AGNTNAME                                            00089400
               WHERE IDNTITY = :WS-UPD-CIM-NUMBER                       00089500
           END-EXEC.                                                    00089600
                                                                        00089700
           EXEC SQL DECLARE UPDT-AGNT-CUR CURSOR FOR                    00089800
11735A*        SELECT *                                                 00089900
11735A         SELECT                                                   00090000
                    IDNTITY,                                            00090100
                    LAST_NAME,                                          00090200
                    FIRST_NAME,                                         00090300
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
                   WHERE IDNTITY = :WS-UPD-CIM-NUMBER                   00097100
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
               WHERE IDNTITY = :WS-UPD-CIM-NUMBER                       00101100
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
                   WHERE IDNTITY = :WS-UPD-CIM-NUMBER                   00103200
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
       LINKAGE SECTION.                                                 00106200
                                                                        00106300
       01 DFHCOMMAREA.                                                  00106400
           COPY UPDCOMA.                                                00106500
                                                                        00106600
      ******************************************************************00108000
                                                                        00108100
       PROCEDURE DIVISION.                                              00108200
                                                                        00108300
       0010-BEGIN-PROGRAM.                                              00108400
           DISPLAY 'NA340B 0010-BEGIN-PROGRAM'.                         00108500
           DISPLAY 'EIBTRNID:' EIBTRNID.                                00108700
           DISPLAY 'COM-CIM:               '  COM-CIM                   00108800
           DISPLAY 'COM-LAST-NAME:         '  COM-LAST-NAME             00108900
           DISPLAY 'COM-FIRST-NAME:        '  COM-FIRST-NAME            00109000
           DISPLAY 'COM-MIDDLE-NAME:       '  COM-MIDDLE-NAME           00109100
           DISPLAY 'COM-PREFIX:            '  COM-PREFIX                00109200
           DISPLAY 'COM-SUFFIX1:           '  COM-SUFFIX1               00109300
           DISPLAY 'COM-SUFFIX2:           '  COM-SUFFIX2               00109400
           DISPLAY 'COM-COMPANY-IND:       '  COM-COMPANY-IND           00109500
           DISPLAY 'COM-COMPANY-IN-ADDRESS:'  COM-COMPANY-IN-ADDRESS    00109600
           DISPLAY 'COM-COMPANY-NAME:      '  COM-COMPANY-NAME          00109700
           DISPLAY 'COM-DISPLAY-NAME:      '  COM-DISPLAY-NAME          00109800
           DISPLAY 'COM-NICKNAME:          '  COM-NICKNAME              00109900
           DISPLAY 'COM-ADDRESS1:          '  COM-ADDRESS1              00110000
           DISPLAY 'COM-ADDRESS2:          '  COM-ADDRESS2              00110100
           DISPLAY 'COM-CITY:              '  COM-CITY                  00110200
           DISPLAY 'COM-STATE:             '  COM-STATE                 00110300
           DISPLAY 'COM-ZIP:               '  COM-ZIP                   00110400
           DISPLAY 'COM-ZIP-PLUS4:         '  COM-ZIP-PLUS4             00110500
           DISPLAY 'COM-COUNTY-CODE:       '  COM-COUNTY-CODE           00110600
           DISPLAY 'COM-AREA-CODE:         '  COM-AREA-CODE             00110700
           DISPLAY 'COM-PHONE:             '  COM-PHONE                 00110800
           DISPLAY 'COM-PHONE-EXTENSION:   '  COM-PHONE-EXTENSION       00110900
           DISPLAY 'COM-SSN:               '  COM-SSN                   00111000
           DISPLAY 'COM-SEX:               '  COM-SEX                   00111100
           DISPLAY 'COM-BIRTH-DATE:        '  COM-BIRTH-DATE            00111200
           DISPLAY 'COM-FINALST-REAS-CODE: '  COM-FINALST-REAS-CODE     00111300
           DISPLAY 'COM-FINALST-OVRD-IND:  '  COM-FINALST-OVRD-IND      00111400
           DISPLAY 'COM-DUP-ADDR-OVRD-IND: '  COM-DUP-ADDR-OVRD-IND     00111500
           DISPLAY 'COM-EFFECTIVE-DATE:    '  COM-EFFECTIVE-DATE        00111600
           DISPLAY 'COM-CHANGE-DATE:       '  COM-CHANGE-DATE           00111700
           DISPLAY 'COM-CHANGE-LOGON:      '  COM-CHANGE-LOGON          00111800
           DISPLAY 'COM-ENTITY-TYPE:       '  COM-ENTITY-TYPE           00111900
           DISPLAY 'COM-RECORD-STATUS:     '  COM-RECORD-STATUS         00112000
           DISPLAY 'COM-ALT-ADDRESS-IND:   '  COM-ALT-ADDRESS-IND       00112100
           DISPLAY 'COM-FUTURE-ADDRESS-IND:'  COM-FUTURE-ADDRESS-IND    00112200
           DISPLAY 'COM-RECORD-ORIGIN:     '  COM-RECORD-ORIGIN         00112300
           DISPLAY 'COM-COMBINED-STATUS:   '  COM-COMBINED-STATUS       00112400
           DISPLAY 'COM-SITE-CODE:         '  COM-SITE-CODE             00112500
           DISPLAY 'COM-NAME-KEY1:         '  COM-NAME-KEY1             00112600
           DISPLAY 'COM-NAME-KEY2:         '  COM-NAME-KEY2             00112700
           DISPLAY 'COM-NAME-KEY3:         '  COM-NAME-KEY3             00112800
           DISPLAY 'COM-ADDRESS-KEY1:      '  COM-ADDRESS-KEY1          00112900
           DISPLAY 'COM-ASSOCIATION1:      '  COM-ASSOCIATION1          00113000
           DISPLAY 'COM-ASSOCIATION2:      '  COM-ASSOCIATION2          00113100
           DISPLAY 'COM-ASSOCIATION3:      '  COM-ASSOCIATION3          00113200
           DISPLAY 'COM-ENTITY-LITERAL:    '  COM-ENTITY-LITERAL        00113300
           DISPLAY 'COM-CASE-SW:           '  COM-CASE-SW               00113400
           DISPLAY 'COM-FINALIST-SW:       '  COM-FINALIST-SW           00113500
           DISPLAY 'COM-FAX-AREA-CODE:     '  COM-FAX-AREA-CODE         00113600
           DISPLAY 'COM-FAX-PHONE:         '  COM-FAX-PHONE             00113700
           DISPLAY 'COM-EMAIL1:            '  COM-EMAIL1                00113800
           DISPLAY 'COM-POSTAL-CODE:       '  COM-POSTAL-CODE           00113900
           DISPLAY 'COM-COUNTRY-CODE:      '  COM-COUNTRY-CODE          00114000
           DISPLAY 'COM-DISP-IND           '  COM-DISP-IND              00114100
                                                                        00114200
           PERFORM 0100-EDIT-CUST-INFO THRU 0100-EXIT.                  00114300
                                                                        00114400
           PERFORM 0200-UPD-DB2-TABLE THRU 0200-EXIT.                   00114500
                                                                        00114600
           DISPLAY 'NA340B 0010-END-PROGRAM'.                           00114800
           EXEC CICS RETURN END-EXEC.                                   00114900
                                                                        00115000
       0010-EXIT.                                                       00115100
           EXIT.                                                        00115200
                                                                        00115300
       0100-EDIT-CUST-INFO.                                             00178600
           DISPLAY '0100-EDIT-CUST-INFO'.                               00178700
           MOVE 'N' TO WS-EDIT-SW.                                      00178800
      *---------------------------------------------------------------* 00179200
      * EDIT CUSTOMER TYPE FOR VALIDITY                               * 00179300
      *---------------------------------------------------------------* 00179400
           DISPLAY 'COM-ENTITY-LITERAL' COM-ENTITY-LITERAL              00179500
           INSPECT COM-ENTITY-LITERAL                                   00179600
               REPLACING ALL 'a' BY 'A'                                 00179700
                             'b' BY 'B'                                 00179800
                             'c' BY 'C'                                 00179900
                             'd' BY 'D'                                 00180000
                             'e' BY 'E'                                 00180100
                             'f' BY 'F'                                 00180200
                             'g' BY 'G'                                 00180300
                             'h' BY 'H'                                 00180400
                             'i' BY 'I'                                 00180500
                             'j' BY 'J'                                 00180600
                             'k' BY 'K'                                 00180700
                             'l' BY 'L'                                 00180800
                             'm' BY 'M'                                 00180900
                             'n' BY 'N'                                 00181000
                             'o' BY 'O'.                                00181100
                                                                        00181200
           INSPECT COM-ENTITY-LITERAL                                   00181300
              REPLACING  ALL 'p' BY 'P'                                 00181400
                             'q' BY 'Q'                                 00181500
                             'r' BY 'R'                                 00181600
                             's' BY 'S'                                 00181700
                             't' BY 'T'                                 00181800
                             'u' BY 'U'                                 00181900
                             'v' BY 'V'                                 00182000
                             'w' BY 'W'                                 00182100
                             'x' BY 'X'                                 00182200
                             'y' BY 'Y'                                 00182300
                             'z' BY 'Z'.                                00182400
      *---------------------------------------------------------------* 00187800
      * EDIT CUSTOMER TYPE                                            * 00187900
      *---------------------------------------------------------------* 00188000
                                                                        00188100
           MOVE COM-COMPANY-NAME TO WS-COMPANY-NAME.                    00188200
           IF  COM-ENTITY-TYPE  = 'CA'                                  00188300
               IF  WS-COMPANY-NAME-LOW-ORDR    >  ' '                   00188400
                   MOVE 'Y'      TO WS-EDIT-SW                          00188500
                   MOVE 'MAP-ENTITY-TYPE' TO WS-ERR-FIELD               00188600
                   MOVE 'NA185'  TO WS-HOLD-MESSAGE                     00188800
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00188900
                                                                        00189000
      *---------------------------------------------------------------* 00192200
      * EDIT CUSTOMER STATUS FOR VALIDITY                             * 00192300
      *---------------------------------------------------------------* 00192400
              IF COM-ENTITY-TYPE = 'BR' OR 'ML'                         00193600
                 MOVE COM-RECORD-STATUS  TO WS-CUSTOMER-STATUS          00193700
                 IF  INVALID-CUSTOMER-STATUS                            00193800
                     MOVE 'Y'      TO WS-EDIT-SW                        00193900
                     MOVE 'MAP-CUST-STATUS' TO WS-ERR-FIELD             00194000
                     MOVE 'NA005'  TO WS-HOLD-MESSAGE                   00194300
                     PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT   00194400
                 END-IF                                                 00194500
              END-IF                                                    00194600
                                                                        00194800
      *---------------------------------------------------------------* 00194900
      * EDIT CUSTOMER SSN STATUS FOR VALIDITY                         * 00195000
      *---------------------------------------------------------------* 00195100
           IF COM-SSN NOT = SPACES AND COM-SSN NOT = LOW-VALUES         00195200
               IF COM-SSN NOT NUMERIC                                   00195300
                   MOVE 'Y'      TO WS-EDIT-SW                          00195400
                   MOVE 'MAP-SSN' TO WS-ERR-FIELD                       00195500
                   MOVE 'NA031'  TO WS-HOLD-MESSAGE                     00195800
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00195900
                                                                        00196000
      *---------------------------------------------------------------* 00196100
      * EDIT RE-ASSEMBLE NAME CHANGE INDICATOR                        * 00196200
      *---------------------------------------------------------------* 00196300
                                                                        00196400
           IF COM-DISP-IND  NOT = 'Y' AND                               00196500
               COM-DISP-IND  NOT = 'N'                                  00196600
                   MOVE 'Y'      TO WS-EDIT-SW                          00196700
                   MOVE 'MAP-DISP-IND' TO WS-ERR-FIELD                  00196800
                   MOVE 'NA180'  TO WS-HOLD-MESSAGE                     00197100
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00197200
                                                                        00197300
      *---------------------------------------------------------------* 00197400
      * EDIT CUSTOMER PHONE NUMBER FOR VALIDITY                       * 00197500
      *---------------------------------------------------------------* 00197600
           IF COM-AREA-CODE NOT = SPACES AND                            00197700
               COM-AREA-CODE NOT = LOW-VALUES                           00197800
               IF COM-AREA-CODE NOT NUMERIC                             00197900
                   MOVE 'Y'      TO WS-EDIT-SW                          00198000
                   MOVE 'MAP-AREA-CODE' to WS-ERR-FIELD                 00198100
                   MOVE 'NA032'  TO WS-HOLD-MESSAGE                     00198400
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00198500
                                                                        00198600
           IF COM-PHONE NOT = SPACES AND                                00198700
               COM-PHONE NOT = LOW-VALUES                               00198800
               IF COM-PHONE NOT NUMERIC                                 00198900
                   MOVE 'Y'      TO WS-EDIT-SW                          00199000
                   MOVE 'MAP-PHONE3' TO WS-ERR-FIELD                    00199100
                   MOVE 'NA033'  TO WS-HOLD-MESSAGE                     00199500
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00199600
                                                                        00199700
           IF COM-PHONE-EXTENSION NOT = SPACES AND                      00199800
               COM-PHONE-EXTENSION NOT = LOW-VALUES                     00199900
               IF COM-PHONE-EXTENSION NOT NUMERIC                       00200000
                   MOVE 'Y'      TO WS-EDIT-SW                          00200100
                   MOVE 'MAP-PHONE-EXT' TO WS-ERR-FIELD                 00200200
                   MOVE 'NA034'  TO WS-HOLD-MESSAGE                     00200500
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00200600
                                                                        00200700
900837*---------------------------------------------------------------* 00200800
900837* EDIT FAX PHONE NUMBER FOR VALIDITY                            * 00200900
900837*---------------------------------------------------------------* 00201000
900837     IF COM-FAX-AREA-CODE NOT = SPACES AND                        00201100
900837         COM-FAX-AREA-CODE NOT = LOW-VALUES                       00201200
900837         IF COM-FAX-AREA-CODE NOT NUMERIC                         00201300
900837             MOVE 'Y'      TO WS-EDIT-SW                          00201400
                   MOVE 'MAP-FAX-AREA-CD' TO WS-ERR-FIELD               00201500
900837             MOVE 'NA186'  TO WS-HOLD-MESSAGE                     00201800
900837             PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00201900
900837                                                                  00202000
900837     IF COM-FAX-PHONE NOT = SPACES AND                            00202100
900837         COM-FAX-PHONE NOT = LOW-VALUES                           00202200
900837         IF COM-FAX-PHONE NOT NUMERIC                             00202300
900837             MOVE 'Y'      TO WS-EDIT-SW                          00202400
                   MOVE 'MAP-FAX-PHONE3' TO WS-ERR-FIELD                00202500
900837             MOVE 'NA187'  TO WS-HOLD-MESSAGE                     00202900
900837             PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00203000
                                                                        00203100
      *---------------------------------------------------------------* 00203200
      * EDIT COMPANY IN ADDRESS FIELD FOR VALIDITY                    * 00203300
      *---------------------------------------------------------------* 00203400
           IF  COM-COMPANY-IN-ADDRESS NOT = 'Y' AND                     00203500
               COM-COMPANY-IN-ADDRESS NOT = 'N'                         00203600
               MOVE 'Y'      TO WS-EDIT-SW                              00203700
               MOVE 'MAP-COMP-IN-ADD' TO WS-ERR-FIELD                   00203800
               MOVE 'NA035'  TO WS-HOLD-MESSAGE                         00204100
               PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.        00204200
                                                                        00204300
           DISPLAY 'VALN COM-FINALST-OVRD-IND:' COM-FINALST-OVRD-IND    00204400
      *---------------------------------------------------------------* 00204500
      * EDIT ADDRESS OVERRIDE INDICATOR FOR VALIDITY                  * 00204600
      *---------------------------------------------------------------* 00204700
           IF COM-FINALST-OVRD-IND NOT = 'N' AND                        00204800
              COM-FINALST-OVRD-IND NOT = 'Y'                            00204900
                   MOVE 'Y'      TO WS-EDIT-SW                          00205000
                   MOVE 'MAP-ADDR-OVRIDE' TO WS-ERR-FIELD               00205100
                   MOVE 'NA036'  TO WS-HOLD-MESSAGE                     00205400
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00205500
                                                                        00205600
      *---------------------------------------------------------------* 00205700
      * CALL NA205 INTERFACE TO FINALIST TO VERIFY/CORRECT ADDRESS.   * 00205800
      * IF ADDRESS CORRECTION HAS BEEN OVERRIDEN THEN SKIP.           * 00205900
      *---------------------------------------------------------------* 00206000
107987     IF COM-ZIP NOT =  SPACES                                     00206100
107987        AND COM-ZIP NOT NUMERIC                                   00206200
              MOVE 'Y'      TO WS-EDIT-SW                               00206300
              MOVE 'MAP-ZIP' TO WS-ERR-FIELD                            00206400
107987        MOVE 'NA194'  TO WS-HOLD-MESSAGE                          00206500
107987        MOVE 'N' TO COM-FINALIST-SW                               00206600
107987        PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT          00206700
107987     END-IF                                                       00206800
107987     IF COM-ZIP EQUAL SPACES OR ZEROS                             00206900
107987        AND COM-FINALST-OVRD-IND = 'Y'                            00207000
              MOVE 'Y'      TO WS-EDIT-SW                               00207100
              MOVE 'MAP-ZIP' TO WS-ERR-FIELD                            00207200
107987        MOVE 'NA194'  TO WS-HOLD-MESSAGE                          00207300
107987        MOVE 'N' TO COM-FINALIST-SW                               00207400
107987        PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT          00207500
107987     END-IF                                                       00207600
                                                                        00209000
      *---------------------------------------------------------------* 00209500
      * EDIT ADDRESS FIELDS FOR SPACES                                * 00209600
      *---------------------------------------------------------------* 00209700
           IF COM-ADDRESS1 = SPACES AND COM-ADDRESS2 = SPACES           00209800
                   MOVE 'Y'      TO WS-EDIT-SW                          00209900
                   MOVE 'MAP-ADDRESS1' TO WS-ERR-FIELD                  00210000
                   MOVE 'NA182'  TO WS-HOLD-MESSAGE                     00210300
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00210400
                                                                        00210500
      *---------------------------------------------------------------* 00210600
      * EDIT CITY FIELD FOR SPACES                                    * 00210700
      *---------------------------------------------------------------* 00210800
           IF COM-CITY = SPACES                                         00210900
                   MOVE 'Y'      TO WS-EDIT-SW                          00211000
                   MOVE 'MAP-CITY' TO WS-ERR-FIELD                      00211100
                   MOVE 'NA183'  TO WS-HOLD-MESSAGE                     00211400
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00211500
                                                                        00211600
      *---------------------------------------------------------------* 00211700
      * EDIT STATE FIELD FOR SPACES                                   * 00211800
      *---------------------------------------------------------------* 00211900
           IF COM-STATE = SPACES                                        00212000
                   MOVE 'Y'      TO WS-EDIT-SW                          00212100
                   MOVE 'MAP-STATE' TO WS-ERR-FIELD                     00212200
                   MOVE 'NA184'  TO WS-HOLD-MESSAGE                     00212500
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00212600
                                                                        00214400
      *---------------------------------------------------------------* 00220400
      * EDIT FIRST MIDDLE LAST AND COMPANY FOR ENTRY                  * 00220500
      *---------------------------------------------------------------* 00220600
                                                                        00220700
           IF COM-LAST-NAME = SPACES AND COM-FIRST-NAME = SPACES AND    00220800
               COM-MIDDLE-NAME = SPACES AND COM-COMPANY-NAME = SPACES   00220900
                   MOVE 'Y'      TO WS-EDIT-SW                          00221000
                   MOVE 'MAP-FIRST-NAME' TO WS-ERR-FIELD                00221100
                   MOVE 'NA027'  TO WS-HOLD-MESSAGE                     00221400
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00221500
                                                                        00221600
      *---------------------------------------------------------------* 00221700
      * EDIT LAST NAME SPACES WHEN FIRST AND MIDDLE ENTERED           * 00221800
      *---------------------------------------------------------------* 00221900
                                                                        00222000
           IF COM-FIRST-NAME NOT = SPACES AND                           00222100
               COM-MIDDLE-NAME NOT = SPACES AND COM-LAST-NAME = SPACES  00222200
               OR COM-FIRST-NAME NOT = SPACES AND COM-LAST-NAME = SPACES00222300
               OR COM-MIDDLE-NAME NOT = SPACES AND COM-LAST-NAME = SPACE00222400
                   MOVE 'Y'      TO WS-EDIT-SW                          00222500
                   MOVE 'MAP-LAST-NAME' TO WS-ERR-FIELD                 00222600
                   MOVE 'NA161'  TO WS-HOLD-MESSAGE                     00222900
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00223000
                                                                        00223100
      *---------------------------------------------------------------* 00223200
      * EDIT FIRST NAME WHEN LAST NAME ENTERED                        * 00223300
      *---------------------------------------------------------------* 00223400
                                                                        00223500
           IF COM-LAST-NAME NOT = SPACES AND COM-FIRST-NAME = SPACE     00223600
                   MOVE 'Y'      TO WS-EDIT-SW                          00223700
                   MOVE 'MAP-FIRST-NAME' TO WS-ERR-FIELD                00223800
                   MOVE 'NA161'  TO WS-HOLD-MESSAGE                     00224100
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00224200
                                                                        00224400
      *---------------------------------------------------------------* 00224500
      * DECIDE IF RECORD BELONGS TO A COMPANY OR AGENT                * 00224600
      *---------------------------------------------------------------* 00224700
           IF (COM-LAST-NAME = SPACES AND COM-FIRST-NAME = SPACES AND   00224800
               COM-MIDDLE-NAME = SPACES) OR                             00224900
               COM-COMPANY-NAME NOT = SPACES                            00225000
                   MOVE 'Y' TO COM-COMPANY-IND                          00225200
                   DISPLAY 'COM-COMPANY-IND' COM-COMPANY-IND            00225300
                   UNSTRING COM-COMPANY-NAME                            00225400
                       DELIMITED BY ALL ' ' OR ','                      00225500
                           INTO COM-NAME-KEY1                           00225600
                                COM-NAME-KEY2                           00225700
                                COM-NAME-KEY3                           00225800
           ELSE                                                         00225900
              MOVE 'N' TO COM-COMPANY-IND                               00226000
              MOVE COM-LAST-NAME   TO COM-NAME-KEY1                     00226100
              MOVE COM-FIRST-NAME  TO COM-NAME-KEY2                     00226200
              MOVE COM-MIDDLE-NAME TO COM-NAME-KEY3                     00226300
R04023*       IF (COM-CARRIER-CODE = 'SH')                              00226400
R04254*       IF (COM-CARRIER-CODE = 'SH' OR 'SV')                      00226500
R04281*       IF (COM-CARRIER-CODE = 'SH' OR 'SV' OR 'MN')              00226600
R04280        IF (COM-CARRIER-CODE = 'SH' OR 'SV' OR 'MN' OR 'SP'       00226700
R04472                                    OR 'IG'                       00226800
R04209             OR 'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'              00226900
R04934             OR 'NO' OR 'NU' OR 'NX'                              00227000
R04209             OR 'LD' OR 'LI' OR 'MI'                              00227100
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'      00227200
R04209             OR 'NQ' OR 'NS'                                      00227300
R04209             OR 'UD' OR 'UT'                                      00227400
R04209             OR 'VI' OR 'WF'                                      00227500
R04832** NEW CIGNA CARRIERS                                             00227600
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'      00227700
R04832             OR 'JH'                                              00227800
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'      00227900
R04832             )                                                    00228000
R04023          OR                                                      00228100
R04023           (COM-CARRIER-CODE = '9D'                               00228200
R04023          AND                                                     00228300
R04023*           COM-PREV-CARRIER = 'SH')                              00228400
R04254*           COM-PREV-CARRIER = 'SH' OR 'SV')                      00228500
R04281*           COM-PREV-CARRIER = 'SH' OR 'SV' OR 'MN')              00228600
R04280            COM-PREV-CARRIER = 'SH' OR 'SV' OR 'MN' OR 'SP'       00228700
R04472*                                   OR 'IG'                       00228800
R04342                                    OR 'IG' OR 'MV' OR 'MB'       00228900
R04209                                    OR 'JJ'                       00229000
R04832                                    OR 'JA')                      00229100
R04023            MOVE COM-FIRST-NAME   TO COM-NAME-KEY1                00229200
R04023            MOVE COM-LAST-NAME  TO COM-NAME-KEY2.                 00229300
      *---------------------------------------------------------------* 00229400
      * EDIT CUSTOMER SEX STATUS FOR VALIDITY                         * 00229500
      *---------------------------------------------------------------* 00229600
      *  AIG DOES NOT UPDATE GENDER/SEX ON CASENAME TABLE             * 00229700
      *  SEX FIELD IS NOT SHOWN ON SCREEN FOR AIG                     * 00229800
      *---------------------------------------------------------------* 00229900
R04023*    IF (COM-CARRIER-CODE = 'SH')                                 00230000
R04254*    IF (COM-CARRIER-CODE = 'SH' OR 'SV')                         00230100
R04281*    IF (COM-CARRIER-CODE = 'SH' OR 'SV' OR 'MN')                 00230200
R04280*    IF (COM-CARRIER-CODE = 'SH' OR 'SV' OR 'MN' OR 'SP')         00230300
R04472     IF (COM-CARRIER-CODE = 'SH' OR 'SV' OR 'MN' OR 'SP' OR 'IG'  00230400
R04342             OR 'MV' OR 'MB'                                      00230500
R04209             OR 'AD' OR 'CR' OR 'DC' OR 'DI' OR 'DN'              00230600
R04934             OR 'NO' OR 'NU' OR 'NX'                              00230700
R04209             OR 'LD' OR 'LI' OR 'MI'                              00230800
R04209             OR 'NA' OR 'NC' OR 'NG' OR 'NH' OR 'NJ' OR 'NP'      00230900
R04209             OR 'NQ' OR 'NS'                                      00231000
R04209             OR 'UD' OR 'UT'                                      00231100
R04209             OR 'VI' OR 'WF'                                      00231200
R04832** NEW CIGNA CARRIERS                                             00231300
R04832             OR 'JB' OR 'JC' OR 'JD' OR 'JE' OR 'JF' OR 'JG'      00231400
R04832             OR 'JH'                                              00231500
R04832             OR 'JL' OR 'JM' OR 'JN' OR 'JO' OR 'JP' OR 'JQ'      00231600
R04832             )                                                    00231700
R04023         OR                                                       00231800
R04023        (COM-CARRIER-CODE = '9D'                                  00231900
R04023         AND                                                      00232000
R04023*        COM-PREV-CARRIER = 'SH')                                 00232100
R04254*        COM-PREV-CARRIER = ('SH' OR 'SV'))                       00232200
R04281*        COM-PREV-CARRIER = ('SH' OR 'SV' OR 'MN'))               00232300
R04280         COM-PREV-CARRIER = ('SH' OR 'SV' OR 'MN' OR 'SP'         00232400
R04209                                  OR 'JJ'                         00232500
R04832                                  OR 'JA'                         00232600
R04472                                  OR 'IG'))                       00232700
R04023         NEXT SENTENCE                                            00232800
R04023     ELSE                                                         00232900
           IF COM-COMPANY-IND NOT = 'Y'                                 00233000
               IF COM-SEX NOT = 'M' AND COM-SEX NOT = 'F'               00233100
                   MOVE 'Y'      TO WS-EDIT-SW                          00233200
                   MOVE 'MAP-SEX' TO WS-ERR-FIELD                       00233300
                   MOVE 'NA154'  TO WS-HOLD-MESSAGE                     00233600
                   PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT.    00233700
                                                                        00233800
           IF COM-COMPANY-IND = 'Y'                                     00233900
                MOVE SPACE TO COM-SEX.                                  00234000
                                                                        00234100
      *---------------------------------------------------------------* 00234200
      * RECORD ORIGIN IS AN ONLINE ADD = 'O'                          * 00234300
      *---------------------------------------------------------------* 00234400
                                                                        00234500
           MOVE 'O' TO COM-RECORD-ORIGIN.                               00234600
                                                                        00234700
      *---------------------------------------------------------------* 00234800
      * EDIT LAST-NAME FOR NUMERICS                                   * 00234900
      *---------------------------------------------------------------* 00235000
                                                                        00235100
           MOVE COM-LAST-NAME TO WS-NUMERIC-CHECK.                      00235200
           PERFORM 0160-NUMERIC-CHECK THRU 0160-EXIT                    00235300
               VARYING ACTION-SUB FROM 1 BY 1                           00235400
                   UNTIL ACTION-SUB > 20.                               00235500
                                                                        00235600
           IF WS-NUMERIC-SW = 'Y'                                       00235700
                   MOVE 'Y'      TO WS-EDIT-SW                          00235800
                   MOVE 'MAP-LAST-NAME' TO WS-ERR-FIELD                 00235900
                   MOVE 'NA022'  TO WS-HOLD-MESSAGE                     00236200
                   PERFORM 0170-SLIDE-ERROR-MESSAGES  THRU 0170-EXIT.   00236300
                                                                        00236400
      *---------------------------------------------------------------* 00236500
      * EDIT FIRST NAME FOR NUMERICS                                  * 00236600
      *---------------------------------------------------------------* 00236700
                                                                        00236800
           MOVE 'N'            TO WS-NUMERIC-SW.                        00236900
           MOVE COM-FIRST-NAME TO WS-NUMERIC-CHECK.                     00237000
           PERFORM 0160-NUMERIC-CHECK  THRU 0160-EXIT                   00237100
               VARYING ACTION-SUB FROM 1 BY 1                           00237200
                   UNTIL ACTION-SUB > 15.                               00237300
                                                                        00237400
           IF WS-NUMERIC-SW = 'Y'                                       00237500
                   MOVE 'Y'      TO WS-EDIT-SW                          00237600
                   MOVE 'MAP-FIRST-NAME' TO WS-ERR-FIELD                00237700
                   MOVE 'NA040'  TO WS-HOLD-MESSAGE                     00238000
                   PERFORM 0170-SLIDE-ERROR-MESSAGES  THRU 0170-EXIT.   00238100
                                                                        00238200
      *---------------------------------------------------------------* 00238300
      * EDIT MIDDLE NAME FOR NUMERICS                                 * 00238400
      *---------------------------------------------------------------* 00238500
                                                                        00238600
           MOVE 'N'             TO WS-NUMERIC-SW.                       00238700
           MOVE COM-MIDDLE-NAME TO WS-NUMERIC-CHECK.                    00238800
           PERFORM 0160-NUMERIC-CHECK  THRU 0160-EXIT                   00238900
               VARYING ACTION-SUB FROM 1 BY 1                           00239000
                   UNTIL ACTION-SUB > 10.                               00239100
                                                                        00239200
           IF WS-NUMERIC-SW = 'Y'                                       00239300
                   MOVE 'Y'      TO WS-EDIT-SW                          00239400
                   MOVE 'MAP-MIDDLE-NAME' TO WS-ERR-FIELD               00239500
                   MOVE 'NA041'  TO WS-HOLD-MESSAGE                     00239800
                   PERFORM 0170-SLIDE-ERROR-MESSAGES  THRU 0170-EXIT.   00239900
                                                                        00240000
R02539*---------------------------------------------------------------* 00240100
R02539* EDIT COUNTRY CODE FOR VALIDITY                                * 00240200
R02539*---------------------------------------------------------------* 00240300
R02539     IF COM-ZIP = '00100'                                         00240400
              DISPLAY 'COM-COUNTRY-CODE:' COM-COUNTRY-CODE              00240500
R02539        EVALUATE TRUE                                             00240600
R02539           WHEN COM-COUNTRY-CODE = SPACES                         00240700
R02539             MOVE 'Y'      TO WS-EDIT-SW                          00240800
                   MOVE 'MAP-COUNTRY-CD' TO WS-ERR-FIELD                00240900
R02539             MOVE 'CM427'  TO WS-HOLD-MESSAGE                     00241100
R02539             PERFORM 0170-SLIDE-ERROR-MESSAGES  THRU 0170-EXIT    00241200
R02539           WHEN COM-COUNTRY-CODE = 'US'                           00241300
R02539             MOVE 'Y'      TO WS-EDIT-SW                          00241400
                   MOVE 'MAP-COUNTRY-CD' TO WS-ERR-FIELD                00241500
R02539             MOVE 'CM428'  TO WS-HOLD-MESSAGE                     00241800
R02539             PERFORM 0170-SLIDE-ERROR-MESSAGES  THRU 0170-EXIT    00241900
R02539        END-EVALUATE                                              00243100
R02539     ELSE                                                         00243200
R02539        IF COM-ZIP NOT EQUAL '00100'                              00243300
R02539           EVALUATE TRUE                                          00243400
R02539              WHEN COM-COUNTRY-CODE = SPACES                      00243500
R02539                 CONTINUE                                         00243600
R02539              WHEN COM-COUNTRY-CODE NOT EQUAL SPACES              00243700
R02539                 MOVE 'Y'      TO WS-EDIT-SW                      00243800
                       MOVE 'MAP-COUNTRY-CD' TO WS-ERR-FIELD            00243900
R02539                 MOVE 'CM428'  TO WS-HOLD-MESSAGE                 00244200
R02539                 PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT 00244300
R02539              END-EVALUATE                                        00244400
R02539        END-IF                                                    00244500
R02539     END-IF.                                                      00244600
R02539                                                                  00244700
R02539*---------------------------------------------------------------* 00244800
R02539* EDIT POSTAL CODE FOR VALIDITY                                 * 00244900
R02539*---------------------------------------------------------------* 00245000
R02539     IF COM-ZIP = '00100'                                         00245100
R02539       IF COM-COUNTRY-CODE = 'CA'                                 00245200
R02539         EVALUATE TRUE ALSO TRUE                                  00245300
R02539           WHEN COM-STATE = 'AB' ALSO COM-POSTAL-CODE (1:1) = 'T' 00245400
R02539             CONTINUE                                             00245500
R02539           WHEN COM-STATE = 'BC' ALSO COM-POSTAL-CODE (1:1) = 'V' 00245600
R02539             CONTINUE                                             00245700
R02539           WHEN COM-STATE = 'LB' ALSO COM-POSTAL-CODE (1:1) = 'A' 00245800
R02539             CONTINUE                                             00245900
R02539           WHEN COM-STATE = 'MB' ALSO COM-POSTAL-CODE (1:1) = 'R' 00246000
R02539             CONTINUE                                             00246100
R02539           WHEN COM-STATE = 'NB' ALSO COM-POSTAL-CODE (1:1) = 'E' 00246200
R02539             CONTINUE                                             00246300
R02539           WHEN COM-STATE = 'NF' ALSO COM-POSTAL-CODE (1:1) = 'A' 00246400
R02539             CONTINUE                                             00246500
R02539           WHEN COM-STATE = 'NA' ALSO COM-POSTAL-CODE (1:1) = 'X' 00246600
R02539             CONTINUE                                             00246700
R02539           WHEN COM-STATE = 'NS' ALSO COM-POSTAL-CODE (1:1) = 'B' 00246800
R02539             CONTINUE                                             00246900
R02539           WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'L' 00247000
R02539           WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'N' 00247100
R02539           WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'K' 00247200
R02539           WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'M' 00247300
R02539           WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'P' 00247400
R02539             CONTINUE                                             00247500
R02539           WHEN COM-STATE = 'PE' ALSO COM-POSTAL-CODE (1:1) = 'C' 00247600
R02539             CONTINUE                                             00247700
R02539           WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'H' 00247800
R02539           WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'J' 00247900
R02539           WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'G' 00248000
R02539             CONTINUE                                             00248100
R02539           WHEN COM-STATE = 'SK' ALSO COM-POSTAL-CODE (1:1) = 'S' 00248200
R02539             CONTINUE                                             00248300
R02539           WHEN COM-STATE = 'YT' ALSO COM-POSTAL-CODE (1:1) = 'Y' 00248400
R02539             CONTINUE                                             00248500
R02539           WHEN OTHER                                             00248600
R02539               MOVE 'Y'      TO WS-EDIT-SW                        00248700
                     MOVE 'MAP-POSTAL-CD' TO WS-ERR-FIELD               00248800
R02539               MOVE 'CM120'  TO WS-HOLD-MESSAGE                   00249100
R02539               PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT   00249200
R02539           END-EVALUATE                                           00249300
R02539       END-IF                                                     00249400
R02539     ELSE                                                         00249500
R02539       IF COM-POSTAL-CODE NOT EQUAL SPACES                        00249600
R02539          MOVE 'Y'      TO WS-EDIT-SW                             00249700
                MOVE 'MAP-POSTAL-CD' TO WS-ERR-FIELD                    00249800
R02539          MOVE 'CM120'  TO WS-HOLD-MESSAGE                        00250100
R02539          PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT        00250200
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
                   MOVE 'MAP-STATE'  TO WS-ERR-FIELD                    00251500
R02539             MOVE 'CM016'  TO WS-HOLD-MESSAGE                     00251800
R02539             PERFORM 0170-SLIDE-ERROR-MESSAGES THRU 0170-EXIT     00251900
R02539        END-EVALUATE                                              00252000
R02539     END-IF.                                                      00252300
                                                                        00259200
       0100-EXIT.                                                       00278500
           EXIT.                                                        00278600
                                                                        00290300
       0160-NUMERIC-CHECK.                                              00312100
                                                                        00312200
           IF WS-NUMERIC-CHECK-BYTE (ACTION-SUB) NUMERIC                00312300
               MOVE 'Y' TO WS-NUMERIC-SW.                               00312400
                                                                        00312500
       0160-EXIT.                                                       00312600
           EXIT.                                                        00312700
                                                                        00312900
       0170-SLIDE-ERROR-MESSAGES.                                       00313000
                                                                        00313100
           IF WS-MESSAGE-NUMBER1 = SPACES OR LOW-VALUES                 00313200
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER1               00313300
               MOVE WS-ERR-FIELD    TO WS-ERR-FIELD1                    00313400
           ELSE                                                         00313500
           IF WS-MESSAGE-NUMBER2 = SPACES OR LOW-VALUES                 00313600
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER2               00313700
               MOVE WS-ERR-FIELD    TO WS-ERR-FIELD2                    00313800
           ELSE                                                         00313900
           IF WS-MESSAGE-NUMBER3 = SPACES OR LOW-VALUES                 00314000
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER3               00314100
               MOVE WS-ERR-FIELD    TO WS-ERR-FIELD3                    00314200
           ELSE                                                         00314300
           IF WS-MESSAGE-NUMBER4 = SPACES OR LOW-VALUES                 00314400
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER4               00314500
               MOVE WS-ERR-FIELD    TO WS-ERR-FIELD4.                   00314600
                                                                        00315000
       0170-EXIT.                                                       00315100
           EXIT.                                                        00315200
                                                                        00315300
       0200-UPD-DB2-TABLE.                                              00316000
           DISPLAY 'WS-EDIT-SW:' WS-EDIT-SW                             00317000
           DISPLAY 'COM-ENTITY-TYPE:' COM-ENTITY-TYPE                   00318000
                                                                        00319000
           MOVE 'N'    TO WS-UPD-SQL-ERROR.                             00320000
                                                                        00330000
           IF WS-EDIT-SW NOT = 'Y'                                      00330100
              DISPLAY 'DB2 UPD START'                                   00330200
920124        IF (COM-ENTITY-TYPE = 'AM' OR 'CA' OR 'LB' OR 'BA'        00330300
MANJU                            OR 'LD' OR 'CAM')                      00330400
                  PERFORM 0220-UPDATE-CASENAME-ROW THRU 0220-EXIT       00330500
              ELSE                                                      00330600
                  PERFORM 0210-UPDATE-AGNTNAME-ROW THRU 0210-EXIT       00330700
              END-IF                                                    00330800
           END-IF.                                                      00330900
                                                                        00331000
       0200-EXIT.                                                       00334500
           EXIT.                                                        00334600
                                                                        00334700
       0210-UPDATE-AGNTNAME-ROW.                                        00334800
           DISPLAY '0210-UPDATE-AGNTNAME-ROW PARA'                      00334900
           DISPLAY 'WS-UPD-CIM-NUMBER:' WS-UPD-CIM-NUMBER               00335000
                                                                        00335100
           EXEC SQL                                                     00335200
               OPEN AGNTCUR                                             00335300
           END-EXEC.                                                    00335400
           IF SQLCODE NOT = ZERO                                        00335500
               MOVE 'Y'           TO WS-UPD-SQL-ERROR                   00335600
               MOVE 'AGNT CURSOR OPEN ERROR' TO WS-UPD-SQL-ERROR-MSG    00335700
               MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1                 00335800
               MOVE SPACES        TO WS-ERR-FIELD1                      00335900
               MOVE SQLCODE       TO WS-UPD-SQLCODE                     00336000
               GO TO 0210-EXIT.                                         00336100
                                                                        00336200
           EXEC SQL FETCH AGNTCUR                                       00336300
R01142         INTO :DCLAGNTNAME:AGNTNAME-INDICATORS                    00336400
           END-EXEC.                                                    00336500
                                                                        00336600
           IF SQLCODE NOT = ZERO                                        00336700
               MOVE 'Y'           TO WS-UPD-SQL-ERROR                   00336800
               MOVE 'AGNT CURSOR FETCH ERROR' TO WS-UPD-SQL-ERROR-MSG   00336900
               MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1                 00337000
               MOVE SPACES        TO WS-ERR-FIELD1                      00337100
               MOVE SQLCODE       TO WS-UPD-SQLCODE                     00337200
               GO TO 0210-EXIT.                                         00337300
                                                                        00337500
           IF IND-LAST-NAME IS NEGATIVE                                 00337600
               MOVE SPACES TO LAST-NAME.                                00337700
           IF IND-FIRST-NAME IS NEGATIVE                                00337800
               MOVE SPACES TO FIRST-NAME.                               00337900
           IF IND-MIDDLE-NAME IS NEGATIVE                               00338000
               MOVE SPACES TO MIDDLE-NAME.                              00338100
           IF IND-PREFIX IS NEGATIVE                                    00338200
               MOVE SPACES TO PREFIX.                                   00338300
           IF IND-SUFFIX1 IS NEGATIVE                                   00338400
               MOVE SPACES TO SUFFIX1.                                  00338500
           IF IND-SUFFIX2 IS NEGATIVE                                   00338600
               MOVE SPACES TO SUFFIX2.                                  00338700
           IF IND-COMPANY-NAME IS NEGATIVE                              00338800
               MOVE SPACES TO COMPANY-NAME.                             00338900
           IF IND-BIRTH-DATE IS NEGATIVE                                00339000
               MOVE ZEROES TO BIRTH-DATE.                               00339100
           IF IND-DISPLAY-NAME IS NEGATIVE                              00339200
               MOVE SPACES TO DISPLAY-NAME.                             00339300
           IF IND-NICKNAME IS NEGATIVE                                  00339400
               MOVE SPACES TO NICKNAME.                                 00339500
           IF IND-ADDRESS2 IS NEGATIVE                                  00339600
               MOVE SPACES TO ADDRESS2.                                 00339700
           IF IND-ASSOCIATION1 IS NEGATIVE                              00339800
               MOVE SPACES TO ASSOCIATION1.                             00339900
           IF IND-ASSOCIATION2 IS NEGATIVE                              00340000
               MOVE SPACES TO ASSOCIATION2.                             00340100
           IF IND-ASSOCIATION3 IS NEGATIVE                              00340200
               MOVE SPACES TO ASSOCIATION3.                             00340300
                                                                        00340400
           EXEC SQL                                                     00340500
               CLOSE AGNTCUR                                            00340600
           END-EXEC.                                                    00340700
                                                                        00340800
           IF SQLCODE NOT = ZERO                                        00340900
               MOVE 'Y'           TO WS-UPD-SQL-ERROR                   00341000
               MOVE 'AGNT CURSOR CLOSE ERROR' TO WS-UPD-SQL-ERROR-MSG   00341100
               MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1                 00341200
               MOVE SPACES        TO WS-ERR-FIELD1                      00341300
               MOVE SQLCODE       TO WS-UPD-SQLCODE                     00341400
               GO TO 0210-EXIT.                                         00341500
                                                                        00341700
      *---------------------------------------------------------------* 00341800
      *    THIS CODE IS NEEDED TO UPDATE THE AUDIT RECORD WITH        * 00341900
      *    THE IMAGE OF THE RECORD BEFORE IT WAS CHANGED. DB2         * 00342000
      *    DOES NOT MAKE THE RECORD AVAILABLE IN THE REDEFINES        * 00342100
      *    AREA WHEN IT IS FETCED FOR UPDATE.                         * 00342200
      *---------------------------------------------------------------* 00342300
                                                                        00342600
           EXEC SQL                                                     00342700
               OPEN UPDT-AGNT-CUR                                       00342800
           END-EXEC.                                                    00342900
                                                                        00343000
           IF SQLCODE NOT = ZERO                                        00343100
               MOVE 'Y'           TO WS-UPD-SQL-ERROR                   00343200
               MOVE 'UPD AGNT CURSOR OPEN ERR' TO WS-UPD-SQL-ERROR-MSG  00343300
               MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1                 00343400
               MOVE SPACES        TO WS-ERR-FIELD1                      00343500
               MOVE SQLCODE       TO WS-UPD-SQLCODE                     00343600
               GO TO 0210-EXIT.                                         00343700
                                                                        00343900
           EXEC SQL FETCH UPDT-AGNT-CUR                                 00344000
R01142         INTO :DCLAGNTNAME:AGNTNAME-INDICATORS                    00344100
           END-EXEC.                                                    00344200
           DISPLAY 'SQLCODE OF FETCH UPDT CUR' SQLCODE                  00344300
           IF SQLCODE = 100                                             00344400
               MOVE 'Y'           TO WS-UPD-SQL-ERROR                   00344500
               MOVE 'AGNT REC NOT FOUND' TO WS-UPD-SQL-ERROR-MSG        00344600
               MOVE SPACES        TO WS-ERR-FIELD1                      00344800
               MOVE SQLCODE       TO WS-UPD-SQLCODE                     00344900
               MOVE 'NA020'  TO WS-MESSAGE-NUMBER1                      00345000
               GO TO 0210-EXIT                                          00345100
           ELSE                                                         00345200
                 IF SQLCODE NOT = ZERO                                  00345700
                    MOVE 'Y'           TO WS-UPD-SQL-ERROR              00345800
                    MOVE 'UPD AGNTCUR FETCH ERROR' TO                   00345900
                                          WS-UPD-SQL-ERROR-MSG          00346000
                    MOVE SPACES        TO WS-ERR-FIELD1                 00346100
                    MOVE SQLCODE       TO WS-UPD-SQLCODE                00346200
                    MOVE 'DBERR'  TO WS-MESSAGE-NUMBER1                 00346300
                    GO TO 0210-EXIT.                                    00346400
                                                                        00346700
           IF IND-LAST-NAME IS NEGATIVE                                 00346800
               MOVE SPACES TO LAST-NAME.                                00346900
           IF IND-FIRST-NAME IS NEGATIVE                                00347000
               MOVE SPACES TO FIRST-NAME.                               00347100
           IF IND-MIDDLE-NAME IS NEGATIVE                               00347200
               MOVE SPACES TO MIDDLE-NAME.                              00347300
           IF IND-PREFIX IS NEGATIVE                                    00347400
               MOVE SPACES TO PREFIX.                                   00347500
           IF IND-SUFFIX1 IS NEGATIVE                                   00347600
               MOVE SPACES TO SUFFIX1.                                  00347700
           IF IND-SUFFIX2 IS NEGATIVE                                   00347800
               MOVE SPACES TO SUFFIX2.                                  00347900
           IF IND-COMPANY-NAME IS NEGATIVE                              00348000
               MOVE SPACES TO COMPANY-NAME.                             00348100
           IF IND-BIRTH-DATE IS NEGATIVE                                00348200
               MOVE ZEROES TO BIRTH-DATE.                               00348300
           IF IND-DISPLAY-NAME IS NEGATIVE                              00348400
               MOVE SPACES TO DISPLAY-NAME.                             00348500
           IF IND-NICKNAME IS NEGATIVE                                  00348600
               MOVE SPACES TO NICKNAME.                                 00348700
           IF IND-ADDRESS2 IS NEGATIVE                                  00348800
               MOVE SPACES TO ADDRESS2.                                 00348900
           IF IND-ASSOCIATION1 IS NEGATIVE                              00349000
               MOVE SPACES TO ASSOCIATION1.                             00349100
           IF IND-ASSOCIATION2 IS NEGATIVE                              00349200
               MOVE SPACES TO ASSOCIATION2.                             00349300
           IF IND-ASSOCIATION3 IS NEGATIVE                              00349400
               MOVE SPACES TO ASSOCIATION3.                             00349500
                                                                        00349600
           PERFORM  0230-MOVE-COMMAREA-TO-TABLE THRU 0230-EXIT.         00349700
MANJU2     DISPLAY 'ENTITY-TYPE IN 210 UPDT PARA' ENTITY-TYPE.          00349800
           EXEC SQL                                                     00349900
900837*       UPDATE AGNTNV03                                           00350000
R00700        UPDATE AGNTNAME                                           00350100
                  SET FIRST_NAME = :FIRST-NAME,                         00350200
                      MIDDLE_NAME = :MIDDLE-NAME,                       00350300
                      LAST_NAME = :LAST-NAME,                           00350400
                      PREFIX = :PREFIX, SUFFIX1 = :SUFFIX1,             00350500
                      SUFFIX2 = :SUFFIX2, COMPANY_IND = :COMPANY-IND,   00350600
                      COMPANY_IN_ADDRESS = :COMPANY-IN-ADDRESS,         00350700
                      COMPANY_NAME = :COMPANY-NAME,                     00350800
                      DISPLAY_NAME = :DISPLAY-NAME,                     00350900
                      NICKNAME = :NICKNAME, ADDRESS1 = :ADDRESS1,       00351000
                      ADDRESS2 = :ADDRESS2, CITY = :CITY,               00351100
                      STATE = :STATE, ZIP=:ZIP,                         00351200
                      ZIP_PLUS4 = :ZIP-PLUS4,                           00351300
                      COUNTY_CODE = :COUNTY-CODE,                       00351400
                      AREA_CODE = :AREA-CODE, PHONE = :PHONE,           00351500
                      PHONE_EXTENSION = :PHONE-EXTENSION,               00351600
                      SSN = :SSN-NUM,                                   00351700
                      SEX = :SEX,                                       00351800
                      BIRTH_DATE = :BIRTH-DATE:IND-BIRTH-DATE,          00351900
                      FINALST_REAS_CODE = :FINALST-REAS-CODE,           00352000
                      FINALST_OVRD_IND = :FINALST-OVRD-IND,             00352100
                      DUP_ADDR_OVRD_IND = :DUP-ADDR-OVRD-IND,           00352200
                    EFFECTIVE_DATE = :EFFECTIVE-DATE:IND-EFFECTIVE-DATE,00352300
                      CHANGE_DATE = CURRENT TIMESTAMP,                  00352400
                      CHANGE_LOGON = :CHANGE-LOGON,                     00352500
                      ENTITY_TYPE = :ENTITY-TYPE,                       00352600
                      RECORD_STATUS = :RECORD-STATUS,                   00352700
                      ALT_ADDRESS_IND = :ALT-ADDRESS-IND,               00352800
                      FUTURE_ADDRESS_IND = :FUTURE-ADDRESS-IND,         00352900
                      RECORD_ORIGIN = :RECORD-ORIGIN,                   00353000
                      COMBINED_STATUS = :COMBINED-STATUS,               00353100
                      SITE_CODE = :SITE-CODE,                           00353200
                      NAME_KEY1 = :NAME-KEY1,                           00353300
                      NAME_KEY2 = :NAME-KEY2,                           00353400
                      NAME_KEY3 = :NAME-KEY3,                           00353500
                      ADDRESS_KEY1 = :ADDRESS-KEY1,                     00353600
                      ASSOCIATION1 = :ASSOCIATION1,                     00353700
                      ASSOCIATION2 = :ASSOCIATION2,                     00353800
                      ASSOCIATION3 = :ASSOCIATION3,                     00353900
900837                FAX_AREA_CODE = :FAX-AREA-CODE,                   00354000
900837                FAX_PHONE = :FAX-PHONE                            00354100
              WHERE CURRENT OF UPDT-AGNT-CUR                            00354200
           END-EXEC.                                                    00354300
                                                                        00354400
           IF SQLCODE NOT = ZERO                                        00355100
              MOVE 'Y'           TO WS-UPD-SQL-ERROR                    00355200
              MOVE 'AGNT REC UPD ERROR' TO WS-UPD-SQL-ERROR-MSG         00355300
              MOVE SPACES        TO WS-ERR-FIELD1                       00355400
              MOVE SQLCODE       TO WS-UPD-SQLCODE                      00355500
              MOVE 'DBERR'  TO WS-MESSAGE-NUMBER1                       00355600
              GO TO 0210-EXIT                                           00355700
           ELSE                                                         00355900
              MOVE 'NA159'           TO WS-MESSAGE-NUMBER1.             00356000
                                                                        00356100
           PERFORM 240-UPD-IDNTITY-INFO THRU 240-EXIT.                  00356200
                                                                        00356300
           IF SQLCODE = 0                                               00356400
             PERFORM 250-ADD-IDNTITY-HISTORY THRU 250-EXIT              00356500
           END-IF                                                       00356600
                                                                        00356700
           IF SQLCODE = 0                                               00356800
             EXEC SQL                                                   00356900
               CLOSE UPDT-AGNT-CUR                                      00357000
             END-EXEC                                                   00357100
                                                                        00357200
             IF SQLCODE NOT = ZERO                                      00357300
                MOVE 'Y'           TO WS-UPD-SQL-ERROR                  00357400
                MOVE 'CLOSE UPDTAGNT CUR ERR' TO WS-UPD-SQL-ERROR-MSG   00357500
                MOVE SPACES        TO WS-ERR-FIELD1                     00357600
                MOVE SQLCODE       TO WS-UPD-SQLCODE                    00357700
                MOVE 'DBERR'  TO WS-MESSAGE-NUMBER1                     00357800
                GO TO 0210-EXIT                                         00357900
             END-IF                                                     00358000
           END-IF.                                                      00358100
                                                                        00358200
       0210-EXIT.                                                       00358300
           EXIT.                                                        00358400
                                                                        00358500
       0220-UPDATE-CASENAME-ROW.                                        00358600
                                                                        00358700
           EXEC SQL                                                     00358800
               OPEN CASECUR                                             00358900
           END-EXEC.                                                    00359000
                                                                        00359100
           IF SQLCODE NOT = ZERO                                        00359200
               MOVE 'Y'           TO WS-UPD-SQL-ERROR                   00359300
               MOVE 'CASE CURSOR OPEN ERROR' TO WS-UPD-SQL-ERROR-MSG    00359400
               MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1                 00359500
               MOVE SPACES        TO WS-ERR-FIELD1                      00359600
               MOVE SQLCODE       TO WS-UPD-SQLCODE                     00359700
               GO TO 0220-EXIT.                                         00359800
                                                                        00359900
KPL        EXEC SQL FETCH CASECUR                                       00360000
R00700*        INTO DCLAGNTNAME:AGNTNAME-INDICATORS                     00360100
C23311     INTO  :IDNTITY, :LAST-NAME :IND-LAST-NAME,                   00360200
C23311       :FIRST-NAME :IND-FIRST-NAME,                               00360300
R00700       :MIDDLE-NAME :IND-MIDDLE-NAME,                             00360400
R00700       :PREFIX :IND-PREFIX, :SUFFIX1 :IND-SUFFIX1,                00360500
R00700       :SUFFIX2 :IND-SUFFIX2,                                     00360600
R00700       :COMPANY-IND, :COMPANY-IN-ADDRESS,                         00360700
C23311       :COMPANY-NAME :IND-COMPANY-NAME,                           00360800
C23311       :DISPLAY-NAME :IND-DISPLAY-NAME,                           00360900
R00700       :NICKNAME :IND-NICKNAME,                                   00361000
C23311       :ADDRESS1, :ADDRESS2 :IND-ADDRESS2,                        00361100
R00700       :CITY, :STATE, :ZIP, :ZIP-PLUS4, :COUNTY-CODE, :AREA-CODE, 00361200
R00700       :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,                  00361300
R00700       :BIRTH-DATE :IND-BIRTH-DATE,                               00361400
R00700       :FINALST-REAS-CODE, :FINALST-OVRD-IND, :DUP-ADDR-OVRD-IND, 00361500
R00700       :EFFECTIVE-DATE, :CHANGE-DATE,                             00361600
R00700       :CHANGE-LOGON, :ENTITY-TYPE,                               00361700
R00700       :RECORD-STATUS, :ALT-ADDRESS-IND, :FUTURE-ADDRESS-IND,     00361800
R00700       :RECORD-ORIGIN, :COMBINED-STATUS, :SITE-CODE, :NAME-KEY1,  00361900
R00700       :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,                     00362000
R00700       :ASSOCIATION1 :IND-ASSOCIATION1,                           00362100
R00700       :ASSOCIATION2 :IND-ASSOCIATION2,                           00362200
R00700       :ASSOCIATION3 :IND-ASSOCIATION3,                           00362300
R00700       :FAX-AREA-CODE, :FAX-PHONE,                                00362400
R04023       :ORIGINAL-STATE,                                           00362500
R02539       :EMAIL1 :IND-EMAIL1, :PASS-WORD :IND-PASSWORD,             00362600
R02539       :COUNTRY-CODE :IND-COUNTRY-CODE,                           00362700
R02539       :POSTAL-CODE :IND-POSTAL-CODE                              00362800
           END-EXEC.                                                    00362900
                                                                        00363000
           IF SQLCODE NOT = ZERO                                        00363100
               MOVE 'Y'           TO WS-UPD-SQL-ERROR                   00363200
               MOVE 'CASE CURSOR FETCH ERROR' TO WS-UPD-SQL-ERROR-MSG   00363300
               MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1                 00363400
               MOVE SPACES        TO WS-ERR-FIELD1                      00363500
               MOVE SQLCODE       TO WS-UPD-SQLCODE                     00363600
               GO TO 0220-EXIT.                                         00363700
                                                                        00363800
           IF IND-LAST-NAME IS NEGATIVE                                 00363900
               MOVE SPACES TO LAST-NAME.                                00364000
           IF IND-FIRST-NAME IS NEGATIVE                                00364100
               MOVE SPACES TO FIRST-NAME.                               00364200
           IF IND-MIDDLE-NAME IS NEGATIVE                               00364300
               MOVE SPACES TO MIDDLE-NAME.                              00364400
           IF IND-PREFIX IS NEGATIVE                                    00364500
               MOVE SPACES TO PREFIX.                                   00364600
           IF IND-SUFFIX1 IS NEGATIVE                                   00364700
               MOVE SPACES TO SUFFIX1.                                  00364800
           IF IND-SUFFIX2 IS NEGATIVE                                   00364900
               MOVE SPACES TO SUFFIX2.                                  00365000
           IF IND-COMPANY-NAME IS NEGATIVE                              00365100
               MOVE SPACES TO COMPANY-NAME.                             00365200
           IF IND-BIRTH-DATE IS NEGATIVE                                00365300
               MOVE ZEROES TO BIRTH-DATE.                               00365400
           IF IND-DISPLAY-NAME IS NEGATIVE                              00365500
               MOVE SPACES TO DISPLAY-NAME.                             00365600
           IF IND-NICKNAME IS NEGATIVE                                  00365700
               MOVE SPACES TO NICKNAME.                                 00365800
           IF IND-ADDRESS2 IS NEGATIVE                                  00365900
               MOVE SPACES TO ADDRESS2.                                 00366000
           IF IND-ASSOCIATION1 IS NEGATIVE                              00366100
               MOVE SPACES TO ASSOCIATION1.                             00366200
           IF IND-ASSOCIATION2 IS NEGATIVE                              00366300
               MOVE SPACES TO ASSOCIATION2.                             00366400
           IF IND-ASSOCIATION3 IS NEGATIVE                              00366500
               MOVE SPACES TO ASSOCIATION3.                             00366600
           IF IND-FAX-PHONE IS NEGATIVE                                 00366700
               MOVE SPACES TO FAX-PHONE.                                00366800
           IF IND-EMAIL1 IS NEGATIVE                                    00366900
               MOVE SPACES TO EMAIL1.                                   00367000
R02539     IF IND-PASSWORD IS NEGATIVE                                  00367100
R02539         MOVE SPACES TO PASS-WORD.                                00367200
R02539     IF IND-POSTAL-CODE IS NEGATIVE                               00367300
R02539         MOVE SPACES TO POSTAL-CODE.                              00367400
R02539     IF IND-COUNTRY-CODE IS NEGATIVE                              00367500
R02539         MOVE SPACES TO COUNTRY-CODE.                             00367600
                                                                        00367700
           EXEC SQL                                                     00367800
               CLOSE CASECUR                                            00367900
           END-EXEC.                                                    00368000
                                                                        00368100
           IF SQLCODE NOT = ZERO                                        00368200
               MOVE 'Y'           TO WS-UPD-SQL-ERROR                   00368300
               MOVE 'CASE CURSOR CLOSE ERROR' TO WS-UPD-SQL-ERROR-MSG   00368400
               MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1                 00368500
               MOVE SPACES        TO WS-ERR-FIELD1                      00368600
               MOVE SQLCODE       TO WS-UPD-SQLCODE                     00368700
               GO TO 0220-EXIT.                                         00368800
                                                                        00368900
      *---------------------------------------------------------------* 00369000
      *    THIS CODE IS NEEDED TO UPDATE THE AUDIT RECORD WITH        * 00369100
      *    THE IMAGE OF THE RECORD BEFORE IT WAS CHANGED. DB2         * 00369200
      *    DOES NOT MAKE THE RECORD AVAILABLE IN THE REDEFINES        * 00369300
      *    AREA WHEN IT IS FETCHED FOR UPDATE.                         *00369400
      *---------------------------------------------------------------* 00369500
           EXEC SQL                                                     00369600
               OPEN UPDT-CASE-CUR                                       00369700
           END-EXEC.                                                    00369800
           IF SQLCODE NOT = ZERO                                        00369900
               MOVE 'Y'           TO WS-UPD-SQL-ERROR                   00370000
               MOVE 'UPD CASE CURSOR OPEN ERR' TO WS-UPD-SQL-ERROR-MSG  00370100
               MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1                 00370200
               MOVE SPACES        TO WS-ERR-FIELD1                      00370300
               MOVE SQLCODE       TO WS-UPD-SQLCODE                     00370400
               GO TO 0220-EXIT.                                         00370500
                                                                        00370600
           EXEC SQL FETCH UPDT-CASE-CUR                                 00370700
110611*        INTO :DCLCASENAME:CASENAME-INDICATORS                    00370800
110611                INTO  :IDNTITY, :LAST-NAME :IND-LAST-NAME,        00370900
110611                      :FIRST-NAME :IND-FIRST-NAME,                00371000
110611                      :MIDDLE-NAME :IND-MIDDLE-NAME,              00371100
110611                      :PREFIX :IND-PREFIX, :SUFFIX1 :IND-SUFFIX1, 00371200
110611                      :SUFFIX2 :IND-SUFFIX2,                      00371300
110611                      :COMPANY-IND, :COMPANY-IN-ADDRESS,          00371400
110611                      :COMPANY-NAME :IND-COMPANY-NAME,            00371500
110611                      :DISPLAY-NAME :IND-DISPLAY-NAME,            00371600
110611                      :NICKNAME :IND-NICKNAME,                    00371700
110611                      :ADDRESS1, :ADDRESS2 :IND-ADDRESS2,         00371800
110611                      :CITY, :STATE, :ZIP, :ZIP-PLUS4,            00371900
110611                      :COUNTY-CODE, :AREA-CODE,                   00372000
110611                      :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,   00372100
110611                      :BIRTH-DATE :IND-BIRTH-DATE,                00372200
110611                      :FINALST-REAS-CODE, :FINALST-OVRD-IND,      00372300
110611                      :DUP-ADDR-OVRD-IND,                         00372400
110611                      :EFFECTIVE-DATE, :CHANGE-DATE,              00372500
110611                      :CHANGE-LOGON, :ENTITY-TYPE,                00372600
110611                      :RECORD-STATUS, :ALT-ADDRESS-IND,           00372700
110611                      :FUTURE-ADDRESS-IND,                        00372800
110611                      :RECORD-ORIGIN, :COMBINED-STATUS,           00372900
110611                      :SITE-CODE, :NAME-KEY1,                     00373000
110611                      :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,      00373100
110611                      :ASSOCIATION1 :IND-ASSOCIATION1,            00373200
110611                      :ASSOCIATION2 :IND-ASSOCIATION2,            00373300
110611                      :ASSOCIATION3 :IND-ASSOCIATION3,            00373400
110611                      :FAX-AREA-CODE, :FAX-PHONE,                 00373500
110611                      :ORIGINAL-STATE,                            00373600
110611                      :EMAIL1 :IND-EMAIL1,                        00373700
R9031A****                  :PASS-WORD :IND-PASSWORD,                   00373800
110611                      :COUNTRY-CODE :IND-COUNTRY-CODE,            00373900
110611                      :POSTAL-CODE :IND-POSTAL-CODE,              00374000
R9031A                      :WS-EMAIL-STATUS                            00374100
           END-EXEC.                                                    00374200
                                                                        00374300
              IF SQLCODE = 100                                          00374500
                  MOVE 'Y'           TO WS-UPD-SQL-ERROR                00374600
                  MOVE 'CASE REC NOT FOUND' TO WS-UPD-SQL-ERROR-MSG     00374700
                  MOVE SPACES        TO WS-ERR-FIELD1                   00374800
                  MOVE SQLCODE       TO WS-UPD-SQLCODE                  00374900
                  MOVE 'NA020'  TO WS-MESSAGE-NUMBER1                   00375000
                  GO TO 0220-EXIT                                       00375100
              ELSE                                                      00375200
                 IF SQLCODE NOT = ZERO                                  00375300
                     MOVE 'Y'           TO WS-UPD-SQL-ERROR             00375400
                     MOVE 'UPD CASECUR FETCH ERROR'                     00375500
                                             TO WS-UPD-SQL-ERROR-MSG    00375600
                     MOVE SPACES        TO WS-ERR-FIELD1                00375700
                     MOVE SQLCODE       TO WS-UPD-SQLCODE               00375800
                     MOVE 'DBERR'  TO WS-MESSAGE-NUMBER1                00375900
                     GO TO 0220-EXIT.                                   00376000
           IF IND-LAST-NAME IS NEGATIVE                                 00376100
               MOVE SPACES TO LAST-NAME.                                00376200
           IF IND-FIRST-NAME IS NEGATIVE                                00376300
               MOVE SPACES TO FIRST-NAME.                               00376400
           IF IND-MIDDLE-NAME IS NEGATIVE                               00376500
               MOVE SPACES TO MIDDLE-NAME.                              00376600
           IF IND-PREFIX IS NEGATIVE                                    00376700
               MOVE SPACES TO PREFIX.                                   00376800
           IF IND-SUFFIX1 IS NEGATIVE                                   00376900
               MOVE SPACES TO SUFFIX1.                                  00377000
           IF IND-SUFFIX2 IS NEGATIVE                                   00377100
               MOVE SPACES TO SUFFIX2.                                  00377200
           IF IND-COMPANY-NAME IS NEGATIVE                              00377300
               MOVE SPACES TO COMPANY-NAME.                             00377400
           IF IND-BIRTH-DATE IS NEGATIVE                                00377500
               MOVE ZEROES TO BIRTH-DATE.                               00377600
           IF IND-DISPLAY-NAME IS NEGATIVE                              00377700
               MOVE SPACES TO DISPLAY-NAME.                             00377800
           IF IND-NICKNAME IS NEGATIVE                                  00377900
               MOVE SPACES TO NICKNAME.                                 00378000
           IF IND-ADDRESS2 IS NEGATIVE                                  00378100
               MOVE SPACES TO ADDRESS2.                                 00378200
           IF IND-ASSOCIATION1 IS NEGATIVE                              00378300
               MOVE SPACES TO ASSOCIATION1.                             00378400
           IF IND-ASSOCIATION2 IS NEGATIVE                              00378500
               MOVE SPACES TO ASSOCIATION2.                             00378600
           IF IND-ASSOCIATION3 IS NEGATIVE                              00378700
               MOVE SPACES TO ASSOCIATION3.                             00378800
R02539     IF IND-POSTAL-CODE IS NEGATIVE                               00378900
R02539         MOVE SPACES TO POSTAL-CODE.                              00379000
R02539     IF IND-COUNTRY-CODE IS NEGATIVE                              00379100
R02539         MOVE SPACES TO COUNTRY-CODE.                             00379200
                                                                        00379300
R9031A     IF WS-EMAIL-STATUS = 'I' AND                                 00379400
R9031A        EMAIL1 > SPACES                                           00379500
R9031A        MOVE ' ' TO WS-EMAIL-STATUS                               00379600
R9031A     END-IF.                                                      00379700
                                                                        00379800
           PERFORM 0230-MOVE-COMMAREA-TO-TABLE THRU 0230-EXIT.          00379900
                                                                        00380000
           EXEC SQL                                                     00380100
900837*       UPDATE CASENV03                                           00380200
R00700        UPDATE CASENAME                                           00380300
                  SET FIRST_NAME = :FIRST-NAME,                         00380400
                      MIDDLE_NAME = :MIDDLE-NAME,                       00380500
                      LAST_NAME = :LAST-NAME,                           00380600
                      PREFIX = :PREFIX, SUFFIX1 = :SUFFIX1,             00380700
                      SUFFIX2 = :SUFFIX2, COMPANY_IND = :COMPANY-IND,   00380800
                      COMPANY_IN_ADDRESS = :COMPANY-IN-ADDRESS,         00380900
                      COMPANY_NAME = :COMPANY-NAME,                     00381000
                      DISPLAY_NAME = :DISPLAY-NAME,                     00381100
                      NICKNAME = :NICKNAME, ADDRESS1 = :ADDRESS1,       00381200
                      ADDRESS2 = :ADDRESS2, CITY = :CITY,               00381300
                      STATE = :STATE, ZIP=:ZIP,                         00381400
                      ZIP_PLUS4 = :ZIP-PLUS4,                           00381500
                      COUNTY_CODE = :COUNTY-CODE,                       00381600
                      AREA_CODE = :AREA-CODE, PHONE = :PHONE,           00381700
                      PHONE_EXTENSION = :PHONE-EXTENSION,               00381800
                      SSN = :SSN-NUM,                                   00381900
                      SEX = :SEX,                                       00382000
                      BIRTH_DATE = :BIRTH-DATE:IND-BIRTH-DATE,          00382100
                      FINALST_REAS_CODE = :FINALST-REAS-CODE,           00382200
                      FINALST_OVRD_IND = :FINALST-OVRD-IND,             00382300
                      DUP_ADDR_OVRD_IND = :DUP-ADDR-OVRD-IND,           00382400
                    EFFECTIVE_DATE = :EFFECTIVE-DATE:IND-EFFECTIVE-DATE,00382500
                      CHANGE_DATE = CURRENT TIMESTAMP,                  00382600
                      CHANGE_LOGON = :CHANGE-LOGON,                     00382700
                      ENTITY_TYPE = :ENTITY-TYPE,                       00382800
                      RECORD_STATUS = :RECORD-STATUS,                   00382900
                      ALT_ADDRESS_IND = :ALT-ADDRESS-IND,               00383000
                      FUTURE_ADDRESS_IND = :FUTURE-ADDRESS-IND,         00383100
                      RECORD_ORIGIN = :RECORD-ORIGIN,                   00383200
                      COMBINED_STATUS = :COMBINED-STATUS,               00383300
                      SITE_CODE = :SITE-CODE,                           00383400
                      NAME_KEY1 = :NAME-KEY1,                           00383500
                      NAME_KEY2 = :NAME-KEY2,                           00383600
                      NAME_KEY3 = :NAME-KEY3,                           00383700
                      ADDRESS_KEY1 = :ADDRESS-KEY1,                     00383800
                      ASSOCIATION1 = :ASSOCIATION1,                     00383900
                      ASSOCIATION2 = :ASSOCIATION2,                     00384000
                      ASSOCIATION3 = :ASSOCIATION3,                     00384100
900837                FAX_AREA_CODE = :FAX-AREA-CODE,                   00384200
900837                FAX_PHONE = :FAX-PHONE,                           00384300
R04023                ORIGINAL_STATE = :ORIGINAL-STATE,                 00384400
R00700                EMAIL     = :EMAIL1,                              00384500
R02539                COUNTRY_CODE = :COUNTRY-CODE,                     00384600
R02539                POSTAL_CODE = :POSTAL-CODE,                       00384700
R9031A                EMAIL_STATUS = :WS-EMAIL-STATUS                   00384800
              WHERE CURRENT OF UPDT-CASE-CUR                            00384900
           END-EXEC.                                                    00385000
                                                                        00385100
           IF SQLCODE NOT = ZERO                                        00385200
              MOVE 'Y'           TO WS-UPD-SQL-ERROR                    00385300
              MOVE 'CASE REC UPD ERROR' TO WS-UPD-SQL-ERROR-MSG         00385400
              MOVE SPACES        TO WS-ERR-FIELD1                       00385500
              MOVE SQLCODE       TO WS-UPD-SQLCODE                      00385600
              MOVE 'DBERR'  TO WS-MESSAGE-NUMBER1                       00385700
              GO TO 0220-EXIT                                           00385800
           ELSE                                                         00385900
              MOVE 'NA159'           TO WS-MESSAGE-NUMBER1.             00386000
                                                                        00390300
           PERFORM 240-UPD-IDNTITY-INFO THRU 240-EXIT.                  00390400
                                                                        00390500
           IF SQLCODE = 0                                               00390600
             PERFORM 250-ADD-IDNTITY-HISTORY THRU 250-EXIT              00390700
           END-IF                                                       00390800
                                                                        00391000
           IF SQLCODE = 0                                               00391100
             EXEC SQL                                                   00391200
               CLOSE UPDT-CASE-CUR                                      00391300
             END-EXEC                                                   00392000
                                                                        00392900
             IF SQLCODE NOT = ZERO                                      00393000
                MOVE 'Y'           TO WS-UPD-SQL-ERROR                  00393100
                MOVE 'CLOSE UPDTAGNT CUR ERR' TO WS-UPD-SQL-ERROR-MSG   00393200
                MOVE SPACES        TO WS-ERR-FIELD1                     00393300
                MOVE SQLCODE       TO WS-UPD-SQLCODE                    00393400
                MOVE 'DBERR'  TO WS-MESSAGE-NUMBER1                     00393500
                GO TO 0220-EXIT                                         00393600
             END-IF                                                     00393700
           END-IF.                                                      00393800
                                                                        00393900
       0220-EXIT.                                                       00394000
           EXIT.                                                        00400000
                                                                        00408500
       0230-MOVE-COMMAREA-TO-TABLE.                                     00408600
                                                                        00408700
           MOVE COM-CIM                TO IDNTITY.                      00408800
           MOVE COM-LAST-NAME          TO LAST-NAME.                    00408900
           MOVE COM-FIRST-NAME         TO FIRST-NAME.                   00409000
           MOVE COM-MIDDLE-NAME        TO MIDDLE-NAME.                  00409100
           MOVE COM-PREFIX             TO PREFIX.                       00409200
           MOVE COM-SUFFIX1            TO SUFFIX1.                      00409300
           MOVE COM-SUFFIX2            TO SUFFIX2.                      00409400
           MOVE COM-COMPANY-IND        TO COMPANY-IND.                  00409500
           MOVE COM-COMPANY-IN-ADDRESS TO COMPANY-IN-ADDRESS.           00409600
           MOVE COM-COMPANY-NAME       TO COMPANY-NAME.                 00409700
           MOVE COM-DISPLAY-NAME       TO DISPLAY-NAME.                 00409800
           MOVE COM-NICKNAME           TO NICKNAME.                     00409900
           MOVE COM-ADDRESS1           TO ADDRESS1.                     00410000
           MOVE COM-ADDRESS2           TO ADDRESS2.                     00410100
           MOVE COM-CITY               TO CITY.                         00410200
           MOVE COM-STATE              TO STATE.                        00410300
           MOVE COM-ZIP                TO ZIP.                          00410400
           MOVE COM-ZIP-PLUS4          TO ZIP-PLUS4.                    00410500
           MOVE COM-COUNTY-CODE        TO COUNTY-CODE.                  00410600
           MOVE COM-AREA-CODE          TO AREA-CODE.                    00410700
           MOVE COM-PHONE              TO PHONE.                        00410800
           MOVE COM-PHONE-EXTENSION    TO PHONE-EXTENSION.              00410900
900837     MOVE COM-FAX-AREA-CODE      TO FAX-AREA-CODE.                00411000
900837     MOVE COM-FAX-PHONE          TO FAX-PHONE.                    00411100
           MOVE COM-SSN                TO SSN-NUM.                      00411200
           MOVE COM-SEX                TO SEX.                          00411300
           MOVE COM-FINALST-REAS-CODE  TO FINALST-REAS-CODE.            00411400
           MOVE COM-FINALST-OVRD-IND   TO FINALST-OVRD-IND.             00411500
           MOVE COM-DUP-ADDR-OVRD-IND  TO DUP-ADDR-OVRD-IND.            00411600
R04023** ISSUE DATE IS ONLY UPDATED FOR AIG, SHOULD NOT CHANGE          00411700
R04023** OTHER CLIENTS ISSUE STATE                                      00411800
R04023     IF COM-ISSUE-STATE NOT = SPACES                              00411900
R04023        MOVE COM-ISSUE-STATE        TO ORIGINAL-STATE.            00412000
R00700     MOVE COM-EMAIL1             TO EMAIL1.                       00412100
R02539     MOVE COM-POSTAL-CODE        TO POSTAL-CODE.                  00412200
R02539     MOVE COM-COUNTRY-CODE       TO COUNTRY-CODE.                 00412300
           DISPLAY ' 230 PARA POSTAL-CODE' POSTAL-CODE.                 00412400
           DISPLAY '230 PARA COUNTRY-CODE' COUNTRY-CODE.                00412500
                                                                        00412900
           MOVE 'WIPRO123'             TO COM-CHANGE-LOGON              00413200
                                          CHANGE-LOGON.                 00413300
           MOVE '01'                   TO COM-SITE-CODE                 00413400
                                          SITE-CODE.                    00413500
                                                                        00413600
           MOVE COM-RECORD-STATUS      TO RECORD-STATUS.                00413700
           MOVE COM-ALT-ADDRESS-IND    TO ALT-ADDRESS-IND.              00413800
           MOVE COM-FUTURE-ADDRESS-IND TO FUTURE-ADDRESS-IND.           00413900
           MOVE COM-RECORD-ORIGIN      TO RECORD-ORIGIN.                00414000
           MOVE COM-COMBINED-STATUS    TO COMBINED-STATUS.              00414100
           MOVE COM-NAME-KEY1          TO NAME-KEY1.                    00414200
           MOVE COM-NAME-KEY2          TO NAME-KEY2.                    00414300
           MOVE COM-NAME-KEY3          TO NAME-KEY3.                    00414400
           MOVE COM-ADDRESS-KEY1       TO ADDRESS-KEY1.                 00414500
           MOVE COM-ASSOCIATION1       TO ASSOCIATION1.                 00414600
           MOVE COM-ASSOCIATION2       TO ASSOCIATION2.                 00414700
           MOVE COM-ASSOCIATION3       TO ASSOCIATION3.                 00414800
           MOVE COM-EFFECTIVE-DATE     TO WS-DATE-R.                    00414900
           MOVE ZERO                   TO IND-EFFECTIVE-DATE.           00415000
           MOVE WS-DATE-R              TO WS-DATE.                      00415100
           MOVE '20'                   TO EF-CC.                        00417100
           MOVE WS-YY-A                TO EF-YY.                        00417200
           MOVE WS-MM-A                TO EF-MM.                        00417300
           MOVE WS-DD-A                TO EF-DD.                        00417400
           MOVE WS-EFFECTIVE-DATE      TO EFFECTIVE-DATE.               00417500
           IF COM-ENTITY-TYPE = 'BR'                                    00417600
                MOVE 'ML' TO ENTITY-TYPE                                00417700
           ELSE                                                         00417800
              MOVE COM-ENTITY-TYPE        TO ENTITY-TYPE.               00417900
                                                                        00418000
           MOVE -1 TO IND-BIRTH-DATE.                                   00420300
                                                                        00579200
       0230-EXIT.                                                       00579300
           EXIT.                                                        00579400
                                                                        00579500
       240-UPD-IDNTITY-INFO.                                            00579600
                                                                        00579700
           DISPLAY '240-UPD-IDNTITY-INFO'.                              00579800
                                                                        00579900
           EXEC SQL                                                     00580000
              UPDATE IDNTITY_INFO                                       00580100
                  SET ADDRESS1 = :ADDRESS1,                             00580200
                      ADDRESS2 = :ADDRESS2, CITY = :CITY,               00580300
                      STATE = :STATE, ZIP=:ZIP,                         00580400
                      ZIP_PLUS4 = :ZIP-PLUS4,                           00580500
                    EFFECTIVE_DATE = :EFFECTIVE-DATE,                   00580600
                      CHANGE_DATE = CURRENT TIMESTAMP,                  00580700
                      ENTITY_TYPE = :ENTITY-TYPE,                       00580800
                      RECORD_STATUS = :RECORD-STATUS                    00580900
              WHERE IDNTITY = :WS-UPD-CIM-NUMBER                        00581000
           END-EXEC.                                                    00581100
                                                                        00581200
           IF SQLCODE NOT = ZERO                                        00581300
                 MOVE 'Y'           TO WS-UPD-SQL-ERROR                 00581914
                 MOVE 'IDNTITY INFO UPDATE ERROR'                       00582014
                                    TO WS-UPD-SQL-ERROR-MSG             00582114
                 MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1               00582214
                 MOVE SPACES        TO WS-ERR-FIELD1                    00582314
                 MOVE SQLCODE       TO WS-UPD-SQLCODE                   00582414
           END-IF.                                                      00582500
                                                                        00582600
       240-EXIT.                                                        00582700
           EXIT.                                                        00582800
                                                                        00582900
       250-ADD-IDNTITY-HISTORY.                                         00583000
                                                                        00583100
           DISPLAY '250-ADD-IDNTITY-HISTORY'.                           00583200
                                                                        00583300
           EXEC SQL                                                     00583400
              INSERT INTO  IDNTITY_HISTORY                              00584000
                (IDNTITY, TRANSACTION, CHANGE_DATE, MONGODB_SYNC)       00590000
           VALUES (:WS-UPD-CIM-NUMBER, :WS-TRAN, CURRENT TIMESTAMP,     00600000
                   :WS-N)                                               00601000
           END-EXEC.                                                    00610000
                                                                        00620000
           IF SQLCODE NOT = ZERO                                        00630000
                 MOVE 'Y'           TO WS-UPD-SQL-ERROR                 00640014
                 MOVE 'IDNTITY HIST INSERT ERROR'                       00650014
                                    TO WS-UPD-SQL-ERROR-MSG             00651014
                 MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1               00660014
                 MOVE SPACES        TO WS-ERR-FIELD1                    00670000
                 MOVE SQLCODE       TO WS-UPD-SQLCODE                   00680014
           END-IF.                                                      00690000
                                                                        00700000
       250-EXIT.                                                        00710000
           EXIT.                                                        00720000
                                                                        00730000
