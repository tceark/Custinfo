           05  AUDIT-PORTION-OLD.                                       AUDCOMM
               10  INITIALS                      PIC X(3).              AUDCOMM
               10  AUDIT-CODE                    PIC X(2).              AUDCOMM
      *---------------------------------------------------------------* AUDCOMM
      *    THE FOLLOWING 88 LEVELS ARE OUTDATED AND VERY INCOMPLETE.  * AUDCOMM
      *    DO NOT RELY ON THESE AS A GUIDE TO VALID AUDIT CODES.      * AUDCOMM
      *      JCT 7/87.                                                * AUDCOMM
      *---------------------------------------------------------------* AUDCOMM
                   88  VALID-AUDITS               VALUE 'CA' 'CP' 'CB'  AUDCOMM
                                                        'XX' 'CY' 'C1'  AUDCOMM
                                                        'CM' ' C' 'CC'  AUDCOMM
                                                        ' B' ' A' ' T'  AUDCOMM
                                                        'CX' 'LX' 'CD'  AUDCOMM
                                                        'LC' 'LA' 'LU'  AUDCOMM
                                                        'CE' ' L' ' E'  AUDCOMM
                                                        'LG' 'BC' ' R'  AUDCOMM
                                                        'BA' 'BU' 'BD'  AUDCOMM
                                                        'PA' 'PU' 'PD'  AUDCOMM
                                                        'R1' 'R2' 'R3'  AUDCOMM
                                                        'RT'.           AUDCOMM
                   88  CASE-AUDIT                 VALUE 'CA' 'CP' 'CB'  AUDCOMM
                                                        'CX' 'XX' 'CY'  AUDCOMM
                                                        'C1' 'LX' 'LG'. AUDCOMM
                   88  CLAIM-AUDIT                VALUE ' C' 'CC' 'CD'  AUDCOMM
                                                        'CE' 'CM'.      AUDCOMM
                   88  EMP-AUDIT                  VALUE ' A' ' T' ' E'  AUDCOMM
                                                        ' R'.           AUDCOMM
                   88  BROKER-ADD                 VALUE 'BA'.           AUDCOMM
                   88  BROKER-UPDATE              VALUE 'BU'.           AUDCOMM
                   88  BROKER-DELETE              VALUE 'BD'.           AUDCOMM
                   88  LICENSE-AUDIT              VALUE ' L' 'LC'.      AUDCOMM
                   88  LARGE-GROUP-AUDIT          VALUE 'LA' 'LU'.      AUDCOMM
                   88  BENEFIT-CHG-AUDIT          VALUE 'BC'.           AUDCOMM
                   88  PARM-AUDIT                 VALUE 'PA' 'PU' 'PD'. AUDCOMM
                   88  CG-RATE-AUDIT              VALUE 'RT'.           AUDCOMM
                   88  EXCL-AUDIT                 VALUE 'R1' 'R2' 'R3'. AUDCOMM
               10  CARRIER                       PIC X(2).              AUDCOMM
               10  FILLER                        PIC X(1).              AUDCOMM
      *        10  MARKET-CODE                   PIC X(1).              AUDCOMM
      *            88 AUDIT-OK-MKTS               VALUE '1' '2' '3' '4' AUDCOMM
      *                                                '5' '6' '7' '8'  AUDCOMM
      *                                                'F' 'G' 'J' 'K'  AUDCOMM
      *                                                'L' 'M' 'N' 'P'  AUDCOMM
      *                                                'Q' 'R' 'T' 'C'. AUDCOMM
      *            88 AUDIT-DB-MKTS               VALUE 'J' 'K' 'L' 'M'.AUDCOMM
               10  DEPTID                        PIC X(5).              AUDCOMM
               10  CLMNUM                        PIC 9(12).             AUDCOMM
               10  MARKET-CODE                   PIC X(2).              AUDCOMM
                   88 AUDIT-OK-MKTS               VALUE '1' '2' '3' '4' AUDCOMM
                                                       '5' '6' '7' '8'  AUDCOMM
                                                       'F' 'G' 'J' 'K'  AUDCOMM
                                                       'L' 'M' 'N' 'P'  AUDCOMM
                                                       'Q' 'R' 'T' 'C'. AUDCOMM
                   88 AUDIT-DB-MKTS               VALUE 'J' 'K' 'L' 'M'.AUDCOMM
               10  FILLER                        PIC X(11).             AUDCOMM
      *        10  LICNUM                        PIC X(13).             AUDCOMM
      *        10  LICBRKDOWN REDEFINES LICNUM.                         AUDCOMM
      *            15  LC-SSN1                   PIC 9(9).              AUDCOMM
      *            15  LC-STATE                  PIC X(2).              AUDCOMM
      *            15  LC-FILL                   PIC X(2).              AUDCOMM
               10  BROKERNUM                     PIC X(10).             AUDCOMM
               10  FILLER REDEFINES BROKERNUM.                          AUDCOMM
                   15  AC-ID-NUMBER              PIC X(8).              AUDCOMM
                   15  FILLER                    PIC X(2).              AUDCOMM
               10  IDNTITY-NAME                  PIC X(6).              AUDCOMM
               10  FILLER REDEFINES IDNTITY-NAME.                       AUDCOMM
                   15  AC-TRANID                 PIC X(4).              AUDCOMM
                   15  FILLER                    PIC X(2).              AUDCOMM
               10  CICS-ERROR-SW                 PIC X(1).              AUDCOMM
               10  CERTDISTCODE                  PIC X(1).              AUDCOMM
               10  REQDATE                       PIC X(6).              AUDCOMM
               10  ACASH1 REDEFINES REQDATE      PIC 9(6).              AUDCOMM
               10  ACASH2                        PIC 9(6).              AUDCOMM
               10  ACASH3                        PIC 9(6).              AUDCOMM
               10  ACASH4                        PIC 9(6).              AUDCOMM
               10  PERIOD1                       PIC 9(4).              AUDCOMM
               10  PERIOD2                       PIC 9(4).              AUDCOMM
               10  PERIOD3                       PIC 9(4).              AUDCOMM
      *        10  PERIOD4                       PIC 9(4).              AUDCOMM
               10  PREV-DEP-LIFE-EXT             PIC X(1).              AUDCOMM
               10  CURR-DEP-LIFE-EXT             PIC X(1).              AUDCOMM
               10  PREV-DENTAL-EXT               PIC X(1).              AUDCOMM
               10  CURR-DENTAL-EXT               PIC X(1).              AUDCOMM
               10  PERIODI.                                             AUDCOMM
                   15  PMO                       PIC 99.                AUDCOMM
                   15  PYR                       PIC 99.                AUDCOMM
               10  CASH-TOT                      PIC 9(6).              AUDCOMM
               10  BROKER-TYPE                   PIC X(1).              AUDCOMM
           05  AUDIT-PORTION-FOR-ERISCO.                                AUDCOMM
               10  PREV-HEALTH                   PIC X(1).              AUDCOMM
               10  PREV-LIFE                     PIC X(1).              AUDCOMM
               10  PREV-MATERNITY                PIC X(1).              AUDCOMM
               10  PREV-DEP-HLTH                 PIC X(1).              AUDCOMM
               10  PREV-DEP-LIFE                 PIC X(1).              AUDCOMM
               10  PREV-STD                      PIC X(1).              AUDCOMM
               10  PREV-DENTAL                   PIC X(1).              AUDCOMM
               10  PREV-PCS                      PIC X(1).              AUDCOMM
               10  PREV-LTD                      PIC X(1).              AUDCOMM
               10  PREV-CODE10                   PIC X(1).              AUDCOMM
               10  PREV-ADANDD                   PIC X(1).              AUDCOMM
               10  PREV-CODE12                   PIC X(1).              AUDCOMM
               10  PREV-DEP-CODE                 PIC X(1).              AUDCOMM
               10  CURR-HEALTH                   PIC X(1).              AUDCOMM
               10  CURR-LIFE                     PIC X(1).              AUDCOMM
               10  CURR-MATERNITY                PIC X(1).              AUDCOMM
               10  CURR-DEP-HLTH                 PIC X(1).              AUDCOMM
               10  CURR-DEP-LIFE                 PIC X(1).              AUDCOMM
               10  CURR-STD                      PIC X(1).              AUDCOMM
               10  CURR-DENTAL                   PIC X(1).              AUDCOMM
               10  CURR-PCS                      PIC X(1).              AUDCOMM
               10  CURR-LTD                      PIC X(1).              AUDCOMM
               10  CURR-CODE10                   PIC X(1).              AUDCOMM
               10  CURR-ADANDD                   PIC X(1).              AUDCOMM
               10  CURR-CODE12                   PIC X(1).              AUDCOMM
               10  CURR-DEP-CODE                 PIC X(1).              AUDCOMM
               10  CASE-INCEPT-DATE.                                    AUDCOMM
                   15  CASE-INCEPT-MM            PIC 9(2).              AUDCOMM
                       88  CASE-INCEPT-MM-OK      VALUE 01 THRU 12.     AUDCOMM
                   15  CASE-INCEPT-DD            PIC 9(2).              AUDCOMM
                       88  CASE-INCEPT-DD-OK      VALUE 01 THRU 31.     AUDCOMM
                   15  CASE-INCEPT-YY            PIC 9(2).              AUDCOMM
               10  EMP-EFF-DATE.                                        AUDCOMM
                   15  EMP-EFF-MM                PIC 9(2).              AUDCOMM
                       88  EMP-EFF-MM-OK          VALUE 01 THRU 12.     AUDCOMM
                   15  EMP-EFF-DD                PIC 9(2).              AUDCOMM
                       88  EMP-EFF-DD-OK          VALUE 01 THRU 31.     AUDCOMM
                   15  EMP-EFF-YY                PIC 9(2).              AUDCOMM
               10  UNIQUENUM                     PIC X(6).              AUDCOMM
               10  AC-PREV-CARR                  PIC X(2).              AUDCOMM
               10  AC-PREV-AREA                  PIC X(2).              AUDCOMM
               10  AC-CURR-AREA                  PIC X(2).              AUDCOMM
               10  AC-PREV-LEVEL                 PIC X(2).              AUDCOMM
               10  AC-CURR-LEVEL                 PIC X(2).              AUDCOMM
               10  AC-PREV-STATE                 PIC X(2).              AUDCOMM
               10  AC-CURR-STATE                 PIC X(2).              AUDCOMM
               10  AC-PREV-LIFE-AMOUNT           PIC S9(7) COMP-3.      AUDCOMM
               10  AC-CURR-LIFE-AMOUNT           PIC S9(7) COMP-3.      AUDCOMM
               10  AC-PREV-SEX                   PIC X(01).             AUDCOMM
               10  AC-CURR-SEX                   PIC X(01).             AUDCOMM
               10  AC-PREV-AGE                   PIC S9(03) COMP-3.     AUDCOMM
               10  AC-CURR-AGE                   PIC S9(03) COMP-3.     AUDCOMM
               10  AC-PREV-SALARY                PIC S9(07) COMP-3.     AUDCOMM
               10  AC-CURR-SALARY                PIC S9(07) COMP-3.     AUDCOMM
               10  AC-PREV-DISABILITY            PIC S9(03) COMP-3.     AUDCOMM
               10  AC-CURR-DISABILITY            PIC S9(03) COMP-3.     AUDCOMM
               10  AC-PREV-EMP-OPTION            PIC X(01).             AUDCOMM
               10  AC-CURR-EMP-OPTION            PIC X(01).             AUDCOMM
               10  AC-PREV-SMOKER                PIC X(01).             AUDCOMM
               10  AC-CURR-SMOKER                PIC X(01).             AUDCOMM
               10  AC-EFFECTIVE-DATE.                                   AUDCOMM
                   15  AC-EFF-YY                 PIC 9(02).             AUDCOMM
                   15  AC-EFF-MM                 PIC 9(02).             AUDCOMM
               10  AC-PREV-CASE-OPTION           PIC X.                 AUDCOMM
               10  AC-CURR-CASE-OPTION           PIC X.                 AUDCOMM
               10  AC-SITE-CODE                  PIC XX.                AUDCOMM
               10  AC-USER-ID                    PIC X(8).              AUDCOMM
               10  AC-REMAINDER-LENGTH          PIC S9(4) COMP VALUE +0.AUDCOMM
               10  AC-PREV-RECORD                PIC X(1700).           AUDCOMM
               10  AC-CURR-RECORD                PIC X(1700).           AUDCOMM
               10  EMPNO                         PIC 9(5).              AUDCOMM
