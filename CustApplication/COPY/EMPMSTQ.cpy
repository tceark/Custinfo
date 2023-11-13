IDENT    04  EMPMSTR-REC.                                               EMPMSTQ
00005      05  EMP-DO-NOT-USE11                  PIC X(01).             EMPMSTQ
01200      05  EMP-SECONDARY-KEY-AREA.                                  EMPMSTQ
01205          10  EMP-2NDKEY-LASTNAME           PIC X(16).             EMPMSTQ
01210          10  EMP-2NDKEY-1STINIT            PIC X(01).             EMPMSTQ
01215          10  EMP-2NDKEY-MIDINIT            PIC X(01).             EMPMSTQ
01220          10  EMP-KEY.                                             EMPMSTQ
01225              15  EMP-IDNTY-NUM              PIC X(06).             EMPMSTQ
01230              15  EMP-EMPLOYEE-NUM          PIC 9(05).             EMPMSTQ
00010      05  EMP-DECLARE-IND                   PIC X(01).             EMPMSTQ
01335      05  EMP-MEDICARE-IND                  PIC X(01).             EMPMSTQ
00055      05  EMP-LTD-CLASS                     PIC X(01).             EMPMSTQ
00060      05  EMP-SEX-CODE                      PIC X(01).             EMPMSTQ
00065          88  EMP-MALE                          VALUE 'M' ' '.     EMPMSTQ
00070          88  EMP-FEMALE                        VALUE 'F'.         EMPMSTQ
00075      05  EMP-DEPENDENCY-CODE               PIC X(01).             EMPMSTQ
               88  EMP-DEPENDENCY-OK                 VALUE              EMPMSTQ
                                                      'S' 'F' 'T' 'K'   EMPMSTQ
                                                      '2' 'X' 'D' 'J'   EMPMSTQ
                                                      'C' 'V' 'Y' 'Z'   EMPMSTQ
                                                      'W' 'A' 'B' 'C'   EMPMSTQ
                                                      'L' '0' '1' '3'   EMPMSTQ
                                                      '4' '5' '6' '7'   EMPMSTQ
                                                      '8' 'P'.          EMPMSTQ
               88  EMP-SINGLE                        VALUE 'S'.         EMPMSTQ
               88  EMP-FAMILY                        VALUE 'F'.         EMPMSTQ
               88  EMP-EMPLOYEE-N-SPOUSE             VALUE 'T'.         EMPMSTQ
               88  EMP-EMPLOYEE-N-CHILDREN           VALUE 'K'.         EMPMSTQ
               88  EMP-EMPLOYEE-N-CHILD              VALUE 'J'.         EMPMSTQ
               88  EMP-EMPLOYEE-N-DEPENDENTS         VALUE '2' 'D'.     EMPMSTQ
               88  EMP-MANUALLY-RATED                VALUE 'X' 'C' 'V'  EMPMSTQ
                                                           'Y' 'Z'.     EMPMSTQ
               88  EMP-OLD-DEPENDENCY-CODES          VALUE              EMPMSTQ
                                                      'A' 'B' 'H' 'L'   EMPMSTQ
                                                      '0' '4' '5' '6'   EMPMSTQ
                                                      '7' '8' '1' '3'.  EMPMSTQ
00085      05  EMP-OPTION-CODE                   PIC X(01).             EMPMSTQ
00090      05  EMP-DATE-OF-BIRTH.                                       EMPMSTQ
00095          10  EMP-DOB-MM                    PIC 99.                EMPMSTQ
                   88  EMP-DOB-MM-OK                 VALUE 01 THRU 12.  EMPMSTQ
00100          10  EMP-DOB-MM-X REDEFINES                               EMPMSTQ
                   EMP-DOB-MM                    PIC XX.                EMPMSTQ
00105          10  EMP-DOB-YY                    PIC 99.                EMPMSTQ
00110      05  EMP-DOB REDEFINES                                        EMPMSTQ
                       EMP-DATE-OF-BIRTH         PIC 9(4).              EMPMSTQ
               88  EMP-NO-DOB                        VALUE 0000.        EMPMSTQ
00115      05  EMP-LIFE-VOLUME                   PIC S9(09) COMP-3.     EMPMSTQ
00120      05  EMP-EFFECTIVE-DATE                PIC 9(04).             EMPMSTQ
00125      05  FILLER            REDEFINES  EMP-EFFECTIVE-DATE.         EMPMSTQ
00130          10  EMP-EFF-MM                    PIC X(02).             EMPMSTQ
00135          10  EMP-EFF-YY                    PIC X(02).             EMPMSTQ
00140      05  EMP-EFF-DATE      REDEFINES  EMP-EFFECTIVE-DATE.         EMPMSTQ
00145          10  EMP-EFF-MO                    PIC 9(02).             EMPMSTQ
                   88  EMP-EFF-MO-OK                 VALUE 01 THRU 12.  EMPMSTQ
00150          10  EMP-EFF-YR                    PIC 9(02).             EMPMSTQ
00155      05  EMP-HEALTH                        PIC 9(03)V99.          EMPMSTQ
00160      05  EMP-LIFE                          PIC 9(03)V99.          EMPMSTQ
00165      05  EMP-CODE3                         PIC 9(03)V99.          EMPMSTQ
00170      05  EMP-CODE4                         PIC 9(03)V99.          EMPMSTQ
00175      05  EMP-CODE5                         PIC 9(03)V99.          EMPMSTQ
00180      05  EMP-CODE6                         PIC 9(03)V99.          EMPMSTQ
00185      05  EMP-CERTIFICATE-AREA.                                    EMPMSTQ
00190          10  EMP-CERT-IND                  PIC X(01).             EMPMSTQ
00195          10  EMP-CERT-DATE                 PIC 9(04).             EMPMSTQ
               10  FILLER REDEFINES EMP-CERT-DATE.                      EMPMSTQ
00200              15  EMP-CERT-MO               PIC 9(02).             EMPMSTQ
                      88  EMP-CERT-MO-OK           VALUE 01 THRU 12.    EMPMSTQ
00205              15  EMP-CERT-YR               PIC 9(02).             EMPMSTQ
00210      05  EMP-DATE-ON-OFF-SYSTEM            PIC 9(04).             EMPMSTQ
           05  FILLER            REDEFINES  EMP-DATE-ON-OFF-SYSTEM.     EMPMSTQ
00215          10  EMP-ON-MO                     PIC XX.                EMPMSTQ
                   88  EMP-ON-MO-OK                VALUE '01' THRU '12'.EMPMSTQ
00220          10  EMP-ON-YR                     PIC XX.                EMPMSTQ
00225      05  EMP-TERMINATION-AREA.                                    EMPMSTQ
00230          10  EMP-TERM-CODE                 PIC X(01).             EMPMSTQ
                   88  EMP-TERMINATED                VALUE 'T'.         EMPMSTQ
00235          10  EMP-REVERSAL-CODE             PIC X(01).             EMPMSTQ
                   88  EMP-REVERSAL                  VALUE 'R'.         EMPMSTQ
00240          10  EMP-PREM-FACTOR               PIC 9(01).             EMPMSTQ
                   88  EMP-FACTOR1                   VALUE 1.           EMPMSTQ
                   88  EMP-FACTOR2                   VALUE 2.           EMPMSTQ
00245          10  EMP-PREM-FACTOR-X REDEFINES                          EMPMSTQ
                   EMP-PREM-FACTOR               PIC X(01).             EMPMSTQ
00250          10  EMP-TERMINATION-DATE          PIC 9(6).              EMPMSTQ
00255          10  EMP-TERM-DATE REDEFINES EMP-TERMINATION-DATE.        EMPMSTQ
00260              15  EMP-TERM-MM               PIC 9(02).             EMPMSTQ
                       88  EMP-TERM-MM-OK            VALUE 01 THRU 12.  EMPMSTQ
00265              15  EMP-TERM-MM-X REDEFINES                          EMPMSTQ
                       EMP-TERM-MM               PIC X(02).             EMPMSTQ
00270              15  EMP-TERM-DD               PIC 9(02).             EMPMSTQ
                       88  EMP-TERM-DD-OK            VALUE 01 THRU 31.  EMPMSTQ
00275              15  EMP-TERM-YY               PIC 9(02).             EMPMSTQ
00280      05  EMP-ANNUAL-SALARY                 PIC S9(09) COMP-3.     EMPMSTQ
00285      05  EMP-SPOUSE-DATE-OF-BIRTH.                                EMPMSTQ
00290          10  EMP-SPOUSE-DOB-MM             PIC 9(2).              EMPMSTQ
                   88  EMP-SPOUSE-DOB-MM-OK          VALUE 01 THRU 12.  EMPMSTQ
00295          10  EMP-SPOUSE-DOB-MM-X REDEFINES                        EMPMSTQ
                   EMP-SPOUSE-DOB-MM             PIC X(2).              EMPMSTQ
00300          10  EMP-SPOUSE-DOB-YY             PIC 9(2).              EMPMSTQ
00305      05  EMP-SPOUSE-DOB REDEFINES EMP-SPOUSE-DATE-OF-BIRTH        EMPMSTQ
                                                 PIC 9(4).              EMPMSTQ
00310      05  EMP-SENTRY-IMPAIRMENT-RIDER       PIC X(01).             EMPMSTQ
00315      05  EMP-SHORT-TERM-DISAB-BENEFIT      PIC S9(05) COMP-3.     EMPMSTQ
01390      05  EMP-RATING-AGE                    PIC S9(3)  COMP-3.     EMPMSTQ
01400      05  EMP-SPOUSE-RATING-AGE             PIC S9(3)  COMP-3.     EMPMSTQ
00370      05  EMP-DOB-DD                        PIC 99.                EMPMSTQ
               88  EMP-DOB-DD-OK                 VALUE 00 THRU 31.      EMPMSTQ
00372      05  EMP-WAIVER-DENT-PLAN-EXT          PIC X.                 EMPMSTQ
00375      05  EMP-PCS-TYPE                      PIC S9(3)  COMP-3.     EMPMSTQ
00380      05  EMP-SSNO                          PIC S9(9)  COMP-3.     EMPMSTQ
               88  EMP-NO-SS                         VALUE +000000000.  EMPMSTQ
               88  EMP-SS-OK                         VALUE              EMPMSTQ
                                                     +000000001 THRU    EMPMSTQ
                                                     +999999999.        EMPMSTQ
00385      05  EMP-SSNO-X REDEFINES                                     EMPMSTQ
               EMP-SSNO                          PIC X(5).              EMPMSTQ
00390      05  EMP-MEDRE-AREA1                   PIC X(35).             EMPMSTQ
           05  FILLER  REDEFINES EMP-MEDRE-AREA1.                       EMPMSTQ
01430          10  EMP-HLTH-AREA                 PIC X(02).             EMPMSTQ
01435          10  EMP-NUM-OF-DEPENDENTS         PIC 9(03) COMP-3.      EMPMSTQ
01445          10  EMP-RATING-ZIP-CODE           PIC X(05).             EMPMSTQ
01440          10  EMP-ZIP-PLUS-4                PIC X(04).             EMPMSTQ
01450          10  EMP-HLTH-LEVEL                PIC X(02).             EMPMSTQ
01455          10  EMP-NEXT-HLTH-LEVEL           PIC X(02).             EMPMSTQ
01460          10  EMP-CHANGE-AREA               PIC X(02).             EMPMSTQ
01465          10  EMP-STATE-CODE                PIC X(02).             EMPMSTQ
01467          10  EMP-CEDED-IND                 PIC X(01).             EMPMSTQ
01468          10  EMP-PRODUCT-NUMBER            PIC X(04).             EMPMSTQ
01470          10  EMP-HIRE-DATE.                                       EMPMSTQ
                   15  EMP-HIRE-DATE-YY          PIC 9(02).             EMPMSTQ
                   15  EMP-HIRE-DATE-MM          PIC 9(02).             EMPMSTQ
                   15  EMP-HIRE-DATE-DD          PIC 9(02).             EMPMSTQ
01471          10  EMP-POSITION-TYPE             PIC X(01).             EMPMSTQ
01472          10  EMP-PREVIOUSLY-UNINSURED      PIC X(01).             EMPMSTQ
               10  FILLER                        PIC X(01).             EMPMSTQ
00395      05  EMP-POSITION-CODE                 PIC X(01).             EMPMSTQ
           05  EMP-POSITION-CODE9 REDEFINES EMP-POSITION-CODE PIC 9(01).EMPMSTQ
00400      05  EMP-WAIVER-DEPL-PLAN-EXT          PIC X(01).             EMPMSTQ
00405      05  EMP-CASE-CONTACT                  PIC X(01).             EMPMSTQ
00410      05  EMP-LTD-OCCUPATION-CODE           PIC X(02).             EMPMSTQ
00420      05  EMP-SMOKER-QUESTION               PIC X(01).             EMPMSTQ
00425      05  EMP-REENTRY-DATE                  PIC 9(6).              EMPMSTQ
           05  FILLER REDEFINES EMP-REENTRY-DATE.                       EMPMSTQ
00430          10  EMP-REENTRY-MM                PIC 99.                EMPMSTQ
00435          10  EMP-REENTRY-DD                PIC 99.                EMPMSTQ
00440          10  EMP-REENTRY-YY                PIC 99.                EMPMSTQ
00445      05  EMP-OPTIONAL-LIFE                 PIC S9(09) COMP-3.     EMPMSTQ
01425      05  EMP-OLD-RATING-ZIP-CODE           PIC S9(5) COMP-3.      EMPMSTQ
00446      05  FILLER REDEFINES EMP-OLD-RATING-ZIP-CODE.                EMPMSTQ
00446***** 05  FILLER.                                                  EMPMSTQ
               10  OLD-OCC                       PIC X(02).             EMPMSTQ
               10  MORE-FILLER                   PIC X(01).             EMPMSTQ
00447      05  EMP-MEDRE-AREA2                   PIC X(20).             EMPMSTQ
           05  FILLER REDEFINES EMP-MEDRE-AREA2.                        EMPMSTQ
               10  EMP-LTD-VOLUME                PIC S9(09) COMP-3.     EMPMSTQ
               10  EMP-SALARY-MODE               PIC X(02).             EMPMSTQ
               10  EMP-WAIVER-DENT-EFF-DD        PIC 9(02).             EMPMSTQ
               10  EMP-WAIVER-DENT-TERM-DD       PIC 9(02).             EMPMSTQ
               10  EMP-DIVISION-NUMBER           PIC X(03).             EMPMSTQ
               10  EMP-DOB-CC                    PIC 9(02).             EMPMSTQ
               10  EMP-SPOUSE-DOB-CC             PIC 9(02).             EMPMSTQ
               10  FILLER                        PIC X(02).             EMPMSTQ
00448      05  EMP-PRODUCT-LINE                  PIC X(03).             EMPMSTQ
           05  EMP-CONT-EFF-DATE                 PIC 9(6).              EMPMSTQ
           05  FILLER REDEFINES EMP-CONT-EFF-DATE.                      EMPMSTQ
               10  EMP-CONT-EFF-MM               PIC 99.                EMPMSTQ
               10  EMP-CONT-EFF-DD               PIC 99.                EMPMSTQ
               10  EMP-CONT-EFF-YY               PIC 99.                EMPMSTQ
00545      05  EMP-WAIVER-HLTH-PLAN              PIC X(02).             EMPMSTQ
00670      05  EMP-WAIVER-DHLTH-PLAN             PIC X(02).             EMPMSTQ
01275      05  EMP-PCS-CARD-COVERAGE-DATE.                              EMPMSTQ
01276          10  EMP-PCS-CARD-COVERAGE-MO      PIC 9(02).             EMPMSTQ
01277          10  EMP-PCS-CARD-COVERAGE-YR      PIC 9(02).             EMPMSTQ
00451      05  EMP-SITE-CODE                     PIC X(02).             EMPMSTQ
00452      05  EMP-PCS-CARD-REQUEST              PIC X(01).             EMPMSTQ
00453      05  EMP-PCS-CARD-DATE                 PIC 9(06).             EMPMSTQ
           05  EMP-ORIG-EFF-DATE REDEFINES                              EMPMSTQ
               EMP-PCS-CARD-DATE.                                       EMPMSTQ
               10  EMP-ORIG-EFF-YY               PIC 9(2).              EMPMSTQ
               10  EMP-ORIG-EFF-MM               PIC 9(2).              EMPMSTQ
               10  EMP-ORIG-EFF-DD               PIC 9(2).              EMPMSTQ
           05  EMP-WOP-EFF-DATE REDEFINES                               EMPMSTQ
R02539         EMP-PCS-CARD-DATE.                                       EMPMSTQ
R02539         10  EMP-WOP-EFF-YY                PIC 9(2).              EMPMSTQ
R02539         10  EMP-WOP-EFF-MM                PIC 9(2).              EMPMSTQ
R02539         10  EMP-WOP-EFF-DD                PIC 9(2).              EMPMSTQ
00455      05  EMP-EFFECTIVE-DATE-WITH-DAY.                             EMPMSTQ
               10  EMP-EFFECTIVE-YY              PIC 9(2).              EMPMSTQ
               10  EMP-EFFECTIVE-MM              PIC 9(2).              EMPMSTQ
               10  EMP-EFFECTIVE-DD              PIC 9(2).              EMPMSTQ
00470      05  EMP-TERMINATION-ORIGIN            PIC X(01).             EMPMSTQ
               88  EMP-VALID-ORIGIN                  VALUE              EMPMSTQ
                                                       'E' 'F' 'G' 'T'  EMPMSTQ
                                                       'M'.             EMPMSTQ
               88  EMP-GREEN-SHEET                   VALUE 'G'.         EMPMSTQ
               88  EMP-TERMINATION-CARD              VALUE 'T'.         EMPMSTQ
               88  EMP-EXPIRED                       VALUE 'E'.         EMPMSTQ
               88  EMP-PSI-FILE                      VALUE 'F'.         EMPMSTQ
               88  EMP-MOVE-POLICY                   VALUE 'M'.         EMPMSTQ
00475      05  EMP-DATE-LAST-UPDATE              PIC 9(06).             EMPMSTQ
           05  FILLER REDEFINES EMP-DATE-LAST-UPDATE.                   EMPMSTQ
00480          10  EMP-DATE-LAST-UPDATE-MM       PIC 9(2).              EMPMSTQ
00485          10  EMP-DATE-LAST-UPDATE-MM-X REDEFINES                  EMPMSTQ
                   EMP-DATE-LAST-UPDATE-MM       PIC X(2).              EMPMSTQ
00490          10  EMP-DATE-LAST-UPDATE-DD       PIC 9(2).              EMPMSTQ
00495          10  EMP-DATE-LAST-UPDATE-YY       PIC 9(2).              EMPMSTQ
00500      05  EMP-NUM-OF-COVEREDS               PIC S9(3) COMP-3.      EMPMSTQ
01466      05  EMP-INDEMNITY-PREPAID-IND         PIC X(01).             EMPMSTQ
00505      05  EMP-PREV-COVER-DEP                PIC X(01).             EMPMSTQ
00510      05  EMP-PREV-COVER-CODE               PIC X(02).             EMPMSTQ
               88  EMP-PREV-COVERS                   VALUE 'W1' 'W3'    EMPMSTQ
                                                      'NL' 'N1' 'M1'    EMPMSTQ
990514*                                               'M2' 'WT'.        EMPMSTQ
990514                                                'M2' 'WT' 'HQ'.   EMPMSTQ
               88  EMP-HLTH-1000-WAIVED              VALUE 'W1'.        EMPMSTQ
               88  EMP-HLTH-2500-WAIVED              VALUE 'W3'.        EMPMSTQ
               88  EMP-HLTH-TOTAL-WAIVED             VALUE 'WT'.        EMPMSTQ
               88  EMP-NLNG                          VALUE 'NL'.        EMPMSTQ
               88  EMP-NLNG-1000-MATERNITY           VALUE 'N1'.        EMPMSTQ
               88  EMP-MATER-1000-WAIVED             VALUE 'M1'.        EMPMSTQ
               88  EMP-MATER-2000-WAIVED             VALUE 'M2'.        EMPMSTQ
00515      05  EMP-PREV-CARR-CODE                PIC X(02).             EMPMSTQ
00520      05  EMP-MEDRE-AREA3                   PIC X(03).             EMPMSTQ
01405      05  EMP-ID-CARD-SENT-DATE             PIC 9(06).             EMPMSTQ
           05  FILLER  REDEFINES EMP-ID-CARD-SENT-DATE.                 EMPMSTQ
               10  EMP-ID-CARD-SENT-MM           PIC 99.                EMPMSTQ
               10  EMP-ID-CARD-SENT-DD           PIC 99.                EMPMSTQ
               10  EMP-ID-CARD-SENT-YY           PIC 99.                EMPMSTQ
00535      05  EMP-COUNTRY-CODE                  PIC X(02).             EMPMSTQ
00540      05  EMP-WAIVER-FIELDS.                                       EMPMSTQ
00550          10  EMP-WAIVER-HLTH-EXCEPTION     PIC X.                 EMPMSTQ
00555          10  EMP-WAIVER-HLTH-EFF-DATE      PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-HLTH-EFF-DATE.           EMPMSTQ
00560              15  EMP-WAIVER-HLTH-EFF-MM    PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-HLTH-EFF-MM-OK  VALUE 01 THRU 12.   EMPMSTQ
00565              15  EMP-WAIVER-HLTH-EFF-YY    PIC 9(2).              EMPMSTQ
00570          10  EMP-WAIVER-HLTH-TERM-DATE     PIC 9(4).              EMPMSTQ
00575          10  FILLER REDEFINES EMP-WAIVER-HLTH-TERM-DATE.          EMPMSTQ
00580              15  EMP-WAIVER-HLTH-TERM-MM   PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-HLTH-TERM-MM-OK VALUE 01 THRU 12.   EMPMSTQ
00585              15  EMP-WAIVER-HLTH-TERM-YY   PIC 9(2).              EMPMSTQ
00590          10  EMP-WAIVER-LIFE-PLAN          PIC X.                 EMPMSTQ
00595          10  EMP-WAIVER-LIFE-EXCEPTION     PIC X.                 EMPMSTQ
00600          10  EMP-WAIVER-LIFE-EFF-DATE      PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-LIFE-EFF-DATE.           EMPMSTQ
00605              15  EMP-WAIVER-LIFE-EFF-MM    PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-LIFE-EFF-MM-OK  VALUE 01 THRU 12.   EMPMSTQ
00610              15  EMP-WAIVER-LIFE-EFF-YY    PIC 9(2).              EMPMSTQ
00615          10  EMP-WAIVER-LIFE-TERM-DATE     PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-LIFE-TERM-DATE.          EMPMSTQ
00620              15  EMP-WAIVER-LIFE-TERM-MM   PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-LIFE-TERM-MM-OK VALUE 01 THRU 12.   EMPMSTQ
00625              15  EMP-WAIVER-LIFE-TERM-YY   PIC 9(2).              EMPMSTQ
00630          10  EMP-WAIVER-MAT-PLAN           PIC X.                 EMPMSTQ
00635          10  EMP-WAIVER-MAT-EXCEPTION      PIC X.                 EMPMSTQ
00640          10  EMP-WAIVER-MAT-EFF-DATE       PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-MAT-EFF-DATE.            EMPMSTQ
00645              15  EMP-WAIVER-MAT-EFF-MM     PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-MAT-EFF-MM-OK VALUE 01 THRU 12.     EMPMSTQ
00650              15  EMP-WAIVER-MAT-EFF-YY     PIC 9(2).              EMPMSTQ
00655          10  EMP-WAIVER-MAT-TERM-DATE      PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-MAT-TERM-DATE.           EMPMSTQ
00660              15  EMP-WAIVER-MAT-TERM-MM    PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-MAT-TERM-MM-OK VALUE 01 THRU 12.    EMPMSTQ
00665              15  EMP-WAIVER-MAT-TERM-YY    PIC 9(2).              EMPMSTQ
00656          10  FILLER                        PIC X.                 EMPMSTQ
00675          10  EMP-WAIVER-DHLTH-EXCEPTION    PIC X.                 EMPMSTQ
00680          10  EMP-WAIVER-DHLTH-EFF-DATE     PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-DHLTH-EFF-DATE.          EMPMSTQ
00685              15  EMP-WAIVER-DHLTH-EFF-MM   PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-DHLTH-EFF-MM-OK VALUE 01 THRU 12.   EMPMSTQ
00690              15  EMP-WAIVER-DHLTH-EFF-YY   PIC 9(2).              EMPMSTQ
00695          10  EMP-WAIVER-DHLTH-TERM-DATE    PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-DHLTH-TERM-DATE.         EMPMSTQ
00700              15  EMP-WAIVER-DHLTH-TERM-MM  PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-DHLTH-TERM-MM-OK VALUE 01 THRU 12.  EMPMSTQ
00705              15  EMP-WAIVER-DHLTH-TERM-YY  PIC 9(2).              EMPMSTQ
00710          10  EMP-WAIVER-DEPL-PLAN          PIC X.                 EMPMSTQ
00715          10  EMP-WAIVER-DEPL-EXCEPTION     PIC X.                 EMPMSTQ
00720          10  EMP-WAIVER-DEPL-EFF-DATE      PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-DEPL-EFF-DATE.           EMPMSTQ
00725              15  EMP-WAIVER-DEPL-EFF-MM    PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-DEPL-EFF-MM-OK  VALUE 01 THRU 12.   EMPMSTQ
00730              15  EMP-WAIVER-DEPL-EFF-YY    PIC 9(2).              EMPMSTQ
00735          10  EMP-WAIVER-DEPL-TERM-DATE     PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-DEPL-TERM-DATE.          EMPMSTQ
00740              15  EMP-WAIVER-DEPL-TERM-MM   PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-DEPL-TERM-MM-OK VALUE 01 THRU 12.   EMPMSTQ
00745              15  EMP-WAIVER-DEPL-TERM-YY   PIC 9(2).              EMPMSTQ
00750          10  EMP-WAIVER-DISAB-PLAN         PIC X.                 EMPMSTQ
00755          10  EMP-WAIVER-DISAB-EXCEPTION    PIC X.                 EMPMSTQ
00760          10  EMP-WAIVER-DISAB-EFF-DATE     PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-DISAB-EFF-DATE.          EMPMSTQ
00765              15  EMP-WAIVER-DISAB-EFF-MM   PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-DISAB-EFF-MM-OK  VALUE 01 THRU 12.  EMPMSTQ
00770              15  EMP-WAIVER-DISAB-EFF-YY   PIC 9(2).              EMPMSTQ
00775          10  EMP-WAIVER-DISAB-TERM-DATE    PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-DISAB-TERM-DATE.         EMPMSTQ
00780              15  EMP-WAIVER-DISAB-TERM-MM  PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-DISAB-TERM-MM-OK VALUE 01 THRU 12.  EMPMSTQ
00785              15  EMP-WAIVER-DISAB-TERM-YY  PIC 9(2).              EMPMSTQ
00790          10  EMP-WAIVER-DENT-PLAN          PIC X.                 EMPMSTQ
00795          10  EMP-WAIVER-DENT-EXCEPTION     PIC X.                 EMPMSTQ
00800          10  EMP-WAIVER-DENT-EFF-DATE      PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-DENT-EFF-DATE.           EMPMSTQ
00805              15  EMP-WAIVER-DENT-EFF-MM    PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-DENT-EFF-MM-OK  VALUE 01 THRU 12.   EMPMSTQ
00810              15  EMP-WAIVER-DENT-EFF-YY    PIC 9(2).              EMPMSTQ
00815          10  EMP-WAIVER-DENT-TERM-DATE     PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-DENT-TERM-DATE.          EMPMSTQ
00820              15  EMP-WAIVER-DENT-TERM-MM   PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-DENT-TERM-MM-OK VALUE 01 THRU 12.   EMPMSTQ
00825              15  EMP-WAIVER-DENT-TERM-YY   PIC 9(2).              EMPMSTQ
00830          10  EMP-WAIVER-PCS-PLAN           PIC X.                 EMPMSTQ
00835          10  EMP-WAIVER-PCS-EXCEPTION      PIC X.                 EMPMSTQ
00840          10  EMP-WAIVER-PCS-EFF-DATE       PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-PCS-EFF-DATE.            EMPMSTQ
00845              15  EMP-WAIVER-PCS-EFF-MM     PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-PCS-EFF-MM-OK   VALUE 01 THRU 12.   EMPMSTQ
00850              15  EMP-WAIVER-PCS-EFF-YY     PIC 9(2).              EMPMSTQ
00855          10  EMP-WAIVER-PCS-TERM-DATE      PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-PCS-TERM-DATE.           EMPMSTQ
00860              15  EMP-WAIVER-PCS-TERM-MM    PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-PCS-TERM-MM-OK  VALUE 01 THRU 12.   EMPMSTQ
00865              15  EMP-WAIVER-PCS-TERM-YY    PIC 9(2).              EMPMSTQ
00870          10  EMP-WAIVER-LTD-PLAN           PIC X.                 EMPMSTQ
00875          10  EMP-WAIVER-LTD-EXCEPTION      PIC X.                 EMPMSTQ
00880          10  EMP-WAIVER-LTD-EFF-DATE       PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-LTD-EFF-DATE.            EMPMSTQ
00885              15  EMP-WAIVER-LTD-EFF-MM     PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-LTD-EFF-MM-OK   VALUE 01 THRU 12.   EMPMSTQ
00890              15  EMP-WAIVER-LTD-EFF-YY     PIC 9(2).              EMPMSTQ
00895          10  EMP-WAIVER-LTD-TERM-DATE      PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-LTD-TERM-DATE.           EMPMSTQ
00900              15  EMP-WAIVER-LTD-TERM-MM    PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-LTD-TERM-MM-OK  VALUE 01 THRU 12.   EMPMSTQ
00905              15  EMP-WAIVER-LTD-TERM-YY    PIC 9(2).              EMPMSTQ
00910          10  EMP-WAIVER-OTHR-PLAN          PIC X.                 EMPMSTQ
00915          10  EMP-WAIVER-OTHR-EXCEPTION     PIC X.                 EMPMSTQ
00920          10  EMP-WAIVER-OTHR-EFF-DATE      PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-OTHR-EFF-DATE.           EMPMSTQ
00925              15  EMP-WAIVER-OTHR-EFF-MM    PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-OTHR-EFF-MM-OK  VALUE 01 THRU 12.   EMPMSTQ
00930              15  EMP-WAIVER-OTHR-EFF-YY    PIC 9(2).              EMPMSTQ
00935          10  EMP-WAIVER-OTHR-TERM-DATE     PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-OTHR-TERM-DATE.          EMPMSTQ
00940              15  EMP-WAIVER-OTHR-TERM-MM   PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-OTHR-TERM-MM-OK VALUE 01 THRU 12.   EMPMSTQ
00945              15  EMP-WAIVER-OTHR-TERM-YY   PIC 9(2).              EMPMSTQ
00950          10  EMP-WAIVER-CD11-PLAN          PIC X.                 EMPMSTQ
00955          10  EMP-WAIVER-CD11-EXCEPTION     PIC X.                 EMPMSTQ
00960          10  EMP-WAIVER-CD11-EFF-DATE      PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-CD11-EFF-DATE.           EMPMSTQ
00965              15  EMP-WAIVER-CD11-EFF-MM    PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-CD11-EFF-MM-OK  VALUE 01 THRU 12.   EMPMSTQ
00970              15  EMP-WAIVER-CD11-EFF-YY    PIC 9(2).              EMPMSTQ
00975          10  EMP-WAIVER-CD11-TERM-DATE     PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-CD11-TERM-DATE.          EMPMSTQ
00980              15  EMP-WAIVER-CD11-TERM-MM   PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-CD11-TERM-MM-OK VALUE 01 THRU 12.   EMPMSTQ
00985              15  EMP-WAIVER-CD11-TERM-YY   PIC 9(2).              EMPMSTQ
00990          10  EMP-WAIVER-CD12-PLAN          PIC X.                 EMPMSTQ
00995          10  EMP-WAIVER-CD12-EXCEPTION     PIC X.                 EMPMSTQ
01000          10  EMP-WAIVER-CD12-EFF-DATE      PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-CD12-EFF-DATE.           EMPMSTQ
01005              15  EMP-WAIVER-CD12-EFF-MM    PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-CD12-EFF-MM-OK  VALUE 01 THRU 12.   EMPMSTQ
01010              15  EMP-WAIVER-CD12-EFF-YY    PIC 9(2).              EMPMSTQ
01015          10  EMP-WAIVER-CD12-TERM-DATE     PIC 9(4).              EMPMSTQ
               10  FILLER REDEFINES EMP-WAIVER-CD12-TERM-DATE.          EMPMSTQ
01020              15  EMP-WAIVER-CD12-TERM-MM   PIC 9(2).              EMPMSTQ
                     88  EMP-WAIVER-CD12-TERM-MM-OK VALUE 01 THRU 12.   EMPMSTQ
01025              15  EMP-WAIVER-CD12-TERM-YY   PIC 9(2).              EMPMSTQ
01030      05  EMP-REINSTATE-AREA.                                      EMPMSTQ
01035          10  EMP-REINSTATE-DATE            PIC 9(06).             EMPMSTQ
01040          10  EMP-REINSTATE-COUNT           PIC 9.                 EMPMSTQ
01041      05  EMP-STAT-DATE.                                           EMPMSTQ
               10  EMP-STAT-MM                   PIC 9(02).             EMPMSTQ
               10  EMP-STAT-YY                   PIC 9(02).             EMPMSTQ
01042      05  EMP-AGE-60-FLAG                   PIC X.                 EMPMSTQ
               88  EMP-AGE-60-COVER-REDUCTION VALUE 'Y'.                EMPMSTQ
01043      05  EMP-AGE-75-FLAG                   PIC X.                 EMPMSTQ
               88  EMP-AGE-75-COVER-REDUCTION VALUE 'Y'.                EMPMSTQ
01235      05  EMP-ADANDD-VOLUME                 PIC S9(09) COMP-3.     EMPMSTQ
01240      05  EMP-AGE-ADJUST-FACTOR             PIC S9(03) COMP-3.     EMPMSTQ
01245      05  EMP-DATE-OF-ENTRY                 PIC S9(6) COMP-3.      EMPMSTQ
01250      05  EMP-CW-CARD-CODE                  PIC X.                 EMPMSTQ
01375      05  EMP-CD12-PREM                     PIC S9(05)V99 COMP-3.  EMPMSTQ
00015      05  EMP-CARRIER                       PIC X(02).             EMPMSTQ
00020      05  EMP-DO-NOT-USE9                   PIC X(01).             EMPMSTQ
00025      05  EMP-NAME-AREA.                                           EMPMSTQ
00030          10  EMP-FIRST-INIT                PIC X(01).             EMPMSTQ
00035          10  FILLER                        PIC X(01).             EMPMSTQ
00040          10  EMP-MID-INIT                  PIC X(01).             EMPMSTQ
00045          10  FILLER                        PIC X(01).             EMPMSTQ
00050          10  EMP-LAST-NAME                 PIC X(16).             EMPMSTQ
           05  EMP-COVERAGE-OVERRIDE-AREA.                              EMPMSTQ
01280          10  EMP-HLTH-OVERRIDE             PIC X.                 EMPMSTQ
01285          10  EMP-LIFE-OVERRIDE             PIC X.                 EMPMSTQ
01290          10  EMP-MAT-OVERRIDE              PIC X.                 EMPMSTQ
01295          10  EMP-DHLTH-OVERRIDE            PIC X.                 EMPMSTQ
01300          10  EMP-DEPL-OVERRIDE             PIC X.                 EMPMSTQ
01305          10  EMP-DISAB-OVERRIDE            PIC X.                 EMPMSTQ
01310          10  EMP-DENT-OVERRIDE             PIC X.                 EMPMSTQ
01315          10  EMP-PCS-OVERRIDE              PIC X.                 EMPMSTQ
01320          10  EMP-LTD-OVERRIDE              PIC X.                 EMPMSTQ
01325          10  EMP-OTHR-OVERRIDE             PIC X.                 EMPMSTQ
01326          10  EMP-24HR-OVERRIDE             PIC X.                 EMPMSTQ
01327          10  EMP-CD11-OVERRIDE             PIC X.                 EMPMSTQ
01328          10  EMP-CD12-OVERRIDE             PIC X.                 EMPMSTQ
01380      05  EMP-CD12-CHANGE-FLAG              PIC X.                 EMPMSTQ
               88  EMP-CD12-CHANGE               VALUE 'Y'.             EMPMSTQ
01330***** 05  FILLER                            PIC X.                 EMPMSTQ
01330      05  EMP-MEDICARE-IND-EXT              PIC X.                 EMPMSTQ
01336      05  EMP-WAIVER-24HR-PLAN              PIC X(2).              EMPMSTQ
01337      05  EMP-WAIVER-24HR-EXCEPTION         PIC X.                 EMPMSTQ
01338      05  EMP-WAIVER-24HR-EFF-DATE          PIC 9(4).              EMPMSTQ
           05  FILLER REDEFINES EMP-WAIVER-24HR-EFF-DATE.               EMPMSTQ
01339          10  EMP-WAIVER-24HR-EFF-MM        PIC 9(2).              EMPMSTQ
                 88  EMP-WAIVER-24HR-EFF-MM-OK   VALUE 01 THRU 12.      EMPMSTQ
01340          10  EMP-WAIVER-24HR-EFF-YY        PIC 9(2).              EMPMSTQ
01341      05  EMP-WAIVER-24HR-TERM-DATE         PIC 9(4).              EMPMSTQ
           05  FILLER REDEFINES EMP-WAIVER-24HR-TERM-DATE.              EMPMSTQ
01342          10  EMP-WAIVER-24HR-TERM-MM       PIC 9(2).              EMPMSTQ
                 88  EMP-WAIVER-24HR-TERM-MM-OK  VALUE 01 THRU 12.      EMPMSTQ
01343          10  EMP-WAIVER-24HR-TERM-YY       PIC 9(2).              EMPMSTQ
01344      05  EMP-24HR-PREM                     PIC S9(05)V99 COMP-3.  EMPMSTQ
01045      05  EMP-MEDRE-AREA5.                                         EMPMSTQ
991207         10  EMP-PCS-KEY.                                         EMPMSTQ
991207             15  EMP-PCS-CARRIER           PIC X(04).             EMPMSTQ
991207             15  EMP-PCS-GROUP.                                   EMPMSTQ
991207                 20  EMP-PCS-GROUP-1       PIC X(02).             EMPMSTQ
991207                 20  EMP-PCS-GROUP-2       PIC X(02).             EMPMSTQ
R02193         10  EMP-WOP-SPACE REDEFINES EMP-PCS-KEY.                 EMPMSTQ
R02193             15  EMP-WOP-TERM-DATE.                               EMPMSTQ
R02539                 20  EMP-WOP-TERM-YY       PIC X(2).              EMPMSTQ
R02539                 20  EMP-WOP-TERM-MM       PIC X(2).              EMPMSTQ
R02539                 20  EMP-WOP-TERM-DD       PIC X(2).              EMPMSTQ
R02193             15  FILLER                    PIC X(2).              EMPMSTQ
08060          10  EMP-BONUS-LIFE-VOLUME         PIC S9(07) COMP-3.     EMPMSTQ
R04203         10  FILLER                        PIC X(01).             EMPMSTQ
R04203         10  EMP-OUTSURG-COVERAGE-IND      PIC X(01).             EMPMSTQ
991207****     10  FILLER                        PIC X(06).             EMPMSTQ
01050      05  EMP-SMFEE-AREA.                                          EMPMSTQ
01055          10  EMP-SMFEE-CREDIT-CODE         PIC X.                 EMPMSTQ
01060          10  EMP-SMFEE-CREDIT-TERM-DATE    PIC 9(6).              EMPMSTQ
01065          10  EMP-SMFEE-CREDIT-OFF-DATE     PIC 9(4).              EMPMSTQ
01070      05  EMP-PREMIUM-BREAKOUT-FLDS.                               EMPMSTQ
      *                                                                 EMPMSTQ
01075         10  EMP-RATE-CHANGE-FLAG          PIC X.                  EMPMSTQ
                  88  EMP-RATE-CHANGED          VALUE 'Y'.              EMPMSTQ
      *                                                                 EMPMSTQ
01080         10  EMP-AGE-CHANGE-FLAG           PIC X.                  EMPMSTQ
                  88  EMP-AGE-CHANGE            VALUE 'Y'.              EMPMSTQ
      *                                                                 EMPMSTQ
01085         10  EMP-AGE-65-FLAG               PIC X.                  EMPMSTQ
                  88  EMP-AGE-65-COVER-REDUCTION VALUE 'Y'.             EMPMSTQ
      *                                                                 EMPMSTQ
01090         10  EMP-AGE-70-FLAG               PIC X.                  EMPMSTQ
                  88  EMP-AGE-70-COVER-REDUCTION VALUE 'Y'.             EMPMSTQ
      *                                                                 EMPMSTQ
01095         10  EMP-X-OR-LARGEGROUP-FLAG      PIC X.                  EMPMSTQ
                  88  EMP-X-DEP-CODE-OR-LARGE-GROUP VALUE 'Y'.          EMPMSTQ
      *                                                                 EMPMSTQ
01100         10  EMP-DATA-ERROR-FLAG           PIC X.                  EMPMSTQ
                  88  EMP-DATA-ERROR            VALUE 'Y'.              EMPMSTQ
      *                                                                 EMPMSTQ
01105         10  EMP-LIFE-PREM                 PIC S9(05)V99 COMP-3.   EMPMSTQ
01110         10  EMP-LIFE-PREMIUM-CHANGE-FLAG  PIC X.                  EMPMSTQ
                  88  EMP-LIFE-PREMIUM-CHANGE   VALUE 'Y'.              EMPMSTQ
      *                                                                 EMPMSTQ
01115         10  EMP-SIN-HLTH-PREM             PIC S9(05)V99 COMP-3.   EMPMSTQ
01120         10  EMP-FAM-HLTH-PREM             PIC S9(05)V99 COMP-3.   EMPMSTQ
01125         10  EMP-HEALTH-CHANGE-FLAG        PIC X.                  EMPMSTQ
                  88  EMP-HEALTH-CHANGE         VALUE 'Y'.              EMPMSTQ
      *                                                                 EMPMSTQ
01130         10  EMP-DISAB-PREM                PIC S9(05)V99 COMP-3.   EMPMSTQ
01135         10  EMP-DISABILITY-CHANGE-FLAG    PIC X.                  EMPMSTQ
                  88  EMP-DISABILITY-PREMIUM-CHANGE VALUE 'Y'.          EMPMSTQ
      *                                                                 EMPMSTQ
01140         10  EMP-DENT-PREM                 PIC S9(05)V99 COMP-3.   EMPMSTQ
01145         10  EMP-DENTAL-CHANGE-FLAG        PIC X.                  EMPMSTQ
                  88  EMP-DENTAL-PREMIUM-CHANGE VALUE 'Y'.              EMPMSTQ
      *                                                                 EMPMSTQ
01150         10  EMP-MAT-PREM                  PIC S9(05)V99 COMP-3.   EMPMSTQ
01155         10  EMP-MATERNITY-CHANGE-FLAG     PIC X.                  EMPMSTQ
                  88  EMP-MATERNITY-PREMIUM-CHANGE VALUE 'Y'.           EMPMSTQ
      *                                                                 EMPMSTQ
01160         10  EMP-PCS-PREM                  PIC S9(05)V99 COMP-3.   EMPMSTQ
01165         10  EMP-PCS-PREMIUM-CHANGE-FLAG   PIC X.                  EMPMSTQ
                  88  EMP-PCS-PREMIUM-CHANGE    VALUE 'Y'.              EMPMSTQ
      *                                                                 EMPMSTQ
01170         10  EMP-LTD-PREM                  PIC S9(05)V99 COMP-3.   EMPMSTQ
01175         10  EMP-LTD-PREMIUM-CHANGE-FLAG   PIC X.                  EMPMSTQ
                  88  EMP-LTD-PREMIUM-CHANGE    VALUE 'Y'.              EMPMSTQ
      *                                                                 EMPMSTQ
01180         10  EMP-DEPLIFE-PREM              PIC S9(05)V99 COMP-3.   EMPMSTQ
01185         10  EMP-DLIFE-PREMIUM-CHANGE-FLAG PIC X.                  EMPMSTQ
                  88  EMP-DEPLIFE-PREMIUM-CHANGE VALUE 'Y'.             EMPMSTQ
      *                                                                 EMPMSTQ
01190         10  EMP-OTHER-PREM                PIC S9(05)V99 COMP-3.   EMPMSTQ
01195         10  EMP-OTHER-PREMIUM-CHANGE-FLAG PIC X.                  EMPMSTQ
                  88  EMP-OTHER-PREMIUM-CHANGE  VALUE 'Y'.              EMPMSTQ
      *                                                                 EMPMSTQ
01255         10  EMP-CD11-PREM                 PIC S9(05)V99 COMP-3.   EMPMSTQ
01260         10  EMP-CD11-CHANGE-FLAG          PIC X.                  EMPMSTQ
                  88  EMP-CD11-CHANGE           VALUE 'Y'.              EMPMSTQ
      *                                                                 EMPMSTQ
01265         10  EMP-TOTAL-PREMIUM             PIC S9(05)V99 COMP-3.   EMPMSTQ
01410      05  EMP-CERT-SENT-DATE.                                      EMPMSTQ
               10  EMP-CERT-SENT-MM             PIC 99.                 EMPMSTQ
               10  EMP-CERT-SENT-DD             PIC 99.                 EMPMSTQ
               10  EMP-CERT-SENT-YY             PIC 99.                 EMPMSTQ
01370      05  EMP-PREMUPD-ERRCODE              PIC X(02).              EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-SIN-HLTH   VALUE 'SH'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-FAM-HLTH   VALUE 'DH'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-LIFE       VALUE 'LF'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-PCS        VALUE 'PC'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-DENTAL     VALUE 'DN'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-DEP-LIFE   VALUE 'DL'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-DISABILITY VALUE 'DS'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-LTD        VALUE 'LT'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-OTHER      VALUE 'XX'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-SIZE1      VALUE 'S1'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-SIZE2      VALUE 'S2'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-SIZE3      VALUE 'S3'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-SIZE4      VALUE 'S4'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-SIZE5      VALUE 'S5'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-SIZE6      VALUE 'S6'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-SIZE7      VALUE 'S7'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-DOB1       VALUE 'D1'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-DOB2       VALUE 'D2'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-DOB3       VALUE 'D3'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-TURBO1     VALUE 'T1'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-TURBO2     VALUE 'T2'.             EMPMSTQ
               88 EMP-PREMUPD-ERR-IS-TURBO3     VALUE 'T3'.             EMPMSTQ
