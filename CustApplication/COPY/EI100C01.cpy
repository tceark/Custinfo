      ******************************************************************EI100C01
      * EI100C01                                                       *EI100C01
      *            EMPLOYEE INFORMATION SYSTEM COPYBOOK LAYOUTS        *EI100C01
      *                      500 CHARACTER RECORD                      *EI100C01
      ******************************************************************EI100C01
       01  WR-EMPLOYEE-RECORD.                                          EI100C01
           05  WR-EMP-KEY.                                              EI100C01
               10  WR-EMP-SSN                    PIC 9(09).             EI100C01
               10  WR-EMP-SSNX REDEFINES WR-EMP-SSN PIC X(9).           EI100C01
           05  WR-EMP-DATA.                                             EI100C01
               10  WR-EMP-NAME                   PIC X(30).             EI100C01
               10  WR-EMP-ADDRESS.                                      EI100C01
                   15  WR-EMP-STREET             PIC X(30).             EI100C01
                   15  WR-EMP-CITY               PIC X(20).             EI100C01
                   15  WR-EMP-STATE              PIC X(02).             EI100C01
                   15  WR-EMP-ZIP                PIC X(05).             EI100C01
               10  WR-EMP-HOME-PHONE.                                   EI100C01
                   15  WR-EMP-AREA-CODE          PIC X(03).             EI100C01
                   15  WR-EMP-NUMBER             PIC X(07).             EI100C01
               10  WR-EMP-WORK-INFO.                                    EI100C01
                   15  WR-EMP-DIVISION           PIC X(02).             EI100C01
                   15  WR-EMP-BRANCH             PIC X(02).             EI100C01
                   15  WR-EMP-DEPARTMENT         PIC X(02).             EI100C01
                   15  WR-EMP-SECTION            PIC X(02).             EI100C01
                   15  WR-EMP-CORP               PIC X(03).             EI100C01
                   15  FILLER                    PIC X(01).             EI100C01
                   15  WR-EMP-BUILDING           PIC X(01).             EI100C01
                   15  WR-EMP-FLOOR              PIC X(01).             EI100C01
                   15  WR-EMP-PSINUM             PIC X(07).             EI100C01
                   15  WR-EMP-OPID               PIC X(03).             EI100C01
                   15  WR-EMP-SECURITY-CODE      PIC X(03).             EI100C01
                   15  FILLER REDEFINES WR-EMP-SECURITY-CODE.           EI100C01
                       20  WR-EMP-SITE-CODE      PIC X(02).             EI100C01
                       20  FILLER                PIC X(01).             EI100C01
                   15  WR-EMP-CICS-SECURITY      PIC X(24).             EI100C01
                   15  WR-EMP-TERMINAL-ID        PIC X(04).             EI100C01
                   15  WR-EMP-PRINTER-ID         PIC X(04).             EI100C01
           05  WR-EMP-KEYAREAS.                                         EI100C01
               10  WR-EMP-ROLM-INFO.                                    EI100C01
                   15  WR-EMP-EXTENSION          PIC X(04).             EI100C01
                   15  WR-EMP-PRIVLINE           PIC X(04).             EI100C01
                   15  WR-EMP-CLASS-OF-SERVICE.                         EI100C01
                       20  FILLER                PIC X(01).             EI100C01
                       20  WR-EMP-CLASS-OF-SERV  PIC X(01).             EI100C01
               10  WR-EMP-AUTOMOBILE1.                                  EI100C01
                   15  WR-EMP-MAKE1              PIC X(08).             EI100C01
                   15  WR-EMP-MODEL1             PIC X(08).             EI100C01
                   15  WR-EMP-COLOR1             PIC X(06).             EI100C01
                   15  WR-EMP-LICENSE1           PIC X(08).             EI100C01
                   15  WR-EMP-AUTO-STATE1        PIC X(02).             EI100C01
                   15  WR-EMP-YEAR1              PIC X(02).             EI100C01
               10  WR-EMP-AUTOMOBILE2.                                  EI100C01
                   15  WR-EMP-MAKE2              PIC X(08).             EI100C01
                   15  WR-EMP-MODEL2             PIC X(08).             EI100C01
                   15  WR-EMP-COLOR2             PIC X(06).             EI100C01
                   15  WR-EMP-LICENSE2           PIC X(08).             EI100C01
                   15  WR-EMP-AUTO-STATE2        PIC X(02).             EI100C01
                   15  WR-EMP-YEAR2              PIC X(02).             EI100C01
           05  WR-EMP-KEY1 REDEFINES WR-EMP-KEYAREAS.                   EI100C01
               10 WR-EMP-EXTKEY                  PIC X(35).             EI100C01
               10 FILLER                         PIC X(43).             EI100C01
           05  WR-EMP-KEY2 REDEFINES WR-EMP-KEYAREAS.                   EI100C01
               10 FILLER                         PIC X(32).             EI100C01
               10 WR-EMP-LICKEY                  PIC X(35).             EI100C01
               10 FILLER                         PIC X(11).             EI100C01
           05  WR-EMP-OTHER-AREAS.                                      EI100C01
               10  WR-EMP-PROJECT-INFO.                                 EI100C01
                   15  WR-EMP-PROJ-MEM-NUM       PIC X(03).             EI100C01
                   15  WR-EMP-PROJ-MGR-ASSIGNED  PIC X(03).             EI100C01
                   15  WR-EMP-ACTIVE-RSAS        PIC S9(05) COMP-3      EI100C01
                                                            VALUE ZERO. EI100C01
                   15  WR-EMP-PENDING-RSAS       PIC S9(05) COMP-3      EI100C01
                                                            VALUE ZERO. EI100C01
                   15  WR-EMP-HELD-RSAS          PIC S9(05) COMP-3      EI100C01
                                                            VALUE ZERO. EI100C01
                   15  WR-EMP-RESEARCH-RSAS      PIC S9(05) COMP-3      EI100C01
                                                            VALUE ZERO. EI100C01
                   15  WR-EMP-CANCEL-RSAS        PIC S9(05) COMP-3      EI100C01
                                                            VALUE ZERO. EI100C01
                   15  WR-EMP-WITHDRAWN-RSAS     PIC S9(05) COMP-3      EI100C01
                                                            VALUE ZERO. EI100C01
                   15  WR-EMP-COMPLETED-RSAS     PIC S9(05) COMP-3      EI100C01
                                                            VALUE ZERO. EI100C01
               10 FILLER                         PIC X(07).             EI100C01
               10  WR-EMP-OTHER-INFO.                                   EI100C01
                   15  WR-EMP-START-DATE.                               EI100C01
                       20  WR-EMP-START-MONTH    PIC X(02).             EI100C01
                       20  WR-EMP-START-DAY      PIC X(02).             EI100C01
                       20  WR-EMP-START-YEAR     PIC X(02).             EI100C01
                   15  WR-EMP-TERMINATION-DATE.                         EI100C01
                       20  WR-EMP-TERM-MONTH     PIC X(02).             EI100C01
                       20  WR-EMP-TERM-DAY       PIC X(02).             EI100C01
                       20  WR-EMP-TERM-YEAR      PIC X(02).             EI100C01
                   15  WR-EMP-TERMINATION-CODE   PIC X.                 EI100C01
                       88  DIRECTORY-NON-DISPLAY-CODE VALUES ARE        EI100C01
                           'T' 'D' 'Z' 'A' 'B' 'C'.                     EI100C01
                       88  DEPT-POINTER-REC  VALUE 'Z'.                 EI100C01
                   15  WR-EMP-EMERGENCY-PHONE    PIC X(10).             EI100C01
                   15  WR-EMP-CONTACT-DBDS       PIC X(8).              EI100C01
                   15  FILLER                    PIC X(12).             EI100C01
                   15  WR-EMP-MARTIAL-STATUS     PIC X.                 EI100C01
                   15  WR-EMP-NUM-OF-DEPENDENTS  PIC X(2).              EI100C01
                   15  WR-EMP-POSITION           PIC X(01).             EI100C01
                   15  WR-EMP-CATEGORY           PIC X(03).             EI100C01
                   15  WR-EMP-MAILSTOP           PIC X(03).             EI100C01
               10  WR-EMP-LAST-UPDATE.                                  EI100C01
                   15  WR-EMP-UPDT-MONTH         PIC X(02).             EI100C01
                   15  WR-EMP-UPDT-DAY           PIC X(02).             EI100C01
                   15  WR-EMP-UPDT-YEAR          PIC X(02).             EI100C01
               10  WR-EMP-MISCELLANEOUS-FIELDS.                         EI100C01
                   15  WR-EMP-SPEEDNUM           PIC X(05).             EI100C01
                   15  WR-EMP-SPEEDNUMX REDEFINES WR-EMP-SPEEDNUM.      EI100C01
                       20  WR-EMP-SPEED1             PIC X(01).         EI100C01
                       20  WR-EMP-SPEEDNUMBER        PIC X(04).         EI100C01
                   15  WR-EMP-AUXEXT1            PIC X(04).             EI100C01
                   15  WR-EMP-AUXEXT2            PIC X(04).             EI100C01
                   15  WR-EMP-AUXEXT3            PIC X(04).             EI100C01
                   15  WR-EMP-AUXEXT4            PIC X(04).             EI100C01
                   15  WR-EMP-AUXEXT5            PIC X(04).             EI100C01
                   15  WR-EMP-AUXEXT6            PIC X(04).             EI100C01
                   15  WR-EMP-MISC1              PIC X(10).             EI100C01
                   15  WR-EMP-DESC1              PIC X(20).             EI100C01
                   15  WR-EMP-EMERGENCY-CONTACT  REDEFINES              EI100C01
                           WR-EMP-DESC1          PIC X(20).             EI100C01
                   15  WR-EMP-MISC2              PIC X(04).             EI100C01
                   15  WR-EMP-LETTER-RETURNED REDEFINES                 EI100C01
                           WR-EMP-MISC2          PIC X.                 EI100C01
                       88  LETTER-RETURNED           VALUE 'X'.         EI100C01
                   15  WR-EMP-DESC2              PIC X(10).             EI100C01
                   15  WR-EMP-MISC3              PIC X(10).             EI100C01
                   15  WR-EMP-HEADSET            PIC X(03).             EI100C01
                   15  WR-EMP-SPEED-GROUP        PIC X(03).             EI100C01
                   15  WR-EMP-PICK-GROUP         PIC X(03).             EI100C01
                   15  WR-EMP-PHONE-TYPE         PIC X(01).             EI100C01
                   15  WR-EMP-ACD-PILOT          PIC X(04).             EI100C01
                   15  FILLER                    PIC X(06).             EI100C01
                   15  WR-EMP-DESC3.                                    EI100C01
                       20  WR-EMP-MISC4          PIC X(04).             EI100C01
                       20  WR-EMP-DESC4          PIC X(10).             EI100C01
                   15  WR-EMP-MISC5              PIC X(10).             EI100C01
                   15  WR-EMP-LASTNAME-KEY.                             EI100C01
                       20  WR-EMP-DESC5          PIC X(20).             EI100C01
                       20  WR-EMP-MISC6          PIC X(9).              EI100C01
                   15  WR-EMP-SNET               PIC X(1).              EI100C01
                   15  FILLER                    PIC X(7).              EI100C01
