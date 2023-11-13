           02  NAME-AND-ADDRESS-COMMAREA.                               NA200C02
                                                                        NA200C02
           05  NA-COMMAREA-LENGTH         PIC S9(4) COMP VALUE +1000.   NA200C02
           05  FILLER                     PIC X(2).                     NA200C02
           05  NA-COMM-VIEW-NUMBER        PIC 9(2)       VALUE 00.      NA200C02
           05  NA-COMM-REQUEST-CODE       PIC X(2).                     NA200C02
                                                                        NA200C02
 ===> * N/A  COMMANDS                                                   NA200C02
 ===> *   WITH THE EXCEPTION OF READ, THESE ARE NOT SUPPORTED PRESENTLY.NA200C02
               88  NA-READ-AGNTNAME-RECORD               VALUES 'RB'    NA200C02
                                                                'R '.   NA200C02
               88  NA-READ-RECORD                        VALUE  'R '.   NA200C02
               88  NA-READ-CASENAME-RECORD               VALUE  'RC'.   NA200C02
                                                                        NA200C02
      *******  88  NA-ADD-RECORD                         VALUE 'A '.    NA200C02
      *******  88  NA-CHG-RECORD                         VALUE 'C '.    NA200C02
      *******  88  NA-DEL-RECORD                         VALUE 'D '.    NA200C02
      *******  88  NA-READ-FOR-UPDATE                    VALUE 'RU'.    NA200C02
      *******  88  NA-RELEASE-RECORD                     VALUE 'RR'.    NA200C02
                                                                        NA200C02
 ===> * XREF COMMANDS (WERE REMOVED ENTIRELY 8/88)                      NA200C02
                                                                        NA200C02
 ===> * MISC COMMANDS                                                   NA200C02
               88  NA-FIND-BROKER-XREF-READ-NA           VALUE 'BN'.    NA200C02
               88  NA-FORMAT-NAME-ADDR                   VALUE 'FM'.    NA200C02
                                                                        NA200C02
               88  NA-OPEN-VSAM-FILES                    VALUE 'O1'.    NA200C02
               88  NA-CLOSE-VSAM-FILES                   VALUE 'C1'.    NA200C02
                                                                        NA200C02
           05  NA-COMM-BROKER-MODE             PIC X(1)  VALUE '0'.     NA200C02
               88  NA-NO-BROKER-ACCESS                   VALUE '0'.     NA200C02
               88  NA-OPEN-BROKER-INPUT-ONLY             VALUE '1'.     NA200C02
               88  NA-OPEN-BROKER-INPUT-OUTPUT           VALUE '2'.     NA200C02
           05  NA-COMM-NAFILE-MODE             PIC X(1)  VALUE '0'.     NA200C02
               88  NA-NO-NAFILE-ACCESS                   VALUE '0'.     NA200C02
               88  NA-OPEN-NAFILE-INPUT-ONLY             VALUE '1'.     NA200C02
               88  NA-OPEN-NAFILE-INPUT-OUTPUT           VALUE '2'.     NA200C02
           05  NA-COMM-NAXREF-MODE             PIC X(1)  VALUE '0'.     NA200C02
               88  NA-NO-NAXREF-ACCESS                   VALUE '0'.     NA200C02
               88  NA-OPEN-NAXREF-INPUT-ONLY             VALUE '1'.     NA200C02
               88  NA-OPEN-NAXREF-INPUT-OUTPUT           VALUE '2'.     NA200C02
           05  NA-COMM-RESERVED-FUTURE-MODES   PIC X(25) VALUE ALL '0'. NA200C02
                                                                        NA200C02
                                                                        NA200C02
FILLER     05  FILLER                          PIC X(28).               NA200C02
                                                                        NA200C02
 ===> * RETURN CODES                                                    NA200C02
           05  NA-COMM-RETURN-CODE             PIC  9(4).               NA200C02
               88  NA-SUCCESSFUL                         VALUE 0.       NA200C02
               88  NA-NOT-OPEN                           VALUE 02.      NA200C02
               88  NA-DUP-REC                            VALUE 04.      NA200C02
               88  NA-NOT-FOUND                          VALUE 06.      NA200C02
               88  NA-END-OF-FILE                        VALUE 08.      NA200C02
               88  NA-OUT-FOR-UPDATE                     VALUE 10.      NA200C02
               88  NA-ILLOGIC                            VALUE 12.      NA200C02
               88  NA-NULL-FIELD                         VALUE 14.      NA200C02
               88  NA-DB2-ATTACH-INACTIVE                VALUE 94.      NA200C02
               88  NA-SQL-FAILED                         VALUE 96.      NA200C02
               88  NA-INVALID-REQUEST                    VALUE 98.      NA200C02
               88  NA-MISC-CICS-ERROR                    VALUE 99.      NA200C02
           05  NA-COMM-SQLCA.                                           NA200C02
               10  NA-COMM-SQLCODE             PIC S9(9).               NA200C02
               10  NA-COMM-SQLERRD3            PIC S9(9).               NA200C02
               10  NA-COMM-SQLWARN0            PIC X.                   NA200C02
               10  NA-COMM-SQLWARN1            PIC X.                   NA200C02
               10  NA-COMM-SQLWARN2            PIC X.                   NA200C02
               10  NA-COMM-SQLWARN3            PIC X.                   NA200C02
               10  NA-COMM-SQLWARN4            PIC X.                   NA200C02
               10  NA-COMM-SQLWARN5            PIC X.                   NA200C02
           05  NA-COMM-CICS-RESPONSE           PIC S9(9) COMP.          NA200C02
           05  NA-COMM-FINALIST-REASON         PIC X(3).                NA200C02
                                                                        NA200C02
FILLER     05  FILLER                          PIC X(23).               NA200C02
                                                                        NA200C02
 ===> * NAME/ADDR DATA                                                  NA200C02
           05  NA-COMM-RECORD-NUMBER           PIC X(8).                NA200C02
           05  NA-COMM-SEARCH-FIELD            PIC X(30).               NA200C02
           05  NA-COMM-PERSONAL-NAME.                                   NA200C02
               10  NA-COMM-TITLE               PIC X(5).                NA200C02
               10  NA-COMM-FIRST-NAME          PIC X(15).               NA200C02
               10  NA-COMM-MIDDLE-NAME         PIC X(15).               NA200C02
               10  NA-COMM-LAST-NAME           PIC X(20).               NA200C02
               10  NA-COMM-SUFFIX1             PIC X(4).                NA200C02
               10  NA-COMM-SUFFIX2             PIC X(4).                NA200C02
           05  NA-COMM-COMPANY-NAME            PIC X(30).               NA200C02
           05  NA-COMM-COMPANY-MAIL            PIC X(1).                NA200C02
           05  NA-COMM-BIRTH-MONTH             PIC 9(2).                NA200C02
           05  NA-COMM-SSN.                                             NA200C02
               10  NA-COMM-SSN-3               PIC 9(3).                NA200C02
               10  NA-COMM-SSN-2               PIC 9(2).                NA200C02
               10  NA-COMM-SSN-4               PIC 9(4).                NA200C02
                                                                        NA200C02
           05  NA-COMM-ADDRESS-1               PIC X(30).               NA200C02
           05  NA-COMM-ADDRESS-2               PIC X(30).               NA200C02
           05  NA-COMM-ADDRESS-3               PIC X(30).               NA200C02
           05  NA-COMM-CITY                    PIC X(16).               NA200C02
           05  NA-COMM-STATE                   PIC X(2).                NA200C02
           05  NA-COMM-ZIP-CODE.                                        NA200C02
               10  NA-COMM-ZIP                 PIC 9(5).                NA200C02
               10  NA-COMM-ZIP-PLUS4           PIC 9(4).                NA200C02
           05  NA-COMM-COUNTY-CODE             PIC 9(5).                NA200C02
           05  NA-COMM-ASSOC-CODE              PIC X(5).                NA200C02
           05  NA-COMM-SITE-CODE               PIC 9(2).                NA200C02
                                                                        NA200C02
           05  NA-COMM-PHONE-NUMBER.                                    NA200C02
               10  NA-COMM-PHONE-AREA          PIC 9(3).                NA200C02
               10  NA-COMM-PHONE-3             PIC 9(3).                NA200C02
               10  NA-COMM-PHONE-4             PIC 9(4).                NA200C02
                                                                        NA200C02
           05  NA-COMM-POSTAL-CODE             PIC X(6).                NA200C02
                                                                        NA200C02
           05  NA-COMM-SEX                     PIC X(01).               NA200C02
FILLER     05  FILLER                          PIC X(23).               NA200C02
                                                                        NA200C02
 ===> * STATUS DATA                                                     NA200C02
           05  NA-COMM-CREATION-DATE.                                   NA200C02
               10  NA-COMM-CREATION-YY         PIC 9(2).                NA200C02
               10  NA-COMM-CREATION-MM         PIC 9(2).                NA200C02
               10  NA-COMM-CREATION-DD         PIC 9(2).                NA200C02
           05  NA-COMM-EFFECTIVE-DATE.                                  NA200C02
               10  NA-COMM-EFFECTIVE-YY        PIC 9(2).                NA200C02
               10  NA-COMM-EFFECTIVE-MM        PIC 9(2).                NA200C02
               10  NA-COMM-EFFECTIVE-DD        PIC 9(2).                NA200C02
           05  NA-COMM-LAST-CHG-DATE.                                   NA200C02
               10  NA-COMM-LAST-CHG-YY         PIC 9(2).                NA200C02
               10  NA-COMM-LAST-CHG-MM         PIC 9(2).                NA200C02
               10  NA-COMM-LAST-CHG-DD         PIC 9(2).                NA200C02
                                                                        NA200C02
           05  NA-COMM-STATUS.                                          NA200C02
               10  NA-COMM-SYSTEM-TYPE         PIC X(2).                NA200C02
               10  NA-COMM-RECORD-STATUS       PIC X.                   NA200C02
               10  NA-COMM-MAIL-STATUS         PIC X.                   NA200C02
               10  NA-COMM-ADDRESS-TYPE        PIC X.                   NA200C02
               10  NA-COMM-RECORD-ORIGIN       PIC X.                   NA200C02
               10  NA-COMM-REASON              PIC 9(3).                NA200C02
                                                                        NA200C02
           05  NA-COMM-COUNTY-NAME             PIC X(25).
           05  NA-COMM-COUNTY-NUM              PIC X(03).

FILLER     05  FILLER                          PIC X(38).
                                                                        NA200C02
 ===> * XREF REC DATA                                                   NA200C02
           05  NA-COMM-RELATED-FILE-ID         PIC X(3).                NA200C02
           05  NA-COMM-RELATED-VERSION-NUMBER  PIC 9(5).                NA200C02
           05  NA-COMM-RELATED-FILE-KEY        PIC X(24).               NA200C02
           05  NA-COMM-RELATED-TIME-STAMP      PIC 9(6).                NA200C02
           05  NA-COMM-RELATED-CREATION-DATE   PIC 9(6).                NA200C02
           05  NA-COMM-RELATED-EFFECTIVE-DATE  PIC 9(6).                NA200C02
           05  NA-COMM-RELATED-CHANGE-DATE     PIC 9(6).                NA200C02
           05  NA-COMM-RELATED-TERM-DATE       PIC 9(6).                NA200C02
           05  NA-COMM-RELATED-RELATION-TYPE   PIC X(3).                NA200C02
                                                                        NA200C02
FILLER     05  FILLER                          PIC X(40).               NA200C02
                                                                        NA200C02
 ===> * FORMATTED LINE                                                  NA200C02
           05  NA-COMM-FORMAT-SWITCH           PIC X VALUE 'Y'.         NA200C02
               88  NA-FORMAT-MAIL-LINES              VALUE 'Y'.         NA200C02
               88  NA-IGNORE-MAIL-LINES              VALUE 'N'.         NA200C02
           05  NA-COMM-CASE-SWITCH             PIC X VALUE 'N'.         NA200C02
               88  NA-MIXED-CASE                     VALUE 'Y'.         NA200C02
               88  NA-UPPER-CASE                     VALUE 'N'.         NA200C02
           05  NA-COMM-MAIL-AREA.                                       NA200C02
               10  NA-COMM-NUMBER-OF-LINES     PIC S9(2).               NA200C02
               10  NA-COMM-LINES-AREA.                                  NA200C02
                   15  NA-COMM-MAIL-LINE1      PIC X(30)  VALUE SPACE.  NA200C02
                   15  NA-COMM-MAIL-LINE2      PIC X(30)  VALUE SPACE.  NA200C02
                   15  NA-COMM-MAIL-LINE3      PIC X(30)  VALUE SPACE.  NA200C02
                   15  NA-COMM-MAIL-LINE4      PIC X(30)  VALUE SPACE.  NA200C02
                   15  NA-COMM-MAIL-LINE5      PIC X(30)  VALUE SPACE.  NA200C02
                   15  NA-COMM-MAIL-LINE6      PIC X(30)  VALUE SPACE.  NA200C02
                   15  NA-COMM-MAIL-LENGTH1    PIC S9(2)  VALUE +30.    NA200C02
                   15  NA-COMM-MAIL-LENGTH2    PIC S9(2)  VALUE +30.    NA200C02
                   15  NA-COMM-MAIL-LENGTH3    PIC S9(2)  VALUE +30.    NA200C02
                   15  NA-COMM-MAIL-LENGTH4    PIC S9(2)  VALUE +30.    NA200C02
                   15  NA-COMM-MAIL-LENGTH5    PIC S9(2)  VALUE +30.    NA200C02
                   15  NA-COMM-MAIL-LENGTH6    PIC S9(2)  VALUE +30.    NA200C02
               10  FILLER   REDEFINES NA-COMM-LINES-AREA.               NA200C02
                   15  NA-COMM-LINE   PIC X(30)        OCCURS 6 TIMES   NA200C02
                       INDEXED BY NA-COMM-LINE-IDX.                     NA200C02
                   15  NA-COMM-LENGTH PIC S9(2)        OCCURS 6 TIMES   NA200C02
                       INDEXED BY NA-COMM-LENGTH-IDX.                   NA200C02
           05  NA-COMM-DISPLAY-NAME            PIC X(30).               NA200C02
                                                                        NA200C02
FILLER     05  FILLER                          PIC X(142).              NA200C02
