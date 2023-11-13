       01  WSQ-COMMAREA.                                                00010001
           02  WSQ-COMM-FIELDS.                                         00020001
               05  WSQ-CICS-COMMAREA-LENGTH PIC S9(4) COMP VALUE +600.  00030001
               05  WSQ-SYSTEM-CODE          PIC X(02).                  00040001
               05  WSQ-ACTION-CODE          PIC X(05).                  00050001
               05  WSQ-CUSTOMER-INFO        PIC X(40).                  00060001
               05  WSQ-CUST-TOKEN-COUNT     PIC S9.                     00070001
               05  WSQ-CUSTOMER-INFO1       PIC X(40).                  00080001
               05  WSQ-CUSTOMER-INFO2       PIC X(30).                  00090001
               05  WSQ-CUSTOMER-INFO3       PIC X(30).                  00100001
               05  WSQ-CUSTOMER-INFO4       PIC X(30).                  00110001
               05  WSQ-CUSTOMER-INFO5       PIC X(30).                  00120001
               05  WSQ-CUSTOMER-INFO6       PIC X(30).                  00130001
               05  FILLER                   PIC X(46).                  00140001
               05  WSQ-PREVIOUS-TRANID      PIC X(4).                   00150001
               05  WSQ-CMDLINE-CHANGED      PIC X(1).                   00160001
               05  WSQ-KEY-CHANGED          PIC X(1).                   00170001
               05  WSQ-CIM-NUMBER           PIC X(8).                   00180001
               05  WSQ-MAIL-LINE-COUNT      PIC 9.                      00190001
               05  WSQ-MAIL-LINE            PIC X(30) OCCURS 5 TIMES.   00200001
               05  WSQ-LAST-ITEM-NUM-BROWSED PIC S9(3) COMP-3.          00210001
               05  WSQ-CUST-TYPE            PIC X(2).                   00220001
               05  WSQ-CUST-STATUS          PIC X.                      00230001
               05  WSQ-CUST-PHONE.                                      00240001
                   10  WSQ-CUST-PHONE-AREA  PIC X(3).                   00250001
                   10  WSQ-CUST-PHONE-EXCH  PIC X(3).                   00260001
                   10  WSQ-CUST-PHONE-NUMB  PIC X(4).                   00270001
               05  FILLER                   PIC X(25).                  00280001
               05  WSQ-MSG-COUNT            PIC 9.                      00290001
               05  WSQ-MSG-ID               PIC X(5) OCCURS 4 TIMES.    00300001
               05  WSQ-MSG-MAX-SEVERITY     PIC X(1).                   00310001
               05  WSQ-NEXT-FUNCTION        PIC X(2).                   00320001
               05  WSQ-CURSOR-POSN          PIC S9(4) COMP.             00330001
               05  FILLER                   PIC X(83).                  00340001
           02  WSQ-AGNTNAME.                                            00350001
               05 WSQ-CIM                   PIC X(8).                   00360001
               05 WSQ-LAST-NAME             PIC X(20).                  00370001
               05 WSQ-FIRST-NAME            PIC X(15).                  00380001
               05 WSQ-MIDDLE-NAME           PIC X(15).                  00390001
               05 WSQ-PREFIX                PIC X(4).                   00400001
               05 WSQ-SUFFIX1               PIC X(4).                   00410001
               05 WSQ-SUFFIX2               PIC X(4).                   00420001
               05 WSQ-COMPANY-IND           PIC X(1).                   00430001
               05 WSQ-COMPANY-IN-ADDRESS    PIC X(1).                   00440001
               05 WSQ-COMPANY-NAME          PIC X(30).                  00450001
               05 WSQ-DISPLAY-NAME          PIC X(30).                  00460001
               05 WSQ-NICKNAME              PIC X(10).                  00470001
R04023         05 WSQ-ISSUE-STATE           PIC X(02).                  00480001
               05 WSQ-ADDRESS1              PIC X(30).                  00490001
               05 WSQ-ADDRESS2              PIC X(30).                  00500001
               05 WSQ-CITY                  PIC X(30).                  00510001
               05 WSQ-STATE                 PIC X(2).                   00520001
               05 WSQ-ZIP                   PIC X(5).                   00530001
               05 WSQ-ZIP-PLUS4             PIC X(4).                   00540001
               05 WSQ-COUNTY-CODE           PIC X(5).                   00550001
               05 WSQ-AREA-CODE             PIC X(3).                   00560001
               05 WSQ-PHONE                 PIC X(7).                   00570001
               05 WSQ-PHONE-EXTENSION       PIC X(4).                   00580001
               05 WSQ-SSN                   PIC X(9).                   00590001
               05 WSQ-SEX                   PIC X(1).                   00600001
Y2KIMR***      05 WSQ-BIRTH-DATE            PIC S9(6)    COMP-3.        00610001
Y2KIMR         05 WSQ-BIRTH-DATE            PIC S9(8)    COMP-3.        00620001
               05 WSQ-FINALST-REAS-CODE     PIC X(3).                   00630001
               05 WSQ-FINALST-OVRD-IND      PIC X(1).                   00640001
               05 WSQ-DUP-ADDR-OVRD-IND     PIC X(1).                   00650001
               05 WSQ-EFFECTIVE-DATE        PIC S9(6)    COMP-3.        00660001
               05 WSQ-CHANGE-DATE           PIC S9(6)    COMP-3.        00670001
               05 WSQ-CHANGE-LOGON          PIC X(8).                   00680001
               05 WSQ-ENTITY-TYPE           PIC X(2).                   00690001
               05 WSQ-RECORD-STATUS         PIC X(1).                   00700001
               05 WSQ-ALT-ADDRESS-IND       PIC X(1).                   00710001
               05 WSQ-FUTURE-ADDRESS-IND    PIC X(1).                   00720001
               05 WSQ-RECORD-ORIGIN         PIC X(1).                   00730001
               05 WSQ-COMBINED-STATUS       PIC X(2).                   00740001
               05 WSQ-SITE-CODE             PIC X(2).                   00750001
               05 WSQ-NAME-KEY1             PIC X(8).                   00760001
               05 WSQ-NAME-KEY2             PIC X(8).                   00770001
               05 WSQ-NAME-KEY3             PIC X(2).                   00780001
               05 WSQ-ADDRESS-KEY1          PIC X(10).                  00790001
               05 WSQ-ASSOCIATION1          PIC X(5).                   00800001
               05 WSQ-ASSOCIATION2          PIC X(5).                   00810001
               05 WSQ-ASSOCIATION3          PIC X(5).                   00820001
               05 WSQ-ENTITY-LITERAL        PIC X(10).                  00830001
               05 WSQ-CASE-SW               PIC X.                      00840001
               05 WSQ-DISP-IND              PIC X.                      00850001
               05 WSQ-FINALIST-SW           PIC X.                      00860001
900837         05 WSQ-FAX-AREA-CODE         PIC X(3).                   00870001
900837         05 WSQ-FAX-PHONE             PIC X(7).                   00880001
R00700         05 WSQ-EMAIL1                PIC X(50).                  00890001
R04023         05 WSQ-POSTAL-CODE           PIC X(09).                  00900001
R04023         05 WSQ-COUNTRY-CODE          PIC X(03).                  00910001
R04023         05 WSQ-CARRIER-CODE          PIC XX.                     00920001
R04023         05 WSQ-PREV-CARRIER          PIC XX.                     00930001
