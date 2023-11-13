           02 WS-UPD-CIM-NUMBER           PIC X(8).

           02  COM-AGNTNAME.                                            00005510
               05 COM-CIM                   PIC X(8).
               05 COM-LAST-NAME             PIC X(20).                  00005530
               05 COM-FIRST-NAME            PIC X(15).                  00005540
               05 COM-MIDDLE-NAME           PIC X(15).                  00005550
               05 COM-PREFIX                PIC X(4).                   00005560
               05 COM-SUFFIX1               PIC X(4).                   00005570
               05 COM-SUFFIX2               PIC X(4).                   00005580
               05 COM-COMPANY-IND           PIC X(1).                   00005590
               05 COM-COMPANY-IN-ADDRESS    PIC X(1).                   00005600
               05 COM-COMPANY-NAME          PIC X(30).                  00005610
               05 COM-DISPLAY-NAME          PIC X(30).                  00005620
               05 COM-NICKNAME              PIC X(10).                  00005630
               05 COM-ISSUE-STATE           PIC X(02).
               05 COM-ADDRESS1              PIC X(30).                  00005640
               05 COM-ADDRESS2              PIC X(30).                  00005650
               05 COM-CITY                  PIC X(30).                  00005660
               05 COM-STATE                 PIC X(2).                   00005670
               05 COM-ZIP                   PIC X(5).                   00005680
               05 COM-ZIP-PLUS4             PIC X(4).                   00005690
               05 COM-COUNTY-CODE           PIC X(5).                   00005700
               05 COM-AREA-CODE             PIC X(3).                   00005710
               05 COM-PHONE                 PIC X(7).                   00005720
               05 COM-PHONE-EXTENSION       PIC X(4).                   00005730
               05 COM-SSN                   PIC X(9).                   00005740
               05 COM-SEX                   PIC X(1).                   00005750
      ******   05 COM-BIRTH-DATE            PIC S9(6)    COMP-3.        00005760
               05 COM-BIRTH-DATE            PIC S9(8)    COMP-3.        00005770
               05 COM-FINALST-REAS-CODE     PIC X(3).                   00005780
               05 COM-FINALST-OVRD-IND      PIC X(1).                   00005790
               05 COM-DUP-ADDR-OVRD-IND     PIC X(1).                   00005800
               05 COM-EFFECTIVE-DATE        PIC S9(6)    COMP-3.        00005810
               05 COM-CHANGE-DATE           PIC S9(6)    COMP-3.        00005820
               05 COM-CHANGE-LOGON          PIC X(8).                   00005830
               05 COM-ENTITY-TYPE           PIC X(2).                   00005840
               05 COM-RECORD-STATUS         PIC X(1).                   00005850
               05 COM-ALT-ADDRESS-IND       PIC X(1).                   00005860
               05 COM-FUTURE-ADDRESS-IND    PIC X(1).                   00005870
               05 COM-RECORD-ORIGIN         PIC X(1).                   00005880
               05 COM-COMBINED-STATUS       PIC X(2).                   00005890
               05 COM-SITE-CODE             PIC X(2).                   00005900
               05 COM-NAME-KEY1             PIC X(8).                   00005910
               05 COM-NAME-KEY2             PIC X(8).                   00005920
               05 COM-NAME-KEY3             PIC X(2).                   00005930
               05 COM-ADDRESS-KEY1          PIC X(10).                  00005940
               05 COM-ASSOCIATION1          PIC X(5).                   00005950
               05 COM-ASSOCIATION2          PIC X(5).                   00005960
               05 COM-ASSOCIATION3          PIC X(5).                   00005970
               05 COM-ENTITY-LITERAL        PIC X(10).                  00005980
               05 COM-CASE-SW               PIC X.                      00005990
               05 COM-DISP-IND              PIC X.
               05 COM-FINALIST-SW           PIC X.                      00006000
               05 COM-FAX-AREA-CODE         PIC X(3).                   00006010
               05 COM-FAX-PHONE             PIC X(7).                   00006020
               05 COM-EMAIL1                PIC X(50).                  00006030
               05 COM-POSTAL-CODE           PIC X(09).                  00006040
               05 COM-COUNTRY-CODE          PIC X(03).                  00006050
               05 COM-CARRIER-CODE          PIC XX.
               05 COM-PREV-CARRIER          PIC XX.

           02  WS-EDIT-SW                   PIC X.                      00004240

           02  WS-MESSAGES.                                             00003040
               05  WS-MESSAGE-NUMBER1       PIC X(5)   VALUE SPACE.     00003480
               05  WS-MESSAGE-NUMBER2       PIC X(5)   VALUE SPACE.     00003490
               05  WS-MESSAGE-NUMBER3       PIC X(5)   VALUE SPACE.     00003500
               05  WS-MESSAGE-NUMBER4       PIC X(5)   VALUE SPACE.     00003510
               05  WS-ERR-FIELD1            PIC X(40)  VALUE SPACES.    00003510
               05  WS-ERR-FIELD2            PIC X(40)  VALUE SPACES.    00003510
               05  WS-ERR-FIELD3            PIC X(40)  VALUE SPACES.    00003510
               05  WS-ERR-FIELD4            PIC X(40)  VALUE SPACES.    00003510

           02  WS-UPD-SQL-AREA.                                         00003040
               05  WS-UPD-SQLCODE           PIC S9(9).                  00003140
               05  WS-UPD-SQL-ERROR         PIC X(1).                   00003140
               05  WS-UPD-SQL-ERROR-MSG     PIC X(40).                  00003140
