           05 WS-CIM-NUMBER               PIC X(8).
                                                                        00005480
           05 LS-TABLE-DATA.
              10 LS-IDNTITY              PIC X(8).
              10 LS-LAST-NAME            PIC X(20).
              10 LS-FIRST-NAME           PIC X(15).
              10 LS-MIDDLE-NAME          PIC X(15).
              10 LS-PREFIX               PIC X(4).
              10 LS-SUFFIX1              PIC X(4).
              10 LS-SUFFIX2              PIC X(4).
              10 LS-COMPANY-IND          PIC X(1).
              10 LS-COMPANY-IN-ADDRESS   PIC X(1).
              10 LS-COMPANY-NAME         PIC X(30).
              10 LS-DISPLAY-NAME         PIC X(30).
              10 LS-NICKNAME             PIC X(10).
              10 LS-ADDRESS1             PIC X(30).
              10 LS-ADDRESS2             PIC X(30).
              10 LS-CITY                 PIC X(30).
              10 LS-STATE                PIC X(2).
              10 LS-ZIP                  PIC X(5).
              10 LS-ZIP-PLUS4            PIC X(4).
              10 LS-COUNTY-CODE          PIC X(5).
              10 LS-AREA-CODE            PIC X(3).
              10 LS-PHONE                PIC X(7).
              10 LS-PHONE-EXTENSION      PIC X(4).
              10 LS-SSN-NUM              PIC X(9).
              10 LS-SEX                  PIC X(1).
              10 LS-BIRTH-DATE           PIC X(10).
              10 LS-FINALST-REAS-CODE    PIC X(3).
              10 LS-FINALST-OVRD-IND     PIC X(1).
              10 LS-DUP-ADDR-OVRD-IND    PIC X(1).
              10 LS-EFFECTIVE-DATE       PIC X(10).
              10 LS-CHANGE-DATE          PIC X(26).
              10 LS-CHANGE-LOGON         PIC X(8).
              10 LS-ENTITY-TYPE          PIC X(2).
              10 LS-RECORD-STATUS        PIC X(1).
              10 LS-ALT-ADDRESS-IND      PIC X(1).
              10 LS-FUTURE-ADDRESS-IND   PIC X(1).
              10 LS-RECORD-ORIGIN        PIC X(1).
              10 LS-COMBINED-STATUS      PIC X(2).
              10 LS-SITE-CODE            PIC X(2).
              10 LS-NAME-KEY1            PIC X(8).
              10 LS-NAME-KEY2            PIC X(8).
              10 LS-NAME-KEY3            PIC X(2).
              10 LS-ADDRESS-KEY1         PIC X(24).
              10 LS-ASSOCIATION1         PIC X(5).
              10 LS-ASSOCIATION2         PIC X(5).
              10 LS-ASSOCIATION3         PIC X(5).
              10 LS-FAX-AREA-CODE        PIC X(3).
              10 LS-FAX-PHONE            PIC X(7).
              10 LS-OFF-PRODUCERID       PIC X(8).
              10 LS-EXTN-AGENCY-NUMBER   PIC X(12).
              10 LS-PRIMRY-AFFL-CODE     PIC X(2).
              10 LS-SERVFEE-AGREE-IND    PIC X(1).
              10 LS-SERVFEE-AGREE-DATE   PIC X(10).
              10 LS-EMAIL1               PIC X(50).
              10 LS-EMAIL2               PIC X(50).
              10 LS-PASS-WORD            PIC X(12).
              10 LS-CELLULAR-AREA-CODE   PIC X(03).
              10 LS-CELLULAR-PHONE       PIC X(07).
              10 LS-INTEREST-CD          PIC X(03).
              10 LS-PRIMRY-BUSINESS-CD   PIC X(04).
              10 LS-PRIMRY-BUS-REVS-DT   PIC X(10).
              10 LS-SELLING-SM-GRP-YR    PIC X(04).
              10 LS-IN-INS-SINCE-YEAR    PIC X(04).
              10 LS-COUNTRY              PIC X(30).
              10 LS-FOREIGN-FLG          PIC X(01).
              10 LS-RANKING-CD           PIC X(02).
              10 LS-DO-NO-UPDATE         PIC X(01).
              10 LS-TIN                  PIC X(09).
              10 LS-DOING-BUS-AS-NAME    PIC X(30).
              10 LS-CERTIFIED-IND        PIC X(01).
              10 LS-EFT-ACCT-NUM         PIC X(18).
              10 LS-TRANSIT-NUM          PIC X(9).
              10 LS-EFT-ACCT-TYPE        PIC X(1).
              10 LS-PAYEE-IND            PIC X(1).
              10 LS-PAYMENT-IND          PIC X(1).
              10 LS-COUNTRY-CODE         PIC X(3).
              10 LS-POSTAL-CODE          PIC X(9).

           05 WS-RECORD-NOTFND            PIC X.                        00004240

           05 WS-SQL-AREA.                                              00003040
              10  WS-SQLCODE             PIC S9(9).                     00003140
              10  WS-SQL-ERROR           PIC X(1).                      00003140
              10  WS-SQL-ERROR-MSG       PIC X(40).                     00003140
