                                                                        NA200C01
       02  NA-COMMAREA.                                                 NA200C01
           05  NA-CICS-COMMAREA-LENGTH COMP PIC S9(4) VALUE +600.       NA200C01
                                                                        NA200C01
 ===> * COMMAND LINE CONTENTS                                           NA200C01
           05  COMM-SYSTEM-CODE        PIC X(02).                       NA200C01
           05  COMM-ACTION-CODE        PIC X(05).                       NA200C01
           05  COMM-CUSTOMER-INFO      PIC X(40).                       NA200C01
           05  COMM-CUST-TOKEN-COUNT   PIC S9.                          NA200C01
           05  COMM-CUSTOMER-INFO1     PIC X(40).                       NA200C01
           05  COMM-CUSTOMER-INFO2     PIC X(30).                       NA200C01
           05  COMM-CUSTOMER-INFO3     PIC X(30).                       NA200C01
           05  COMM-CUSTOMER-INFO4     PIC X(30).                       NA200C01
           05  COMM-CUSTOMER-INFO5     PIC X(30).                       NA200C01
           05  COMM-CUSTOMER-INFO6     PIC X(30).                       NA200C01
                                                                        NA200C01
           05  FILLER                  PIC X(38).                       NA200C01
                                                                        NA200C01
 ===> * INTER-PROGRAM CONTROL ITEMS (BETWEEN PROGRAMS)                  NA200C01
           05  COMM-PREV-IDNTITY       PIC X(8).                        NA200C01
           05  COMM-PREVIOUS-TRANID    PIC X(4).                        NA200C01
           05  COMM-CMDLINE-CHANGED    PIC X(1).                        NA200C01
           05  COMM-KEY-CHANGED        PIC X(1).                        NA200C01
           05  COMM-IDNTITY            PIC X(8).                        NA200C01
           05  COMM-MAIL-LINE-COUNT    PIC 9.                           NA200C01
           05  COMM-MAIL-LINE          PIC X(30) OCCURS 5 TIMES.        NA200C01
           05  COMM-LAST-ITEM-NUM-BROWSED PIC S9(3) COMP-3.             NA200C01
           05  COMM-CUST-TYPE          PIC X(02).                       NA200C01
           05  COMM-CUST-STATUS        PIC X(01).                       NA200C01
           05  COMM-CUST-PHONE.                                         NA200C01
               10  COMM-CUST-PHONE-AREA  PIC X(03).                     NA200C01
               10  COMM-CUST-PHONE-EXCH  PIC X(03).                     NA200C01
               10  COMM-CUST-PHONE-NUMB  PIC X(04).                     NA200C01
           05  COMM-COMBINE-ID1        PIC X(8).                        NA200C01
           05  COMM-COMBINE-ID2        PIC X(8).                        NA200C01
           05  COMM-EXTERNAL-CUST      PIC X.                           NA200C01
                                                                        NA200C01
      *****05  FILLER                  PIC X(8).                        NA200C01
           05  COMM-WEB-LOGON-ID       PIC X(8).                        NA200C01
                                                                        NA200C01
           05  COMM-MSG-COUNT          PIC 9.                           NA200C01
           05  COMM-MSG-ID             PIC X(5) OCCURS 4 TIMES.         NA200C01
           05  COMM-MSG-MAX-SEVERITY   PIC X(1).                        NA200C01
           05  COMM-NEXT-FUNCTION      PIC X(2).                        NA200C01
           05  COMM-CURSOR-POSN        PIC S9(4) COMP.                  NA200C01
                                                                        NA200C01
      *    05  FILLER                  PIC X(83).                       NA200C01
           05  COMM-WEB-DEBUG-IND      PIC X(01).                       NA200C01
      *    05  FILLER                  PIC X(82).                       NA200C01
           05  COMM-CONVERSION-IND     PIC X(1).                        NA200C01
      *****05  FILLER                  PIC X(81).                       NA200C01
           05  NA200C01-COMM-UNIQUE-NUM  PIC X(06).                     NA200C01
           05  NA200C01-COMM-EFFECTIVE-DATE     PIC X(10).              NA200C01
      *****05  FILLER                  PIC X(65).                       NA200C01
           05  COMM-ON-EXCH-TYPE       PIC X(1).                        NA200C01
               88  COMM-ON-EXCH-NEW             VALUE 'E'.              NA200C01
               88  COMM-ON-EXCH-CHG             VALUE 'C'.              NA200C01
               88  COMM-ON-EXCH-RENEW           VALUE 'R'.              NA200C01
           05  FILLER                  PIC X(64).                       NA200C01
                                                                        NA200C01
      *---------------------------------------------------------------* NA200C01
      *  END OF COPYBOOK: NA200C01                                    * NA200C01
      *---------------------------------------------------------------* NA200C01
