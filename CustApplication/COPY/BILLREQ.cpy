           02  BRR-REQUEST-RECORDX.                                     BILLREQ
               05  BRR-REQUEST-TYPE          PIC X(2).                  BILLREQ
               05  BRR-CASE-NUMBER           PIC X(6).                  BILLREQ
               05  BRR-REQUEST-TIMESTAMP     PIC X(26).                 BILLREQ
               05  BRR-REQUEST-LOGONID       PIC X(8).                  BILLREQ
               05  BRR-PROCESS-STATUS        PIC X.                     BILLREQ
               05  BRR-PROCESS-TIMESTAMP     PIC X(26).                 BILLREQ
               05  BRR-CHECK-DIGIT           PIC X.                     BILLREQ
               05  BRR-CARRIER-CODE          PIC XX.                    BILLREQ
               05  BRR-REPORT-TYPE           PIC X.                     BILLREQ
               05  BRR-AMOUNT      PIC S999V99 USAGE COMP-3.            BILLREQ
               05  BRR-BILL-PERIOD-1         PIC X(10).                 BILLREQ
               05  BRR-BILL-PERIOD-2         PIC X(10).                 BILLREQ
               05  BRR-SITE-CODE-1           PIC XX.                    BILLREQ
               05  BRR-SITE-CODE-2           PIC XX.                    BILLREQ
               05  BRR-REQUEST-COPIES        PIC X.                     BILLREQ
               05  BRR-REASON-CODE           PIC X.                     BILLREQ
               05  BRR-BILL-FORM-TYPE        PIC X.                     BILLREQ
               05  BRR-DATA-LOCATION         PIC X.                     BILLREQ
               05  BRR-LIST-BILL-IDNTITY         PIC X(8).              BILLREQ
               05  BRR-EMPLOYEE-NUMBER     PIC S99999V USAGE COMP-3.    BILLREQ
               05  BRR-DEPENDENT-NUMBER      PIC S999V USAGE COMP-3.    BILLREQ
               05  BRR-DIVISION-NUMBER       PIC X(2).                  BILLREQ
               05  BRR-BRANCH-NUMBER         PIC X(2).                  BILLREQ
               05  BRR-SOURCE-PROGRAM        PIC X(8).                  BILLREQ
               05  BRR-AFFL-CODE             PIC X(2).                  BILLREQ
               05  BRR-ERROR-CODE            PIC X(5).                  BILLREQ
               05  BRR-NEXT-BILL-YY          PIC X(2).                  BILLREQ
               05  BRR-NEXT-BILL-MM          PIC X(2).                  BILLREQ
               05  BRR-NEXT-BILL-DD          PIC X(2).                  BILLREQ
               05  BRR-HOW-BILLED            PIC X(2).                  BILLREQ
