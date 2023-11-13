       01  INPUT-REC.
           05 REC-TYPE                 PIC 9(01).
           05 CUST-ID                  PIC S9(4) COMP.
           05 VAR-REC                  PIC X(69).
           05 CUST-REC REDEFINES VAR-REC.
               10 CUST-NAME            PIC X(10).                       ARCDVC
               10 CUST-ADDRESS         PIC X(40).                       ARCDVC
               10 CUST-CITY            PIC X(10).                       ARCDVC
               10 CUST-STATE           PIC X(2).                        ARCDVC
               10 CUST-ZIP             PIC 9(5)                         ARCDVC
               10 CUST-ACCTS           PIC S9(4) COMP.                  ARCDVC
           05 ACCT-REC REDEFINES VAR-REC.
               10 ACCT-NO              PIC S9(8) COMP.
               10 ACCT-TYPE            PIC X(8).
               10 ACCT-BAL             PIC S9(5)V9(2) COMP-3.
           05 TRAN-REC REDEFINES VAR-REC.
               10 TRAN-TYPE            PIC X(6).                        ARCDVC
               10 TRAN-DATE            PIC X(8).                        ARCDVC
               10 TRAN-AMT             PIC S9(5)V9(2) COMP-3.           ARCDVC
