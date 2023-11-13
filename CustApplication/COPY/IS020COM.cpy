        01  IS020-COMMAREA.                                               IS020C
             05  IS020CA-PROGRAM-NAME      PIC X(8).                      IS020C
             05  IS020CA-MAP-NAME          PIC X(8).                      IS020C
             05  IS020CA-CASE-NUMBER       PIC X(6).                      IS020C
             05  IS020CA-EMPLOYEE-NUMBER   PIC X(5).                      IS020C
             05  IS020CA-CARRIER           PIC X(2).                      IS020C
             05  IS020CA-TIME-CASE-UPDATED PIC X(6).                      IS020C
             05  IS020CA-FILE-PREFIX       PIC X(2).                      IS020C
             05  IS020CA-PREVIOUS-RECORD   PIC X(766).                    IS020C
             05  IS020CA-CURRENT-RECORD    PIC X(766).                    IS020C
             05  IS020CA-DB2-DATA.                                        IS020C
                 10 IS020CA-PREVIOUS-DB2   PIC X(900).                    IS020C
                 10 IS020CA-CURRENT-DB2    PIC X(900).                    IS020C
                 10 IS020CA-CIM            PIC X(8).                      IS020C
                 10 IS020CA-SMOKER-QUESTION PIC X(1).                     IS020C
                 10 IS020CA-UPD-COV-HIST   PIC X(1).                      IS020C
                 10 FILLER                 PIC X(73).                     IS020C
             05  IS020CA-LISTBILL-9D-IND   PIC X(1).                      IS020C
             05  IS020CA-BILLPRINT-LVL-CIM PIC X(8).                      IS020C
             EJECT                                                        IS020C
