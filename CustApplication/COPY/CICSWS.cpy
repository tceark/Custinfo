       01  WS-CICS-ERROR.
           05  CICS-ERROR-COMM-AREA.
               10  CICSERR-EIB         PIC X(76).
               10  CICS-PROGID         PIC X(8).
               10  CICS-KEY            PIC X(26) VALUE SPACES.
           05  CICS-EIB                PIC X(8).
           05  CICS-RET-EIB.
               10  FILLER              PIC X(16).
               10  CICS-RET-PROGID     PIC X(8).
