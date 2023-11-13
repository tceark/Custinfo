       01  MN1FMS1I.
           02  FILLER PIC X(12).
           02  SYSPAGEL    COMP  PIC  S9(4).
           02  SYSPAGEF    PICTURE X.
           02  FILLER REDEFINES SYSPAGEF.
             03 SYSPAGEA    PICTURE X.
           02  SYSPAGEI  PIC X(8).
           02  SYSDATEL    COMP  PIC  S9(4).
           02  SYSDATEF    PICTURE X.
           02  FILLER REDEFINES SYSDATEF.
             03 SYSDATEA    PICTURE X.
           02  SYSDATEI  PIC X(10).
           02  SYS-IDL    COMP  PIC  S9(4).
           02  SYS-IDF    PICTURE X.
           02  FILLER REDEFINES SYS-IDF.
             03 SYS-IDA    PICTURE X.
           02  SYS-IDI  PIC X(4).
           02  SYSTIMEL    COMP  PIC  S9(4).
           02  SYSTIMEF    PICTURE X.
           02  FILLER REDEFINES SYSTIMEF.
             03 SYSTIMEA    PICTURE X.
           02  SYSTIMEI  PIC X(11).
           02  OPTNUML    COMP  PIC  S9(4).
           02  OPTNUMF    PICTURE X.
           02  FILLER REDEFINES OPTNUMF.
             03 OPTNUMA    PICTURE X.
           02  OPTNUMI  PIC X(1).
           02  SYSMSG1L    COMP  PIC  S9(4).
           02  SYSMSG1F    PICTURE X.
           02  FILLER REDEFINES SYSMSG1F.
             03 SYSMSG1A    PICTURE X.
           02  SYSMSG1I  PIC X(78).
       01  MN1FMS1O REDEFINES MN1FMS1I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  SYSPAGEO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  SYSDATEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  SYS-IDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  SYSTIMEO  PIC X(11).
           02  FILLER PICTURE X(3).
           02  OPTNUMO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  SYSMSG1O  PIC X(78).
