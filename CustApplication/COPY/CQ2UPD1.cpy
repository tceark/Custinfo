       01  CQ2UPD1I.
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
           02  REQNUMBL    COMP  PIC  S9(4).
           02  REQNUMBF    PICTURE X.
           02  FILLER REDEFINES REQNUMBF.
             03 REQNUMBA    PICTURE X.
           02  REQNUMBI  PIC X(12).
           02  CUNUMBL    COMP  PIC  S9(4).
           02  CUNUMBF    PICTURE X.
           02  FILLER REDEFINES CUNUMBF.
             03 CUNUMBA    PICTURE X.
           02  CUNUMBI  PIC X(12).
           02  CULNAMEL    COMP  PIC  S9(4).
           02  CULNAMEF    PICTURE X.
           02  FILLER REDEFINES CULNAMEF.
             03 CULNAMEA    PICTURE X.
           02  CULNAMEI  PIC X(28).
           02  CUFNAMEL    COMP  PIC  S9(4).
           02  CUFNAMEF    PICTURE X.
           02  FILLER REDEFINES CUFNAMEF.
             03 CUFNAMEA    PICTURE X.
           02  CUFNAMEI  PIC X(20).
           02  CUMNAMEL    COMP  PIC  S9(4).
           02  CUMNAMEF    PICTURE X.
           02  FILLER REDEFINES CUMNAMEF.
             03 CUMNAMEA    PICTURE X.
           02  CUMNAMEI  PIC X(20).
           02  CUADR1L    COMP  PIC  S9(4).
           02  CUADR1F    PICTURE X.
           02  FILLER REDEFINES CUADR1F.
             03 CUADR1A    PICTURE X.
           02  CUADR1I  PIC X(48).
           02  CUADR2L    COMP  PIC  S9(4).
           02  CUADR2F    PICTURE X.
           02  FILLER REDEFINES CUADR2F.
             03 CUADR2A    PICTURE X.
           02  CUADR2I  PIC X(48).
           02  CUCITYL    COMP  PIC  S9(4).
           02  CUCITYF    PICTURE X.
           02  FILLER REDEFINES CUCITYF.
             03 CUCITYA    PICTURE X.
           02  CUCITYI  PIC X(28).
           02  CUSTATEL    COMP  PIC  S9(4).
           02  CUSTATEF    PICTURE X.
           02  FILLER REDEFINES CUSTATEF.
             03 CUSTATEA    PICTURE X.
           02  CUSTATEI  PIC X(28).
           02  CUZIPL    COMP  PIC  S9(4).
           02  CUZIPF    PICTURE X.
           02  FILLER REDEFINES CUZIPF.
             03 CUZIPA    PICTURE X.
           02  CUZIPI  PIC X(12).
           02  CUPHONHL    COMP  PIC  S9(4).
           02  CUPHONHF    PICTURE X.
           02  FILLER REDEFINES CUPHONHF.
             03 CUPHONHA    PICTURE X.
           02  CUPHONHI  PIC X(18).
           02  CUPHONWL    COMP  PIC  S9(4).
           02  CUPHONWF    PICTURE X.
           02  FILLER REDEFINES CUPHONWF.
             03 CUPHONWA    PICTURE X.
           02  CUPHONWI  PIC X(18).
           02  CUPHONCL    COMP  PIC  S9(4).
           02  CUPHONCF    PICTURE X.
           02  FILLER REDEFINES CUPHONCF.
             03 CUPHONCA    PICTURE X.
           02  CUPHONCI  PIC X(18).
           02  CULDATEL    COMP  PIC  S9(4).
           02  CULDATEF    PICTURE X.
           02  FILLER REDEFINES CULDATEF.
             03 CULDATEA    PICTURE X.
           02  CULDATEI  PIC X(10).
           02  CULTIMEL    COMP  PIC  S9(4).
           02  CULTIMEF    PICTURE X.
           02  FILLER REDEFINES CULTIMEF.
             03 CULTIMEA    PICTURE X.
           02  CULTIMEI  PIC X(11).
           02  CUTOKENL    COMP  PIC  S9(4).
           02  CUTOKENF    PICTURE X.
           02  FILLER REDEFINES CUTOKENF.
             03 CUTOKENA    PICTURE X.
           02  CUTOKENI  PIC X(3).
           02  SYSMSG1L    COMP  PIC  S9(4).
           02  SYSMSG1F    PICTURE X.
           02  FILLER REDEFINES SYSMSG1F.
             03 SYSMSG1A    PICTURE X.
           02  SYSMSG1I  PIC X(78).
           02  SYSMSG2L    COMP  PIC  S9(4).
           02  SYSMSG2F    PICTURE X.
           02  FILLER REDEFINES SYSMSG2F.
             03 SYSMSG2A    PICTURE X.
           02  SYSMSG2I  PIC X(78).
           02  PFROW23L    COMP  PIC  S9(4).
           02  PFROW23F    PICTURE X.
           02  FILLER REDEFINES PFROW23F.
             03 PFROW23A    PICTURE X.
           02  PFROW23I  PIC X(78).
       01  CQ2UPD1O REDEFINES CQ2UPD1I.
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
           02  REQNUMBO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  CUNUMBO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  CULNAMEO  PIC X(28).
           02  FILLER PICTURE X(3).
           02  CUFNAMEO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  CUMNAMEO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  CUADR1O  PIC X(48).
           02  FILLER PICTURE X(3).
           02  CUADR2O  PIC X(48).
           02  FILLER PICTURE X(3).
           02  CUCITYO  PIC X(28).
           02  FILLER PICTURE X(3).
           02  CUSTATEO  PIC X(28).
           02  FILLER PICTURE X(3).
           02  CUZIPO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  CUPHONHO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  CUPHONWO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  CUPHONCO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  CULDATEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  CULTIMEO  PIC X(11).
           02  FILLER PICTURE X(3).
           02  CUTOKENO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  SYSMSG1O  PIC X(78).
           02  FILLER PICTURE X(3).
           02  SYSMSG2O  PIC X(78).
           02  FILLER PICTURE X(3).
           02  PFROW23O  PIC X(78).
