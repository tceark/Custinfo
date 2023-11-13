      ***************************************                           SECCOMMC
      *  SECURITY COMMUNICATION AREA                                    SECCOMMC
      *  PROGRAM:         SECCHECK                                      SECCOMMC
      *  COPYBOOK NAME:   SECCOMMC                                      SECCOMMC
      *  COPYBOOK LENGTH: 31 BYTES                                      SECCOMMC
      ***************************************                           SECCOMMC
           05  SECURITY-COMM-AREA.                                      SECCOMMC
               10  SEC-RESOURCE-NAME    PIC X(8)        VALUE SPACES.   SECCOMMC
               10  FILLER               PIC X           VALUE SPACE.    SECCOMMC
               10  SEC-RETURN-CODE      PIC X           VALUE SPACE.    SECCOMMC
               10  SEC-FUNCTION-CODE    PIC X           VALUE SPACE.    SECCOMMC
               10  SEC-CARRIER-CODE     PIC XX          VALUE SPACES.   SECCOMMC
               10  SEC-SITE-CODE        PIC XX          VALUE SPACES.   SECCOMMC
               10  SEC-CHK-RESOURCE     PIC X           VALUE SPACE.    SECCOMMC
                   88  SEC-YES-CHECK-RESOURCE           VALUE 'Y'.      SECCOMMC
               10  SEC-CHK-CARRIER      PIC X           VALUE SPACE.    SECCOMMC
                   88  SEC-YES-CHECK-CARR               VALUE 'Y'.      SECCOMMC
               10  SEC-CHK-SITE         PIC X           VALUE SPACE.    SECCOMMC
                   88  SEC-YES-CHECK-SITE               VALUE 'Y'.      SECCOMMC
               10  FILLER               PIC X(13)       VALUE SPACES.   SECCOMMC
                                                                        SECCOMMC
