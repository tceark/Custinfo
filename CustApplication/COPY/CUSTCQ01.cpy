           EXEC SQL DECLARE CUSTACC1 TABLE
                (
                 CUQ1_NUMBER            CHAR (12)      NOT NULL,
                 CUQ1_STATUS            CHAR (1)       NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_LAST_NAME         CHAR (28)      NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_FIRST_NAME        CHAR (20)      NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_MID_NAME          CHAR (20)      NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_ADDRESS_1         CHAR (48)      NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_ADDRESS_2         CHAR (48)      NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_CITY              CHAR (28)      NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_STATE             CHAR (28)      NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_POSTAL_CODE       CHAR (12)      NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_PHONE_HOME        CHAR (18)      NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_PHONE_WORK        CHAR (18)      NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_PHONE_CELL        CHAR (18)      NOT NULL
                                                       WITH DEFAULT,
      *          CUQ1_CREDIT_LIMIT      DECIMAL (7,0)  NOT NULL
      *                                                WITH DEFAULT,
      *          CUQ1_DISCOUNT_CODE_1   INTEGER        NOT NULL
      *                                                WITH DEFAULT,
      *          CUQ1_DISCOUNT_RATE_1   NUMBER (2,3)   NOT NULL
      *                                                WITH DEFAULT,
      *          CUQ1_DISCOUNT_DATE_1   CHAR (8)       NOT NULL
      *                                                WITH DEFAULT,
                 CUQ1_LADATE            CHAR (8)       NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_LATIME            CHAR (8)       NOT NULL
                                                       WITH DEFAULT,
                 CUQ1_TOKEN             CHAR (3)       NOT NULL
                                                       WITH DEFAULT
                )
           END-EXEC.

       01  CUQ1-RECORD.
           05  CUQ1-NUMBER             PIC X(12).
           05  CUQ1-DATA.
               10  CUQ1-STATUS         PIC X.
               10  CUQ1-NAME.
                   15  CUQ1-LAST-NAME  PIC X(28).
                   15  CUQ1-FIRST-NAME PIC X(20).
                   15  CUQ1-MID-NAME   PIC X(20).
               10  CUQ1-ADDRESS-1      PIC X(48).
               10  CUQ1-ADDRESS-2      PIC X(48).
               10  CUQ1-CITY           PIC X(28).
               10  CUQ1-STATE          PIC X(28).
               10  CUQ1-POSTAL-CODE    PIC X(12).
               10  CUQ1-PHONE-HOME     PIC X(18).
               10  CUQ1-PHONE-WORK     PIC X(18).
               10  CUQ1-PHONE-CELL     PIC X(18).
               10  CUQ1-CREDIT-LIMIT   PIC 9(7)    COMP-3.
               10  CUQ1-DISCOUNT       OCCURS 3 TIMES.
                   15  CUQ1-DISCOUNT-CODE  PIC S9(3)   COMP.
                   15  CUQ1-DISCOUNT-RATE  PIC S9(2)V999.
                   15  CUQ1-DISCOUNT-DATE  PIC X(8).
               10  CUQ1-LADATE         PIC X(8).
               10  CUQ1-LATIME         PIC X(8).
               10  CUQ1-TOKEN          PIC X(3).
               10  FILLER              PIC X(145).
