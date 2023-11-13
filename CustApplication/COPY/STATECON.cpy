      ******************************************************************
      *   DCLGEN FOR STATECON     VIEW #1 OF STATE_COUNTY_ZIP
      ******************************************************************
           EXEC SQL DECLARE STATE_COUNTY_ZIP TABLE
           ( STATE_CD                CHAR(2)     NOT NULL,
             STATE_NUMBER            CHAR(2)     NOT NULL,
             COUNTY_NUMBER           CHAR(3)     NOT NULL,
             ZIP_CODE                CHAR(5)     NOT NULL,
             COUNTY                  CHAR(30)    NOT NULL,
             CITY_NAME               CHAR(30)    NOT NULL,
             ADDRESS_TYPE            CHAR(1)     NOT NULL,
             AREA_CODE               CHAR(3)     NOT NULL,
             EFFECTIVE_DATE          DATE        NOT NULL WITH DEFAULT,
             LOGIN_ID                CHAR(8)     NOT NULL,
             REVISED_DATE            TIMESTAMP   NOT NULL WITH DEFAULT,
             OUT_FOR_UPDATE          CHAR(1)     NOT NULL WITH DEFAULT
           ) END-EXEC.
      ******************************************************************
      * FIELD PREFIXED COBOL DECLARATION FOR STATECON                  *
      ******************************************************************
       01  SCZ-STATE-COUNTY-ZIP-RECORD.
           10 SCZ-STATE-CD                  PIC X(02).
           10 SCZ-STATE-NUMBER              PIC X(02).
           10 SCZ-COUNTY-NUMBER             PIC X(03).
           10 SCZ-ZIP-CODE                  PIC X(05).
           10 SCZ-COUNTY                    PIC X(30).
           10 SCZ-CITY-NAME                 PIC X(30).
           10 SCZ-ADDRESS-TYPE              PIC X(01).
           10 SCZ-AREA-CODE                 PIC X(03).
           10 SCZ-EFFECTIVE-DATE            PIC X(10).
           10 SCZ-LOGON-ID                  PIC X(08).
           10 SCZ-REVISED-DATE              PIC X(26).
           10 SCZ-OUT-FOR-UPDATE            PIC X(1).
      ******************************************************************
      * INDICATOR FIELDS FOR TABLE STATE_COUNTY_ZIP                    *
      ******************************************************************
       01  SCZ-INDICATOR-ARRAY.
           05  SCZ-INDICATORS  OCCURS 8 TIMES  PIC S9(04) COMP.
       01  SCZ-INDICATORS REDEFINES SCZ-INDICATOR-ARRAY.
           10 SCZ-STATE-CD-IND                     PIC S9(04) COMP.
           10 SCZ-STATE-NUMBER-IND                 PIC S9(04) COMP.
           10 SCZ-COUNTY-NUMBER-IND                PIC S9(04) COMP.
           10 SCZ-ZIP-CODE-IND                     PIC S9(04) COMP.
           10 SCZ-COUNTY-IND                       PIC S9(04) COMP.
           10 SCZ-CITY-NAME-IND                    PIC S9(04) COMP.
           10 SCZ-ADDRESS-TYPE-IND                 PIC S9(04) COMP.
           10 SCZ-AREA-CODE-IND                    PIC S9(04) COMP.
           10 SCZ-EFFECTIVE-DATE-IND               PIC S9(04) COMP.
           10 SCZ-LOGON-ID-IND                     PIC S9(04) COMP.
           10 SCZ-EFFECTIVE-DATE-IND               PIC S9(04) COMP.
           10 SCZ-REVISED-DATE-IND                 PIC S9(04) COMP.
           10 SCZ-OUT-FOR-UPDATE-IND               PIC S9(04) COMP.
