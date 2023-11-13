      ******************************************************************CASNVW3
      * DCLGEN TABLE CASENV03     VIEW #3 FOR CASENAME                  CASNVW3
      ******************************************************************CASNVW3
           EXEC SQL DECLARE CASENAME TABLE                              CASNVW3
           ( IDNTITY                        CHAR(8) NOT NULL,           CASNVW3
             LAST_NAME                      CHAR(20),                   CASNVW3
             FIRST_NAME                     CHAR(15),                   CASNVW3
             MIDDLE_NAME                    CHAR(15),                   CASNVW3
             PREFIX                         CHAR(4),                    CASNVW3
             SUFFIX1                        CHAR(4),                    CASNVW3
             SUFFIX2                        CHAR(4),                    CASNVW3
             COMPANY_IND                    CHAR(1) NOT NULL,           CASNVW3
             COMPANY_IN_ADDRESS             CHAR(1) NOT NULL,           CASNVW3
             COMPANY_NAME                   CHAR(30),                   CASNVW3
             DISPLAY_NAME                   CHAR(30),                   CASNVW3
             NICKNAME                       CHAR(10),                   CASNVW3
             ADDRESS1                       CHAR(30) NOT NULL,          CASNVW3
             ADDRESS2                       CHAR(30),                   CASNVW3
             CITY                           CHAR(30) NOT NULL,          CASNVW3
             STATE                          CHAR(2) NOT NULL,           CASNVW3
             ZIP                            CHAR(5) NOT NULL,           CASNVW3
             ZIP_PLUS4                      CHAR(4) NOT NULL,           CASNVW3
             COUNTY_CODE                    CHAR(5) NOT NULL,           CASNVW3
             AREA_CODE                      CHAR(3) NOT NULL,           CASNVW3
             PHONE                          CHAR(7) NOT NULL,           CASNVW3
             PHONE_EXTENSION                CHAR(4) NOT NULL,           CASNVW3
             SSN                            CHAR(9),                    CASNVW3
             SEX                            CHAR(1) NOT NULL,           CASNVW3
             BIRTH_DATE                     DATE,                       CASNVW3
             FINALST_REAS_CODE              CHAR(3) NOT NULL,           CASNVW3
             FINALST_OVRD_IND               CHAR(1) NOT NULL,           CASNVW3
             DUP_ADDR_OVRD_IND              CHAR(1) NOT NULL,           CASNVW3
             EFFECTIVE_DATE                 DATE NOT NULL,              CASNVW3
             CHANGE_DATE                    TIMESTAMP NOT NULL,         CASNVW3
             CHANGE_LOGON                   CHAR(8) NOT NULL,           CASNVW3
             ENTITY_TYPE                    CHAR(2) NOT NULL,           CASNVW3
             RECORD_STATUS                  CHAR(1) NOT NULL,           CASNVW3
             ALT_ADDRESS_IND                CHAR(1) NOT NULL,           CASNVW3
             FUTURE_ADDRESS_IND             CHAR(1) NOT NULL,           CASNVW3
             RECORD_ORIGIN                  CHAR(1) NOT NULL,           CASNVW3
             COMBINED_STATUS                CHAR(2) NOT NULL,           CASNVW3
             SITE_CODE                      CHAR(2) NOT NULL,           CASNVW3
             NAME_KEY1                      CHAR(8) NOT NULL,           CASNVW3
             NAME_KEY2                      CHAR(8) NOT NULL,           CASNVW3
             NAME_KEY3                      CHAR(2) NOT NULL,           CASNVW3
             ADDRESS_KEY1                   CHAR(24) NOT NULL,          CASNVW3
             ASSOCIATION1                   CHAR(5),                    CASNVW3
             ASSOCIATION2                   CHAR(5),                    CASNVW3
             ASSOCIATION3                   CHAR(5),                    CASNVW3
             FAX_AREA_CODE                  CHAR(3) NOT NULL,           CASNVW3
             FAX_PHONE                      CHAR(7) NOT NULL,           CASNVW3
             ORIGINAL_STATE                 CHAR(2) NOT NULL,
             EMAIL                          CHAR(50),
             PASSWORD                       CHAR(12),                   CASNVW3
             ORIGINAL_ZIP                   CHAR(5) NOT NULL,
             ORIGINAL_ZIP_PLUS4             CHAR(4) NOT NULL,
R04209       EMAIL_STATUS                   CHAR(1) NOT NULL,
             COUNTRY_CODE                   CHAR(3),                    CASNVW3
             POSTAL_CODE                    CHAR(9)                     CASNVW3
           ) END-EXEC.                                                  CASNVW3
