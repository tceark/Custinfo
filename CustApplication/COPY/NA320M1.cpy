      ***************************************************************** NA320M1
      * SDF: NA320M1 NA320M1                                            NA320M1
      ***************************************************************** NA320M1
       01   NA320M1I.                                                   NA320M1
           02 FILLER    PIC X(12).                                      NA320M1
      * UR00051                                                         NA320M1
           02 MAP-SYSTEM-CDL      COMP PIC S9(4).                       NA320M1
           02 MAP-SYSTEM-CDF      PIC X.                                NA320M1
           02 MAP-SYSTEM-CDI      PIC X(2).                             NA320M1
      * UR00052                                                         NA320M1
           02 MAP-ACTION-CDL      COMP PIC S9(4).                       NA320M1
           02 MAP-ACTION-CDF      PIC X.                                NA320M1
           02 MAP-ACTION-CDI      PIC X(5).                             NA320M1
      * UR00053                                                         NA320M1
           02 MAP-CUST-INFOL      COMP PIC S9(4).                       NA320M1
           02 MAP-CUST-INFOF      PIC X.                                NA320M1
           02 MAP-CUST-INFOI      PIC X(40).                            NA320M1
      * UR00001 PREVENT CICS USING '/' IN NAME FIELDS AS PAGING COMMAND.NA320M1
           02 MAP-MDT-BYTEL  COMP PIC S9(4).                            NA320M1
           02 MAP-MDT-BYTEF  PIC X.                                     NA320M1
           02 MAP-MDT-BYTEI  PIC X.                                     NA320M1
      * UR00001                                                         NA320M1
           02 MAP-LITERAL1L  COMP PIC S9(4).                            NA320M1
           02 MAP-LITERAL1F  PIC X.                                     NA320M1
           02 MAP-LITERAL1I  PIC X(20).                                 NA320M1
      * NA00030                                                         NA320M1
           02 MAP-FIRST-NAMEL     COMP PIC S9(4).                       NA320M1
           02 MAP-FIRST-NAMEF     PIC X.                                NA320M1
           02 MAP-FIRST-NAMEI     PIC X(15).                            NA320M1
      * UR00001                                                         NA320M1
           02 MAP-LITERAL2L  COMP PIC S9(4).                            NA320M1
           02 MAP-LITERAL2F  PIC X.                                     NA320M1
           02 MAP-LITERAL2I  PIC X(3).                                  NA320M1
      * NA00035                                                         NA320M1
           02 MAP-MIDDLE-NAMEL    COMP PIC S9(4).                       NA320M1
           02 MAP-MIDDLE-NAMEF    PIC X.                                NA320M1
           02 MAP-MIDDLE-NAMEI    PIC X(10).                            NA320M1
      * UR00001                                                         NA320M1
           02 MAP-LITERAL3L  COMP PIC S9(4).                            NA320M1
           02 MAP-LITERAL3F  PIC X.                                     NA320M1
           02 MAP-LITERAL3I  PIC X(4).                                  NA320M1
      * NA00040                                                         NA320M1
           02 MAP-LAST-NAMEL      COMP PIC S9(4).                       NA320M1
           02 MAP-LAST-NAMEF      PIC X.                                NA320M1
           02 MAP-LAST-NAMEI      PIC X(20).                            NA320M1
      * UR00001                                                         NA320M1
           02 MAP-LITERAL4L  COMP PIC S9(4).                            NA320M1
           02 MAP-LITERAL4F  PIC X.                                     NA320M1
           02 MAP-LITERAL4I  PIC X(3).                                  NA320M1
      * NA00151                                                         NA320M1
           02 MAP-SEXL  COMP PIC S9(4).                                 NA320M1
           02 MAP-SEXF  PIC X.                                          NA320M1
           02 MAP-SEXI  PIC X.                                          NA320M1
      * UR000001                                                        NA320M1
           02 MAP-LITERAL8L  COMP PIC S9(4).                            NA320M1
           02 MAP-LITERAL8F  PIC X.                                     NA320M1
           02 MAP-LITERAL8I  PIC X(9).                                  NA320M1
      * NA00140                                                         NA320M1
           02 MAP-SUFFIX1L   COMP PIC S9(4).                            NA320M1
           02 MAP-SUFFIX1F   PIC X.                                     NA320M1
           02 MAP-SUFFIX1I   PIC X(4).                                  NA320M1
      * UR00001                                                         NA320M1
           02 MAP-LITERAL6L  COMP PIC S9(4).                            NA320M1
           02 MAP-LITERAL6F  PIC X.                                     NA320M1
           02 MAP-LITERAL6I  PIC X.                                     NA320M1
      * NA00140                                                         NA320M1
           02 MAP-SUFFIX2L   COMP PIC S9(4).                            NA320M1
           02 MAP-SUFFIX2F   PIC X.                                     NA320M1
           02 MAP-SUFFIX2I   PIC X(4).                                  NA320M1
      * UR00001                                                         NA320M1
           02 MAP-LITERAL5L  COMP PIC S9(4).                            NA320M1
           02 MAP-LITERAL5F  PIC X.                                     NA320M1
           02 MAP-LITERAL5I  PIC X(8).                                  NA320M1
      * NA00050                                                         NA320M1
           02 MAP-NICKNAMEL  COMP PIC S9(4).                            NA320M1
           02 MAP-NICKNAMEF  PIC X.                                     NA320M1
           02 MAP-NICKNAMEI  PIC X(10).                                 NA320M1
      * UR00001                                                         NA320M1
           02 MAP-ISS-LITERALL    COMP PIC S9(4).                       NA320M1
           02 MAP-ISS-LITERALF    PIC X.                                NA320M1
           02 MAP-ISS-LITERALI    PIC X(05).                            NA320M1
      * UR00001                                                         NA320M1
           02 MAP-ST-LITERALL     COMP PIC S9(4).                       NA320M1
           02 MAP-ST-LITERALF     PIC X.                                NA320M1
           02 MAP-ST-LITERALI     PIC X(05).                            NA320M1
      * NA00077                                                         NA320M1
           02 MAP-ISSUE-STATEL    COMP PIC S9(4).                       NA320M1
           02 MAP-ISSUE-STATEF    PIC X.                                NA320M1
           02 MAP-ISSUE-STATEI    PIC X(02).                            NA320M1
      * CM00055                                                         NA320M1
           02 MAP-COMP-NAMEL      COMP PIC S9(4).                       NA320M1
           02 MAP-COMP-NAMEF      PIC X.                                NA320M1
           02 MAP-COMP-NAMEI      PIC X(30).                            NA320M1
           02 MAP-PREFIXL    COMP PIC S9(4).                            NA320M1
           02 MAP-PREFIXF    PIC X.                                     NA320M1
           02 MAP-PREFIXI    PIC X(04).                                 NA320M1
      * NA00154                                                         NA320M1
           02 MAP-DIS-NAMEL  COMP PIC S9(4).                            NA320M1
           02 MAP-DIS-NAMEF  PIC X.                                     NA320M1
           02 MAP-DIS-NAMEI  PIC X(30).                                 NA320M1
      * NA00168                                                         NA320M1
           02 MAP-DIS-CHG-INDL    COMP PIC S9(4).                       NA320M1
           02 MAP-DIS-CHG-INDF    PIC X.                                NA320M1
           02 MAP-DIS-CHG-INDI    PIC X(01).                            NA320M1
      * CM00155                                                         NA320M1
           02 MAP-ADDRESS1L  COMP PIC S9(4).                            NA320M1
           02 MAP-ADDRESS1F  PIC X.                                     NA320M1
           02 MAP-ADDRESS1I  PIC X(22).                                 NA320M1
      * UR00001                                                         NA320M1
           02 MAP-LITERAL7L  COMP PIC S9(4).                            NA320M1
           02 MAP-LITERAL7F  PIC X.                                     NA320M1
           02 MAP-LITERAL7I  PIC X(9).                                  NA320M1
      * NA00152                                                         NA320M1
           02 MAP-BIRTH-DATEL     COMP PIC S9(4).                       NA320M1
           02 MAP-BIRTH-DATEF     PIC X.                                NA320M1
           02 MAP-BIRTH-DATEI     PIC X(10).                            NA320M1
      * CM00160                                                         NA320M1
           02 MAP-ADDRESS2L  COMP PIC S9(4).                            NA320M1
           02 MAP-ADDRESS2F  PIC X.                                     NA320M1
           02 MAP-ADDRESS2I  PIC X(22).                                 NA320M1
      * UR00001                                                         NA320M1
           02 SSN-LITERALL   COMP PIC S9(4).                            NA320M1
           02 SSN-LITERALF   PIC X.                                     NA320M1
           02 SSN-LITERALI   PIC X(7).                                  NA320M1
      * NA00105                                                         NA320M1
           02 MAP-SSNL  COMP PIC S9(4).                                 NA320M1
           02 MAP-SSNF  PIC X.                                          NA320M1
           02 MAP-SSNI  PIC X(9).                                       NA320M1
      * NA00065                                                         NA320M1
           02 MAP-ADDRESS3L  COMP PIC S9(4).                            NA320M1
           02 MAP-ADDRESS3F  PIC X.                                     NA320M1
           02 MAP-ADDRESS3I  PIC X(30).                                 NA320M1
      * UR00001                                                         NA320M1
           02 MAP-LITERAL9L  COMP PIC S9(4).                            NA320M1
           02 MAP-LITERAL9F  PIC X.                                     NA320M1
           02 MAP-LITERAL9I  PIC X(33).                                 NA320M1
      * CM00165                                                         NA320M1
           02 MAP-CITYL      COMP PIC S9(4).                            NA320M1
           02 MAP-CITYF      PIC X.                                     NA320M1
           02 MAP-CITYI      PIC X(30).                                 NA320M1
      * UR00001                                                         NA320M1
           02 MAP-LITERAL10L      COMP PIC S9(4).                       NA320M1
           02 MAP-LITERAL10F      PIC X.                                NA320M1
           02 MAP-LITERAL10I      PIC X(30).                            NA320M1
      * NA00157                                                         NA320M1
           02 MAP-COMP-IN-ADDL    COMP PIC S9(4).                       NA320M1
           02 MAP-COMP-IN-ADDF    PIC X.                                NA320M1
           02 MAP-COMP-IN-ADDI    PIC X.                                NA320M1
      * CM00175                                                         NA320M1
           02 MAP-STATEL     COMP PIC S9(4).                            NA320M1
           02 MAP-STATEF     PIC X.                                     NA320M1
           02 MAP-STATEI     PIC XX.                                    NA320M1
      * CM00170                                                         NA320M1
           02 MAP-ZIPL  COMP PIC S9(4).                                 NA320M1
           02 MAP-ZIPF  PIC X.                                          NA320M1
           02 MAP-ZIPI  PIC X(5).                                       NA320M1
      * CM01760                                                         NA320M1
           02 MAP-ZIP-PLUS4L      COMP PIC S9(4).                       NA320M1
           02 MAP-ZIP-PLUS4F      PIC X.                                NA320M1
           02 MAP-ZIP-PLUS4I      PIC X(4).                             NA320M1
      * NA00164                                                         NA320M1
           02 MAP-ADDR-OVRIDEL    COMP PIC S9(4).                       NA320M1
           02 MAP-ADDR-OVRIDEF    PIC X.                                NA320M1
           02 MAP-ADDR-OVRIDEI    PIC X.                                NA320M1
      * NA00188                                                         NA320M1
           02 MAP-COUNTRY-CDL     COMP PIC S9(4).                       NA320M1
           02 MAP-COUNTRY-CDF     PIC X.                                NA320M1
           02 MAP-COUNTRY-CDI     PIC X(3).                             NA320M1
      * NA00189                                                         NA320M1
           02 MAP-POSTAL-CDL      COMP PIC S9(4).                       NA320M1
           02 MAP-POSTAL-CDF      PIC X.                                NA320M1
           02 MAP-POSTAL-CDI      PIC X(9).                             NA320M1
      * UR00001                                                         NA320M1
           02 ALT-LITERALL   COMP PIC S9(4).                            NA320M1
           02 ALT-LITERALF   PIC X.                                     NA320M1
           02 ALT-LITERALI   PIC X(31).                                 NA320M1
      * NA00158                                                         NA320M1
           02 MAP-ALT-ADDRL  COMP PIC S9(4).                            NA320M1
           02 MAP-ALT-ADDRF  PIC X.                                     NA320M1
           02 MAP-ALT-ADDRI  PIC X.                                     NA320M1
      * NA00100                                                         NA320M1
           02 MAP-AREA-CODEL      COMP PIC S9(4).                       NA320M1
           02 MAP-AREA-CODEF      PIC X.                                NA320M1
           02 MAP-AREA-CODEI      PIC X(3).                             NA320M1
      * NA00100                                                         NA320M1
           02 MAP-PHONE3L    COMP PIC S9(4).                            NA320M1
           02 MAP-PHONE3F    PIC X.                                     NA320M1
           02 MAP-PHONE3I    PIC X(3).                                  NA320M1
      * CM00180                                                         NA320M1
           02 MAP-PHONE4L    COMP PIC S9(4).                            NA320M1
           02 MAP-PHONE4F    PIC X.                                     NA320M1
           02 MAP-PHONE4I    PIC X(4).                                  NA320M1
      * UR00001                                                         NA320M1
           02 EXT-LITERALL   COMP PIC S9(4).                            NA320M1
           02 EXT-LITERALF   PIC X.                                     NA320M1
           02 EXT-LITERALI   PIC X(3).                                  NA320M1
      * NA00150                                                         NA320M1
           02 MAP-PHONE-EXTL      COMP PIC S9(4).                       NA320M1
           02 MAP-PHONE-EXTF      PIC X.                                NA320M1
           02 MAP-PHONE-EXTI      PIC X(4).                             NA320M1
      * UR00001                                                         NA320M1
           02 FUT-LITERALL   COMP PIC S9(4).                            NA320M1
           02 FUT-LITERALF   PIC X.                                     NA320M1
           02 FUT-LITERALI   PIC X(31).                                 NA320M1
      * NA00159                                                         NA320M1
           02 MAP-FUTURE-ADDRL    COMP PIC S9(4).                       NA320M1
           02 MAP-FUTURE-ADDRF    PIC X.                                NA320M1
           02 MAP-FUTURE-ADDRI    PIC X.                                NA320M1
      * NA00170                                                         NA320M1
           02 MAP-FAX-AREA-CDL    COMP PIC S9(4).                       NA320M1
           02 MAP-FAX-AREA-CDF    PIC X.                                NA320M1
           02 MAP-FAX-AREA-CDI    PIC X(3).                             NA320M1
      * NA00170                                                         NA320M1
           02 MAP-FAX-PHONE3L     COMP PIC S9(4).                       NA320M1
           02 MAP-FAX-PHONE3F     PIC X.                                NA320M1
           02 MAP-FAX-PHONE3I     PIC X(3).                             NA320M1
      * NA00170                                                         NA320M1
           02 MAP-FAX-PHONE4L     COMP PIC S9(4).                       NA320M1
           02 MAP-FAX-PHONE4F     PIC X.                                NA320M1
           02 MAP-FAX-PHONE4I     PIC X(4).                             NA320M1
           02 MAP-EMAIL1L    COMP PIC S9(4).                            NA320M1
           02 MAP-EMAIL1F    PIC X.                                     NA320M1
           02 MAP-EMAIL1I    PIC X(35).                                 NA320M1
      * NA00124                                                         NA320M1
           02 MAP-ENTITY-TYPEL    COMP PIC S9(4).                       NA320M1
           02 MAP-ENTITY-TYPEF    PIC X.                                NA320M1
           02 MAP-ENTITY-TYPEI    PIC X(15).                            NA320M1
           02 MAP-EMAIL2L    COMP PIC S9(4).                            NA320M1
           02 MAP-EMAIL2F    PIC X.                                     NA320M1
           02 MAP-EMAIL2I    PIC X(35).                                 NA320M1
      * NA00125                                                         NA320M1
           02 MAP-CUST-STATUSL    COMP PIC S9(4).                       NA320M1
           02 MAP-CUST-STATUSF    PIC X.                                NA320M1
           02 MAP-CUST-STATUSI    PIC X(15).                            NA320M1
      * UR00001                                                         NA320M1
           02 SITE-LITERALL  COMP PIC S9(4).                            NA320M1
           02 SITE-LITERALF  PIC X.                                     NA320M1
           02 SITE-LITERALI  PIC X(4).                                  NA320M1
      * NA00130                                                         NA320M1
           02 MAP-SITE-CODEL      COMP PIC S9(4).                       NA320M1
           02 MAP-SITE-CODEF      PIC X.                                NA320M1
           02 MAP-SITE-CODEI      PIC XX.                               NA320M1
      * NA00130                                                         NA320M1
           02 MAP-SITE-DESCL      COMP PIC S9(4).                       NA320M1
           02 MAP-SITE-DESCF      PIC X.                                NA320M1
           02 MAP-SITE-DESCI      PIC X(27).                            NA320M1
      * NA00115                                                         NA320M1
           02 MAP-EFF-DATEL  COMP PIC S9(4).                            NA320M1
           02 MAP-EFF-DATEF  PIC X.                                     NA320M1
           02 MAP-EFF-DATEI  PIC X(8).                                  NA320M1
      * NA00160                                                         NA320M1
           02 MAP-LOGONL     COMP PIC S9(4).                            NA320M1
           02 MAP-LOGONF     PIC X.                                     NA320M1
           02 MAP-LOGONI     PIC X(22).                                 NA320M1
      * NA00156                                                         NA320M1
           02 MAP-CHANGE-DATEL    COMP PIC S9(4).                       NA320M1
           02 MAP-CHANGE-DATEF    PIC X.                                NA320M1
           02 MAP-CHANGE-DATEI    PIC X(8).                             NA320M1
      * UR00001                                                         NA320M1
           02 FIN-LITERALL   COMP PIC S9(4).                            NA320M1
           02 FIN-LITERALF   PIC X.                                     NA320M1
           02 FIN-LITERALI   PIC X(31).                                 NA320M1
      * NA00161                                                         NA320M1
           02 MAP-FINALST-CDL     COMP PIC S9(4).                       NA320M1
           02 MAP-FINALST-CDF     PIC X.                                NA320M1
           02 MAP-FINALST-CDI     PIC X(3).                             NA320M1
      * NA00145                                                         NA320M1
           02 MAP-ASSOC1L    COMP PIC S9(4).                            NA320M1
           02 MAP-ASSOC1F    PIC X.                                     NA320M1
           02 MAP-ASSOC1I    PIC X(5).                                  NA320M1
      * NA00145                                                         NA320M1
           02 MAP-ASSOC2L    COMP PIC S9(4).                            NA320M1
           02 MAP-ASSOC2F    PIC X.                                     NA320M1
           02 MAP-ASSOC2I    PIC X(5).                                  NA320M1
      * NA00145                                                         NA320M1
           02 MAP-ASSOC3L    COMP PIC S9(4).                            NA320M1
           02 MAP-ASSOC3F    PIC X.                                     NA320M1
           02 MAP-ASSOC3I    PIC X(5).                                  NA320M1
      * NA00010                                                         NA320M1
           02 MAP-CIML  COMP PIC S9(4).                                 NA320M1
           02 MAP-CIMF  PIC X.                                          NA320M1
           02 MAP-CIMI  PIC X(8).                                       NA320M1
      * MU00013                                                         NA320M1
           02 MAP-PF-BYTEL   COMP PIC S9(4).                            NA320M1
           02 MAP-PF-BYTEF   PIC X.                                     NA320M1
           02 MAP-PF-BYTEI   PIC X.                                     NA320M1
      * UR00001                                                         NA320M1
           02 MAP-ERROR-MSGD OCCURS   4 TIMES .                         NA320M1
             03 MAP-ERROR-MSGL    COMP PIC S9(4).                       NA320M1
             03 MAP-ERROR-MSGF    PIC X.                                NA320M1
             03 MAP-ERROR-MSGI    PIC X(38).                            NA320M1
      ***************************************************************** NA320M1
      * SDF: NA320M1 NA320M1                                            NA320M1
      ***************************************************************** NA320M1
       01   NA320M1O REDEFINES NA320M1I.                                NA320M1
           02 FILLER    PIC X(12).                                      NA320M1
      * UR00051                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-SYSTEM-CDA      PIC X.                                NA320M1
           02 MAP-SYSTEM-CDO      PIC X(2).                             NA320M1
      * UR00052                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ACTION-CDA      PIC X.                                NA320M1
           02 MAP-ACTION-CDO      PIC X(5).                             NA320M1
      * UR00053                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-CUST-INFOA      PIC X.                                NA320M1
           02 MAP-CUST-INFOO      PIC X(40).                            NA320M1
      * UR00001 PREVENT CICS USING '/' IN NAME FIELDS AS PAGING COMMAND.NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-MDT-BYTEA  PIC X.                                     NA320M1
           02 MAP-MDT-BYTEO  PIC X.                                     NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LITERAL1A  PIC X.                                     NA320M1
           02 MAP-LITERAL1O  PIC X(20).                                 NA320M1
      * NA00030                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-FIRST-NAMEA     PIC X.                                NA320M1
           02 MAP-FIRST-NAMEO     PIC X(15).                            NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LITERAL2A  PIC X.                                     NA320M1
           02 MAP-LITERAL2O  PIC X(3).                                  NA320M1
      * NA00035                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-MIDDLE-NAMEA    PIC X.                                NA320M1
           02 MAP-MIDDLE-NAMEO    PIC X(10).                            NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LITERAL3A  PIC X.                                     NA320M1
           02 MAP-LITERAL3O  PIC X(4).                                  NA320M1
      * NA00040                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LAST-NAMEA      PIC X.                                NA320M1
           02 MAP-LAST-NAMEO      PIC X(20).                            NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LITERAL4A  PIC X.                                     NA320M1
           02 MAP-LITERAL4O  PIC X(3).                                  NA320M1
      * NA00151                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-SEXA  PIC X.                                          NA320M1
           02 MAP-SEXO  PIC X.                                          NA320M1
      * UR000001                                                        NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LITERAL8A  PIC X.                                     NA320M1
           02 MAP-LITERAL8O  PIC X(9).                                  NA320M1
      * NA00140                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-SUFFIX1A   PIC X.                                     NA320M1
           02 MAP-SUFFIX1O   PIC X(4).                                  NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LITERAL6A  PIC X.                                     NA320M1
           02 MAP-LITERAL6O  PIC X.                                     NA320M1
      * NA00140                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-SUFFIX2A   PIC X.                                     NA320M1
           02 MAP-SUFFIX2O   PIC X(4).                                  NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LITERAL5A  PIC X.                                     NA320M1
           02 MAP-LITERAL5O  PIC X(8).                                  NA320M1
      * NA00050                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-NICKNAMEA  PIC X.                                     NA320M1
           02 MAP-NICKNAMEO  PIC X(10).                                 NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ISS-LITERALA    PIC X.                                NA320M1
           02 MAP-ISS-LITERALO    PIC X(05).                            NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ST-LITERALA     PIC X.                                NA320M1
           02 MAP-ST-LITERALO     PIC X(05).                            NA320M1
      * NA00077                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ISSUE-STATEA    PIC X.                                NA320M1
           02 MAP-ISSUE-STATEO    PIC X(02).                            NA320M1
      * CM00055                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-COMP-NAMEA      PIC X.                                NA320M1
           02 MAP-COMP-NAMEO      PIC X(30).                            NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-PREFIXA    PIC X.                                     NA320M1
           02 MAP-PREFIXO    PIC X(04).                                 NA320M1
      * NA00154                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-DIS-NAMEA  PIC X.                                     NA320M1
           02 MAP-DIS-NAMEO  PIC X(30).                                 NA320M1
      * NA00168                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-DIS-CHG-INDA    PIC X.                                NA320M1
           02 MAP-DIS-CHG-INDO    PIC X(01).                            NA320M1
      * CM00155                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ADDRESS1A  PIC X.                                     NA320M1
           02 MAP-ADDRESS1O  PIC X(22).                                 NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LITERAL7A  PIC X.                                     NA320M1
           02 MAP-LITERAL7O  PIC X(9).                                  NA320M1
      * NA00152                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-BIRTH-DATEA     PIC X.                                NA320M1
           02 MAP-BIRTH-DATEO     PIC X(10).                            NA320M1
      * CM00160                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ADDRESS2A  PIC X.                                     NA320M1
           02 MAP-ADDRESS2O  PIC X(22).                                 NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 SSN-LITERALA   PIC X.                                     NA320M1
           02 SSN-LITERALO   PIC X(7).                                  NA320M1
      * NA00105                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-SSNA  PIC X.                                          NA320M1
           02 MAP-SSNO  PIC X(9).                                       NA320M1
      * NA00065                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ADDRESS3A  PIC X.                                     NA320M1
           02 MAP-ADDRESS3O  PIC X(30).                                 NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LITERAL9A  PIC X.                                     NA320M1
           02 MAP-LITERAL9O  PIC X(33).                                 NA320M1
      * CM00165                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-CITYA      PIC X.                                     NA320M1
           02 MAP-CITYO      PIC X(30).                                 NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LITERAL10A      PIC X.                                NA320M1
           02 MAP-LITERAL10O      PIC X(30).                            NA320M1
      * NA00157                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-COMP-IN-ADDA    PIC X.                                NA320M1
           02 MAP-COMP-IN-ADDO    PIC X.                                NA320M1
      * CM00175                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-STATEA     PIC X.                                     NA320M1
           02 MAP-STATEO     PIC XX.                                    NA320M1
      * CM00170                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ZIPA  PIC X.                                          NA320M1
           02 MAP-ZIPO  PIC X(5).                                       NA320M1
      * CM01760                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ZIP-PLUS4A      PIC X.                                NA320M1
           02 MAP-ZIP-PLUS4O      PIC X(4).                             NA320M1
      * NA00164                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ADDR-OVRIDEA    PIC X.                                NA320M1
           02 MAP-ADDR-OVRIDEO    PIC X.                                NA320M1
      * NA00188                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-COUNTRY-CDA     PIC X.                                NA320M1
           02 MAP-COUNTRY-CDO     PIC X(3).                             NA320M1
      * NA00189                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-POSTAL-CDA      PIC X.                                NA320M1
           02 MAP-POSTAL-CDO      PIC X(9).                             NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 ALT-LITERALA   PIC X.                                     NA320M1
           02 ALT-LITERALO   PIC X(31).                                 NA320M1
      * NA00158                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ALT-ADDRA  PIC X.                                     NA320M1
           02 MAP-ALT-ADDRO  PIC X.                                     NA320M1
      * NA00100                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-AREA-CODEA      PIC X.                                NA320M1
           02 MAP-AREA-CODEO      PIC X(3).                             NA320M1
      * NA00100                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-PHONE3A    PIC X.                                     NA320M1
           02 MAP-PHONE3O    PIC X(3).                                  NA320M1
      * CM00180                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-PHONE4A    PIC X.                                     NA320M1
           02 MAP-PHONE4O    PIC X(4).                                  NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 EXT-LITERALA   PIC X.                                     NA320M1
           02 EXT-LITERALO   PIC X(3).                                  NA320M1
      * NA00150                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-PHONE-EXTA      PIC X.                                NA320M1
           02 MAP-PHONE-EXTO      PIC X(4).                             NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 FUT-LITERALA   PIC X.                                     NA320M1
           02 FUT-LITERALO   PIC X(31).                                 NA320M1
      * NA00159                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-FUTURE-ADDRA    PIC X.                                NA320M1
           02 MAP-FUTURE-ADDRO    PIC X.                                NA320M1
      * NA00170                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-FAX-AREA-CDA    PIC X.                                NA320M1
           02 MAP-FAX-AREA-CDO    PIC X(3).                             NA320M1
      * NA00170                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-FAX-PHONE3A     PIC X.                                NA320M1
           02 MAP-FAX-PHONE3O     PIC X(3).                             NA320M1
      * NA00170                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-FAX-PHONE4A     PIC X.                                NA320M1
           02 MAP-FAX-PHONE4O     PIC X(4).                             NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-EMAIL1A    PIC X.                                     NA320M1
           02 MAP-EMAIL1O    PIC X(35).                                 NA320M1
      * NA00124                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ENTITY-TYPEA    PIC X.                                NA320M1
           02 MAP-ENTITY-TYPEO    PIC X(15).                            NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-EMAIL2A    PIC X.                                     NA320M1
           02 MAP-EMAIL2O    PIC X(35).                                 NA320M1
      * NA00125                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-CUST-STATUSA    PIC X.                                NA320M1
           02 MAP-CUST-STATUSO    PIC X(15).                            NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 SITE-LITERALA  PIC X.                                     NA320M1
           02 SITE-LITERALO  PIC X(4).                                  NA320M1
      * NA00130                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-SITE-CODEA      PIC X.                                NA320M1
           02 MAP-SITE-CODEO      PIC XX.                               NA320M1
      * NA00130                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-SITE-DESCA      PIC X.                                NA320M1
           02 MAP-SITE-DESCO      PIC X(27).                            NA320M1
      * NA00115                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-EFF-DATEA  PIC X.                                     NA320M1
           02 MAP-EFF-DATEO  PIC X(8).                                  NA320M1
      * NA00160                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-LOGONA     PIC X.                                     NA320M1
           02 MAP-LOGONO     PIC X(22).                                 NA320M1
      * NA00156                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-CHANGE-DATEA    PIC X.                                NA320M1
           02 MAP-CHANGE-DATEO    PIC X(8).                             NA320M1
      * UR00001                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 FIN-LITERALA   PIC X.                                     NA320M1
           02 FIN-LITERALO   PIC X(31).                                 NA320M1
      * NA00161                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-FINALST-CDA     PIC X.                                NA320M1
           02 MAP-FINALST-CDO     PIC X(3).                             NA320M1
      * NA00145                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ASSOC1A    PIC X.                                     NA320M1
           02 MAP-ASSOC1O    PIC X(5).                                  NA320M1
      * NA00145                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ASSOC2A    PIC X.                                     NA320M1
           02 MAP-ASSOC2O    PIC X(5).                                  NA320M1
      * NA00145                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-ASSOC3A    PIC X.                                     NA320M1
           02 MAP-ASSOC3O    PIC X(5).                                  NA320M1
      * NA00010                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-CIMA  PIC X.                                          NA320M1
           02 MAP-CIMO  PIC X(8).                                       NA320M1
      * MU00013                                                         NA320M1
           02 FILLER    PIC X(2).                                       NA320M1
           02 MAP-PF-BYTEA   PIC X.                                     NA320M1
           02 MAP-PF-BYTEO   PIC X.                                     NA320M1
      * UR00001                                                         NA320M1
           02 DFHMS1 OCCURS   4 TIMES .                                 NA320M1
             03 FILLER       PIC X(2).                                  NA320M1
             03 MAP-ERROR-MSGA    PIC X.                                NA320M1
             03 MAP-ERROR-MSGO    PIC X(38).                            NA320M1
