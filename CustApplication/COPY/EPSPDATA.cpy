      ******************************************************************
      * Licensed materials - Property of IBM                           *
      * 5724-T07(C) Copyright IBM Corp. 2018                           *
      * All rights reserved                                            *
      * US Government users restricted rights  -  Use, duplication or  *
      * disclosure restricted by GSA ADP schedule contract with IBM    *
      * Corp.                                                          *
      *                                                                *
      * IBM Developer for z/OS (IDz)                                   *
      * IBM z/OS Automated Unit Testing Framework (zUnit)              *
      * Enterprise COBOL zUnit Test Case Sample EPSPDATA.cpy           *
      *                                                                *
      * @since   14.1.5.0                                              *
      * @version 14.1.5.0                                              *
      ******************************************************************
       01  EPSPDATA.
      * INPUT
           03 EPSPDATA-PRINCIPLE-DATA   PIC S9(9)V99 COMP.
           03 EPSPDATA-NUMBER-OF-YEARS  PIC S9(4)    COMP.
           03 EPSPDATA-NUMBER-OF-MONTHS PIC S9(4)    COMP.
           03 EPSPDATA-QUOTED-INTEREST-RATE
                                        PIC S9(2)v9(3) COMP.
           03 EPSPDATA-YEAR-MONTH-IND   PIC X.
      * OUTPUT
           03 EPSPDATA-RETURN-MONTH-PAYMENT
                                        PIC S9(7)V99 COMP.
           03 EPSPDATA-RETURN-ERROR     PIC X(80).
