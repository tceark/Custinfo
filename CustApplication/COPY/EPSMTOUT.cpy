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
      * Enterprise COBOL zUnit Test Case Sample EPSMTOUT.cpy           *
      *                                                                *
      * @since   14.1.5.0                                              *
      * @version 14.1.5.0                                              *
      ******************************************************************
      * OUTPUT
          10 EPSPCOM-RETURN-MONTH-PAYMENT
                                      PIC S9(7)V99 COMP.
          10 EPSPCOM-ERRMSG           PIC X(80).
          10 EPSPCOM-PROGRAM-RETCODE  PIC 9(4).
             88 EPS02-REQUEST-SUCCESS VALUE 0.
          10 EPSPCOM-PROGRAM-RETCODE-RDF
                  REDEFINES EPSPCOM-PROGRAM-RETCODE
                                      PIC X(4).
