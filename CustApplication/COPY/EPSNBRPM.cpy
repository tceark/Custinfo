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
      * Enterprise COBOL zUnit Test Case Sample EPSNBRPM.cpy           *
      *                                                                *
      * @since   14.1.5.0                                              *
      * @version 14.1.5.0                                              *
      ******************************************************************
       01  EPS-NUMBER-VALIDATION.
      * INPUT - change 3
           03 EPSPARM-VALIDATE-DATA     PIC X(13).
           03 EPSPARM-MAX-LENGTH        PIC 99.
      * OUTPUT
           03 EPSPARM-NUMBER            PIC 9(13).
           03 EPSPARM-DECIMAL           PIC V9(13).
           03 EPSPARM-BINARY-NUMBER     PIC 9(9)V99 COMP.
           03 EPSPARM-RETURN-ERROR      PIC X(80).
