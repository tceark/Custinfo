       01  WT-CNTL0705.                                                 00000010
      *------------------------PROGRAM PURPOSE-------------------------*00000020
      *                                                                *00000030
      *    COPYBOOK TITLE: TURB0705 (FORMERLY CNTL0705)                *00000040
      *    COPYBOOK TEXT: DISTRICT COPYBOOK FOR DISTRICT TABLE         *00000050
      *                                                                *00000060
      *    THIS TABLE CONTAINS DISTRICT CODE AND ASSOICATED            *00000070
      *    REGION FOR GLOBAL DISTRICT ADJUSTMENT.                      *00000080
      *    THE KEY OF THE RECORD IS DISTRICT CODE (4 BYTES).           *00000090
      *----------------------------------------------------------------*00000100
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000310
           05  WT-B0705-CONTROL         PIC X(8) VALUE SPACE.           00000320
           05  WT-B0705-NAME            PIC X(8) VALUE 'TURB0705'.      00000330
           05  WT-B0705-REQUEST         PIC X    VALUE 'S'.             00000340
           05  FILLER                   PIC X    VALUE SPACE.           00000350
           05  WT-B0705-RETURN          PIC S9(4) COMP VALUE +0.        00000360
           05  WT-B0705-KEY-LENGTH      PIC S9(4) COMP VALUE +4.        00000370
           05  WT-B0705-ENTRY-LENGTH    PIC S9(4) COMP VALUE +27.       00000380
           05  WT-B0705-PARM-LENGTH     PIC S9(4) COMP VALUE +80.       00000390
           05  WT-B0705-KEY.                                            00000400
               10 WT-B0705-DIST         PIC 9(4).                       00000410
           05  WT-B0705-REGION-ID       PIC 9(2).                       00000420
           05  WT-B0705-DESCRIPTION     PIC X(25) VALUE SPACE.          00000430
           05  WT-B0705-PARM            PIC X(80) VALUE                 00000440
      *                                                                 00000450
      * IMR CHANGE BEGIN.                                               00000460
      *                                                                 00000470
      *              ' CONTROL DD=IS600V02,M=0705,K=(5,4),D=(41,27),S=B,00000480
      *-         'E=(31,1, )'.                                          00000490
                     ' CONTROL DD=IS600V02,M=0705,K=(5,4),D=(51,27),S=B,00000500
      -          'E=(41,1, )'.                                          00000510
      *                                                                 00000520
      * IMR CHANGE END.                                                 00000530
      *                                                                 00000540
           05  FILLER                  PIC X(8) VALUE 'GSFRB.10'.       00000550
                                                                        00000560
