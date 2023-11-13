      *------------------------PROGRAM PURPOSE-------------------------*00000010
      *                                                                *00000020
      *     COPYBOOK TITLE: TURB0161                                   *00000030
      *     COPYBOOK TEXT:  TABLE COPYBOOK FOR SITE CODE TABLE         *00000040
      *                                                                *00000050
       01  WT-CNTL0161.                                                 00000790
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000800
           05  WT-C0161-CONTROL         PIC X(8) VALUE SPACE.           00000810
           05  WT-C0161-NAME            PIC X(8) VALUE 'TBV30161'.      00000820
           05  WT-C0161-REQUEST         PIC X    VALUE 'S'.             00000830
           05  FILLER                   PIC X    VALUE SPACE.           00000840
           05  WT-C0161-RETURN          PIC S9(4) COMP VALUE +0.        00000850
           05  WT-C0161-KEY-LENGTH      PIC S9(4) COMP VALUE +2.        00000860
           05  WT-C0161-ENTRY-LENGTH    PIC S9(4) COMP VALUE +236.      00000870
           05  WT-C0161-PARM-LENGTH     PIC S9(4) COMP VALUE +81.       00000880
           05  WT-C0161-KEY.                                            00000890
               10 WT-C0161-SITE         PIC X(2).                       00000900
           05  WT-C0161-DATA.                                           00000910
               10  WT-C0161-DEST        PIC X(8).                       00000920
               10  WT-C0161-AREA        PIC X(3).                       00000930
               10  WT-C0161-COMPANY-NAME PIC X(30).                     00000940
               10  WT-C0161-ADDRESS-ONE  PIC X(30).                     00000950
               10  WT-C0161-ADDRESS-TWO  PIC X(30).                     00000960
               10  WT-C0161-CITY         PIC X(20).                     00000970
               10  WT-C0161-STATE-CODE   PIC X(02).                     00000980
               10  WT-C0161-ZIP-CODE     PIC 9(05).                     00000990
               10  WT-C0161-PHONE-IN-ST  PIC 9(10).                     00001000
               10  WT-C0161-PHONE-OUT-ST PIC 9(10).                     00001010
               10  WT-C0161-ZIP-FOUR     PIC 9(04).                     00001020
               10  WT-C0161-FED-ID-NUM   PIC 9(09).                     00001030
               10  WT-C0161-ADMIN-SITE   PIC X(2).                      00001040
               10  WT-C0161-OFFICE-NAME  PIC X(20).                     00001050
               10  WT-C0161-ACCTG-ADDR   PIC X(20).                     00001060
               10  WT-C0161-ACCTG-ZIP    PIC X(10).                     00001070
               10  WT-C0161-CASE-CONSV-PH-INST PIC X(10).               00001080
               10  WT-C0161-CASE-CONSV-PH-OUTST PIC X(10).              00001090
               10  WT-C0161-PROCESS-SITE PIC X(02).                     00001100
               10  WT-C0161-CASH-MENU    PIC X(01).                     00001110
           05  WT-C0161-PARM            PIC X(81) VALUE                 00001120
      *                                                                 00001130
      * IMR CHANGE BEGIN                                                00001140
      *                                                                 00001150
      *           ' CONTROL DD=IS600V02,M=0161,K=(5,2),D=(41,163,294,70,00001160
      *-     '424,2,456,1),S=B,E=(31,1, )'.                             00001170
      *          ' CONTROL DD=IS600V02,M=0161,K=(5,2),D=(51,163,304,70,'00001180
                  ' CONTROL DD=IS600V02,M=0161,K=(5,2),D=(51,163,304,70,00001180
      -           '434,2,466,1),S=B,E=(41,1, )'.                        00001190
      *                                                                 00001200
      * IMR CHANGE END                                                  00001210
      *                                                                 00001220
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00001230
