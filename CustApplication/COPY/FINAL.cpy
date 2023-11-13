       01  FINAL-CALL-AREA.                                             00000310
           05  FINAL-FILLER.                                            00000320
               10  FILLER   PIC S9(5) COMP SYNC VALUE -1.               00000330
               10  FILLER   PIC S9(5) COMP SYNC VALUE -1.               00000340
               10  FILLER   PIC S9(5) COMP SYNC VALUE -1.               00000350
                                                                        00000360
           05  FINAL-FUNCTION-AREA.                                     00000370
               10  FINAL-FUNCTION-CODE        PIC  X(01).               00000380
               10  FINAL-FUNCTION-OPTION      PIC  X(03).               00000390
                                                                        00000400
           05  CALL-INIT.                                               00000410
               10 INITMOD                     PIC  X(08) VALUE SPACES.  00000420
               10 INITBUFF                    PIC S9(05) COMP SYNC.     00000430
                                                                        00000440
           05  CALL-CICS  REDEFINES CALL-INIT.                          00000450
               10 CAMODNAM                    PIC  X(08).               00000460
               10 CAEXCPSW                    PIC  X(01).               00000470
               10 FILLER                      PIC  X(03).               00000480
                                                                        00000490
           05  CALL-IMS   REDEFINES CALL-INIT.                          00000500
               10  IMS-INITMOD                PIC  X(08).               00000510
               10  FILLER REDEFINES IMS-INITMOD.                        00000520
                   15 IMS-INITDAT             PIC S9(05) COMP SYNC.     00000530
                   15 IMS-INITCITY            PIC S9(05) COMP SYNC.     00000540
               10 IMS-ALT-BLKSIZE             PIC S9(05) COMP SYNC.     00000550
                                                                        00000560
           05  FINAL-CNFIG-ID                 PIC  X(08).               00000570
           05  FINAL-INIT-CITY   REDEFINES FINAL-CNFIG-ID.              00000580
               10 INITCITY                    PIC  X(01).               00000590
               10 FILLER                      PIC  X(07).               00000600
           05  FINAL-TAILOR-OPTS REDEFINES FINAL-CNFIG-ID.              00000610
               10 FINAL-UNIQUE-OPT            PIC  X(01).               00000620
               10 FINAL-STRTPHON-OPT          PIC  X(01).               00000630
               10 FINAL-FIRMCORR-OPT          PIC  X(01).               00000640
               10 FINAL-CITYPHON-OPT          PIC  X(01).               00000650
               10 FINAL-WEIGHT-OPT            PIC  X(01).               00000660
               10 FINAL-ZIPCORR-OPT           PIC  X(01).               00000670
               10 FINAL-CITYCORR-OPT          PIC  X(01).               00000680
               10 FINAL-STRCOSM-OPT           PIC  X(01).               00000690
           05  FINAL-FRMPRS-OPT               PIC  X(01).               00000700
           05  FINAL-UNITDES-OPT              PIC  X(01).               00000710
           05  FINAL-CTYLONG-OPT              PIC  X(01).               00000720
           05  FINAL-ALSLBL-OPT               PIC  X(01).               00000730
           05  SYS-NUMBER-REPORTS.                                      00000740
               10 FINAL-VSE1-PRT              PIC  X(03) VALUE '021'.   00000750
               10 FINAL-VSE2-PRT              PIC  X(03) VALUE '022'.   00000760
               10 FINAL-VSE3-PRT              PIC  X(03) VALUE '023'.   00000770
               10 FINAL-VSE4-PRT              PIC  X(03) VALUE '024'.   00000780
               10 FINAL-VSE5-PRT              PIC  X(03) VALUE '025'.   00000790
               10 FINAL-VSE6-PRT              PIC  X(03) VALUE '026'.   00000800
               10 FINAL-VSE7-PRT              PIC  X(03) VALUE '027'.   00000810
                                                                        00000820
           05  FINAL-EXPANDED-INFO.                                     00000830
               10  FINAL-STAT-REPORT-OPTIONS  PIC  X(08) VALUE 'Y'.     00000840
               10  FINAL-REPORT-CALL-AREA.                              00000850
                   15  FINAL-REPORT-SELECT1   PIC  X(01) VALUE 'Y'.     00000860
                   15  FINAL-REPORT-SELECT2   PIC  X(01) VALUE 'Y'.     00000870
                   15  FINAL-REPORT-SELECT3   PIC  X(01) VALUE 'Y'.     00000880
                   15  FINAL-REPORT-SELECT4   PIC  X(01) VALUE 'Y'.     00000890
                   15  FINAL-REPORT-SELECT5   PIC  X(01) VALUE 'Y'.     00000900
                   15  FINAL-REPORT-SELECT6   PIC  X(01) VALUE 'Y'.     00000910
                   15  FINAL-FIRMLBL-OPT      PIC  X(01) VALUE 'I'.     00000920
                   15  FILLER                 PIC  X(01).               00000930
                   15  FINAL-REPORT-NTH       PIC  9(04).               00000940
                   15  FINAL-REPORT-TITLE     PIC  X(35)                00000950
                       VALUE 'PITNEY BOWES SOFTWARE SYSTEMS'.           00000960
               10  FILLER  REDEFINES FINAL-REPORT-CALL-AREA.            00000970
                   15  FILLER                 PIC  X(07).               00000980
                   15  FINAL-REPORT-KEY       PIC  X(40).               00000990
               10  FINAL-REPORT1-MAX          PIC S9(09) COMP SYNC      00001000
                                                        VALUE +1000.    00001010
               10  FINAL-REPORT2-MAX          PIC S9(09) COMP SYNC      00001020
                                                        VALUE +1000.    00001030
               10  FINAL-REPORT3-MAX          PIC S9(09) COMP SYNC      00001040
                                                        VALUE +1000.    00001050
               10  FINAL-REPORT4-MAX          PIC S9(09) COMP SYNC      00001060
                                                        VALUE +1000.    00001070
               10  FINAL-REPORT5-MAX          PIC S9(09) COMP SYNC      00001080
                                                        VALUE +1000.    00001090
               10  FINAL-REPORT6-MAX          PIC S9(09) COMP SYNC      00001100
                                                        VALUE +1000.    00001110
               10  FINAL-NUMBER-OF-LINES      PIC S9(04) COMP SYNC      00001120
                                                        VALUE +57.      00001130
               10  FINAL-3553-SECTIONS.                                 00001140
                   15  FINAL-3553-SECB1       PIC  X(26).               00001150
                   15  FINAL-3553-SECB4       PIC  X(26).               00001160
                   15  FINAL-3553-SECB5       PIC  X(23).               00001170
                   15  FINAL-3553-SECD2A      PIC  X(38).               00001180
                   15  FINAL-3553-SECD2B      PIC  X(38).               00001190
                   15  FINAL-3553-SECD2C      PIC  X(38).               00001200
                   15  FINAL-3553-SECD2D      PIC  X(38).               00001210
                   15  FINAL-3553-SECD2E      PIC  X(38).               00001220
                   15  FINAL-3553-SECD2F      PIC  X(38).               00001230
               10  FINAL-INPUT-ADDR-AREA REDEFINES FINAL-3553-SECTIONS. 00001240
                   15  USER-INPUT-FIRM-LINE   PIC  X(70).               00001250
                   15  USER-INPUT-ADDRESS-1   PIC  X(70).               00001260
                   15  USER-INPUT-ADDRESS-2   PIC  X(70).               00001270
                   15  USER-INPUT-CSZ-AREA.                             00001280
                       20  USER-INPUT-CITY-STATE   PIC  X(70).          00001290
                       20  USER-INPUT-ZIP          PIC  X(05).          00001300
                       20  USER-INPUT-SEC-SEG      PIC  X(04).          00001310
                       20  USER-INPUT-CR           PIC  X(05).          00001320
                   15  FILLER                 PIC  X(09).               00001330
               10  Z4-RESERVED                PIC  X(120).              00001340
               10  FILLER                     PIC  X(83).               00001350
                                                                        00001360
           05  FINAL-ORIGINAL-RETURN-AREA.                              00001370
               10  FINAL-ONLINE-RETURN-CODE   PIC  X(01).               00001380
               10  FINAL-OLD-RETURN-CODE2     PIC  X(01).               00001390
               10  FINAL-OLD-RETURN-CODE3     PIC  X(01).               00001400
               10  FINAL-ISOL                 PIC  X(01).               00001410
               10  FINAL-RECORD-NUMBER        PIC S9(09) COMP-3.        00001420
               10  FINAL-ZIP.                                           00001430
                   15  FINAL-SCF              PIC  X(03).               00001440
                   15  FINAL-ZONE             PIC  X(02).               00001450
               10  FINAL-SEC-SEG              PIC  X(04).               00001460
               10  FINAL-CR-RTE               PIC  X(05).               00001470
               10  FINAL-STATE                PIC  X(02).               00001480
               10  FINAL-CITY                 PIC  X(13).               00001490
               10  FINAL-COUNTY               PIC  X(05).               00001500
               10  FINAL-DIRECTION-1.                                   00001510
                   15  FINAL-DIR11            PIC  X(02).               00001520
                   15  FINAL-SFX11            PIC  X(04).               00001530
                   15  FINAL-SFX12            PIC  X(04).               00001540
                   15  FINAL-PDIR1            PIC  X(02).               00001550
               10  FINAL-DIRECTION-2.                                   00001560
                   15  FINAL-DIR21            PIC  X(02).               00001570
                   15  FINAL-SFX21            PIC  X(04).               00001580
                   15  FINAL-SFX22            PIC  X(04).               00001590
                   15  FINAL-PDIR2            PIC  X(02).               00001600
               10  FINAL-DIRECTION-3.                                   00001610
                   15  FINAL-DIR31            PIC  X(02).               00001620
                   15  FINAL-SFX31            PIC  X(04).               00001630
                   15  FINAL-SFX32            PIC  X(04).               00001640
                   15  FINAL-PDIR3            PIC  X(02).               00001650
               10  FILLER                     PIC  X(04).               00001660
               10  FINAL-HOUSE-NUM            PIC  X(10).               00001670
               10  FINAL-PRE-DIR              PIC  X(02).               00001680
               10  FINAL-STREET-NAME          PIC  X(28).               00001690
               10  FINAL-CICS-ERROR REDEFINES FINAL-STREET-NAME.        00001700
                   15  CAERRMOD               PIC  X(08).               00001710
                   15  CAERRSRC               PIC  X(08).               00001720
                   15  CAERRDSC               PIC  X(09).               00001730
                   15  FILLER                 PIC  X(03).               00001740
               10  FINAL-POST-DIR             PIC  X(02).               00001750
               10  FINAL-SFX1                 PIC  X(04).               00001760
               10  FINAL-SFX2                 PIC  X(04).               00001770
               10  FINAL-APT-NUM              PIC  X(10).               00001780
               10  FINAL-EXTRA                PIC  X(09).               00001790
               10  ORIG-ZIP.                                            00001800
                   15  ORIG-SCF               PIC  X(03).               00001810
                   15  ORIG-ZONE              PIC  X(02).               00001820
               10  ORIG-SEC-SEG               PIC  X(04).               00001830
               10  ORIG-CR-RTE                PIC  X(05).               00001840
               10  ORIG-STATE                 PIC  X(02).               00001850
               10  ORIG-CITY                  PIC  X(28).               00001860
               10  ORIG-ZIPP                  PIC S9(05) COMP-3.        00001870
               10  ORIG-SECSEGP               PIC S9(05) COMP-3.        00001880
               10  FINAL-STATE-SCF-VER        PIC  X(01).               00001890
               10  FINAL-RETSCF               PIC  X(03).               00001900
               10  FILLER                     PIC  X(24).               00001910
               10  FINAL-UNITDES              PIC  X(04).               00001920
               10  FILLER                     PIC  X(03).               00001930
                                                                        00001940
           05  FINAL-EXPANDED-RETURN-AREA.                              00001950
               10  FINAL-OUTPUT-SELECT.                                 00001960
                   15 FINAL-OUTSEL-GOOD       PIC  X(01).               00001970
                   15 FINAL-OUTSEL-BAD        PIC  X(01).               00001980
                   15 FINAL-OUTSEL-CHANGE     PIC  X(01).               00001990
               10  FINAL-RETURN-CODES.                                  00002000
                   15  FINAL-RETURN-CODE1     PIC  X(01).               00002010
                   15  FILLER                 PIC  X(01).               00002020
               10  FINAL-REASON-CODES.                                  00002030
                   15  FINAL-REASON-CODE1     PIC  X(01).               00002040
                   15  FINAL-REASON-CODE2     PIC  X(01).               00002050
                   15  FINAL-REASON-CODE3     PIC  X(01).               00002060
                   15  FINAL-REASON-CODE4     PIC  X(01).               00002070
                   15  FINAL-REASON-CODE5     PIC  X(01).               00002080
                   15  FINAL-REASON-CODE6     PIC  X(01).               00002090
                   15  FINAL-REASON-CODE7     PIC  X(01).               00002100
                   15  FINAL-REASON-CODE8     PIC  X(01).               00002110
                   15  FINAL-REASON-CODE9     PIC  X(01).               00002120
                   15  FINAL-REASON-CODE10    PIC  X(01).               00002130
                   15  FILLER                 PIC  X(02).               00002140
               10  FINAL-ADDRESS-INFO-CODES.                            00002150
                   15  FINAL-INFO-CODE1       PIC  X(01).               00002160
                   15  FINAL-INFO-CODE2       PIC  X(01).               00002170
                   15  FINAL-INFO-CODE3       PIC  X(01).               00002180
                   15  FINAL-INFO-CODE4       PIC  X(01).               00002190
                   15  FINAL-INFO-CODE5       PIC  X(01).               00002200
                   15  FINAL-INFO-CODE678.                              00002210
                     20 FINAL-INFO-CODE6      PIC  X(01).               00002220
                     20 FINAL-INFO-CODE7      PIC  X(01).               00002230
                     20 FINAL-INFO-CODE8      PIC  X(01).               00002240
                   15  FILLER                 PIC  X(02).               00002250
               10  FINAL-5D-BARCODE.                                    00002260
                   15  FINAL-5D-BEG           PIC  X(01).               00002270
                   15  FINAL-5D-ZIP           PIC  X(05).               00002280
                   15  FINAL-5D-CKDIGIT       PIC  X(01).               00002290
                   15  FINAL-5D-END           PIC  X(01).               00002300
               10  FINAL-BARCODE.                                       00002310
                   15  FINAL-BC-BEG           PIC  X(01).               00002320
                   15  FINAL-BC-ZIP           PIC  X(05).               00002330
                   15  FINAL-BC-ZIP4          PIC  X(04).               00002340
                   15  FINAL-BC-CKDIGIT       PIC  X(01).               00002350
                   15  FINAL-BC-END           PIC  X(01).               00002360
               10  FINAL-ADVANCED-BARCODE.                              00002370
                   15  FINAL-ABC-BEG          PIC  X(01).               00002380
                   15  FINAL-ABC-ZIP          PIC  X(05).               00002390
                   15  FINAL-ABC-ZIP4         PIC  X(04).               00002400
                   15  FINAL-ABC-WSEQ         PIC  X(02).               00002410
                   15  FINAL-ABC-CKDIGIT      PIC  X(01).               00002420
                   15  FINAL-ABC-END          PIC  X(01).               00002430
               10  FINAL-ALT-ISOLATION.                                 00002440
                   15  FINAL-ALT-RANGE        PIC  X(10).               00002450
                   15  FINAL-ALT-PRE-DIR      PIC  X(02).               00002460
                   15  FINAL-ALT-STREET       PIC  X(25).               00002470
                   15  FINAL-ALT-SUFFIX       PIC  X(04).               00002480
                   15  FINAL-ALT-POST-DIR     PIC  X(02).               00002490
                   15  FINAL-ALT-UNIT         PIC  X(06).               00002500
                   15  FILLER REDEFINES FINAL-ALT-UNIT.                 00002510
                     20  FILLER               PIC  X(02).               00002520
                     20  FINAL-ALT-UNIT-4BYTE PIC  X(04).               00002530
                   15  FINAL-ALT-RANGE2       PIC  X(10).               00002540
               10  FINAL-FULL-CITY-NAME       PIC  X(25).               00002550
               10  FINAL-LABEL-RETURN-AREA.                             00002560
                   15  FINAL-LABEL-RC         PIC  X(01).               00002570
                   15  FILLER                 PIC  X(01).               00002580
                   15  FINAL-FIRM-LENGTH1     PIC  9(02).               00002590
                   15  FINAL-FIRM-LINE1       PIC  X(70).               00002600
                   15  FILLER                 PIC  X(01).               00002610
                   15  FINAL-LABEL-LENGTH1    PIC  9(02).               00002620
                   15  FINAL-LABEL-LINE1      PIC  X(70).               00002630
                   15  FILLER                 PIC  X(01).               00002640
                   15  FINAL-LABEL-LENGTH2    PIC  9(02).               00002650
                   15  FINAL-LABEL-LINE2      PIC  X(70).               00002660
                   15  FILLER                 PIC  X(01).               00002670
                   15  FINAL-LABEL-LENGTH3    PIC  9(02).               00002680
                   15  FINAL-LABEL-LINE3      PIC  X(70).               00002690
                   15  FILLER                 PIC  X(01).               00002700
                   15  FINAL-ALT-LABEL-LENGTH2 PIC 9(02).               00002710
                   15  FINAL-ALT-LABEL-LINE2   PIC X(70).               00002720
               10  FINAL-MAIL-FIRM-NAME       PIC  X(40).               00002730
               10  FINAL-CITY-VANITY-NAME     PIC  X(13).               00002740
               10  FINAL-JOB-INFO.                                      00002750
                   15  FINAL-TIME-STAMP       PIC  X(08).               00002760
                   15  FINAL-DATE-STAMP       PIC  X(08).               00002770
                   15  FINAL-DATA-FILE-VER    PIC  X(04).               00002780
                   15  FINAL-DATA-FILE-UPDATE PIC  X(08).               00002790
                   15  FINAL-CITY-FILE-VER    PIC  X(04).               00002800
                   15  FINAL-CITY-FILE-UPDATE PIC  X(08).               00002810
                   15  FINAL-PRODUCT-VERSION  PIC  X(03).               00002820
               10  FINAL-BATCH-FILLER         PIC  X(42).               00002830
               10  FINAL-IDMSFLDS   REDEFINES FINAL-BATCH-FILLER.       00002840
                   15  FINAL-IDMSMSG          PIC  X(34).               00002850
                   15  FINAL-IDMSERMD         PIC  X(08).               00002860
