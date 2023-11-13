      *---------------------------------------------------------------*
      *                      'ATTRB'                                  *
      *       COPYBOOK OF ALL POSSIBLE ATTRIBUTE BYTE SETTINGS        *
      *       (IBM'S COPYBOOK LACKS SOME SETTINGS)                    *
      *                                                               *
      *---------------------------------------------------------------*
       01  ATTRB.
           05 ATTRB-UNPROT                       PIC X VALUE SPACE.
           05 ATTRB-UNPROT-FSET                  PIC X VALUE 'A'.
           05 ATTRB-UNPROT-PEN                   PIC X VALUE 'D'.
           05 ATTRB-UNPROT-PEN-FSET              PIC X VALUE 'E'.
           05 ATTRB-UNPROT-BRT-PEN               PIC X VALUE 'H'.
           05 ATTRB-UNPROT-BRT-PEN-FSET          PIC X VALUE 'I'.
           05 ATTRB-UNPROT-DRK                   PIC X VALUE '<'.
           05 ATTRB-UNPROT-DRK-FSET              PIC X VALUE '('.
           05 ATTRB-PROT                         PIC X VALUE '-'.
           05 ATTRB-PROT-FSET                    PIC X VALUE '/'.
           05 ATTRB-PROT-PEN                     PIC X VALUE 'U'.
           05 ATTRB-PROT-PEN-FSET                PIC X VALUE 'V'.
           05 ATTRB-PROT-BRT-PEN                 PIC X VALUE 'Y'.
           05 ATTRB-PROT-BRT-PEN-FSET            PIC X VALUE 'Z'.
           05 ATTRB-PROT-DRK                     PIC X VALUE '%'.
           05 ATTRB-PROT-DRK-FSET                PIC X VALUE '_'.
           05 ATTRB-PROT-ASKIP                   PIC X VALUE '0'.
           05 ATTRB-PROT-ASKIP-FSET              PIC X VALUE '1'.
           05 ATTRB-PROT-ASKIP-PEN               PIC X VALUE '4'.
           05 ATTRB-PROT-ASKIP-PEN-FSET          PIC X VALUE '5'.
           05 ATTRB-PROT-ASKIP-BRT-PEN           PIC X VALUE '8'.
           05 ATTRB-PROT-ASKIP-BRT-PEN-FSET      PIC X VALUE '9'.
           05 ATTRB-PROT-ASKIP-DRK               PIC X VALUE '@'.
           05 ATTRB-PROT-ASKIP-DRK-FSET          PIC X VALUE QUOTE.
