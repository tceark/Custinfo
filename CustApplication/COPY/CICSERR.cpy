       9999-CICS-ERROR.

           DISPLAY 'UNHANDLED CICS ERROR CONDITION'.

           EXEC CICS RETURN
                     END-EXEC.

000112*    EXEC CICS ASSIGN
000112*        PROGRAM(CICS-PROGID)
000112*    END-EXEC.

      *    EXEC CICS XCTL PROGRAM('ER001')
      *                   COMMAREA(CICS-ERROR-COMM-AREA)
990629*                   LENGTH(LENGTH OF CICS-ERROR-COMM-AREA)
      *                   END-EXEC.
