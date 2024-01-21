       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DATAFILE
       ASSIGN TO
      * Change the directory based on your device
       "C:\Users\tyron\OneDrive\Desktop\COBOL\ATM-MACHINE\Data.txt"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATAFILE.
      * USED TO WRITE ON THE FILE
       01 USERDATA.
          02 USER-ID PIC 9(5).
          02 USER-NAME PIC X(20).
          02 USER-BALANCE PIC 9(12).
       WORKING-STORAGE SECTION.
      * USED TO READ ON THE FILE
       01 WS-USER-DATA.
          02 WS-ID PIC 9(5) VALUE 12345.
          02 WS-NAME PIC X(20) VALUE "TYRON BECHAYDA".
          02 WS-BALANCE PIC 9(12) VALUE 12000.
      * USED TO FLAG HE END OF THE FILE
       01 WS-ENDFILE PIC A.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
               OPEN EXTEND DATAFILE.
               MOVE WS-ID TO USER-ID.
               MOVE WS-NAME TO USER-NAME.
               MOVE WS-BALANCE TO USER-BALANCE.
               WRITE USERDATA.
               CLOSE DATAFILE.
               STOP RUN.
       READ-FILE.
               OPEN INPUT DATAFILE.
               PERFORM UNTIL WS-ENDFILE IS EQUAL TO "Y"
                 READ DATAFILE INTO WS-USER-DATA
                 AT END MOVE "Y" TO WS-ENDFILE
                 NOT AT END DISPLAY WS-USER-DATA
               END-PERFORM.
               CLOSE DATAFILE.
               STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
