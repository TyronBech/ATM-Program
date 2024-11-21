       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM-MACHINE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT USERDATA ASSIGN TO
       "files.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY F-PIN.
       DATA DIVISION.
       FILE SECTION.
       FD USERDATA.
       01 F-DATA.
         02 F-PIN PIC 9(20) VALUE ZEROES.
         02 F-BAL PIC 9(12).
         02 F-NAME PIC X(20) VALUE SPACES.
         02 F-AGE PIC Z9 VALUE ZEROES.
       WORKING-STORAGE SECTION.
       01 WS-GEN-PIN PIC 9(6) VALUE ZEROES.
       01 WS-IS-EXISTS PIC 9.
       01 WS-CHOICE PIC Z.
       01 WS-MAIN-CHOICE PIC Z.
       01 WS-AMOUNT PIC -ZZZ,ZZZ,ZZZ,ZZ9.
       01 WS-C-AMOUNT PIC 9(12) VALUE ZEROES.
       01 WS-DEPOSIT PIC 9(12) VALUE ZEROES.
       01 WS-WITHDRAW PIC 9(12) VALUE ZEROES.
       01 WS-QUIT PIC X.
       01 WS-FLAG PIC 9 VALUE 1.
       01 WS-IS-VALID PIC X VALUE "Y".
       01 WS-IDX PIC 99 VALUE 1.
       01 WS-CHAR PIC X VALUE SPACE.
       01 I PIC 9(3) VALUE ZEROES.
       01 J PIC 9(3) VALUE ZEROES.
       01 GEN-NUM PIC 9(3) VALUE ZEROES.
       01 GEN-COL PIC 9 VALUE ZERO.
       01 WS-MES PIC X(8) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN I-O USERDATA.
           PERFORM UNTIL WS-MAIN-CHOICE IS EQUAL TO 3
             DISPLAY " " ERASE SCREEN
             PERFORM P-BOARDER
             PERFORM P-STARS
             DISPLAY "ATM MACHINE COBOL PROGRAM" AT 0647
             FOREGROUND-COLOR 3
             DISPLAY "1 - SIGN-IN" AT 0850
             DISPLAY "2 - LOG-IN" AT 0950
             DISPLAY "3 - EXIT" AT 1050
             DISPLAY "ENTER YOUR CHOICE:" AT 1150
             ACCEPT WS-MAIN-CHOICE AT 1169
             MOVE 0 TO F-BAL
             ADD 1 TO F-BAL
             EVALUATE WS-MAIN-CHOICE
               WHEN 1 PERFORM SIGN-IN
               WHEN 2 PERFORM LOG-IN
               WHEN 3 DISPLAY " "
               WHEN OTHER PERFORM P-INVALID
             END-EVALUATE
           END-PERFORM.
           CLOSE USERDATA.
           DISPLAY " " ERASE SCREEN.
           STOP RUN.
       SIGN-IN.
           DISPLAY " " ERASE SCREEN.
           PERFORM P-BOARDER.
           PERFORM P-STARS.
           DISPLAY "SIGN-IN SECTION" AT 0653 FOREGROUND-COLOR 3.
           PERFORM UNTIL WS-FLAG > 3
             MOVE "Y" TO WS-IS-VALID
             DISPLAY "ENTER YOUR NAME:" AT 0850
             ACCEPT F-NAME AT 0867
             PERFORM P-VALID-NAME
             IF WS-IS-VALID IS EQUAL TO "Y" THEN
               EXIT PERFORM
             END-IF
             ADD 1 TO WS-FLAG
           END-PERFORM.
           IF WS-IS-VALID IS EQUAL TO "Y" THEN
             DISPLAY "ENTER YOUR AGE:" AT 0950
             ACCEPT F-AGE AT 0966
             IF F-AGE IS LESS THAN 18 THEN
               DISPLAY "YOU ARE NOT OLD ENOUGH TO ENTER" AT 1144
               FOREGROUND-COLOR 4
               MOVE SPACES TO F-NAME
               MOVE ZERO TO F-AGE
             ELSE
              COMPUTE WS-GEN-PIN = FUNCTION RANDOM * (99999 + 1) + 99999
               DISPLAY "GENERATED PIN:" AT 1050
               DISPLAY WS-GEN-PIN AT 1065
               MOVE WS-GEN-PIN TO F-PIN
               MOVE ZERO TO F-BAL
               WRITE F-DATA
                 INVALID KEY DISPLAY
                 "SOMETHING WENT WRONG, PLEASE TRY AGAIN" AT 1341
               END-WRITE
             END-IF
           ELSE
             DISPLAY "YOU HAVE TRIED MANY TIMES" AT 1447
             FOREGROUND-COLOR 4
             DISPLAY "SIGN-IN TERMINATED" AT 1551 FOREGROUND-COLOR 4
           END-IF.
           PERFORM P-PAUSE.
           MOVE 1 TO WS-FLAG.
           MOVE "Y" TO WS-IS-VALID.
           EXIT.
       P-VALID-NAME.
           PERFORM UNTIL WS-IDX > LENGTH OF F-NAME
             MOVE F-NAME(WS-IDX:1) TO WS-CHAR
             IF WS-CHAR IS NUMERIC THEN
               MOVE "N" TO WS-IS-VALID
             END-IF
               IF WS-IS-VALID IS EQUAL TO "N" THEN
                 DISPLAY "NAME CONTAINS NUMERIC CHARACTER" AT 1145
                 FOREGROUND-COLOR 4
                 DISPLAY "PLEASE TRY AGAIN" AT 1252 FOREGROUND-COLOR 4
                 PERFORM P-PAUSE
                 DISPLAY "                         " AT 0867
                 DISPLAY "                                 " AT 1144
                 DISPLAY "                  " AT 1251
                 DISPLAY "                                " AT 1843
                 EXIT PERFORM
               END-IF
               ADD 1 TO WS-IDX
           END-PERFORM.
           MOVE 1 TO WS-IDX.
           EXIT.
       LOG-IN.
           SET WS-IS-EXISTS TO 0.
           DISPLAY " " ERASE SCREEN.
           PERFORM P-BOARDER.
           PERFORM P-STARS.
           DISPLAY "LOG-IN SECTION" AT 0653 FOREGROUND-COLOR 3.
           DISPLAY "ENTER YOUR PIN:" AT 0950.
           ACCEPT WS-GEN-PIN AT 0966 NO ECHO.
           MOVE WS-GEN-PIN TO F-PIN.
           READ USERDATA
             INVALID KEY MOVE 1 TO WS-IS-EXISTS
           END-READ.
           IF WS-IS-EXISTS IS EQUAL TO 1 THEN
             DISPLAY "YOU ENTERED WRONG PASSWORD" AT 1146
             FOREGROUND-COLOR 4
             PERFORM P-PAUSE
           ELSE
             MOVE F-BAL TO WS-AMOUNT
             PERFORM ATM UNTIL WS-CHOICE IS EQUAL TO 4
             MOVE ZEROES TO F-PIN
           END-IF.
           MOVE ZERO TO WS-CHOICE.
           EXIT.
       ATM.
           DISPLAY " " ERASE SCREEN.
           PERFORM P-BOARDER.
           PERFORM P-STARS.
           DISPLAY "ATM CONSOLE PROGRAM" AT 0650 FOREGROUND-COLOR 3.
           DISPLAY "WELCOME:" AT 0750.
           DISPLAY F-NAME AT 0759 FOREGROUND-COLOR 6.
           DISPLAY "1 - BALANCE" AT 0952.
           DISPLAY "2 - DEPOSIT" AT 1052.
           DISPLAY "3 - WITHDRAW" AT 1152.
           DISPLAY "4 - EXIT" AT 1252.
           DISPLAY "ENTER YOUR CHOICE:" AT 1352.
           ACCEPT WS-CHOICE AT 1371.
           EVALUATE WS-CHOICE
               WHEN 1 PERFORM P-BALANCE
               WHEN 2 PERFORM P-DEPOSIT
               WHEN 3 PERFORM P-WITHDRAW
               WHEN 4 DISPLAY " "
               WHEN OTHER PERFORM P-INVALID
           END-EVALUATE.
           PERFORM P-PAUSE.
           EXIT.
       P-BALANCE.
           DISPLAY " " ERASE SCREEN.
           PERFORM P-BOARDER.
           PERFORM P-STARS.
           DISPLAY "BALANCE SECTION" AT 0652 FOREGROUND-COLOR 3.
           DISPLAY "CURRENT BALANCE: " AT 0842.
           MOVE F-BAL TO WS-AMOUNT.
           DISPLAY WS-AMOUNT AT 0860.
           EXIT.
       P-DEPOSIT.
           DISPLAY " " ERASE SCREEN.
           PERFORM P-BOARDER.
           PERFORM P-STARS.
           MOVE F-BAL TO WS-AMOUNT.
           MOVE WS-AMOUNT TO WS-DEPOSIT.
           DISPLAY "DEPOSIT SECTION" AT 0652 FOREGROUND-COLOR 3.
           DISPLAY "ENTER THE AMOUNT TO DEPOSIT:" AT 0842.
           ACCEPT WS-AMOUNT AT 0871.
           MOVE WS-AMOUNT TO WS-C-AMOUNT.
           COMPUTE WS-DEPOSIT = WS-DEPOSIT + WS-C-AMOUNT.
           MOVE WS-DEPOSIT TO F-BAL.
           REWRITE F-DATA
           END-REWRITE.
           MOVE "DEPOSIT" TO WS-MES.
           PERFORM P-BUNNY.
           EXIT.
       P-WITHDRAW.
           DISPLAY " " ERASE SCREEN.
           PERFORM P-BOARDER.
           PERFORM P-STARS.
           DISPLAY "WITHDRAW SECTION" AT 0651 FOREGROUND-COLOR 3.
           DISPLAY "ENTER THE AMOUNT TO WITHDRAW:" AT 0841.
           ACCEPT WS-AMOUNT AT 0873.
           MOVE WS-AMOUNT TO WS-WITHDRAW.
           IF WS-WITHDRAW IS LESS THAN 0 THEN
               DISPLAY "INVALID AMOUNT" FOREGROUND-COLOR 4
           ELSE IF WS-WITHDRAW IS GREATER THAN F-BAL THEN
               DISPLAY "INSUFFICIENT BALANCE" AT 0949 FOREGROUND-COLOR 4
           ELSE
               SUBTRACT WS-WITHDRAW FROM F-BAL
               GIVING F-BAL
               REWRITE F-DATA
               END-REWRITE
               MOVE "WITHDRAW" TO WS-MES
               PERFORM P-BUNNY
           END-IF.
           EXIT.
       P-PAUSE.
           DISPLAY "PRESS ENTER KEY TO CONTINUE..." AT 1844.
           ACCEPT WS-QUIT AT 1874.
           EXIT.
       P-INVALID.
           DISPLAY "THAT IS A INVALID CHOICE" AT 1647
           PERFORM P-PAUSE
           EXIT.
       P-BOARDER.
           PERFORM VARYING I FROM 15 BY 1 UNTIL I > 105
             DISPLAY "*" AT LINE 004 POSITION I
           END-PERFORM.
           PERFORM VARYING I FROM 5 BY 1 UNTIL I > 26
             DISPLAY "*" AT LINE I POSITION 15
             DISPLAY "*" AT LINE I POSITION 105
           END-PERFORM.
           PERFORM VARYING I FROM 15 BY 1 UNTIL I > 105
             DISPLAY "*" AT LINE 26 POSITION I
           END-PERFORM.
           EXIT.
       P-STARS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= 30
             PERFORM VARYING J FROM 1 BY 1 UNTIL J >= 7
               COMPUTE GEN-NUM = FUNCTION RANDOM * (118 + 1) + 1
               COMPUTE GEN-COL = FUNCTION RANDOM * (6 + 1) + 1
               IF (GEN-NUM < 15 OR GEN-NUM > 105) OR
                 (I < 4 OR I > 26) THEN
                 DISPLAY "+" AT LINE I POSITION GEN-NUM
                 FOREGROUND-COLOR GEN-COL
               END-IF
             END-PERFORM
           END-PERFORM.
           EXIT.
       P-BUNNY.
           DISPLAY "   /\_/\" AT 2168.
           DISPLAY " _(,`-`,)__________________________" AT 2268.
           DISPLAY "|   U U                            |" AT 2368.
           DISPLAY "|   THANK YOU FOR YOUR" AT 2468
           DISPLAY WS-MES AT 2491.
           DISPLAY "    |" AT 2499.
           DISPLAY "|__________________________________|" AT 2568.
           EXIT.
       END PROGRAM ATM-MACHINE.
