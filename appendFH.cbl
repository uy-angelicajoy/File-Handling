       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUFILEHANDLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "student_prof.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STUDENT-NUMBER
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-RECORD.
           05 STUDENT-NUMBER     PIC X(20).
           05 STUDENT-NAME       PIC X(40).
           05 STUDENT-PROGRAM    PIC X(50).
           05 STUDENT-YEAR       PIC 9.
           05 STUDENT-GRADE      PIC X(5).

       WORKING-STORAGE SECTION.
       01 WS-MENU-CHOICE        PIC 9.
       01 WS-CONSENT            PIC X.
       01 WS-FILE-STATUS        PIC XX.
       01 WS-EOF                PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-MENU
           PERFORM UNTIL WS-MENU-CHOICE = 4
               PERFORM CLEAR-SCREEN
               EVALUATE WS-MENU-CHOICE
                   WHEN 1
                       PERFORM CREATE-STUDENT-PROFILE
                   WHEN 2
                       PERFORM DISPLAY-STUDENT-INFORMATION
                   WHEN 3
                       DISPLAY "Exiting program... Thank you!"
                       STOP RUN
                   WHEN OTHER
                       DISPLAY "Invalid choice. Please try again."
               END-EVALUATE
               PERFORM DISPLAY-MENU
           END-PERFORM.
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY "========================================"
           DISPLAY "||          MAIN MENU                ||"
           DISPLAY "========================================"
           DISPLAY "|| 1. Create Student Profile         ||"
           DISPLAY "|| 2. Display Student Information    ||"
           DISPLAY "|| 3. Exit                           ||"
           DISPLAY "========================================"
           DISPLAY "Enter your choice: " NO ADVANCING
           ACCEPT WS-MENU-CHOICE.

       CLEAR-SCREEN.
           DISPLAY X"1B" "[2J"
           DISPLAY X"1B" "[H".

       CREATE-STUDENT-PROFILE.
           OPEN I-O STUDENT-FILE
           IF WS-FILE-STATUS = "35"
               CLOSE STUDENT-FILE
               OPEN OUTPUT STUDENT-FILE
               CLOSE STUDENT-FILE
               OPEN I-O STUDENT-FILE
           END-IF.

           DISPLAY "Student Number: " NO ADVANCING
           ACCEPT STUDENT-NUMBER

           *> Check if record already exists
           READ STUDENT-FILE
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   DISPLAY "Student number already exists!"
                   CLOSE STUDENT-FILE
                   EXIT PARAGRAPH
           END-READ

           DISPLAY "Student Name: " NO ADVANCING
           ACCEPT STUDENT-NAME
           DISPLAY "Program: " NO ADVANCING
           ACCEPT STUDENT-PROGRAM
           DISPLAY "Year Level: " NO ADVANCING
           ACCEPT STUDENT-YEAR
           DISPLAY "Final Grade: " NO ADVANCING
           ACCEPT STUDENT-GRADE

           WRITE STUDENT-RECORD
               INVALID KEY
                   DISPLAY "Error writing record!"
               NOT INVALID KEY
                   DISPLAY "Student profile created successfully."
           END-WRITE

           CLOSE STUDENT-FILE.

       DISPLAY-STUDENT-INFORMATION.
           OPEN INPUT STUDENT-FILE
           IF WS-FILE-STATUS = "35"
               DISPLAY "No records found."
               EXIT PARAGRAPH
           END-IF.

           MOVE "N" TO WS-EOF
           START STUDENT-FILE KEY IS NOT LESS THAN STUDENT-NUMBER
               INVALID KEY
                   MOVE "Y" TO WS-EOF
           END-START

           PERFORM UNTIL WS-EOF = "Y"
               READ STUDENT-FILE NEXT RECORD
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       DISPLAY "=================================="
                       DISPLAY "Student Number: " STUDENT-NUMBER
                       DISPLAY "Student Name  : " STUDENT-NAME
                       DISPLAY "Program      : " STUDENT-PROGRAM
                       DISPLAY "Year Level   : " STUDENT-YEAR
                       DISPLAY "Final Grade  : " STUDENT-GRADE
               END-READ
           END-PERFORM

           CLOSE STUDENT-FILE.
