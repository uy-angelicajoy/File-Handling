      ******************************************************************
      * Author: JAMES MICHAEL
      * Date: 1/18/25
      * Purpose: PROJECT [INDEXED FILE HANDLING]
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDEX-FILEHANDLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "STUDENTFILE.TXT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STUD-ID
               FILE STATUS IS FILESTATUS.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-RECORD.
           05 STUD-ID          PIC X(5).
           05 STUD-NAME        PIC X(20).
           05 STUD-PGM         PIC X(10).
           05 FILLER           PIC X(5).

       WORKING-STORAGE SECTION.
           01 FILESTATUS       PIC X(2).
           01 WS-OPTION        PIC 9.
           01 WS-EndOfFile     PIC X VALUE 'N'.

       PROCEDURE DIVISION.
           PERFORM MAIN-PROCEDURE.
           STOP RUN.

       MAIN-PROCEDURE.
            PERFORM UNTIL WS-EndOfFile = 'Y'
               *>CALL "SYSTEM" USING "CLS" *> this is for clearing the entirety of the screen #mejares
               DISPLAY "      <PUP-T STUDENT DATABASE >"
               DISPLAY "------------------------------------"
               DISPLAY "1. ADD STUDENT"
               DISPLAY "2. SEARCH STUDENT"
               DISPLAY "3. EXIT"
               DISPLAY "Please enter your desired option: " NO ADVANCING.
               ACCEPT WS-OPTION
              
               EVALUATE WS-OPTION
                   WHEN 1 PERFORM ADD-STUDENT
                   WHEN 2 PERFORM SEARCH-STUDENT
                   WHEN 3 MOVE 'Y' TO WS-EndOfFile
                   WHEN OTHER DISPLAY "INVALID OPTION"
           END-PERFORM.

       ADD-STUDENT.
           OPEN I-O STUDENT-FILE.
           IF FILESTATUS = "35"
               OPEN OUTPUT STUDENT-FILE
               CLOSE STUDENT-FILE
               OPEN I-O STUDENT-FILE
           END-IF.
           DISPLAY "------------------------------------"
           DISPLAY "Enter Student ID: " NO ADVANCING.
           ACCEPT STUD-ID.
           DISPLAY "Enter Student Name: " NO ADVANCING.
           ACCEPT STUD-NAME.
           DISPLAY "Enter Program: " NO ADVANCING.
           ACCEPT STUD-PGM.

           WRITE STUDENT-RECORD.
           IF FILESTATUS NOT = "00"
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY "  Error in Writing Student Record!"
               DISPLAY "------------------------------------"
               DISPLAY " "
           ELSE
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY "    Student Added Successfully!"
               DISPLAY "------------------------------------"
               DISPLAY " "
           END-IF.
           CLOSE STUDENT-FILE.

       SEARCH-STUDENT.
           OPEN I-O STUDENT-FILE.
           DISPLAY "------------------------------------"
           DISPLAY "Enter Student ID to Search: " NO ADVANCING.
           ACCEPT STUD-ID.
           READ STUDENT-FILE KEY IS STUD-ID
               INVALID KEY 
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "   Student not found/Registered!"
                   DISPLAY "------------------------------------"
                   DISPLAY " "
               NOT INVALID KEY 
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "Student Found: " STUD-NAME SPACE STUD-PGM.
                   DISPLAY "------------------------------------"
                   DISPLAY " "
           CLOSE STUDENT-FILE.
           IF FILESTATUS NOT = "00"
               DISPLAY "Error in Reading Student Record!"
           END-IF.
