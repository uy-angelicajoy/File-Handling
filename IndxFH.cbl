      ******************************************************************
      * Author: TECHNEWJEANS
      * Date: 1/18/25
      * Purpose: PROJECT [INDEXED FILE HANDLING]
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDEX-FILEHANDLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "STUDENTFILE.DAT" *> this is for the file name #mejares
               ORGANIZATION IS INDEXED *> this is for the organization of the file #mejares
               ACCESS MODE IS DYNAMIC *> this is for the access mode of the file #mejares
               RECORD KEY IS STUD-ID *> this is for the record key of the file #mejares
               FILE STATUS IS FILESTATUS. *> this is for the file status #mejares

           SELECT CSV-FILE ASSIGN TO "STUDENTFILE.CSV" *> this is for the file name #mejares
               ORGANIZATION IS LINE SEQUENTIAL. *> this is for the organization of the file #mejares

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-PROFILE.
           05 STUD-ID          PIC X(5). *> ALPHA NUMBERIC WITH 5 CHARACTERS #mejares student id
           05 STUD-NAME        PIC X(30). *> ALPHA NUMBERIC WITH 30 CHARACTERS #Mmejares student name
           05 STUD-PROGRAM     PIC X(5). *> ALPHA NUMBERIC WITH 10 CHARACTERS #mejares student program
           05 STUD-YEAR-LVL    PIC X(5). *> ALPHA NUMBERIC WITH 10 CHARACTERS #mejares year lvl
           05 STUD-GPA         PIC X. *> (alpha)NUMBERIC WITH 4 CHARACTERS #mejares student gpaz
       
       FD CSV-FILE.
       01 CSV-RECORD           PIC X(80).

       WORKING-STORAGE SECTION.
           01 FILESTATUS       PIC X(2). 
           01 WS-OPTION        PIC 9. *> ALPHA NUMBERIC WITH 1 CHARACTERS  #meajres
           01 WS-EndOfFile     PIC X VALUE 'N'. *> THE DEFAULT VALUE IS N 
           01 WS-WAITFORINPUT  PIC X.
           01 GREEN            PIC X(5) VALUE X'1B5B33326D'. *> this is for the color green #mejares
           01 RST              PIC X(5) VALUE X'1B5B306D'. *> this is for resetting the color #mejares

       PROCEDURE DIVISION.
           PERFORM MAIN-MENU.
           STOP RUN.

       MAIN-MENU.
           PERFORM UNTIL WS-EndOfFile = 'Y'
               CALL "SYSTEM" USING "CLS" *> this is for clearing the entirety of the screen #mejares
               DISPLAY "===================================="
               DISPLAY GREEN "| PUP-T STUDENT INFORMATION SYSTEM |" RST
               DISPLAY "===================================="
               DISPLAY "|1| CREATE STUDENT PROFILE         |"
               DISPLAY "|2| SEARCH STUDENT                 |"
               DISPLAY "|3| EDIT STUDENT PROFILE           |"
               DISPLAY "|4| DELETE STUDENT BY ID           |"
               DISPLAY "|5| DISPLAY ALL STUDENTS           |"
               DISPLAY "|6| EXPORT TO READABLE FILE/CSV    |"
               DISPLAY "|7| EXIT                           |"
               DISPLAY "===================================="
               DISPLAY GREEN "Enter the no. to go to: " 
                   RST NO ADVANCING
               ACCEPT WS-OPTION
              
               EVALUATE WS-OPTION
                   WHEN 1 PERFORM CREATE-STUDENT-PROFILE
                   WHEN 2 PERFORM SEARCH-STUDENT
                   WHEN 3 PERFORM EDIT-STUDENT-PROFILE
                   WHEN 4 PERFORM DELETE-STUDENT
                   WHEN 5 PERFORM DISPLAY-ALL-STUDENTS
                   WHEN 6 PERFORM EXPORT-TO-CSV
                   WHEN 7 *> this is for exiting the program #mejares
                       DISPLAY " "
                       DISPLAY GREEN 
                           "[SYSTEM] TERMINATING PROGRAM..." RST
                       CLOSE STUDENT-FILE *> CLOSE THE FILE BEFORE EXITING #mejares 
                       MOVE 'Y' TO WS-EndOfFile *> this is for exiting the program #mejares
                   WHEN OTHER 
                       DISPLAY " "
                       DISPLAY GREEN "INVALID OPTION"
                       DISPLAY"[SYSTEM] Press any key to continue..."RST
                       ACCEPT WS-WAITFORINPUT
               END-EVALUATE
           END-PERFORM.

       CREATE-STUDENT-PROFILE. *> 1. CREATE STUDENT PROFILE
           OPEN I-O STUDENT-FILE. *> this is for opening the file #mejares
           IF FILESTATUS = "35" *> this is for checking if the file exists filestatus 35 means file not found #mejares
               OPEN OUTPUT STUDENT-FILE *> this is for creating the file if it does not exist #mejares
               CLOSE STUDENT-FILE *> this is for closing the file #mejares
               OPEN I-O STUDENT-FILE *> this is for opening the file #mejares
           END-IF.

           CALL "SYSTEM" USING "CLS"
           DISPLAY "------------------------------------"
           DISPLAY GREEN "       CREATE STUDENT PROFILE" RST
           DISPLAY "------------------------------------"
           DISPLAY GREEN "    Enter student profile details" RST
           DISPLAY "------------------------------------"
           DISPLAY "Enter Student ID: " NO ADVANCING
           ACCEPT STUD-ID.
           READ STUDENT-FILE *> error handler for duplicate student id #mejares
               INVALID KEY 
                   CONTINUE 
               NOT INVALID 
                   DISPLAY " "
                   DISPLAY GREEN "Student ID Number already exists!"
                   CLOSE STUDENT-FILE
                   DISPLAY "[SYSTEM] Press any key to continue..." RST
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
           END-READ
           DISPLAY "Enter Student Name: " NO ADVANCING
           ACCEPT STUD-NAME.
           DISPLAY "Enter Program: " NO ADVANCING
           ACCEPT STUD-PROGRAM.
           DISPLAY "Enter Year Level: " NO ADVANCING
           ACCEPT STUD-YEAR-LVL.
           DISPLAY "Enter GPA: " NO ADVANCING
           ACCEPT STUD-GPA.

           WRITE STUDENT-PROFILE.
           IF FILESTATUS NOT = "00" *> error handler for writing student profile/file status 00 means succesful file #mejares
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY GREEN "  Error in Writing Student Profile!" RST
               DISPLAY "------------------------------------"
               DISPLAY " "
               DISPLAY GREEN "[SYSTEM] Press any key to continue..." RST
               ACCEPT WS-WAITFORINPUT
           ELSE
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY GREEN "      Student Profile created!" RST
               DISPLAY "------------------------------------"
               DISPLAY " "
               DISPLAY GREEN "[SYSTEM] Press any key to continue..." RST
               ACCEPT WS-WAITFORINPUT
           END-IF.
           CLOSE STUDENT-FILE. *> this is for closing the file #mejares

       SEARCH-STUDENT. *> 2. SEARCH STUDENT
           OPEN I-O STUDENT-FILE. *> this is for opening the file #mejares
           IF FILESTATUS = "35" *> this is for checking if the file exists/filestatus 35 means file not found #mejares 
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY GREEN "           No data found." RST
               DISPLAY "------------------------------------"
               DISPLAY GREEN "[SYSTEM] Press any key to continue..." RST
               ACCEPT WS-WAITFORINPUT
               EXIT PARAGRAPH
           END-IF.

           CALL "SYSTEM" USING "CLS"
           DISPLAY "------------------------------------"
           DISPLAY GREEN"       SEARCH STUDENT PROFILE" RST
           DISPLAY "------------------------------------"
           DISPLAY "Enter Student ID to Search: " NO ADVANCING
           ACCEPT STUD-ID.
           READ STUDENT-FILE KEY IS STUD-ID *> this is for reading the student file #mejares
               INVALID KEY *> error handler for student not found #mejares
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY GREEN "   Student not found/Registered!" RST
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   DISPLAY GREEN 
                       "[SYSTEM] Press any key to continue..." RST
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
               NOT INVALID KEY *> this is for displaying the student profile #mejares
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY GREEN "       Student Profile Found!" RST
                   DISPLAY "------------------------------------"
                   DISPLAY "Student Name: " STUD-NAME.
                   DISPLAY "Student ID: " STUD-ID.
                   DISPLAY "Student Program: " STUD-PROGRAM.
                   DISPLAY "Student Year Level: " STUD-YEAR-LVL.
                   DISPLAY "Student GPA: " STUD-GPA.
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   DISPLAY GREEN 
                       "[SYSTEM] Press any key to continue..." RST
                   ACCEPT WS-WAITFORINPUT
           CLOSE STUDENT-FILE.
           IF FILESTATUS NOT = "00" *> error handler for reading student profile/file status 00 means succesful file #mejares
               DISPLAY " "
               DISPLAY GREEN "Error in Reading Student Profile!"
               DISPLAY "[SYSTEM] Press any key to continue..." RST
               ACCEPT WS-WAITFORINPUT
           END-IF.
           
       EDIT-STUDENT-PROFILE. *> 3. EDIT STUDENT PROFILE
           OPEN I-O STUDENT-FILE.
           IF FILESTATUS = '35' *> this is for checking if the file exists/filestatus 35 means file not found #mejares
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY GREEN "           No data found" RST
               DISPLAY "------------------------------------"
               DISPLAY GREEN "[SYSTEM] Press any key to continue..." RST
               ACCEPT WS-WAITFORINPUT
               EXIT PARAGRAPH
           END-IF.

           CALL "SYSTEM" USING "CLS"
           DISPLAY "------------------------------------"
           DISPLAY GREEN "       EDIT STUDENT PROFILE" RST
           DISPLAY "------------------------------------"
           DISPLAY "Enter  Student ID to edit: " NO ADVANCING
           ACCEPT STUD-ID.
           READ STUDENT-FILE KEY IS STUD-ID
               INVALID KEY 
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY GREEN "       Student ID not found!" RST
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   DISPLAY GREEN 
                       "[SYSTEM] Press any key to continue..." RST
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
               NOT INVALID KEY 
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY GREEN "       Student Profile Found!" RST
                   DISPLAY "------------------------------------"
                   DISPLAY GREEN" Enter new student profile details" RST
                   DISPLAY "------------------------------------"
                   DISPLAY "Enter Student Name: " NO ADVANCING
                   ACCEPT STUD-NAME.
                   DISPLAY "Enter Program: " NO ADVANCING
                   ACCEPT STUD-PROGRAM.
                   DISPLAY "Enter Year Level: " NO ADVANCING
                   ACCEPT STUD-YEAR-LVL.
                   DISPLAY "Enter GPA: " NO ADVANCING
                   ACCEPT STUD-GPA.
                   REWRITE STUDENT-PROFILE.
                   IF FILESTATUS = "00" *> error handler for editing student profile/file status 00 means succesful file #mejares
                       DISPLAY " "
                       DISPLAY "------------------------------------"
                       DISPLAY GREEN "       Student Profile Updated"RST
                       DISPLAY "------------------------------------"
                       DISPLAY " "
                       DISPLAY GREEN 
                           "[SYSTEM] Press any key to continue..." RST
                       ACCEPT WS-WAITFORINPUT
                   ELSE
                       DISPLAY " "
                       DISPLAY "------------------------------------"
                       DISPLAY GREEN "       Error Updating Profile!"RST
                       DISPLAY "------------------------------------"
                       DISPLAY " "
                       DISPLAY GREEN 
                           "[SYSTEM] Press any key to continue..." RST
                       ACCEPT WS-WAITFORINPUT
                   END-IF
                   
           CLOSE STUDENT-FILE.
       
       DELETE-STUDENT. *> 4. DELETE STUDENT BY ID
           OPEN I-O STUDENT-FILE.
           IF FILESTATUS = '35' *> this is for checking if the file exists/filestatus 35 means file not found #mejares
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY GREEN "           No data found" RST
               DISPLAY "------------------------------------"
               DISPLAY GREEN "[SYSTEM] Press any key to continue..." RST
               ACCEPT WS-WAITFORINPUT
               EXIT PARAGRAPH
           END-IF.

           CALL "SYSTEM" USING "CLS"
           DISPLAY "------------------------------------"
           DISPLAY GREEN "       DELETE STUDENT PROFILE" RST
           DISPLAY "------------------------------------"
           DISPLAY "Enter Student ID to delete: " NO ADVANCING
           ACCEPT STUD-ID.
           READ STUDENT-FILE KEY IS STUD-ID
               INVALID KEY 
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY GREEN "       Student ID not found!" RST
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   DISPLAY GREEN 
                       "[SYSTEM] Press any key to continue..." RST
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
               NOT INVALID KEY 
                   DELETE STUDENT-FILE *> this is for deleting the student profile #mejares
                   IF FILESTATUS = "00" *> error handler for deleting student profile/file status 00 means succesful file #mejares
                       DISPLAY " "
                       DISPLAY "------------------------------------"
                       DISPLAY GREEN "       Student Profile Deleted"RST
                       DISPLAY "------------------------------------"
                       DISPLAY " "
                       DISPLAY GREEN 
                           "[SYSTEM] Press any key to continue..." RST
                       ACCEPT WS-WAITFORINPUT
                   ELSE
                       DISPLAY " "
                       DISPLAY "------------------------------------"
                       DISPLAY GREEN "       Error Deleting Profile!"RST
                       DISPLAY "------------------------------------"
                       DISPLAY " "
                       DISPLAY GREEN 
                           "[SYSTEM] Press any key to continue..." RST
                       ACCEPT WS-WAITFORINPUT
                   END-IF

           CLOSE STUDENT-FILE.

       DISPLAY-ALL-STUDENTS. *> 5. DISPLAY ALL STUDENTS
           OPEN INPUT STUDENT-FILE
           IF FILESTATUS = "35" *> this is for checking if the file exists/filestatus 35 means file not found #mejares 
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY GREEN "           No data found." RST
               DISPLAY "------------------------------------"
               DISPLAY GREEN "[SYSTEM] Press any key to continue..." RST
               ACCEPT WS-WAITFORINPUT
               EXIT PARAGRAPH
           END-IF.
           
           CALL "SYSTEM" USING "CLS"
           DISPLAY "------------------------------------"
           DISPLAY GREEN "       PUP-T STUDENTS DATABASE" RST
           DISPLAY "------------------------------------"
           PERFORM UNTIL FILESTATUS = "10" *>10 means it reached end of file #mejares
               READ STUDENT-FILE
                   AT END
                       MOVE "10" TO FILESTATUS *> this is for checking if the file reached the end #mejares
                   NOT AT END
                       DISPLAY GREEN "Student ID   : " STUD-ID
                       DISPLAY "Student Name : " STUD-NAME
                       DISPLAY "Program      : " STUD-PROGRAM
                       DISPLAY "Year Level   : " STUD-YEAR-LVL
                       DISPLAY "GPA          : " STUD-GPA RST
                       DISPLAY "===================================="
               END-READ
           END-PERFORM

           CLOSE STUDENT-FILE.
           DISPLAY " "
           DISPLAY GREEN "[SYSTEM] Press any key to continue..." RST
           ACCEPT WS-WAITFORINPUT.

       EXPORT-TO-CSV. *> 6. EXPORT TO READABLE FILE/CSV
           OPEN OUTPUT CSV-FILE. *>this is for creating the csv file #mejares
           OPEN I-O STUDENT-FILE. *> this is for opening the csv file #mejares
           IF FILESTATUS = "35" *> this is for checking if the file exists/filestatus 35 means file not found #mejares 
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY GREEN "           No data found." RST
               DISPLAY "------------------------------------"
               DISPLAY GREEN "[SYSTEM] Press any key to continue..." RST
               ACCEPT WS-WAITFORINPUT
               CLOSE STUDENT-FILE
               EXIT PARAGRAPH
           END-IF.

           DISPLAY " "
           DISPLAY GREEN 
               "[SYSTEM] Exporting Student Data to CSV File..." RST

           PERFORM UNTIL FILESTATUS = "10" *>perform until the end of the file #mejares
               READ STUDENT-FILE
                   AT END
                       MOVE "10" TO FILESTATUS *>make the filestatus 10 if it reached the end of the file #mejares
                       EXIT PERFORM
                   NOT AT END
                       MOVE STUD-ID TO CSV-RECORD
                       MOVE STUD-NAME TO CSV-RECORD(6:30)
                       MOVE STUD-PROGRAM TO CSV-RECORD(36:5)
                       MOVE STUD-YEAR-LVL TO CSV-RECORD(46:5)
                       MOVE STUD-GPA TO CSV-RECORD(56:5)
                       WRITE CSV-RECORD
               END-READ
           END-PERFORM.

           CLOSE STUDENT-FILE.
           CLOSE CSV-FILE.
           DISPLAY GREEN "[SYSTEM] Data exported to CSV Successfully!"
           DISPLAY " "
           DISPLAY "[SYSTEM] Press any key to continue..." RST
           ACCEPT WS-WAITFORINPUT.
