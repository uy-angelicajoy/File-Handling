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
           05 STUD-ID          PIC X(5). *> ALPHA NUMBERIC WITH 5 CHARACTERS #mejares
           05 STUD-NAME        PIC X(20). *> ALPHA NUMBERIC WITH 20 CHARACTERS #Mmejares
           05 STUD-PROGRAM     PIC X(10). *> ALPHA NUMBERIC WITH 10 CHARACTERS #mejares
       
       FD CSV-FILE.
       01 CSV-RECORD           PIC X(40).

       WORKING-STORAGE SECTION.
           01 FILESTATUS       PIC X(2). 
           01 WS-OPTION        PIC 9. *> ALPHA NUMBERIC WITH 1 CHARACTERS  #meajres
           01 WS-EndOfFile     PIC X VALUE 'N'. *> THE DEFAULT VALUE IS N 
           01 WS-WAITFORINPUT  PIC X.

       PROCEDURE DIVISION.
           PERFORM MAIN-MENU.
           STOP RUN.

       MAIN-MENU.
           PERFORM UNTIL WS-EndOfFile = 'Y'
               CALL "SYSTEM" USING "CLS" *> this is for clearing the entirety of the screen #mejares
               DISPLAY "===================================="
               DISPLAY "|     <PUP-T STUDENT DATABASE>     |"
               DISPLAY "===================================="
               DISPLAY "|1. CREATE STUDENT PROFILE         |"
               DISPLAY "|2. SEARCH STUDENT                 |"
               DISPLAY "|3. EDIT STUDENT PROFILE           |"
               DISPLAY "|4. DELETE STUDENT BY ID           |"
               DISPLAY "|5. DISPLAY ALL STUDENTS           |"
               DISPLAY "|6. EXPORT TO READABLE FILE/CSV    |"
               DISPLAY "|7. EXIT                           |"
               DISPLAY "===================================="
               DISPLAY "Enter your desired option: " NO ADVANCING
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
                       DISPLAY "[SYSTEM] TERMINATING PROGRAM..."
                       CLOSE STUDENT-FILE *> CLOSE THE FILE BEFORE EXITING #mejares 
                       MOVE 'Y' TO WS-EndOfFile *> this is for exiting the program #mejares
                   WHEN OTHER DISPLAY "INVALID OPTION"
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
           DISPLAY "       CREATE STUDENT PROFILE"
           DISPLAY "------------------------------------"
           DISPLAY "    Enter student profile details"
           DISPLAY "------------------------------------"
           DISPLAY "Enter Student ID: " NO ADVANCING
           ACCEPT STUD-ID.
           READ STUDENT-FILE *> error handler for duplicate student id #mejares
               INVALID KEY 
                   CONTINUE 
               NOT INVALID 
                   DISPLAY "Student ID Number already exists!"
                   CLOSE STUDENT-FILE
                   DISPLAY "[SYSTEM] Press any key to continue..."
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
           END-READ
           DISPLAY "Enter Student Name: " NO ADVANCING
           ACCEPT STUD-NAME.
           DISPLAY "Enter Program: " NO ADVANCING
           ACCEPT STUD-PROGRAM.

           WRITE STUDENT-PROFILE.
           IF FILESTATUS NOT = "00" *> error handler for writing student profile/file status 00 means succesful file #mejares
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY "  Error in Writing Student Profile!"
               DISPLAY "------------------------------------"
               DISPLAY " "
               DISPLAY "[SYSTEM] Press any key to continue..."
               ACCEPT WS-WAITFORINPUT
           ELSE
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY "      Student Profile created!"
               DISPLAY "------------------------------------"
               DISPLAY " "
               DISPLAY "[SYSTEM] Press any key to continue..."
               ACCEPT WS-WAITFORINPUT
           END-IF.
           CLOSE STUDENT-FILE. *> this is for closing the file #mejares

       SEARCH-STUDENT. *> 2. SEARCH STUDENT
           OPEN I-O STUDENT-FILE. *> this is for opening the file #mejares
           CALL "SYSTEM" USING "CLS"
           DISPLAY "------------------------------------"
           DISPLAY "       SEARCH STUDENT PROFILE"
           DISPLAY "------------------------------------"
           DISPLAY "Enter Student ID to Search: " NO ADVANCING
           ACCEPT STUD-ID.
           READ STUDENT-FILE KEY IS STUD-ID *> this is for reading the student file #mejares
               INVALID KEY *> error handler for student not found #mejares
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "   Student not found/Registered!"
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   DISPLAY "[SYSTEM] Press any key to continue..."
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
               NOT INVALID KEY *> this is for displaying the student profile #mejares
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "       Student Profile Found!"
                   DISPLAY "------------------------------------"
                   DISPLAY "Student Name: " STUD-NAME.
                   DISPLAY "Student ID: " STUD-ID.
                   DISPLAY "Student Program: " STUD-PROGRAM.
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   DISPLAY "[SYSTEM] Press any key to continue..."
                   ACCEPT WS-WAITFORINPUT
           CLOSE STUDENT-FILE.
           IF FILESTATUS NOT = "00" *> error handler for reading student profile/file status 00 means succesful file #mejares
               DISPLAY " "
               DISPLAY "Error in Reading Student Profile!"
               DISPLAY "[SYSTEM] Press any key to continue..."
               ACCEPT WS-WAITFORINPUT
           END-IF.
           
       EDIT-STUDENT-PROFILE. *> 3. EDIT STUDENT PROFILE
           OPEN I-O STUDENT-FILE.
           IF FILESTATUS = '35' *> this is for checking if the file exists/filestatus 35 means file not found #mejares
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY "  No data found/Error Opening File."
               DISPLAY "------------------------------------"
               DISPLAY "[SYSTEM] Press any key to continue..."
               ACCEPT WS-WAITFORINPUT
               EXIT PARAGRAPH
           END-IF.

           CALL "SYSTEM" USING "CLS"
           DISPLAY "------------------------------------"
           DISPLAY "       EDIT STUDENT PROFILE"
           DISPLAY "------------------------------------"
           DISPLAY "Enter  Student ID to edit: " NO ADVANCING
           ACCEPT STUD-ID.
           READ STUDENT-FILE KEY IS STUD-ID
               INVALID KEY 
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "       Student ID not found!"
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   DISPLAY "[SYSTEM] Press any key to continue..."
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
               NOT INVALID KEY 
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "       Student Profile Found!"
                   DISPLAY "------------------------------------"
                   DISPLAY " Enter new student profile details"
                   DISPLAY "------------------------------------"
                   *>DISPLAY "Enter Student ID: " NO ADVANCING
                   *>ACCEPT STUD-ID.
                   DISPLAY "Enter Student Name: " NO ADVANCING
                   ACCEPT STUD-NAME.
                   DISPLAY "Enter Program: " NO ADVANCING
                   ACCEPT STUD-PROGRAM.
                   REWRITE STUDENT-PROFILE.
                   IF FILESTATUS = "00" *> error handler for editing student profile/file status 00 means succesful file #mejares
                       DISPLAY " "
                       DISPLAY "------------------------------------"
                       DISPLAY "       Student Profile Updated!"
                       DISPLAY "------------------------------------"
                       DISPLAY " "
                       DISPLAY "[SYSTEM] Press any key to continue..."
                       ACCEPT WS-WAITFORINPUT
                   ELSE
                       DISPLAY " "
                       DISPLAY "------------------------------------"
                       DISPLAY "       Error Updating Profile!"
                       DISPLAY "------------------------------------"
                       DISPLAY " "
                       DISPLAY "[SYSTEM] Press any key to continue..."
                       ACCEPT WS-WAITFORINPUT
                   END-IF
                   
           CLOSE STUDENT-FILE.
       
       DELETE-STUDENT. *> 4. DELETE STUDENT BY ID
           OPEN I-O STUDENT-FILE.
           IF FILESTATUS = '35' *> this is for checking if the file exists/filestatus 35 means file not found #mejares
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY "  No data found/Error Opening File."
               DISPLAY "------------------------------------"
               DISPLAY "[SYSTEM] Press any key to continue..."
               ACCEPT WS-WAITFORINPUT
               EXIT PARAGRAPH
           END-IF.

           CALL "SYSTEM" USING "CLS"
           DISPLAY "------------------------------------"
           DISPLAY "       DELETE STUDENT PROFILE"
           DISPLAY "------------------------------------"
           DISPLAY "Enter Student ID to delete: " NO ADVANCING
           ACCEPT STUD-ID.
           READ STUDENT-FILE KEY IS STUD-ID
               INVALID KEY 
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "       Student ID not found!"
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   DISPLAY "[SYSTEM] Press any key to continue..."
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
               NOT INVALID KEY 
                   DELETE STUDENT-FILE *> this is for deleting the student profile #mejares
                   IF FILESTATUS = "00" *> error handler for deleting student profile/file status 00 means succesful file #mejares
                       DISPLAY " "
                       DISPLAY "------------------------------------"
                       DISPLAY "       Student Profile Deleted!"
                       DISPLAY "------------------------------------"
                       DISPLAY " "
                       DISPLAY "[SYSTEM] Press any key to continue..."
                       ACCEPT WS-WAITFORINPUT
                   ELSE
                       DISPLAY " "
                       DISPLAY "------------------------------------"
                       DISPLAY "       Error Deleting Profile!"
                       DISPLAY "------------------------------------"
                       DISPLAY " "
                       DISPLAY "[SYSTEM] Press any key to continue..."
                       ACCEPT WS-WAITFORINPUT
                   END-IF

           CLOSE STUDENT-FILE.

       DISPLAY-ALL-STUDENTS. *> 5. DISPLAY ALL STUDENTS
           OPEN INPUT STUDENT-FILE
           IF FILESTATUS = "35" *> this is for checking if the file exists/filestatus 35 means file not found #mejares 
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY "           No data found."
               DISPLAY "------------------------------------"
               DISPLAY "[SYSTEM] Press any key to continue..."
               ACCEPT WS-WAITFORINPUT
               EXIT PARAGRAPH
           END-IF.
           
           CALL "SYSTEM" USING "CLS"
           DISPLAY "------------------------------------"
           DISPLAY "       PUP-T STUDENTS DATABASE"
           DISPLAY "------------------------------------"
           PERFORM UNTIL FILESTATUS = "10" *>10 means it reached end of file #mejares
               READ STUDENT-FILE
                   AT END
                       MOVE "10" TO FILESTATUS *> this is for checking if the file reached the end #mejares
                   NOT AT END
                       DISPLAY "Student ID: " STUD-ID
                       DISPLAY "Student Name  : " STUD-NAME
                       DISPLAY "Program      : " STUD-PROGRAM
                       DISPLAY "===================================="
               END-READ
           END-PERFORM

           CLOSE STUDENT-FILE.
           DISPLAY "[SYSTEM] Press any key to continue..."
           ACCEPT WS-WAITFORINPUT.

       EXPORT-TO-CSV. *> 6. EXPORT TO READABLE FILE/CSV
           OPEN OUTPUT CSV-FILE. *>this is for creating the csv file #mejares
           OPEN I-O STUDENT-FILE. *> this is for opening the csv file #mejares
           DISPLAY " "
           DISPLAY "[SYSTEM] Exporting Student Data to CSV File..."

           PERFORM UNTIL FILESTATUS = "10"
               READ STUDENT-FILE
                   AT END
                       MOVE "10" TO FILESTATUS
                       EXIT PERFORM
                   NOT AT END
                       MOVE STUD-ID TO CSV-RECORD
                       MOVE STUD-NAME TO CSV-RECORD(6:20)
                       MOVE STUD-PROGRAM TO CSV-RECORD(26:10)
                       WRITE CSV-RECORD
               END-READ
           END-PERFORM.

           CLOSE STUDENT-FILE.
           CLOSE CSV-FILE.
           DISPLAY "[SYSTEM] Data exported to CSV Successfully!"
           DISPLAY "[SYSTEM] Press any key to continue..."
           ACCEPT WS-WAITFORINPUT.
