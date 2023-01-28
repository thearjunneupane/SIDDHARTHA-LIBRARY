
DECLARE SUB add_record ()
DECLARE SUB display_record ()
DECLARE SUB modify_record ()
DECLARE SUB delete_record ()
DECLARE SUB search_record ()
DECLARE SUB exit_program ()
DECLARE ask_return ()
DECLARE SUB menu_lines ()
DECLARE SUB add_lines ()
DECLARE SUB display_lines ()
DECLARE SUB modify_lines ()
DECLARE SUB delete_lines ()
DECLARE SUB search_lines ()
DECLARE SUB exit_lines ()
DECLARE SUB extra_lines ()
DECLARE SUB ask_return_lines ()
DECLARE SUB DELAY (period!)

_Title "SIDDHARTHA LIBRARY."
$ExeIcon:'sid-lib.ico'

CLS
SCREEN _NewImage(640, 480, 32)
FOR i = 1 To 41
    photono$ = Str$(i)
    lengphoto = Len(photono$) - 1
    photoseq$ = Mid$(photono$, 2, lengphoto)
    seqfile$ = "intro_sequence/(" + photoseq$ + ").jpg"
    seqimg = _LoadImage(seqfile$)
    _PutImage (0, 0), seqimg
    DELAY (0.04)
Next i
Sleep 2

main:
DO
CLS
SCREEN 12
Call menu_lines
Locate 5, 24
Color 0, 15
Print "SIDDHARTHA BOARDING LIBRARY"
Locate 4, 31
Color 0, 15
Print "\\ WELCOME //"

Locate 7, 31
Color 13, 0
Print "~~~MAIN MENU~~~"

Color 14
Locate 16, 26
Print "Use Arrows Key To Choose"

Locate 17, 30
COLOR 14
Print " PRESS ENTER "

Locate 9, 19
Color 7,0
Print "1."
Locate 10, 19
Print "2."
Locate 11, 19
Print "3."
Locate 12, 19
Print "4."
Locate 13, 19
Print "5."
Locate 14, 19
Print "6."

a = 1
    check0:
    IF a > 6 THEN
        a = 1
    ELSEIF a < 1 THEN
        a = 6
    END IF
IF a = 1 THEN

Locate 9, 24
COLOR 7,2: Print "   Add     Book's Records        "
Locate 10, 24
COLOR 7,0: Print "   Display Book's Records        "
Locate 11, 24
COLOR 7,0: Print "   Modify  Book's Records        "
Locate 12, 24
COLOR 7,0: Print "   Delete  Book's Records        "
Locate 13, 24
COLOR 7,0: Print "   Search  Book's Records        "
Locate 14, 24
COLOR 7,0: Print "   Exit    Program               "

    ELSEIF a = 2 THEN

Locate 9, 24
COLOR 7,0: Print "   Add     Book's Records        "
Locate 10, 24
COLOR 7,2: Print "   Display Book's Records        "
Locate 11, 24
COLOR 7,0: Print "   Modify  Book's Records        "
Locate 12, 24
COLOR 7,0: Print "   Delete  Book's Records        "
Locate 13, 24
COLOR 7,0: Print "   Search  Book's Records        "
Locate 14, 24
COLOR 7,0: Print "   Exit    Program               "


    ELSEIF a = 3 THEN

Locate 9, 24
COLOR 7,0: Print "   Add     Book's Records        "
Locate 10, 24
COLOR 7,0: Print "   Display Book's Records        "
Locate 11, 24
COLOR 7,2: Print "   Modify  Book's Records        "
Locate 12, 24
COLOR 7,0: Print "   Delete  Book's Records        "
Locate 13, 24
COLOR 7,0: Print "   Search  Book's Records        "
Locate 14, 24
COLOR 7,0: Print "   Exit    Program               "


    ELSEIF a = 4 THEN

Locate 9, 24
COLOR 7,0: Print "   Add     Book's Records        "
Locate 10, 24
COLOR 7,0: Print "   Display Book's Records        "
Locate 11, 24
COLOR 7,0: Print "   Modify  Book's Records        "
Locate 12, 24
COLOR 7,4: Print "   Delete  Book's Records        "
Locate 13, 24
COLOR 7,0: Print "   Search  Book's Records        "
Locate 14, 24
COLOR 7,0: Print "   Exit    Program               "


    ELSEIF a = 5 THEN

Locate 9, 24
COLOR 7,0: Print "   Add     Book's Records        "
Locate 10, 24
COLOR 7,0: Print "   Display Book's Records        "
Locate 11, 24
COLOR 7,0: Print "   Modify  Book's Records        "
Locate 12, 24
COLOR 7,0: Print "   Delete  Book's Records        "
Locate 13, 24
COLOR 7,2: Print "   Search  Book's Records        "
Locate 14, 24
COLOR 7,0: Print "   Exit    Program               "


    ELSEIF a = 6 THEN

Locate 9, 24
COLOR 7,0: Print "   Add     Book's Records        "
Locate 10, 24
COLOR 7,0: Print "   Display Book's Records        "
Locate 11, 24
COLOR 7,0: Print "   Modify  Book's Records        "
Locate 12, 24
COLOR 7,0: Print "   Delete  Book's Records        "
Locate 13, 24
COLOR 7,0: Print "   Search  Book's Records        "
Locate 14, 24
COLOR 7,2: Print "   Exit    Program               "


COLOR 7,0
END IF

    DO
    keypress$ = UCASE$(INKEY$)
    IF keypress$ = CHR$(0) + CHR$(80) THEN
            a = a + 1
            GOTO check0
        ELSEIF keypress$ = CHR$(0) + CHR$(72) THEN
            a = a - 1
            GOTO check0
        END IF
    LOOP UNTIL keypress$ = CHR$(13)

Select Case a
    Case 1
        Call add_record
    Case 2
        Call display_record
    Case 3
        Call modify_record
    Case 4
        Call delete_record
    Case 5
        Call search_record
    Case 6
        Call exit_program
End Select

Cls
COLOR 2,0
    For i = 23 To 53
        Locate 8, i: Print Chr$(205);
        Locate 14, i: Print Chr$(205);
        Locate 16, i: Print Chr$(205);
    Next i
    For j = 8 To 16
        Locate j, 23: Print Chr$(186);
        Locate j, 53: Print Chr$(186);
    Next j
    Locate 8, 23: Print Chr$(201);
    Locate 8, 53: Print Chr$(187);
    Locate 16, 23: Print Chr$(200);
    Locate 16, 53: Print Chr$(188);
    Color 15, 0: Locate 15, 27: Print "Press Enter To Choose."

    a = 1
    check:
    IF a > 2 THEN
        a = 2
    ELSEIF a < 1 THEN
        a = 1
    END IF
    IF a = 1 THEN
        COLOR 7, 2
    Locate 10, 28: Print "1. RETURN TO MAIN MENU"
        COLOR 7, 0
    Locate 12, 28: Print "2. EXIT PROGRAM       "
    ELSEIF a = 2 THEN
        COLOR 7, 0
    Locate 10, 28: Print "1. RETURN TO MAIN MENU"
        COLOR 7, 2
    Locate 12, 28: Print "2. EXIT PROGRAM       "
        COLOR 7, 0
    END IF
    DO
        keypress$ = UCASE$(INKEY$)

        IF keypress$ = CHR$(0) + CHR$(80) THEN
            a = a + 1
            GOTO check
        ELSEIF keypress$ = CHR$(0) + CHR$(72) THEN
            a = a - 1
            GOTO check
        END IF

    LOOP UNTIL keypress$ = CHR$(13)
LOOP WHILE a = 1
Call exit_program
END

Sub menu_lines
    Color 2
    For i = 16 To 59
        Locate 3, i: Print Chr$(205);
        Locate 18, i: Print Chr$(205); ' horizontal line
        Locate 6, i: Print Chr$(205); ' horizontal line
        Locate 8, i: Print Chr$(205); ' horizontal line
        Locate 15, i: Print Chr$(205); ' horizontal line
    Next i
    For j = 4 To 17
        Locate j, 16: Print Chr$(186); ' vertical line
        Locate j, 59: Print Chr$(186); ' vertical line
    Next j
    For k = 9 To 14
        Locate k, 22: Print Chr$(186); ' vertical line
    Next k
    Locate 3, 16: Print Chr$(201); ' top-left corner
    Locate 3, 59: Print Chr$(187); ' top-right corner
    Locate 18, 16: Print Chr$(200); ' bottom-left corner
    Locate 18, 59: Print Chr$(188); ' bottom-right corner
    Locate 8, 22: Print Chr$(201);
    Locate 15, 22: Print Chr$(200);
End Sub

Sub add_lines
    Color 2
    For i = 9 To 66
        Locate 3, i: Print Chr$(205);
        Locate 5, i: Print Chr$(205);
        Locate 7, i: Print Chr$(205);
        Locate 15, i: Print Chr$(205);
        LOCATE 17, i: Print Chr$(205);

    Next i
    For j = 4 To 17
        Locate j, 9: Print Chr$(186);
        Locate j, 66: Print Chr$(186);
    Next j
    For k = 5 To 15
        Locate k, 18: Print Chr$(186);
        Locate k, 37: Print Chr$(186);
        Locate k, 55: Print Chr$(186);
    Next k
    Locate 3, 9: Print Chr$(201);
    Locate 3, 66: Print Chr$(187);
    Locate 17, 9: Print Chr$(200);
    Locate 17, 66: Print Chr$(188);

    Locate 5, 18: Print Chr$(201);
    Locate 5, 37: Print Chr$(201);
    Locate 5, 55: Print Chr$(201);
    Locate 15, 18: Print Chr$(200);
    Locate 15, 37: Print Chr$(200);
    Locate 15, 55: Print Chr$(200);

End Sub


Sub add_record
    Cls
    more:
    a = -1
    Do
        CALL add_lines
        COLOR 7,0
        Open "SID-LIBRARY.dat" For Append As #1
        a = a + 1
        Locate 4, 30
        Print "RECQUIRED DETAILS"
        Locate 6, 11
        Print "B-CODE:"
        Locate 6, 21
        Print "B-NAME:"
        Locate 6, 39
        Print "Author's NAME : "
        Locate 6, 57
        Print "B-COUNT:"

        Locate 8 + a, 13
        Input "", b_no
        Locate 8 + a, 21
        Input "", b_na$
        Locate 8 + a, 39
        Input "", a_na$
        Locate 8 + a, 60
        Input "", b_co
        Write #1, b_no, b_na$, a_na$, b_co
            COLOR 15
    c=1
    check1:
    IF c > 2 THEN
        c = 2
        ELSEIF c < 1 THEN
            c = 1
        END IF 

            IF c = 1 THEN
            Call extra_lines
            Locate 21, 29
            Color 7
            Print "Add more Records ?"
            COLOR 15, 2
            LOCATE 22, 27
            PRINT "   YES   "
            COLOR 15, 0
            LOCATE 22, 37
            PRINT "   NO    "
            ELSEIF c=2 THEN
            Call extra_lines
            Locate 21, 29
            Color 7
            Print "Add more Records ?"            
            COLOR 15, 0
            LOCATE 22, 27
            PRINT "   YES   "
            COLOR 15, 2
            LOCATE 22, 37
            PRINT "   NO    "
        END IF
    DO
        keypress$=UCASE$(INKEY$)
    
            IF keypress$ = CHR$(0) + CHR$(77) THEN
                c = c + 1
                GOTO check1
            ELSEIF keypress$ = CHR$(0) + CHR$(75) THEN
                c = c - 1
                GOTO check1
            END IF

    LOOP UNTIL keypress$ = CHR$(13)       
CLOSE #1
LOOP WHILE c=1 AND a<9
            Locate 16, 19
            COLOR 7,0
            Print "Your record has been stored successfully"
COLOR 15,0
SLEEP 2
END SUB

Sub delete_lines
    Color 2
    For i = 9 To 66
        Locate 3, i: Print Chr$(205);
        Locate 19, i: Print Chr$(205);
        Locate 5, i: Print Chr$(205);
        Locate 15, i: Print Chr$(205);
        Locate 17, i: Print Chr$(205);

    Next i
    For j = 4 To 19
        Locate j, 9: Print Chr$(186);
        Locate j, 66: Print Chr$(186);
    Next j
    For k = 5 To 15
        Locate k, 30: Print Chr$(186);
    Next k

    Locate 3, 9: Print Chr$(201);
    Locate 3, 66: Print Chr$(187);
    Locate 19, 9: Print Chr$(200);
    Locate 19, 66: Print Chr$(188);

    Locate 5, 30: Print Chr$(201);
    Locate 15, 30: Print Chr$(200);

End Sub

SUB delete_record
    Cls
    more:
    a = -1
    f=0
    Do
        OPEN "SID-LIBRARY.dat" FOR INPUT AS #1
    OPEN "temp.dat" FOR OUTPUT AS #2
    cls
    CALL delete_lines
COLOR 7,0
LOCATE 4,20
INPUT "Enter Book's CODE to DELETE: ";del_code
LOCATE 7,11
PRINT "BOOK'S   CODE:"
LOCATE 9, 11
PRINT "BOOK'S   NAME:"
LOCATE 11,11
PRINT "Authur's NAME:"
LOCATE 13,11
PRINT "BOOK'S   COUNT:"
flag=0
WHILE NOT EOF(1)
INPUT #1, b_no, b_na$, a_na$, b_co
IF (del_code <> b_no) THEN
WRITE #2, b_no, b_na$, a_na$, b_co 
ELSE
LOCATE 8,
LOCATE 7, 32
PRINT b_no 
LOCATE 9,32
PRINT b_na$
LOCATE 11,32
PRINT a_na$
LOCATE 13,32
PRINT b_co 
flag=1
DO
LOCATE 16,25
INPUT "WANT to DELETE THIS(y/n)";delete_this$
LOOP UNTIL delete_this$="y" OR delete_this$="Y" OR delete_this$="n" OR delete_this$="N"
IF delete_this$="y" OR delete_this$="Y" THEN 
f=f+1
GOTO check1
Else
WRITE #2, b_no, b_na$, a_na$, b_co
END IF
END IF
WEND
IF flag=0 THEN
LOCATE 16,32
PRINT "No Record FOUND!"
END IF
c=1
    check1:
    IF c > 2 THEN
        c = 2
        ELSEIF c < 1 THEN
            c = 1
        END IF 

            IF c = 1 THEN
            Call extra_lines
            Locate 21, 29
            Color 7
            Print "DELETE more Records ?"
            COLOR 15, 2
            LOCATE 22, 27
            PRINT "     YES    "
            COLOR 15, 0
            LOCATE 22, 41
            PRINT "     NO     "
            ELSEIF c=2 THEN
            Call extra_lines
            Locate 21, 29
            Color 7
            Print "DELETE more Records ?"            
            COLOR 15, 0
            LOCATE 22, 27
            PRINT "    YES     "
            COLOR 15, 2
            LOCATE 22, 41
            PRINT "    NO      "
        END IF
    DO
        keypress$=UCASE$(INKEY$)
    
            IF keypress$ = CHR$(0) + CHR$(77) THEN
                c = c + 1
                GOTO check1
            ELSEIF keypress$ = CHR$(0) + CHR$(75) THEN
                c = c - 1
                GOTO check1
            END IF

    LOOP UNTIL keypress$ = CHR$(13)
CLOSE #1, #2
KILL "SID-LIBRARY.dat"
NAME "temp.dat" AS "SID-LIBRARY.dat"    
LOOP WHILE c=1
        IF f<1 THEN
            Locate 16, 19
            COLOR 7,0
            Print "      No record has been deleted!"
            GOTO last
        End If
            Locate 16, 19
            COLOR 7,0
            Print "Your record has been deleted successfully!"
last:
COLOR 15,0
SLEEP 2
END SUb

Sub display_lines
    Color 2
    For i = 9 To 66
        Locate 1, i: Print Chr$(205);
        Locate 3, i: Print Chr$(205);
        Locate 17, i: Print Chr$(205);
        Locate 19, i: Print Chr$(205);
        Locate 5, i: Print Chr$(205);
        Locate 15, i: Print Chr$(205);

    Next i
    For j = 2 To 19
        Locate j, 9: Print Chr$(186);
        Locate j, 66: Print Chr$(186);
    Next j
    For k = 4 To 15
        Locate k, 19: Print Chr$(186);
        Locate k, 37: Print Chr$(186);
        Locate k, 55: Print Chr$(186);
    Next k

    Locate 1, 9: Print Chr$(201);
    Locate 1, 66: Print Chr$(187);
    Locate 19, 9: Print Chr$(200);
    Locate 19, 66: Print Chr$(188);

    Locate 3, 19: Print Chr$(201);
    Locate 3, 37: Print Chr$(201);
    Locate 3, 55: Print Chr$(201);
    Locate 15, 19: Print Chr$(200);
    Locate 15, 37: Print Chr$(200);
    Locate 15, 55: Print Chr$(200);

End Sub

Sub display_record
    Cls
    Call display_lines
    Open "SID-LIBRARY.dat" For Input As #1
    c = 0
    Color 7, 0
    Locate 2, 29: Print "AVAILABLE RECORDS!"
    Locate 4, 11: Print "B-CODE:"
    Locate 4, 21: Print "   B-NAME:"
    Locate 4, 39: Print "Author's NAME:"
    Locate 4, 57: Print "B-COUNT:"
    IF EOF(1) THEN
    LOCATE 6,11: PRINT "   _"
    LOCATE 6,21: PRINT "      _"
    LOCATE 6,39: PRINT "      _"
    LOCATE 6,57: PRINT "   _"
    Locate 16, 26 
    Print "NO RECORDS AVAILABLE!"
    Locate 18,24: Color 15,2: Print "  Press ENTER To Continue  "
    DO
    keypress$ = UCASE$(INKEY$)
    LOOP UNTIL keypress$=CHR$(13)
    ELSE
    a = -1

    Do
        a = a + 1
        Input #1, b_no, b_na$, a_na$, b_co
        Locate 6 + a, 11
        Print b_no
        Locate 6 + a, 21
        Print b_na$
        Locate 6 + a, 39
        Print a_na$
        Locate 6 + a, 57
        Print b_co
        c = c + 1
    Loop Until EOF(1) Or a = 8  
    Locate 16, 25
    Print " TOTAL RECORD COUNT:- "; c
    Locate 18,24: Color 15,2: Print "  Press ENTER To Continue  "
    DO
    keypress$ = UCASE$(INKEY$)
    LOOP UNTIL keypress$=CHR$(13)
    END IF
    Close #1
    COLOR 15,0
End Sub

Sub modify_lines
    Color 2
    For i = 9 To 66
        Locate 3, i: Print Chr$(205);
        Locate 5, i: Print Chr$(205);
        Locate 15, i: Print Chr$(205);
        Locate 17, i: Print Chr$(205);

    Next i
    For j = 4 To 17
        Locate j, 9: Print Chr$(186);
        Locate j, 66: Print Chr$(186);
    Next j
    For k = 5 To 15
        Locate k, 30: Print Chr$(186);
    Next k

    Locate 3, 9: Print Chr$(201);
    Locate 3, 66: Print Chr$(187);
    Locate 17, 9: Print Chr$(200);
    Locate 17, 66: Print Chr$(188);

    Locate 5, 30: Print Chr$(201);
    Locate 15, 30: Print Chr$(200);

End Sub

Sub modify_record
    Cls
    more1:
    f=0
    DO
    Open "SID-LIBRARY.dat" For Input As #1
    Open "temp.dat" For Output As #2
    Cls
    Call modify_lines
    Color 7, 0
    Locate 4, 20
    Input "Enter Book's CODE to MODIFY: "; mod_code
    Locate 7, 11
    Print "BOOK'S   CODE:"
    Locate 9, 11
    Print "BOOK'S   NAME:"
    Locate 11, 11
    Print "Authur's NAME:"
    Locate 13, 11
    Print "BOOK'S   COUNT:"
    flag = 0
    While Not EOF(1)
        Input #1, b_no, b_na$, a_na$, b_co
        If (mod_code <> b_no) Then
            Write #2, b_no, b_na$, a_na$, b_co
        Else
            Locate 7, 32
            Print b_no
            Locate 9, 32
            Print b_na$
            Locate 11, 32
            Print a_na$
            Locate 13, 32
            Print b_co
            flag = 1
            Do
                Locate 16, 25
                Input "WANT to MODIFY THIS(y/n)"; modify_this$
            Loop Until modify_this$ = "y" Or modify_this$ = "Y" Or modify_this$ = "n" Or modify_this$ = "N"
            If UCase$(modify_this$) = "Y" Then
                Cls: Call modify_lines
                Color 7, 0
                Locate 4, 26
                Print "UPDATE THE INFORMATIONS!"
                Locate 7, 11
                Print "New BOOK'S   CODE:"
                Locate 9, 11
                Print "New BOOK'S   NAME:"
                Locate 11, 11
                Print "New AUTHUR'S NAME:"
                Locate 13, 11
                Print "New BOOK'S  COUNT:"

                Locate 7, 32
                Input "", b_no
                Locate 9, 32
                Input "", b_na$
                Locate 11, 32
                Input "", a_na$
                Locate 13, 32
                Input "", b_co

                Write #2, b_no, b_na$, a_na$, b_co
                f=f+1
            Else
                Write #2, b_no, b_na$, a_na$, b_co
            End If
        End If
    Wend
    If flag = 0 Then
        Locate 16, 30
        Print "No Record FOUND!"
    End If
c=1
    check4:
    IF c > 2 THEN
        c = 2
        ELSEIF c < 1 THEN
            c = 1
        END IF 

            IF c = 1 THEN
            Call extra_lines
            Locate 21, 29
            Color 7
            Print "MODIFY more Records ?"
            COLOR 15, 2
            LOCATE 22, 27
            PRINT "     YES    "
            COLOR 15, 0
            LOCATE 22, 41
            PRINT "     NO     "
            ELSEIF c=2 THEN
            Call extra_lines
            Locate 21, 29
            Color 7
            Print "MODIFY more Records ?"            
            COLOR 15, 0
            LOCATE 22, 27
            PRINT "    YES     "
            COLOR 15, 2
            LOCATE 22, 41
            PRINT "    NO      "
        END IF
    DO
        keypress$=UCASE$(INKEY$)
    
            IF keypress$ = CHR$(0) + CHR$(77) THEN
                c = c + 1
                GOTO check4
            ELSEIF keypress$ = CHR$(0) + CHR$(75) THEN
                c = c - 1
                GOTO check4
            END IF

    LOOP UNTIL keypress$ = CHR$(13)
CLOSE #1, #2
KILL "SID-LIBRARY.dat"
NAME "temp.dat" AS "SID-LIBRARY.dat"    
LOOP WHILE c=1
        IF f<1 THEN
            Locate 16, 19
            COLOR 7,0
            Print "      No record has been Modified!"
            GOTO last
        End If
            Locate 16, 19
            COLOR 7,0
            Print "Your record has been modified successfully!"
last:
COLOR 15,0
SLEEP 2
End Sub

Sub search_lines
    Color 2
    For i = 9 To 66
        Locate 3, i: Print Chr$(205);
        Locate 5, i: Print Chr$(205);
        Locate 15, i: Print Chr$(205);
        Locate 17, i: Print Chr$(205);

    Next i
    For j = 4 To 17
        Locate j, 9: Print Chr$(186);
        Locate j, 66: Print Chr$(186);
    Next j
    For k = 5 To 15
        Locate k, 30: Print Chr$(186);
    Next k

    Locate 3, 9: Print Chr$(201);
    Locate 3, 66: Print Chr$(187);
    Locate 17, 9: Print Chr$(200);
    Locate 17, 66: Print Chr$(188);

    Locate 5, 30: Print Chr$(201);
    Locate 15, 30: Print Chr$(200);

End Sub

Sub search_record
    Cls
    more3:
    Do
    OPEN "SID-LIBRARY.dat" FOR INPUT AS #1
    CLS
    Call search_lines
    Color 7, 0
    Locate 4, 20
    IF EOF(1) THEN
    PRINT "NO RECORD TO SEARCH!"
    END IF
    Input "Enter Book's CODE to SEARCH: "; search_code
    Locate 7, 11: Print "BOOK'S    CODE:"
    Locate 9, 11: Print "BOOK'S    NAME:"
    Locate 11, 11: Print "Authur's NAME:"
    Locate 13, 11: Print "BOOK'S   COUNT:"
    flag=0
    While Not EOF(1)
        Input #1, b_no, b_na$, a_na$, b_co
        If search_code = b_no Then
            Cls: Call search_lines: Color 7, 0: Locate 4, 21: Print "INFO. OF THE BOOK WITH CODE:"; search_code
            Locate 7, 11: Print "BOOK'S    CODE:"
            Locate 9, 11: Print "BOOK'S    NAME:"
            Locate 11, 11: Print "Authur's NAME:"
            Locate 13, 11: Print "BOOK'S   COUNT:"

            Locate 7, 32
            Print b_no
            Locate 9, 32
            Print b_na$
            Locate 11, 32
            Print a_na$
            Locate 13, 32
            Print b_co
            flag = 1
            Locate 16, 30
            Print "RECORD FOUNDED!"
        End If
    WEND
    If flag = 0 Then
        Locate 16, 30
        Print "NO RECORD FOUND!"
    End If
    c=1
    check5:
    IF c > 2 THEN
        c = 2
        ELSEIF c < 1 THEN
            c = 1
        END IF 

            IF c = 1 THEN
            Call extra_lines
            Locate 21, 29
            Color 7
            Print "Search more Records ?"
            COLOR 15, 2
            LOCATE 22, 27
            PRINT "     YES    "
            COLOR 15, 0
            LOCATE 22, 41
            PRINT "     NO     "
            ELSEIF c=2 THEN
            Call extra_lines
            Locate 21, 29
            Color 7
            Print "Search more Records ?"            
            COLOR 15, 0
            LOCATE 22, 27
            PRINT "    YES     "
            COLOR 15, 2
            LOCATE 22, 41
            PRINT "    NO      "
        END IF
    DO
        keypress$=UCASE$(INKEY$)
    
            IF keypress$ = CHR$(0) + CHR$(77) THEN
                c = c + 1
                GOTO check5
            ELSEIF keypress$ = CHR$(0) + CHR$(75) THEN
                c = c - 1
                GOTO check5
            END IF

    LOOP UNTIL keypress$ = CHR$(13)
        Close #1
LOOP WHILE c=1
    last:
Color 15,0
End Sub


Sub exit_program
    Screen _NewImage(640, 350, 256)
    Cls
    For i = 1 To 10
        For j = 1 To 6
            Line (128 + (i * 4) + (j * 4), 74 + (i * 4) + (j * 4))-(512 - (i * 4) - (j * 4), 278 - (i * 4) - (j * 4)), 10, B
        Next j
    Next i

    Color 14, 0
    Locate 10, 33
    Print "Thanks For Using!"
    Locate 11, 30
    Print "Made with love by ARJUN"
    Locate 12, 33
    Print " Class: 10 'sun'"
    Locate 13, 32
    Print "Siddhartha Boarding"
    End
End Sub

Sub extra_lines
    Color 2, 0
    FOR i = 21 to 22
    Locate i, 9: Print Chr$(186);
    Locate i, 66: Print Chr$(186);
    NEXT i
    For j = 9 To 65
        Locate 20, j: Print Chr$(205);
        Locate 23, j: Print Chr$(205);
    Next j

    Locate 20, 9: Print Chr$(201);
    Locate 20, 66: Print Chr$(187);
    Locate 23, 9: Print Chr$(200);
    Locate 23, 66: Print Chr$(188);
End Sub


Sub DELAY (period As Single)
    time = Timer
    Do
    Loop Until (Timer - time + 86400) - (Int((Timer - time + 86400) / 86400) * 86400) > period
End Sub
