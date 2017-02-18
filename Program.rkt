; Simon Aagaard Pedersen
; sape13@student.aau.dk
; 20136013

(load "helper_functions.rkt")

; Constructor function for calendars. Takes an owner, a list of appointments and a calendar as arguments.
; Returns a closure that is used to simulate an object.
(define (construct-calendar owner appointments calendar)
  (let ((appointment_list (cond((null? appointments) '())(else appointments)))
        (calendar_list (cond((null? calendar) '())(else calendar))))
    (define (getowner) owner)
    (define (getappointments) appointment_list)
    (define (getcalendars) calendar_list)
    (define (self message)
    (cond ((eqv? message 'getowner) getowner)
          ((eqv? message 'getappointments) getappointments)
          ((eqv? message 'getcalendars) getcalendars)
           (else (error "Message not understood" message))))
    self))

; Adds an appointment to a calendar.
; The function is pure in the sense it returns a new calendar, and doesn't modify parameters.
; Returns a new calendar.
(define (add-appointment calendar appointment)
  (construct-calendar ((calendar 'getowner)) (append((calendar 'getappointments))appointment) ((calendar 'getcalendars))))

; Removs an appointment for a calendar.
; This function is pure in the sense it returns a new calendar and doesn't modify parameters.
; Note that this implementation assumes the appointment is in the outer-most calendar.
; Returns a new calendar.
(define (remove-appointment calendar appointment)
  (construct-calendar ((calendar 'getowner)) (rm-ap-helper ((calendar 'getappointments)) appointment) ((calendar 'getcalendars))))

; Auxiliary function to remove appointment.
; Recursively iterates through the list of appointments, appending elements unless they're equal to ap.
; Returns a list of appointments excluding appointments that are equal to ap.
(define (rm-ap-helper aplist ap)
  (cond ((null? aplist) '())
        ((appointments-equal?(car aplist) ap) rm-ap-helper (cdr aplist) ap)
        (else (append (list(car aplist)) (rm-ap-helper (cdr aplist) ap)))))

; Flattens a calendar.
; All appointments are moved to the top-most calendar.
; The function is pure in the sense it returns a new calendar, and doesn't modify parameters.
; Returns a new calendar.
(define (flatten cal)
  (cond ((null? ((cal 'getcalendars))) cal)
        (else (construct-calendar ((cal 'getowner))(append ((cal 'getappointments)) (flatten-helper ((cal 'getcalendars))))'()))))

; Auxiliary function to flatten calendar.
; Takes a list of calendars as input.
; Recursively goes through this list and sub-lists, appending lists of appointments together.
; Returns a list of appointments.
(define (flatten-helper callist)
  (cond ((null? callist) '())
        (else (append(((car callist)'getappointments)) (flatten-helper (cdr callist))))))

; Function to filter a calendar with a predicate.
; Returns a list of appointments for which the predicate is true.
(define (find-appointments cal pred)
  (filter pred (((flatten cal) 'getappointments))))

; Function to return first appointment (i.e. first starting appointment)
; Uses the find-appointments function to obtain a list of appointments for which the predicate is true.
; Then sorts this list and selects the first element.
; Returns an appointment.
(define (find-first-appointment cal pred)
  (let ((predlist (find-appointments cal pred)))
    (cond ((null? predlist) #f)
          (else (car (sort:sort predlist less??))))))

; Function to return last appointment (i.e. last starting appointment)
; Same functionality as above, with the exception we reverse the sorted list.
; Returns an appointment.
(define (find-last-appointment cal pred)
  (let ((predlist (find-appointments cal pred)))
    (cond ((null? predlist) #f)
          (else (car (reverse (sort:sort predlist less??)))))))

; Function to determine if two appointments are equal.
; Appointments are compared on all attributes.
; Returns #t or #f.
(define (appointments-equal? ap1 ap2)
  (cond ((and(and(eq? ((ap1 'getname)) ((ap2 'getname)))(= ((ap1 'getstart_minutes)) ((ap2 'getstart_minutes)))) (= ((ap1 'getduration)) ((ap2 'getduration))) #t))
        (else #f)))

; Predicate used for sorting.
; Returns #t if ap1 starts before ap2. #f otherwise.
(define (less?? ap1 ap2)
  (< ((ap1 'getstart_minutes)) ((ap2 'getstart_minutes))))

; Adds calendar2 to calendar1
; The function is pure in the sense it returns a new calendar, and doesn't modify parameters.
; Returns a new version of calendar1 with calendar2 as a sub-calendar.
(define (add-calendar calendar1 calendar2)
  (construct-calendar ((calendar1 'getowner)) ((calendar1 'getappointments)) (append ((calendar1 'getcalendars)) calendar2)))

; Removes calendar2 from calendar1.
; Constructs and returns new calendar not containing calendar2.
(define (remove-calendar calendar1 calendar2)
  (construct-calendar((calendar1 'getowner)) ((calendar1 'getappointments)) (remove-calendar-helper ((calendar1 'getcalendars)) calendar2)))

; Helper function.
; Recursively goes through the list of calendars.
; A calendar is removed from the list if cal-equals returns true.
; Returns list of calendars with "cal" removed.
(define (remove-calendar-helper callist cal)
  (cond ((null? callist) '())
        ((cal-equals? (car callist) cal) (cdr callist))
        (else append (car callist) (remove-calendar-helper (cdr callist) cal))))

; Function to determine if two calendars are equal.
; Checks owner, all appointsments and all sub-calendars recursively.
; Returns #t or #f.
(define (cal-equals? cal1 cal2)
  (cond ((not(string=?((cal1 'getowner)) ((cal2 'getowner))))#f)
        ((not(ap-list-equal?((cal1 'getappointments)) ((cal2 'getappointments))))#f)
        ((not(eq? (length ((cal1 'getcalendars))) (length((cal2 'getcalendars))))) #f)
        ((null? ((cal1 'getcalendars))) #t)
        (else (cal-equals? (car((cal1 'getcalendars))) (car((cal2 'getcalendars)))))))

; Function to check if two lists of appointments are equal.
; The appointments must be in the same order for the lists to be equal.
; Returns #t or #f.
(define (ap-list-equal? aplst1 aplst2)
  (cond ((and(null? aplst1) (null? aplst2)) #t)
        ((not(appointments-equal? (car aplst1) (car aplst2))) #f)
        (else (ap-list-equal? (cdr aplst1) (cdr aplst2)))))

      
; Function to construct an appointment.
; Takes a name, a start time, and end time and details.
; Name and details are strings, start time and end time are of the time construct, created with "construct-time".
; Returns a lambda expression to emulate an object.
(define (construct-appointment name start end details)
    (define (getname) name)
    (define (getstart) start)
    (define (getstart_minutes) (convert-time-to-minute start)) ; Calculates the start time of the appointment IN MINUTES.
    (define (getduration) (-(convert-time-to-minute end) (convert-time-to-minute start)))
    (define (getdetails) details)
    (define (getend) end) ; Calculates the end of the appointment IN MINUTES.
    (define (getend_minutes) (convert-time-to-minute end))
    (define (self message)
      (cond ((eqv? message 'getname) getname)
            ((eqv? message 'getstart) getstart)
            ((eqv? message 'getstart_minutes) getstart_minutes)
            ((eqv? message 'getduration) getduration)
            ((eqv? message 'getdetails) getdetails)
            ((eqv? message 'getend) getend)
            ((eqv? message 'getend_minutes) getend_minutes)
            (else (error "Message not understood" message))))
    self)
  
; Checks if two times overlap.
; Returns #t or #f.
(define(overlaps? time1 duration1 time2 duration2)
  (let ((start1 (convert-time-to-minute time1))
        (start2 (convert-time-to-minute time2))
        (end1 (+ (convert-time-to-minute time1) duration1))
        (end2 (+ (convert-time-to-minute time2) duration2)))
    (<=(max? start1 start2)(min? end1 end2))))

; Returns the larger number of the two inputs.
(define (max? i j)
  (cond ((> i j) i)
        (else j)))

; Returns the smaller number of the two inputs.
(define (min? i j)
  (cond ((> i j) j)
        (else i)))

; Function to determine if two appointments overlap.
; Returns #t or #f.
(define (appointments-overlap? ap1 ap2)
  (let ((start1 ((ap1 'getstart)))
        (start2 ((ap2 'getstart)))
        (duration1 ((ap1 'getduration)))
        (duration2 ((ap2 'getduration))))
    (overlaps? start1 duration1 start2 duration2)))

; Function to determine if two calendars overlap.
; This uses the flatten function to obtain two lists of appointments.
; First we check if either calendar contains no appointments. If this is the case, there is no overlap.
; Secondly, I create a lambda expression to use as a predicate for the "find-appointments" function.
; This predicate compares the first appointment in cal1 with the input.
; Thus, line 6 returns a list of appointments that are equal to the first element in cal1.
; If this list is empty, we recursively test the overlap on the rest of the appointment list.
; Returns #t or #t.
(define (calendars-overlap? cal1 cal2)
  (let ((aplst1 (((flatten cal1)'getappointments)))
        (aplst2 (((flatten cal2)'getappointments))))
    (define (helper x) (appointments-overlap? (car aplst1) x))
    (cond ((or(null? aplst1)(null? aplst2)) #f)
          ((<= 1 (length(find-appointments cal2 helper))) #t)
          (else (calendars-overlap? (construct-calendar ((cal1 'getowner)) (cdr aplst1) '()) cal2)))))


; ---------------------HANDLING TIME STARTS HERE:------------------------------------------------------

; Simple constructor for time.
; We do not consider error-check dates here.
; Returns a list of 5 elements.
(define (construct-time year month day hour minute)
  (list year month day hour minute))

; Constructs a time list with error checking.
; Indentation for this one is a bit troublesome.
; Essentially all parameters are range-checked.
; Returns a time-list if parameters are okay, otherwise gives an error.
(define (construct-time-error-check year month day hour minute)
  (let ((time (construct-time year month day hour minute)))
    (cond ((and (and (and (and (> year 1969)
                               (and (> month -1)
                           (< month 13)))
                      (and (> day -1)
                           (<= day (days-in-month month (is-leap-year? year)))))
                      (and (> hour -1)
                           (< hour 24)))
                      (and (> minute -1)
                           (< minute 60 ))) time)
          (else (error "Errorneus date entered." (convert-time-to-string time))))))
        
; Returns first element of the list.
(define (getyear time)
  (car time))

; Returns second element of the list.
(define (getmonth time)
  (car (cdr time)))

; Returns third element of the list.
(define (getday time)
  (car (cdr (cdr time))))

; Returns fourth element of the list.
(define (gethour time)
  (car (cdr (cdr (cdr time)))))

; Returns fifth element of the list.
(define (getminute time)
  (car (cdr (cdr (cdr (cdr time))))))

; Function to determine if a year is a leap year.
; Returns #t or #f.
(define (is-leap-year? year)
  (cond ((= (modulo year 400) 0) #t)
        ((and (= (modulo year 4) 0)(= (modulo year 100)0)) #f)
        ((= (modulo year 4) 0) #t)
        (else #f)))

; Calculate total minutes from unix start time (1/1/1970)
(define (convert-time-to-minute time)
  (minutes-since-start-unix time))

; Calculates end-time in minutes based on start-time and duration.
; Returns a number.
(define (calc-endtime start duration)
  (+ duration (convert-time-to-minute start)))

; Time is converted to minutes by splitting the calculations up, such that full years are calculated first.
; Afterwards, full months for the entered year are calculated.
; Finally, full days are converted to minutes, hours are converted and minutes are added.

; To illustrate, take 15:20, 10/10/2016.
; First, the full years passed since 1970 are converted to minutes. Hence, 2016 is not included.
; Secondly, 9 full months have passed in 2016, thus we calculate the first 9 months in minutes.
; Thirdly, 9 full days have passed. This is also converted to minutes.
; Lastly, 15 hours and 20 minutes is converted to minutes and everything is added together.

; Function to determine amount of minutes in a year.
; Returns a number.
(define (minutes-in-year year)
  (cond ((is-leap-year? year) (* 366 (* 24 60)))
        (else (* 365 (* 24 60)))))

; Function that determines amount of time passed since unix start time.
; Splits calculations up into elapsed time in current year + time before current year.
; Returns a number.
(define (minutes-since-start-unix time)
  (+ (minutes-since-start-help (-(getyear time)1))
     (minutes-current-year time)))

; This function gives amount of minutes between 1/1/1970 12:00 am and 1/1/year 12:00 am.
(define (minutes-since-start-help year)
  (cond ((= year 1969) 0)
        (else (+ (minutes-in-year year)
                 (minutes-since-start-help (- year 1))))))
  
;Calculate minutes in this year.
(define (minutes-current-year time)
  (+ (months-to-minute (-(getmonth time)1) (is-leap-year? (getyear time)))
     (*(-(getday time)1)24 60)
     (*(gethour time) 60)
     (getminute time)))

; Calculates accumulated amount of minutes for one year till a given month. 
(define (months-to-minute month leap_year)
  (cond ((= month 0) 0)
        (else (+ (*(days-in-month month leap_year)60 24)
                 (months-to-minute (- month 1) leap_year)))))

; Calculates amount of days in a month. Also considers if leap year or not.
; Returns 28, 29, 30 or 31.
(define (days-in-month month leap_year)
  (cond ((or (= month 1) (= month 3) (= month 5) (= month 7) (= month 8) (= month 10) (= month 12)) 31)
        ((or (= month 4) (= month 6) (= month 9) (= month 11)) 30)
        (leap_year 29)
        (else 28)))
; ------------------ HTML PRINTER -------------------------------------------------

; Function to print html.
; First we use let to create local variables.
; aplst is used as the list of appointments that are in range of the times given.
; The output-port is the output stream.
; This function has an unspecified return type.

(define (present-calendar-html cal from-time to-time)
  (let ((aplst (find-appointments-in-range cal from-time to-time))
        (output-port (open-output-file "output.html")))
    (html-title output-port 'Output aplst)
    (close-output-port output-port)))

; Function to filter a calendar for appointments within a time range.
; The function filters based on overlap with the interval [from-time, to-time].
; First we use let* to allow local variables to reference eachother on initialization.
; test-ap is an appointment used to test for overlap.
; aplst is a list of appointments given by calling flatten on the calendar.
; pred is a lambda expression passed to the filtering.
; The predicate simply tests for overlap between an input and the test-ap appointment.
; Finally, the output list from filtering is sorted on start-time and returned.
; Returns a list of appointments.
(define (find-appointments-in-range cal from-time to-time)
  (let* ((test-ap (construct-appointment "" from-time to-time ""))
        (aplst (((flatten cal)'getappointments)))
        (pred (lambda (x) (appointments-overlap? x test-ap))))
    (sort:sort (filter pred aplst) less??)))

; Function to convert a time-list to a string.
; Returns a string of the format dd/mm/yyyy hh:mm.
(define (convert-time-to-string time)
  (string-append (number->string (getday time)) "/" (number->string (getmonth time)) "/" (number->string (getyear time)) " " (number->string (gethour time)) ":" (number->string (getminute time))))

; Writes to the output stream.
; Handles printing the body of the html file and specifying the table structure.
(define (print-body outputstream appointments)
  (write '<table outputstream)
  (write-char #\space outputstream)
  (write 'style= outputstream)
  (write-char #\" outputstream)
  (write 'width:100% outputstream)
  (write-char #\" outputstream)
  (write '> outputstream)
  (newline outputstream)
  (print-body-start outputstream)
  (print-body-recursive outputstream appointments)
  (write '</table> outputstream)
  (newline outputstream))

; Writes to the output stream a.
; Creates an initial row to illustrate what each coloumn is.
(define (print-body-start outputstream)
  (write '<th> outputstream)
  (write 'Appointment outputstream)
  (write '</th> outputstream)
  (write '<th> outputstream)
  (write 'Start outputstream)
  (write '</th> outputstream)
  (write '<th> outputstream)
  (write 'End outputstream)
  (write '</th> outputstream)
  (write '<th> outputstream)
  (write 'Duration outputstream)
  (write '</th> outputstream)
  (newline outputstream)
  (write '</tr> outputstream))

; Recursively prints out the appointments to the output stream a.
; Assumes that a table is created in the html file.
(define (print-body-recursive outputstream appointments)
  ( cond ((null? appointments) )
         (else (write '<tr> outputstream)
               (newline outputstream)
               (write '<th> outputstream)
               (write (((car appointments)'getname)) outputstream)
               (write '</th> outputstream)
               (write '<th> outputstream)
               (write (convert-time-to-string (((car appointments)'getstart))) outputstream)
               (write '</th> outputstream)
               (write '<th> outputstream)
               (write (convert-time-to-string (((car appointments)'getend))) outputstream)
               (write '</th> outputstream)
               (write '<th> outputstream)
               (write (((car appointments)'getduration)) outputstream)
               (write '</th> outputstream)
               (newline outputstream)
               (write '</tr> outputstream)
               (newline outputstream)
               (print-body-recursive outputstream (cdr appointments)))))

; Gives the html file a header and wraps the html tag around body.
(define (html-title outputstream page-title appointments)
  (write '<!DOCTYPE outputstream)
  (write-char #\space outputstream)
  (write 'html> outputstream)
  (newline outputstream)
  (write '<html> outputstream)
  (newline outputstream)
  (write '<title> outputstream)
  (write page-title outputstream)
  (write '</title> outputstream)
  (newline outputstream)
  (write '</head> outputstream)
  (newline outputstream)
  (print-body outputstream appointments)
  (write '</html> outputstream))

; -------------------------------------TEST ENVIRONMENT SETUP---------------------------------------------------------

;(minutes-since-start-unix (construct-time-error-check 2016 12 24 23 59))
(define a1 (construct-appointment "Meeting1" (construct-time-error-check 2016 1 1 10 0) (construct-time-error-check 2016 1 1 11 0) "Important meeting"))

(define a2 (construct-appointment "Meeting2" (construct-time-error-check 2016 1 2 10 0) (construct-time-error-check 2016 3 2 12 0) "Important meeting"))

(define a3 (construct-appointment "Meeting3" (construct-time-error-check 2016 1 3 10 0) (construct-time-error-check 2016 1 3 13 0) "Important meeting"))

(define a4 (construct-appointment "Meeting4" (construct-time-error-check 2016 1 4 10 0) (construct-time-error-check 2016 1 4 14 0) "Important meeting"))

(define a5 (construct-appointment "Meeting5" (construct-time-error-check 2016 1 5 10 0) (construct-time-error-check 2016 2 5 15 10) "Important meeting"))

(define a6 (construct-appointment "Meeting6" (construct-time-error-check 2018 1 5 10 0) (construct-time-error-check 2086 1 5 15 10) "Important meeting"))

(define c2 (construct-calendar "Simon" (list a2 a6) '()))

(define c4 (construct-calendar "Simon" '() '()))

(define c3 (construct-calendar "Simon" (list a4) (list c4)))

(define c1 (construct-calendar "Simon" (list a1 a3 a5) (list c2 c3 )))

(define t1 (construct-time-error-check 2015 1 1 12 0))

(define t2 (construct-time-error-check 2017 1 1 12 1))

(present-calendar-html c1 t1 t2)

;(find-appointments-in-range c1 t1 t2)
;(convert-time-to-string t1)
;(flatten c1)

;(define f1 (flatten c1))

;(list? ((f1 'getappointments)))

;(sort:sort ((f1 'getappointments)) less??)

;(define (test-pred ap) (string=?((ap 'getname)) "meet bob1"))

;(find-appointments f1 test-pred)

;(((car (find-appointments f1 test-pred)) 'getstart_minutes))
;(((car(cdr (find-appointments f1 test-pred))) 'getstart_minutes))

;(find-appointments c1 test-pred)

;(((find-first-appointment c1 test-pred) 'getstart_minutes))

;(((find-last-appointment c1 test-pred) 'getstart_minutes))

;(((car (reverse (sort:sort (find-appointments f1 test-pred) less??)))'getstart_minutes))
;(appointments-overlap? a1 a2)
;(appointments-equal? a1 a2)

;(rm-ap-helper (list a1 a2 a3 a4 a5) a3)