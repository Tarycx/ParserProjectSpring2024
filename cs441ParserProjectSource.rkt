#lang racket
;CS 441 Programming Langagues
;Instructor: Hare
;Name: Thomas Reilly
;Due: Feb 3, Sun 11:59pm, 2024

#|GRAMMER REQUIRED:
program -> linelist $$ 
linelist -> line linelist | epsilon 
line ->  label stmt linetail 
label -> id: | epsilon 
linetail -> ;stmt+ | epsilon 
stmt -> id = expr | if (boolean) stmt | while (boolean) linelist endwhile | read id | write expr | goto id | gosub id | return | break | end
boolean -> true | false | expr bool-op expr 
bool-op -> < | > | >= | <= | <> | =
expr -> id etail | num etail | (expr) 
etail -> + expr | - expr | * expr | / expr | epsilon
id -> [a-zA-Z][a-zA-Z0-9]*
num -> numsign digit digit*
numsign -> + | - | epsilon 
digit -> [0-9]
|#

;importing for maybe and either types
(require data/either)
(require data/maybe) 

;Scanning part(creating tokens)
;future implementation, Testing
(define (tokenize token)
  (cond
    ;matching token types
    [(string=? token "=") 'equals]
    [(string=? token "if") 'if]
    [(string=? token "then") 'then]
    [(string=? token "read") 'read]
    [(string=? token "write") 'write]
    [(string=? token "goto") 'goto]
    [(string=? token "gosub") 'gosub]
    [(string=? token "return") 'return]
    [(string=? token "(") 'l-parens]
    [(string=? token ")") 'r-parens]
    [(string=? token "+") 'plus]
    [(string=? token "-") 'minus]
    [(string=? token ":") 'colon]
    [(string=? token "*") 'mult]
    [(regexp-match #rx"^([a-zA-Z]+)$" token) 'num]
    [(or (regexp-match? #rx"^[1-9][0-9]*$" token)
         (string=? token "0")) 'id]
    [else
     'UNKNOWN-SYMBOL]))


;parse: takes file reads it, tokenize file data, checks syntax and outputs acceptance or failure
;Outputs provided file name to user, Calls read-file and returns final output to user
;FixMe: error handling, extra output(maybe conditions)
(define (parse fileName)
  (displayln (format "File: ~a" fileName))
             
  (if (not (read-file fileName))
      nothing 
      (just (displayln (format "Accept")))))



;Func: Read and split lines in list, aids in tokenization
(define (read-file fileName)
  (begin
    ;read lines from file
    (define filelist (file->lines fileName))
    ;begin Tokenization then Parse lines
    (filechecker filelist)))


;Func: deals with end marker identifier and spacing issure at end of file
(define (adjust-end-mark lst)
  ;Func: to check if a line ends with "$$"
  (define (trim-and-check-end-marker line)
    (equal? (string-trim line) "$$"))

  ;Func: removes end marker "$$"
  (let loop ([remaining lst] [result '()])
    (cond
      [(null? remaining) (reverse result)]  ;Base case
      [else
       (let ([current (car remaining)]
             [rest (cdr remaining)])
         (if (trim-and-check-end-marker current)
             (reverse (cons current result))  ;if end marker found, return the reversed result
             (loop rest (cons current result))))])))  ;continue

(define (filechecker in)
  (let ([adjusted-list (adjust-end-mark in)])  ;adjust the input list
    (and 
      (linelist adjusted-list)  ;check if the adjusted list is valid for file processing
      (equal? (string-trim (last adjusted-list)) "$$"))))  ;check if the last line is the end marker

(define (linelist in)
  (or 
    (equal? (string-trim (car in)) "$$")  ;Check if the first line is the end marker
    (and (line (car in))  ;check if the first line is a valid line
         (linelist (cdr in)))))  ;recursively check the rest of the lines

;variable: tracks line number for error messages
(define linenumber 0) 

;line: checks if the line is made up of index statement and a linetail.
;line -> label stmt linetail
(define (line tkn)
  (define strLineList (split-parenthesis tkn))
  (increment linenumber)
  (cond
    ;checks if line contains label,statement, and linetail
    [(and (label (car strLineList))
          (statement (cdr strLineList))
          (linetail (last strLineList))) 
     (just #t)] ;checks for lable
    ;checks if line contains statement and linetail
    [(and (statement strLineList)
          (linetail (last strLineList)))
     (just #t)]
    ;else invalid
    [else (display (format "SYNTAX ERROR: Invalid Line, line: ~a\n" linenumber)) #f]))

;use for spliting parenthesis to tokens
(define (split-parenthesis tkn)
  (filter (lambda (str) (not (string=? "" str)))
  (filter non-empty-string?
          (regexp-split (pregexp "((?=\\()|(?<=\\()|(?=\\))|(?<=\\))|(\\s+))") tkn))))


;label
;check if label is syntaxically correct
;label -> id: | epsilon
(define (label tkn)
  (begin (cond
           [(empty? tkn) #t]
           [(colon-split tkn) #t]
           [else #f])))


;colonsplit
;checking id, dealing with spliting colon to tokens
(define (colon-split tkn)
  (define my-list (regexp-split (pregexp "((?=:)|(?<!:)(?=$))") tkn))
  (cond
    [(and (id (car my-list)) (string=? (last my-list) ":")) #t]
    [else #f]))


;linetail
;checks linetail syntax
;linetail -> ;stmt+ | epsilon
;helper used in line func 
(define (linetail rtr) #t)


;Statement
;checks statement syntax
;stmt -> id = expr | if (boolean) stmt | while (boolean) linelist endwhile | read id | write expr | goto id | gosub id | return | break | end
;variable setting while-checker
(define while-checker #f)

;Statement Funciton
(define (statement tkn)
  (begin (cond
           
           [(and (> (length tkn) 1)
                 (id (car tkn))
                 (string=? (cadr tkn) "=")
                 (expression (cdr (cdr tkn)))) (just #t)] ;id = expr

           [(and (string=? (car tkn) "if")
                 (boolean (cdr tkn))) (just #t)] ;if (boolean) stmt

           [(and (string=? (car tkn) "while")
                 (boolean (cdr tkn))) (while-state-change #t) (just #t)] ;While (boolean) lineList endwhile
           
           [(and (equal? while-checker #t)
                 (equal? (car tkn) "endwhile")) (while-state-change #f) (just #t)]
           
           [(and (> (length tkn) 1)
                 (string=? (car tkn) "read")
                 (id (cadr tkn))) (just #t)]; read id

           [(and (string=? (car tkn) "write")
                 (expression (cdr tkn))) (just #t)] ;write expr
           
           [(and (> (length tkn) 1)
                 (string=? (car tkn) "goto")
                 (id (cadr tkn))) (just #t)] ;goto id
           
           [(and (> (length tkn) 1)
                 (string=? (car tkn) "gosub")
                 (id (cadr tkn))) (just #t)] ; gosub id
           
           [(and (equal? (length tkn) 1)
                 (string=? (car tkn) "return")) (just #t)] ;return
           
           [(and (equal? (length tkn) 1)
                 (string=? (car tkn) "break")) (just #t)] ;break
           
           [(and (equal? (length tkn) 1)
                 (string=? (car tkn) "end")) (just #t)] ;end
           
           [else (display (format "SYNTAX ERROR: Invalid Statement, line: ~a\n" linenumber)) #f])))


;Func: Changes while state boolean, for looping purposes
(define (while-state-change boolVal)
  (set! while-checker boolVal))

;Func: used to help increament line number, used to track error position
(define (increment num)
  (set! linenumber (+ 1 num)))

;
;boolean -> true | false | expr bool-op expr
(define (boolean tkn)
  
  (define boolList1 (list (car tkn) (cadr tkn)))
  (define boolList2 (caddr tkn))
  (define boolList3 (cdddr tkn))

  (cond
           [(equal? tkn "true") (just #t)] ;true case
           [(equal? tkn "false") (just #t)] ;false case
           [(and (expression boolList1)
                 (bool-op boolList2)
                 (par-expr boolList3)) (just #t)] ;exp bool-op
           [else (display (format " SYNTAX ERROR: Invalid Boolean, line: ~a\n" (linenumber))) #f]))

;bool-operations:
;bool-op -> < | > | >= | <= | <> | =
(define (bool-op tkn)
  (begin (cond
           [(equal? tkn "<") (just #t)]
           [(equal? tkn ">") (just #t)]
           [(equal? tkn ">=") (just #t)]
           [(equal? tkn "<=") (just #t)]
           [(equal? tkn "<>") (just #t)]
           [(equal? tkn "=") (just #t)]
           [else #f])))


;Expressions:
;expr -> id etail | num etail | (expr)
(define (expression tkn)
  (begin (cond
           [(empty? tkn) (just #t)]
           
           [(and (string=? (car tkn) "(")
                 (par-expr (cdr tkn))) (just #t)]
           
           [(and (id (car tkn))
                 (etail (cdr tkn))) (just #t)]
           
           [(and (num (car tkn))
                 (etail (cdr tkn))) (just #t)]
           
           [else (display (format "SYNTAX ERROR: Invalid expression, line: ~a\n" linenumber)) #f])))


;parenthesis(expr):
;helping to deal with parenthesis relating to expressions function
(define (par-expr tkn)
  (begin (cond
           [(empty? tkn) (just #t)]
           [(and (id (car tkn))
                 (par-etail (cdr tkn))) (just #t)]
           
           [(and (num (car tkn))
                 (par-etail (cdr tkn))) (just #t)]
           
           [(and (equal? (car tkn) "(")
                 (par-expr (cdr tkn))) (just #t)]
           
           [else #f])))

;parenthesis(etail)
;etail -> + expr | - expr | * expr | / expr | epsilon
;helping with closing side with parenthesis
(define (par-etail tkn)
  (begin (cond
           [(empty? tkn) (just #t)]
           
           [(and (equal? (car tkn) ")")
                 (etail (cdr tkn))) (just #t)] ;closing par case
           
           [(and (equal? (car tkn) "+")
                 (par-expr (cdr tkn))) (just #t)];case expression after a addition sign 
           
           [(and (equal? (car tkn) "-")
                 (par-expr (cdr tkn))) (just #t)];case expression after a minus sign
           
           [(and (equal? (car tkn) "*")
                 (par-expr (cdr tkn))) (just #t)];case expression after a multiplication sign
           
           [(and (equal? (car tkn) "/")
                 (par-expr (cdr tkn))) (just #t)];case expression after a division sign
           
           [else #f])))

;etail func:
;checks expression tail syntax
;etail -> + expr | - expr | * expr | / expr | epsilon
(define (etail tkn)
  (begin (cond
           [(empty? tkn) (just #t)]
           
           [(and (equal? (car tkn) "+")
                 (expression (cdr tkn))) (just #t)];case + expr
           
           [(and (equal? (car tkn) "-")
                 (expression (cdr tkn))) (just #t)];case - expr
           
           [(and (equal? (car tkn) "*")
                 (expression (cdr tkn))) (just #t)];case * expr
           
           [(and (equal? (car tkn) "/")
                 (expression (cdr tkn))) (just #t)];case / expr
           
           [(and (equal? (car tkn) ";")
                 (statement (cdr tkn))) (just #t)]
           
           [else #f])))

;id Func:
;checks identifier syntax
;id -> [a-zA-Z][a-zA-Z0-9]*
(define (id tkn)
    (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9]*$" tkn ))

;num Func:
;checks syntax of numbers
;num -> numsign digit digit*
(define (num tkn)
  (begin (cond
           [(numsign tkn) #t] ;numsign checker
           [(digit tkn) #t] ;digit checker
           [else #f])))

;numsign Func:
;checks number sign syntax
;numsign -> + | - | epsilon
(define (numsign tkn)
  (regexp-match? #rx"^[+-][0-9]+$" tkn))

;digit Func:
;checks digit syntax
;digit -> [0-9]
(define (digit tkn)
  (regexp-match? #rx"^[0-9]+$" tkn))


;Main: parse function call
;Sample Files: File01.txt-File05.txt
;Enter desired file name here
(parse "File05.txt")
