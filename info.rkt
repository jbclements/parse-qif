#lang info

(define name "parse-qif")
(define collection "parse-qif")
(define version "2018-03-02")

(define raco-commands
  (list
   (list "parse-qif"
         "main.rkt"
         "parse a qif file, associate categories, write out again"
         #f)))