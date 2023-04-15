#lang info

(define name "parse-qif")
(define collection "parse-qif")
(define version "1.0")

(define raco-commands
  (list
   (list "parse-qif"
         "main.rkt"
         "parse a qif file, associate categories, write out again"
         #f)))

(define deps
  "base"
  "rackunit-typed"
  "srfi-lite-lib"
  "typed-racket-lib")