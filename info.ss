#lang setup/infotab
(define name "Identicons")
(define blurb
  '("Library for generating identicons from 32-bit numbers. Based on an idea by "
    (a ((href "http://www.docuverse.com/blog/donpark/2007/01/18/visual-security-9-block-ip-identification")) "Don Park")
    "."))
(define categories '(media net))
(define repositories '("4.x"))
(define can-be-loaded-with 'all)
(define primary-file "identicons.ss")
(define scribblings '(("manual.scrbl" ())))
(define version "1.1")
(define release-notes '("Fixed on-bitmap and display-in-frame methods, and added a new method as-image-snip. Thanks to Danny Yoo for patches!"))
