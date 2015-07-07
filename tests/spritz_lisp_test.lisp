;;; Spritz stream hash
;;;
;;; Copyright 2015 Russell Sim <russell.sim@gmail.com>
;;;
;;; This module is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This module is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;;; spritz_lisp_test.lisp

(defpackage #:spritz/tests
  (:use #:cl #:spritz)
  (:import-from
   #:fiveam
   #:def-test
   #:def-suite
   #:in-suite
   #:is))

(in-package #:spritz/tests)

(def-suite :spritz)

(in-suite :spritz)

(def-test basic-test1 ()
  (let ((spritz (make-state)))
   (absorb spritz "ABC")
   (is
    (equalp
     #(#x77 #x9a #x8e #x01 #xf9 #xe9 #xcb #xc0)
     (squeeze spritz 8)))))

(def-test basic-test2 ()
  (let ((spritz (make-state)))
   (absorb spritz "spam")
   (is
    (equalp
     #(#xf0 #x60 #x9a #x1d #xf1 #x43 #xce #xbf)
     (squeeze spritz 8)))))

(def-test basic-test3 ()
  (let ((spritz (make-state)))
   (absorb spritz "arcfour")
   (is
    (equalp
     #(#x1a #xfa #x8b #x5e #xe3 #x37 #xdb #xc7)
     (squeeze spritz 8)))))

(def-test hash-test1 ()
  (is
   (equalp
    #(#x02 #x8f #xa2 #xb4 #x8b #x93 #x4a #x18)
    (subseq (hash "ABC" :size 32) 0 8))))

(def-test hash-test2 ()
  (is
   (equalp
    #(#xac #xbb #xa0 #x81 #x3f #x30 #x0d #x3a)
    (subseq (hash "spam" :size 32) 0 8))))

(def-test hash-test3 ()
  (is
   (equalp
    #(#xff #x8c #xf2 #x68 #x09 #x4c #x87 #xb9)
    (subseq (hash "arcfour" :size 32) 0 8))))
