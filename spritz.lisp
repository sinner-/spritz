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

;;;; spritz.lisp

(defpackage #:spritz
  (:use #:cl)
  (:export
   #:make-state
   #:absorb
   #:squeeze
   #:hash))

(in-package #:spritz)

(defstruct
    (state
     (:constructor
         make-state
         (&key
            (N 256) (i 0) (j 0) (k 0) (z 0) (a 0) (w 1)
            (S (let ((new-array
                       (make-array N
                                   :element-type '(unsigned-byte 8))))
                 (dotimes (v N new-array)
                   (setf (elt new-array v) v))))
            )))
  N (i 0 :type (unsigned-byte 8))
  (j 0 :type (unsigned-byte 8)) (k 0 :type (unsigned-byte 8))
  (z 0 :type (unsigned-byte 8)) (a 0 :type (unsigned-byte 8))
  (w 1 :type (unsigned-byte 8)) S)

(declaim (inline low))
(defun low (b)
  (logand b #X0f))

(declaim (inline high))
(defun high (b)
  (ash b -4))

(defmacro elt-state-S (state index)
  `(elt (slot-value ,state 'S)
        (mod ,index (slot-value ,state 'N))))

(defun output (spritz)
  (macrolet ((S (index)
               `(elt-state-S spritz ,index)))
   (with-slots (i j k z)
       spritz
     (setf z (S (+ j (S (+ i (S (+ z k))))))))))

(defun update (spritz)
  (macrolet ((elt-S (index)
               `(elt-state-S spritz ,index)))
    (with-slots (i j k w N S)
        spritz
      (setf i (mod (+ i w) N))
      (setf j (mod (+ k (elt-S (+ j (elt-S i)))) N))
      (setf k (mod (+ i k (elt-S j)) N))
      (rotatef (elt S i)
               (elt S j)))))

(defun whip (spritz r)
  (dotimes (v r)
    (update spritz))
  (with-slots (w N)
      spritz
   (loop
     :do (setf w (mod (1+ w) N))
     :until (= (gcd w N) 1))))

(defun crush (spritz)
  (with-slots (N S)
      spritz
   (dotimes (v (/ N 2))
     (when (> (elt S v) (elt S (- N 1 v)))
       (rotatef (elt S v) (elt S (- N 1 v)))))))

(defun shuffle (spritz)
  (with-slots (N a)
      spritz
    (whip spritz (* N 2))
    (crush spritz)
    (whip spritz (* N 2))
    (crush spritz)
    (whip spritz (* N 2))
    (setf a 0)))

(defun drip (spritz)
  (with-slots (a)
      spritz
   (when (> a 0)
     (shuffle spritz)))
  (update spritz)
  (output spritz))

(defun absorb-nibble (spritz byte)
  (macrolet ((elt-S (index)
               `(elt-state-S spritz ,index)))
   (with-slots (N a)
       spritz
     (if (= a (/ N 2))
         (shuffle spritz))
     (rotatef (elt-S a) (elt-S (+ (/ N 2) byte)))
     (incf a))))

(defun absorb-byte (spritz byte)
  (absorb-nibble spritz (low byte))
  (absorb-nibble spritz (high byte)))

(defun absorb (spritz data)
  (etypecase data
    (list
       (loop :for byte :in data
             :do (absorb-byte spritz byte)))
    (vector
       (loop :for byte :across data
             :do (absorb-byte spritz (char-code byte))))))

(defun absorb-stop (spritz)
  (with-slots (N a)
      spritz
    (when (= a (/ N 2))
        (shuffle spritz))
    (incf a)))

(defun squeeze (spritz len)
  (with-slots (N a)
      spritz
   (when (> a 0)
     (shuffle spritz))
   (let ((p (make-array len :element-type '(unsigned-byte 8))))
     (dotimes (v len p)
       (setf (elt p v) (drip spritz))))))

(defun hash (data &key (size 20))
  (let ((spritz (make-state)))
    (absorb spritz data)
    (absorb-stop spritz)
    (absorb
     spritz
     (loop :for x = size :then (ash x -8)
           :while (> x 0)
           :collect (logand #xff x)))
    (squeeze spritz size)))
