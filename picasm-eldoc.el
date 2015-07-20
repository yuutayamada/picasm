;;; picasm-eldoc.el --- eldoc support for picasm -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Yuta Yamada

;; Author: Yuta Yamada <cokesboy@gmail.com>
;; Keywords: picasm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Currently, I only added instructions of PIC16F505 and PIC16F609.

;;; Code:

(require 'eldoc)

;; d   = either ",W" or ",F"
;; b   = any bit from 0-7
;; loc = limited to 0x00 -> 0x1F on 12 bit core parts or cblock's
;; variable.
(defconst picasm-eldoc--instructions
  '((ADDLW  . "(value) Add the Literal VALUE to the W")
    (ADDWF  . "(loc,d) Add W and F registers at LOCation F (F + W -> d)")
    (ANDLW  . "(value) Perform AND with a Literal VALUE *&* W register")
    (ANDWF  . "(loc,d) Perform a logical AND with W and value found at LOCation in the F register")
    (BCF    . "(F-loc,b) Bit Clear(0) an location of F's B-th bit")
    (BSF    . "(F-loc,b) Bit Set(1) an location of F's B-th bit")
    (CLRF   . "(F-loc) Clear & store a zero in the location of F | Z")
    (CLRW   . " Clear(put zero into) W register | Z")
    (COMF   . "(F-loc,F|W) Complement (invert) the bits found in F-loc and store to F|W")
    (CLRWDT . "Clear the Watchdog Timer | Z")
    (DECF   . "(loc, F|W) Decrements location and store it to F|W")
    (INCF   . "(loc, F|W) Increments location and store it to F|W")
    (IORWF  . "(loc, d) Performs an Inclusive OR with W and Location. The result is stored at D resister.")
    (IORLW  . "(value) Performs an Inclusive OR with a Literal VALUE and and W. The result is in the W.")
    (MOVF   . "(loc, d) Moves Location's value to d")
    (MOVLW  . "(value) Moves VALUE into W")
    (MOVFW  . "(loc) location to W") ; not official?
    (MOVWF  . "(loc) W to location")
    (OPTION . " Copies the value found in the W register into the Option register")
    (NOP    . " Do nothing, but it spends 1 instruction time ")
    (RLF    . "(loc,d) Rotate(shift) Left the Location reg through the Carry Flag to D")
    (RRF    . "(loc,d) Rotate(shift) Right the Location reg though the Carry Flag to D")
    (SLEEP  . " Go to a standby state by going to sleep")
    (SUBLW  . "(value) L - W => W where L is literal VALUE")
    (SUBWF  . "(loc,d) F - W => d")
    (SWAPF  . "(loc,d) swap the high nibble and the low nibble in the Location")
    (TRIS   . "(loc) Copies the W into the TRIS register. 1 = input, 0 = output")
    (XORLW  . "(value) Exclusive OR XOR(VALUE, W) => W")
    (XORWF  . "(loc,d) Exclusive OR XOR(loc, W) => F|W")
    ;; one or two instruction time
    (BTFSS  . "(loc, b) Bit Test an F, skip next inst if it's Set(1)")
    (BTFSC  . "(loc, b) Bit Test an F, skip next inst if it's Clear(0)")
    (DECFSZ . "(loc, d) Decrements loc and skip next inst if the result is zero")
    (INCFSZ . "(loc, d) Increments loc and skip next inst if the result is zero")
    ;; two instruction time
    (CALL   . "(label) Call subroutine LABEL")
    (GOTO   . "(address) GOTO ADDRESS or Label name")
    (RETFIE . " Return From an Interrupt")
    (RETLW  . "(value) Return with the Literal VALUE in the W register")
    (RETURN . " Returns to the next instruction after a CALL instruction")))

(defun picasm-eldoc-function ()
  "Return a doc string appropriate for the current context, or nil."
  (save-excursion
    (back-to-indentation)
    (let ((sym (thing-at-point 'symbol)))
      (when sym
        (picasm-eldoc-get (intern-soft (upcase sym)))))))

(defun picasm-eldoc-get (symbol)
  "Return message that show on eldoc's mini-buffer from SYMBOL."
  (let ((msg (assoc-default symbol picasm-eldoc--instructions)))
    (when msg
      (let* ((obj (format "%s%s" symbol msg))
             (len (length (symbol-name symbol))))
        (put-text-property 0 len 'face 'font-lock-function-name-face obj)
        obj))))

(defun picasm-eldoc-setup ()
  (when (eq major-mode 'picasm-mode)
    (set (make-local-variable 'eldoc-documentation-function)
         'picasm-eldoc-function)))

(provide 'picasm-eldoc)
;;; picasm-eldoc.el ends here
