;;; picasm-rx.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Yuta Yamada

;; Author: Yuta Yamada <sleepboy.zzz@gmail.com>
;; Keywords:

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

;;

;;; Code:
(require 'rx)

(eval-and-compile

  (defvar picasm-rx-constituents
    `((inst
       . ,(rx symbol-start
              (or
               ;; A
               "ADDLW" "ADDWF" "ANDLW" "ANDWF" "ANDWF"
               ;; B
               "BCF" "BSF"
               ;; C
               "CLRF" "COMF" "CLRW" "CLRWDT"
               ;; D
               "DECF"
               ;; I
               "INCF" "IORWF" "IORLW"
               ;; M
               "MOVF" "MOVFW" "MOVLW" "MOVWF"
               ;; O
               "OPTION"
               ;; N
               "NOP"
               ;; R
               "RLF" "RRF"
               ;; S
               "SLEEP" "SUBLW" "SUBWF" "SWAPF"
               ;; T
               "TRIS"
               ;; X
               "XORLW" "XORWF")
              symbol-end))
      (inst-one-or-two-cycle
       . ,(rx symbol-start
              (or "BTFSC" "BTFSS" "DECFSZ" "INCFSZ")))
      (inst-two-cycle
       . ,(rx symbol-start
              (or "CALL" "GOTO" "RETFIE" "RETLW" "RETURN")))
      (numbers
       . ,(rx
           (or
            (and (or "b" "B") "'" (1+ (in "0-1")) "'")
            (and (or "o" "O") "'" (1+ (in "0-7")) "'")
            (and (or "q" "Q") "'" (1+ (in "0-7")) "'")
            (and (or "d" "D") "'" (1+ num) "'")
            (and (or "h" "H") "'" (1+ hex) "'")
            (and (1+ (in "0-1"))       (or "b" "B"))
            (and (1+ (in "0-7"))       (or "o" "O"))
            (and (1+ (in "0-7"))       (or "q" "Q"))
            (and (1+ num) (or "d" "D"))
            (and (1+ hex) (or "h" "H"))
            (and "." (1+ num))
            (and "0" (or "x" "X") (1+ hex)))))
      (pp-directive
       . ,(rx
           (or "list" "equ" "constant" "res" "MACRO" "ENDM" "ORG" "RADIX" "PAGESEL" "BANKSEL"
               (and "#" (or "include" "define" "if" "else" "endif" "ifdef" "ifndef")))))
      (section-marker
       . ,(rx symbol-start
              (or (and "UDATA" (? "_SHR")) "CODE" "__CONFIG" "END")
              symbol-end))
      (block-re
       . ,(rx (or "CBLOCK" "ENDC")))
      (label
       . ,(rx line-start
              (group
               (or (any "a-zA-Z_"))
               (regex "\\(?:[[:alnum:]]\\|_\\)\\{0,31\\}")
               ":")
              (* blank)
              (? ";" (0+ any) line-end)))
      (cblock
       . ,(rx
           (and symbol-start "cblock" symbol-end
                (* blank)
                (1+ alnum)
                (* blank)
                (? ";" (0+ any) line-end))))
      (endc
       . ,(rx
           (and symbol-start "endc" symbol-end
                (* blank)
                (? ";" (0+ any) line-end))))
      (identifier
       . "[[:alnum:]_,<>]+[^:]")
      (syntheticop-keyword
       . ,(rx symbol-start
              (and (or "BANK" "PAGE") "SEL")
              symbol-end))))

  (defmacro picasm-rx (&rest regexps)
    "Picasm mode specialized rx macro.
This variant of `rx' supports common picasm named REGEXPS."
    (let ((rx-constituents (append picasm-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t)))))

  (add-to-list 'picasm-rx-constituents
               (cons 'all-inst
                     (picasm-rx (or inst inst-one-or-two-cycle inst-two-cycle))))

  (add-to-list 'picasm-rx-constituents
               (cons 'start-block
                     (picasm-rx (or label cblock))))
  ) ; end of ‘eval-and-compile’

(provide 'picasm-rx)
;;; picasm-rx.el ends here
