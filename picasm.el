;;; picasm.el --- PIC ASM editing mode -*- lexical-binding: t; -*-
;; supports PIC10, PIC12 and PIC16 instruction sets
;;
;; Author: Daniel Debertin <dan@mapcar.org>
;; Maintainer: Yuta Yamada <cokesboy@gmail.com>
;;
;; Features:
;;
;; Syntax highlighting
;; Indentation
;; Electric comment
;; Bundled PIC chip database
;; Assembler/Linker support for GNU gputils and Microchip's MPASM
;; Programmer support for the pk2cmd program
;; Delay loop calculation and code generation

;;; Commentary:

;;; Code:

(require 'xml)
(require 'cl-lib)
(require 'picasm-vars)
(require 'picasm-rx)
(require 'picasm-external)
(require 'picasm-eldoc)
(require 'rx)

(defconst picasm-mode-font-lock-keywords
  `((,(picasm-rx inst)                  . font-lock-keyword-face)
    (,(picasm-rx inst-one-or-two-cycle) . font-lock-warning-face)
    (,(picasm-rx inst-two-cycle)        . font-lock-constant-face)
    (,(picasm-rx syntheticop-keyword)   . font-lock-builtin-face)
    (,(picasm-rx pp-directive)          . font-lock-preprocessor-face)
    (,(picasm-rx section-marker)        . font-lock-keyword-face)
    (,(picasm-rx no-indent)             . (1 font-lock-function-name-face))
    (,(picasm-rx identifier)            . font-lock-variable-name-face)
    (,(picasm-rx numbers)               . font-lock-constant-face)))

(defun picasm-strip-trailing-whitespace ()
  (save-excursion
    (beginning-of-line)
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match ""))))

(defun picasm-mode-indent-instruction-line ()
  "Indent an instruction."
  (interactive)
  (let ((indent
         (save-excursion
           (back-to-indentation)
           (cond ((looking-at (picasm-rx section-marker))
                  picasm-section-marker-indent-spaces)
                 ((looking-at (picasm-rx (or all-inst "ORG")))
                  picasm-instruction-indent-spaces)
                 (t
                  (forward-comment -1)
                  (cond
                   ((looking-back (picasm-rx start-block) nil)
                    picasm-instruction-indent-spaces)
                   ((looking-back (picasm-rx condition-block) nil)
                    picasm-condition-block-indent-spaces)
                   (t ; use previous indent
                    (back-to-indentation)
                    (min (current-column)
                         picasm-instruction-indent-spaces))))))))
    (when indent
      (indent-line-to indent))))

(defun picasm-electric-comment ()
  "Insert a comment at EOL, move point to it.
If there is already a comment there, move point to it.  Otherwise, insert
a semicolon."
  (interactive)
  (let ((p (point)))
    (beginning-of-line)
    (cond ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+[^ \t]+[ \t]*$")
           (picasm-strip-trailing-whitespace)
           (end-of-line)
           (cl-dotimes (_ picasm-instruction-comment-indent-tabs)
             (insert "  "))
           (insert "; "))
          ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+[^ \t]+[ \t]+;.*$")
           (end-of-line))
          (t
           (goto-char p)
           (insert ";")))))

(defun picasm-read-pic-database ()
  (message "Reading the chip database...")
  (require 'xml)
  (let ((rawdb (car (xml-parse-file pic-database-file))))
    (cl-dolist (entry (xml-get-children rawdb 'Chip))
      (setf (gethash (xml-get-attribute entry 'Name) pic-database) entry))))

;;;###autoload
(define-derived-mode picasm-mode prog-mode "asm"
  "A major mode for an assembly language of PIC."
  :group 'picasm
  (setq-local indent-tabs-mode nil)
  (set-syntax-table picasm-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(picasm-mode-font-lock-keywords nil t))
  (set (make-local-variable 'indent-line-function) 'picasm-mode-indent-instruction-line)
  (setq mode-name (format "PICasm [%s]" (or picasm-chip-select "???")))
  (set (make-local-variable 'font-lock-keywords)
       '(picasm-mode-font-lock-keywords))
  (set (make-local-variable 'comment-start) ";")
  (when (= (hash-table-size pic-database) 0)
    (picasm-read-pic-database))
  (picasm-eldoc-setup))

(defun picasm-select-chip (newchip)
  (interactive "MSelect chip: ")
  (setq picasm-chip-select (upcase newchip))
  (setq mode-name (format "PICasm [%s]" (upcase newchip)))
  (force-mode-line-update))

(defun picasm-describe-chip (&optional chip-name)
  (interactive "P")
  (let ((temp-buffer-name (format "*chip (%s) description*" picasm-chip-select))
        (chip-descr (gethash (or chip-name picasm-chip-select) pic-database)))
    (if (null chip-descr)
        (error (format "%s is not in the database. Please contact the author to have it added." picasm-chip-select)))
    (with-output-to-temp-buffer temp-buffer-name
      (set-buffer temp-buffer-name)
      (insert (format "Name: %s\nPins: %d\n"
                      (xml-get-attribute chip-descr 'Name)
                      (string-to-number (xml-get-attribute chip-descr 'PinCount))))
      (let* ((periphs (car (xml-get-children chip-descr 'Peripherals)))
             (adc (or (cl-caddar (xml-get-children periphs 'ADCChannels)) "0"))
             (uart (or (cl-caddar (xml-get-children periphs 'UARTs)) "0"))
             (usart (or (cl-caddar (xml-get-children periphs 'USARTs)) "0"))
             (spi (or (cl-caddar (xml-get-children periphs 'SPI)) "0"))
             (i2c (or (cl-caddar (xml-get-children periphs 'I2C)) "0"))
             (ssp (or (cl-caddar (xml-get-children periphs 'SSP)) "0"))
             (mssp (or (cl-caddar (xml-get-children periphs 'MSSP)) "0")))
        (insert (format "ADC Channels: %s\nUARTs: %s\nUSARTs: %s\nSPI: %s\nI2C: %s\nSSP: %s\n MSSP: %s\n" adc uart usart spi i2c ssp mssp))
        (dolist (timer (xml-get-children periphs 'Timer))
          (insert (format "Timer: %d at %d bits\n" (string-to-number (xml-get-attribute timer 'Count)) (string-to-number (xml-get-attribute timer 'Bits))))))
      (insert (format "Memory: %d\n" (string-to-number (cl-caddar (xml-get-children chip-descr 'Memory)))))
      (dolist (osc (xml-get-children chip-descr 'Oscillator))
        (insert (format "Oscillator: %s\n" (xml-get-attribute osc 'Speed)))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|inc\\)\\'" . picasm-mode))

(provide 'picasm)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; picasm.el ends here
