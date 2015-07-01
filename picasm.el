;; PIC ASM editing mode
;; supports PIC10, PIC12 and PIC16 instruction sets
;;
;; Daniel Debertin <dan@mapcar.org>
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
(require 'xml)
(require 'cl-lib)
(require 'picasm-external)
(require 'rx)

(defcustom pic-database-file "~/.emacs.d/picasm/chips.xml"
  "Location of the PIC chip database (XML-format)."
  :type 'string :group 'picasm)

(defvar pic-database (make-hash-table :test 'equal))

(defconst picasm-mode-synthetic-instructions
  '("pagesel"
    "banksel"))


(defconst picasm-mode-font-lock-instruction-re
  (rx symbol-start
      (or
       "ADDLW" "ADDWF" "ANDLW" "ANDWF" "ANDWF"
       "BCF" "BSF" "BTFSC" "BTFSS"
       "CALL" "CLRW" "CLRF" "COMF" "CLRWDT"
       "DECF" "DECFSZ" "GOTO" "INCF" "INCFSZ" "IORWF" "IORLW"
       "MOVF" "MOVFW" "MOVLW" "MOVWF" "NOP"
       "RETURN" "RETLW" "RETFIE" "RLF" "RRF"
       "SLEEP" "SUBLW" "SUBWF" "SWAPF" "TRIS"
       "XORLW" "XORWF")
      symbol-end))

;; This RE picks up the canonical GPASM number syntaxes as well as
;; legacy MPASM syntaxes for binary, octal, decimal and hexadecimal
;; number literals.
(defconst picasm-mode-number-literal-re
  (rx
   (or
    (and (or "b" "B") "'" (1+ (in "0-1")) "'")
    (and (or "o" "O") "'" (1+ (in "0-7")) "'")
    (and (or "q" "Q") "'" (1+ (in "0-7")) "'")
    (and (or "d" "D") "'" (1+ (in "0-9")) "'")
    (and (or "h" "H") "'" (1+ (in "0-9A-Fa-f")) "'")
    (and (1+ (in "0-1"))       (or "b" "B"))
    (and (1+ (in "0-7"))       (or "o" "O"))
    (and (1+ (in "0-7"))       (or "q" "Q"))
    (and (1+ (in "0-9"))       (or "d" "D"))
    (and (1+ (in "0-9A-Fa-f")) (or "h" "H"))
    (and "." (1+ (in "0-9")))
    (and "0" (or "x" "X") (1+ (in "0-9A-Fa-f"))))))

(defconst picasm-mode-pp-directive-re
  (rx
   (or "list" "equ" "constant" "res" "MACRO" "ENDM" "__CONFIG" "ORG" "RADIX"
       (and "#" (or "include" "define" "if" "else" "endif" "ifdef" "ifndef")))))

(defconst picasm-mode-section-marker-re
  (rx (or (and "UDATA" (? "_SHR")) "CODE")))

(defconst picasm-mode-block-re
  (rx (or "CBLOCK" (and "END" (? "C")))))

(defconst picasm-mode-label-re
  (rx (1+ (in "a-zA-Z_")) ":"))

(defconst picasm-mode-identifier-re
  "[[:alnum:]_,<>]+[^:]")

(defconst picasm-mode-font-lock-syntheticop-keyword-re
  (rx symbol-start
      (and (or "BANK" "PAGE") "SEL")
      symbol-end))

(defconst picasm-mode-font-lock-keywords
  `((,picasm-mode-font-lock-instruction-re         . font-lock-keyword-face)
    (,picasm-mode-font-lock-syntheticop-keyword-re . font-lock-builtin-face)
    (,picasm-mode-number-literal-re                . font-lock-constant-face)
    (,picasm-mode-pp-directive-re                  . font-lock-preprocessor-face)
    (,picasm-mode-section-marker-re                . font-lock-keyword-face)
    (,picasm-mode-label-re                         . font-lock-function-name-face)
    (,picasm-mode-block-re                         . font-lock-type-face)
    (,picasm-mode-identifier-re                    . font-lock-variable-name-face)))

(defcustom picasm-instruction-indent-spaces 6
  "Number of spaces to indent instruction lines."
  :type 'integer :group 'picasm)

(defcustom picasm-section-marker-indent-spaces 8
  "Number of spaces to indent section markers."
  :type 'integer :group 'picasm)

(defcustom picasm-instruction-argument-indent-tabs 2
  "Number of tabs to insert after instructions, before arguments."
  :type 'integer :group 'picasm)

(defcustom picasm-instruction-comment-indent-tabs 2
  "Number of tabs to indent comments."
  :type 'integer :group 'picasm)

(defcustom picasm-require-comment t
  "Whether to require a comment on every line (even if empty)."
  :type 'boolean :group 'picasm)

(defun strip-trailing-whitespace ()
  (save-excursion
    (beginning-of-line)
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match ""))))

(defun picasm-mode-indent-instruction-line ()
  "Indent an instruction."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (cond ((looking-at "^[ \t]*\\(UDATA\\(?:_SHR\\)?\\|CODE\\|__CONFIG\\|END\\)")
     (progn
       (indent-line-to picasm-section-marker-indent-spaces)
       (end-of-line)))
    ((looking-at "^[[:alnum:]_]:?")   ; label
     (progn
       (indent-line-to 0)
       (end-of-line)))
    ((looking-at "^\s*$")   ; line is empty, assume we want to enter an ins
     (indent-line-to picasm-instruction-indent-spaces))
    ; instruction, but no arg. advance to argument position.
    ((looking-at "^[ \t]+[[:alpha:]]+$")
     (progn
       (end-of-line)
       (dotimes (i picasm-instruction-argument-indent-tabs)
         (insert "\t"))))
    ; at argument position, erase any tabs and re-tabify
    ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+$")
     (progn
       (strip-trailing-whitespace)
       (forward-word 1)
       (save-excursion
         (dotimes (i picasm-instruction-argument-indent-tabs)
     (insert "\t")))
       (end-of-line)))
    ; complete instruction/argument pair. re-indent and leave point at eol or comment if enabled.
    ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+[^ \t]+[ \t]*$")
     (progn
       (strip-trailing-whitespace)
       (end-of-line)
       (if picasm-require-comment
     (progn
       (dotimes (i picasm-instruction-comment-indent-tabs)
         (insert "\t"))
       (insert "; "))
         (progn
     (indent-line-to picasm-instruction-indent-spaces)
     (end-of-line)))))
    ;; complete instruction/argument pair with comment; leave point at eol
    ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+[^ \t]+[ \t]+;.*$")
     (end-of-line))
    ((looking-at (concat "^[ \t]*" picasm-mode-font-lock-syntheticop-keyword-re))
     (progn
       (indent-line-to picasm-instruction-indent-spaces)
       (end-of-line)))
    ((looking-at (concat "[ \t]*" picasm-mode-pp-directive-re))
     (progn
       (indent-line-to 0)
       (end-of-line)))
    (t (message "don't know how to indent this line")))))

(defun picasm-electric-comment ()
  "Insert a comment at EOL, move point to it.
If there is already a comment there, move point to it.  Otherwise, insert
a semicolon."
  (interactive)
  (let ((p (point)))
    (beginning-of-line)
    (cond ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+[^ \t]+[ \t]*$")
     (progn
       (strip-trailing-whitespace)
       (end-of-line)
       (dotimes (i picasm-instruction-comment-indent-tabs)
         (insert "\t"))
       (insert "; ")))
    ((looking-at "^[ \t]+[[:alpha:]]+[ \t]+[^ \t]+[ \t]+;.*$")
     (end-of-line))
    (t (progn
         (goto-char p)
         (insert ";"))))))

(defvar picasm-chip-select "PIC16F84A")

(defvar picasm-mode-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?\; "<" tab)
    (modify-syntax-entry ?\n ">" tab)
    tab))

(defvar picasm-mode-map (make-keymap))

(defcustom picasm-use-default-keybindings t
  "Whether to assume you want to use my keybindings (recommended!)."
  :type 'boolean :group 'picasm)

(defun picasm-setup-default-keybindings ()
  "Default keybinds."
  (define-key picasm-mode-map "\t" 'picasm-mode-indent-instruction-line)
  (define-key picasm-mode-map ";" 'picasm-electric-comment)
  (define-key picasm-mode-map "\C-c\C-c" 'assemble-file)
  (define-key picasm-mode-map "\C-c\C-p" 'picasm-pk2cmd-program)
  (define-key picasm-mode-map "\C-c\C-e" 'picasm-pk2cmd-erase)
  (define-key picasm-mode-map "\C-c\C-v" 'picasm-pk2cmd-verify)
  (define-key picasm-mode-map "\C-c\C-r" 'picasm-pk2cmd-read)
  (define-key picasm-mode-map [134217741] '(lambda ()
               (interactive)
               (end-of-line)
               (newline)
               (picasm-mode-indent-instruction-line))))

(defcustom picasm-mode-hook nil
  "Hook run when picasm-mode is initialized."
  :type 'hook :group 'picasm)

(defun read-pic-database ()
  (message "Reading the chip database...")
  (require 'xml)
  (let ((rawdb (car (xml-parse-file pic-database-file))))
    (dolist (entry (xml-get-children rawdb 'Chip))
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
  (when picasm-use-default-keybindings
    (picasm-setup-default-keybindings))
  (when (= (hash-table-size pic-database) 0)
    (read-pic-database)))

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


(defcustom picasm-show-assembler-output nil
  "Whether to display assembler output in a new window"
  :type 'boolean :group 'picasm)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.asm$" . picasm-mode))

(provide 'picasm)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; picasm.el ends here
