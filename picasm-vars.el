;;; picasm-vars.el ---                               -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;; Code:

(defcustom picasm-show-assembler-output nil
  "Whether to display assembler output in a new window."
  :type 'boolean :group 'picasm)

(defcustom picasm-picloops-program "~/.emacs.d/picasm/picloops"
  "Location of the picloops loop calculation program"
  :type 'string :group 'picasm-external)

(defcustom pic-database-file "~/.emacs.d/picasm/chips.xml"
  "Location of the PIC chip database (XML-format)."
  :type 'string :group 'picasm)

(defvar pic-database (make-hash-table :test 'equal))

(defvar picasm-chip-select "")

;; Syntax table
(defvar picasm-mode-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?\; "<" tab)
    (modify-syntax-entry ?\n ">" tab)
    tab))

;; key bind
(defvar picasm-mode-map
  (let* ((map (make-sparse-keymap)))
    (define-key map "\t" 'picasm-mode-indent-instruction-line)
    (define-key map ";" 'picasm-electric-comment)
    (define-key map "\C-c\C-c" 'assemble-file)
    (define-key map "\C-c\C-p" 'picasm-pk2cmd-program)
    (define-key map "\C-c\C-e" 'picasm-pk2cmd-erase)
    (define-key map "\C-c\C-v" 'picasm-pk2cmd-verify)
    (define-key map "\C-c\C-r" 'picasm-pk2cmd-read)
    (define-key map (kbd "M-RET")
      '(lambda ()
         (interactive)
         (end-of-line)
         (newline)
         (picasm-mode-indent-instruction-line)))
    map))

;; Indent
(defcustom picasm-instruction-indent-spaces 8
  "Number of spaces to indent instruction lines."
  :type 'integer :group 'picasm)

(defcustom picasm-condition-block-indent-spaces
  (+ picasm-instruction-indent-spaces 2)
  "Number of spaces to indent condition instruction lines."
  :type 'integer :group 'picasm)

(defcustom picasm-section-marker-indent-spaces 0
  "Number of spaces to indent section markers."
  :type 'integer :group 'picasm)

(defcustom picasm-instruction-argument-indent-tabs 2
  "Number of tabs to insert after instructions, before arguments."
  :type 'integer :group 'picasm)

(defcustom picasm-instruction-comment-indent-tabs 2
  "Number of tabs to indent comments."
  :type 'integer :group 'picasm)

;; from picasm-external.el
(defcustom picasm-assembler-program 'gpasm
  "Select either GPASM or MPASM to assemble source files"
  :options '(gpasm mpasm) :group 'picasm-external)

(defcustom picasm-gpasm-program "/usr/bin/gpasm"
  "Location of the gpasm executable"
  :type 'string :group 'picasm-external)

(defcustom picasm-gplink-program "/usr/bin/gplink"
  "Location of the gplink executable"
  :type 'string :group 'picasm-external)

(defcustom picasm-output-format "inhx32"
  "Output format for HEX files"
  :options '("inhx8m" "inhx8s" "inhx16" "inhx32") :group 'picasm-external)

(defcustom picasm-mpasm-wine-program "/usr/bin/wine"
  "Location of the WINE executable (for running MPASM, if enabled)"
  :type 'string :group 'picasm-external)

(defcustom picasm-mpasm-program "~/.wine/drive_c/Program\ Files/Microchip/MPASM Suite/MPASMWIN.exe"
  "Location of the MPASMWIN executable (run under WINE)"
  :type 'file :group 'picasm-external)

(defcustom picasm-includes (list ".")
  "List of include directories"
  :type '(list string) :group 'picasm-external)

(defcustom picasm-default-radix "HEX"
  "Default radix for assembling files"
  :options '("BIN" "DEC" "OCT" "HEX") :group 'picasm-external)

(defcustom picasm-pk2cmd-program "/usr/local/bin/pk2cmd"
  "Location of the pk2cmd executable"
  :type 'string :group 'picasm-external)


(provide 'picasm-vars)
;;; picasm-vars.el ends here
