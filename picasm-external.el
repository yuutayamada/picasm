;; Interfaces to external programs and output parsers.






(defun assemble-file ()
  (interactive)
  (let* ((file (buffer-file-name (current-buffer)))
   (chip picasm-chip-select)
   (output-file (concat (file-name-sans-extension file) ".o")))
    (if picasm-show-assembler-output
	(display-buffer (get-buffer-create "*Assembler Output*")))
    (if (not (zerop (run-assembler file chip)))
	(message (format "Assembly of %s failed" file))
      (if (not (zerop (picasm-link output-file)))
    (message (format "%s: Linker errors" file)))
	(message (format "Assemble %s: Success" file)))))

(defun run-assembler (file chip)
  (cl-case picasm-assembler-program
    (gpasm (run-gpasm file chip))
    (mpasm (run-mpasm file chip))))

(defun run-gpasm (file chip)
  "Run the GNU gputils gpasm assembler on FILE for CHIP."
  (let ((flags (append (list (mapconcat #'(lambda (dir) (concat "-I " dir)) picasm-includes " "))
                       (list "-p" chip)
                       (list "-r" picasm-default-radix)
                       (list "-c")
                       (list file))))
    (picasm-asm picasm-gpasm-program flags)))

(defun picasm-asm (program flags)
  ""
  (shell-command (concat program " " (mapconcat #'(lambda (x) x) flags " "))
                 (and picasm-show-assembler-output "*Assembler Output*")))

(defun run-mpasm (file chip)
  "Run the Microchip MPASM assembler on FILE for CHIP. MPASM for Linux (via WINE) can be downloaded as part of MPLAB-X. See README.MPASM."
  (let ((flags (append (list (concat "/p" chip))   ;; no spaces between flag and arg
           (list (concat "/r" picasm-default-radix))
           (list (concat "/a" (upcase picasm-output-format)))
           (list "/q")
           (list (replace-regexp-in-string "/" "\\\\\\\\" file)))))   ; win{e,doze} confused by '/'
    (picasm-asm picasm-mpasm-program flags)))

(defun picasm-link (file)
  "Run the final link stage to generate the HEX file from an object file"
  (let ((flags (append (list "-o" (concat (file-name-sans-extension file) ".hex"))
           (list "-a" picasm-output-format)
           (list file))))
    (shell-command (concat picasm-gplink-program " " (mapconcat #'(lambda (x) x) flags " ")))))

;; Interface to the pk2cmd command-line PIC programmer

(defun picasm-pk2cmd-erase ()
  (interactive)
  (compare-chips)
  (let ((output (run-pk2cmd (append (list "-P" picasm-chip-select)
            (list "-E")))))
    (if (stringp (string-match "Succeeded" output))
	(message "chip erased"))))

(defun picasm-pk2cmd-read ()
  ; Read the entire PIC, regardless of how much program mem it has
  (interactive)
  (compare-chips)
  (if (not (string-equal (buffer-name (current-buffer)) "*Chip Contents*"))
      (progn
	(split-window-vertically)
	(other-window 1)))
  (if (get-buffer "*Chip Contents*")
	(switch-to-buffer "*Chip Contents*")
    (switch-to-buffer (get-buffer-create "*Chip Contents*")))
  (erase-buffer)
  (let ((output
   (run-pk2cmd (append (list "-P" picasm-chip-select)
           (list "-GP" "0-FFFFFFF")))))
    (cl-dolist (line (split-string output "\n"))
      (if (string-match "^[[:digit:]A-Fa-f]" line)
    (insert (concat line "\n"))))))

(defun picasm-pk2cmd-verify (file)
  (interactive "fVerify file (HEX format): ")
  (compare-chips)
  (let ((output (run-pk2cmd (append (list "-P" picasm-chip-select)
            (list (concat "-F" (expand-file-name file)))   ; space causes problems (??)
            (list "-Y")))))
    (if (string-match "Verify Succeeded." output)
	(message "Chip verify succeeded")
      (message "Chip verify failed"))))

(defun picasm-pk2cmd-program (file)
  (interactive "fProgram file (HEX format): ")
  (compare-chips)
  (let ((output (run-pk2cmd (append (list "-P" picasm-chip-select)
            (list (concat "-F" (expand-file-name file)))
            (list "-MP")))))
    (if (string-match "Program Succeeded." output)
	(message "Chip progamming succeeded")
      (message "Chip programming failed"))))

(defun run-pk2cmd (args)
  (shell-command-to-string (mapconcat #'(lambda (x) x) `(,picasm-pk2cmd-program ,@args) " ")))

(defun get-programmer-chip ()
  (let ((output (run-pk2cmd (append (list "-P" picasm-chip-select)
            (list "-I")))))
    (string-match "Device Name = \\([^ \t\n]+\\)" output)
    (match-string-no-properties 1 output)))

(defun compare-chips ()
  (let ((phy-dev (get-programmer-chip)))
    (if (not (string-equal phy-dev picasm-chip-select))
	(error (format "Chip selected does not match programmer (Have %s, want %s)." phy-dev picasm-chip-select))
      t)))


(provide 'picasm-external)
