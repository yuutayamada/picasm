;; Interface to "picloops" external program -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'picasm-vars)

(defun picloops-loop-1 (label counter-a)
  "Initializes and outputs a single-stage loop"
  (cl-macrolet ((i-f (str &rest args)
                     `(insert (format ,str ,@args))))
    (i-f "\ndelay_%s:\n" label)
    (i-f "\tMOVLW\tD'%d'\n" counter-a)
    (i-f "\tMOVWF\tCounterA\n")
    (i-f "delay_%s_loop:\n" label)
    (i-f "\tDECFSZ\tCounterA,f\n")
    (i-f "\tGOTO\tdelay_%s_loop\n" label)
    (i-f "\tRETURN\n")))

(defun picloops-loop-2 (label counter-a counter-b)
  "Initializes and outputs a two-stage loop"
  (cl-macrolet ((i-f (str &rest args)
                     `(insert (format ,str ,@args))))
    (i-f "\ndelay_%s:\n" label)
    (i-f "\tMOVLW\tD'%d'\n" counter-b)
    (i-f "\tMOVWF\tCounterB\n")
    (i-f "\tMOVLW\tD'%d'\n" counter-a)
    (i-f "\tMOVWF\tCounterA\n")
    (i-f "delay_%s_loop:\n" label)
    (i-f "\tDECFSZ\tCounterA,f\n")
    (i-f "\tGOTO\tdelay_%s_loop\n" label)
    (i-f "\tDECFSZ\tCounterB,f\n")
    (i-f "\tGOTO\tdelay_%s_loop\n" label)
    (i-f "\tRETURN\n")))

(defun picloops-loop-3 (label counter-a counter-b counter-c)
  "Initializes and outputs a three-stage loop"
  (cl-macrolet ((i-f (str &rest args)
                     `(insert (format ,str ,@args))))
    (i-f "\ndelay_%s:\n" label)
    (i-f "\tMOVLW\tD'%d'\n" counter-c)
    (i-f "\tMOVWF\tCounterC\n")
    (i-f "delay_%s_loop:\n" label)
    (i-f "\tCALL\tsub_delay_%s\n" label)
    (i-f "\tDECFSZ\tCounterC,f\n")
    (i-f "\tGOTO\tdelay_%s_loop\n" label)
    (i-f "\tRETURN\n\n")
    (i-f "sub_delay_%s:\n" label)
    (i-f "\tMOVLW\tD'%d'\n" counter-b)
    (i-f "\tMOVWF\tCounterB\n")
    (i-f "\tMOVLW\tD'%d'\n" counter-a)
    (i-f "\tMOVWF\tCounterA\n")
    (i-f "sub_delay_%s_loop:\n" label)
    (i-f "\tDECFSZ\tCounterA,f\n")
    (i-f "\tGOTO\tsub_delay_%s_loop\n" label)
    (i-f "\tDECFSZ\tCounterB,f\n")
    (i-f "\tGOTO\tsub_delay_%s_loop\n" label)
    (i-f "\tRETURN\n")))

(defun picloops-calc (label seconds clock-mhz)
  (interactive "Mlabel: \nnSeconds: \nnClock (MHz): ")
  (let ((counters (picasm-run-picloops seconds clock-mhz)))
    (cond ((= (cadr counters) -1)
           (picloops-loop-1 label (car counters)))
          ((= (cl-caddr counters) -1)
           (picloops-loop-2 label (car counters) (cadr counters)))
          (t
           (picloops-loop-3 label (car counters) (cadr counters) (cl-caddr counters))))))

;; Loop calculation uses an external C program because it's easier
;; (read: possible) to do floating point in C.
(defun picasm-insert-delay (label seconds clock-mhz)
  "Insert a routine at point, using LABEL as a name component, that will cause a delay of SECONDS length assuming a clock running at CLOCK-MHZ."
  (interactive "Mlabel: \nnSeconds: \nnClock (MHz): ")
  (let ((counters (picasm-run-picloops seconds clock-mhz)))
    (cond ((= (cadr counters) -1)
           (picloops-loop-1 label (car counters)))
          ((= (cl-caddr counters) -1)
           (picloops-loop-2 label (car counters) (cadr counters)))
          (t
           (picloops-loop-3 label (car counters) (cadr counters) (cl-caddr counters))))))

(defun picasm-run-picloops (seconds clock-mhz)
  "Return value is a list of counter values, from counterA to counterC"
  (mapcar 'string-to-number (split-string (shell-command-to-string (format "%s %f %f" picasm-picloops-program  seconds clock-mhz)))))

(provide 'picasm-loops)
