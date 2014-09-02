;;; elmacro.el --- Convert keyboard macros to elisp

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/elmacro
;; Keywords: macro, elisp, convenience
;; Version: 0.1.0
;; Package-Requires: ((s "1.9.0") (dash "1.5.0") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

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

;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)

(defgroup elmacro nil
  "Show macros as emacs lisp."
  :group 'keyboard
  :group 'convenience)

(defvar elmacro-recorded-commands '()
  "Where elmacro process commands from variable `command-history'.")

(defcustom elmacro-filters '(ido smex isearch)
  "List of symbols representing packages to filter."
  :group 'elmacro
  :type '(repeat symbol))

(defcustom elmacro-custom-recorded-functions
  '(copy-file
    copy-directory
    rename-file
    delete-file
    make-directory)
  "List of additional custom functions to record."
  :group 'elmacro
  :type '(repeat symbol))

(defcustom elmacro-objects-to-convert '(frame window buffer)
  "List of symbols representing which object to convert.

For example, converts <#window 42> to (elmacro-get-window-object 42)."
  :group 'elmacro
  :type '(repeat symbol))

(defcustom elmacro-concatenate-multiple-inserts t
  "Wether to concatenate multiple `insert' or not."
  :group 'elmacro
  :type 'boolean)

(defun elmacro-process-latest-command ()
  "Process the latest command of variable `command-history' into `elmacro-recorded-commands'."
  (--each (elmacro-transform-command (car command-history))
    (!cons it elmacro-recorded-commands)))

(defun elmacro-transform-command (cmd)
  "Transform CMD into a list of one or more modified commands if needed."
  (let* ((func (car cmd)))
    (cond
     ;; Transform self-insert-command (if not in minibuffer)
     ((equal func 'self-insert-command)
      (unless (minibufferp)
        (let ((previous-command (car elmacro-recorded-commands))
              (character (string last-command-event)))
          ;; TODO maybe do this as post-processing instead, that way we
          ;; can also detect backspaces and delete accordingly
          (if (or (not elmacro-concatenate-multiple-inserts)
                  (not (equal 'insert (car previous-command))))
              `((insert ,character))
            (setcdr previous-command
                    (list (concat (cadr previous-command) character)))
            nil))))

     ;; Filter ido
     ((and (s-starts-with? "ido" (symbol-name func)) (-contains? elmacro-filters 'ido))
      '())

     ;; Filter smex
     ((and (s-starts-with? "smex" (symbol-name func)) (-contains? elmacro-filters 'smex))
      '())

     ;; Filter isearch
     ((and (s-starts-with? "isearch" (symbol-name func)) (-contains? elmacro-filters 'isearch))
      (cl-case func
        ;; isearch-printing-char needs last-command-event
        (isearch-printing-char
         (list (elmacro-last-command-event) cmd))
        ;; isearch-other-control-char should be ignored, it only triggers other isearch commands
        (isearch-other-control-char
         '())
        (otherwise
         (list cmd))))

     ;; Default
     (t
      (list cmd)))))

(defun elmacro-last-command-event ()
  "Return an expression setting up `last-command-event'."
  (if (symbolp last-command-event)
      `(setq last-command-event ',last-command-event)
    `(setq last-command-event ,last-command-event)))

(defun elmacro-extract-last-kbd-macro (commands)
  "Extract the last keyboard macro from COMMANDS."
  (let ((starters '(start-kbd-macro kmacro-start-macro kmacro-start-macro-or-insert-counter))
        (finishers '(end-kbd-macro kmacro-end-macro kmacro-end-or-call-macro kmacro-end-and-call-macro)))
    (-drop 1 (--take-while (not (-contains? starters (car it)))
                           (--drop-while (not (-contains? finishers (car it))) commands)))))

(defun elmacro-get-frame-object (name)
  "Return the frame object named NAME."
  (--first (s-match (format "#<frame .* %s>" name) (prin1-to-string it))
           (frame-list)))

(defun elmacro-get-window-object (number)
  "Return the window object numbered NUMBER."
  (--first (s-match (format "#<window %d[^>]+>" number) (prin1-to-string it))
           (window-list)))

(defun elmacro-object-to-string (obj)
  "Print OBJ like `prin1-to-string' but handle windows, buffers, etc."
  (let* ((print-quoted t)
         (str (prin1-to-string obj)))

    ;; Handle #<frame> objects
    (when (and (-contains? elmacro-objects-to-convert 'frame) (s-contains? "#<frame" str))
      (setq str (replace-regexp-in-string "#<frame [^0]+\\(0x[0-9a-f]+\\)>" ",(elmacro-get-frame-object \"\\1\")" str))
      (setq str (replace-regexp-in-string "'(" "`(" str)))

    ;; Handle #<window> objects
    (when (and (-contains? elmacro-objects-to-convert 'window) (s-contains? "#<window" str))
      (setq str (replace-regexp-in-string "#<window \\([0-9]+\\)[^>]+>" ",(elmacro-get-window-object \\1)" str))
      (setq str (replace-regexp-in-string "'(" "`(" str)))

    ;; Handle #<buffer> objects
    (when (and (-contains? elmacro-objects-to-convert 'buffer) (s-contains? "#<buffer" str))
      (setq str (replace-regexp-in-string "#<buffer \\([^>]+\\)>" ",(get-buffer \"\\1\")" str))
      (setq str (replace-regexp-in-string "'(" "`(" str)))

    ;; Prettify last-command-event
    (if (string-match "(setq last-command-event \\([0-9]+\\))" str)
        (replace-match (format "?%s" (string (string-to-number (match-string 1 str)))) t t str 1)
      str)))

(defun elmacro-show-defun (name commands)
  "Create a buffer NAME containing a defun from COMMANDS."
  (let ((buffer (get-buffer-create (format "* elmacro - %s.el *" name))))
    (set-buffer buffer)
    (erase-buffer)
    (insert (format "(defun %s ()\n" name))
    (insert "\"Change me!\"\n")
    (insert "(interactive)\n")
    (insert (mapconcat 'elmacro-object-to-string commands "\n"))
    (insert ")\n")
    (emacs-lisp-mode)
    (indent-region (point-min) (point-max))
    (pop-to-buffer buffer)
    (goto-char (point-min))))

(defun elmacro-make-advice-lambda (function)
  `(lambda ()
     (!cons ,(list '\` (list function ',@(ad-get-args 0))) elmacro-recorded-commands)))

(defun elmacro-mode-on ()
  "Turn elmacro mode on."
  (defadvice call-interactively (before elmacro-save-all-commands (func &optional record keys) activate)
    "Always save whatever is called interactively in the variable `command-history'."
    (setq record t))
  (--each elmacro-custom-recorded-functions
    (ad-add-advice it
                   `(elmacro-record-command nil t (advice . ,(elmacro-make-advice-lambda it)))
                   'before
                   0)
    (ad-activate it))
  (add-hook 'post-command-hook 'elmacro-process-latest-command))

(defun elmacro-mode-off ()
  "Turn elmacro mode off."
  (ad-remove-advice 'call-interactively 'before 'elmacro-save-all-commands)
  (--each elmacro-custom-recorded-functions
    (ad-remove-advice it 'before 'elmacro-record-command))
  (remove-hook 'post-command-hook 'elmacro-process-latest-command))

;;;###autoload
(defun elmacro-show-last-macro (name)
  "Show the last macro as elisp with NAME."
  (interactive "sMacro name: ")
  (let ((macro-commands (reverse (elmacro-extract-last-kbd-macro elmacro-recorded-commands))))
    (if macro-commands
        (elmacro-show-defun name macro-commands)
      (message "You have to record a macro before using this command (F3/f4)."))))

;;;###autoload
(defun elmacro-show-lossage ()
  "Show lossage as elisp."
  (interactive)
  (elmacro-show-defun "lossage" (reverse (-take 300 elmacro-recorded-commands))))

;;;###autoload
(define-minor-mode elmacro-mode
  "Toggle elmacro mode."
  nil
  " elmacro"
  nil
  :global t
  :group 'elmacro
  (if elmacro-mode
      (elmacro-mode-on)
    (elmacro-mode-off)))

(provide 'elmacro)

;;; elmacro.el ends here
