;;; elmacro.el --- Convert keyboard macros to elisp

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/elmacro
;; Keywords: macro, elisp, convenience
;; Version: 0.2.0
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

(defcustom elmacro-unwanted-commands-regexp "^\\(ido\\|smex\\)"
  "Regexp used to filter unwanted commands."
  :group 'elmacro
  :type 'regexp)

(defcustom elmacro-additional-recorded-functions '(copy-file
                                                   copy-directory
                                                   rename-file
                                                   delete-file
                                                   make-directory)
  "List of additional functions to record."
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
  (--each (elmacro-preprocess-command (car command-history))
    (!cons it elmacro-recorded-commands)))

(defun elmacro-preprocess-self-insert-command ()
  "Transorm `self-insert-command' into the appropriate form."
  (unless (minibufferp)
    (let ((previous-command (car elmacro-recorded-commands))
          (character (string last-command-event)))
      ;; TODO maybe do this as post-processing instead, that way we
      ;; can also detect backspaces and delete accordingly
      (if (or (not elmacro-concatenate-multiple-inserts)
              (not (equal 'insert (car previous-command))))
          `((insert ,character))
        (setcdr previous-command (list (concat (cadr previous-command) character)))
        nil))))

(defun elmacro-preprocess-command (form)
  "Transform FORM into a list of one or more modified forms if needed."
  (let* ((command-symbol (car form))
         (command-string (symbol-name command-symbol)))
    (cond
     ;; Transform self-insert-command
     ((equal command-symbol 'self-insert-command)
      (elmacro-preprocess-self-insert-command))

     ;; Filter unwanted commands
     ((s-matches? elmacro-unwanted-commands-regexp command-string)
      nil)

     ;; Default
     (t
      (list form)))))

(defun elmacro-last-command-event ()
  "Return a form setting up `last-command-event'."
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

(defun elmacro-quoted-arguments (args)
  "Helper to correctly quote functions arguments of `elmacro-additional-recorded-functions'."
  (--map-when (and (symbolp it)
                   (not (keywordp it))
                   (not (eq nil it))
                   (not (eq t it)))
              `(quote ,it) args))

(defun elmacro-make-advice-lambda (function)
  "Generate the `defadvice' lambda used to record FUNCTION.

See the variable `elmacro-additional-recorded-functions'."
  `(lambda ()
     (!cons ,(list '\` (list function ',@(elmacro-quoted-arguments (ad-get-args 0))))
            elmacro-recorded-commands)))

(defun elmacro-mode-on ()
  "Turn elmacro mode on."
  (defadvice call-interactively (before elmacro-save-all-commands (func &optional record keys) activate)
    "Always save whatever is called interactively in the variable `command-history'."
    (setq record t))
  (--each elmacro-additional-recorded-functions
    (ad-add-advice it
                   `(elmacro-record-command nil t (advice . ,(elmacro-make-advice-lambda it)))
                   'before
                   0)
    (ad-activate it))
  (add-hook 'post-command-hook 'elmacro-process-latest-command))

(defun elmacro-mode-off ()
  "Turn elmacro mode off."
  (ad-remove-advice 'call-interactively 'before 'elmacro-save-all-commands)
  (--each elmacro-additional-recorded-functions
    (ad-remove-advice it 'before 'elmacro-record-command))
  (remove-hook 'post-command-hook 'elmacro-process-latest-command))

;;;###autoload
(defun elmacro-show-last-macro (name)
  "Show the last macro as elisp with NAME."
  (interactive "sMacro name: ")
  (let ((macro-commands (reverse (elmacro-extract-last-kbd-macro elmacro-recorded-commands))))
    (if macro-commands
        (elmacro-show-defun name macro-commands)
      (message "You have to record a macro before using this command (F3/F4)."))))

;;;###autoload
(defun elmacro-show-last-commands (&optional count)
  "Take the latest COUNT commands and show them as elisp.

The default number of commands shown is 300. You can change this
number by using a numeric prefix argument or by using the
universal argument, in which case it'll ask for how many in the
minibuffer. See also `kmacro-edit-lossage'."
  (interactive
   (list
    (cond
     ((equal current-prefix-arg nil)
      300)
     ((equal current-prefix-arg '(4))
      (read-number "How many commands?" 300))
     (t
      (prefix-numeric-value current-prefix-arg)))))
  (elmacro-show-defun "last-commands" (reverse (-take count elmacro-recorded-commands))))

;;;###autoload
(defun elmacro-clear-recorded-commands ()
  "Clear the list of recorded commands."
  (interactive)
  (setq elmacro-recorded-commands '()))

;;;###autoload
(define-minor-mode elmacro-mode
  "Toggle emacs activity recording (elmacro mode).
With a prefix argument ARG, enable elmacro mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil."
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
