;;; elmacro.el --- convert macro to elisp

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/elmacro
;; Keywords: macro, elisp, convenience
;; Version: 0.0.1
;; Package-Requires: ((s "1.9.0") (dash "1.5.0"))

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
      (if (not (minibufferp))
          `((insert ,(string last-command-event)))))

     ;; Filter ido
     ((and (s-starts-with? "ido" (symbol-name func)) (-contains? elmacro-filters 'ido))
      '())

     ;; Filter smex
     ((and (s-starts-with? "smex" (symbol-name func)) (-contains? elmacro-filters 'smex))
      '())

     ;; Filter isearch
     ((and (s-starts-with? "isearch" (symbol-name func)) (-contains? elmacro-filters 'isearch))
      (case func
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

(defun elmacro-get-window-object (number)
  "Return the window object numbered NUMBER."
  (--first (s-match (format "#<window %s[^>]+>" number) (prin1-to-string it))
           (window-list)))

(defun elmacro-object-to-string (obj)
  "Print OBJ like `prin1-to-string' but handle windows, buffers, etc."
  (let* ((print-quoted t)
         (str (prin1-to-string obj)))

    ;; Handle #<window> objects
    (when (s-contains? "#<window" str)
      (setq str (replace-regexp-in-string "#<window \\([0-9]+\\)[^>]+>" ",(elmacro-get-window-object \\1)" str))
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
    (goto-line 1)))

(defun elmacro-on ()
  "Turn elmacro mode on."
  (defadvice call-interactively (before elmacro-save-all-commands (func &optional record keys) activate)
    "Always save whatever is called interactively in `command-history'."
    (setq record t))
  (add-hook 'post-command-hook 'elmacro-process-latest-command))

(defun elmacro-off ()
  "Turn elmacro mode off."
  (ad-remove-advice 'call-interactively 'before 'elmacro-save-all-commands)
  (remove-hook 'post-command-hook 'elmacro-process-latest-command))

;;;###autoload
(defun elmacro-show-last-macro (name)
  "Show the last macro as elisp with NAME."
  (interactive "sMacro name: ")
  (elmacro-show-defun name (reverse (elmacro-extract-last-kbd-macro elmacro-recorded-commands))))

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
      (elmacro-on)
    (elmacro-off)))

(provide 'elmacro)

;;; elmacro.el ends here
