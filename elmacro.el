;; TODO Make a minor mode?
;; TODO Write documentation M-x checkdoc
;; TODO Handle isearch properly
;; TODO (defvar elmacro-filter '(smex isearch))
;; TODO (defvar elmacro-make-last-command-event '(isearch-printing-char))
;; TODO M-x elmacro-set-logging 'always or 'on-macro
;; TODO extract macro with start-kbd-macro instead of kmacro-start-macro?
;; TODO (defvar elmacro-store-all-commands 'always)
;; TODO MELPA / Cask?

(require 's)
(require 'dash)

(defvar elmacro-recorded-commands '())
(defvar elmacro-commands-with-input '(isearch-printing-char isearch-other-control-char))

(defun elmacro-process-latest-command ()
  "Process the latest command of variable `command-history' into `elmacro-recorded-commands'."
  (--each (elmacro-transform-command (car command-history))
    (!cons it elmacro-recorded-commands)))

(defun elmacro-last-command-event ()
  "Return form setting up `last-command-event'."
  (if (symbolp last-command-event)
      `(setq last-command-event ',last-command-event)
    `(setq last-command-event ,last-command-event)))

(defun elmacro-transform-command (cmd)
  "Process CMD into something more suitable if needed."
  (let* ((func (car cmd))
         (commands-with-input '(isearch-printing-char isearch-other-control-char)))
    (cond ((equal func 'self-insert-command)
           (unless (minibufferp)
             `((insert ,(string last-command-event)))))
          ((-contains? commands-with-input func)
           `(,(elmacro-last-command-event)
             ,cmd))
          ((eq t t)
           `(,cmd)))))

(defun elmacro-extract-last-kbd-macro (commands)
  "Extract the last keyboard macro from COMMANDS."
  (-drop 1 (--take-while (not (equal it '(kmacro-start-macro nil)))
                         (--drop-while (not (equal it '(kmacro-end-macro nil)))
                                       commands))))

(defun elmacro-get-window-object (number)
  "Return the window object numbered NUMBER."
  (--first (s-match (format "#<window %s[^>]+>" number) (prin1-to-string it))
           (window-list)))

(defun elmacro-prin1-to-string (obj)
  "Print OBJ like `prin1-to-string' but handle windows, buffers, etc."
  (let* ((str (prin1-to-string obj))
         (str (replace-regexp-in-string "#<window \\([0-9]+\\)[^>]+>" "(elmacro-get-window-object \\1)" str))
         (str (replace-regexp-in-string "(quote" "(backquote" str)))
    str))

(defun elmacro-show-defun (name commands)
  (let ((buffer (get-buffer-create (format "* elmacro - %s.el *" name))))
    (set-buffer buffer)
    (erase-buffer)
    (insert (format "(defun %s ()\n" name))
    (insert "\"Change me!\"\n")
    (insert "(interactive)\n")
    (insert (mapconcat 'elmacro-prin1-to-string commands "\n"))
    (insert ")\n")
    (emacs-lisp-mode)
    (indent-region (point-min) (point-max))
    (pop-to-buffer buffer)
    (goto-line 1)))

(defun elmacro-show-last-macro (name)
  "Show the last macro as elisp."
  (interactive "sMacro name: ")
  (elmacro-show-defun name (reverse (elmacro-extract-last-kbd-macro elmacro-recorded-commands))))

(defun elmacro-edit-lossage ()
  "Show lossage as elisp."
  (interactive)
  (elmacro-show-defun "lossage" (reverse (-take 300 elmacro-recorded-commands))))

(defadvice call-interactively (before elmacro-record-command (func &optional record keys) activate)
  "Always save whatever is called interactively in `command-history'."
  (setq record t))

(add-hook 'post-command-hook 'elmacro-process-latest-command)
