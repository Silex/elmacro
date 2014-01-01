;; TODO Write documentation M-x checkdoc
;; TODO M-x elmacro-set-logging 'always or 'on-macro
;; TODO extract macro with start-kbd-macro instead of kmacro-start-macro?
;; TODO (defvar elmacro-store-all-commands 'always)
;; TODO MELPA / Cask?

(require 's)
(require 'dash)

(defvar elmacro-recorded-commands '())
(defvar elmacro-filters '(ido smex isearch))

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
  "Return form setting up `last-command-event'."
  (if (symbolp last-command-event)
      `(setq last-command-event ',last-command-event)
    `(setq last-command-event ,last-command-event)))

(defun elmacro-extract-last-kbd-macro (commands)
  "Extract the last keyboard macro from COMMANDS."
  (-drop 1 (--take-while (not (equal it '(kmacro-start-macro nil)))
                         (--drop-while (not (equal it '(kmacro-end-macro nil)))
                                       commands))))

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

(defun elmacro-show-last-macro (name)
  "Show the last macro as elisp."
  (interactive "sMacro name: ")
  (elmacro-show-defun name (reverse (elmacro-extract-last-kbd-macro elmacro-recorded-commands))))

(defun elmacro-show-lossage ()
  "Show lossage as elisp."
  (interactive)
  (elmacro-show-defun "lossage" (reverse (-take 300 elmacro-recorded-commands))))

(define-minor-mode elmacro-mode
  "Toggle elmacro mode."
  nil
  " elmacro"
  nil
  :global t
  :group 'elmacro
  (if elmacro-mode
      (progn
        (defadvice call-interactively (before elmacro-save-all-commands (func &optional record keys) activate)
          "Always save whatever is called interactively in `command-history'."
          (setq record t))
        (add-hook 'post-command-hook 'elmacro-process-latest-command))
    (progn
      (ad-remove-advice 'call-interactively 'before 'elmacro-save-all-commands)
      (remove-hook 'post-command-hook 'elmacro-process-latest-command))))

(provide 'elmacro-mode)
