;; TODO Make a minor mode?
;; TODO Write documentation M-x checkdoc
;; TODO Handle isearch properly
;; TODO (defvar elmacro-filter '(smex isearch))
;; TODO (defvar elmacro-make-last-command-event '(isearch-printing-char))
;; TODO M-x elmacro-set-logging 'always or 'on-macro
;; TODO extract macro with start-kbd-macro instead of kmacro-start-macro?
;; TODO (defvar elmacro-store-all-commands 'always)
;; TODO see the todo in defadvice call-interactively
;; TODO MELPA / Cask?

(require 's)
(require 'dash)

(setq history-length t)

(defadvice call-interactively (around elmacro-store-command last (func &optional record keys) activate)
  "Always save whatever is called interactively in `command-history' and process it."
  (setq record t)
  ad-do-it
  ;; TODO copy last command into `elmacro-commands' and process this instead.
  ;; The problem we currently have is commands like (smex) which
  ;; appear in command-history BUT NOT when copied... also some
  ;; commands don't appear the same... probably because of the magic
  ;; happening in ido-ubiquitous. Figure what happens there first.
  (elmacro-process-last-command 'command-history))

(defun elmacro-handle-self-insert-command (list-var)
  "Replace (self-insert-command 1) with (insert \"x\")."
  (setcar (symbol-value list-var) `(insert ,(string last-command-event))))

(defun elmacro-handle-isearch-printing-char (list-var)
  "FIXME"
  (set list-var (-insert-at
                 1
                 `(setq last-command-event ,last-command-event)
                 (symbol-value list-var))))

(defun elmacro-handle-isearch-other-control-char (list-var)
  "FIXME"
  (set list-var (-insert-at
                 1
                 `(setq last-command-event ,last-command-event)
                 (symbol-value list-var))))

(defun elmacro-process-last-command (list-var)
  "Process the latest command of LIST-VAR into something more suitable."
  (let* ((func (-first-item (-first-item (symbol-value list-var))))
         (handler (intern (format "elmacro-handle-%s" func))))
    (if (fboundp handler)
        (funcall handler list-var))))

(defun elmacro-extract-last-kbd-macro (lst)
  "Extracts the last keyboard macro from LST."
  (-drop 1 (--take-while (not (equal it '(kmacro-start-macro nil)))
                         (--drop-while (not (equal it '(kmacro-end-macro nil)))
                                       lst))))

(defun elmacro-get-window-object (number)
  ""
  (--first (s-match (format "#<window %s[^>]+>" number) (prin1-to-string it))
           (window-list)))

(defun elmacro-prin1-to-string (obj)
  "Redefinition of prin1-to-string in order to serialize buffers, windows, etc."
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
  (elmacro-show-defun name (reverse (elmacro-extract-last-kbd-macro command-history))))

(defun elmacro-edit-lossage ()
  "Show lossage as elisp."
  (interactive)
  (elmacro-show-defun "lossage" (reverse (-take 300 command-history))))
