(require 'slime)
(require 'cl-lib)
(eval-when-compile (require 'epg))

;;; bits of the following taken from slime-asdf.el

(define-slime-contrib slime-quicklisp
  "Quicklisp support."
  (:authors "Matthew Kennedy <burnsidemk@gmail.com>")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-quicklisp))

;;; Utilities

(defgroup slime-quicklisp nil
  "Quicklisp support for Slime."
  :prefix "slime-quicklisp-"
  :group 'slime)

(defvar slime-quicklisp-system-history nil
  "History list for Quicklisp system names.")

(defun slime-read-quicklisp-system-name (&optional prompt default-value)
  "Read a Quick system name from the minibuffer, prompting with PROMPT."
  (let* ((completion-ignore-case nil)
         (prompt (or prompt "Quicklisp system"))
         (quicklisp-system-names (slime-eval `(swank:list-quicklisp-systems)))
         (prompt (concat prompt (if default-value
                                    (format " (default `%s'): " default-value)
                                  ": "))))
    (completing-read prompt (slime-bogus-completion-alist quicklisp-system-names)
                     nil nil nil
                     'slime-quicklisp-system-history default-value)))

(defun slime-quicklisp-quickload (system)
  "Load a Quicklisp system."
  (slime-save-some-lisp-buffers)
  (slime-display-output-buffer)
  (slime-repl-shortcut-eval-async `(ql:quickload ,system)))

;;; Quickstart Helper

(defvar slime-quicklisp-quickstart-url
  "https://beta.quicklisp.org/quicklisp.lisp")

(defvar slime-quicklisp-quickstart-signature-url
  "https://beta.quicklisp.org/quicklisp.lisp.asc")

(defun slime-quicklisp--get (url-string)
  (message "Downloading %s..." url-string)
  (slime-eval `(cl:format cl:t "~&Downloading ~a...~%" ,url-string))
  (let* ((url (url-generic-parse-url url-string))
         (buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (delete-region (point-min)
                     (1+ (marker-position url-http-end-of-headers)))
      ;; Ensure LF line endings for SBCL's and GnuPG's sanity.
      (set-buffer-file-coding-system 'unix)
      (let* ((name (file-name-nondirectory (url-filename url)))
             (file (make-temp-file (concat (file-name-sans-extension name) "-")
                                   nil
                                   (concat "." (file-name-extension name)))))
        (let ((inhibit-message t))
          (write-file file))
        (kill-buffer)
        file))))

(defun slime-quicklisp--verify (file signature-file)
  (let* ((context (epg-make-context)))
    (epg-verify-file context signature-file file)
    ;; assuming there's only one signature.
    (let ((sig (first (epg-context-result-for context 'verify))))
      (check-type sig epg-signature)
      (slime-eval `(cl:format cl:t "~&~a.~%" ,(epg-signature-to-string sig)))
      (or (eq (epg-signature-status sig) 'good)
          (yes-or-no-p (concat (epg-signature-to-string sig)
                               ".\n\nFailed to verify Quicklisp signature. \
                                Continue anyway? "))))))

(defun slime-quicklisp--suggest-add-to-init-file (result)
  (declare (ignore result))
  (when (yes-or-no-p "Add Quicklisp to init file? ")
    (slime-eval-async `(ql-util:without-prompting
                         (ql:add-to-init-file)
                         (cl:values)))))

(defun slime-quicklisp--suggest-install (success)
  (when (and success
             (yes-or-no-p "Proceed with quicklisp-quickstart:install? "))
    (slime-eval-async '(quicklisp-quickstart:install)
                      #'slime-quicklisp--suggest-add-to-init-file)))

(defun slime-quicklisp-install ()
  "Install the Quicklisp client."
  (interactive)
  (require 'epg)
  (slime-display-output-buffer)
  (slime-eval `(cl:progn (cl:terpri) (cl:terpri)))
  (let ((quickstart (slime-quicklisp--get slime-quicklisp-quickstart-url))
        (signature (slime-quicklisp--get slime-quicklisp-quickstart-signature-url)))
    (when (slime-quicklisp--verify quickstart signature)
      (slime-eval-async `(cl:load ,quickstart)
                        #'slime-quicklisp--suggest-install))))

;;; REPL shortcuts

(defslime-repl-shortcut slime-repl-quicklisp-quickload ("quicklisp-quickload" "ql")
  (:handler (lambda ()
              (interactive)
              (slime-quicklisp-quickload (slime-read-quicklisp-system-name))))
  (:one-liner "Load a system known to Quicklisp."))

(defslime-repl-shortcut slime-repl-quicklisp-install ("quicklisp-install")
  (:handler 'slime-quicklisp-install)
  (:one-liner "Install Quicklisp."))

(provide 'slime-quicklisp)
