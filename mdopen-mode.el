;;; mdopen-mode.el --- Instant Markdown preview using mdopen.   -*- lexical-binding: t; -*-

;; Author: jcook3701
;; Homepage: https://github.com/jcook3701/mdopen-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, markdown, preview

;; This file is not part of GNU Emacs.

;;; Commentary:

;; mdopen-mode is an Emacs minor mode that provides instant preview of Markdown
;; files using the mdopen command-line tool.

;; Install:
;; From melpa: `M-x package-install RET mdopen-mode RET`.
;; Add the following to your `init.el`:
;; ```emacs-lisp
;; (use-package mdopen-mode
;;   :hook ((markdown-mode . mdopen-mode)
;; 	 (after-save-hook . mdopen-refresh))
;;   :bind
;;   (:map markdown-mode-command-map
;; 	("C-c C-m" . mdopen-mode))
;;   :ensure t)
;; ```
;;
;; Install:
;; From: github:
;; Add the following to your `init.el`:
;; ```emacs-lisp
;; (use-package mdopen-mode
;;   :hook ((markdown-mode . mdopen-mode)
;; 	 (after-save-hook . mdopen-refresh))
;;   :bind
;;   (:map markdown-mode-command-map
;; 	("C-c C-m" . mdopen-mode))
;;   :ensure (:fetcher github :repo "jcook3701/mdopen-mode"))
;;
;; Run `M-x mdopen-mode` to preview a Markdown file with your defined preferences.

;;; Code:

(defgroup mdopen nil
  "Instant Markdown preview using mdopen."
  :group 'markdown
  :link '(url-link :tag "Homepage" "https://github.com/jcook3701/mdopen-mode"))

(defcustom mdopen-binary-path "mdopen"
  "Path to the mdopen executable."
  :type 'string
  :group 'mdopen)

(defcustom mdopen-server-host "http://127.0.0.1:5032"
  "Base URL for the mdopen server."
  :type 'string
  :group 'mdopen)

(defvar mdopen--process nil
  "Handle to the mdopen process.")

;;; Helper Functions

(defun mdopen--kill-process ()
  "Kill the mdopen process linked to the current buffer."
  (when (and mdopen--process (process-live-p mdopen--process))
    (delete-process mdopen--process)
    (message "Killed mdopen process for buffer: %s" (buffer-name))
    (setq mdopen--process nil)))

(defun mdopen--start-process ()
  "Start the mdopen process for the current buffer's file."
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (mdopen--kill-process) ;; Ensure no duplicate processes
  (setq mdopen--process
        (start-process "mdopen-process" "*mdopen-output*"
                       mdopen-binary-path buffer-file-name))
  (message "Started mdopen process for: %s" (buffer-file-name)))

(defun mdopen-process-running-p ()
  "Determine if the `mdopen` process is currently running."
  (and mdopen--process (process-live-p mdopen--process)))

(defun mdopen-refresh ()
  "Refresh the Markdown preview via mdopen without opening a new tab."
  (interactive)
  (let* ((file-relative-path (file-relative-name buffer-file-name))
         (preview-url (format "%s/%s" mdopen-server-host file-relative-path)))
    (if (mdopen-process-running-p)
        (progn
          ;; Notify user and refresh (reuse the same tab in the browser)
          (message "Refreshing existing mdopen preview for: %s" buffer-file-name)
          ;; Use `browse-url` and rely on the browser's tab reuse feature
          (browse-url preview-url))
      ;; If the process is not running, restart it
      (message "mdopen process is not running; restarting...")
      (mdopen--start-process)
      (message "Opening preview in browser for: %s" preview-url)
      (browse-url preview-url))))

;;; Interactive Functions

(defun mdopen-start-preview ()
  "Start the Markdown preview process via mdopen."
  (interactive)
  (when buffer-file-name
    (add-hook 'after-save-hook #'mdopen-refresh nil t)
    (add-hook 'kill-buffer-hook #'mdopen--kill-process nil t)
    (mdopen--start-process)
    (let* ((file-relative-path (file-relative-name buffer-file-name))
           (preview-url (format "%s/%s" mdopen-server-host file-relative-path)))
      (message "Opening mdopen preview at: %s" preview-url)
      (browse-url preview-url))))

(defun mdopen-stop-preview ()
  "Stop the Markdown preview process and clear hooks."
  (interactive)
  (remove-hook 'after-save-hook #'mdopen-refresh t)
  (remove-hook 'kill-buffer-hook #'mdopen--kill-process t)
  (mdopen--kill-process)
  (message "Stopped mdopen preview for buffer: %s" (buffer-name)))

;;;###autoload
(define-minor-mode mdopen-mode
  "Minor mode for previewing Markdown files via mdopen."
  :lighter " mdopen"
  (if mdopen-mode
      (mdopen-start-preview)
    (mdopen-stop-preview)))

(provide 'mdopen-mode)

;;; mdopen-mode.el ends here
