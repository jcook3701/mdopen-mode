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
  "Handle to the mdopen process for the current buffer.")

(defvar mdopen--last-url nil
  "Stores the URL of the current preview.")

;;; Helper Functions

(defun mdopen--kill-process ()
  "Kill the mdopen process for the current buffer."
  (when (and mdopen--process (process-live-p mdopen--process))
    (delete-process mdopen--process)
    (setq mdopen--process nil)
    (message "Killed the mdopen process for buffer: %s" (buffer-name))))

(defun mdopen--start-process ()
  "Start or reuse the mdopen process for the current buffer."
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  ;; Only start a new process if one isn't already running
  (unless (and mdopen--process (process-live-p mdopen--process))
    (setq mdopen--process
          (start-process "mdopen-process" "*mdopen-output*"
                         mdopen-binary-path buffer-file-name))
    (message "Started mdopen process for: %s" buffer-file-name)))

(defun mdopen-refresh-browser ()
  "Refresh the browser for the current preview URL."
  (interactive)
  (if mdopen--last-url
      (progn
        (message "Refreshing browser for URL: %s" mdopen--last-url)
        ;; Reload manually in the browser if the URL is already open
        (browse-url mdopen--last-url))
    (message "No preview URL available to refresh.")))

(defun mdopen-refresh ()
  "Refresh the Markdown preview after saving."
  (interactive)
  (let* ((file-relative-path (file-relative-name buffer-file-name))
         (preview-url (format "%s/%s" mdopen-server-host file-relative-path)))
    ;; Ensure the mdopen process is running
    (if (and mdopen--process (process-live-p mdopen--process))
        (progn
          (message "Reusing existing mdopen process for buffer: %s" (buffer-name))
          ;; Refresh browser manually if needed
          (setq mdopen--last-url preview-url))
      ;; Restart the process if necessary
      (message "mdopen process not found, restarting...")
      (mdopen--start-process)
      (setq mdopen--last-url preview-url))
    ;; The browser is refreshed manually, not automatically
    (message "Markdown preview updated. Use M-x mdopen-refresh-browser to refresh the browser.")))

(defun mdopen-start-preview ()
  "Start the Markdown preview with mdopen."
  (interactive)
  (when buffer-file-name
    (add-hook 'after-save-hook #'mdopen-refresh nil t)
    (add-hook 'kill-buffer-hook #'mdopen--kill-process nil t)
    ;; Start or reuse the existing mdopen process
    (mdopen--start-process)
    ;; Open the browser only once
    (let* ((file-relative-path (file-relative-name buffer-file-name))
           (preview-url (format "%s/%s" mdopen-server-host file-relative-path)))
      (setq mdopen--last-url preview-url)
      (browse-url preview-url)
      (message "Opened mdopen preview for: %s" preview-url))))

(defun mdopen-stop-preview ()
  "Stop the Markdown preview process and remove hooks."
  (interactive)
  (remove-hook 'after-save-hook #'mdopen-refresh t)
  (remove-hook 'kill-buffer-hook #'mdopen--kill-process t)
  ;; Stop the mdopen process
  (mdopen--kill-process)
  ;; Clear the URL
  (setq mdopen--last-url nil)
  (message "Stopped mdopen preview for this buffer."))

;;;###autoload
(define-minor-mode mdopen-mode
  "Minor mode for live previewing Markdown files via mdopen."
  :lighter " mdopen"
  (if mdopen-mode
      (mdopen-start-preview)
    (mdopen-stop-preview)))

(provide 'mdopen-mode)

;;; mdopen-mode.el ends here
