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
  "Path to the `mdopen` binary executable."
  :type 'string
  :group 'mdopen)

(defvar mdopen--process nil
  "Stores the active process object for mdopen.")

(defvar mdopen--preview-initialized nil
  "Flag to track if a preview has been initialized.")

(defvar mdopen--last-buffer nil
  "Stores the last buffer associated with the mdopen process.")

;;; Internal Functions

(defun mdopen--kill-process ()
  "Kill the `mdopen` process if it exists."
  (when (and mdopen--process (process-live-p mdopen--process))
    (delete-process mdopen--process)
    (setq mdopen--process nil)
    (message "Stopped mdopen process.")))

(defun mdopen--generate-relative-path ()
  "Generate the relative path of the current buffer file."
  (let ((default-directory (or (vc-root-dir) default-directory)))
    (file-relative-name buffer-file-name default-directory)))

;;; Core Functions

(defun mdopen-refresh ()
  "Refresh the mdopen backend process silently for the current buffer."
  (let ((relative-path (mdopen--generate-relative-path)))
    ;; Check if the process needs restarting or updating
    (if (or (not (process-live-p mdopen--process))
            (not (eq mdopen--last-buffer (current-buffer))))
        (progn
          (mdopen--kill-process)
          (setq mdopen--process
                (start-process "mdopen-process" "*mdopen-output*"
                               mdopen-binary-path relative-path))
          (setq mdopen--last-buffer (current-buffer))
          (message "Restarted mdopen process for: %s" relative-path))
      (message "Refreshed mdopen process for: %s" relative-path))))

(defun mdopen-start-preview ()
  "Start the Markdown live preview for the current buffer using mdopen."
  (interactive)
  ;; Validate buffer
  (when buffer-file-name
    ;; Add hooks for saving and cleanup
    (add-hook 'after-save-hook #'mdopen-refresh nil t)
    (add-hook 'kill-buffer-hook #'mdopen--kill-process nil t)
    ;; Start the mdopen process or refresh if already initialized
    (let ((relative-path (mdopen--generate-relative-path)))
      (unless mdopen--preview-initialized
        (message "Starting Markdown preview for: %s" relative-path)
        (setq mdopen--preview-initialized t))
      ;; Ensure the process starts for the current file
      (mdopen-refresh))))

(defun mdopen-stop-preview ()
  "Stop the Markdown preview and cleanup the process."
  (interactive)
  ;; Remove hooks
  (remove-hook 'after-save-hook #'mdopen-refresh t)
  (remove-hook 'kill-buffer-hook #'mdopen--kill-process t)
  ;; Kill the process and reset state
  (mdopen--kill-process)
  (setq mdopen--preview-initialized nil)
  (setq mdopen--last-buffer nil)
  (message "Stopped Markdown preview for this buffer."))

;;;###autoload
(define-minor-mode mdopen-mode
  "Minor mode to enable live Markdown previews using mdopen."
  :lighter " mdopen"
  (if mdopen-mode
      (mdopen-start-preview)
    (mdopen-stop-preview)))

(provide 'mdopen-mode)

;;; mdopen-mode.el ends here
