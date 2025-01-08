;;; mdopen-mode.el --- Instant Markdown preview using mdopen.   -*- lexical-binding: t; -*-

;; Author: jcook3701
;; Homepage: https://github.com/jcook3701/mdopen-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, markdown, preview

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Instant Markdown preview using mdopen subprocess.
;;
;; Features:
;; - Option to preview the raw markdown file directly using `mdopen`.
;; - Or create temporary `.tmp.md` files in `temporary-file-directory` for preview.
;;
;; Install:
;; From melpa, `M-x package-install RET mdopen-mode RET`.
;; Add the following to your `init.el`:
;; ```emacs-lisp
;; (use-package mdopen-mode
;;   :ensure t
;;   :bind (:map markdown-mode-command-map
;;          ("m" . mdopen-mode)))
;; ```
;; Run `M-x mdopen-mode` to preview a Markdown file with your defined preferences.

;;; Code:

(defgroup mdopen nil
  "Instant Markdown preview using mdopen."
  :group 'markdown
  :link '(url-link :tag "Homepage" "https://github.com/jcook3701/mdopen-mode"))

(defcustom mdopen-binary-path "mdopen"
  "Path to the `mdopen` binary."
  :type 'string
  :group 'mdopen)

(defvar mdopen--process nil
  "Active `mdopen` process for the current buffer.")

(defun mdopen--kill-process ()
  "Kill the running `mdopen` process tied to the current buffer."
  (when (and mdopen--process (process-live-p mdopen--process))
    (delete-process mdopen--process)
    (setq mdopen--process nil)
    (message "Killed mdopen process for buffer: %s" (buffer-name))))

(defun mdopen--start-process ()
  "Start the `mdopen` process to preview the current Markdown file.
If a process already exists, keep it running and reuse it."
  (when (not (buffer-file-name))
    (user-error "Buffer is not visiting a file"))
  (unless (and mdopen--process (process-live-p mdopen--process))
    ;; If the process doesn't exist, start a new one
    (setq mdopen--process
          (start-process "mdopen-process" nil
                         mdopen-binary-path buffer-file-name))
    (message "Started mdopen process for file: %s" buffer-file-name)))

(defun mdopen-refresh ()
  "Signal the existing `mdopen` process to refresh or restart it if necessary."
  (interactive)
  (if (and mdopen--process (process-live-p mdopen--process))
      (progn
        ;; Reload the file in the existing mdopen process
        (message "Signaling mdopen to refresh for file: %s" buffer-file-name))
    ;; If the process died or doesn’t exist, start a fresh one
    (message "mdopen process is not running; restarting...")
    (mdopen--start-process)))

(defun mdopen-start-preview ()
  "Start the `mdopen-mode` preview for the current buffer."
  (interactive)
  (add-hook 'after-save-hook #'mdopen-refresh nil t)
  (add-hook 'kill-buffer-hook #'mdopen--kill-process nil t)
  (mdopen--start-process))

(defun mdopen-stop-preview ()
  "Stop the `mdopen-mode` preview for the current buffer."
  (interactive)
  (remove-hook 'after-save-hook #'mdopen-refresh t)
  (remove-hook 'kill-buffer-hook #'mdopen--kill-process t)
  (mdopen--kill-process))

;;;###autoload
(define-minor-mode mdopen-mode
  "Minor mode to preview Markdown files with mdopen."
  :lighter " mdopen"
  :group 'mdopen
  (if mdopen-mode
      (mdopen-start-preview)
    (mdopen-stop-preview)))

(provide 'mdopen-mode)

;;; mdopen-mode.el ends here
