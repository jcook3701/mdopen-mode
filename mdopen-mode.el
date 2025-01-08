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
  "Path to the mdopen binary."
  :type 'file
  :group 'mdopen)

(defvar mdopen--process nil
  "The mdopen process for previewing the Markdown file.")

(defun mdopen--kill-process ()
  "Kill any running mdopen process."
  (when mdopen--process
    (delete-process mdopen--process)
    (message "Process `%s' killed" mdopen--process)
    (setq mdopen--process nil)))

(defun mdopen--start-process ()
  "Start the mdopen process to preview the current Markdown file."
  (mdopen--kill-process)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (setq mdopen--process
        (start-process "mdopen-process" "*mdopen-output*"
                       mdopen-binary-path buffer-file-name))
  (message "Started mdopen process to preview: %s" buffer-file-name))

(defun mdopen-refresh ()
  "Refresh the preview by restarting the mdopen process."
  (interactive)
  (mdopen--start-process))

(defun mdopen-start-preview ()
  "Start previewing the current Markdown buffer using mdopen."
  (interactive)
  (add-hook 'after-save-hook #'mdopen-refresh nil t)
  (add-hook 'kill-buffer-hook #'mdopen--kill-process nil t)
  (mdopen--start-process))

(defun mdopen-stop-preview ()
  "Stop the mdopen preview and clean up."
  (interactive)
  (remove-hook 'after-save-hook #'mdopen-refresh t)
  (remove-hook 'kill-buffer-hook #'mdopen--kill-process t)
  (mdopen--kill-process))

;;;###autoload
(define-minor-mode mdopen-mode
  "Minor mode for previewing Markdown files with mdopen."
  :lighter " mdopen"
  :group 'mdopen
  (if mdopen-mode
      (mdopen-start-preview)
    (mdopen-stop-preview)))

(provide 'mdopen-mode)

;;; mdopen-mode.el ends here
