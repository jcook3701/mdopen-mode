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

(defcustom mdopen-preview-use-temp t
  "If non-nil, use a temporary `.tmp.md` file in `temporary-file-directory` for preview.
Otherwise, preview the raw Markdown file directly."
  :type 'boolean
  :group 'mdopen)

(defvar mdopen--process nil
  "The mdopen process for previewing.")

(defvar-local mdopen--temp-file-path nil
  "Path to the temporary `.tmp.md` file.")

(defun mdopen--kill-process ()
  "Kill any running mdopen process and clean up temporary files."
  (when mdopen--process
    (delete-process mdopen--process)
    (message "Process `%s' killed" mdopen--process)
    (setq mdopen--process nil))
  (when (and mdopen--temp-file-path
             (file-exists-p mdopen--temp-file-path))
    (delete-file mdopen--temp-file-path)
    (setq mdopen--temp-file-path nil)))

(defun mdopen--create-temp-file ()
  "Create a temporary `.tmp.md` file based on the current buffer."
  (let ((temp-file (concat (file-name-as-directory temporary-file-directory)
                           (file-name-nondirectory buffer-file-name)
                           ".tmp.md")))
    (with-temp-file temp-file
      (insert-buffer-substring-no-properties (current-buffer)))
    temp-file))

(defun mdopen--start-process ()
  "Start the mdopen process to preview the Markdown."
  (mdopen--kill-process)
  (let ((preview-file
         (if mdopen-preview-use-temp
             (setq mdopen--temp-file-path (mdopen--create-temp-file))
           buffer-file-name)))
    (setq mdopen--process
          (start-process "mdopen-process" "*mdopen-output*"
                         mdopen-binary-path preview-file))
    (message "Started mdopen process for file: %s" preview-file)))

(defun mdopen-refresh ()
  "Refresh the preview by restarting the mdopen process."
  (when (and mdopen--process (process-live-p mdopen--process))
    (mdopen--kill-process))
  (mdopen--start-process))

(defun mdopen-start-preview ()
  "Start previewing the current Markdown buffer using mdopen."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (add-hook 'after-save-hook #'mdopen-refresh nil t)
  (add-hook 'kill-buffer-hook #'mdopen--kill-process nil t)
  (mdopen--start-process))

(defun mdopen-stop-preview ()
  "Stop the mdopen preview and clean up."
  (interactive)
  (remove-hook 'after-save-hook #'mdopen-refresh t)
  (mdopen--kill-process))

;;;###autoload
(define-minor-mode mdopen-mode
  "Minor mode for previewing Markdown files using mdopen."
  :lighter " mdopen"
  (if mdopen-mode
      (mdopen-start-preview)
    (mdopen-stop-preview)))

(provide 'mdopen-mode)

;;; mdopen-mode.el ends here
