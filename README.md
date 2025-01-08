# mdopen-mode

`mdopen-mode` is an Emacs minor mode that allows for an instant preview of Markdown files via `mdopen`. It is inspired by `grip-mode` but specifically aims to streamline the preview process while providing alternative approaches for handling temporary files.  

## Features

- Instantly preview your Markdown files using `mdopen`.  
- **Customizable Temporary File Behavior**:  
  - Either preview the **raw Markdown file directly**.  
  - Or generate temporary `.tmp.md` files stored in Emacs' `temporary-file-directory`   (instead of placing them next to the original file).  
- Automatically refreshes the preview whenever you save the file.  
- Graceful cleanup of preview processes and temporary files.  
- Fully compatible with Emacs' `markdown-mode`.  

## Installation

1. Clone the repository or download the `mdopen-mode.el` file.  
2. Place `mdopen-mode.el` in your Emacs load path.  
3. Add the following to your Emacs configuration (`init.el`):  

	```elisp
	(add-to-list 'load-path "/path/to/mdopen-mode")
	(require 'mdopen-mode)

	;; Optionally enable mdopen-mode automatically for markdown-mode:
	(add-hook 'markdown-mode-hook #'mdopen-mode)
	```

4. (Optional) If you use use-package:  
   
   ``` elisp
   (use-package mdopen-mode
   :load-path "/path/to/mdopen-mode"
   :hook (markdown-mode . mdopen-mode)
   :ensure t)
   ```

5. (Optional) with elpca package manager  
   
   ``` elisp
   (use-package mdopen-mode
   :load-path "/path/to/mdopen-mode"
   :hook (markdown-mode . mdopen-mode)
   :ensure (:fetcher github :repo "jcook3701/mdopen-mode"))
   ```
	
## Customization

`mdopen-mode` provides several customization options:

1. Path to the mdopen Binary  

Make sure mdopen is installed on your system and accessible in your $PATH. If it is installed in a custom location, you can set the binary path:  

``` elisp
(setq mdopen-binary-path "/custom/path/to/mdopen")
```

2. Temporary File Behavior  

By default, mdopen-mode uses temporary .tmp.md files for previews. This ensures your original Markdown files remain unaffected. To enable or disable this behavior:     

``` elisp
;; Use temporary `.tmp.md` files for previews (default)
(setq mdopen-preview-use-temp t)

;; Directly preview the original Markdown file
(setq mdopen-preview-use-temp nil)
```

3. Automatic Refresh  

The preview updates automatically whenever you save the file, ensuring the displayed content is always current.  

``` elisp
(add-hook 'after-save-hook #'mdopen-refresh)
```

## Example Configuration
Here’s how you can configure mdopen-mode with your preferences:  

``` elisp
(use-package mdopen-mode
	:hook (markdown-mode . mdopen-mode)
	:config
	(setq mdopen-binary-path "/usr/local/bin/mdopen") ;; Replace with path to mdopen
	(setq mdopen-preview-use-temp t) ;; Use temp files for previews
	:bind 
	(:map markdown-mode-command-map
		("C-c C-m" . mdopen-mode))
	:ensure (:fetcher github :repo "jcook3701/mdopen-mode"))
```

## Contributing
Contributions are welcome! Please fork the repository, make your changes, and submit a pull request.  

