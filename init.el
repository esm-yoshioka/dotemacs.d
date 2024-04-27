;;; init.el --- My init.el  -*- coding: utf-8 ; lexical-binding: t -*-
;; 
;; Author: esm-yoshioka
;; Version: 29.3
;; 


(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))


;; Describe own settings below
;; =========================================================================================
(when load-file-name
  (setq user-emacs-directory
        (expand-file-name (file-name-directory load-file-name))))

(defconst my:d:vars
  (expand-file-name "vars/" user-emacs-directory))
(unless (file-directory-p my:d:vars)
  (make-directory my:d:vars))
(defconst my:d:backup
  (expand-file-name "backup/" user-emacs-directory))
(unless (file-directory-p my:d:backup)
  (make-directory my:d:backup))

;; -----------------------------------------------------------------------------------------

(leaf Ime
  :config
  (set-language-environment "Japanese")
  (leaf windows
    :when (eq system-type 'windows-nt)
    :config
    (leaf tr-ime
      :doc "Emulator of IME patch for Windows"
      :ensure t
      :config
      (tr-ime-advanced-install)
      )
    (set-language-environment "Japanese")

    (setq default-input-method "W32-IME")
    (setq-default w32-ime-mode-line-state-indicator "[Aa]")
    (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
    (w32-ime-initialize)
    ;; IME disable pattern
    (w32-ime-wrap-function-to-control-ime 'universal-argument)
    (w32-ime-wrap-function-to-control-ime 'read-string)
    (w32-ime-wrap-function-to-control-ime 'read-char)
    (w32-ime-wrap-function-to-control-ime 'read-from-minibuffer)
    (w32-ime-wrap-function-to-control-ime 'y-or-n-p)
    (w32-ime-wrap-function-to-control-ime 'yes-or-no-p)
    (w32-ime-wrap-function-to-control-ime 'map-y-or-n-p)
    (w32-ime-wrap-function-to-control-ime 'register-read-with-preview)
    )
  )
  
(leaf Setting
  :doc "general settings"
  :setq
  ;; default directory
  (default-directory . "~/")
  (command-line-default-directory . "~/")
  :config
  ;; coding-code
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8)
  :custom
  (confirm-kill-emacs . 'y-or-n-p)      ; check on exit
  (use-short-answers . t)               ; y-or-n
  ;; scroll
  (scroll-conservatively . 1)
  (scroll-margin . 3)
  (next-screen-context-lines . 3)
  (scroll-preserve-screen-position . t)
  
  (echo-keystrokes . 0.1)               ; echo Area key-strokes
  (ring-bell-function . 'ignore)        ; error beep off
  (kill-ring-max . 200)                 ; keep kill-ring
  (require-final-newline . t)           ; auto-insert last line
  )

(leaf SystemFiles
  :doc "system file"
  :custom
  ;; backup file
  (backup-directory-alist . `(("." . ,my:d:backup)))
  (backup-by-copying . t)
  (version-control . t)
  (kept-new-versions . 50)
  (kept-old-versions . 0)
  (delete-old-versions . t)
  ;; auto-save file
  ;; 下記設定が効いてないのでいったんコメントアウト
  ;; auto-save-listに仮保存されるのをそのうち対応したい
  ;; (auto-save-list-file-prefix . ,`(locate-user-emacs-file "backup/.saves-"))
  (auto-save-file-name-transforms . `((".*" ,my:d:backup t)))
  (auto-save-timeout . 15)
  (auto-save-interval . 120)
  ;; lock file
  (create-lockfiles . nil)
  )

(leaf Search
  :custom
  ;; ignore upper/lower case
  (case-fold-search . t)
  (isearch-case-fold-search . t)
  (completion-ignore-case . t)
  (read-file-name-completion-ignore-case . t)
  (read-buffer-completion-ignore-case . t)
  )

(leaf Saveplace
  :doc "memorise last cursor position"
  :custom
  `((save-place . t)
    (save-place-file
     . ,(expand-file-name "save-places"  my:d:vars))
    )
  :hook (emacs-startup-hook . save-place-mode)
  :config
  (setq save-place-ingore-files-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                save-place-ignore-files-regexp
                tramp-file-name-regexp))
  )

(leaf Autorevert
  :doc "auto-reload updated files outside emacs"
  :custom
  (auto-revert-interval . 1)
  :global-minor-mode global-auto-revert-mode
  )

(leaf Image
  :doc "always display picture"
  :global-minor-mode auto-image-file-mode
  )

(leaf Looks
  :doc "app style"
  :custom
  (menu-bar-mode . nil)                 ; non-display menu-bar
  (scroll-bar-mode . nil)               ; non-display scroll-bar
  (tool-bar-mode . nil)                 ; non-display tool-bar
  (inhibit-startup-message . t)         ; non-display startup
  (initial-scratch-message . "")        ; scratch is null
  (show-paren-mode . t)                 ; hightlight matching paren
  )

;; =========================================================================================

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
