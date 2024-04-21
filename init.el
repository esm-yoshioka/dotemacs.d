;;; init.el --- My init.el  -*- coding: utf-8 ; lexical-binding: t -*-

;; Author: esm-yoshioka
;; Version: 29.2


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
(leaf Files
  :doc "system file"
  :custom
  ;; backup file
  (backup-directory-alist . `((".*" . ,(locate-user-emacs-file "backup"))))
  (version-control . t)
  (kept-new-versions . 50)
  (kept-old-versions . 0)
  (delete-old-versions . t)
  ;; auto-save file
  (auto-save-file-name-transforms . `((".*" ,(locate-user-emacs-file "backup/") t)))
  (auto-save-list-file-prefix . ,`(locate-user-emacs-file "backup/.saves-"))
  (auto-save-timeout . 15)
  (auto-save-interval . 120)
  ;; lock file
  (create-lockfiles . nil)
  )

(leaf Looks
  :doc "app style"
  :custom
  (menu-bar-mode . nil)
  (scroll-bar-mode . nil)
  (tool-bar-mode . nil)
  (inhibit-startup-message . t)
  (initial-scratch-message . "")
  (show-paren-mode . t)
  )

;; =========================================================================================

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
