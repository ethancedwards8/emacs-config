;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!
(setq gc-cons-threshold (* 50 1000 1000))

;; initialize package sources
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("ublt" . "https://elpa.ubolonton.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(defun find-config ()
  "Edit README.org/init.el"
  (interactive)
  (find-file "~/.emacs.d/README.org"))

(global-set-key (kbd "C-c I") 'find-config)

;; (setq custom-file (make-temp-file "emacs-custom"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (setq custom-file (make-temp-file "emacs-custom.el"))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist `(("." . "~/.saves")))

;; (when (version<= "26.0.50" emacs-version )
;;   (global-display-line-numbers-mode))

;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(setq confirm-kill-emacs 'y-or-n-p)

(setq user-full-name "Ethan Carter Edwards"
      user-mail-address "ethancarteredwards@gmail.com")

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; '(warning-suppress-log-types '((comp) (comp)))
 ;; '(warning-suppress-types '((comp))))

(setq warning-supress-log-types '((comp)))
(setq warning-supress-types '((comp)))

;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'enlarge-window)
(global-set-key (kbd "S-C-<up>") 'shrink-window)

(global-set-key (kbd "C-S-v") 'scroll-up-command)

(defun my/custom-S-o-from-vim (times)
  "Inserts a newline(s) above the line conataining the cursor.
Very Similar to S-o from Vim"
  (interactive "p")
  (save-excursion 
    (move-beginning-of-line 1)
    (newline times)))

(global-set-key (kbd "C-S-o")
		'my/custom-S-o-from-vim)

;; Automatically tangle our Emacs.org config file when we save it
(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/README.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

;; Have org-agenda files list recursively
(defun my/refresh-org-files ()
      (interactive)
      (setq org-agenda-files (apply 'append
				    (mapcar
				     (lambda (directory)
				       (directory-files-recursively
					directory org-agenda-file-regexp))
				     '("~/Nextcloud/Org/")))))

;; (setq default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "JB  " :family "JetBrains Mono"))))

(set-face-attribute 'default t :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 98 :width 'normal :foundry "JB  " :family "JetBrains Mono")

(setq ansi-color-faces-vector
  [default default default italic underline success warning error])
(setq ansi-color-names-vector
  ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])

(use-package spacegray-theme :defer t)
(use-package doom-themes
  :defer t
  :init (load-theme 'doom-palenight t))

;; (use-package powerline
;;   :config
;;   (powerline-default-theme))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

(use-package dashboard
  :custom
  (dashboard-banner-logo-title "The Grind is not Glamorous - Casey Neistat")
  (dashboard-startup-banner "~/.emacs.d/images/floating-meditate.png")
  ;; (dashboard-startup-banner 'logo)
  :config
  (setq dashboard-items '((recents  . 5)
			(bookmarks . 5)
			;; (projects . 5)
			(agenda . 5)
			(registers . 5)))
  (dashboard-setup-startup-hook))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :diminish
  :config
  (evil-commentary-mode))

(use-package evil-org
  ;; :diminish evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package magit
  :bind (("C-x g" . magit-status)))

;; (global-set-key (kbd "C-x g") 'magit-status)

(use-package evil-magit
  :after magit)

(use-package magit-todos
  :defer t)

;; Pulled from David Wilson's config, probably won't use
(global-set-key (kbd "C-M-;") 'magit-status)

(use-package org
  :custom
  (org-directory "~/Nextcloud/org")
  (diary-file "~/Nextcloud/emacs-diary")
  (org-log-done t)
  (org-agenda-include-diary t)
  :bind (("C-c l" . org-stored-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (eval-after-load "org"
    '(require 'ox-md nil t))
  (eval-after-load "org"
    '(require 'org-tempo))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))

  (my/refresh-org-files))

(use-package ox-twbs)

(use-package vterm
  :custom
  (vterm-always-compile-module t)
  :bind (("C-x v" . vterm)
	 ("C-x 4 v" . vterm-other-window)
	 :map vterm-mode-map
	 ;; came up with this myself, pretty proud of it not going to lie :)
	 ("<C-backspace>" . (lambda () (interactive) (vterm-send-meta-backspace)))))

(use-package eshell-git-prompt)

(use-package eshell
  :ensure nil
  :custom (eshell-aliases-file "~/.emacs.d/eshell-alias")
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destory-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "iotop")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package ivy
  :diminish
  :custom (ivy-initial-inputs-alist nil)
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-S-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-j" . ivy-next-line)
	 ("C-d" . ivy-switch-buffer-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-x j" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package exwm
  :config
  (require 'exwm-config)
  (exwm-config-example))

(use-package general
  :config
  (general-auto-unbind-keys)
  (general-override-mode +1)

  (general-create-definer my/leader-key
    :states '(normal insert visual emacs treemacs)
    :keymap 'override
    :prefix "SPC"
    :global-prefix "C-SPC"
    :non-normal-prefix "C-SPC"))

(my/leader-key 
      "SPC"  '(counsel-find-file :wk "counsel find file")
      "o r" '(my/refresh-org-files :wk "refresh my org files")
      "I" '(find-config :wk "edit README.org/init.el")
      ;; "o a" '(org-agenda :wk "org agenda")
      "TAB" '(evil-switch-to-windows-last-buffer :wk "switch to previous buffer"))

(use-package rainbow-mode
  :config
  ;; (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package rg)

(use-package hl-todo
  :config
  (hl-todo-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package haskell-mode)

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
	("\\.yaml\\'" . yaml-mode))

(use-package docker-compose-mode)

(use-package dockerfile-mode)

(use-package elcord
  ;; :config
  ;; (when (string= (system-name) "archpc")
  ;;   (elcord-mode))
  )

(use-package chess)

(setq-default c-basic-offset 8)
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "linux")))
