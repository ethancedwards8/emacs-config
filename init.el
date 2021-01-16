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
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; (setq use-package-compute-statistics t)
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

(defun find-main ()
  "Find Main.org, Main Org file"
  (interactive)
  (find-file "~/Nextcloud/Org/Main.org"))

(global-set-key (kbd "C-c O") 'find-main)

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
      user-mail-address "ethan@ethancedwards.com")

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

(defun stop ()
"Proves I'm sane, not losing my sanity whatsoever"
  (interactive)
  (defvar name "*I can quit at any time*")
  (generate-new-buffer name)
  (switch-to-buffer name)
  (insert "I can stop at any time\n")
  (insert "I am in control"))

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

(use-package hydra)

(my/leader-key
      "SPC"  '(counsel-find-file :wk "counsel find file")
      "I" '(find-config :wk "edit README.org/init.el"))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 5))

;; (setq default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "JB  " :family "JetBrains Mono"))))


(set-face-attribute 'default nil :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 98 :width 'normal :foundry "JB  " :family "JetBrains Mono")

(when (string= system-type "darwin")
  (set-face-attribute 'default nil :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 130 :width 'normal :foundry "JB  " :family "JetBrains Mono"))

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
  :custom ((doom-modeline-height 30))
  :config
  (display-time-mode)
  (setq display-time-load-average nil)
  ;; https://emacs.stackexchange.com/questions/20783/remove-load-average-from-time-string-displayed-in-mode-line
  (setq display-time-default-load-average nil)
  (display-battery-mode))

(use-package dashboard
  :config
  ;;(setq dashboard-banner-logo-title "The Grind is not Glamorous - Casey Neistat")
  ;;(setq dashboard-banner-logo-title "Ad Victoriam - Paladin Danse")
  (setq dashboard-banner-logo-title "I'm just a simple man, trying to make my way in the universe. - Jango Fett")
  (setq dashboard-startup-banner "~/.emacs.d/images/floating-meditate.png")
  (setq dashboard-items '((recents  . 5)
			(bookmarks . 5)
			(projects . 5)
			(agenda . 5)
			(registers . 5)))
  (dashboard-setup-startup-hook))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; :bind (:map evil-motion-state-map
  ;;       ("/" . counsel-grep-or-swiper))
  :config
  (evil-mode 1)

  (defhydra my/window-hydra ()
    ("h" evil-window-left)
    ("j" evil-window-down)
    ("k" evil-window-up)
    ("l" evil-window-right)
    ("c" evil-window-delete)
    ("v" evil-window-vsplit)
    ("s" evil-window-split)
    ("o" delete-other-windows)
    ("q" nil "quit"))

  (my/leader-key
      "w"   '(:ignore t :wk "window")
      "w h" '(evil-window-left :wk "move to left window")
      "w j" '(evil-window-down :wk "move to down window")
      "w k" '(evil-window-up :wk "move to up window")
      "w l" '(evil-window-right :wk "move to right window")
      "w c" '(evil-window-delete :wk "move to right window")
      "w v" '(evil-window-vsplit :wk "move to right window")
      "w s" '(evil-window-split :wk "move to right window")
      "w o" '(delete-other-windows :wk "move to right window")
      "TAB" '(evil-switch-to-windows-last-buffer :wk "switch to previous buffer")
      "w w" '(my/window-hydra/body :wk "window hydra")))

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

(use-package magit-todos
  :defer t)

;; Pulled from David Wilson's config, probably won't use
(global-set-key (kbd "C-M-;") 'magit-status)

(use-package dired
  :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil)))

(use-package projectile
  :bind (:map projectile-mode-map
	      (("C-c p" . projectile-command-map)))
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  ;; I don't really want this running all the time, so I `toggle' it from time to time
  (defalias 'toggle-projectile 'projectile-mode))

;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))

(use-package treemacs)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package org
  :custom
  (org-directory "~/Nextcloud/org")
  (diary-file "~/Nextcloud/Org/emacs-diary")
  (org-log-done t)
  (org-agenda-include-diary t)
  :bind (("C-c L" . org-stored-link)
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

  (my/leader-key
      "o r" '(my/refresh-org-files :wk "refresh my org files")
      "o a" '(org-agenda :wk "org agenda"))

  (my/refresh-org-files))

(use-package org-roam
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Nextcloud/Org")
      :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n g" . org-roam-graph))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c n I" . org-roam-insert-immediate))))

(use-package ox-twbs
  :defer t)

(use-package vterm
  :custom
  (vterm-always-compile-module t)
  :bind (("C-x v" . vterm)
	 ("C-x 4 v" . vterm-other-window)
	 :map vterm-mode-map
	 ("<C-backspace>" . (lambda () (interactive) (vterm-send-meta-backspace)))))
	 ;; came up with this myself, fixes C-backspace, pretty proud of it not going to lie :)

(use-package eshell-git-prompt)

(use-package eshell
  :ensure nil
  :custom (eshell-aliases-file "~/.emacs.d/eshell/eshell-alias")
  :config

  (my/leader-key
      "e" '(eshell :wk "eshell"))

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

;; (use-package exwm)

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
  ([remap describe-key] . helpful-key)
  :config
  (my/leader-key
    "h" '(:ignore t :wk "helpful")
    "h f" '(counsel-describe-function :wk "describe function")
    "h v" '(counsel-describe-variable :wk "describe variable")
    "h k" '(helpful-key :wk "describe keybind")))

(use-package rg
  :defer t)

(use-package hl-todo
  :config
  (hl-todo-mode))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize))
  )

(use-package spdx
  :bind (:map prog-mode-map
	 ("C-c i l" . spdx-insert-spdx))
  :custom
  (spdx-copyright-holder 'auto)
  (spdx-project-detection 'auto))

(use-package direnv
 :config
 (direnv-mode))

(use-package debbugs)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package company
  :after lsp-mode
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
	(:map lsp-mode-map
	      ("<tab>" . company-indent-or-complete-common)))

(use-package flycheck
  :hook (lsp-deferred . flycheck-mode))

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp-deferred))

(use-package web-mode
  :mode ("\\.html\\'" . web-mode)
  :mode ("\\.xhtml\\'" . web-mode)
  :mode ("\\.css\\'" . css-mode)
  :mode ("\\.scss\\'" . scss-mode))

(use-package rjsx-mode
  :config
  :mode ("\\.js\\'" . rjsx-mode)
  :mode ("\\.jsx\\'" . rjsx-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package nix-mode
  :config
  (require 'lsp)
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
		    :major-modes '(nix-mode)
		    :server-id 'nix)))

(use-package guix)

(use-package geiser
  :config
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(gambit guile)))

(use-package haskell-mode)

(use-package gdscript-mode)

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
	("\\.yaml\\'" . yaml-mode))

(use-package docker-compose-mode
  :mode ("docker-compose.yml\\'" . docker-compose-mode)
	("docker-compose.yaml\\'" . docker-compose-mode)
	("stack.yml\\'" . docker-compose-mode))

(use-package dockerfile-mode)

(use-package elcord
  :defer t
  ;; :config
  ;; (when (string= (system-name) "archpc")
  ;;   (elcord-mode))
  )

(use-package spotify
  :defer t)

(use-package chess
  :defer t)

(use-package gnugo
  :defer t)

(use-package 2048-game
  :defer t)

(use-package snow
  :defer t)

(my/leader-key
  "a" '(:ignore t :wk "applications"))

;; (use-package mu4e
;;   :ensure nil
;;   ;; :if (and (eq system-type 'gnu/linux) (string-equal system-name "archpc"))
;;   :config
;;   ;; add mu4e to the load path on Arch
;;   (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
;;   (require 'mu4e))

(when (string= (system-name) "archpc")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
  (require 'mu4e))

(use-package emms
  :commands emms
  :config
  (emms-standard)
  (emms-default-players))
