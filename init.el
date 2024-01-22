;; NOTE: init.el is now generated from README.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!
;; (setq gc-cons-threshold (* 50 1000 1000))

;; initialize package sources
;; (require 'package)
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;; 			 ("melpa" . "https://melpa.org/packages/")
;; 			 ("ublt" . "https://elpa.ubolonton.org/packages/")
;; 			 ("org" . "https://orgmode.org/elpa/")))

;; (package-initialize)
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (require 'use-package)
;; ;; (setq use-package-compute-statistics t)
;; (setq use-package-always-ensure t)

;; install straight.el
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
 ;; Enable :elpaca use-package keyword.
 (elpaca-use-package-mode)
 ;; Assume :elpaca t unless otherwise specified.
 (setq elpaca-use-package-by-default t))

;; Necessary to use the `:elpaca' use-package keyword at the top-level.
(elpaca-wait)

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
  "Find index org file"
  (interactive)
  (org-roam-jump-to-index))

;; (global-set-key (kbd "C-c O") '(lambda () "Find index org file" (interactive) (org-roam-jump-to-index)))
(global-set-key (kbd "C-c O") 'find-main)

;; (setq custom-file (make-temp-file "emacs-custom"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (setq custom-file (make-temp-file "emacs-custom.el"))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-to-list 'exec-path "~/.cargo/bin")

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

(setq inhibit-startup-screen t) ;; disables startup screen, which means the default buffer is the scratch buffer!

(show-paren-mode)
(electric-pair-mode)

;; Enable flyspell in these modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
;; disable them in these modes
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
 (add-hook hook (lambda () (flyspell-mode -1))))

;; Enable mouse for MacOS
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(add-hook 'before-save-hook #'(lambda () (delete-trailing-whitespace)))

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
  :demand t
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)
  (general-override-mode +1)

  (general-create-definer my/leader-key
    :states '(normal insert visual emacs treemacs)
    :keymap 'override
    :prefix "SPC"
    :global-prefix "C-SPC"
    :non-normal-prefix "C-SPC"))
(elpaca-wait)

(my/leader-key
	"SPC"  '(counsel-find-file :wk "counsel find file")
	"I" '(find-config :wk "edit README.org/init.el")
	"O" '(find-main :wk "edit index/main org file")
	"." '(counsel-M-x :wk "M-x")
	"b" '(:ignore t :wk "buffer")
	"b k" '(kill-buffer :wk "kill buffer")
	"b b" '(switch-to-buffer :wk "switch buffer")
	"b B" '(ibuffer :wk "all buffers"))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 5))

(set-face-attribute 'default nil :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 98 :width 'normal :foundry "JB  " :family "JetBrains Mono")

(when (string= system-type "darwin")
  (set-face-attribute 'default nil :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 130 :width 'normal :foundry "JB  " :family "JetBrains Mono"))

(when (string= system-name "navidad")
  (set-face-attribute 'default nil :inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :height 130 :width 'normal :foundry "JB  " :family "JetBrains Mono"))

(setq ansi-color-faces-vector
  [default default default italic underline success warning error])
(setq ansi-color-names-vector
  ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])

(use-package spacegray-theme :defer t)
(use-package doom-themes
  :defer t
  :init (load-theme 'doom-palenight t))

(use-package powerline
  :config
  (powerline-default-theme))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-S-u") 'universal-argument)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(my/leader-key
	"w"   '(:ignore t :wk "window")
	"w h" '(evil-window-left :wk "move to left window")
	"w j" '(evil-window-down :wk "move to down window")
	"w k" '(evil-window-up :wk "move to up window")
	"w l" '(evil-window-right :wk "move to right window")
	"w c" '(evil-window-delete :wk "close window")
	"w v" '(evil-window-vsplit :wk "split window vertically")
	"w s" '(evil-window-split :wk "split window horizontally")
	"w o" '(delete-other-windows :wk "delete other windows")
	"TAB" '(evil-switch-to-windows-last-buffer :wk "switch to previous buffer"))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :diminish
  :after evil
  :config
  (evil-commentary-mode))

(fset 'evil-redirect-digit-argument 'ignore) ;; before evil-org loaded

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

;; https://github.com/Somelauw/evil-org-mode/issues/93
;; (add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line)
;; (evil-define-key 'motion 'evil-org-mode
;;     (kbd "0") 'evil-org-beginning-of-line)

(use-package dired
  :elpaca nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil)))

  (my/leader-key
    "e" '(dired-jump :wk "dired")
    "E" '(dired :wk "dired"))

(use-package dired-subtree
	  :bind (:map dired-mode-map
		      ("<tab>" . dired-subtree-toggle)
		      ("<backtab>" . dired-subtree-cycle)))

(use-package org
  :custom
  (org-directory "~/Nextcloud/org")
  (diary-file "~/Nextcloud/Org/emacs-diary")
  (org-default-notes-file "~/Nextcloud/Org/Notes.org")
  (org-log-done t)
  (org-agenda-include-diary t)
  (org-image-actual-width nil)
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

  (my/refresh-org-files))

  (my/leader-key
	"n r" '(my/refresh-org-files :wk "refresh my org files")
	"n a" '(org-agenda :wk "org agenda"))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package vterm
  :custom
  (vterm-always-compile-module t)
  ;; https://github.com/akermu/emacs-libvterm/issues/525
  :bind (("C-x v" . (lambda () (interactive) (vterm t)))
	   ("C-x 4 v" . vterm-other-window)
	   :map vterm-mode-map
	   ("<C-backspace>" . (lambda () (interactive) (vterm-send-meta-backspace)))))
	   ;; came up with this myself, fixes C-backspace, pretty proud of it not going to lie :)
(my/leader-key
	"v v" '((lambda () (interactive) (vterm t)) :wk "vterm"))

(use-package eshell-git-prompt)
(elpaca-wait)

(use-package eshell
  ;; :ensure nil
  :elpaca nil
  :custom (eshell-aliases-file "~/.emacs.d/eshell/eshell-alias")
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destory-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "iotop")))

  (eshell-git-prompt-use-theme 'powerline))

  (my/leader-key
	"v e" '(eshell :wk "eshell"))

(use-package counsel
  :bind (("C-x j" . 'counsel-switch-buffer)
	   :map minibuffer-local-map
	   ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

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

(my/leader-key
    "h" '(:ignore t :wk "helpful")
    "h f" '(counsel-describe-function :wk "describe function")
    "h v" '(counsel-describe-variable :wk "describe variable")
    "h k" '(helpful-key :wk "describe keybind"))

(use-package rg
  :defer t)

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; only install when on macos
(when (string= system-type "darwin")
  (use-package exec-path-from-shell
    :config
    (when (memq window-system '(mac ns))
	(exec-path-from-shell-initialize))
    (when (daemonp)
	(exec-path-from-shell-initialize))
    ))

(use-package envrc
 :init
 (envrc-global-mode))

(use-package debbugs)

(use-package company
  :config
  (company-tng-mode 0)
  :custom (company-minimum-prefix-length 2)
  :bind (:map company-active-map
		("<tab>" . company-complete-selection))
	  (:map lsp-mode-map
		("<tab>" . company-indent-or-complete-common)))

(use-package elcord
  :defer t)

(use-package spotify
  :defer t)

(my/leader-key
    "a" '(:ignore t :wk "applications")
    "a s SPC" '(spotify-playpause :wk "play-pause")
    "a s n" '(spotify-next :wk "spotify next")
    "a s p" '(spotify-previous :wk "spotify previous")
    "a s c" '(spotify-current :wk "spotify current song"))

(use-package chess
  :defer t)

(use-package gnugo
  :defer t)

(use-package snow
  :defer t)

(use-package emms
  :commands emms
  :config
  (emms-standard)
  (emms-default-players))

(use-package elpher)
