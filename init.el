;; startup time function 
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes '(deeper-blue))
 '(menu-bar-mode nil)
 '(org-agenda-files (list org-directory))
 '(org-directory "~/Nextcloud/Org/")
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("ublt" . "https://elpa.ubolonton.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(evil-org rg pdf-tools spotify 2048-game rustic flycheck lsp-ui lsp-mode dashboard debbugs haskell-mode magit-todos evil-magit evil-commentary evil-collection evil elcord ox-twbs org-drill hl-todo chess org powerline vterm docker-compose-mode dockerfile-mode magit use-package))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "JB  " :family "JetBrains Mono")))))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist `(("." . "~/.saves")))
(set 'ad-redefinition-action 'accept)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(setq global-display-line-numbers-mode 'relative)

(setq-default c-basic-offset 8)
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "linux")))

;; My full name and email address for whatever reason this is required
(setq user-full-name "Ethan Carter Edwards"
      user-mail-address "ethancarteredwards@gmail.com")

(add-to-list 'load-path "~/.emacs.d/lisp")

;; from the "better defaults" github page source: https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; various different bindings, never can remember the org ones though :/
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-S-v") 'scroll-up-command)
(global-set-key (kbd "C-z f") 'fzf)
(global-set-key (kbd "C-z l") 'ielm)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x v") 'vterm)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; keybind for "u" to go up a directory in dired
;; (global-set-key (kbd "u") 'dired-up-directory)

;; keybings for window resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'enlarge-window)
(global-set-key (kbd "S-C-<up>") 'shrink-window)

(setq org-log-done t)
(setq confirm-kill-emacs 'y-or-n-p)

(defun my/custom-S-o-from-vim (times)
  "Inserts a newline(s) above the line conataining the cursor.
Very Similar to S-o from Vim"
  (interactive "p")
  (save-excursion 
    (move-beginning-of-line 1)
    (newline times)))

(global-set-key (kbd "C-S-o")
		'my/custom-S-o-from-vim)

(setq truncate-lines t)
;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;;; Packages

;; (use-package org-eldoc)

(use-package rg
  :ensure t
  :config
  (require 'rg))

(use-package lsp-ui)

(use-package lsp-mode
  ;; :hook (;; replace XXX-mode with concrete major-mode
  :commands lsp)

(use-package rustic
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode)))

(add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (lsp-format-buffer))))
	   
(use-package dashboard
  :config
  (setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        ;; (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  (dashboard-setup-startup-hook))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (require 'evil)
  (evil-mode 1)
  (global-undo-tree-mode 0))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-commentary
  :ensure t
  :config
  (require 'evil-commentary)
  (evil-commentary-mode))

(use-package evil-magit
  :ensure t
  :config
  (require 'evil-magit))

(use-package magit-todos
  :ensure t
  :config
  (require 'magit-todos))

(use-package org
  :ensure t
  :config
  (eval-after-load "org"
    '(require 'ox-md nil t))
  (eval-after-load "org"
    '(require 'org-tempo))
  (setq org-log-done t)
  (setq diary-file "~/Nextcloud/emacs-diary")
  (setq org-agenda-include-diary t))

(use-package org-drill
  :ensure t
  :config
  (require 'org-drill))

 (use-package haskell-mode
   :ensure t)

;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install))

(use-package ox-twbs
  :ensure t)

(use-package elcord
  :ensure t)

(use-package chess
  :ensure t)

;; (use-package fzf
;;   :ensure t)

;; (use-package doom-modeline
;;   :ensure t
;;   :config
;;   (doom-modeline-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))
  
(use-package hl-todo
  :ensure t
  :config
  (hl-todo-mode))

(use-package vterm
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package magit
  :ensure t)
