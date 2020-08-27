;;; Setting up use-package I think?
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  (require 'use-package))
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
     ("ublt" . "https://elpa.ubolonton.org/packages/")))
 '(package-selected-packages
   '(evil-collection magit-todos evil-magit evil elcord chess fzf powerline hl-todo vterm docker-compose-mode dockerfile-mode org magit))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "JB  " :family "JetBrains Mono")))))
(setq backup-directory-alist `(("." . "~/.saves")))
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

;; from the "better defaults" github page source: https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; various different bindings, never can remember the org ones though :/
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z f") 'fzf)
(global-set-key (kbd "C-z l") 'ielm)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x v") 'vterm)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done t)
(setq confirm-kill-emacs 'y-or-n-p)

(require 'dired+)

;;; Packages

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (require 'evil)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-magit
  :ensure t
  :config
  (require 'evil-magit))

(use-package magit-todos
  :ensure t
  :config
  )

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

(use-package elcord
  :ensure t)

(use-package chess
  :ensure t)

(use-package fzf
  :ensure t)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))
  
(use-package hl-todo
  :ensure t)

(use-package vterm
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package magit
  :ensure t)
