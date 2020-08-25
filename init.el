;;; Setting up use-package I think?
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
)
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
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(elcord chess fzf powerline hl-todo vterm docker-compose-mode dockerfile-mode org magit))
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
(powerline-default-theme)
(setq-default c-basic-offset 8)
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "linux")))
;;(require 'rust-mode)
;;(add-hook 'rust-mode-hook
;;	  (lambda () (setq indent-tabs-mode nil)))
;; (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode)
;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-compelete-common)
;; (setq company-tooltip-align-annotations t)

;; from the "better defaults" github page source: https://github.com/technomancy/better-defaults/blob/master/better-defaults.el
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(require 'elcord)
;; (elcord-mode)


;; My full name and email address for whatever reason this is required
(setq user-full-name "Ethan Carter Edwards"
      user-mail-address "ethancarteredwards@gmail.com")

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

(eval-after-load "org"
  '(require 'ox-md nil t))
(eval-after-load "org"
  '(require 'org-tempo))

(setq diary-file "~/Nextcloud/emacs-diary")
(setq org-agenda-include-diary t)

;; (custom-set-variables
;;  '(org-directory "~/Nextcloud/Org")
;;  '(org-agenda-files (list org-directory)))

;; (setq org-agenda-files (list "~/Nextcloud/Org/glusterfs.org"
;; 			     "~/Nextcloud/Org/School.org"
;; 			     "~/Nextcloud/Org/Work.org"
;; 			     "~/Nextcloud/Org/basics.org"))
