;; Default font
(set-default-font "Inconsolata 13")

;; Get rid of splash page
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Get rid of screen clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq-default scroll-step 1)

;; Kill the annoying bell... >:[
(setq ring-bell-function 'ignore)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Auto install packages not present
(defvar my-packages '(clojure-mode
		      evil
		      evil-surround
		      gist
		      helm
		      ido
		      linum-relative
		      org
		      powerline
		      python-mode
		      solarized-theme))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Evil mode stuff
(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)

;; For easy gists
(require 'gist)

;; Helm stuff
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)


;; Show numbers
(require 'linum-relative)
(global-linum-mode 1)
(linum-relative-on)
(setq linum-relative-format "%3s \u2502")
(setq linum-relative-current-symbol "")

;; Powerline stuff
(require 'powerline)
(powerline-center-evil-theme)

;; Ido mode stuff
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
