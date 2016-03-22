;; Save session, every 60 seconds.
(desktop-save-mode t)
(desktop-change-dir "/home/shemmy/.emacs.d/")
(setq desktop-auto-save-timeout 60)

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
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) ;issue with TLS
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade.ferrier.me.uk/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Auto install packages not present
(defvar my-packages '(auto-complete
		      ac-cider
		      cider
		      clojure-mode
		      evil
		      evil-surround
		      evil-smartparens
		      evil-commentary
		      gist
		      helm
		      ido
		      linum-off
		      linum-relative
		      org
		      popup
		      powerline
		      projectile
		      helm-projectile
		      python-mode
		      rainbow-delimiters
		      smartparens
		      solarized-theme))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Evil mode stuff
(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(evil-commentary-mode)

;; For easy gists
(require 'gist)

;; Helm stuff
(require 'helm-config)
(helm-mode 1)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-l") 'helm-locate)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-match t)
(setq helm-find-files-fuzzy-match t)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-swith-project-action 'helm-projectile-find-file)

;; Show numbers
(require 'linum-relative)
(global-linum-mode 1)
(linum-relative-on)
(setq linum-relative-format "%3s \u2502")
(setq linum-relative-current-symbol "")

;; Remove line numbers form certain modes
(require 'linum-off)
(defun nolinum ()
  (interactive)
  (linum-mode))
(global-set-key (kbd "<f6>") 'nolinum)
(add-hook 'org-mode-hook 'nolinum)
(add-hook 'helm-mode-hook 'nolinum)

;; Rainbow delims
(require 'rainbow-delimiters)
(add-hook 'prod-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; Highlight matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)
;; Smart parentheses for lisps
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'lisp-mode-hook 'smartparens-mode)
(add-hook 'scheme-mode-hook 'smartparens-mode)
(add-hook 'clojure-mode-hook 'smartparens-mode)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
    "If the matching paren is offscreen show the matching line in the
     echo area. Has no effect if the character before point is not of
     syntax class ')'."
    (interactive)
    (let* ((cb (char-before (point)))
	   (matching-text (and cb
			       (char-equal (char-syntax cb) ?\) )
			       (blink-matching-open))))
       (when matching-text (message matching-text))))

;; Powerline stuff
(require 'powerline)
(powerline-center-evil-theme)

;; Ido mode stuff
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)

;; CIDER stuff
(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
;; Auto Complete stuff
(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)

;; ac-cider (clojure) stuff
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;; Popping-up contextual docs
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-popup-doc))


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
