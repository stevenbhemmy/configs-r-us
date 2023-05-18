;; Save session, every 60 seconds.
(defconst my-savefile-dir "/home/shemmy/.emacs.d")
(setq desktop-path (list my-savefile-dir))
(setq desktop-dirname my-savefile-dir)
(setq desktop-restore-eager 5)
(setq desktop-load-locked-desktop t)
(setq desktop-auto-save-timeout 60)
(desktop-save-mode +1)

;; Default font
(set-face-attribute 'default t
                    :font "Hack 10")
(set-frame-font "Hack 10" nil t)

;; Don't make me type "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

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

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Auto install packages not present
(defvar my-packages '(auto-complete
                      ac-cider
                      blacken
                      cider
                      clojure-mode
                      color-theme-approximate
                      evil
                      evil-surround
                      evil-smartparens
                      evil-commentary
                      exec-path-from-shell
                      gist
                      helm
                      helm-projectile
                      ido
                      jedi
                      jinja2-mode
                      linum-off
                      linum-relative
                      org
                      popup
                      powerline
                      projectile
                      python-mode
                      rainbow-delimiters
                      smartparens
                      solarized-theme
                      undo-tree))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; '' For stuff not in package repositories
;; (add-to-list 'load-path "/home/shemmy/.emacs.d/parinfer-mode/")
;; 
;; ;;Parinfer
;; (require 'parinfer-mode)
;; (add-hook 'prod-mode-hook 'parinfer-mode)
;; (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
;; (add-hook 'lisp-mode-hook 'parinfer-mode)
;; (add-hook 'scheme-mode-hook 'parinfer-mode)
;; (add-hook 'clojure-mode-hook 'parinfer-mode)

(require 'cider)

;; Mac fix for environment var weirdness
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Evil mode stuff
(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(evil-commentary-mode)
;; evil mode requires setting an undo system for some god forsaken reason
;; apparently redo working out of the box is too much to ask ffs
(global-undo-tree-mode)
(setq evil-undo-system 'undo-tree)
(define-key evil-normal-state-map "u" 'undo-tree-undo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)

;; Detect .jinja suffix
(require 'jinja2-mode)
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode))
(setq sgml-basic-offset 4)

;; Change javascript indent
(setq js-indent-level 2)

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
(setq helm-autoresize-mode 30)
(setq helm-split-window-in-side-p t)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-swith-project-action 'helm-projectile-find-file)

;; Show numbers
(require 'linum-relative)
(global-linum-mode t)
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
;; (require 'smartparens-config)
;; (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
;; (add-hook 'lisp-mode-hook 'smartparens-mode)
;; (add-hook 'scheme-mode-hook 'smartparens-mode)
;; (add-hook 'clojure-mode-hook 'smartparens-mode)
;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;; (defadvice show-paren-function
;;     (after show-matching-paren-offscreen activate)
;;     "If the matching paren is offscreen show the matching line in the
;;      echo area. Has no effect if the character before point is not of
;;      syntax class ')'."
;;     (interactive)
;;     (let* ((cb (char-before (point)))
;; 	   (matching-text (and cb
;; 			       (char-equal (char-syntax cb) ?\) )
;; 			       (blink-matching-open))))
;;        (when matching-text (message matching-text))))


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

;; Python auto complete
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; Solarized theme
(color-theme-approximate-on)
(load-theme 'solarized-dark t)

;; Powerline stuff
(require 'powerline)
(powerline-center-evil-theme)
(setq powerline-color1 "#073642")
(setq powerline-color2 "#002b36")

(set-face-attribute 'mode-line nil
                    :foreground "#fdf6e3"
                    :background "#2aa198"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :inverse-video nil
                    :box nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default)))
 '(line-number-mode nil)
 '(package-selected-packages
   (quote
    (color-theme-approximate undo-tree undo-fu solarized-theme rainbow-delimiters python-mode powerline linum-relative linum-off jinja2-mode jedi helm-projectile gist exec-path-from-shell evil-surround evil-smartparens evil-commentary ac-cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
