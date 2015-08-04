;; Unbind C-z (minimize/suspend window) because it  doesn't work well with xmonad.
(global-unset-key (kbd "C-z"))

(global-subword-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path "~/.emacs.d/emacsAddons/")
(let ((default-directory "~/.emacs.d/emacsAddons/"))
  (normal-top-level-add-subdirs-to-load-path))


(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade.ferrier.me.uk/packages/")))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


;; Projectile mode
(projectile-global-mode)

;; No more annoying paste behavior!
(setq mouse-yank-at-point t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; No more tabs
(setq-default indent-tabs-mode nil)
(setq-default tabwidth 2)
(defvaralias 'c-basic-offset 'tabwidth)
(defvaralias 'cperl-indent-level 'tabwidth)

;; theme paths
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/noctilux-theme/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/mytheme/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; smooth scrolling
(require 'smooth-scroll)
(smooth-scroll-mode 1)

;; Hides tool bar and menu bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; no more trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; compile tex to pdf
(setq TeX-PDF-mode t)

;; set up org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; haskell stuff:
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;; haskell-indentation2
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  (define-key haskell-mode-map [f9] 'haskell-mode-stylish-buffer)))

;; always reload TAGS
(setq tags-revert-without-query 1)

;; Use hasktags
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-x C-s") (lambda () (interactive)
                                                  (call-interactively 'save-buffer) (haskell-process-generate-tags))))
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;; highlight parens
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(require 'linum) ;; line numbers
(global-linum-mode 1)

;; storing temporary data in the system's temporary directory instead of the current folder:
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(set-face-attribute 'default nil :font"DejaVuSansMono-15")


(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; hotkeys for resizing buffers.
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(ido-toggle-case) ;; case sensitive search.
(setq ido-enable-flex-matching t) ; fuzzy matching
(setq ido-auto-merge-work-directories-length -1) ; stay the fuck out of my current folder

;; glsl syntax highlighting:
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;; qml-mode.
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))


;; transparancy
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

;; expand region!
(require 'expand-region)
(global-set-key (kbd "C-æ") 'er/expand-region)
(delete-selection-mode 1)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)

(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map "ø" 'evil-beginning-of-line)
(define-key evil-normal-state-map "æ" 'evil-end-of-line)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector
   [unspecified "#2b2b2b" "#da4939" "#a5c261" "#ffc66d" "#6d9cbe" "#b6b3eb" "#519f50" "#f9f7f3"])
 '(custom-enabled-themes (quote (base16-halvorgb-railscasts)))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "5153bfc712cc864558306f2a824b9ea52795e62aefc0960c13a8b6cad53aadb9" "3fd36152f5be7e701856c3d817356f78a4b1f4aefbbe8bbdd1ecbfa557b50006" "4b82ebe1275ead296aa283cf3c079b9e47f2e3f47309f602d863ceb18498c98c" "764e3a6472a3a4821d929cdbd786e759fab6ef6c2081884fca45f1e1e3077d1d" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-tags-on-save t)
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(linum-format "%i")
 '(safe-local-variable-values (quote ((TeX-master . "../main")))))
