;; Unbind C-z (minimize/suspend window) because it  doesn't work well with xmonad.
(global-unset-key (kbd "C-z"))

(global-subword-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path "~/.emacs.d/emacsAddons/")
(let ((default-directory "~/.emacs.d/emacsAddons/"))
  (normal-top-level-add-subdirs-to-load-path))
;; the package manager
(require 'package)
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; company mode
(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil))

;; run ensime when opening scala files.
 (add-hook 'scala-mode-hook 'ensime)
(use-package ensime
             :commands ensime ensime-mode)

(use-package sbt-mode)
(use-package scala-mode2)

;; Projectile mode
(projectile-global-mode)
;; auto-reload buffers
(global-auto-revert-mode t)

;; No more annoying paste behavior!
(setq mouse-yank-at-point t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; No more tabs
(setq-default indent-tabs-mode nil)
(setq-default tabwidth 2)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tabwidth)
(defvaralias 'cperl-indent-level 'tabwidth)

;; theme paths
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/noctilux-theme/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/mytheme/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; eval math in region
(require 'macro-math)
(global-set-key "\C-x~" 'macro-math-eval-and-round-region)
(global-set-key "\C-x=" 'macro-math-eval-region)

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


;; sql mode: default to psql highlight
(add-to-list 'auto-mode-alist
             '("\\.sql$" . (lambda ()
                             (sql-mode)
                             (sql-highlight-postgres-keywords))))


;; set up org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; haskell stuff:
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; Automatic haskell indentation.
(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)
;;(setq hindent-style "johan-tibell")

(setq haskell-interactive-popup-errors nil)
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

(set-face-attribute 'default nil :font"Inconsolata-10")


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
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-enable-flex-matching t) ; fuzzy matching
(setq ido-auto-merge-work-directories-length -1) ; stay the fuck out of my current folder

(setq tramp-default-method "ssh")
;; transparancy
;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;; (set-frame-parameter (selected-frame) 'alpha '(85 85))
;; (add-to-list 'default-frame-alist '(alpha 85 85))

;; expand region!
(require 'expand-region)
(global-set-key (kbd "C-æ") 'er/expand-region)
(delete-selection-mode 1)
;; text completion:
(global-set-key (kbd "C-ø") 'dabbrev-expand)

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
 '(ansi-color-names-vector
   ["#2b2b2b" "#da4939" "#a5c261" "#ffc66d" "#6d9cbe" "#b6b3eb" "#6d9cbe" "#e6e1dc"])
 '(ansi-term-color-vector
   [unspecified "#2b2b2b" "#da4939" "#a5c261" "#ffc66d" "#6d9cbe" "#b6b3eb" "#519f50" "#f9f7f3"])
 '(custom-enabled-themes (quote (base16-halvorgb-railscasts)))
 '(custom-safe-themes
   (quote
    ("67b6ff0b4786c485ea606167da3b963a35ba37406017c4f50754dcd16230b75b" "4cc014287035b11d1f8d45af1ff18f3509496a760650d16c7771ac9bdf16b1a6" "0240d45644b370b0518e8407f5990a243c769fb0150a7e74297e6f7052a04a72" "37783713b151d949b0da66ff7cd8736dd0893089cbad12eb5a71f3a72e201b47" "8a36587d6cbcc30c85372568ed29d45ec393a32e3c779cba8cfd5fade229025d" "3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" "fe6fb0cb1aa50dc563d81aad98c470a30f4e89db6d55a108f1083f33317ad413" "73ae6088787f6f72ef52f19698b25bc6f0edf47b9e677bf0a85e3a1e8a7a3b17" "f0e69da2cf73c7f153fc09ed3e0ba6e1fd670fec09b8a6a8ed7b4f9efea3b501" "17fd8388e49d3055185e817ed3a2b7c955a2dda92b990f475c14a8e1d97dbe4b" "cc495c40747ae22dd2de6e250cbd9a408e3588b59989368af565deeeff334126" "d72836155cd3b3e52fd86a9164120d597cbe12a67609ab90effa54710b2ac53b" "17f35b689dd41e49cb740bfb810ac8a53d13292cbebf68f41f772787d8b3aebf" "e7ec0cc3ce134cc0bd420b98573bbd339a908ac24162b8034c98e1ba5ee1f9f6" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "5153bfc712cc864558306f2a824b9ea52795e62aefc0960c13a8b6cad53aadb9" "3fd36152f5be7e701856c3d817356f78a4b1f4aefbbe8bbdd1ecbfa557b50006" "4b82ebe1275ead296aa283cf3c079b9e47f2e3f47309f602d863ceb18498c98c" "764e3a6472a3a4821d929cdbd786e759fab6ef6c2081884fca45f1e1e3077d1d" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-tags-on-save t)
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(linum-format "%i")
 '(safe-local-variable-values (quote ((TeX-master . "../main")))))
