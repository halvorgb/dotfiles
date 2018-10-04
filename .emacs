;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 2)

;; modes
(electric-indent-mode 0)

;; global keybindings
(global-unset-key (kbd "C-z"))

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(set-face-attribute 'default nil :font "Menlo-13")

(use-package evil
  :demand)

(use-package projectile
  :config
  (projectile-global-mode)
)

;; auto-reload buffers
(global-auto-revert-mode t)

;; No more annoying paste behavior!
(setq mouse-yank-at-point t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; smooth scrolling
;; (pixel-scroll-mode 1)

;; Hides tool bar and menu bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; no more trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; highlight parens
(use-package highlight-parentheses
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t)
)

(global-display-line-numbers-mode 1)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; hotkeys for resizing buffers.
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(use-package ido-vertical-mode
  :config
  (ido-mode 1)
  (ido-vertical-mode 1)
  (ido-toggle-case) ;; case sensitive search.
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-enable-flex-matching t) ; fuzzy matching
  (setq ido-auto-merge-work-directories-length -1) ; stay the fuck out of my current folder
)

;; expand region!
(use-package expand-region
  :config
 (global-set-key (kbd "C-æ") 'er/expand-region)
)
(require 'expand-region)

(delete-selection-mode 1)
;; text completion:
(global-set-key (kbd "C-ø") 'dabbrev-expand)

;; smex
(use-package smex
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "<menu>") 'smex)
)

;; evil

(use-package evil
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map "ø" 'evil-beginning-of-line)
  (define-key evil-normal-state-map "æ" 'evil-end-of-line)
  (setq evil-want-C-u-scroll t)
)

;; reformatting strings from camelCase to snake_case ++
(use-package string-inflection
  :config
  (global-set-key (kbd "C-å") 'string-inflection-all-cycle)
)

;; scala stuff:
; ensime
(use-package ensime
  :ensure t
  :pin melpa-stable)
(add-to-list 'exec-path "/usr/bin")

(defun sql-beautify (beg end)
  (interactive "r")
  (shell-command-on-region
   beg end
   "python -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), reindent=True))'"
   t t))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-woodland t)
)

(use-package markdown-mode)

;; sup
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" default)))
 '(package-selected-packages
   (quote
    (base16-theme use-package string-inflection smex projectile ido-vertical-mode highlight-parentheses expand-region evil ensime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
