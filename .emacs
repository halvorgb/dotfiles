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

;; theme paths
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16/")
(set-face-attribute 'default nil :font "Inconsolata-13")

(use-package evil
  :demand)

(projectile-global-mode)
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
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(global-display-line-numbers-mode 1)

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

;; evil
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map "ø" 'evil-beginning-of-line)
(define-key evil-normal-state-map "æ" 'evil-end-of-line)

;; reformatting strings from camelCase to snake_case ++
(require 'string-inflection)
(global-set-key (kbd "C-å") 'string-inflection-all-cycle)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#19171c" "#be4678" "#2a9292" "#a06e3b" "#576ddb" "#955ae7" "#576ddb" "#8b8792"])
 '(ansi-term-color-vector
   [unspecified "#19171c" "#be4678" "#2a9292" "#a06e3b" "#576ddb" "#955ae7" "#576ddb" "#8b8792"])
 '(custom-enabled-themes (quote (base16-unikitty-dark)))
 '(custom-safe-themes
   (quote
    ("3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "daa7ac1dde9d089a998fa2f90c19354fc4ef12bcfd312aca1bcf003a7c381501" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "aeb698d431751b18153e89b5f838fc3992433780a39a082740db216c7202a1c9" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "c11421683c971b41d154b1a4ef20a2c800537b72fefa618b50b184bbfe6b48a0" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "542e6fee85eea8e47243a5647358c344111aa9c04510394720a3108803c8ddd1" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" default)))
 '(package-selected-packages
   (quote
    (redis elm-mode rust-mode toml-mode clojure-mode dockerfile-mode string-inflection csv-mode yaml-mode haskell-mode terraform-mode lua-mode json-mode ensime use-package sql-indent smooth-scroll smex scala-mode sbt-mode projectile markdown-mode macro-math ido-vertical-mode highlight-parentheses groovy-mode expand-region evil company base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
