;;(load "~/lib/emacs/haskell-mode/haskell-site-file")
(add-to-list 'load-path "~/.emacs.d/emacsAddons/")
(let ((default-directory "~/.emacs.d/emacsAddons/"))
  (normal-top-level-add-subdirs-to-load-path))

;; theme paths
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/noctilux-theme/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/mytheme/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")


;; smooth scrolling
(require 'smooth-scroll)


(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Hides tool bar and menu bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;;(require 'rainbow-delimiters)
;;(global-rainbow-delimiters-mode)
;; associate .m files with Octave.
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
;;Finally, to turn on the abbrevs, auto-fill and font-lock features automatically, also add the following lines to one of the Emacs startup files:
(add-hook 'octave-mode-hook
	  (lambda ()
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))





;; compile tex to pdf
(setq TeX-PDF-mode t)

;; set up org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; haskell stuff:
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; magit test
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa.milkbox.net/packages/")))

;; no more trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)



(electric-pair-mode +1)

;; highlight parens
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(require 'linum) ;; line numbers
(global-linum-mode 1)

;; camelCase
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'haskell-mode-hook 'subword-mode)

;; storing temporary data in the system's temporary directory instead of the current folder:
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; variables for TRAMP:
(setq tramp-default-method "ssh")
(setq tramp-default-user "halvobj"
      tramp-default-host "clustis3.idi.ntnu.no")




(set-face-attribute 'default nil :font"Inconsolata-13")

;; setting default window size:
 (if window-system
      (set-frame-size (selected-frame) 80 24))


(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; hotkeys for resizing buffers.
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


;; drupal bullshit:
(require 'php-mode)
(require 'drupal-mode)

(add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("\\.\\(php\\|inc\\)$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))


(ido-mode)
(ido-toggle-case) ;; case sensitive search.
(setq ido-enable-flex-matching t) ; fuzzy matching


;; glsl syntax highlighting:
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#282a2e" "#da4939" "#a5c261" "#ffc66d" "#6d9cbe" "#b6b3eb" "#6d9cbe" "#f4f1ed"])
 '(ansi-term-color-vector [unspecified "#282a2e" "#da4939" "#a5c261" "#ffc66d" "#6d9cbe" "#b6b3eb" "#6d9cbe" "#f4f1ed"] t)
 '(background-color "#fdf6e3")
 '(background-mode light)
 '(cursor-color "#657b83")
 '(custom-enabled-themes (quote (base16-halvorgb-mocha)))
 '(custom-safe-themes (quote ("9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "fc2d9b2a9c4a4c05a5de7824fd8999b2213e70504c4f5b032cf3792d86cc805d" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "d4f150cd4a43baf0c00e1d18753be4526402ca7a4625b9d4c95f77c27cf7c18d" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "645599a2aab022fd7677124515a3104a60ba64d2cafdd77a6e7703f8ae97250c" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "4cb3034cbb7fd36bf0989fad19cac0beb818472854a7cbc8d2a597538b1f2cf0" "91b5a381aa9b691429597c97ac56a300db02ca6c7285f24f6fe4ec1aa44a98c3" "4cb2c00712b8d0681142fc932e9f977ac82e77deb8d6a843bfeb6e793213a190" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "9e7e1bd71ca102fcfc2646520bb2f25203544e7cc464a30c1cbd1385c65898f4" "5153bfc712cc864558306f2a824b9ea52795e62aefc0960c13a8b6cad53aadb9" "65f4912d08f7ea366b143d2c15b4465b0a58516d8182763a7182bf8a76276e13" "ad2e4e289479dca9b9d16ac59ec3683f568b9b8d170663e071af9fdd176e5655" "a79d046ba20cb64791b2aeaa972c07b9d86cdd6cd097d0d7685d7cd3db645178" "15a93ee8a166d0608550911d62a66ac815e34ec6db1ae3ce13aa0ea592af47a3" "8920901f23784735e36de0acb3d1ac1bd57c8ffb85644e899472adcda651a800" "0c3f5ccb31602499ba3dd7e0543201fca1fd204650c487fcdbc58039275a3599" "80e1ac56bcd1c32fd9e446def54c85f0b1135ffc4fc0d5a329fbcd2bd1dd487f" "7cd77efdb74989d9efe482c530b0839cd3c34003aca88311d09bc08ed2669ecf" default)))
 '(foreground-color "#657b83")
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(linum-format "%i"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
