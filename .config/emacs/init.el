(package-initialize)
(add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package base16-theme)
(load-theme 'base16-solarized-dark t)

(setq org-agenda-files '("~/org/"))
(setq calendar-week-start-day 1)

; https://orgmode.org/manual/Activation.html
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

; https://lists.gnu.org/archive/html/emacs-orgmode/2017-07/msg00213.html
; (setq org-log-repeat nil)

; (setq org-startup-truncated nil)
; (add-hook 'org-mode-hook #'toggle-word-wrap)

;; Hide unnecessary toolbars
(tool-bar-mode -1)
(menu-bar-mode -1)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
