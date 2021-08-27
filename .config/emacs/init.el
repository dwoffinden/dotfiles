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
