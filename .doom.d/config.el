(setq user-full-name "Jakub Dlugosz"
      user-mail-address "jdlugosz963@gmail.com")

(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "Mononoki Nerd Font Mono" :size 13))

(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)

(setq org-babel-default-header-args:python
      '((:session . "*python*")
        (:results . "output")))

(setq fancy-splash-image "~/.doom.d/doom.png")

(use-package! lsp-tailwindcss)
