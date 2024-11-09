(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode))
(use-package rainbow-delimiters
  :ensure t)
(use-package eat
  :ensure t)
(use-package ace-window
  :ensure t)
(use-package blamer
  :ensure t)
(use-package org-ac
  :ensure t)
(use-package org-bullets
  :ensure t)
(use-package gruvbox-theme
  :ensure t)
(use-package posframe
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package go-autocomplete
  :ensure t)
(use-package mark-multiple
  :ensure t)
(use-package move-text
  :ensure t)
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package elfeed
  :ensure t)
(use-package package-lint
  :ensure t)
(use-package yasnippet-snippets
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-use-C-h-commands nil)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

(use-package beacon
  :ensure t
  :config
  (beacon-mode t)
  (setq beacon-color "red")
  (setq beacon-blink-delay 0.1)
  (setq beacon-blink-duration 0.1))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode t))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package consult-yasnippet
  :ensure t
  :bind (("C-c y i" . consult-yasnippet)))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-scroll-amount-horizontal 2)
  (mouse-wheel-flip-direction t))

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose.*\.yml\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package emojify
  :ensure t
  :after erc
  :defer 15
  :config
  (global-emojify-mode))

(use-package ffap
  :bind ("C-c v" . ffap))
  
(use-package yasnippet
  :ensure t
  :demand t
  :diminish yas-minor-mode
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas/global-mode)
         ("C-c y m" . yas/minor-mode)
         ("C-c y r" . yas-reload-all)
         ("C-c y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-global-mode 1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix)))

(use-package consult
  :ensure t
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-x p b" . consult-project-buffer)
	 ("C-x b" . consult-buffer)
	 ("C-x C-b" . consult-project-buffer)
	 ("C-M-s" . consult-ripgrep)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'(lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package vertico-mouse
  :ensure nil
  :after vertico
  :init (vertico-mouse-mode))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))        ;; good alternative: M-.
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-mixed-indicator t)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (:all embark consult)
  :demand t
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package dired-subtree
  :ensure t
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)))



;; Programming languages
(use-package go-mode
  :ensure t
  :bind (("C-c C-k" . go-run-this-file)
  ("C-c C-c" . go-compile))
  :hook ((before-save . eglot-format-buffer)))

(use-package lua-mode
  :ensure t)

(use-package repeat
  :config
  (setq set-mark-command-repeat-pop t)

  (repeat-mode 1)

  (defvar switchy-window-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "o") #'switchy-window)
      map))
  
  (put 'switchy-window 'repeat-map 'switchy-window-repeat-map)
  
  (defvar isearch-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "r") #'isearch-repeat-backward)
      map))

  (dolist (cmd '(isearch-repeat-backward))
    (put cmd 'repeat-map 'isearch-repeat-map))

  (defvar buffer-navigation-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "p") #'backward-paragraph)
      (define-key map (kbd "n") #'forward-paragraph)
      (define-key map (kbd "f") #'forward-char)
      (define-key map (kbd "b") #'backward-char)
      map))

  (dolist (cmd '(forward-paragraph backward-paragraph forward-char backward-char))
    (put cmd 'repeat-map 'buffer-navigation-map))

  (defvar paragraph-navigation-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'next-line)
      (define-key map (kbd "p") #'previous-line)
      (define-key map (kbd "f") #'forward-word)
      (define-key map (kbd "b") #'backward-word)
      map))
  
  (dolist (cmd '(next-line previous-line forward-word backward-word))
    (put cmd 'repeat-map 'paragraph-navigation-map)))

(use-package switchy-window
  :bind (("C-x o" . switchy-window))
  :init
  (switchy-window-minor-mode))

(use-package auto-tab-groups
  :vc (:url "https://github.com/MArpogaus/auto-tab-groups")
  :after tab-bar project
  :custom
  ;; Automatically create tabs for projects, denote and dirvish buffers
  (auto-tab-groups-create-commands
   '(((project-prompt-project-dir project-switch-to-buffer) . auto-tab-groups-group-name-project)
     ((denote-create-note denote-menu-list-notes consult-denote-find consult-denote-grep) . "denote")
     ((dirvish dirvish-fd) . "dirvish")))
  ;; Close the project tabs when the project buffers are killed
  (auto-tab-groups-close-commands
   '((project-kill-buffers . auto-tab-groups-group-name-project)
     (dirvish-quit . "dirvish")))
  ;; ;; Enable the "eyecandy-mode" for modern tab bars.
  ;; (auto-tab-groups-eyecandy-mode t)
  ;; ;; Define tab group icons (requires nerd-icons)
  ;; (auto-tab-groups-eyecandy-icons
  ;;  '(("HOME"                                   . "")
  ;;    ("dirvish"                                . "")
  ;;    ("denote"                                 . "󱓩")
  ;;    (auto-tab-groups-eyecandy-name-is-project . auto-tab-groups-eyecandy-group-icon-project)))
  ;; :init
  (auto-tab-groups-mode t))

(setq tab-bar-format '(tab-bar-format-tabs-groups))

(use-package wgrep
  :ensure t)
(require 'wgrep)

(use-package eglot
  :ensure t
  :defer t
  :hook ((python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :custom
  (eglot-report-progress nil)  ; Prevent minibuffer spam

  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives '(("pyright-langserver" "--stdio"))))))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :ensure t
  :after eglot
  :config (eglot-booster-mode))
