(use-package eglot
  :ensure t
  :defer t
  :hook ((python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives '(("pyright-langserver" "--stdio"))))))

(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load)
  (python-ts-mode . flymake-ruff-load)
  (eglot-managed-mode . flymake-ruff-load))


(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "--output-format=text"
            (eval (when buffer-file-name
                    (concat "--stdin-filename=" buffer-file-name)))
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes (python-mode python-ts-mode))

(defun python-flycheck-setup ()
  (progn
    (flycheck-select-checker 'python-ruff)
    (flycheck-add-next-checker 'python-ruff)))

(use-package flycheck
  :config
  (setq flycheck-python-mypy-executable "mypy")
  (flycheck-add-mode 'python-mypy 'python-ts-mode)
  (flycheck-add-mode 'python-mypy 'python-mode)
  (flycheck-add-next-checker 'python-ruff 'python-mypy)
  (global-flycheck-mode t))

(defcustom ruff-format-import-command "ruff"
  "Ruff command to use for formatting."
  :type 'string
  :group 'ruff-format-import)

;;;###autoload (autoload 'ruff-format-import-buffer "ruff-format-import" nil t)
;;;###autoload (autoload 'ruff-format-import-region "ruff-format-import" nil t)
;;;###autoload (autoload 'ruff-format-import-on-save-mode "ruff-format-import" nil t)
(reformatter-define ruff-format-import
  :program ruff-format-import-command
  :args (list "check" "--fix" "--select=I" "--stdin-filename" (or (buffer-file-name) input-file))
  :lighter " RuffFmt"
  :group 'ruff-format-import)

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package pyconf
  :ensure t
  :vc (:url "https://github.com/andcarnivorous/pyconf" :branch "main"))

;; Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (pyconf-add-config (pyconf-config :name "mymainconfig2"                                       ;;
;;                                    :pyconf-exec-command "python"                              ;;
;;                                    :pyconf-file-to-exec "~/test/main.py"                      ;;
;;                                    :pyconf-path-to-exec "~/test/"                             ;;
;;                                    :pyconf-params ""                                          ;;
;;                                    :pyconf-env-vars '("FOO=BAR")))                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a") ; these collide with activities.el
  (setq dape-key-prefix "\C-x\C-d") ; instead of list dir

  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load))

  :init
  ;; To use window configuration like gud (gdb-mi)
  (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'gud)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer))


(add-to-list 'flycheck-checkers 'python-ruff)
(add-hook 'python-mode-local-vars-hook #'python-flycheck-setup 'append)
(add-hook 'python-mode-hook 'ruff-format-import-on-save-mode)
(add-hook 'python-ts-mode-hook 'ruff-format-import-on-save-mode)
(add-hook 'python-mode-hook 'ruff-format-on-save-mode)
(add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode)

