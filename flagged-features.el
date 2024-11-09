(setq use-company t)

(when use-company
  (use-package company
    :ensure t
    :hook ((prog-mode . company-mode))
    :bind (:map company-active-map
                ("<return>" . nil)
                ("RET" . nil)
                ("C-<return>" . company-complete-selection)
                ([tab] . company-complete-selection)
                ("TAB" . company-complete-selection)))
  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode)))

(unless use-company
  (use-package corfu
    :after orderless
    ;; Optional customizations
    :custom
    (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    (corfu-auto t)                 ;; Enable auto completion
    (corfu-separator ?\s)          ;; Orderless field separator
    (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
    (corfu-scroll-margin 5)        ;; Use scroll margin

    ;; Enable Corfu only for certain modes.
    :hook ((prog-mode . corfu-mode)
           (shell-mode . corfu-mode)
           (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.
    ;; This is recommended since Dabbrev can be used globally (M-/).
    ;; See also `corfu-excluded-modes'.
    :init
    (global-corfu-mode)
    (setq corfu-auto t))
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)  ;; corfu-previous

  (use-package kind-icon
    :ensure t
    :after corfu
    :custom
    (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))
