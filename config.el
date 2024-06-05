;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Austin Theriault"
      user-mail-address "austin@cutedogs.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-city-lights)

(setq doom-big-font-increment 3)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(add-to-list 'default-frame-alist '(undecorated-round . t))

(setq org-latex-src-block-backend 'minted)
(setq org-latex-custom-lang-environments
      '(
        (emacs-lisp "common-lispcode")
        ))

(setq org-latex-minted-options
      '(("frame" "single")
        ("fontsize" "\\scriptsize")
        ("linenos" "")))

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; accept completion from copilot and fallback to company

;;
;; General settings
;;

(setq ocaml-indent-level 2)
(setq elisp-indent-level 2)
(setq vterm-timer-delay 0.001)

;;
;; Package stuff
;;
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion))
  :config (setq copilot--indentation-alist '(
                                             (tuareg-mode ocaml-indent-level)
                                             (emacs-lisp-mode elisp-indent-level))))

(use-package! vlf
  :config
  (setq vlf-application 'always))

(use-package! lsp-mode
  :config
  (setq lsp-progress-function 'lsp-on-progress-legacy)
  (setq lsp-disabled-clients '((tuareg-mode . semgrep-ls)))
  (setq lsp-semgrep-scan-jobs 10)
  (setq lsp-rust-features "all"))

(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package! tuareg
  :init (require 'dap-ocaml))

(use-package! dap-mode
  :hook (lsp-mode . dap-mode)
  :config
  (setq dap-auto-configure-features '(sessions locals breakpoints))
  (dap-ui-controls-mode 0))

(use-package! jsonnet-mode
  :defer t
  :config
  (set-electric! 'jsonnet-mode :chars '(?\n ?: ?{ ?})))

(use-package! ace-window
  :config
  (setq aw-background nil))

;;
;; after/hooks/conditions
;;

(after! doom-themes-ext-treemacs
  (with-eval-after-load 'treemacs
    (remove-hook 'treemacs-mode-hook #'doom-themes-hide-fringes-maybe)
    (advice-remove #'treemacs-select-window #'doom-themes-hide-fringes-maybe)))

(after! dap-mode
  (advice-add 'dap-ui-controls-mode :override #'ignore))

(add-hook! 'prog-mode-hook 'ace-window-display-mode)
(add-hook! 'prog-mode-hook 'auto-fill-mode)

(add-hook! org-mode 'org-fragtog-mode)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; File templates
(set-file-template! "\\.ml$" :trigger "__.ml" :mode 'tuareg-mode)

;;
;; custom functions
;;

(defun gpg-sign-string (message)
  (shell-command-to-string
   (format "echo \"%s\" | gpg --clearsign -o-" message))
  )

(defun gpg-sign-comment (start end)
  (interactive "r")
  (let ((comment (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert (gpg-sign-string comment)))
  )


;;
;; keybindings
;;

;; window switching
(mapc (lambda (x)
        (let ((key (car x))
              (val (cdr x)))
          (map! :leader :desc (format "Switch to window ") :n (format "w %s" key)
                (lambda ()
                  (interactive)
                  (let ((wnd (nth val (aw-window-list))))
                    (when wnd
                      (select-window wnd)
                      (select-frame-set-input-focus (selected-frame))))))))
      '(("1" . 0) ("2" . 1) ("3" . 2) ("4" . 3) ("5" . 4)
        ("6" . 5) ("7" . 6) ("8" . 7) ("9" . 8)))


;; kitty
(map! :leader :desc "Run a command in Kitty" :n "k r" #'+kitty/run)
(map! :leader :desc "Run region as command in Kitty" :n "k R" #'+kitty/send-region)
(map! :leader :desc "Rerun a command in Kitty" :n "k k" #'+kitty/rerun)
(map! :leader :desc "Run a a command in a new Kitty window" :n "k n" #'+kitty/run-in-new-window)
(map! :leader :desc "Run a command in a new Kitty tab" :n "k t" #'+kitty/run-in-new-tab)
(map! :leader :desc "Run a command in a new Kitty OS window" :n "k o" #'+kitty/run-in-new-os-window)
(map! :leader :desc "Launch a new Kitty window" :n "k N" #'+kitty/new-window)
(map! :leader :desc "Launch a new Kitty tab" :n "k T" #'+kitty/new-window)
(map! :leader :desc "Launch a new Kitty OS window" :n "k O" #'+kitty/launch)
(map! :leader :desc "CD to a directory in Kitty" :n "k c" #'+kitty/cd)
(map! :leader :desc "CD to current directory in Kitty" :n "k d" #'+kitty/cd-to-here)
(map! :leader :desc "CD to project root in Kitty" :n "k p" #'+kitty/cd-to-project)

;; ace
(map! :leader :desc "Next Frame" :n "w ]" #'+evil/next-frame)
(map! :leader :desc "Previous Frame" :n "w [" #'+evil/previous-frame)
(map! :leader :desc "Ace Window" :n "w a" #'ace-window)

;; dap
(map! :leader :desc "DAP Debug" :n "d d" #'dap-debug)
(map! :leader :desc "DAP Disconnect" :n "d D" #'dap-disconnect)
(map! :leader :desc "DAP Toggle Breakpoint" :n "d b" #'dap-breakpoint-toggle)
(map! :leader :desc "DAP Step In" :n "d i" #'dap-step-in)
(map! :leader :desc "DAP Step Out" :n "d O" #'dap-step-out)
(map! :leader :desc "DAP Step Over" :n "d o" #'dap-next)
(map! :leader :desc "DAP Restart" :n "d r" #'dap-debug-restart)
(map! :leader :desc "DAP Debug Last" :n "d l" #'dap-debug-last)
(map! :leader :desc "DAP Continue" :n "d c" #'dap-continue)
(map! :leader :desc "DAP Breakpoint Condition" :n "d C" #'dap-breakpoint-condition)
(map! :leader :desc "DAP UI Show" :n "d u" #'dap-ui-show-many-windows)
(map! :leader :desc "DAP UI Hide" :n "d U" #'dap-ui-hide-many-windows)

;; copilot
(map! :mode copilot-mode "<backtab>" #'copilot-accept-completion)

;; gpg
(map! :leader :desc "Sign region" :n "S" #'gpg-sign-comment)

;; vlf
(map! :leader :desc "Open very large file with VLF" :n "f v" #'vlf)

;; counsel
(map! :leader :desc "Ripgrep current directory" :n "s s" #'counsel-rg)
;; projectile
(map! :leader :desc "Projectile find and replace" :n "p R" #'projectile-replace)

;; format
(map! :leader :desc "Format buffer" :n "b f" #'+format/buffer)

;; treemacs
(map! :leader :desc "Treemacs" :n "f t" #'treemacs)
