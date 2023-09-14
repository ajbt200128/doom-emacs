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

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion))
  :config (setq copilot-node-executable "~/.nvm/versions/node/v17.9.1/bin/node"))

(add-hook! org-mode 'org-fragtog-mode)

(after! lsp-mode
  (setq lsp-progress-function 'lsp-on-progress-legacy)
  (setq lsp-semgrep-trace-server "verbose"))

(setq vterm-timer-delay 0.0001)

(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

(add-hook 'tuareg-mode-hook
          (lambda()
            (setq-local comment-style 'multi-line)
            ))

(use-package! dap-mode
  :hook (lsp-mode . dap-mode)
  :config (setq dap-auto-configure-features '(sessions locals breakpoints expressions)))

(after! doom-themes-ext-treemacs
  (with-eval-after-load 'treemacs
    (remove-hook 'treemacs-mode-hook #'doom-themes-hide-fringes-maybe)
    (advice-remove #'treemacs-select-window #'doom-themes-hide-fringes-maybe)))

;; File templates
(set-file-template! "\\.ml$" :trigger "__.ml" :mode 'tuareg-mode)

(add-hook! 'prog-mode-hook 'ace-window-display-mode)
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

(setq aw-background nil)

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

(map! :leader :desc "Next Frame" :n "w ]" #'+evil/next-frame)
(map! :leader :desc "Previous Frame" :n "w [" #'+evil/previous-frame)
(map! :leader :desc "Ace Window" :n "w a" #'ace-window)

(map! :mode copilot-mode "<backtab>" #'copilot-accept-completion)
