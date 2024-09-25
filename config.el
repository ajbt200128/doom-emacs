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


(add-to-list 'default-frame-alist '(undecorated-round . t))

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

(setq vterm-timer-delay 0.001)
(setq +format-on-save-disabled-modes (add-to-list '+format-on-save-disabled-modes 'dune-mode))

;;
;; Package stuff
;;
(use-package! org
  :config
  (setq org-directory "~/org/"
        org-default-notes-file "~/org/agenda/refile.org"
        org-capture-templates
        '(("t" "TODO" entry (file "~/org/agenda/refile.org")
           "* TODO %? %^G\n%T")))
  (setq org-agenda-files '("~/org/agenda")
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil ;; i.e. today
        org-agenda-span 1
        org-agenda-start-on-weekday nil)
  (setq org-latex-src-block-backend 'minted
        org-latex-custom-lang-environments
        '((emacs-lisp "common-lispcode"))
        org-latex-minted-options
        '(("frame" "single")
          ("fontsize" "\\scriptsize")
          ("linenos" ""))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '(
                            (:name "Today - Stretch"
                             :and (:tag "today" :tag "stretch")
                             :order 2)
                            (:name "This Week - Stretch"
                             :and (:tag "thisweek" :tag "stretch")
                             :order 4)
                            (:name "Today"
                             :date today
                             :tag "today"
                             :order 1)
                            (:name "This Week"
                             :tag "thisweek"
                             :order 3)
                            (:name "Anytime"
                             :tag "anytime"
                             :order 5)
                            (:discard (:anything))))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "To refile"
                             :file-path "refile\\.org"
                             :order 5)
                            (:name "Due Today"
                             :deadline today
                             :order 6)
                            (:name "Due Soon"
                             :deadline future
                             :order 7)
                            (:name "Overdue"
                             :deadline past
                             :order 8)
                            (:discard (:anything))))))))))
  :config
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (org-save-all-org-buffers)))
  (map! :leader :desc "Agenda View" :n "a" #'(lambda (&rest _) (interactive)
                                               (org-agenda nil "c")))
  (org-super-agenda-mode))


(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind
  (("C-TAB" . 'copilot-accept-completion-by-word)
   ("C-<tab>" . 'copilot-accept-completion-by-word)
   :map copilot-completion-map
   ("<tab>" . 'copilot-accept-completion)
   ("TAB" . 'copilot-accept-completion))
  :config
  (setq copilot--indentation-alist
        '((tuareg-mode 2)
          (emacs-lisp-mode 2)))
  (map! :leader :desc "Open very large file with VLF" :n "f v" #'vlf))

(use-package! vlf
  :config
  (setq vlf-application 'always))

(use-package! lsp-mode
  :config
  (setq lsp-progress-function 'lsp-on-progress-legacy
        lsp-disabled-clients '((tuareg-mode . semgrep-ls))
        lsp-semgrep-scan-jobs 10
        lsp-rust-features "all"))

(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package! poetry
  :config
  (setq poetry-tracking-strategy 'projectile))

(use-package! dap-mode
  :hook (lsp-mode . dap-mode)
  :config
  (setq dap-auto-configure-features '(sessions locals breakpoints))
  (map! :leader
        (:prefix ("d" . "debug")
         :desc "DAP Debug" "d" #'dap-debug
         :desc "DAP Disconnect" "D" #'dap-disconnect
         :desc "DAP Toggle Breakpoint" "b" #'dap-breakpoint-toggle
         :desc "DAP Step In" "i" #'dap-step-in
         :desc "DAP Step Out" "O" #'dap-step-out
         :desc "DAP Step Over" "o" #'dap-next
         :desc "DAP Restart" "r" #'dap-debug-restart
         :desc "DAP Debug Last" "l" #'dap-debug-last
         :desc "DAP Continue" "c" #'dap-continue
         :desc "DAP Breakpoint Condition" "C" #'dap-breakpoint-condition
         :desc "DAP UI Show" "u" #'dap-ui-show-many-windows
         :desc "DAP UI Hide" "U" #'dap-ui-hide-many-windows))
  (dap-ui-controls-mode 0))

(use-package! jsonnet-mode
  :defer t
  :config
  (set-electric! 'jsonnet-mode :chars '(?\n ?: ?{ ?})))

(use-package! ace-window
  :config
  (setq aw-background nil)
  (map! :leader
        (:prefix ("w" . "window")
         :desc "Next Frame" "]" #'+evil/next-frame
         :desc "Previous Frame" "[" #'+evil/previous-frame
         :desc "Ace Window" "a" #'ace-window))
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
  (ace-window-display-mode))

(use-package! vundo
  :config
  (map! (:mode 'vundo-mode
         :n "l" #'vundo-forward
         :n "l" #'vundo-forward
         :n "<right>" #'vundo-forward
         :n "h" #'vundo-backward
         :n "<left>" #'vundo-backward
         :n "j" #'vundo-next
         :n "<down>" #'vundo-next
         :n "k" #'vundo-previous
         :n "<up>" #'vundo-previous
         :n "<home>" #'vundo-stem-root
         :n "<end>" #'vundo-stem-end
         :n "q" #'vundo-quit
         :n "C-g" #'vundo-quit
         :n "RET" #'vundo-confirm))
  (map! :leader :desc "Visualize undo tree" :n "s u" #'vundo))

;;
;; after/hooks/conditions
;;

(after! doom-themes-ext-treemacs
  (with-eval-after-load 'treemacs
    (remove-hook 'treemacs-mode-hook #'doom-themes-hide-fringes-maybe)
    (advice-remove #'treemacs-select-window #'doom-themes-hide-fringes-maybe)))

(after! dap-mode
  (advice-add 'dap-ui-controls-mode :override #'ignore))

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



;; kitty
(map! :leader
      (:prefix ("k" . "kitty")
       :desc "Run a command in Kitty" "r" #'+kitty/run
       :desc "Run region as command in Kitty" "R" #'+kitty/send-region
       :desc "Rerun a command in Kitty" "k" #'+kitty/rerun
       :desc "Run a a command in a new Kitty window" "n" #'+kitty/run-in-new-window
       :desc "Run a command in a new Kitty tab" "t" #'+kitty/run-in-new-tab
       :desc "Run a command in a new Kitty OS window" "o" #'+kitty/run-in-new-os-window
       :desc "Launch a new Kitty window" "N" #'+kitty/new-window
       :desc "Launch a new Kitty tab" "T" #'+kitty/new-window
       :desc "Launch a new Kitty OS window" "O" #'+kitty/launch
       :desc "CD to a directory in Kitty" "c" #'+kitty/cd
       :desc "CD to current directory in Kitty" "d" #'+kitty/cd-to-here
       :desc "CD to project root in Kitty" "p" #'+kitty/cd-to-project))

;; gpg
(map! :leader :desc "Sign region" :n "S" #'gpg-sign-comment)

;; counsel
(map! :leader :desc "Ripgrep current directory" :n "s s" #'counsel-rg)

;; projectile
(map! :leader :desc "Projectile find and replace" :n "p R" #'projectile-replace)

;; format
(map! :leader :desc "Format buffer" :n "b f" #'+format/buffer)

;; treemacs
(map! :leader :desc "Treemacs" :n "f t" #'treemacs)
