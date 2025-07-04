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
        org-default-notes-file "~/org/agenda/todo.org"
        org-log-into-drawer t
        org-log-done 'time
        org-capture-templates
        '(("t" "TODO" entry (file+headline "~/org/agenda/todo.org" "Anytime")
           "* TODO %^{Headline}\n%T\n%?")
          ("d" "DONE" entry (file+headline "~/org/agenda/todo.org" "Today")
           "* DONE %^{Headline}\nCLOSED: %U\n%T\n%?")
          ("m" "MTNG" entry (file "~/org/agenda/meeting.org")
           "* MTNG %T %^{Headline}\n** People\n * %?\n** Notes\n*** \n* Action Items\n - [ ] ")
          ("i" "IDEA" entry (file "~/org/agenda/ideas.org")
           "* IDEA %^{Headline}\n%T\n%?"))
        org-todo-keywords '((sequence "TODO(t!)" "STRT(s!)" "WAIT(w@/!)" "IDEA(i)"
                             "|" "DONE(d!)" "CANCELLED(c@)" "REVISIT(r@)" "MTNG(m)")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                            (sequence "|" "YES(y)" "NO(n)"))
        org-todo-keyword-faces (append org-todo-keyword-faces '(("IDEA" . (:foreground "blue" :weight bold))
                                                                ("MTNG" . (:foreground "purple" :weight bold))
                                                                ("CANCELLED" . (:foreground "orange" :weight bold)))))
  (setq org-agenda-files '("~/org/agenda")
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day nil ;; i.e. today
        org-agenda-span 1
        org-agenda-start-on-weekday nil)
  (map! :mode 'org-mode :leader :desc "org-add-note" :n "m N" #'org-add-note)
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
  (map! :leader :desc "Agenda View" :n "a"
        #'(lambda (&rest _)
            (interactive)
            (org-agenda nil "c")))
  (let ((org-save-all (list
                       'org-refile
                       'org-todo
                       'org-archive-subtree
                       'org-archive-subtree-default)))
    (dolist (fn org-save-all)
      (advice-add fn :after
                  (lambda (&rest _)
                    (org-save-all-org-buffers)))))
  (org-super-agenda-mode))


(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(tuareg-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(use-package! vlf
  :config
  (setq vlf-application 'always)
  (map! :leader :desc "Open very large file with VLF" :n "f v" #'vlf)
  (map! (:mode 'vlf-mode
         :leader
         :desc "VLF next" :n "m n" #'vlf-next-batch
         :desc "VLF previous" :n "m p" #'vlf-prev-batch)
        )

  )

(use-package! jinx
  :hook (emacs-startup . global-jinx-mode)
  :config
  (map!
   :desc "jinx-correct" :n "z =" #'jinx-correct
   :desc "jinx-correct-all" :n "z S" #'jinx-correct-all
   )
  )

(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

(defun whoami ()
  (shell-command-to-string "whoami"))

(defun projectile-add-known-projects (dirs)
  (dolist (dir dirs)
    (projectile-add-known-project dir))
  )

(use-package! poetry
  :config
  (setq poetry-tracking-strategy 'projectile))

;; TODO DAPE mode
;; (use-package! dap-mode
;;   :config
;;   (setq dap-auto-configure-features '(sessions locals breakpoints))
;;   (map! :leader
;;         (:prefix ("d" . "debug")
;;          :desc "DAP Debug" "d" #'dap-debug
;;          :desc "DAP Disconnect" "D" #'dap-disconnect
;;          :desc "DAP Toggle Breakpoint" "b" #'dap-breakpoint-toggle
;;          :desc "DAP Step In" "i" #'dap-step-in
;;          :desc "DAP Step Out" "O" #'dap-step-out
;;          :desc "DAP Step Over" "o" #'dap-next
;;          :desc "DAP Restart" "r" #'dap-debug-restart
;;          :desc "DAP Debug Last" "l" #'dap-debug-last
;;          :desc "DAP Continue" "c" #'dap-continue
;;          :desc "DAP Breakpoint Condition" "C" #'dap-breakpoint-condition
;;          :desc "DAP UI Show" "u" #'dap-ui-show-many-windows
;;          :desc "DAP UI Hide" "U" #'dap-ui-hide-many-windows))
;;   (dap-ui-controls-mode 0))

(use-package! apheleia
  :config
  (push '(jsonnet-fmt . (jsonnet-fmt-command "-")) apheleia-formatters)
  (setf (alist-get 'jsonnet-mode apheleia-mode-alist) 'jsonnet-fmt))

(use-package! jsonnet-mode
  :defer t
  :config
  (set-electric! 'jsonnet-mode :chars '(?\n ?: ?{ ?})))

(use-package! consult
  :config
  (map! :leader
        :nv "P" #'yank-from-kill-ring
        :n "c e" #'consult-flycheck
        :n "M" #'consult-mode-command
        :n "s c" #'consult-tramp))

;; Configure vetico mouse extension.
(use-package! vertico-mouse
  :after vertico
  :config
  ;; Enable vertico-mouse
  (vertico-mouse-mode))

;; Configure vertico directory extension.
(use-package! vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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

(use-package! undo-tree
  :config
  (defun +undo-tree-escape-hook nil "Exit undo tree"
         (if (eq major-mode 'undo-tree-visualizer-mode)
             (undo-tree-visualizer-quit))
         )
  :hook (doom-escape-hook . +undo-tree-escape-hook))

(use-package! git-link
  :config
  (map! :leader :desc "Copy git link" :n "g d" #'git-link-dispatch))

(use-package! vterm-toggle
  :after vterm
  :config
  (map! :leader :desc "Toggle vterm" :n "o t" #'vterm-toggle-cd))

(use-package! eglot
  :config
  (setq eglot-autoshutdown t))

(use-package! eglot-booster
  :after eglot
  :config
  (eglot-booster-mode))


(use-package! eldoc-box
  :hook
  (unless (display-graphic-p)
    (eglot-managed-mode-hook . eldoc-box-hover-mode)))

(use-package! flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

(use-package! ocaml-eglot
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))

(use-package! magit
  :config
  (transient-define-suffix magit-submodule-update-all (args)
    :class 'magit--git-submodule-suffix
    :description "Update all         git submodule update [--force] [--no-fetch]
                     [--remote] [--recursive] [--checkout|--rebase|--merge]"
    (interactive
     (list
      (magit-submodule-arguments
       "--force" "--remote" "--recursive" "--checkout" "--rebase" "--merge"
       "--no-fetch")))
    (magit-with-toplevel
      (magit-run-git-async "submodule" "update" args)))
  (transient-insert-suffix 'magit-submodule "l" '("U" magit-submodule-update-all)))

(use-package! magit-gt
  :config
  (map! :mode 'magit-status-mode :desc "Magit Graphite" :n "w" #'magit-gt))

(message "%s" (transient-get-suffix 'magit-submodule "f"))
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

;; File templates
(set-file-template! "\\.ml$" :trigger "__.ml" :mode 'tuareg-mode)


;;
;; custom functions
;;

(defun gpg-sign-string (message)
  (shell-command-to-string
   (format "echo \"%s\" | gpg --clearsign -o-" message)))

(defun gpg-sign-comment (start end)
  (interactive "r")
  (let ((comment (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert (gpg-sign-string comment))))


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

;; projectile
(map! :leader :desc "Projectile find and replace" :n "p R" #'projectile-replace)

;; format
(map! :leader :desc "Format buffer" :n "b f" #'+format/buffer)
