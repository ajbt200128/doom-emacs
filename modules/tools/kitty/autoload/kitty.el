;;; tools/kitty/autoload/kitty.el -*- lexical-binding: t; -*-

;; Inspired by :tools tmux

(defvar +kitty-history nil
  "The shell history.")

(defvar +kitty-last-retcode nil
  "The last kitty return code.")

(defcustom +kitty-history-file "~/.zsh_history"
  "The file to read shell history from."
  :type 'string
  :group 'kitty)

(defun last-shell-history-items (&optional file limit)
  "Get the last shell history items from FILE."
  (let* ((file (or file +kitty-history-file))
         (lines (split-string (f-read file) "\n" t))
         (items (reverse (seq-filter
                          (lambda (line)
                            (and
                             (not (string-empty-p line))
                             (not (string-match "^: " line))))
                          lines))))
    (append
     +kitty-history
     (if limit
         (seq-take items limit)
       items))))

(defun +kitty-completing-read (prompt &optional initial limit)
  "Read a command from the shell history."
  (let ((candidates (last-shell-history-items nil limit)))
    (list
     (consult--read
      candidates
      :prompt prompt
      :initial initial
      :sort nil
      :default (car candidates)
      :require-match nil
      ))))
;;
;; Commands

;;;###autoload
(defun +kitty (command &rest args)
  "Execute COMMAND in kitty"
  (let ((bin (executable-find "kitty")))
    (unless bin
      (error "Could not find kitty executable"))
    (let ((socket (car (f-glob "/tmp/mykitty*"))))
      (let* ((args (mapcar #'shell-quote-argument (delq nil args)))
             (cmdstr (format "%s @ --to unix:%s %s" bin socket (if args (apply #'format command args) command)))
             (output (get-buffer-create " *kitty stdout*"))
             (errors (get-buffer-create " *kitty stderr*"))
             code)
        (unwind-protect
            (if (= 0 (setq code (quiet! (shell-command cmdstr output errors))))
                (with-current-buffer output
                  (buffer-string))
              (error "[%d] kitty $ %s (%s)"
                     code
                     (with-current-buffer errors
                       (buffer-string))
                     cmdstr))
          (and (kill-buffer output)
               (kill-buffer errors)))))))

;;;###autoload
(defun +kitty/run (command &optional noreturn)
  "Run COMMAND in kitty. If NORETURN is non-nil, send the commands as keypresses
but do not execute them."
  (interactive
   (+kitty-completing-read "run:"))
  (add-to-list '+kitty-history command)
  (+kitty (concat "send-text "
                  (shell-quote-argument command)
                  (unless noreturn "'\n'"))))

;;;###autoload
(defun +kitty/send-region (beg end &optional noreturn)
  "Send region to kitty."
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (+kitty/run (string-trim (buffer-substring-no-properties beg end))
              noreturn))

;;;###autoload
(defun +kitty/rerun (lastc)
  "Rerun the last command executed by `+kitty' and `+kitty/run'."
  (interactive
   (+kitty-completing-read "rerun:" (car +kitty-history)))
  (unless lastc
    (user-error "No last command to run"))
  (apply #'+kitty/run lastc))

;;;###autoload
(defun +kitty/cd (&optional directory )
  "Change the pwd of the currently active kitty pane to DIRECTORY.

DIRECTORY defaults to `default-directory' if omitted, or to `doom-project-root'
if prefix arg is non-nil.

If NORETURN is non-nil, send the cd command to kitty, but do not execute the
command."
  (interactive (let ((default-directory (or (doom-project-root) default-directory)))
                 (find-file-read-args "cd: " nil)))
  (+kitty/run (format "cd %s" (or directory (if current-prefix-arg
                                                (doom-project-root)
                                              default-directory)))))

;;;###autoload
(defun +kitty/cd-to-here ()
  "cd into `default-directory' in kitty."
  (interactive)
  (+kitty/cd default-directory))

;;;###autoload
(defun +kitty/cd-to-project ()
  "cd into `doom-project-root' in kitty."
  (interactive)
  (+kitty/cd (doom-project-root)))

;;;###autoload
(defun +kitty/launch(&optional title type)
  "launch a new kitty window with optional TITLE, with TYPE"
  (interactive "s[?]Title: ")
  (let ((type (or type "os-window")))
    (+kitty (concat (format "launch --type=%s " type)
                    (unless (string-empty-p title)
                      (format "--title=%s" (shell-quote-argument title)))))))

;;;###autoload
(defun +kitty/new-window (&optional title)
  "launch a new kitty window with optional TITLE."
  (interactive "s[?]Title: ")
  (+kitty/launch title "window"))

;;;###autoload
(defun +kitty/new-tab (&optional title)
  "launch a new kitty tab with optional TITLE."
  (interactive "s[?]Title: ")
  (+kitty/launch title "tab"))

;;;###autoload
(defun +kitty/run-in-new-window (command)
  "Run COMMAND in a new kitty window."
  (interactive "sCommand: ")
  (+kitty/new-window command)
  (+kitty/run command))

;;;###autoload
(defun +kitty/run-in-new-tab (command)
  "Run COMMAND in a new kitty tab."
  (interactive "sCommand: ")
  (+kitty/new-tab command)
  (+kitty/run command))

;;;###autoload
(defun +kitty/run-in-new-os-window (command)
  "Run COMMAND in a new kitty os-window."
  (interactive "sCommand: ")
  (+kitty/launch command)
  (+kitty/run command))


(defun get-windows ()
  "Get all kitty windows."
  (json-parse-string (+kitty "ls")))

(defun pp-process (process)
  "Pretty print a process."
  (mapconcat #'identity (gethash "cmdline" process) " "))

(defun pp-window (window)
  "Pretty print a window."
  (let ((id (gethash "id" window))
        (active (if (gethash "active" window) "[active]" ""))
        (focused (if (gethash "focused" window) "[focused]" ""))
        (title (gethash "title" window))
        (processes (mapcar #'pp-process (gethash "foreground_processes" window)))
        (cwd (gethash "cwd" window)))
    (format "(%d)%s%s %s [%s]\n       $ %s" id active focused title cwd
            (mapconcat #'identity processes "\n       $ "))
    )
  )

(defun pp-tab (tab)
  "Pretty print a tab."
  (let ((id (gethash "id" tab))
        (active (if (gethash "active" tab) "[active]" ""))
        (focused (if (gethash "focused" tab) "[focused]" ""))
        (title (gethash "title" tab))
        (windows (mapcar #'pp-window (gethash "windows" tab))))
    (format "(%d)%s%s %s\n    > %s" id active focused title
            (mapconcat #'identity windows "\n    > "))
    ))

(defun pp-os-window (window)
  "Pretty print an os-window."
  (let ((id (gethash "id" window))
        (active (if (gethash "active" window) "[active]" ""))
        (focused (if (gethash "focused" window) "[focused]" ""))
        (tabs (mapcar #'pp-tab (gethash "tabs" window))))
    (format "(%d)kitty%s%s\n | %s" id active focused
            (mapconcat #'identity tabs "\n | "))))

(defun pp-windows ()
  "Pretty print all kitty windows."
  (mapconcat #'pp-os-window (get-windows) "\n"))

;;;###autoload
(defun +kitty/list-windows ()
  "List all kitty windows."
  (interactive)
  (display-message-or-buffer (pp-windows)))
