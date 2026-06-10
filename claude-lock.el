;;; claude-lock.el -*- lexical-binding: t; -*-

;; Tell Claude Code which files I'm in the middle of editing. A file counts as
;; locked while a modified (unsaved) buffer visits it; Claude's PreToolUse hook
;; asks this Emacs live over emacsclient (see claude-lock-wait.sh) and waits
;; instead of editing under me. Nothing is stored -- it's read straight off the
;; live buffer list on demand.

(require 'subr-x)

(defgroup claude-lock nil
  "Make Claude Code wait on files with unsaved Emacs buffers."
  :group 'tools)

(defcustom claude-lock-script (expand-file-name "~/.claude/hooks/claude-lock-wait.sh")
  "Where the PreToolUse hook script is symlinked for Claude Code to run."
  :type 'file
  :group 'claude-lock)

(defvar claude-lock--source
  (expand-file-name "claude-lock-wait.sh"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "In-repo source of the hook script, alongside this file.")

(defun claude-lock--modified-files ()
  "Paths (truename and expanded) of all modified file-visiting buffers."
  (let (paths)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name (buffer-modified-p))
          (push (file-truename buffer-file-name) paths)
          (push (expand-file-name buffer-file-name) paths))))
    (delete-dups paths)))

(defun claude-lock-locked-p (path)
  "Non-nil if PATH is visited by a modified (unsaved) buffer."
  (let ((locked (claude-lock--modified-files)))
    (and (or (member (expand-file-name path) locked)
             (member (file-truename path) locked))
         t)))

(defun claude-lock--json-target (jsonfile)
  "Read JSONFILE of hook input and return its tool target path, or nil."
  (ignore-errors
    (let* ((obj (json-parse-string
                 (with-temp-buffer (insert-file-contents jsonfile) (buffer-string))))
           (input (and (hash-table-p obj) (gethash "tool_input" obj))))
      (and (hash-table-p input)
           (or (gethash "file_path" input) (gethash "notebook_path" input))))))

(defun claude-lock-hook-status (jsonfile)
  "Return `locked' if the tool target in JSONFILE is locked, else `allow'.
Called by the PreToolUse hook over emacsclient."
  (let ((path (claude-lock--json-target jsonfile)))
    (if (and path (claude-lock-locked-p path)) 'locked 'allow)))

(defun claude-lock-hook-target (jsonfile)
  "Return the tool target path string from JSONFILE, or empty string."
  (or (claude-lock--json-target jsonfile) ""))

(defun claude-lock-list ()
  "Echo the files currently locked by unsaved buffers."
  (interactive)
  (let (files)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name (buffer-modified-p))
          (push (abbreviate-file-name buffer-file-name) files))))
    (setq files (sort (delete-dups files) #'string<))
    (message (if files (string-join files "\n") "No files locked."))))

(defun claude-lock-install-hook ()
  "Symlink the in-repo hook script to `claude-lock-script' for Claude Code."
  (interactive)
  (unless (file-exists-p claude-lock--source)
    (user-error "Hook script not found: %s" claude-lock--source))
  (set-file-modes claude-lock--source #o755)
  (make-directory (file-name-directory claude-lock-script) t)
  (cond
   ((file-symlink-p claude-lock-script) (delete-file claude-lock-script))
   ((file-exists-p claude-lock-script)
    (if (yes-or-no-p (format "%s exists and is not a symlink; replace it? "
                             claude-lock-script))
        (delete-file claude-lock-script)
      (user-error "Aborted; left %s in place" claude-lock-script))))
  (make-symbolic-link claude-lock--source claude-lock-script t)
  (message "Linked %s -> %s" claude-lock-script claude-lock--source))

(provide 'claude-lock)
;;; claude-lock.el ends here
