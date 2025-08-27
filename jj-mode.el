;;; jj-mode.el -*- lexical-binding: t; -*-

(require 'magit-section)
(require 'transient)
(require 'ansi-color)

(defgroup jj nil
  "Interface to jj version control system."
  :group 'tools)

(defcustom jj-executable "jj"
  "Path to jj executable."
  :type 'string
  :group 'jj)

(defcustom jj-log-limit 50
  "Default number of commits to show in log."
  :type 'integer
  :group 'jj)

(defvar jj-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'jj-log-visit-commit)
    (define-key map (kbd "TAB") 'magit-section-toggle)
    (define-key map (kbd "g") 'jj-log-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "n") 'magit-section-forward)
    (define-key map (kbd "p") 'magit-section-backward)
    (define-key map (kbd "M-n") 'magit-section-forward-sibling)
    (define-key map (kbd "M-p") 'magit-section-backward-sibling)
    (define-key map (kbd "j") 'jj-goto-next-changeset)
    (define-key map (kbd "k") 'jj-goto-prev-changeset)
    (define-key map (kbd "c") 'jj-commit)
    (define-key map (kbd "C") 'jj-commit-amend)
    (define-key map (kbd "e") 'jj-edit-commit)
    (define-key map (kbd "s") 'jj-squash)
    (define-key map (kbd "S") 'jj-split)
    (define-key map (kbd "b") 'jj-branch-create)
    (define-key map (kbd "B") 'jj-branch-list)
    (define-key map (kbd "P") 'jj-git-push)
    (define-key map (kbd "F") 'jj-git-fetch)
    (define-key map (kbd "r") 'jj-rebase-transient)
    (define-key map (kbd "d") 'jj-diff)
    (define-key map (kbd "l") 'jj-log-limit)
    (define-key map (kbd "?") 'jj-help)
    (define-key map (kbd "x") 'jj-changeset-menu)
    (define-key map (kbd "v") 'jj-revert-hunk)
    (define-key map (kbd "V") 'jj-revert-file)
    (define-key map (kbd "a") 'jj-apply-hunk)
    map)
  "Keymap for `jj-mode'.")

(define-derived-mode jj-mode magit-section-mode "JJ"
  "Major mode for interacting with jj version control system."
  :group 'jj
  (setq-local revert-buffer-function 'jj-log-refresh)
  ;; Clear rebase selections when buffer is killed
  (add-hook 'kill-buffer-hook 'jj-rebase-clear-selections nil t))

(defun jj--run-command (&rest args)
  "Run jj command with ARGS and return output."
  (with-temp-buffer
    (apply #'call-process jj-executable nil t nil args)
    (buffer-string)))

(defun jj--run-command-color (&rest args)
  "Run jj command with ARGS and return colorized output."
  (with-temp-buffer
    (let ((process-environment (cons "FORCE_COLOR=1" (cons "CLICOLOR_FORCE=1" process-environment))))
      (apply #'call-process jj-executable nil t nil "--color=always" args))
    (ansi-color-apply (buffer-string))))

(defun jj--run-command-async (callback &rest args)
  "Run jj command with ARGS asynchronously and call CALLBACK with output."
  (let ((buffer (generate-new-buffer " *jj-async*")))
    (set-process-sentinel
     (apply #'start-process "jj" buffer jj-executable args)
     (lambda (process _event)
       (when (eq (process-status process) 'exit)
         (with-current-buffer (process-buffer process)
           (funcall callback (buffer-string)))
         (kill-buffer (process-buffer process)))))))

(defclass jj-commit-section (magit-section)
  ((commit-id :initarg :commit-id)
   (author :initarg :author)
   (date :initarg :date)
   (description :initarg :description)
   (branches :initarg :branches)))

(defclass jj-commits-section (magit-section) ())
(defclass jj-status-section (magit-section) ())
(defclass jj-diff-stat-section (magit-section) ())
(defclass jj-log-graph-section (magit-section) ())
(defclass jj-log-entry-section (magit-section)
  ((commit-id :initarg :commit-id)
   (description :initarg :description)
   (branches :initarg :branches)))
(defclass jj-diff-section (magit-section) ())
(defclass jj-file-section (magit-section)
  ((file :initarg :file)))
(defclass jj-hunk-section (magit-section)
  ((file :initarg :file)
   (start :initarg :hunk-start)
   (header :initarg :header)))

(defun jj--parse-log-line (line)
  "Parse a single jj log line."
  (when (string-match "^\\([a-z]+\\)\\s-+\\([^|]+\\)\\s-*|\\s-*\\(.*\\)$" line)
    (let* ((id (match-string 1 line))
           (info (match-string 2 line))
           (desc (match-string 3 line))
           (branches (when (string-match "\\(([^)]+)\\)" info)
                       (match-string 1 info)))
           (author-date (replace-regexp-in-string "\\s-*([^)]+)\\s-*" "" info)))
      (list :id id
            :info info
            :author-date author-date
            :description desc
            :branches branches))))

(defun jj-log-insert-commits ()
  "Insert jj log commits into current buffer."
  (let* ((log-output (jj--run-command "log" 
                                      "--no-graph"
                                      "-r" (format "ancestors(@, %d)" jj-log-limit)
                                      "--template" "change_id.shortest() ++ \" \" ++ if(branches, branches.join(\", \") ++ \" \", \"\") ++ author.name() ++ \" \" ++ author.timestamp().ago() ++ \" | \" ++ if(description, description.first_line(), \"(no description)\")"))
         (lines (split-string log-output "\n" t)))
    (when lines
      (magit-insert-section (jj-commits-section)
        (magit-insert-heading "Recent Commits")
        (dolist (line lines)
          (when-let ((commit-data (jj--parse-log-line line)))
            (magit-insert-section section (jj-commit-section)
                                  (oset section commit-id (plist-get commit-data :id))
                                  (oset section description (plist-get commit-data :description))
                                  (insert (propertize (format "%-8s" (plist-get commit-data :id))
                                                      'face 'magit-hash))
                                  (when (plist-get commit-data :branches)
                                    (insert (propertize (plist-get commit-data :branches)
                                                        'face 'magit-branch-local) " "))
                                  (insert (propertize (plist-get commit-data :author-date)
                                                      'face 'magit-log-author))
                                  (insert " ")
                                  (insert (propertize (plist-get commit-data :description)
                                                      'face 'magit-section-highlight))
                                  (insert "\n"))))))))

(defun jj--parse-log-graph-line (line)
  "Parse a line from jj log output to extract changeset info."
  ;; Look for lines with commit markers (circles) followed by change IDs
  (when (string-match "\\([○◉◆●◯◍@]\\)\\s-*\\([a-z][a-z0-9]+\\)" line)
    (let* ((marker (match-string 1 line))
           (id (match-string 2 line))
           (rest (substring line (match-end 0)))
           branches description)
      ;; Extract branches and description from the rest of the line
      (when (string-match "\\s-+\\(.*\\)" rest)
        (setq description (match-string 1 rest)))
      ;; Check if there are branch names at the beginning
      (when (and description (string-match "^\\(\\S-+\\)\\s-+\\(.*\\)" description))
        (let ((first-word (match-string 1 description)))
          (when (not (string-match "^[0-9]" first-word))
            (setq branches first-word
                  description (match-string 2 description)))))
      (when id
        (list :marker marker
              :id id
              :branches branches
              :description description
              :full-line line)))))

(defun jj-log-insert-logs ()
  "Insert jj log graph into current buffer."
  (let ((log-output (jj--run-command-color "log")))
    (when (and log-output (not (string-empty-p log-output)))
      (magit-insert-section (jj-log-graph-section)
        (magit-insert-heading "Log Graph")
        (let ((lines (split-string log-output "\n" t)))
          (dolist (line lines)
            (if-let ((changeset-data (jj--parse-log-graph-line line)))
                ;; This line represents a changeset
                (magit-insert-section section (jj-log-entry-section)
                                      (oset section commit-id (plist-get changeset-data :id))
                                      (oset section description (plist-get changeset-data :description))
                                      (oset section branches (plist-get changeset-data :branches))
                                      (insert line "\n"))
              ;; This is a graph line or other content
              (insert line "\n"))))
        (insert "\n")))))

(defun jj-log-insert-status ()
  "Insert jj status into current buffer."
  (let ((status-output (jj--run-command-color "status")))
    (when (and status-output (not (string-empty-p status-output)))
      (magit-insert-section (jj-status-section)
        (magit-insert-heading "Working Copy Status")
        (insert status-output)
        (insert "\n")))))

(defun jj-log-insert-diff ()
  "Insert jj diff with hunks into current buffer."
  (let ((diff-output (jj--run-command-color "diff")))
    (when (and diff-output (not (string-empty-p diff-output)))
      (magit-insert-section (jj-diff-section)
        (magit-insert-heading "Uncommitted Changes")
        (jj--insert-diff-hunks diff-output)
        (insert "\n")))))

(defun jj--insert-diff-hunks (diff-output)
  "Parse and insert diff output as navigable hunk sections."
  (let ((lines (split-string diff-output "\n"))
        current-file
        file-section-content
        in-file-section)
    (dolist (line lines)
      (cond
       ;; File header
       ((string-match "^diff --git a/\\(.*\\) b/\\(.*\\)$" line)
        ;; Process any pending file section
        (when (and in-file-section current-file)
          (jj--insert-file-section current-file file-section-content))
        ;; Start new file section
        (setq current-file (or (match-string 2 line) (match-string 1 line))
              file-section-content (list line)
              in-file-section t))
       ;; Accumulate lines for current file section
       (in-file-section
        (push line file-section-content))
       ;; Outside of any file section
       (t nil)))
    ;; Process final file section if any
    (when (and in-file-section current-file)
      (jj--insert-file-section current-file file-section-content))))

(defun jj--insert-file-section (file lines)
  "Insert a file section with its hunks."
  (magit-insert-section file-section (jj-file-section)
                        (oset file-section file file)
                        (insert (propertize (concat "modified   " file "\n")
                                            'face 'magit-filename))
                        ;; Process the lines to find and insert hunks
                        (let ((remaining-lines (nreverse lines))
                              hunk-lines
                              in-hunk)
                          (dolist (line remaining-lines)
                            (cond
                             ;; Start of a hunk
                             ((string-match "^@@.*@@" line)
                              ;; Insert previous hunk if any
                              (when in-hunk
                                (jj--insert-hunk-lines file (nreverse hunk-lines)))
                              ;; Start new hunk
                              (setq hunk-lines (list line)
                                    in-hunk t))
                             ;; Skip header lines
                             ((string-match "^\\(diff --git\\|index\\|---\\|\\+\\+\\+\\|new file\\|deleted file\\)" line)
                              nil)
                             ;; Accumulate hunk lines
                             (in-hunk
                              (push line hunk-lines))))
                          ;; Insert final hunk if any
                          (when in-hunk
                            (jj--insert-hunk-lines file (nreverse hunk-lines))))))

(defun jj--insert-hunk-lines (file lines)
  "Insert a hunk section from LINES."
  (when lines
    (let ((header-line (car lines)))
      (when (string-match "^\\(@@.*@@\\)\\(.*\\)$" header-line)
        (let ((header (match-string 1 header-line))
              (context (match-string 2 header-line)))
          (magit-insert-section hunk-section (jj-hunk-section)
                                (oset hunk-section file file)
                                (oset hunk-section header header)
                                ;; Insert the hunk header
                                (insert (propertize header 'face 'magit-diff-hunk-heading))
                                (when (and context (not (string-empty-p context)))
                                  (insert (propertize context 'face 'magit-diff-hunk-heading)))
                                (insert "\n")
                                ;; Insert the hunk content
                                (dolist (line (cdr lines))
                                  (cond
                                   ((string-prefix-p "+" line)
                                    (insert (propertize line 'face 'magit-diff-added) "\n"))
                                   ((string-prefix-p "-" line)
                                    (insert (propertize line 'face 'magit-diff-removed) "\n"))
                                   (t
                                    (insert (propertize line 'face 'magit-diff-context) "\n"))))))))))

;;;###autoload
(defun jj-log ()
  "Display jj log in a magit-style buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*jj-log*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (jj-mode)
        (magit-insert-section (jjbuf)  ; Root section wrapper
          (jj-log-insert-logs)
          (jj-log-insert-status)
          (jj-log-insert-diff)
          (jj-log-insert-commits))
        (goto-char (point-min))))
    (switch-to-buffer buffer)))

(defun jj-log-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the jj log buffer."
  (interactive)
  (when (derived-mode-p 'jj-mode)
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (magit-insert-section (jjbuf)  ; Root section wrapper
        (jj-log-insert-logs)
        (jj-log-insert-status)
        (jj-log-insert-diff)
        (jj-log-insert-commits))
      (goto-char pos))))

(defun jj-log-visit-commit ()
  "Show details of commit at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (commit-id (or (and (eq (oref section type) 'jj-commit-section)
                                  (oref section commit-id))
                             (and (eq (oref section type) 'jj-log-entry-section)
                                  (oref section commit-id)))))
    (let ((buffer (get-buffer-create (format "*jj-commit-%s*" commit-id))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (jj--run-command-color "show" "-r" commit-id))
          (diff-mode)
          (ansi-color-apply-on-region (point-min) (point-max))
          (goto-char (point-min))))
      (display-buffer buffer))))

(defun jj-commit (&optional amend)
  "Create a new commit. With AMEND, amend the current commit."
  (interactive "P")
  (let ((desc (read-string (if amend "Amend message: " "Commit message: "))))
    (if amend
        (jj--run-command "describe" "-m" desc)
      (jj--run-command "commit" "-m" desc)))
  (jj-log-refresh))

(defun jj-commit-amend ()
  "Amend the current commit."
  (interactive)
  (jj-commit t))

(defun jj-edit-commit ()
  "Edit commit at point."
  (interactive)
  (when-let ((commit-id (jj-get-changeset-at-point)))
    (jj--run-command "edit" commit-id)
    (jj-log-refresh)
    (message "Now editing commit %s" commit-id)))

(defun jj-squash ()
  "Squash commits."
  (interactive)
  (when-let ((commit-id (jj-get-changeset-at-point)))
    (jj--run-command "squash" "-r" commit-id)
    (jj-log-refresh)
    (message "Squashed commit %s" commit-id)))

(defun jj-split ()
  "Split commit at point or current commit."
  (interactive)
  (if-let ((commit-id (jj-get-changeset-at-point)))
      (progn
        (jj--run-command "split" "-r" commit-id)
        (jj-log-refresh)
        (message "Split commit %s" commit-id))
    ;; If no commit at point, split current working copy
    (jj--run-command "split")
    (jj-log-refresh)
    (message "Split current commit")))

(defun jj-branch-create ()
  "Create a new branch."
  (interactive)
  (let ((name (read-string "Branch name: ")))
    (jj--run-command "branch" "create" name)
    (jj-log-refresh)))

(defun jj-branch-list ()
  "List branches."
  (interactive)
  (let ((buffer (get-buffer-create "*jj-branches*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (jj--run-command-color "branch" "list"))
        (goto-char (point-min))
        (view-mode)))
    (display-buffer buffer)))

(defun jj-git-push ()
  "Push to git remote."
  (interactive)
  (let ((branch (read-string "Push branch (empty for current): ")))
    (if (string-empty-p branch)
        (jj--run-command "git" "push")
      (jj--run-command "git" "push" "--branch" branch)))
  (jj-log-refresh))

(defun jj-git-fetch ()
  "Fetch from git remote."
  (interactive)
  (jj--run-command "git" "fetch")
  (jj-log-refresh)
  (message "Fetched from remote"))

(defun jj-rebase-quick ()
  "Quick rebase onto commit at point (original behavior)."
  (interactive)
  (when-let ((commit-id (jj-get-changeset-at-point)))
    (jj--run-command "rebase" "-d" commit-id)
    (jj-log-refresh)
    (message "Rebased onto %s" commit-id)))

(defun jj-diff ()
  "Show diff for current changes or commit at point."
  (interactive)
  (let* ((commit-id (jj-get-changeset-at-point))
         (buffer (get-buffer-create "*jj-diff*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if commit-id
            (insert (jj--run-command-color "show" "-r" commit-id))
          (insert (jj--run-command-color "diff")))
        (diff-mode)
        (ansi-color-apply-on-region (point-min) (point-max))
        (goto-char (point-min))))
    (display-buffer buffer)))

(defun jj-log-limit ()
  "Change the number of commits shown in log."
  (interactive)
  (let ((new-limit (read-number "Number of commits to show: " jj-log-limit)))
    (setq jj-log-limit new-limit)
    (jj-log-refresh)))

(defun jj-help ()
  "Show jj help."
  (interactive)
  (describe-keymap jj-mode-map))

;;;###autoload
(defun jj-goto-next-changeset ()
  "Navigate to the next changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (< (point) (point-max)))
      (magit-section-forward)
      (when-let ((section (magit-current-section)))
        (when (and (memq (oref section type) '(jj-log-entry-section jj-commit-section))
                   (> (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

;;;###autoload
(defun jj-goto-prev-changeset ()
  "Navigate to the previous changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (> (point) (point-min)))
      (magit-section-backward)
      (when-let ((section (magit-current-section)))
        (when (and (memq (oref section type) '(jj-log-entry-section jj-commit-section))
                   (< (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

(defun jj-changeset-menu ()
  "Show a transient menu for changeset operations."
  (interactive)
  (if-let ((commit-id (jj-get-changeset-at-point)))
      (message "Operations for changeset %s:\ne - Edit\ns - Squash\nS - Split\nr - Rebase\nd - Diff\nRET - Show details"
               commit-id)
    (message "No changeset at point")))

(defun jj-get-changeset-at-point ()
  "Get the changeset ID at point."
  (when-let ((section (magit-current-section)))
    (cond
     ((and (slot-boundp section 'commit-id)
           (memq (oref section type) '(jj-log-entry-section jj-commit-section)))
      (oref section commit-id))
     (t nil))))

(defun jj-revert-hunk ()
  "Revert the hunk at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (_ (eq (oref section type) 'jj-hunk-section))
              (file (oref section file))
              (header (oref section header)))
    (when (yes-or-no-p (format "Revert this hunk in %s? " file))
      ;; Get the hunk content from the section
      (let ((hunk-lines '()))
        (save-excursion
          (goto-char (oref section start))
          (forward-line 1) ; Skip header
          (while (< (point) (oref section end))
            (push (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))
                  hunk-lines)
            (forward-line 1)))
        (setq hunk-lines (nreverse hunk-lines))
        ;; Create a reverse patch
        (let ((patch-file (make-temp-file "jj-revert-hunk" nil ".patch")))
          (with-temp-file patch-file
            (insert "diff --git a/" file " b/" file "\n")
            (insert "--- a/" file "\n")
            (insert "+++ b/" file "\n")
            (insert header "\n")
            ;; Insert reversed hunk lines
            (dolist (line hunk-lines)
              (cond
               ((string-prefix-p "+" line)
                (insert "-" (substring line 1) "\n"))
               ((string-prefix-p "-" line)
                (insert "+" (substring line 1) "\n"))
               (t
                (insert line "\n")))))
          ;; Apply the patch using jj
          (let ((result (shell-command-to-string
                         (format "cd %s && patch -p1 < %s 2>&1"
                                 (magit-toplevel)
                                 patch-file))))
            (delete-file patch-file)
            (if (string-match-p "FAILED\\|rejected" result)
                (message "Failed to apply patch: %s" result)
              (jj-log-refresh)
              (message "Hunk reverted successfully"))))))))

(defun jj-revert-file ()
  "Revert all changes in the file at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (file (cond
                     ((eq (oref section type) 'jj-file-section)
                      (oref section file))
                     ((eq (oref section type) 'jj-hunk-section)
                      (oref section file)))))
    (when (yes-or-no-p (format "Revert all changes in %s? " file))
      ;; Use jj restore to revert file to working copy parent
      (let ((result (shell-command-to-string
                     (format "cd %s && %s restore %s 2>&1"
                             (magit-toplevel)
                             jj-executable
                             (shell-quote-argument file)))))
        (if (string-match-p "error\\|Error" result)
            (message "Failed to restore file: %s" result)
          (jj-log-refresh)
          (message "File %s restored" file))))))

(defun jj-apply-hunk ()
  "Apply the hunk at point (for staging-like functionality)."
  (interactive)
  (when-let* ((section (magit-current-section))
              (_ (eq (oref section type) 'jj-hunk-section)))
    (message "Apply hunk functionality would go here - jj doesn't have staging area like git")))

;; Rebase state management
(defvar jj-rebase-source nil
  "Currently selected source commit for rebase.")

(defvar jj-rebase-destinations nil
  "List of currently selected destination commits for rebase.")

(defvar jj-rebase-source-overlay nil
  "Overlay for highlighting the selected source commit.")

(defvar jj-rebase-destination-overlays nil
  "List of overlays for highlighting selected destination commits.")

;;;###autoload
(defun jj-rebase-clear-selections ()
  "Clear all rebase selections and overlays."
  (interactive)
  (setq jj-rebase-source nil
        jj-rebase-destinations nil)
  (when jj-rebase-source-overlay
    (delete-overlay jj-rebase-source-overlay)
    (setq jj-rebase-source-overlay nil))
  (dolist (overlay jj-rebase-destination-overlays)
    (delete-overlay overlay))
  (setq jj-rebase-destination-overlays nil)
  (message "Cleared all rebase selections"))

;;;###autoload
(defun jj-rebase-set-source ()
  "Set the commit at point as rebase source."
  (interactive)
  (when-let ((commit-id (jj-get-changeset-at-point))
             (section (magit-current-section)))
    ;; Clear previous source overlay
    (when jj-rebase-source-overlay
      (delete-overlay jj-rebase-source-overlay))
    ;; Set new source
    (setq jj-rebase-source commit-id)
    ;; Create overlay for visual indication
    (setq jj-rebase-source-overlay 
          (make-overlay (oref section start) (oref section end)))
    (overlay-put jj-rebase-source-overlay 'face '(:background "dark green" :foreground "white"))
    (overlay-put jj-rebase-source-overlay 'before-string "[SOURCE] ")
    (message "Set source: %s" commit-id)))

;;;###autoload
(defun jj-rebase-toggle-destination ()
  "Toggle the commit at point as a rebase destination."
  (interactive)
  (when-let ((commit-id (jj-get-changeset-at-point))
             (section (magit-current-section)))
    (if (member commit-id jj-rebase-destinations)
        ;; Remove from destinations
        (progn
          (setq jj-rebase-destinations (remove commit-id jj-rebase-destinations))
          ;; Remove overlay
          (dolist (overlay jj-rebase-destination-overlays)
            (when (and (>= (overlay-start overlay) (oref section start))
                       (<= (overlay-end overlay) (oref section end)))
              (delete-overlay overlay)
              (setq jj-rebase-destination-overlays (remove overlay jj-rebase-destination-overlays))))
          (message "Removed destination: %s" commit-id))
      ;; Add to destinations
      (push commit-id jj-rebase-destinations)
      ;; Create overlay for visual indication
      (let ((overlay (make-overlay (oref section start) (oref section end))))
        (overlay-put overlay 'face '(:background "dark blue" :foreground "white"))
        (overlay-put overlay 'before-string "[DEST] ")
        (push overlay jj-rebase-destination-overlays)
        (message "Added destination: %s" commit-id)))))

;;;###autoload
(defun jj-rebase-execute ()
  "Execute rebase with selected source and destinations."
  (interactive)
  (if (and jj-rebase-source jj-rebase-destinations)
      (when (yes-or-no-p (format "Rebase %s -> %s? " 
                                 jj-rebase-source 
                                 (string-join jj-rebase-destinations ", ")))
        (let* ((dest-args (apply 'append (mapcar (lambda (dest) (list "-d" dest)) jj-rebase-destinations)))
               (all-args (append (list "rebase" "-s" jj-rebase-source) dest-args))
               (result (apply #'jj--run-command all-args)))
          (if (string-match-p "error\\|Error" result)
              (message "Rebase failed: %s" result)
            (progn
              (jj-rebase-clear-selections)
              (jj-log-refresh)
              (message "Rebase completed: %s -> %s" 
                       jj-rebase-source 
                       (string-join jj-rebase-destinations ", "))))))
    (message "Please select source (s) and at least one destination (d) first")))

;; Transient rebase menu
;;;###autoload
(defun jj-rebase-transient ()
  "Transient for jj rebase operations."
  (interactive)
  (jj-rebase-transient--internal))

(transient-define-prefix jj-rebase-transient--internal ()
  "Internal transient for jj rebase operations."
  [:description
   (lambda ()
     (concat "JJ Rebase"
             (when jj-rebase-source
               (format " | Source: %s" jj-rebase-source))
             (when jj-rebase-destinations
               (format " | Destinations: %s"
                       (string-join jj-rebase-destinations ", ")))))
   :class transient-columns
   ["Selection"
    ("s" "Set source" jj-rebase-set-source
     :description (lambda ()
                    (if jj-rebase-source
                        (format "Set source (current: %s)" jj-rebase-source)
                      "Set source"))
     :transient t)
    ("d" "Toggle destination" jj-rebase-toggle-destination
     :description (lambda ()
                    (format "Toggle destination (%d selected)"
                            (length jj-rebase-destinations)))
     :transient t)
    ("c" "Clear selections" jj-rebase-clear-selections
     :transient t)]
   ["Actions"
    ("r" "Execute rebase" jj-rebase-execute
     :if (lambda () (and jj-rebase-source jj-rebase-destinations))
     :description (lambda ()
                    (if (and jj-rebase-source jj-rebase-destinations)
                        (format "Rebase %s -> %s"
                                jj-rebase-source
                                (string-join jj-rebase-destinations ", "))
                      "Execute rebase (select source & destinations first)"))
     :transient nil)
    ("n" "Next" jj-goto-next-changeset
     :transient t)
    ("p" "Prev" jj-goto-prev-changeset
     :transient t)
    ("q" "Quit" transient-quit-one)]])

(provide 'jj-mode)
