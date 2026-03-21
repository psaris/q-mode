;;; q-mode.el --- A q editing mode    -*- lexical-binding: t -*-

;; Copyright (C) 2006-2026 Nick Psaris <nick.psaris@gmail.com>
;; Keywords: faces files q
;; Package-Requires: ((emacs "28"))
;; Created: 8 Jun 2015
;; Version: 0.1
;; URL: https://github.com/psaris/q-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing q (the language written by Kx Systems, see
;; URL `https://code.kx.com') in Emacs.

;; Some of its major features include:
;;
;;  - syntax highlighting (font-lock-mode),
;;
;;  - syntax checking (flymake-mode),
;;
;;  - interaction with inferior q[con] instance (comint-mode),
;;
;;  - variable and function indexing (imenu),
;;
;;  - completion at point (CAPF),
;;
;;  - signature help (eldoc),
;;
;;  - definition/reference navigation (xref),
;;
;;  - code folding (hideshow).
;;
;; To load `q-mode' on-demand, instead of at startup, add this to your
;; initialization file

;; (autoload 'q-mode "q-mode")

;; Then add the following to your initialization file to open all .k
;; and .q files with q-mode as major mode automatically:

;; (add-to-list 'auto-mode-alist '("\\.[kq]\\'" . q-mode))

;; If you load ess-mode, it will attempt to associate the .q extension
;; with S-mode.  To stop this, add the following lines to your
;; initialization file.

;; (defun remove-ess-q-extn ()
;;   (when (assoc "\\.[qsS]\\'" auto-mode-alist)
;;    (setq auto-mode-alist
;;          (remassoc "\\.[qsS]\\'" auto-mode-alist))))
;; (add-hook 'ess-mode-hook 'remove-ess-q-extn)
;; (add-hook 'inferior-ess-mode-hook 'remove-ess-q-extn)

;; Use `M-x q' to start an inferior q shell.  Or use `M-x q-qcon' to
;; create an inferior qcon shell to communicate with an existing q
;; process.  Both can be prefixed with the universal-argument `C-u` to
;; customize the arguments used to start the processes.

;; The first q[con] session opened becomes the activated buffer.
;; To open a new session and send code to the new buffer, it must be
;; activated.  Switch to the desired buffer and type `C-c M-RET' to
;; activate it.

;; Displaying tables with many columns will wrap around the buffer -
;; making the data hard to read.  You can use the
;; `toggle-truncate-lines' function to prevent the wrapping.  You can
;; then scroll left and right in the buffer to see all the columns.

;; The following commands are available to interact with an inferior
;; q[con] process/buffer.  `C-c C-j' (as well as `C-c C-l' and
;; `C-M-x') sends a single line, `C-c C-f' sends the surrounding
;; function, `C-c C-s' sends the symbol at point, `C-c C-r' sends
;; the selected region and `C-c C-b' sends the whole buffer.  If
;; prefixed with `C-u C-u', or pressing `C-c M-j' `C-c M-f' `C-c
;; M-s' `C-c M-r' respectively, will also switch point to the active
;; q process buffer for direct interaction.

;; If the source file exists on the same machine as the q process,
;; `C-c M-l' can be used to load the file associated with the active
;; buffer.

;; Quick access to variable and function definitions can be obtained
;; using the `imenu' binding `M-g i'.  Completion is available via
;; `completion-at-point' (usually `M-TAB').  Candidates are annotated
;; with their kind (<function>, <variable>, <keyword>, or <builtin>).
;; Eldoc displays signatures while you type, and xref provides `M-.'
;; for definitions, `M-?' for references, and `C-M-.' for apropos
;; search across all known identifiers in the project.

;; Code folding is available via `hs-minor-mode'.  Once enabled, use
;; the standard hideshow bindings to fold and unfold {} blocks.

;; `which-function-mode' is supported and will display the name of the
;; enclosing function in the mode line as you move point.  Enable it
;; globally with (which-function-mode 1) in your initialization file,
;; or per-buffer with M-x which-function-mode.


;; `M-x customize-group' can be used to customize the `q' group.
;; Specifically, the `q-program' and `q-qcon-program' variables can be
;; changed depending on your environment.  The `q-rescan-idle-delay'
;; variable controls how long to wait after a save before rescanning;
;; it debounces rapid saves and defers the check for out-of-band disk
;; changes such as those made by git pull.

;; Q-mode indents each level based on `q-indent-step'.  To indent code
;; based on {}-, ()-, and []-groups instead of equal width tabs, you
;; can set this value to nil.

;; The variables `q-msg-prefix' and `q-msg-postfix' can be customized
;; to prefix and postfix every msg sent to the inferior q[con]
;; process.  This can be used to change directories before evaluating
;; definitions within the q process and then changing back to the root
;; directory.  To make the variables change values depending on which
;; file they are sent from, values can be defined in a single line at
;; the top of each .q file:

;; / -*- q-msg-prefix: "system \"d .jnp\";"; q-msg-postfix: ";system \"d .\"";-*-

;; or at the end:

;; / Local Variables:
;; / q-msg-prefix: "system \"d .jnp\";"
;; / q-msg-postfix: ";system \"d .\""
;; / End:


(require 'cl-lib)
(require 'comint)
(require 'hideshow nil t)
(require 'project nil t)
(require 'xref nil t)

;;; Code:

;; local variable for the flymake-dedicated q process
(defvar-local q--flymake-proc nil)

(defgroup q nil "Major mode for editing q code." :group 'languages)

(defcustom q-program "q"
  "Program name for invoking an inferior q."
  :type 'file
  :group 'q)

(defcustom q-host ""
  "If non-nil, Q-Shell will ssh to the remote host before executing q."
  :safe 'stringp
  :type 'string
  :group 'q)

(defcustom q-user ""
  "User to use when `ssh'-ing to the remote host."
  :safe 'stringp
  :type 'string
  :group 'q)

(defcustom q-indent-step 1
  "Length of indent used by `q-indent-line`.
If nil, code is aligned to {}-, ()-, and []-groups.  Otherwise,
each level is indented by this amount."
  :type '(choice (const nil) integer)
  :group 'q)

(defcustom q-comment-start "/"
  "String to insert to start a new comment (some prefer a double forward slash)."
  :safe 'stringp
  :type 'string
  :group 'q)

(defcustom q-msg-prefix ""
  "String to prefix every message sent to inferior q[con] process."
  :safe 'stringp
  :type 'string
  :group 'q)

(defcustom q-msg-postfix ""
  "String to postfix every message sent to inferior q[con] process."
  :safe 'stringp
  :type 'string
  :group 'q)

(defcustom q-flymake-on-save nil
  "If non-nil, only run Flymake checks after saving the buffer.
If nil, run checks for unsaved buffers by writing the current
buffer contents to a temporary file before invoking q."
  :safe 'booleanp
  :type 'boolean
  :group 'q)

(defcustom q-rescan-idle-delay 1.0
  "Seconds of idle time before rescanning after a save.
Debounces rapid successive saves and defers the check for out-of-band
disk changes (e.g. from git pull) until Emacs has been idle this long."
  :safe 'numberp
  :type 'number
  :group 'q)

(defgroup q-init nil "Q initialization variables." :group 'q)

(defcustom q-init-port 0
  "If non-zero, Q-Shell will start with the specified server port."
  :safe 'integerp
  :type 'integer
  :group 'q-init)

(defcustom q-init-slaves 0
  "If non-zero, Q-Shell will start with the specified number of slaves."
  :safe 'integerp
  :type 'integer
  :group 'q-init)

(defcustom q-init-workspace 0
  "If non-zero, Q-Shell will start with the specified workspace limit."
  :safe 'integerp
  :type 'integer
  :group 'q-init)

(defcustom q-init-garbage-collect nil
  "If non-nil, Q-Shell will start with garbage collection enabled."
  :safe 'booleanp
  :type 'boolean
  :group 'q-init)

(defcustom q-init-file ""
  "If non-empty, Q-Shell will load the specified file."
  :type 'file
  :group 'q-init)

(defgroup q-qcon nil "Q qcon arguments." :group 'q)

(defcustom q-qcon-program "qcon"
  "Program name for invoking an inferior qcon."
  :type 'file
  :group 'q-qcon)

(defcustom q-qcon-server ""
  "Remote q server."
  :safe 'stringp
  :type 'string
  :group 'q-qcon)

(defcustom q-qcon-port 5000
  "Port for remote q server."
  :safe 'integerp
  :type 'integer
  :group 'q-qcon)

(defcustom q-qcon-user ""
  "If non-nil, qcon will log in to remote q server with this id."
  :safe 'stringp
  :type 'string
  :group 'q-qcon)

(defcustom q-qcon-password ""
  "Password for remote q server."
  :safe 'stringp
  :type 'string
  :group 'q-qcon)

(defun q-customize ()
  "Customize `q-mode'."
  (interactive)
  (customize-group "q"))

(defvar q-active-buffer nil
  "The q-shell buffer to send q commands.")

(defun q-activate-this-buffer ()
  "Set the `q-activate-buffer' to the currently active buffer."
  (interactive)
  (q-activate-buffer (current-buffer)))

(defun q-shell-buffer-p (buffer)
  "Return non-nil if BUFFER is a live Q shell buffer.
BUFFER can be a buffer object, buffer name, or cons cell from completion."
  (let* ((target (if (consp buffer) (car buffer) buffer))
         (buf (and target (get-buffer target))))
    (and buf
         (buffer-live-p buf)
         (comint-check-proc buf)
         (with-current-buffer buf
           (eq major-mode 'q-shell-mode)))))

(defun q-activate-buffer (buffer)
  "Set the `q-active-buffer' to the supplied BUFFER.
Prompt with a list of live Q Shell buffers if called interactively."
  (interactive
   (list (read-buffer "activate buffer: "
                      nil
                      t
                      #'q-shell-buffer-p)))
  (when (called-interactively-p 'any) (display-buffer buffer))
  (setq q-active-buffer (get-buffer buffer)))

(defun q-default-args ()
  "Build the default q command-line argument string from `q-init-*' variables."
  (concat
   (unless (equal q-init-file "") (format " %s" (shell-quote-argument q-init-file)))
   (unless (equal q-init-port 0) (format " -p %s" q-init-port))
   (unless (equal q-init-slaves 0) (format " -s %s" q-init-slaves))
   (unless (equal q-init-workspace 0) (format " -w %s" q-init-workspace))
   (when q-init-garbage-collect " -g 1")))

(defun q-qcon-default-args ()
  "Build the default qcon command-line argument string from `q-qcon-*' variables."
  (concat (format "%s:%s" q-qcon-server q-qcon-port)
          (unless (equal q-qcon-user "") (format ":%s:%s" q-qcon-user q-qcon-password))))

(defun q-shell-name (server port)
  "Build name of q-shell based on SERVER and PORT."
  (if (and (equal server "") (equal port ""))
      "q"
    (concat "q-"
            (if (equal server "") "localhost" server)
            (unless (equal port "") (format ":%s" port)))))

;;;###autoload
(defun q (&optional host user args)
  "Start a new q process.
The optional argument HOST and USER allow the q process to be
started on a remote machine.  The optional ARGS argument
specifies the command line args to use when executing q; the
default ARGS are obtained from the q-init customization
variables.  In interactive use, a prefix argument directs this
command to read the command line arguments from the minibuffer."
  (interactive (let* ((args (q-default-args))
                      (user  q-user)
                      (host  q-host))
                 (if current-prefix-arg
                     (list (read-string "Host: " host)
                           (read-string "User: " user)
                           (read-string "Q command line args: " args))
                   (list host user args))))

  (unless (equal (or user "") "") (setq host (format "%s@%s" user host)))
  (let* ((cmd q-program)
         (args (or args ""))
         (host (or host ""))
         (cmd (if (equal args "") cmd (concat cmd args)))
         (qs (not (equal host "")))
         (port (let ((case-fold-search nil))
                 (if (string-match "-p *\\([0-9]+\\)" args) (match-string 1 args) "")))
         (buffer (get-buffer-create (format "*%s*" (q-shell-name host port))))
         (command (if qs "ssh" (or shell-file-name (getenv "SHELL") "/bin/sh")))
         (switches (append (if qs (list "-t" host) (list "-c")) (list cmd)))
         ;; disable kdb-x rlwrap functionality
         (process-environment (cons "KX_LINE=0" process-environment))
         process)
    (when (called-interactively-p 'any) (pop-to-buffer buffer))
    (when (or current-prefix-arg (not (q-shell-buffer-p buffer)))
      (with-current-buffer buffer
        (message "q: starting q with command \"%s\"" cmd)
        (q-shell-mode)
        (let ((comint-args (list buffer "q" command nil switches)))
          (setq process (get-buffer-process (apply 'comint-exec comint-args))))
        (setq comint-input-ring-file-name "~/.q_history")
        (comint-read-input-ring t)
        (set-process-sentinel process 'q-process-sentinel)))
    (q-activate-buffer buffer)
    process))

;;;###autoload
(defun q-qcon (&optional args)
  "Connect to a pre-existing q process.
Optional argument ARGS specifies the command line args to use
when executing qcon; the default ARGS are obtained from the
`q-host' and `q-init-port' customization variables.
In interactive use, a prefix argument directs this command
to read the command line arguments from the minibuffer."
  (interactive (let* ((args (q-qcon-default-args)))
                 (list (if current-prefix-arg
                           (read-string "qcon command line args: " args)
                         args))))
  (let* ((buffer (get-buffer-create (format "*qcon-%s*" args)))
         process)
    (when (called-interactively-p 'any) (pop-to-buffer buffer))
    (when (or current-prefix-arg (not (q-shell-buffer-p buffer)))
      (with-current-buffer buffer
        (message "q: starting qcon with command \"%s\"" (concat q-qcon-program " " args))
        (q-shell-mode)
        (setq comint-process-echoes nil)
        (setq process (get-buffer-process (comint-exec buffer "qcon" q-qcon-program nil (list args))))
        (setq comint-input-ring-file-name (concat (getenv "HOME") "/.qcon_history"))
        (comint-read-input-ring)
        (set-process-sentinel process 'q-process-sentinel)))
    (q-activate-buffer buffer)
    process))

(defun q-show-q-buffer ()
  "Switch to the active q process, or start a new one (passing in args)."
  (interactive)
  (unless (q-shell-buffer-p q-active-buffer)
    (q))
  (if (called-interactively-p 'any)
      (pop-to-buffer q-active-buffer)
    (display-buffer q-active-buffer)))

(defun q-kill-q-buffer ()
  "Kill the q process and its buffer."
  (interactive)
  (when q-active-buffer
    (kill-buffer q-active-buffer)
    (unless (buffer-live-p q-active-buffer) (setq q-active-buffer nil))))

(defun q-process-sentinel (process message)
  "Sentinel for use with q processes.
This marks the PROCESS with a MESSAGE, at a particular time point."
  (comint-write-input-ring)
  (let ((buffer (process-buffer process))
        (text (format "\nProcess %s %s at %s\n"
                      (process-name process)
                      (replace-regexp-in-string "[\r\n]+\\'" "" (or message ""))
                      (current-time-string))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert-before-markers text)))))

(defun q-strip (text)
  "Strip TEXT of all trailing comments, newlines and excessive whitespace.
The order of operations matters and must not be rearranged."
  (setq text (replace-regexp-in-string "^\\(?:[^\\\\].*\\)?[ \t]\\(/.*\\)\n" "" text t t 1)) ; / comments
  (setq text (replace-regexp-in-string "^/.+$" "" text t t)) ; / comments
  (setq text (replace-regexp-in-string "[ \t\n]+$" "" text t t)) ; excess white space
  (setq text (replace-regexp-in-string "\n[ \t]+" "" text t t)) ; fold functions
  text)

(defun q-send-string (string)
  "Send STRING to the inferior q process stored in `q-active-buffer'."
  (unless (stringp string)
    (user-error "Nothing to send"))
  (unless (q-shell-buffer-p q-active-buffer)
    (user-error "No active q buffer; run `M-x q` or activate a q shell with `C-c M-RET`"))
  (let ((msg (concat q-msg-prefix string q-msg-postfix)))
    (with-current-buffer q-active-buffer
      (unless comint-process-echoes
        (goto-char (point-max))
        (insert-before-markers (concat msg "\n")))
      (comint-simple-send (get-buffer-process q-active-buffer) msg)))
  (when (equal current-prefix-arg '(16)) (q-show-q-buffer)))

(defun q-eval-region (start end)
  "Send the region between START and END to the inferior q[con] process."
  (interactive "r")
  (q-send-string (q-strip (buffer-substring start end)))
  (setq deactivate-mark t))

(defun q-eval-line ()
  "Send the current line to the inferior q[con] process."
  (interactive)
  (q-eval-region (line-beginning-position) (line-end-position)))

(defun q-eval-line-and-step ()
  "Send the current line to the inferior q[con] process and step to the next line."
  (interactive)
  (q-eval-line)
  (forward-line))

(defun q-eval-symbol ()
  "Send the symbol at point to the inferior q[con] process."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (unless symbol
      (user-error "No symbol at point"))
    (q-send-string symbol)))

(defun q-eval-buffer ()
  "Load current buffer into the inferior q[con] process."
  (interactive)
  (q-eval-region (point-min) (point-max)))


(defconst q-symbol-regex
  "`\\(?:\\(?:\\w\\|[.]\\)\\(?:\\w\\|[_.]\\)*\\)?"
  "Regular expression used to find symbols.")

(defconst q-file-regex
  (concat q-symbol-regex ":\\(?:\\w\\|[/:_.]\\)*")
  "Regular expression used to find files.")

(defconst q-name-regex
  "\\_<\\([.]?[a-zA-Z]\\(?:\\w\\|[_.]\\)*\\)\\s-*"
  "Regular expression used to find variable or function names.")

(defconst q-function-regex
  (concat q-name-regex
          ":"                           ; assignment
          ":?"                          ; view
          "\\s-*"                       ; potential white space
          "\\(?:"                       ; one of the following
          "{"                           ; function declaration
          "\\|'\\s-*\\["                ; composition
          "\\|[^;{\n]*?\\(?:::\\|[-.~=!@#$%^&*_+|,<>?/\\:']" ; trailing binary operator
          "\\)"
          "\\s-*"                       ; potential white space
          "\\(?:\\s<\\|$\\|;\\)"        ; opening comment, new line, or semicolon
          "\\)"
          )
  "Regular expression used to find function declarations.")

(defconst q-variable-regex
  (concat q-name-regex
          "[-.~=!@#$%^&*_+|,<>?]?"      ; potential compound assignment
          ":"                           ; assignment
          ":?"                          ; view
          "\\s-*"                       ; potential space
          "[^ )}:;\n]"                  ; something else
          )
  "Regular expression used to find variable declarations.")

(defun q-eval-function ()
  "Send the current function to the inferior q[con] process."
  (interactive)
  (condition-case nil
      (save-excursion
        (goto-char (line-end-position))          ; go to end of line
        (let ((start (re-search-backward (concat "^" q-function-regex))) ; find beginning of function
              (end   (re-search-forward ":")) ; find end of function name
              (fun   (thing-at-point 'sexp))) ; find function body
          (unless fun
            (user-error "Could not parse function body"))
          (q-send-string (q-strip (concat (buffer-substring start end) fun)))))
    (search-failed
     (user-error "No function found around point"))))

(defun q-and-go (fun)
  "Call FUN interactively and show active q buffer."
  (let ((current-prefix-arg '(16))) (call-interactively fun)))

(defun q-eval-line-and-go ()
  "Send the current line to the inferior q[con] process and show active q buffer."
  (interactive)
  (q-and-go 'q-eval-line))

(defun q-eval-function-and-go ()
  "Send the function to the inferior q[con] process and show active q buffer."
  (interactive) (q-and-go 'q-eval-function))

(defun q-eval-region-and-go ()
  "Send the active region to the inferior q[con] process and show active q buffer."
  (interactive)
  (q-and-go 'q-eval-region))

(defun q-eval-symbol-and-go ()
  "Send current symbol to the inferior q[con] process and show active q buffer."
  (interactive)
  (q-and-go 'q-eval-symbol))

(defun q-load-file ()
  "Load current buffer's file into the inferior q[con] process after saving."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (save-buffer)
  (q-send-string (format "\\l %s" (shell-quote-argument buffer-file-name))))

;; keymaps

(defvar q-shell-mode-map
  (let ((q-shell-mode-map (make-sparse-keymap)))
    (define-key q-shell-mode-map (kbd "C-c M-RET") 'q-activate-this-buffer)
    (set-keymap-parent q-shell-mode-map comint-mode-map)
    q-shell-mode-map)
  "Keymap for inferior q mode.")

(defvar q-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-l"    'q-eval-line)
    (define-key map "\C-c\C-j"    'q-eval-line)
    (define-key map "\C-c\M-j"    'q-eval-line-and-go)
    (define-key map (kbd "<C-return>") 'q-eval-line-and-step)
    (define-key map "\C-\M-x"     'q-eval-function)
    (define-key map "\C-c\C-f"    'q-eval-function)
    (define-key map "\C-c\M-f"    'q-eval-function-and-go)
    (define-key map "\C-c\C-r"    'q-eval-region)
    (define-key map "\C-c\M-r"    'q-eval-region-and-go)
    (define-key map "\C-c\C-s"    'q-eval-symbol)
    (define-key map "\C-c\M-s"    'q-eval-symbol-and-go)
    (define-key map "\C-c\C-b"    'q-eval-buffer)
    (define-key map "\C-c\M-l"    'q-load-file)
    (define-key map (kbd "C-c M-RET") 'q-activate-buffer)
    (define-key map "\C-c\C-q"   'q-show-q-buffer)
    (define-key map "\C-c\C-\\"  'q-kill-q-buffer)
    (define-key map "\C-c\C-z"   'q-customize)
    (define-key map "\C-c\C-c"   'comment-region)
    map)
  "Keymap for q major mode.")

;; menu bars
(easy-menu-define q-menu q-mode-map
  "Menubar for q script commands."
  '("Q"
    ["Eval Line"             q-eval-line t]
    ["Eval Line and Step"    q-eval-line-and-step t]
    ["Eval Line and Go"      q-eval-line-and-go t]
    ["Eval Function"         q-eval-function t]
    ["Eval Function and Go"  q-eval-function-and-go t]
    ["Eval Region"           q-eval-region t]
    ["Eval Region and Go"    q-eval-region-and-go t]
    ["Eval Symbol"           q-eval-symbol t]
    ["Eval Symbol and Go"    q-eval-symbol-and-go t]
    ["Eval Buffer"           q-eval-buffer t]
    ["Load File"             q-load-file t]
    "---"
    ["Comment Region" comment-region t]
    "---"
    ["Customize Q"    q-customize t]
    ["Show Q Shell"   q-show-q-buffer t]
    ["Kill Q Shell"   q-kill-q-buffer t]
    ))

(easy-menu-define q-shell-menu q-shell-mode-map
  "Menubar for q shell commands."
  '("Q-Shell"
    ["Activate Buffer" q-activate-this-buffer t]
    ))

;; faces

;; font-lock-comment-face font-lock-comment-delimiter-face
;; font-lock-string-face font-loc-doc-face
;; font-lock-keyword-face font-lock-builtin-face
;; font-lock-function-name-face
;; font-lock-variable-name-face font-lock-type-face
;; font-lock-constant-face font-lock-warning-face
;; font-lock-negation-char-face font-lock-preprocessor-face

(defconst q-keyword-list
  '("abs" "acos" "asin" "atan" "avg" "bin" "binr" "by" "cor" "cos" "cov" "dev" "delete"
    "div" "do" "enlist" "exec" "exit" "exp" "from" "getenv" "hopen" "if" "in" "insert" "last"
    "like" "log" "max" "min" "prd" "select" "setenv" "sin" "sqrt" "ss"
    "sum" "tan" "update" "var" "wavg" "while" "within" "wsum" "xexp")
  "Keywords for q mode defined in .Q.res.")

(defconst q-keywords
  (concat "\\_<"
          "\\(?:[_]\\)?"                ; leading _ is not a symbol
          (regexp-opt q-keyword-list t)
          "\\_>")
  "Keyword regex for q mode defined in .Q.res.")

(defconst q-builtin-word-list
  '("aj" "aj0" "ajf" "ajf0" "all" "and" "any" "asc" "asof" "attr" "avgs" "ceiling"
    "cols" "count" "cross" "csv" "cut" "deltas" "desc"
    "differ" "distinct" "dsave" "each" "ej" "ema" "eval" "except" "fby" "fills"
    "first" "fkeys" "flip" "floor" "get" "group" "gtime" "hclose" "hcount"
    "hdel" "hsym" "iasc" "idesc" "ij" "ijf" "inter" "inv" "key" "keys"
    "lj" "ljf" "load" "lower" "lsq" "ltime" "ltrim" "mavg" "maxs" "mcount" "md5"
    "mdev" "med" "meta" "mins" "mmax" "mmin" "mmu" "mod" "msum" "neg"
    "next" "not" "null" "or" "over" "parse" "peach" "pj" "prds" "prior"
    "prev" "rand" "rank" "ratios" "raze" "read0" "read1" "reciprocal" "reval"
    "reverse" "rload" "rotate" "rsave" "rtrim" "save" "scan" "scov" "sdev" "set" "show"
    "signum" "ssr" "string" "sublist" "sums" "sv" "svar" "system" "tables" "til"
    "trim" "type" "uj" "ujf" "ungroup" "union" "upper" "upsert" "use" "value"
    "view" "views" "vs" "where" "wj" "wj1" "ww" "xasc" "xbar" "xcol" "xcols" "xdesc"
    "xgroup" "xkey" "xlog" "xprev" "xrank")
  "Builtin functions for q mode defined in q.k.")

(defconst q-builtin-words
  (concat "\\_<"
          "\\(?:[_]\\)?"                ; leading _ is not a symbol
          "\\("
          "\\(?:[.]q[.]\\)?"
          (regexp-opt q-builtin-word-list)
          "\\)"
          "\\_>")
  "Builtin function regex for q mode defined in q.k.")

(defconst q-builtin-dot-z-word-list
  '(".z.D" ".z.H" ".z.K" ".z.N" ".z.P" ".z.T" ".z.W" ".z.X" ".z.Z" ".z.a"
    ".z.ac" ".z.b" ".z.bm" ".z.c" ".z.d" ".z.e" ".z.ex" ".z.exit"
    ".z.ey" ".z.f" ".z.h" ".z.i" ".z.k" ".z.l" ".z.n" ".z.o" ".z.p"
    ".z.pc" ".z.pd" ".z.pg" ".z.ph" ".z.pi" ".z.pm" ".z.po" ".z.pp"
    ".z.pq" ".z.ps" ".z.pw" ".z.q" ".z.r" ".z.s" ".z.t" ".z.ts" ".z.u"
    ".z.vs" ".z.w" ".z.wc" ".z.wo" ".z.ws" ".z.x" ".z.z" ".z.zd")
  "Builtin .z functions/constants defined for q mode.")

(defconst q-builtin-dot-z-words
  (concat "\\_<"
          "\\(?:[_]\\)?"                ; leading _ is not a symbol
          (regexp-opt q-builtin-dot-z-word-list t)
          "\\_>")
  "Builtin .z functions/constants regex defined for q mode.")

(defconst q-builtin-dot-Q-word-list
  '(".Q.a" ".Q.A" ".Q.b6" ".Q.chk" ".Q.cn" ".Q.def" ".Q.dpft" ".Q.dsftg"
    ".Q.en" ".Q.ens" ".Q.f" ".Q.fc" ".Q.fmt" ".Q.fp" ".Q.fqk" ".Q.fs"
    ".Q.fsn" ".Q.ft" ".Q.fu" ".Q.gc" ".Q.hdpf" ".Q.hg" ".Q.hp" ".Q.id"
    ".Q.j10" ".Q.j12" ".Q.k" ".Q.l" ".Q.n" ".Q.nA" ".Q.pd" ".Q.pf"
    ".Q.pn" ".Q.pt" ".Q.pv" ".Q.pw" ".Q.qp" ".Q.qt" ".Q.res" ".Q.s"
    ".Q.s1" ".Q.s2" ".Q.sbt" ".Q.sha" ".Q.t" ".Q.te" ".Q.trp" ".Q.ts"
    ".Q.ty" ".Q.u" ".Q.v" ".Q.vp" ".Q.w" ".Q.x10" ".Q.x12" ".Q.xf"
    ".Q.xR" ".Q.xs")
  "Builtin .Q functions/constants defined for q mode.")

(defconst q-builtin-dot-Q-words
  (concat "\\_<"
          "\\(?:[_]\\)?"                ; leading _ is not a symbol
          (regexp-opt q-builtin-dot-Q-word-list t)
          "\\_>")
  "Builtin .Q functions/constants regex defined for q mode.")

(defconst q-builtin-dot-h-word-list
  '(".h.hn" ".h.hp" ".h.hr" ".h.ht" ".h.hta" ".h.htac" ".h.htc" ".h.html"
    ".h.http" ".h.hu" ".h.hug" ".h.hy" ".h.jx" ".h.pre" ".h.ta" ".h.td"
    ".h.text" ".h.th" ".h.tr" ".h.tx" ".h.ty" ".h.val" ".h.xd" ".h.xmp"
    ".h.xs" ".h.xt")
  "Builtin .h functions/constants defined for q mode.")

(defconst q-builtin-dot-h-words
  (concat "\\_<"
          "\\(?:[_]\\)?"                ; leading _ is not a symbol
          (regexp-opt q-builtin-dot-h-word-list t)
          "\\_>")
  "Builtin .h functions/constants regex defined for q mode.")

(defconst q-builtin-dot-j-word-list
  '(".j.j" ".j.jd" ".j.k")
  "Builtin .j functions/constants defined for q mode.")

(defconst q-builtin-dot-j-words
  (concat "\\_<"
          "\\(?:[_]\\)?"                ; leading _ is not a symbol
          (regexp-opt q-builtin-dot-j-word-list t)
          "\\_>")
  "Builtin .j functions/constants regex defined for q mode.")

(defconst q-font-lock-keywords ;; keywords
  (list
   ;; q single-letter system commands keep comments
   '("^\\\\\\(?:\\w\\|[_]\\)\\(?:\\s-.*?\\)?$" 0 font-lock-preprocessor-face keep)
   ;; os multi-letter system commands ignore comments
   '("^\\\\\\w\\w.*?$" 0 font-lock-preprocessor-face prepend)
   '("^'.*" . font-lock-warning-face) ; error
   (list (concat "[; ]\\('" q-symbol-regex "\\)") 1 font-lock-warning-face nil) ; signal
   (cons q-file-regex 'font-lock-preprocessor-face) ; files
   (cons q-symbol-regex 'font-lock-constant-face) ; symbols
   )
  "Minimal highlighting expressions for q mode.")

(defconst q-font-lock-keywords-1          ; symbols
  (append q-font-lock-keywords
          (list
           (list q-keywords 1 'font-lock-keyword-face nil) ; select from
           '("\\b[0-2]:" . font-lock-builtin-face)         ; IO/IPC
           (list q-builtin-words 1 'font-lock-builtin-face nil) ; q.k
           (list q-builtin-dot-z-words 1 'font-lock-builtin-face nil) ; .z.*
           (list q-builtin-dot-Q-words 1 'font-lock-builtin-face nil) ; .Q.*
           (list q-builtin-dot-h-words 1 'font-lock-builtin-face nil) ; .h.*
           (list q-builtin-dot-j-words 1 'font-lock-builtin-face nil) ; .j.*
           ))
  "More highlighting expressions for q mode.")

(defconst q-font-lock-keywords-2 ; function/variable names and literals
  (append q-font-lock-keywords-1
          (list
           (list q-function-regex 1 'font-lock-function-name-face nil) ; functions
           (list q-variable-regex 1 'font-lock-variable-name-face nil) ; variables
           '("\\_<[0-9]\\{4\\}\\.[0-9]\\{2\\}\\(?:m\\|\\.[0-9]\\{2\\}\\(?:T\\(?:[0-9]\\{2\\}\\(?::[0-9]\\{2\\}\\(?::[0-9]\\{2\\}\\(?:\\.[0-9]*\\)?\\)?\\)?\\)?\\)?\\)\\_>" . font-lock-constant-face) ; month/date/datetime
           '("\\_<\\(?:[0-9]\\{4\\}\\.[0-9]\\{2\\}\\.[0-9]\\{2\\}\\|[0-9]+\\)D\\(?:[0-9]\\(?:[0-9]\\(?::[0-9]\\{2\\}\\(?::[0-9]\\{2\\}\\(?:\\.[0-9]*\\)?\\)?\\)?\\)?\\)?\\_>" . font-lock-constant-face) ; timespan/timestamp
           '("\\_<[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\_>" . font-lock-constant-face) ; guid
           '("\\_<[0-9]\\{2\\}:[0-9]\\{2\\}\\(?::[0-9]\\{2\\}\\(?:\\.[0-9]\\|\\.[0-9]\\{2\\}\\|\\.[0-9]\\{3\\}\\)?\\)?\\_>" . font-lock-constant-face) ; time
           '("\\_<\\(?:[0-9]+\\.\\(?:[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?[ef]?\\_>" . font-lock-constant-face) ; floats/reals
           '("\\_<[0-9]+[cefhijnptuv]?\\_>" . font-lock-constant-face) ; char/real/float/short/int/long/time-types
           '("\\_<[01]+b\\_>" . font-lock-constant-face) ; bool
           '("\\_<0x[0-9a-fA-F]+\\_>" . font-lock-constant-face) ; bytes
           '("\\_<0[nNwW][cefghijmndzuvtp]?\\_>" . font-lock-constant-face) ; null/infinity
           '("\\(?:TODO\\|NOTE\\)\\:?" 0 font-lock-warning-face t) ; TODO
           ))
  "Most highlighting expressions for q mode.")

(defconst q-font-lock-defaults
  '((q-font-lock-keywords q-font-lock-keywords-1 q-font-lock-keywords-2)
    nil nil nil nil)
  "Font lock defaults for q mode.
Syntactic context (strings, comments) is handled by
`q-syntax-propertize', not by font-lock-syntactic-keywords.")


;; syntax table

(defvar q-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" ".  " table) ; treat " as punctuation
    (modify-syntax-entry ?\/ ".  " table) ; treat / as punctuation
    (modify-syntax-entry ?\n ">  " table) ; comments are ended by a new line
    (modify-syntax-entry ?\r ">  " table) ; comments are ended by a new line
    (modify-syntax-entry ?\. "_  " table) ; treat _ as symbol
    (modify-syntax-entry ?\_ "_  " table) ; treat _ as symbol
    (modify-syntax-entry ?\\ ".  " table) ; treat \ as punctuation
    (modify-syntax-entry ?\$ ".  " table) ; treat $ as punctuation
    (modify-syntax-entry ?\% ".  " table) ; treat % as punctuation
    (modify-syntax-entry ?\& ".  " table) ; treat & as punctuation
    (modify-syntax-entry ?\+ ".  " table) ; treat + as punctuation
    (modify-syntax-entry ?\, ".  " table) ; treat , as punctuation
    (modify-syntax-entry ?\- ".  " table) ; treat - as punctuation
    (modify-syntax-entry ?\= ".  " table) ; treat = as punctuation
    (modify-syntax-entry ?\* ".  " table) ; treat * as punctuation
    (modify-syntax-entry ?\< ".  " table) ; treat < as punctuation
    (modify-syntax-entry ?\> ".  " table) ; treat > as punctuation
    (modify-syntax-entry ?\| ".  " table) ; treat | as punctuation
    (modify-syntax-entry ?\` ".  " table) ; treat ` as punctuation
    table)
  "Syntax table for `q-mode'.")

(defun q-syntax-propertize (start end)
  "Apply syntax properties for q strings and comments between START and END."
  (funcall
   (syntax-propertize-rules
    ;; bare / line opens a block comment
    ("^\\(/\\)[ \t]*$"
     (1 (unless (nth 4 (syntax-ppss)) (string-to-syntax "< b"))))
    ;; bare \ line closes a block comment
    ("^\\(\\\\\\)[ \t]*$"
     (1 (when (nth 4 (syntax-ppss)) (string-to-syntax "> b"))))
    ;; " opens or closes a string, honouring \" escapes
    ("\\(\"\\)\\(?:[^\"\\\\]\\|\\\\.\\)*\\(\"\\)?"
     (1 (string-to-syntax "\""))
     (2 (string-to-syntax "\"")))
    ;; / after whitespace or BOL starts a line comment, not inside a string
    ("\\(?:^\\|[ \t]\\)\\(/\\)"
     (1 (unless (nth 3 (syntax-ppss)) (string-to-syntax "<")))))
   start end))


;; flymake

(defun q-flymake (report-fn &rest _args)
  "Flymake backend using the q program.
Takes a Flymake callback REPORT-FN as argument, as expected of a member
of `flymake-diagnostic-functions'.  q evaluates source code while
checking it; this backend therefore performs a runtime check.
When `q-flymake-on-save' is nil, diagnostics are produced from the
current buffer by checking a temporary file."
  (when (process-live-p q--flymake-proc)
    (kill-process q--flymake-proc))

  (let ((source (current-buffer))
        (file (buffer-file-name))
        (default-directory (file-name-directory (buffer-file-name))))
    (cond
     ((not file)
      (funcall report-fn nil))
     ((and q-flymake-on-save (buffer-modified-p))
      (funcall report-fn nil))
     (t
      (let ((input-file file))
        (when (buffer-modified-p)
          (save-restriction
            (widen)
            (setq input-file (make-temp-file "q-flymake-" nil ".q"))
            (write-region (point-min) (point-max) input-file nil 'silent)))
        ;; reset the `q--flymake-proc' process to a new q process
        (setq
         q--flymake-proc
         (make-process
          :name "q-flymake" :noquery t :connection-type 'pipe
          :buffer (generate-new-buffer " *q-flymake*")
          :command (list q-program input-file)
          :sentinel
          (lambda (proc _event)
            ;; check that the process has exited (not just suspended)
            (when (memq (process-status proc) '(exit signal))
              (unwind-protect
                  ;; only proceed if `proc' is the same as
                  ;; `q--flymake-proc', which indicates that `proc' is
                  ;; not an obsolete process
                  (if (and (buffer-live-p source)
                           (with-current-buffer source (eq proc q--flymake-proc)))
                      (with-current-buffer (process-buffer proc)
                        (goto-char (point-min))
                        (cl-loop
                         while (search-forward-regexp
                                (concat
                                 "^'[0-9.:T]* \\(.*\\)"  ; error message
                                 "\\(?:.\\|\\\n\\)*\\\n" ; stack trace
                                 "\\( *[[][0-9][]] *.*\\.[kq]:\\([0-9]+\\): \\).*\\\n" ; line number
                                 "\\( +^\\)$" ; carat showing column of error
                                 )
                                nil t)
                         for msg = (match-string 1)
                         for prefix = (match-string 2)
                         for row = (string-to-number (match-string 3))
                         for carat = (match-string 4)
                         for col = (- (length carat) (length prefix))
                         for (beg . end) = (flymake-diag-region source row col)
                         when (and beg end)
                         collect (flymake-make-diagnostic source beg end :error msg)
                         into diags
                         finally (funcall report-fn diags)))
                    (flymake-log :warning "Canceling obsolete check %s" proc))
                (when (and input-file (not (equal input-file file)))
                  (ignore-errors (delete-file input-file)))
                (kill-buffer (process-buffer proc))))))))
        (process-send-eof q--flymake-proc)))))

(defconst q-capf-core-words
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (w (append q-keyword-list q-builtin-word-list q-builtin-dot-z-word-list
                       q-builtin-dot-Q-word-list q-builtin-dot-h-word-list
                       q-builtin-dot-j-word-list))
      (puthash w t ht))
    ht)
  "Core words used for q completion candidates (hash table for O(1) lookup).")

;; Project scan caches
;;
;; Design goals:
;;   1. eldoc / CAPF never block on I/O or file-list expansion.
;;   2. Rescans happen on an idle timer, not inline on every keystroke.
;;   3. All buffers belonging to the same project share one cache entry
;;      so the project is scanned once regardless of how many files are open.
;;   4. The file list (including \l expansion) is cached separately with
;;      a lightweight sentinel so it is not recomputed on every tick.
;;   5. The per-file scan state uses mtimes only (one stat per file), not
;;      full buffer-modified-p walks across the entire project.
;;   6. Saving a file triggers an incremental rescan of that file only.
;;      The merged indexes are rebuilt from the per-file sub-index, so
;;      unchanged files are never re-read.
;;   7. Out-of-band disk changes (e.g. git pull) are detected by comparing
;;      stored mtimes against current mtimes before each idle rescan.  When
;;      any mtime has drifted the rescan is promoted to a full scan.  The
;;      file just saved is excluded from this check since its mtime change
;;      is expected and should not trigger a full rescan.

;; Per-buffer state: only the idle timer is buffer-local; all scan data
;; lives in the shared project store below.

(defvar-local q--rescan-idle-timer nil
  "Pending idle timer for the next project rescan.")

;; Shared project store: project-root -> plist
;;
;; Keys stored in each plist:
;;   :file-index              hash-table  file-path (or :buffer) ->
;;                              plist of :definitions :references :symbols :mtime
;;   :definition-index        hash-table  canonical-name -> entry list  (merged)
;;   :reference-index         hash-table  canonical-name -> entry list  (merged)
;;   :completion-candidates   list                                       (merged)
;;   :scan-state              list of (file . mtime) pairs
;;   :file-list               list of expanded q source files
;;   :file-list-sentinel      (project-key buffer-file-name) for cache validity

(defvar q--project-cache (make-hash-table :test #'equal)
  "Global map from project key to shared scan cache plist.
All `q-mode' buffers belonging to the same project read and write the
same entry, so the project is scanned at most once at any given time.")

;; project key and plist accessors

(defun q--project-key ()
  "Return a string key identifying the current buffer's project.
Uses the expanded project root when available, otherwise the buffer's
file path.  Returns nil for unsaved buffers with no project."
  (or (and (featurep 'project)
           (let ((p (project-current nil)))
             (and p (expand-file-name (project-root p)))))
      (buffer-file-name)))

(defun q--project-plist-get (prop)
  "Return PROP from the shared cache plist for the current buffer's project."
  (plist-get (gethash (q--project-key) q--project-cache) prop))

(defun q--project-plist-put (&rest kvs)
  "Set key-value pairs KVS in the shared cache plist for the current project."
  (let* ((key (q--project-key))
         (plist (gethash key q--project-cache)))
    (while kvs
      (setq plist (plist-put plist (pop kvs) (pop kvs))))
    (puthash key plist q--project-cache)))

;; scannable file predicates

(defun q--scannable-q-file-p (file)
  "Return non-nil when FILE is a scannable q source file."
  (and (string-match-p "\\.[kq]\\'" file)
       (not (string-prefix-p ".#" file))
       (file-regular-p file)
       (file-readable-p file)))

;; \l load-target discovery

(defconst q--load-command-regex "^\\\\l\\s-+\\([^ \t\n]+\\)"
  "Regex matching q load commands.")

(defconst q--namespace-command-regex "^\\\\d\\s-+\\([^ \t\n]+\\)"
  "Regex matching q namespace switch commands.")

(defun q--resolve-load-path (raw file)
  "Resolve RAW load path from FILE context."
  (let* ((arg (string-trim raw))
         (base-dir (and file (file-name-directory file)))
         (path (if (file-name-absolute-p arg)
                   arg
                 (expand-file-name arg (or base-dir default-directory)))))
    (and (file-regular-p path) (file-readable-p path) path)))

(defun q--load-targets-in-buffer (&optional file)
  "Return loaded FILE targets referenced in current buffer."
  (let (targets)
    (goto-char (point-min))
    (while (re-search-forward q--load-command-regex nil t)
      (let ((resolved (q--resolve-load-path
                       (match-string-no-properties 1)
                       file)))
        (when resolved
          (push resolved targets))))
    (delete-dups targets)))

(defun q--load-targets-in-file (file)
  "Return loaded file targets referenced by FILE.
Uses a visiting buffer when modified; otherwise reads from disk."
  (let ((buf (find-buffer-visiting file)))
    (if (and buf (buffer-modified-p buf))
        (with-current-buffer buf
          (save-excursion
            (q--load-targets-in-buffer file)))
      (with-temp-buffer
        (condition-case nil
            (progn
              (insert-file-contents file)
              (q--load-targets-in-buffer file))
          (file-missing nil))))))

(defun q--expand-loaded-files (roots)
  "Return ROOTS plus recursively loaded files from \\l commands."
  (let ((seen (make-hash-table :test #'equal))
        (queue nil)
        (all nil))
    (cl-labels ((enqueue-unique (file)
                  (unless (gethash file seen)
                    (puthash file t seen)
                    (push file queue)
                    (push file all))))
      (dolist (file roots)
        (enqueue-unique file))
      (while queue
        (let ((file (pop queue)))
          (dolist (loaded (q--load-targets-in-file file))
            (enqueue-unique loaded)))))
    (nreverse all)))

;; file-list cache

(defun q--project-root-files ()
  "Return top-level (non-\\l-expanded) q files for the current project."
  (if (featurep 'project)
      (let ((project (project-current nil)))
        (when project
          (cl-remove-if-not #'q--scannable-q-file-p
                            (project-files project))))
    (let ((file (buffer-file-name)))
      (and file (q--scannable-q-file-p file) (list file)))))

(defun q--ensure-project-file-list ()
  "Return the cached expanded file list, refreshing when project or file changes.
The \\l expansion only runs when the sentinel changes, not on every
eldoc or CAPF invocation."
  (let* ((file     (buffer-file-name))
         (sentinel (list (q--project-key) file)))
    (unless (equal sentinel (q--project-plist-get :file-list-sentinel))
      (let ((files (q--expand-loaded-files
                    (or (q--project-root-files)
                        (and file (list file))))))
        (q--project-plist-put :file-list-sentinel sentinel
                               :file-list files))))
  (q--project-plist-get :file-list))

;; scan-cache state

(defun q--file-mtime (file)
  "Return the modification time of FILE, or :missing when unavailable."
  (condition-case nil
      (file-attribute-modification-time (file-attributes file))
    (file-missing :missing)))

(defun q--compute-scan-cache-state (files)
  "Return a cache-state token for FILES as a list of (file . mtime) pairs."
  (mapcar (lambda (f) (cons f (q--file-mtime f)))
          (sort (copy-sequence files) #'string<)))

(defun q--scan-state-stale-p (&optional exclude-file)
  "Return non-nil if any file in the stored scan-state has a changed mtime.
Detects out-of-band disk changes such as those made by git pull.
EXCLUDE-FILE, if provided, is skipped -- its mtime change is expected
after an in-Emacs save and should not trigger a full rescan."
  (cl-some (lambda (entry)
             (and (not (equal (car entry) exclude-file))
                  (not (equal (cdr entry) (q--file-mtime (car entry))))))
           (q--project-plist-get :scan-state)))

;; source scanning

(defconst q--identifier-token-regex (concat q-name-regex "\\_>")
  "Regex matching q identifiers for reference scanning.")

(defun q--canonicalize-name (namespace name)
  "Return canonical fully-scoped NAME using NAMESPACE context."
  (if (string-search "." name)
      name
    (concat (or namespace ".") "." name)))

(defun q--namespace-at-point ()
  "Return the active q namespace at point based on preceding \\d commands."
  (save-excursion
    (let ((limit (point))
          (namespace nil))
      (goto-char (point-min))
      (while (re-search-forward q--namespace-command-regex limit t)
        (setq namespace (match-string-no-properties 1)))
      namespace)))

(defun q--make-entry (meta &optional doc signature file)
  "Return scanner entry from META with optional DOC, SIGNATURE and FILE location."
  (append (list :summary (plist-get meta :summary))
          (when signature (list :signature signature))
          (when doc       (list :doc doc))
          (if file
              (list :file file
                    :line (plist-get meta :line)
                    :col (plist-get meta :col))
            (list :buffer (current-buffer)
                  :pos (plist-get meta :pos)))))

(defun q--function-signature (name summary)
  "Return a signature string for NAME from its definition SUMMARY line.
Parses explicit args from {[a;b;c]...} or infers implicit args by
checking which of x, y, z are referenced in the function body.
Returns nil when SUMMARY does not look like a function definition."
  (when (string-match "{" summary)
    (let ((args
           (if (string-match "{\\[\\([^]]*\\)\\]" summary)
               ;; explicit argument list
               (split-string (match-string 1 summary) ";" t "[ \t]+")
             ;; implicit: infer from x, y, z references in the body
             (let ((body (substring summary (string-match "{" summary))))
               (cl-remove-if-not
                (lambda (arg)
                  (string-match (concat "\\_<" arg "\\_>") body))
                '("x" "y" "z"))))))
      (format "%s[%s]" name (string-join args ";")))))

(defun q--definition-doc (pos)
  "Return doc text for definition at POS from inline or preceding comments."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (or (when (looking-at ".*[ \t]/+[ \t]*\\(.*\\)$")
          (match-string-no-properties 1))
        (let (comments)
          (while (and (= (forward-line -1) 0)
                      (looking-at "^[ \t]*/+[ \t]*\\(.*\\)$"))
            (push (match-string-no-properties 1) comments))
          (when comments
            (string-join comments " "))))))

(defun q--scan-source-in-current-buffer (&optional file)
  "Return scan artifacts from current buffer, optionally for FILE."
  (save-excursion
    (let ((def-index (make-hash-table :test #'equal))
          (ref-index (make-hash-table :test #'equal))
          (symbols nil)
          (def-pattern (concat "^" q-variable-regex))
          (namespace nil))
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (if (looking-at q--namespace-command-regex)
            (setq namespace (match-string-no-properties 1))
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (line (line-number-at-pos line-start))
                 (summary (buffer-substring-no-properties line-start line-end)))
            (cl-labels ((make-meta (pos)
                          (list :pos pos
                                :line line
                                :col (save-excursion
                                       (goto-char pos)
                                       (current-column))
                                :summary summary)))
              (when (looking-at def-pattern)
                (let* ((name (match-string-no-properties 1))
                       (def-pos (match-beginning 0))
                       (canonical (q--canonicalize-name namespace name))
                       (meta (make-meta def-pos))
                       (doc (q--definition-doc def-pos))
                       (signature (q--function-signature name summary))
                       (entry (q--make-entry meta doc signature file)))
                  (puthash canonical (cons entry (gethash canonical def-index)) def-index)
                  (push canonical symbols)))
              (while (re-search-forward q--identifier-token-regex line-end t)
                (let* ((name (match-string-no-properties 1))
                       (ref-pos (match-beginning 1))
                       (canonical (q--canonicalize-name namespace name))
                       (meta (make-meta ref-pos))
                       (entry (q--make-entry meta nil file)))
                  (puthash canonical (cons entry (gethash canonical ref-index)) ref-index))))))
        (forward-line 1))
      (maphash
       (lambda (name entries)
         (puthash name (nreverse entries) def-index))
       def-index)
      (maphash
       (lambda (name entries)
         (puthash name (nreverse entries) ref-index))
       ref-index)
      (list :definitions def-index
            :references ref-index
            :symbols (delete-dups symbols)))))

(defun q--scan-file-artifacts (file)
  "Return scan artifacts for FILE or the current buffer when FILE is nil."
  (if (not file)
      (q--scan-source-in-current-buffer)
    (let ((buf (find-buffer-visiting file)))
      (if (and buf (buffer-modified-p buf))
          (with-current-buffer buf
            (q--scan-source-in-current-buffer))
        (with-temp-buffer
          (condition-case nil
              (progn
                (insert-file-contents file)
                (set-syntax-table q-mode-syntax-table)
                (q--scan-source-in-current-buffer file))
            (file-missing
             (list :definitions (make-hash-table :test #'equal)
                   :references (make-hash-table :test #'equal)
                   :symbols nil))))))))

;; merged index rebuild (from per-file sub-index)

(defun q--rebuild-merged-indexes ()
  "Rebuild the merged indexes from the per-file sub-index.
Called after any per-file entry is updated so all buffers in the project
see a consistent view without re-reading any unchanged files.
Uses `append' (not `nconc') to avoid destructively modifying the source
lists stored in the per-file sub-index, which would cause circular lists
on repeated rebuilds."
  (let ((file-index (q--project-plist-get :file-index))
        (def-index  (make-hash-table :test #'equal))
        (ref-index  (make-hash-table :test #'equal))
        (symbols    nil))
    (when (hash-table-p file-index)
      (maphash (lambda (_file artifacts)
                 (maphash (lambda (name entries)
                            (puthash name (append (gethash name def-index) entries) def-index))
                          (plist-get artifacts :definitions))
                 (maphash (lambda (name entries)
                            (puthash name (append (gethash name ref-index) entries) ref-index))
                          (plist-get artifacts :references))
                 (setq symbols (append symbols (plist-get artifacts :symbols))))
               file-index))
    (let ((candidates (make-hash-table :test #'equal :size (hash-table-count q-capf-core-words))))
      (maphash (lambda (k _) (puthash k t candidates)) q-capf-core-words)
      (dolist (sym symbols) (puthash sym t candidates))
      (q--project-plist-put
       :definition-index      def-index
       :reference-index       ref-index
       :completion-candidates candidates))))

;; full and incremental rescans

(defun q--do-full-rescan (buf)
  "Scan every file in the project for BUF and rebuild the shared cache.
Used on first load, after a git pull, or when the file list changes.
Emits a message with timing when scanning a multi-file project."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((files (q--ensure-project-file-list))
             (state (q--compute-scan-cache-state files)))
        (unless (equal state (q--project-plist-get :scan-state))
          (let ((file-index (make-hash-table :test #'equal))
                (t0 (float-time)))
            (dolist (file (or files (list nil)))
              (let ((artifacts (q--scan-file-artifacts file)))
                (puthash (or file :buffer) artifacts file-index)))
            (q--project-plist-put :scan-state state
                                   :file-index file-index)
            (q--rebuild-merged-indexes)
            ;; Only report when there are real project files to scan.
            ;; Suppress the message for the single-unsaved-buffer fallback.
            (when files
              (message "q: scanned %d file%s in %.2fs"
                       (length files)
                       (if (= (length files) 1) "" "s")
                       (- (float-time) t0)))))))))

(defun q--do-incremental-rescan (buf file)
  "Re-scan only FILE in the shared cache for BUF, then rebuild merged indexes.
When FILE is nil, re-scans the current buffer's in-memory content.
Falls back to a full rescan when no per-file sub-index exists yet."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((file-index (q--project-plist-get :file-index))
             (key        (or file :buffer)))
        (if (not (hash-table-p file-index))
            (q--do-full-rescan buf)
          (let* ((new-mtime  (and file (q--file-mtime file)))
                 (old-entry  (gethash key file-index))
                 (old-mtime  (and old-entry (plist-get old-entry :mtime))))
            ;; Skip if the file on disk has not changed since the last scan.
            ;; This guards against spurious after-save-hook fires and
            ;; ensures we only do work when content has actually changed.
            (unless (and old-mtime new-mtime (equal old-mtime new-mtime))
              (let ((artifacts (q--scan-file-artifacts file)))
                (puthash key (plist-put artifacts :mtime new-mtime) file-index)
                ;; Recompute scan-state so the staleness check stays coherent.
                (q--project-plist-put
                 :scan-state (q--compute-scan-cache-state
                              (q--project-plist-get :file-list)))
                (q--rebuild-merged-indexes)))))))))

;; idle-timer scheduling

(defun q--schedule-rescan ()
  "Schedule an idle rescan for the current buffer's file.
Promotes to a full rescan when out-of-band disk changes are detected --
e.g. after git pull; otherwise performs a cheaper incremental rescan of
the saved file only.  The saved file is excluded from the staleness
check since its mtime change is expected.  Debounces any pending timer."
  (let ((buf  (current-buffer))
        (file (buffer-file-name)))
    (when (timerp q--rescan-idle-timer)
      (cancel-timer q--rescan-idle-timer))
    (setq-local q--rescan-idle-timer
                (run-with-idle-timer
                 q-rescan-idle-delay nil
                 (lambda ()
                   (if (q--scan-state-stale-p file)
                       (progn
                         (message "q: project files changed, rescanning...")
                         (q--do-full-rescan buf))
                     (q--do-incremental-rescan buf file)))))))

(defun q--ensure-project-scan-cache ()
  "Ensure the shared project cache is populated for the current buffer.
On the very first call the scan runs synchronously so callers have
data immediately.  Subsequent calls return instantly; the idle timer
(triggered by save/revert hooks) keeps the cache fresh."
  (when (and (q--project-key)
             (null (q--project-plist-get :scan-state)))
    (q--do-full-rescan (current-buffer))))

(defun q--maybe-evict-project-cache ()
  "Remove the shared cache entry when no more q-mode buffers exist.
Intended for use in `kill-buffer-hook' to avoid unbounded cache growth."
  (let ((key (q--project-key)))
    (when key
      (unless (cl-some (lambda (buf)
                         (and (not (eq buf (current-buffer)))
                              (buffer-live-p buf)
                              (with-current-buffer buf
                                (and (eq major-mode 'q-mode)
                                     (equal (q--project-key) key)))))
                       (buffer-list))
        (remhash key q--project-cache)))))

;; completion at point

(defconst q--capf-keyword-set
  (make-hash-table :test #'equal)
  "Hash set of q keywords for O(1) lookup during completion annotation.")

(defconst q--capf-builtin-set
  (make-hash-table :test #'equal)
  "Hash set of q builtins for O(1) lookup during completion annotation.")

;; Populate the hash sets once at load time from the existing word lists.
;; This gives O(1) membership tests in q--capf-kind rather than O(n) member calls.
(dolist (w q-keyword-list)
  (puthash w t q--capf-keyword-set))
(dolist (w (append q-builtin-word-list q-builtin-dot-z-word-list
                   q-builtin-dot-Q-word-list q-builtin-dot-h-word-list
                   q-builtin-dot-j-word-list))
  (puthash w t q--capf-builtin-set))

(defun q--capf-kind (candidate)
  "Return a Company completion kind symbol for CANDIDATE."
  (let ((entry (car (q--entries-for-identifier candidate :definition-index))))
    (cond
     (entry                                   (if (plist-get entry :signature)
                                                  'function
                                                'variable))
     ((gethash candidate q--capf-keyword-set) 'keyword)
     ((gethash candidate q--capf-builtin-set) 'builtin))))

(defun q--capf-annotation (candidate)
  "Return an annotation string for completion CANDIDATE."
  (when-let (kind (q--capf-kind candidate))
    (concat " <" (symbol-name kind) ">")))

(defun q--capf-doc-buffer (candidate)
  "Return a documentation buffer for completion CANDIDATE.
Used by Company and Corfu to populate the popup doc window (C-h).
Shows the function signature, doc comment, and source location."
  (let ((entry (car (q--entries-for-identifier candidate :definition-index))))
    (when entry
      (let ((sig      (plist-get entry :signature))
            (doc      (plist-get entry :doc))
            (file     (plist-get entry :file))
            (line     (plist-get entry :line))
            (buffer   (plist-get entry :buffer)))
        (with-current-buffer (get-buffer-create " *q-doc*")
          (erase-buffer)
          (when sig  (insert sig "\n\n"))
          (when doc  (insert doc "\n\n"))
          (cond (file   (insert (format "%s:%d" (file-name-nondirectory file) line)))
                (buffer (insert (format "%s" (buffer-name buffer)))))
          (current-buffer))))))

(defun q--complete-with-action (string predicate action)
  "Perform q completion according to ACTION.
STRING and PREDICATE are used as in `try-completion'."
  (q--ensure-project-scan-cache)
  (complete-with-action action
                        (or (q--project-plist-get :completion-candidates)
                            q-capf-core-words)
                        string predicate))

(defun q-completion-at-point ()
  "Provide completion candidates for q symbols."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds) (cdr bounds)
            #'q--complete-with-action
            :exclusive           'no
            :annotation-function #'q--capf-annotation
            :company-kind        #'q--capf-kind
            :company-doc-buffer  #'q--capf-doc-buffer))))

;; xref backend

(defun q--entry->xref (entry)
  "Convert cached ENTRY plist into an xref object."
  (let ((summary (plist-get entry :summary))
        (file (plist-get entry :file))
        (buffer (plist-get entry :buffer)))
    (xref-make summary
               (if file
                   (xref-make-file-location file
                                            (plist-get entry :line)
                                            (plist-get entry :col))
                 (xref-make-buffer-location (or buffer (current-buffer))
                                            (plist-get entry :pos))))))

(defun q--entries-for-identifier (identifier index-key)
  "Return raw cache entries for IDENTIFIER from the shared INDEX-KEY.
INDEX-KEY is a plist keyword such as :definition-index or :reference-index."
  (q--ensure-project-scan-cache)
  (let ((index (q--project-plist-get index-key)))
    (when (hash-table-p index)
      (gethash (q--canonicalize-name (q--namespace-at-point) identifier)
               index))))

(defun q--identifier-at-point ()
  "Return q identifier at point, or nil when unavailable."
  (thing-at-point 'symbol t))

(defun q-xref-backend ()
  "Return xref backend for `q-mode'."
  'q)

(when (featurep 'xref)
  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql q)))
    (q--identifier-at-point))

  (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql q)))
    #'q--complete-with-action)

  (cl-defmethod xref-backend-definitions ((_backend (eql q)) identifier)
    (mapcar #'q--entry->xref
            (q--entries-for-identifier identifier :definition-index)))

  (cl-defmethod xref-backend-references ((_backend (eql q)) identifier)
    (mapcar #'q--entry->xref
            (q--entries-for-identifier identifier :reference-index)))

  (cl-defmethod xref-backend-apropos ((_backend (eql q)) pattern)
    (q--ensure-project-scan-cache)
    (let ((index (q--project-plist-get :definition-index))
          results)
      (when (hash-table-p index)
        (maphash (lambda (name entries)
                   (when (string-match-p pattern name)
                     (dolist (entry entries)
                       (push (q--entry->xref entry) results))))
                 index))
      results)))

;; eldoc

(defun q-eldoc-function (&rest _ignored)
  "Return a signature and doc string for the definition at point.
Combines the parsed signature (e.g. fname[a;b;c]) with any inline doc
comment, matching the convention used by most language modes.
For variable definitions with no signature, falls back to the raw
definition line so the value remains visible without jumping to it.
This function never triggers I/O; it only reads from cached data."
  (let* ((identifier (q--identifier-at-point))
         (entry (and identifier
                     (car (q--entries-for-identifier identifier :definition-index)))))
    (when entry
      (let ((sig (plist-get entry :signature))
            (doc (plist-get entry :doc)))
        (cond ((and sig doc) (concat sig "  " doc))
              (sig           sig)
              (doc           doc)
              (t             (plist-get entry :summary)))))))

;; modes

;;;###autoload
(define-derived-mode q-shell-mode comint-mode "Q-Shell"
  "Major mode for interacting with a q interpreter."
  :syntax-table q-mode-syntax-table
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m nil t)
  (setq-local comint-prompt-regexp "^\\(q)+\\|[^:]*:[0-9]+>\\)")
  (setq-local font-lock-defaults q-font-lock-defaults)
  (setq-local syntax-propertize-function #'q-syntax-propertize)
  (setq-local comint-process-echoes nil))

(defconst q-imenu-generic-expression
  (list
   (list nil (concat "^" q-name-regex ":") 1))
  "Regular expressions to get q expressions into imenu.")

(with-eval-after-load 'hideshow
  (add-to-list 'hs-special-modes-alist
               (list 'q-mode "{" "}" "/[ \t]*" nil nil)))

(defun q-current-defun ()
  "Return the name of the q function enclosing point, or nil.
Used by `which-function-mode' and `add-log-current-defun-function'."
  (save-excursion
    (when (re-search-backward (concat "^" q-function-regex) nil t)
      (match-string-no-properties 1))))

;;;###autoload
(define-derived-mode q-mode prog-mode "Q-Script"
  "Major mode for editing q language files."
  :group 'q
  (setq-local font-lock-defaults q-font-lock-defaults)
  (setq-local syntax-propertize-function #'q-syntax-propertize)
  (setq-local comment-start q-comment-start)
  (setq-local comment-start-skip "\\(^\\|[ \t]\\)\\(/+[ \t]*\\)")
  (setq-local comment-end "")
  (setq-local indent-line-function 'q-indent-line)
  ;; enable imenu
  (setq-local imenu-generic-expression q-imenu-generic-expression)
  ;; which-function-mode
  (setq-local add-log-current-defun-function #'q-current-defun)
  ;; editor integrations
  (add-hook 'completion-at-point-functions #'q-completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'q-eldoc-function nil t)
  (when (featurep 'xref)
    (add-hook 'xref-backend-functions #'q-xref-backend nil t))
  (add-hook 'flymake-diagnostic-functions 'q-flymake nil t)
  ;; Schedule rescans on save/revert rather than inline on every eldoc tick.
  ;; Saves trigger an incremental rescan of the changed file only; out-of-band
  ;; disk changes (e.g. git pull) are detected and promote to a full rescan.
  (add-hook 'after-save-hook #'q--schedule-rescan nil t)
  (add-hook 'after-revert-hook #'q--schedule-rescan nil t)
  ;; Evict the shared project cache when the last buffer for a project closes.
  (add-hook 'kill-buffer-hook #'q--maybe-evict-project-cache nil t))

;; indentation

(defun q-indent-line ()
  "Indent current line as q."
  (let* ((savep (point))
         (indent (condition-case nil
                     (save-excursion
                       (forward-line 0)
                       (skip-chars-forward " \t")
                       (if (>= (point) savep) (setq savep nil))
                       (or (if (null q-indent-step)
                               (q-compute-indent-sexp)
                             (* q-indent-step (q-compute-indent-tab)))
                           0))
                   (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun q-compute-indent-sexp ()
  "Compute the indent for a line using sexp."
  (backward-up-list)
  (let ((savepos (point)))
    (beginning-of-line)
    (+ 1 (- savepos (point)))))

(defun q-compute-indent-tab ()
  "Compute the indent for a line using tabs."
  (let ((n 0)
        pos)
    (condition-case nil
        (while (progn (setq pos (point))
                      (backward-up-list)
                      (/= (point) pos))
          (setq n (+ n 1)))
      (scan-error n))))

(provide 'q-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[kq]\\'" . q-mode))

;;; q-mode.el ends here
