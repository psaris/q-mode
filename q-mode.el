;;; q-mode.el --- A q editing mode    -*- lexical-binding: t -*-

;; Copyright (C) 2006-2026 Nick Psaris <nick.psaris@gmail.com>
;; Keywords: faces files q
;; Package-Requires: ((emacs "24"))
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
;;  - variable and function indexing (imenu).
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
;; function, `C-c C-r' sends the selected region and `C-c C-b' sends
;; the whole buffer.  If prefixed with `C-u C-u', or pressing `C-c
;; M-j' `C-c M-f' `C-c M-r' respectively, will also switch point to
;; the active q process buffer for direct interaction.

;; If the source file exists on the same machine as the q process,
;; `C-c M-l' can be used to load the file associated with the active
;; buffer.

;; Quick access to variable and function definitions can be obtained
;; using the `imenu' binding `M-g i'.


;; `M-x customize-group' can be used to customize the `q' group.
;; Specifically, the `q-program' and `q-qcon-program' variables can be
;; changed depending on your environment.

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
   (unless (not q-init-garbage-collect) " -g 1")))

(defun q-qcon-default-args ()
  "Build the default qcon command-line argument string from `q-qcon-*' variables."
  (concat (format "%s:%s" q-qcon-server q-qcon-port)
          (unless (equal q-qcon-user "") (format ":%s:%s" q-qcon-user q-qcon-password))))

(defun q-shell-name (server port)
  "Build name of q-shell based on SERVER and PORT."
  (concat "q-"
          (if (equal server "") "localhost" server)
          (unless (equal port "") (format ":%s" port))))

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
        (message "Starting q with the following command: \"%s\"" cmd)
        (q-shell-mode)
        (setq args (list buffer "q" command nil switches))
        (setq process (get-buffer-process (apply 'comint-exec args)))

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
  (let* ((buffer (get-buffer-create (format "*%s*" (format "qcon-%s" args))))
         process)
    (when (called-interactively-p 'any) (pop-to-buffer buffer))
    (when (or current-prefix-arg (not (q-shell-buffer-p buffer)))
      (with-current-buffer buffer
        (message "Starting qcon with the following cmd: \"%s\"" (concat q-qcon-program " " args))
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
  (let ((buffer (process-buffer process)))
    (setq message (replace-regexp-in-string "[\r\n]+\\'" "" (or message "")))
    (setq message (format "\nProcess %s %s at %s\n"
                          (process-name process) message (current-time-string)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert-before-markers message)))))

(defun q-strip (text)                 ; order matters, don't rearrange
  "Strip TEXT of all trailing comments, newlines and excessive whitespace."
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
  (if (equal current-prefix-arg '(16)) (q-show-q-buffer)))

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

(defun q-eval-symbol-at-point ()
  "Evaluate current symbol."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (unless symbol
      (user-error "No symbol at point"))
    (q-send-string symbol)))

(defun q-eval-buffer ()
  "Load current buffer into the inferior q[con] process."
  (interactive)
  (q-eval-region (point-min) (point-max)))


(defvar q-symbol-regex
  "`\\(?:\\(?:\\w\\|[.]\\)\\(?:\\w\\|[_.]\\)*\\)?"
  "Regular expression used to find symbols.")

(defvar q-file-regex
  (concat q-symbol-regex ":\\(?:\\w\\|[/:_.]\\)*")
  "Regular expression used to find files.")

(defvar q-name-regex
  "\\_<\\([.]?[a-zA-Z]\\(?:\\w\\|[_.]\\)*\\)\\s-*"
  "Regular expression used to find variable or function names.")

(defvar q-function-regex
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

(defvar q-variable-regex
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

(defvar q-keywords
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

(defvar q-builtin-words
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

(defvar q-builtin-dot-z-words
  (concat "\\_<"
          "\\(?:[_]\\)?"                ; leading _ is not a symbol
          (regexp-opt q-builtin-dot-z-word-list t)
          "\\_>")
  "Builtin .z functions/constants regex defined for q mode.")

(defvar q-font-lock-keywords ;; keywords
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

(defvar q-font-lock-keywords-1          ; symbols
  (append q-font-lock-keywords
          (list
           (list q-keywords 1 'font-lock-keyword-face nil) ; select from
           '("\\b[0-2]:" . font-lock-builtin-face)         ; IO/IPC
           (list q-builtin-words 1 'font-lock-builtin-face nil) ; q.k
           (list q-builtin-dot-z-words 1 'font-lock-builtin-face nil) ; .z.*
           ))
  "More highlighting expressions for q mode.")

(defvar q-font-lock-keywords-2 ; function/variable names and literals
  (append q-font-lock-keywords-1
          (list
           (list q-function-regex 1 'font-lock-function-name-face nil) ; functions
           (list q-variable-regex 1 font-lock-variable-name-face nil) ; variables
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

(defvar q-font-lock-defaults
  '((q-font-lock-keywords q-font-lock-keywords-1 q-font-lock-keywords-2)
    nil nil nil nil
    (font-lock-syntactic-keywords . (("^\\(\/\\)\\s-*$"     1 "< b") ; begin multiline comment /
                                     ("^\\(\\\\\\)\\s-*$"   1 "> b") ; end multiline comment   \
                                     ("\\(?:^\\|[ \t]\\)\\(\/\\)"    1 "<  ") ; comments start flush left or after white space
                                     ("\\(\"\\)\\(?:[^\"\\\\]\\|\\\\.\\)*?\\(\"\\)" (1 "\"") (2 "\""))
                                     )))
  "List of font lock keywords to properly highlight q syntax.")


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
    (modify-syntax-entry ?\= ".  " table) ; treat < as punctuation
    (modify-syntax-entry ?\* ".  " table) ; treat * as punctuation
    (modify-syntax-entry ?\< ".  " table) ; treat < as punctuation
    (modify-syntax-entry ?\> ".  " table) ; treat > as punctuation
    (modify-syntax-entry ?\| ".  " table) ; treat | as punctuation
    (modify-syntax-entry ?\` "_  " table) ; treat ` as symbol
    table)
  "Syntax table for `q-mode'.")


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
        (file (buffer-file-name)))
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
  (append q-keyword-list q-builtin-word-list q-builtin-dot-z-word-list)
"Core words used for q completion candidates.")

(defvar-local q--project-definition-index-cache nil
  "Cached map of canonical name to definition entries.")

(defvar-local q--project-reference-index-cache nil
  "Cached map of canonical name to reference entries.")

(defvar-local q--project-completion-candidates-cache nil
  "Cached list of completion candidates for current scope.")

(defvar-local q--project-scan-cache-state nil
  "State signature for project scan caches.")

(defconst q--identifier-token-regex (concat q-name-regex "\\_>")
  "Regex matching q identifiers for reference scanning.")

(defconst q--namespace-command-regex "^\\\\d\\s-+\\([^ \t\n]+\\)"
  "Regex matching q namespace switch commands.")

(defun q--scannable-q-file-p (file)
  "Return non-nil when FILE is a scannable q source file."
  (and (string-match-p "\\.[kq]\\'" file)
       (not (string-prefix-p ".#" (file-name-nondirectory file)))
       (file-regular-p file)
       (file-readable-p file)))

(defun q--project-q-files ()
  "Return q source files for current project, or current file when available."
  (let ((project-files
         (when (featurep 'project)
           (let ((project (project-current nil)))
             (when project (project-files project))))))
    (if project-files
        (cl-remove-if-not #'q--scannable-q-file-p project-files)
      (let ((file (buffer-file-name)))
        (and file (q--scannable-q-file-p file) (list file))))))

(defun q--canonicalize-name (namespace name)
  "Return canonical fully-scoped NAME using NAMESPACE context."
  (if (char-equal ?. (aref name 0))
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

(defun q--make-entry (meta &optional file)
  "Return scanner entry from META with optional FILE location."
  (append (list :summary (plist-get meta :summary))
          (if file
              (list :file file
                    :line (plist-get meta :line)
                    :col (plist-get meta :col))
            (list :buffer (current-buffer)
                  :pos (plist-get meta :pos)))))

(defun q--scan-source-in-current-buffer (&optional file)
  "Return scan artifacts from current buffer, optionally for FILE."
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
                     (entry (q--make-entry meta file)))
                (puthash canonical (cons entry (gethash canonical def-index)) def-index)
                (push canonical symbols)))
            (while (re-search-forward q--identifier-token-regex line-end t)
              (let* ((name (match-string-no-properties 1))
                     (ref-pos (match-beginning 1))
                     (canonical (q--canonicalize-name namespace name))
                     (meta (make-meta ref-pos))
                     (entry (q--make-entry meta file)))
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
          :symbols (delete-dups symbols))))

(defun q--scan-file-artifacts (file)
  "Return scan artifacts for FILE."
  (let ((buf (find-buffer-visiting file)))
    (if (and buf (buffer-modified-p buf))
        (with-current-buffer buf
          (save-excursion
            (q--scan-source-in-current-buffer)))
      (with-temp-buffer
        (condition-case nil
            (progn
              (insert-file-contents file)
              (save-excursion
                (q--scan-source-in-current-buffer file)))
          (file-missing
           (list :definitions (make-hash-table :test #'equal)
                 :references (make-hash-table :test #'equal)
                 :symbols nil)))))))

(defun q--merge-index! (dest src)
  "Destructively merge SRC hash index into DEST hash index."
  (maphash
   (lambda (name entries)
     (puthash name (nconc (gethash name dest) entries) dest))
   src))

(defun q--source-file-state (file)
  "Return state signature for FILE considering unsaved visiting buffers."
  (let ((buf (find-buffer-visiting file)))
    (if (and buf (buffer-modified-p buf))
        (with-current-buffer buf
          (list :buffer (buffer-chars-modified-tick)))
      (condition-case nil
          (list :file (file-attribute-modification-time
                       (file-attributes file)))
        (file-missing
         (list :missing t))))))

(defun q--compute-project-scan-cache-state (files)
  "Return cache state signature for project scan caches over FILES."
  (if files
      (mapcar (lambda (file)
                (cons file (q--source-file-state file)))
              (sort (copy-sequence files) #'string<))
    (list :buffer-tick (buffer-chars-modified-tick))))

(defun q--ensure-project-scan-cache ()
  "Ensure project scan caches are populated and fresh."
  (let* ((files (q--project-q-files))
         (state (q--compute-project-scan-cache-state files)))
    (unless (equal state q--project-scan-cache-state)
      (let ((def-index (make-hash-table :test #'equal))
            (ref-index (make-hash-table :test #'equal))
            (symbols nil))
        (if files
            (dolist (file files)
              (let ((artifacts (q--scan-file-artifacts file)))
                (q--merge-index! def-index (plist-get artifacts :definitions))
                (q--merge-index! ref-index (plist-get artifacts :references))
                (setq symbols (nconc symbols (plist-get artifacts :symbols)))))
          (save-excursion
            (let ((artifacts
                   (q--scan-source-in-current-buffer)))
              (setq def-index (plist-get artifacts :definitions)
                    ref-index (plist-get artifacts :references)
                    symbols (plist-get artifacts :symbols)))))
        (setq q--project-scan-cache-state state
              q--project-definition-index-cache def-index
              q--project-reference-index-cache ref-index
              q--project-completion-candidates-cache
              (delete-dups (append q-capf-core-words symbols)))))))

(defun q--completion-candidate-list ()
  "Return completion candidates for current scope."
  (q--ensure-project-scan-cache)
  q--project-completion-candidates-cache)

(defun q--complete-with-action (string predicate action)
  "Perform q completion according to ACTION.
STRING and PREDICATE are used as in ‘try-completion’."
  (complete-with-action action (q--completion-candidate-list) string predicate))

(defun q-completion-at-point ()
  "Provide completion candidates for q symbols."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds) (cdr bounds)
            #'q--complete-with-action
            :exclusive 'no))))

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

(defun q--identifier-target (identifier)
  "Return canonical project symbol name for IDENTIFIER at point."
  (q--canonicalize-name (q--namespace-at-point) identifier))

(defun q--entries-for-identifier (identifier index-cache-var)
  "Return raw cache entries for IDENTIFIER from INDEX-CACHE-VAR symbol."
  (q--ensure-project-scan-cache)
  (let ((index-cache (symbol-value index-cache-var)))
    (when (hash-table-p index-cache)
      (gethash (q--identifier-target identifier) index-cache))))

(defun q--find-xrefs (identifier index-cache-var)
  "Return xrefs for IDENTIFIER from INDEX-CACHE-VAR symbol."
  (mapcar #'q--entry->xref
          (q--entries-for-identifier identifier index-cache-var)))

(defun q--find-definitions (identifier)
  "Return xref definitions for IDENTIFIER in active scope."
  (q--find-xrefs identifier 'q--project-definition-index-cache))

(defun q--find-references (identifier)
  "Return xref references for IDENTIFIER in active scope."
  (q--find-xrefs identifier 'q--project-reference-index-cache))

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
    (q--find-definitions identifier))

  (cl-defmethod xref-backend-references ((_backend (eql q)) identifier)
    (q--find-references identifier)))

;; modes

;;;###autoload
(define-derived-mode q-shell-mode comint-mode "Q-Shell"
  "Major mode for interacting with a q interpreter."
  :syntax-table q-mode-syntax-table
  (add-hook (make-local-variable 'comint-output-filter-functions) 'comint-strip-ctrl-m)
  (setq-local comint-prompt-regexp "^\\(q)+\\|[^:]*:[0-9]+>\\)")
  (setq-local font-lock-defaults q-font-lock-defaults)
  (setq-local comint-process-echoes nil)
  )

(defvar q-imenu-generic-expression
  (list
   (list nil (concat "^" q-name-regex ":") 1))
  "Regular expressions to get q expressions into imenu.")

;;;###autoload
(define-derived-mode q-mode prog-mode "Q-Script"
  "Major mode for editing q language files."
  :group 'q
  (setq-local font-lock-defaults q-font-lock-defaults)
  (setq-local comment-start q-comment-start)
  (setq-local comment-start-skip "\\(^\\|[ \t]\\)\\(/+[ \t]*\\)")
  (setq-local comment-end "")
  (setq-local indent-line-function 'q-indent-line)
  ;; enable imenu
  (setq-local imenu-generic-expression q-imenu-generic-expression)
  ;; editor integrations
  (add-hook 'completion-at-point-functions #'q-completion-at-point nil t)
  (when (featurep 'xref)
    (add-hook 'xref-backend-functions #'q-xref-backend nil t))
  (add-hook 'flymake-diagnostic-functions 'q-flymake nil t)
  )

;;; Indentation

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
            (setq n (+ n 1))
            n)
        (scan-error n))))

(provide 'q-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[kq]\\'" . q-mode))

;;; q-mode.el ends here
