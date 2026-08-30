;;; q-mode.el --- A q editing mode    -*- lexical-binding: t -*-

;; Copyright (C) 2006-2026 Nick Psaris <nick.psaris@gmail.com>
;; Keywords: faces files q
;; Package-Requires: ((emacs "28"))
;; Created: 8 Jun 2015
;; Version: 0.2
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
;;  - inline eval results next to the code that produced them (`q-inline-mode'),
;;
;;  - native Emacs qcon replacement supporting TLS (`q-con'),
;;
;;  - secure password retrieval (auth-source),
;;
;;  - named remote connections (q-connections-alist),
;;
;;  - incremental, project-wide indexing (imenu, xref),
;;
;;  - completion at point (CAPF),
;;
;;  - signature help (eldoc),
;;
;;  - current function in the mode line (which-function-mode),
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
;; process.  Both can be prefixed with the universal-argument `C-u' to
;; customize the arguments used to start the processes.

;; `M-x q-con' talks to an existing q process the same way `q-qcon'
;; does, but without executing an external qcon binary: Emacs opens
;; the TCP connection itself.  This matters for the password - qcon
;; receives it as a literal command-line argument, so it's visible to
;; anyone on the machine running `ps'; `q-con' resolves it from
;; auth-source only for the instant it takes to write it to the
;; socket, and it never becomes a command-line argument at all.
;; `q-con' also supports TLS: prefix a host with `tcps://' - in
;; `q-connection-host', a `q-connections-alist' entry, or typed ad-hoc at the
;; prompt - to connect over TLS instead of plain tcp.

;; When prompted this way, `q-qcon' and `q-con' both offer named
;; connections from `q-connections-alist' as completion candidates,
;; alongside the option of typing an ad-hoc "host:port[:user]" string.
;; Each entry in `q-connections-alist' is a (NAME HOST PORT USER) list,
;; letting you refer to a remote q server by a short name instead of
;; retyping its host/port/user every time.  In every case, the
;; password itself is never typed or stored in `q-connections-alist' - it's
;; always resolved from auth-source.  `.netrc'/`.authinfo' is the
;; common case, but auth-source is backend-agnostic: anything
;; registered as an `auth-source-backend' (e.g. the system Secret
;; Service/macOS Keychain via `auth-source-pass' or `secrets.el', or a
;; custom backend you write yourself) is consulted the same way, so
;; the password need not live in a plaintext file at all.

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
;; q process buffer for direct interaction.  Evaluations performed in
;; non-root namespaces are pre/post-fixed with a command to change
;; directory.

;; If the source file exists on the same machine as the q process,
;; `C-c M-l' can be used to load the file associated with the active
;; buffer.

;; `M-x q-inline-mode' toggles showing eval results as an overlay next
;; to the code that produced them, in addition to the usual q[con]
;; buffer output.  `C-c C-.' pops up the full, untruncated reply for
;; the result at point (useful for tables, which are shown truncated
;; to their first line inline); `C-c C-k' clears the result at point,
;; and `C-u C-c C-k' clears every result in the buffer.  Editing the
;; code a result is attached to clears that result automatically.  The
;; `q-inline-duration' variable controls how long a result stays
;; visible: nil (the default) leaves it until the code is edited or
;; explicitly cleared, a number of seconds auto-clears it after that
;; long, and the symbol `command' clears it as soon as any command
;; runs.

;; `C-c C-g' triggers a manual rescan of the project, re-scanning only
;; files whose mtime has changed.  Prefix with `C-u' to force all files
;; to be re-scanned regardless of mtime, which is useful after a branch
;; switch where file timestamps may be preserved.

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

;; WARNING: The `q-program' cannot use `rlwrap'.  The usage of
;; `rlwrap' creates many issues, the most obvious is that the `q)'
;; prompt can no longer be observed.

;; Q-mode indents each level based on `q-indent-step'.  To indent code
;; based on {}-, ()-, and []-groups instead of equal width tabs, you
;; can set this value to nil.

;; Flymake behavior is controlled by `q-flymake-on-save'.  When
;; non-nil, checks run only after saving.  When nil (default), checks
;; run for unsaved buffers by evaluating a temporary file containing
;; current buffer contents.


(require 'cl-lib)
(require 'subr-x)
(require 'comint)
(require 'compile)
(require 'auth-source nil t)
(require 'hideshow nil t)
(require 'project nil t)
(require 'xref nil t)

;;; Code:

;; local variable for the flymake-dedicated q process
(defvar-local q--flymake-proc nil)

;; local to each q-shell-mode buffer: pending replies awaiting a match, FIFO.
;; Each entry is (SOURCE-BEG SOURCE-END REPLY-START-MARKER):
;;   SOURCE-BEG/END     - span of source-buffer text that produced the
;;                        request, or both nil if none was given - see
;;                        `q-send-string'.  Passed through verbatim to
;;                        `q-reply-functions'; this file attaches no
;;                        meaning to it beyond that.
;;   REPLY-START-MARKER - where in this shell buffer the reply begins,
;;                        i.e. point-max at the moment the request was sent.
;; See `q-send-string' (producer) and `q--reply-filter' (consumer).
(defvar-local q--reply-queue nil)

(defun q--reply-queue-clear ()
  "Discard every entry in `q--reply-queue', releasing its markers.
Used wherever pending replies can no longer be trusted to arrive, or
to arrive in order: on process death (`q--process-sentinel')."
  (dolist (entry q--reply-queue)
    (dolist (marker entry) (when marker (set-marker marker nil))))
  (setq q--reply-queue nil))

(defgroup q nil "Major mode for editing q code." :group 'languages)

(defgroup q-connection nil "Q remote connection arguments." :group 'q)

(defcustom q-connections-alist nil
  "Alist of named q connections.
Each element is (NAME HOST PORT USER); USER may be \"\".  Password is
not stored here — it is resolved from auth-source at connect time by
`q--connection-resolve-credentials'.  Auth-source is not tied to
`.netrc'/`.authinfo': any configured auth-source-backend (Secret
Service, macOS Keychain, a custom backend, etc.) works the same way,
since `q--connection-resolve-credentials' just calls
`auth-source-search', which is backend-agnostic."
  :type '(alist :key-type (string :tag "name")
                :value-type (list (string :tag "host")
                                   (string :tag "port")
                                   (string :tag "user")))
  :group 'q-connection)

(defcustom q-program "q"
  "Program name for invoking an inferior q."
  :type 'file
  :group 'q)

(defcustom q-qcon-program "qcon"
  "Program name for invoking an inferior qcon."
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
  "Length of indent used by `q-indent-line'.
If nil, code is aligned to {}-, ()-, and []-groups.  Otherwise,
each level is indented by this amount."
  :type '(choice (const nil) integer)
  :group 'q)

(defcustom q-comment-start "/"
  "String to insert to start a new comment (some prefer a double forward slash).
Note: this only affects comment insertion and recognition by commands such
as `comment-region' and `comment-dwim'.  Syntax highlighting and sexp
navigation always treat a bare / as the comment delimiter."
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

(defcustom q-allow-shell-buffer nil
  "If non-nil, allow any live comint buffer as the active q buffer.
This lets you point `q-active-buffer' at a `*shell*' buffer (or
similar) that is running a q process directly, even though it is
not a `q-shell-mode' buffer.  When nil (the default) only buffers
in `q-shell-mode' are accepted."
  :safe 'booleanp
  :type 'boolean
  :group 'q)

(defcustom q-eval-prefix "(::) "
  "Prefixed onto every expression `q-eval-symbol/line/region' sends.
Empty string disables this entirely.  The default, `(::) ', is q's
identity function and causes the result (even assignment) to be
displayed."
  :safe 'stringp
  :type 'string
  :group 'q)

(defun q--eval-prefix (string)
  "Prefix STRING with `q-eval-prefix'.
If STRING is empty or a `\\'-prefixed system command, STRING is returned
unmodified."
  (if (and (not (string-empty-p string))
           (not (string-prefix-p "\\" string)))
      (concat q-eval-prefix string)
    string))

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
  "If non-nil, Q-Shell starts with q's -g 1 (immediate garbage collection).
Nil starts with the default, -g 0 (deferred).  q's -g flag only ever
takes these two modes, so this is a plain boolean rather than an
integer, unlike the other `q-init-*' variables it sits alongside."
  :safe 'booleanp
  :type 'boolean
  :group 'q-init)

(defcustom q-init-file ""
  "If non-empty, Q-Shell will load the specified file."
  :type 'file
  :group 'q-init)

(define-obsolete-variable-alias 'q-qcon-server 'q-connection-host "0.2")
(define-obsolete-variable-alias 'q-qcon-host 'q-connection-host "0.2")
(defcustom q-connection-host ""
  "Remote q server."
  :safe 'stringp
  :type 'string
  :group 'q-connection)

(define-obsolete-variable-alias 'q-qcon-port 'q-connection-port "0.2")
(defcustom q-connection-port 5000
  "Port for remote q server."
  :safe 'integerp
  :type 'integer
  :group 'q-connection)

(define-obsolete-variable-alias 'q-qcon-user 'q-connection-user "0.2")
(defcustom q-connection-user ""
  "If non-nil, log in to the remote q server with this id."
  :safe 'stringp
  :type 'string
  :group 'q-connection)

(defun q-customize ()
  "Customize `q-mode'."
  (interactive)
  (customize-group "q"))

(defvar q-active-buffer nil
  "The q-shell buffer to send q commands.")

(defun q-activate-this-buffer ()
  "Set the `q-active-buffer' to the currently active buffer."
  (interactive)
  (q-activate-buffer (current-buffer)))

(defun q--shell-buffer-p (buffer)
  "Return non-nil if BUFFER is a live Q shell buffer.
BUFFER can be a buffer object, buffer name, or cons cell from completion.
When `q-allow-shell-buffer' is non-nil, any live comint buffer with a
running process is accepted, not just `q-shell-mode' buffers."
  (let* ((target (if (consp buffer) (car buffer) buffer))
         (buf (and target (get-buffer target))))
    (and buf
         (buffer-live-p buf)
         (comint-check-proc buf)
         (with-current-buffer buf
           (or (derived-mode-p 'q-shell-mode)
               (and q-allow-shell-buffer
                    (derived-mode-p 'comint-mode)))))))

(defun q-activate-buffer (buffer)
  "Set the `q-active-buffer' to the supplied BUFFER.
Prompt with a list of live Q Shell buffers if called interactively."
  (interactive
   (list (read-buffer "activate buffer: "
                      nil
                      t
                      #'q--shell-buffer-p)))
  (when (called-interactively-p 'any) (display-buffer buffer))
  (setq q-active-buffer (get-buffer buffer)))

(defun q--default-args ()
  "Build the default q command-line argument string from `q-init-*' variables."
  (concat
   (unless (equal q-init-file "") (format " %s" (shell-quote-argument q-init-file)))
   (unless (equal q-init-port 0) (format " -p %s" q-init-port))
   (unless (equal q-init-slaves 0) (format " -s %s" q-init-slaves))
   (unless (equal q-init-workspace 0) (format " -w %s" q-init-workspace))
   (when q-init-garbage-collect " -g 1")))

;; `q-con' and `q-qcon' share one (HOST PORT USER ALIAS TLS) tuple through
;; their prompt/default/resolve/format pipeline:
;;   HOST  - always already stripped of any tcp[s]:// scheme prefix.
;;   PORT  - a number or numeric string.
;;   USER  - "" when unset.
;;   ALIAS - a matched `q-connections-alist' entry name, or nil.
;;   TLS   - non-nil only for an explicit tcps:// prefix, parsed out of
;;           HOST exactly once, then carried alongside rather than
;;           re-derived later by re-parsing HOST.
;; A password is never accepted as typed input; it's only ever resolved
;; from auth-source, at most once per connection attempt.

(defun q--connection-default-args ()
  "Return the default q connection args as a plist.
Keys are :host :port :user :alias :tls, built from the `q-connection-*'
variables.  :alias is always nil here, since no `q-connections-alist'
selection happens on this path."
  (let* ((parsed (q--parse-host-scheme q-connection-host))
         (tls (car parsed))
         (host (cdr parsed)))
    (list :host host :port q-connection-port :user q-connection-user :alias nil :tls tls)))

(defun q--connection-resolve-credentials (host port user)
  "Resolve a (LOGIN . PASSWORD) cons for HOST/PORT, given USER.
USER may be \"\" (unset).  Looks up auth-source by HOST+PORT+USER when
USER is known, or HOST+PORT when it's blank (so a netrc entry can supply
the login) — passing `:user' as nil to `auth-source-search' is treated
the same as omitting the keyword entirely, unlike passing \"\", which
would only match an entry whose login is literally empty.  A
.netrc/.authinfo entry with no `port' token matches regardless of PORT,
so this covers plain host-scoped entries too, without a separate
fallback query.  LOGIN falls back to USER, or \"\", when nothing
matches; PASSWORD is nil then."
  (let* ((have-user (and user (not (equal user ""))))
         (creds (when (featurep 'auth-source)
                  (car (auth-source-search :host host :port port
                                            :user (and have-user user) :max 1)))))
    (cons (if have-user user (or (and creds (plist-get creds :user)) ""))
          (and creds (auth-info-password creds)))))

(defun q--connection-names ()
  "Return the configured connection names from `q-connections-alist'."
  (mapcar #'car q-connections-alist))

(defun q--connection-prompt (prompt default)
  "Use PROMPT to select `q-connections-alist' pre-filled with DEFAULT.
Returns a 5-item list (NAME HOST PORT USER TLS): NAME and HOST/PORT/USER
come from a matched `q-connections-alist' entry, or from splitting ad-hoc
input on \":\" when nothing matches (missing fields default to \"\").
Either way, HOST's tcp[s]:// scheme is parsed exactly once here, before
any colon-splitting happens - splitting \"tcps://host:5000\" on \":\"
first, without this, would misread \"tcps\" as HOST and \"//host\" as
PORT.  A 4th field is rejected as an attempted password with a
`user-error'; a matched entry always yields exactly three fields, so
this can't misfire for it.  An empty PORT is likewise rejected - a q
connection always needs one, unlike a local q process."
  (let* ((choice (completing-read prompt (q--connection-names) nil nil nil nil default))
         (entry (assoc choice q-connections-alist))
         (parsed (q--parse-host-scheme (if entry (nth 1 entry) choice)))
         (tls (car parsed))
         (fields (if entry
                     (list (cdr parsed) (nth 2 entry) (nth 3 entry))
                   (split-string (cdr parsed) ":"))))
    (when (> (length fields) 3)
      (user-error
       (concat "Refusing typed password; typing a password here leaves "
               "it sitting in the minibuffer (and in `savehist' if enabled). "
               "Add an entry to your .netrc/.authinfo file instead")))
    (let ((port (or (nth 1 fields) "")))
      (when (equal port "")
        (user-error "A port is required (e.g. \"host:port[:user]\")"))
      (list (car entry) (nth 0 fields) port (or (nth 2 fields) "") tls))))

(defun q--qcon-resolve-args (host port user)
  "Resolve credentials for HOST/PORT/USER, returning (HOST PORT LOGIN PASSWORD)."
  (let* ((resolved (q--connection-resolve-credentials host port user))
         (login (car resolved))
         (password (or (cdr resolved) "")))
    (list host port login password)))

(defun q--connection-display-args (host port user &optional tls)
  "Return \"[tcps://]host:port[:user]\" for HOST, PORT, USER, with no password.
HOST is expected already scheme-stripped; TLS, when non-nil, prefixes
it back with \"tcps://\" for display.  Never resolves credentials, so
it's safe anywhere a password shouldn't be shown yet - messages,
minibuffer prompt defaults."
  (concat (and tls "tcps://")
          (format "%s:%s" host port)
          (unless (equal user "") (format ":%s" user))))

(defun q--con-default-args ()
  "Build the default q connection args plist for `q-con'.
See `q--connection-default-args' for the shape."
  (q--connection-default-args))

(defun q--connection-prompt-args ()
  "Prompt for a q connection, returning a plist.
Keys are :host :port :user :alias :tls (see `q--connection-default-args'
for the shape).  Offers `q-connections-alist' as completion candidates
alongside an ad-hoc \"host:port[:user]\" string.  The minibuffer default
is built by parsing `q-connection-host' for its scheme here, rather than
reusing an already-scheme-stripped default - so a configured tcps://
still shows in the default, and accepting it as-is preserves TLS
instead of silently dropping it."
  (let* ((parsed (q--parse-host-scheme q-connection-host))
         (default (q--connection-display-args
                   (cdr parsed) q-connection-port q-connection-user (car parsed)))
         (result (q--connection-prompt
                  "q connection (name, or host:port[:user]): " default)))
    ;; result is (NAME HOST PORT USER TLS); NAME becomes :alias.
    (cl-destructuring-bind (name host port user tls) result
      (list :host host :port port :user user :alias name :tls tls))))

(defun q--setup-shell-buffer (process)
  "Set up current q shell buffer for PROCESS.
Input history is shared across every kind of q buffer, in a single
`~/.q_history', since what's recorded is just the q expressions typed
at the prompt, regardless of how this buffer's process is reached."
  (setq comint-input-ring-file-name (expand-file-name "~/.q_history"))
  (comint-read-input-ring t)
  (set-process-sentinel process 'q--process-sentinel))

(defun q--format-buffer-name (type &optional host port alias tls)
  "Return a standard `q-mode' buffer name.
TYPE is one of :shell, :con, or :qcon; HOST and PORT are appended for
remote connections and optionally for a local process.  ALIAS, when
non-nil, is shown in brackets before HOST/PORT - it's the caller's job
to supply it; this function doesn't search `q-connections-alist' itself.
TLS, when non-nil, prefixes HOST with \"tcps://\"."
  (let* ((type-str (substring (symbol-name type) 1))
         (parsed (when host (q--parse-host-scheme host)))
         (clean-host (cdr parsed))
         (port-str (when port (number-to-string (q--con-port-number port)))))
    (concat "*q-" type-str
            (when (and clean-host port-str)
              (concat ":"
                      (when (and alias (not (string-empty-p alias)))
                        (concat " [" alias "]"))
                      " " (and tls "tcps://") clean-host ":" port-str))
            "*")))

;;;###autoload
(defun q (&optional host user args)
  "Start a new q process.
The optional argument HOST and USER allow the q process to be
started on a remote machine.  The optional ARGS argument
specifies the command line args to use when executing q; the
default ARGS are obtained from the q-init customization
variables.  In interactive use, a prefix argument directs this
command to read the command line arguments from the minibuffer."
  (interactive (let* ((args (q--default-args))
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
                 (and (string-match "-p *\\([0-9]+\\)" args) (match-string 1 args))))
         (buffer (get-buffer-create (q--format-buffer-name :shell host port)))
         (command (if qs "ssh" (or shell-file-name (getenv "SHELL") "/bin/sh")))
         (switches (append (if qs (list "-t" host) (list "-c")) (list cmd)))
         ;; disable kdb-x rlwrap functionality
         (process-environment (cons "KX_LINE=0" process-environment))
         process)
    (when (called-interactively-p 'any) (pop-to-buffer buffer))
    (when (or current-prefix-arg (not (q--shell-buffer-p buffer)))
      (with-current-buffer buffer
        (message "q: starting q with command \"%s\"" cmd)
        (q-shell-mode)
        (let ((comint-args (list buffer "q" command nil switches)))
          (setq process (get-buffer-process (apply 'comint-exec comint-args))))
        (q--setup-shell-buffer process)))
    (q-activate-buffer buffer)
    (get-buffer-process buffer)))

(defun q--qcon-format-args (host port user password)
  "Join HOST, PORT, USER, and PASSWORD into a qcon args string.
USER \"\" means no credentials were resolved, so none are appended -
PASSWORD is only ever included alongside a non-empty USER.  This is the
one place the real password is written back out, and it's only ever
handed to a child qcon process, never to a buffer name or a message."
  (concat (format "%s:%s" host port)
          (unless (equal user "") (format ":%s:%s" user password))))

(defun q--qcon-redact-args (host port user password)
  "Join HOST, PORT, and USER into a qcon-style args string, password redacted.
PASSWORD itself is never even read; only its presence alongside a
non-empty USER decides whether \"****\" is appended in its place.  Safe
anywhere the real password shouldn't appear - buffer names, messages."
  (ignore password)
  (concat (format "%s:%s" host port)
          (unless (equal user "") (format ":%s:****" user))))

(defun q--start-connection-buffer (buffer-name interactive-call message start-process-fn mode)
  "Shared buffer-management core for starting or reusing a q connection.
BUFFER-NAME is the buffer to create or reuse.  INTERACTIVE-CALL is
whether the calling command was itself invoked interactively -
`called-interactively-p' can't be called in here directly, since this
function is never the interactive entry point, so the caller passes its
own answer through.  MESSAGE is echoed with `message' when a new
process is actually started.  START-PROCESS-FN is called with no
arguments, inside the buffer, with MODE (default `q-shell-mode') already
turned on and `comint-process-echoes' already set to nil; it must start
and return the buffer's process - this is the only part that differs
between callers, e.g. an inferior process versus a dummy placeholder
paired with a custom input sender.  Always returns BUFFER-NAME's
process, whether it was just started here or already running from an
earlier call."
  (let ((buffer (get-buffer-create buffer-name)))
    (when interactive-call (pop-to-buffer buffer))
    (when (or current-prefix-arg (not (q--shell-buffer-p buffer)))
      (with-current-buffer buffer
        (message "%s" message)
        (funcall (or mode #'q-shell-mode))
        (setq comint-process-echoes nil)
        (q--setup-shell-buffer (funcall start-process-fn))))
    (q-activate-buffer buffer)
    (get-buffer-process buffer)))

;;;###autoload
(cl-defun q-qcon (&key host port (user "") alias tls)
  "Connect to a pre-existing q process.
HOST, PORT, and USER identify the connection; the default for all
three comes from the `q-connection-*' customization variables.  ALIAS, when
non-nil, is a matched `q-connections-alist' entry name, shown in the buffer
name.  TLS is only used to warn that qcon doesn't support it - qcon
always connects over plain tcp regardless.  In interactive use, a
prefix argument directs this command to prompt for connection args,
offering `q-connections-alist' as completion candidates while still accepting
an ad-hoc \"host:port[:user]\" string."
  (interactive
   (if current-prefix-arg
       (q--connection-prompt-args)
     (q--connection-default-args)))
  (when tls (message "q: qcon does not support tcps protocol, continuing with tcp"))
  (cl-destructuring-bind (clean-host clean-port login password)
      (q--qcon-resolve-args host port user)
    (let ((cmd-args (q--qcon-format-args clean-host clean-port login password))
          (display-args (q--qcon-redact-args clean-host clean-port login password)))
      (q--start-connection-buffer
       (q--format-buffer-name :qcon clean-host clean-port alias)
       (called-interactively-p 'any)
       (format "q: starting qcon with command \"%s\"" (concat q-qcon-program " " display-args))
       (lambda ()
         (get-buffer-process (comint-exec (current-buffer) "qcon" q-qcon-program nil (list cmd-args))))
       #'q-qcon-mode))))

(defun q--parse-host-scheme (host)
  "Parse HOST for a valid kdb+ protocol scheme.
Returns a cons cell (USE-TLS . CLEAN-HOST).  Throws a `user-error' if
an unsupported scheme is provided."
  (if (string-match "\\`\\([^:]+\\)://\\(.*\\)" host)
      (let ((scheme (downcase (match-string 1 host)))
            (clean-host (match-string 2 host)))
        (pcase scheme
          ("tcps" (cons t clean-host))
          ("tcp"  (cons nil clean-host))
          (_ (user-error (concat "Unsupported protocol scheme \"%s://\"; "
                                 "use plain host names or tcp[s]://")
                         scheme))))
    ;; No "://" found, treat as plain host
    (cons nil host)))

(defun q--con-prompt-text (host port tls)
  "Return the `q-con' buffer prompt for HOST, PORT and TLS flag."
  (concat (and tls "tcps://") (format "%s:%s>" host port)))

(defvar-local q--con-target nil
  "For a `q-con' buffer, the (HOST PORT USER TLS) tuple to reconnect with.
Resolved to a login/password pair fresh for every query, so the
password sits in Emacs only for the instant it takes to write it to a
new socket.")

(defun q--con-port-number (port)
  "Return PORT, a number or a numeric string, as a number."
  (if (stringp port) (string-to-number port) port))

(defun q--con-handshake-and-query (login password query)
  "Build the login-handshake-plus-query bytes for a freshly opened q socket.
LOGIN and PASSWORD are the resolved credentials (either may be \"\");
QUERY is the q expression to evaluate. A null byte is unconditionally
appended to QUERY."
  (concat login
          (unless (string-empty-p password) (concat ":" password))
          "\0" query "\0"))

(defvar-local q--con-inflight-proc nil
  "The network process for the in-flight `q-con' request.
Nil if idle.  See `q--con-input-sender'.")

(defvar-local q--con-dispatch-queue nil
  "FIFO of `q-con' query strings not yet sent.
Each query is sent after the prior response is received.  Using the queue
instead of blocking, allows queries to queue up and prevents Emacs from
blocking.")

(defun q--con-format-reply (reply)
  "Ensure REPLY ends with a newline, unless REPLY is empty."
  (if (and (not (string-empty-p reply)) (not (string-suffix-p "\n" reply)))
      (concat reply "\n")
    reply))

(defun q--output-filter (buffer text)
  "Call `comint-output-filter' for BUFFER with TEXT."
  (let ((proc (get-buffer-process buffer)))
    (when proc
      (comint-output-filter
       proc (concat text
                    (q--con-prompt-text (nth 0 q--con-target)
                                        (nth 1 q--con-target)
                                        (nth 3 q--con-target)))))))

(defun q--con-finish (shell-buffer text)
  "Insert TEXT plus a fresh prompt into SHELL-BUFFER.
Then send next query (if any) from `q--con-dispatch-queue'."
  (when (buffer-live-p shell-buffer)
    (with-current-buffer shell-buffer
      (setq q--con-inflight-proc nil)
      (q--output-filter shell-buffer text)
      (q--con-dispatch-next))))

(defun q--con-filter (shell-buffer)
  "Return a process filter for `q-con' SHELL-BUFFER.
Captures the first chunk as a `q-con' reply and closes connection."
  (lambda (proc chunk)
    (unless (process-get proc 'q-con-handled)
      (process-put proc 'q-con-handled t)
      (delete-process proc)
      (q--con-finish shell-buffer (q--con-format-reply chunk)))))

(defun q--con-sentinel (shell-buffer)
  "Return a process sentinel for `q-con' SHELL-BUFFER.
The underlying protocol is one-shot: the server closes the connection
itself once it has replied, even when the reply is empty - so
`process-status' being `closed' or `exit' here just means an empty
reply, not a failure.  Only `failed' - the connection itself never
came up: TLS handshake, host unreachable, connection refused - is
reported as an error.  A non-empty reply is already fully handled,
including `delete-process', by `q--con-filter' before this ever gets a
chance to run; the `q-con-handled' process property is how it tells
the two cases apart."
  (lambda (proc event)
    (unless (process-get proc 'q-con-handled)
      (process-put proc 'q-con-handled t)
      (q--con-finish shell-buffer
                     (if (eq (process-status proc) 'failed)
                         (q--con-format-reply
                          (format "q-con error: %s" (string-trim event)))
                       "")))))

(defun q--con-start-query (shell-buffer query)
  "Open an async connection to SHELL-BUFFER's `q--con-target' and send QUERY.
Returns the network process.  USER is resolved to a login/password
pair immediately before it's written to the socket and never leaves
Emacs any other way - unlike qcon, no external process is exec'd, so
nothing about the connection, let alone the password, is ever visible
to `ps'; see `q--connection-resolve-credentials'."
  (cl-destructuring-bind (host port user tls) q--con-target
    (condition-case err
        (let* ((resolved (q--connection-resolve-credentials host port user))
               (login (car resolved))
               (password (or (cdr resolved) ""))
               (proc (open-network-stream "q-con" nil host (q--con-port-number port)
                                          :type (if tls 'tls 'plain)
                                          :coding 'binary)))
          (set-process-filter proc (q--con-filter shell-buffer))
          (set-process-sentinel proc (q--con-sentinel shell-buffer))
          (process-send-string proc (q--con-handshake-and-query login password query))
          (process-send-eof proc)
          proc)
      (error
       (q--con-finish shell-buffer
                      (q--con-format-reply (format "q-con error: %s" (error-message-string err))))
       nil))))

(defun q--con-dispatch-next ()
  "Send next `q-con' request if none are already in flight."
  (when (and (not q--con-inflight-proc) q--con-dispatch-queue)
    (setq q--con-inflight-proc (q--con-start-query (current-buffer) (pop q--con-dispatch-queue)))))

(defun q--con-input-sender (_proc string)
  "Comint input sender used to answer queries in a `q-con' buffer.
Never blocks: appends STRING to `q--con-dispatch-queue' and starts it
immediately via `q--con-dispatch-next' if nothing is already in flight;
otherwise it waits its turn.  Ignores PROC - the dummy placeholder
process kept only so `comint-mode' considers the buffer to have a live
process."
  (setq q--con-dispatch-queue (nconc q--con-dispatch-queue (list string)))
  (q--con-dispatch-next))

(defun q--con-abort ()
  "Abort the in-flight `q-con' request, if any.
Drop everything queued behind it."
  (interactive)
  (when q--con-inflight-proc
    (process-put q--con-inflight-proc 'q-con-handled t)
    (delete-process q--con-inflight-proc)
    (setq q--con-inflight-proc nil))
  (setq q--con-dispatch-queue nil)
  (q--reply-queue-clear)
  (q--output-filter (current-buffer) "\nConnection aborted.\n"))

;;;###autoload
(cl-defun q-con (&key host port (user "") alias tls)
  "Connect to a pre-existing q process natively, without spawning qcon.
Emacs opens the TCP[S] socket itself instead of executing an external
qcon binary.  That matters for the password: qcon receives it as a
literal command-line argument, so anyone on the machine can read it
with `ps'; this instead resolves it from auth-source only for the
instant it takes to write it to the socket, and it never becomes a
command-line argument to any process at all.  Also supports TLS, via a
tcps:// scheme prefix on the host.

HOST, PORT, and USER identify the connection; the default for all
three comes from the `q-connection-*' customization variables.  ALIAS, when
non-nil, is a matched `q-connections-alist' entry name, shown in the buffer
name.  In interactive use, a prefix argument prompts for connection
args, offering `q-connections-alist' as completion candidates while still
accepting an ad-hoc \"host:port[:user]\" string.

Because the underlying protocol is one-shot - the q process replies and
the connection is closed for every single request - the same as qcon -
this can't keep one persistent socket alive for the whole buffer the way
a real inferior process would.  Instead the buffer's process is a dummy
placeholder that never sees any real traffic; every send - a line,
region, function, or the whole buffer - opens its own connection and
resolves the password from auth-source again each time.  Sending never
blocks Emacs: queries wait in `q--con-dispatch-queue' and are sent one
at a time, in order, as each previous one finishes - see
`q--con-input-sender'.  An in-flight request can be aborted with
\\[q--con-abort] bound in `q-con-shell-mode'."
  (interactive
   (if current-prefix-arg
       (q--connection-prompt-args)
     (q--con-default-args)))
  (let ((display-args (q--connection-display-args host port user tls)))
    (q--start-connection-buffer
     (q--format-buffer-name :con host port alias tls)
     (called-interactively-p 'any)
     (format "q: connecting natively to %s " display-args)
     (lambda ()
       (setq-local q--con-target (list host port user tls))
       (setq-local comint-input-sender #'q--con-input-sender)
       ;; A dummy process to keep comint happy, exactly as ielm does it -
       ;; it never gets any real input.  `q--con-input-sender' bypasses
       ;; it entirely and talks to the q process over its own one-shot
       ;; connections instead.
       (let ((process (condition-case nil
                          (start-process "q-con" (current-buffer) "cat")
                        (file-error (start-process "q-con" (current-buffer) "hexl")))))
         (set-process-query-on-exit-flag process nil)
         (set-process-filter process #'comint-output-filter)
         (comint-output-filter process (q--con-prompt-text host port tls))
         process))
     #'q-con-mode)))

(defun q-show-q-buffer ()
  "Switch to the active q process, or start a new one (passing in args)."
  (interactive)
  (unless (q--shell-buffer-p q-active-buffer)
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

(defun q--process-sentinel (process message)
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
        (insert-before-markers text)
        ;; Any queued replies will never arrive now.
        (q--reply-queue-clear)))))

(defun q--strip (text)
  "Strip TEXT of all trailing comments, newlines and excessive whitespace.
The order of operations matters and must not be rearranged."
  (setq text (replace-regexp-in-string "^\\(?:[^\\\\].*\\)?[ \t]\\(/.*\\)\\(?:\n\\|\\'\\)" "" text t t 1)) ; / comments
  (setq text (replace-regexp-in-string "^[ \t]*/.+$" "" text t t)) ; / comments
  (setq text (replace-regexp-in-string "[ \t\n]+$" "" text t t)) ; excess white space
  (setq text (replace-regexp-in-string "\n[ \t]+" "" text t t)) ; fold functions
  text)

(defun q-send-string (string &optional source-span)
  "Send STRING to the inferior q process stored in `q-active-buffer'.
Goes through the buffer-local `comint-input-sender' rather than
hardcoding `comint-simple-send', so this works unchanged for a real
inferior q or qcon process, and also for a `q-con' buffer, where
`comint-input-sender' is `q--con-input-sender' and STRING instead
travels over a fresh one-shot network connection.

SOURCE-SPAN, if non-nil, is a (BEG . END) pair of positions in the
calling \(source\) buffer describing where STRING came from - see
`q--eval-source-span'.  Once a reply arrives, `q--reply-filter' runs
`q-reply-functions' with the reply text and markers at BEG and END (or
both nil, if SOURCE-SPAN was nil).

When SOURCE-SPAN is given, the namespace in effect at its BEG (per
`q--namespace-at-point') is used to \\d into that namespace before
evaluating STRING."
  (unless (stringp string)
    (user-error "Nothing to send"))
  (unless (q--shell-buffer-p q-active-buffer)
    (user-error "No active q buffer; run `M-x q' or activate a q shell with `C-c M-RET'"))
  (let* ((msg (if source-span
                  (q--namespace-wrap-string
                   string (q--namespace-at-point (car source-span)))
                string))
         (source-beg (and source-span (copy-marker (car source-span))))
         (source-end (and source-span (copy-marker (cdr source-span)))))
    (with-current-buffer q-active-buffer
      (unless comint-process-echoes
        (goto-char (point-max))
        (insert-before-markers (concat msg "\n")))
      ;; Always queue, even with no span, so FIFO order can't desync
      ;; between spanned and unspanned sends interleaved on one
      ;; connection - e.g. `q-eval-buffer' (no span) followed by
      ;; `q-eval-line' (spanned): both need an entry, in order, or the
      ;; second reply would be matched to the first's entry.
      (setq q--reply-queue
            (nconc q--reply-queue
                   (list (list source-beg source-end (copy-marker (point-max))))))
      (funcall comint-input-sender (get-buffer-process q-active-buffer) msg)))
  ;; Allow `M-g n' (and `q-next-error'/`C-c `') from source buffers.
  (setq next-error-last-buffer q-active-buffer)
  (when (equal current-prefix-arg '(16)) (q-show-q-buffer)))

(defvar q-reply-functions nil
  "Hook run when a reply to a `q-send-string' call arrives.
Each function is called with three arguments: REPLY, the reply text;
and SOURCE-BEG and SOURCE-END, markers for the span of source-buffer
text that produced the request, or both nil if `q-send-string' was
called with no SOURCE-SPAN.  Runs in the shell buffer the reply
arrived in, not the source buffer - a function that needs the source
buffer should get it from `(marker-buffer source-beg)' and check it is
still live before doing anything with it.")

(defun q--eval-source-span (start end)
  "Return (BEG . END) source positions spanning START..END.
BEG is the beginning of the line containing START; END is the end of
the line containing END."
  (cons (save-excursion (goto-char start) (line-beginning-position))
        (save-excursion (goto-char end) (line-end-position))))

(defun q-eval-region (start end)
  "Send the region between START and END to the inferior q[con] process."
  (interactive "r")
  (q-send-string (q--eval-prefix (q--strip (buffer-substring start end)))
                 (q--eval-source-span start end))
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
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (unless bounds
      (user-error "No symbol at point"))
    (q-eval-region (car bounds) (cdr bounds))))

(defun q-eval-buffer ()
  "Load current buffer into the inferior q[con] process.
Sends via `q-send-string' directly rather than `q-eval-region', so
this passes no source span - there's no single well-defined
provenance region for evaluating an entire buffer the way there is for
one line, symbol, or function."
  (interactive)
  (q-send-string (q--strip (buffer-substring (point-min) (point-max)))))

(defconst q-symbol-regexp
  "`\\(?:\\(?:\\w\\|[.]\\)\\(?:\\w\\|[_.]\\)*\\)?"
  "Regular expression used to find symbols.")

(defconst q-file-regexp
  (concat q-symbol-regexp ":\\(?:\\w\\|[/:_.]\\)*")
  "Regular expression used to find files.")

(defconst q-name-regexp
  "\\_<\\([.]?[a-zA-Z]\\(?:\\w\\|[_.]\\)*\\)\\s-*"
  "Regular expression used to find variable or function names.")

(defconst q-function-regexp
  (concat q-name-regexp
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

(defconst q-variable-regexp
  (concat q-name-regexp
          "[-.~=!@#$%^&*_+|,<>?]?"      ; potential compound assignment
          ":"                           ; assignment
          ":?"                          ; view
          "\\s-*"                       ; potential space
          "[^ )}:;\n]"                  ; something else
          )
  "Regular expression used to find variable declarations.")

;; q runtime stack traces include entries like:
;;   [2] /path/to/file.q:42: expr
;; Reuse the same pattern for both Flymake parsing and shell navigation.
(defconst q--stack-frame-regexp
  "\\(?:  \\|>>\\)?\\[[0-9]+\\] *\\(.*\\.[kq]\\):\\([0-9]+\\): "
  "Regular expression matching a q stack-frame location (file + line).")

(defun q-eval-function ()
  "Send the current function to the inferior q[con] process."
  (interactive)
  (condition-case nil
      (save-excursion
        (goto-char (line-end-position))          ; go to end of line
        (let ((start (re-search-backward (concat "^" q-function-regexp))) ; find beginning of function
              (_   (re-search-forward ":")) ; find end of function name
              (bounds   (bounds-of-thing-at-point 'sexp))) ; find function body
          (unless bounds
            (user-error "Could not parse function body"))
          (q-eval-region start (cdr bounds))))
    (search-failed
     (user-error "No function found around point"))))

(defun q--and-go (fun)
  "Call FUN interactively and show active q buffer."
  (let ((current-prefix-arg '(16))) (call-interactively fun)))

(defun q-eval-line-and-go ()
  "Send the current line to the inferior q[con] process and show active q buffer."
  (interactive)
  (q--and-go 'q-eval-line))

(defun q-eval-function-and-go ()
  "Send the function to the inferior q[con] process and show active q buffer."
  (interactive) (q--and-go 'q-eval-function))

(defun q-eval-region-and-go ()
  "Send the active region to the inferior q[con] process and show active q buffer."
  (interactive)
  (q--and-go 'q-eval-region))

(defun q-eval-symbol-and-go ()
  "Send current symbol to the inferior q[con] process and show active q buffer."
  (interactive)
  (q--and-go 'q-eval-symbol))

(defun q-load-file ()
  "Load current buffer's file into the inferior q[con] process after saving."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (save-buffer)
  (q-send-string (format "\\l %s" (shell-quote-argument buffer-file-name))))

(defun q-next-error (&optional n)
  "Jump to the Nth next stack-frame error in the active q shell buffer."
  (interactive "p")
  (unless (q--shell-buffer-p q-active-buffer)
    (user-error "No active q buffer; run `M-x q' or activate a q shell with `C-c M-RET'"))
  (setq next-error-last-buffer q-active-buffer)
  (next-error (or n 1)))

(defun q-rescan-project (&optional force)
  "Rescan the current project, skipping files whose mtime is unchanged.
With a prefix argument FORCE, re-scan all files regardless of mtime."
  (interactive "P")
  (q--project-plist-put :file-list-sentinel nil)
  (when force
    (q--project-plist-put :scan-state nil))
  (q--do-full-rescan (current-buffer)
                     (if force "forced rescan" "manual rescan")
                     t))

;; reply matching
;;
;; `q-send-string' pushes a (source-beg source-end reply-start-marker)
;; entry onto `q--reply-queue' for every send.  `q--reply-filter' runs on
;; `comint-output-filter-functions' in every q-shell/q-con/q-qcon buffer
;; and watches for the next prompt; once one appears, the oldest queued
;; reply is known to be complete, is popped off, and `q-reply-functions'
;; is run with the reply text and SOURCE-BEG/SOURCE-END.

(defun q--reply-filter (_output)
  "Comint output filter matching completed replies to `q--reply-queue'.
Runs after every chunk of process output is inserted into the current
\(shell\) buffer."
  (when q--reply-queue
    (let* ((proc (get-buffer-process (current-buffer)))
           (reply-end (and proc
                           (save-excursion
                             (goto-char (process-mark proc))
                             (forward-line 0)
                             (and (looking-at-p comint-prompt-regexp) (point))))))
      (when reply-end
        (cl-destructuring-bind (source-beg source-end reply-start) (pop q--reply-queue)
          (let ((reply (string-trim
                        (buffer-substring-no-properties reply-start reply-end))))
            (set-marker reply-start nil)
            (run-hook-with-args 'q-reply-functions reply source-beg source-end)))))))

;; q-inline-mode

(defvar q-inline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-.") 'q-inline-show-full)
    (define-key map (kbd "C-c C-k") 'q-inline-clear)
    (define-key map (kbd "C-u C-c C-k") 'q-inline-clear-buffer)
    map)
  "Keymap for `q-inline-mode'.")

;;;###autoload
(define-minor-mode q-inline-mode
  "Toggle showing eval results inline, next to the code that produced them.

When enabled, `q-eval-line' and friends show their reply as an overlay
at the point they were sent from -- in addition to the q-shell/qcon/con
buffer.  Implemented as a subscriber to the generic `q-reply-functions'
hook - see `q--inline-reply-handler' for how a reply gets matched back
to where it came from.  See `q-inline-duration' for how long the overlay
stays visible.

\\{q-inline-mode-map}"
  :lighter " Q-Inline"
  :keymap q-inline-mode-map
  (unless q-inline-mode
    (q-inline-clear-buffer)))

(defface q-inline-face
  '((t :inherit shadow :slant italic))
  "Face used for the inline eval-result overlay shown by `q-eval-line' et al."
  :group 'q)

(defcustom q-inline-duration nil
  "How long an eval-result overlay stays visible before auto-clearing.
- nil (the default): persists until the underlying code is edited, or
  `q-inline-clear'/`q-inline-clear-buffer' is used - see
  `q--inline-invalidate'.
- a number: seconds until this specific overlay is deleted, via a
  one-shot timer.
- the symbol `command': this specific overlay is deleted the moment
  any command runs after it appears - closer to a fleeting tooltip
  than a persistent annotation."
  :safe (lambda (v) (or (null v) (numberp v) (eq v 'command)))
  :type '(choice (const :tag "Persist until edited or cleared" nil)
                 (integer :tag "Duration in seconds")
                 (const :tag "Until next command" command))
  :group 'q)

(defun q--inline-show (source-beg source-end reply)
  "Display REPLY as an overlay spanning SOURCE-BEG..SOURCE-END.
Both are markers into a source buffer, still live only if that buffer
is.  Any earlier eval-result overlay overlapping the same span is
replaced rather than stacked.  The overlay's `after-string' - and so
the visible result text - always renders at SOURCE-END regardless of
where SOURCE-BEG is, but the overlay's own extent covers the whole
span, so `q-inline-show-full' and edit-invalidation both work
from anywhere within it, not just where the text happens to print.
REPLY is truncated to its first line for that display text; the full
text is kept on the overlay's `q-eval-full-reply' property."
  (let ((buf (marker-buffer source-beg)))
    (when (and buf (buffer-live-p buf) (not (string-empty-p reply)))
      (with-current-buffer buf
        (let* ((beg (marker-position source-beg))
               (end (marker-position source-end))
               (first-line (car (split-string reply "\n")))
               (label (if (string-match-p "\n" reply) (concat first-line " ...") first-line)))
          (dolist (ov (overlays-in beg end))
            (when (overlay-get ov 'q-inline-result) (delete-overlay ov)))
          (let ((ov (make-overlay beg end buf)))
            (overlay-put ov 'q-inline-result t)
            (overlay-put ov 'q-eval-full-reply reply)
            (overlay-put ov 'after-string
                        (propertize (concat "  => " label) 'face 'q-inline-face))
            (overlay-put ov 'modification-hooks '(q--inline-invalidate))
            (overlay-put ov 'insert-in-front-hooks '(q--inline-invalidate))
            (q--inline-schedule-clear ov))))))
  (set-marker source-beg nil)
  (set-marker source-end nil))

(defun q--inline-reply-handler (reply source-beg source-end)
  "Show REPLY as an overlay spanning SOURCE-BEG..SOURCE-END.
Registered unconditionally on `q-reply-functions' at load time, below
- does nothing if SOURCE-BEG is nil (the request carried no source
span - e.g. `q-eval-buffer'), if its buffer is no longer live, or if
that buffer doesn't currently have `q-inline-mode' turned on."
  (when (and source-beg
             (buffer-live-p (marker-buffer source-beg))
             (buffer-local-value 'q-inline-mode (marker-buffer source-beg)))
    (q--inline-show source-beg source-end reply)))

(add-hook 'q-reply-functions #'q--inline-reply-handler)

(defun q--inline-delete (ov)
  "Delete overlay OV."
  (delete-overlay ov))

(defun q--inline-schedule-clear (ov)
  "Arrange for OV to auto-clear per `q-inline-duration'.
A nil `q-inline-duration' does nothing."
  (pcase q-inline-duration
    ((pred numberp)
     (run-at-time q-inline-duration nil #'q--inline-delete ov))
    ('command
     (letrec ((clear (lambda ()
                        (remove-hook 'pre-command-hook clear t)
                        (q--inline-delete ov))))
       (add-hook 'pre-command-hook clear nil t)))))

(defun q--inline-invalidate (overlay after-p &rest _)
  "Delete an eval-result OVERLAY once its underlying text is edited.
Modification hooks run once before the change and once after; AFTER-P
distinguishes the two, and only the after call deletes the overlay."
  (when after-p (delete-overlay overlay)))

(defun q-inline-show-full ()
  "Pop up the untruncated reply for the eval-result overlay at point.
Only needed for multi-line replies (e.g. tables), where the inline
overlay itself only shows the first line.  Works from anywhere the
overlay spans."
  (interactive)
  (let ((ov (cl-find-if (lambda (o) (overlay-get o 'q-inline-result))
                        (overlays-at (point)))))
    (unless ov
      (user-error "No eval result at point"))
    (let ((reply (overlay-get ov 'q-eval-full-reply))
          (buf (get-buffer-create "*q-inline-result*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert reply)
        (goto-char (point-min))
        (q--setup-font-lock)            ; not full q-mode
        (font-lock-mode 1)
        (font-lock-ensure))
      (display-buffer buf))))

(defun q-inline-clear (&optional whole-buffer)
  "Delete the eval-result overlay at point and optionally the WHOLE-BUFFER.
With a prefix argument WHOLE-BUFFER, delete every eval-result overlay."
  (interactive "P")
  (if whole-buffer
      (q-inline-clear-buffer)
    (let ((ov (cl-find-if (lambda (o) (overlay-get o 'q-inline-result))
                          (overlays-at (point)))))
      (unless ov
        (user-error "No eval result at point"))
      (delete-overlay ov))))

(defun q-inline-clear-buffer ()
  "Delete every eval-result overlay in the current buffer."
  (interactive)
  (let ((count 0))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'q-inline-result)
        (delete-overlay ov)
        (setq count (1+ count))))
    (message (if (= count 0) "No eval results to clear"
               (format "Cleared %d eval result%s" count (if (= count 1) "" "s"))))))

(easy-menu-define q-inline-menu q-inline-mode-map
  "Menubar for `q-inline-mode' commands."
  '("Q-Inline"
    ["Show Full Result"     q-inline-show-full t]
    ["Clear Result"         q-inline-clear t]
    ["Clear All Results"    q-inline-clear-buffer t]))

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
    (define-key map "\C-c\C-g"   'q-rescan-project)
    (define-key map "\C-c`"       'q-next-error)
    map)
  "Keymap for q major mode.")

;; menu bars
(easy-menu-define q-menu q-mode-map
  "Menubar for `q-mode' commands."
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
    ["Rescan Project"  q-rescan-project t]
    ["Next Error"      q-next-error t]
    "---"
    ["Customize Q"    q-customize t]
    ["Show Q Shell"   q-show-q-buffer t]
    ["Kill Q Shell"   q-kill-q-buffer t]
    ))

(easy-menu-define q-shell-menu q-shell-mode-map
  "Menubar for `q-shell-mode' commands."
  '("Q-Shell"
    ["Activate Buffer" q-activate-this-buffer t]
    ))

;; faces

;; font-lock-comment-face font-lock-comment-delimiter-face
;; font-lock-string-face font-lock-doc-face
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

(defconst q-keywords-regexp
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

(defconst q-builtin-words-regexp
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

(defconst q-builtin-dot-z-words-regexp
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

(defconst q-builtin-dot-Q-words-regexp
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

(defconst q-builtin-dot-h-words-regexp
  (concat "\\_<"
          "\\(?:[_]\\)?"                ; leading _ is not a symbol
          (regexp-opt q-builtin-dot-h-word-list t)
          "\\_>")
  "Builtin .h functions/constants regex defined for q mode.")

(defconst q-builtin-dot-j-word-list
  '(".j.j" ".j.jd" ".j.k")
  "Builtin .j functions/constants defined for q mode.")

(defconst q-builtin-dot-j-words-regexp
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
   (list (concat "[; ]\\('" q-symbol-regexp "\\)") 1 font-lock-warning-face nil) ; signal
   (cons q-file-regexp 'font-lock-preprocessor-face) ; files
   (cons q-symbol-regexp 'font-lock-constant-face) ; symbols
   )
  "Minimal highlighting expressions for q mode.")

(defconst q-font-lock-keywords-1          ; symbols
  (append q-font-lock-keywords
          (list
           (list q-keywords-regexp 1 'font-lock-keyword-face nil) ; select from
           '("\\b[0-2]:" . font-lock-builtin-face)         ; IO/IPC
           (list q-builtin-words-regexp 1 'font-lock-builtin-face nil) ; q.k
           (list q-builtin-dot-z-words-regexp 1 'font-lock-builtin-face nil) ; .z.*
           (list q-builtin-dot-Q-words-regexp 1 'font-lock-builtin-face nil) ; .Q.*
           (list q-builtin-dot-h-words-regexp 1 'font-lock-builtin-face nil) ; .h.*
           (list q-builtin-dot-j-words-regexp 1 'font-lock-builtin-face nil) ; .j.*
           ))
  "More highlighting expressions for q mode.")

(defconst q-font-lock-keywords-2 ; function/variable names and literals
  (append q-font-lock-keywords-1
          (list
           (list q-function-regexp 1 'font-lock-function-name-face nil) ; functions
           (list q-variable-regexp 1 'font-lock-variable-name-face nil) ; variables
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
`q-syntax-propertize', not by `font-lock-syntactic-keywords'.")


;; syntax table

(defvar q-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" ".  " table) ; treat " as punctuation
    (modify-syntax-entry ?\/ ".  " table) ; treat / as punctuation
    (modify-syntax-entry ?\. "_  " table) ; treat . as symbol
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

(defun q--setup-font-lock ()
  "Set up q syntax highlighting in the current buffer.
Set the syntax table, `font-lock-defaults', and the
`syntax-propertize-function'."
  (set-syntax-table q-mode-syntax-table)
  (setq-local font-lock-defaults q-font-lock-defaults)
  (setq-local syntax-propertize-function #'q-syntax-propertize))

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
    ;; / after whitespace or BOL starts a line comment, not inside a string;
    ;; also mark the terminating newline as comment-ender since \n has no
    ;; comment-ender syntax in the table (handled entirely via text properties)
    ("\\(?:^\\|[ \t]\\)\\(/\\)\\([^\n]*\\)\\(\n\\)"
     (1 (unless (nth 3 (syntax-ppss)) (string-to-syntax "<")))
     (3 (unless (nth 3 (syntax-ppss)) (string-to-syntax ">")))))
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
                                 "\\(" q--stack-frame-regexp "\\).*\\\n" ; line number
                                 "\\( +^\\)$" ; carat showing column of error
                                 )
                                nil t)
                         for msg = (match-string 1)
                         for prefix = (match-string 2)
                         for row = (string-to-number (match-string 4))
                         for carat = (match-string 5)
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

(defconst q--capf-core-words-set
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
;;   :rescan-timer            pending idle timer (shared across project buffers)

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
  (let* ((key   (q--project-key))
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

(defconst q--load-command-regexp "^\\(?:\\\\\\|system\\s-+\"\\)l\\s-+:?\\([^ \t\n\"]+\\)"
  "Regex matching q load commands.")

(defconst q--namespace-command-regexp "^\\\\d\\s-+\\([^ \t\n]+\\)"
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
    (while (re-search-forward q--load-command-regexp nil t)
      (let ((resolved (q--resolve-load-path
                       (match-string-no-properties 1)
                       file)))
        (when resolved
          (push resolved targets))))
    (delete-dups targets)))

(defun q--load-targets-in-file (file)
  "Return loaded file targets referenced by FILE.
Uses a visiting buffer when modified; otherwise reads from disk."
  ;; with-temp-buffer is intentional here: this function is called
  ;; recursively via q--expand-loaded-files, so a shared scratch buffer
  ;; would require re-entrancy guarantees we do not have.  The call
  ;; frequency is low (file-list expansion only, not the hot scan loop)
  ;; so the per-call allocation cost is acceptable.
  (let ((buf (find-buffer-visiting file)))
    (if (and buf (buffer-modified-p buf))
        (with-current-buffer buf (save-excursion (q--load-targets-in-buffer file)))
      (with-temp-buffer
        (condition-case nil
            (progn (insert-file-contents file) (q--load-targets-in-buffer file))
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
  "Return the cached expanded file list, refreshing when the project has changed.
The \\l expansion only runs when the sentinel changes, not on every
eldoc or CAPF invocation."
  (let* ((file     (buffer-file-name))
         (sentinel (q--project-key)))
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

(defconst q--identifier-token-regexp (concat q-name-regexp "\\_>")
  "Regex matching q identifiers for reference scanning.")

(defun q--canonicalize-name (namespace name)
  "Return canonical fully-scoped NAME using NAMESPACE context."
  (if (string-search "." name)
      name
    (concat (or namespace ".") "." name)))

(defun q--namespace-at-point (&optional pos)
  "Return the active q namespace at POS (default point)."
  (save-excursion
    (let ((limit (or pos (point)))
          (namespace nil))
      (goto-char (point-min))
      (while (re-search-forward q--namespace-command-regexp limit t)
        (setq namespace (match-string-no-properties 1)))
      namespace)))

(defun q--namespace-wrap-string (string namespace)
  "Return STRING wrapped with \\d commands to change into NAMESPACE and back.
Empty or null namespace leaves string untouched"
  (if (and namespace (not (string= namespace ".")))
      (concat "system\"d " namespace "\";" string ";system\"d .\";")
    string))

(defun q--make-entry (meta &optional doc signature file)
  "Return scanner entry from META with optional DOC, SIGNATURE and FILE location."
  (append (list :summary (plist-get meta :summary))
          (when signature (list :signature signature))
          (when doc       (list :doc doc))
          (if file
              (list :file file
                    :line (plist-get meta :line))
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
          (def-pattern (concat "^" q-variable-regexp))
          (namespace nil))
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (if (looking-at q--namespace-command-regexp)
            (setq namespace (match-string-no-properties 1))
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (line (line-number-at-pos line-start t))
                 (summary (buffer-substring-no-properties line-start line-end)))
            (cl-labels ((make-meta (pos)
                          (list :pos pos
                                :line line
                                :summary summary)))
              (when (looking-at def-pattern)
                (let* ((name (match-string-no-properties 1))
                       (def-pos (match-beginning 0))
                       (canonical (q--canonicalize-name namespace name))
                       (meta (make-meta def-pos))
                       (doc (q--definition-doc def-pos))
                       (signature (q--function-signature canonical summary))
                       (entry (q--make-entry meta doc signature file)))
                  (puthash canonical (cons entry (gethash canonical def-index)) def-index)
                  (push canonical symbols)))
              (while (re-search-forward q--identifier-token-regexp line-end t)
                (let* ((name (match-string-no-properties 1))
                       (ref-pos (match-beginning 1))
                       (canonical (q--canonicalize-name namespace name))
                       (meta (make-meta ref-pos))
                       (entry (q--make-entry meta nil nil file)))
                  (puthash canonical (cons entry (gethash canonical ref-index)) ref-index))))))
        (forward-line 1))
      ;; Reverse row order to make them ascending
      (dolist (index (list def-index ref-index))
        (maphash (lambda (name entries) (puthash name (nreverse entries) index)) index))
      (list :definitions def-index
            :references ref-index
            :symbols (delete-dups symbols)))))


(defun q--scan-file-artifacts-into (file buf)
  "Return scan artifacts for FILE using reusable BUF.
Reuses BUF across calls to avoid per-file buffer allocation and GC
pressure.  The caller is responsible for creating and killing BUF."
  (let ((visiting (find-buffer-visiting file)))
    (if (and visiting (buffer-modified-p visiting))
        ;; Live modified buffer: scan it directly without touching BUF.
        (with-current-buffer visiting
          (save-excursion
            (q--scan-source-in-current-buffer file)))
      ;; Otherwise load from disk into the reusable scratch buffer.
      (with-current-buffer buf
        (erase-buffer)
        (condition-case nil
            (progn
              (insert-file-contents file)
              (set-syntax-table q-mode-syntax-table)
              (q--scan-source-in-current-buffer file))
          (file-missing
           (list :definitions (make-hash-table :test #'equal)
                 :references  (make-hash-table :test #'equal)
                 :symbols     nil)))))))

;; merged index rebuild (from per-file sub-index)

(defun q--merge-index (src dst)
  "Merge all entries from sub-index SRC into merged index DST.
Uses `copy-sequence' + `nconc' to avoid allocating intermediate lists:
each per-file entry list is shallow-copied once, then spliced onto the
accumulator destructively.  The per-file sub-index lists in SRC are
never mutated, so repeated rebuilds remain safe."
  (maphash (lambda (name entries)
             (puthash name (nconc (gethash name dst) (copy-sequence entries)) dst))
           src))

(defun q--rebuild-merged-indexes ()
  "Rebuild the merged indexes from the per-file sub-index.
Called after any per-file entry is updated so all buffers in the project
see a consistent view without re-reading any unchanged files."
  (let* ((def-index  (make-hash-table :test #'equal))
         (ref-index  (make-hash-table :test #'equal))
         (candidates (copy-hash-table q--capf-core-words-set))
         (file-index (or (q--project-plist-get :file-index) (make-hash-table)))
         ;; Ensure files are in sorted order
         (files (sort (hash-table-keys file-index)
                       (lambda (a b) (and (not (eq a :buffer)) (or (eq b :buffer) (string< a b)))))))
    (dolist (file files)
      (let ((artifacts (gethash file file-index)))
        (q--merge-index (plist-get artifacts :definitions) def-index)
        (q--merge-index (plist-get artifacts :references)  ref-index)
        (dolist (sym (plist-get artifacts :symbols)) (puthash sym t candidates))))
    (q--project-plist-put
     :definition-index      def-index
     :reference-index       ref-index
     :completion-candidates candidates)))

;; full and incremental rescans

(defun q--do-full-rescan (buf reason &optional force)
  "Scan files in the project for BUF whose mtime has changed, then rebuild.
REASON is a short string describing why the rescan was triggered.
Files whose mtime is unchanged are reused from the existing cache.
If FORCE is non-nil, run even if the scan-state appears current.
Emits a progress message before scanning and a timing message after."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (message "q: %s, enumerating project files..." reason)
      (redisplay)
      (let* ((files (q--ensure-project-file-list))
             (state (q--compute-scan-cache-state files)))
        (unless (and (not force) (equal state (q--project-plist-get :scan-state)))
          (let* ((old-state      (q--project-plist-get :scan-state))
                 (old-mtimes     (make-hash-table :test #'equal))
                 (old-file-index (q--project-plist-get :file-index))
                 (file-index     (make-hash-table :test #'equal))
                 (all-files      (or files (list nil)))
                 (scanned 0)
                 (progress 0)
                 (t0 (float-time))
                 (reporter (make-progress-reporter
                            (format "q: %s, scanning..." reason)
                            0 (length all-files))))
            (dolist (entry old-state)
              (puthash (car entry) (cdr entry) old-mtimes))
            ;; Single reusable scratch buffer for all on-disk reads.
            ;; Space-prefixed name disables undo automatically, eliminating
            ;; undo-list GC pressure from insert-file-contents.
            (let ((scan-buf (generate-new-buffer " *q-scan-tmp*")))
              (unwind-protect
                  (dolist (file all-files)
                    (let* ((key       (or file :buffer))
                           (new-mtime (and file (q--file-mtime file)))
                           (old-mtime (and file (gethash file old-mtimes)))
                           (changed   (not (equal old-mtime new-mtime))))
                      (if (and (not changed)
                               (hash-table-p old-file-index)
                               (gethash key old-file-index))
                          (puthash key (gethash key old-file-index) file-index)
                        (puthash key
                                 (if file
                                     (q--scan-file-artifacts-into file scan-buf)
                                   (q--scan-source-in-current-buffer))
                                 file-index)
                        (setq scanned (1+ scanned)))
                      (progress-reporter-update reporter (setq progress (1+ progress)))))
                (kill-buffer scan-buf)))
            (progress-reporter-done reporter)
            (q--project-plist-put :scan-state state
                                   :file-index file-index)
            (message "q: %s, building indexes..." reason)
            (redisplay)
            (q--rebuild-merged-indexes)
            (let* ((elapsed (- (float-time) t0))
                   (what (if files
                             (format "%d/%d file%s" scanned (length files)
                                     (if (= (length files) 1) "" "s"))
                           "current buffer")))
              (message "q: %s, scanned %s in %.2fs" reason what elapsed))))))))

(defun q--do-incremental-rescan (buf file)
  "Re-scan only FILE in the shared cache for BUF, then rebuild merged indexes.
When FILE is nil, re-scans the current buffer's in-memory content.
Falls back to a full rescan when no per-file sub-index exists yet."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((file-index (q--project-plist-get :file-index))
             (key        (or file :buffer)))
        (if (not (hash-table-p file-index))
            (q--do-full-rescan buf "no scan cache")
          (let* ((new-mtime  (and file (q--file-mtime file)))
                 (old-entry  (gethash key file-index))
                 (old-mtime  (and old-entry (plist-get old-entry :mtime))))
            ;; Skip if the file on disk has not changed since the last scan.
            ;; This guards against spurious after-save-hook fires and
            ;; ensures we only do work when content has actually changed.
            (unless (and old-mtime new-mtime (equal old-mtime new-mtime))
              (let ((artifacts (if file
                                   (let ((scan-buf (generate-new-buffer " *q-scan-tmp*")))
                                     (unwind-protect
                                         (q--scan-file-artifacts-into file scan-buf)
                                       (kill-buffer scan-buf)))
                                 (q--scan-source-in-current-buffer))))
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
check since its mtime change is expected.  Debounces any pending timer.
The timer is stored in the project cache so only one fires per project."
  (let ((buf  (current-buffer))
        (file (buffer-file-name)))
    (when (timerp (q--project-plist-get :rescan-timer))
      (cancel-timer (q--project-plist-get :rescan-timer)))
    (q--project-plist-put
     :rescan-timer
     (run-with-idle-timer
      q-rescan-idle-delay nil
      (lambda ()
        (if (q--scan-state-stale-p file)
            (progn
              (q--project-plist-put :file-list-sentinel nil)
              (q--do-full-rescan buf "project files changed"))
          (q--do-incremental-rescan buf file)))))))

(defun q--ensure-project-scan-cache ()
  "Ensure the shared project cache is populated for the current buffer.
On the very first call the scan runs synchronously so callers have data
immediately.  Subsequent calls return instantly; the idle timer, which
is triggered by save/revert hooks, keeps the cache fresh."
  (when (and (q--project-key)
             (null (q--project-plist-get :scan-state)))
    (q--do-full-rescan (current-buffer) "initial scan")))

(defun q--maybe-evict-project-cache ()
  "Remove the shared cache entry when no more `q-mode' buffers exist.
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
Used by Company and Corfu to populate the popup doc window (`C-h').
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
                            q--capf-core-words-set)
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

(defun q--fontify-string (str)
  "Return STR with `q-mode' font-lock face properties applied."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks (q-mode))
    (font-lock-ensure)
    (buffer-string)))

(defun q--entry->xref (entry)
  "Convert cached ENTRY plist into an xref object."
  (let ((summary (q--fontify-string (plist-get entry :summary)))
        (file (plist-get entry :file))
        (buffer (plist-get entry :buffer)))
    (xref-make summary
               (if file
                   (xref-make-file-location file
                                            (plist-get entry :line)
                                            0)
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
  "Return the fully-scoped q identifier at point, or nil when unavailable."
  (let ((name (thing-at-point 'symbol t)))
    (when name
      (q--canonicalize-name (q--namespace-at-point) name))))

(defun q--xref-backend ()
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
  (q--setup-font-lock)
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions #'q--reply-filter nil t)
  (setq-local comint-prompt-regexp "^\\(q)+\\|\\(?:tcps://\\)?[^:]*:[0-9]+>\\)")
  ;; Make q stack-trace file/line entries clickable in REPL output.
  (add-to-list 'compilation-error-regexp-alist-alist
               `(q-stack-frame ,(concat "^" q--stack-frame-regexp) 1 2))
  (setq-local compilation-error-regexp-alist
              (cons 'q-stack-frame compilation-error-regexp-alist))
  (compilation-shell-minor-mode 1)
  (setq-local comint-process-echoes nil))

(define-derived-mode q-con-mode q-shell-mode "Q-Con"
  "Major mode for a `q-con' buffer.
A native Emacs connection to a remote q process.")

(define-key q-con-mode-map (kbd "C-c C-c") 'q--con-abort)

(define-derived-mode q-qcon-mode q-shell-mode "Q-QCon"
  "Major mode for a `q-qcon' buffer.
A qcon subprocess relaying to a remote q process.")

(defun q-imenu-create-index ()
  "Build the fully-scoped `imenu' index alist for the current buffer."
  (let (entries)
    (maphash (lambda (name defs)
               (dolist (def defs)
                 (push (cons name (copy-marker (plist-get def :pos))) entries)))
             (plist-get (q--scan-source-in-current-buffer) :definitions))
    (sort entries (lambda (a b) (< (cdr a) (cdr b))))))

(with-eval-after-load 'hideshow
  (add-to-list 'hs-special-modes-alist
               (list 'q-mode "{" "}" "/[ \t]*" nil nil)))

(defun q-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a q function definition.
With ARG, do it that many times."
  (re-search-backward (concat "^" q-function-regexp) nil 'move (or arg 1)))

(defun q-end-of-defun ()
  "Move forward to the end of a q function definition.
For brace-delimited functions, finds the closing } matching the opening {.
For point-free definitions with no braces, moves to end of line."
  (goto-char (line-beginning-position))
  (if (re-search-forward "{" (line-end-position) t)
      (progn (backward-char) (forward-sexp))
    (end-of-line)))

(defun q-current-defun ()
  "Return the fully-scoped name of the q function enclosing point, or nil.
Used by `which-function-mode' and `add-log-current-defun-function'."
  (save-excursion
    (let ((start (point))
          name)
      (goto-char (line-end-position))
      (when (re-search-backward (concat "^" q-function-regexp) nil t)
        (let* ((candidate (match-string-no-properties 1))
               (end (match-end 0))
               (close-line (if (eq (char-before end) ?{)
                                (progn (goto-char (1- end))
                                       (ignore-errors (forward-sexp) (line-number-at-pos)))
                              (line-number-at-pos end))))
          (when (and close-line (<= (line-number-at-pos start) close-line))
            (setq name candidate))))
      (when name
        (q--canonicalize-name (q--namespace-at-point start) name)))))

;;;###autoload
(define-derived-mode q-mode prog-mode "Q"
  "Major mode for editing q language files."
  :group 'q
  (q--setup-font-lock)
  (setq-local comment-start q-comment-start)
  (setq-local comment-start-skip (concat "\\(^\\|[ \t]\\)\\("
                                         (regexp-quote q-comment-start)
                                         "+[ \t]*\\)"))
  (setq-local comment-end "")
  (setq-local indent-line-function 'q-indent-line)
  ;; enable imenu
  (setq-local imenu-create-index-function #'q-imenu-create-index)
  ;; which-function-mode
  (setq-local add-log-current-defun-function #'q-current-defun)
  (setq-local beginning-of-defun-function #'q-beginning-of-defun)
  (setq-local end-of-defun-function #'q-end-of-defun)
  ;; editor integrations
  (add-hook 'completion-at-point-functions #'q-completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'q-eldoc-function nil t)
  (when (featurep 'xref)
    (add-hook 'xref-backend-functions #'q--xref-backend nil t))
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
                               (q--compute-indent-sexp)
                             (* q-indent-step (q--compute-indent-tab)))
                           0))
                   (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun q--compute-indent-sexp ()
  "Compute the indent for a line using sexp."
  (backward-up-list)
  (let ((savepos (point)))
    (beginning-of-line)
    (+ 1 (- savepos (point)))))

(defun q--compute-indent-tab ()
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
