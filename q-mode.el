;;; q-mode.el --- A q editing mode    -*- coding: utf-8 -*-

;; Copyright (C) 2006-2015 Nick Psaris <nick.psaris@gmail.com>
;; Keywords: faces files q
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing q (the language written by Kx Systems,
;; see URL `http://www.kx.com') in Emacs.
;;
;; Some of its major features include:
;;
;;  - syntax highlighting (font lock),
;;
;;  - interaction with inferior q or qcon instance,
;;
;;  - scans declarations and places them in a menu.
;;
;; To load `q-mode` on-demand, instead of at startup, add this to your
;; initialization file

;; (autoload 'q-mode "q-mode")

;; The add the following to your initialization file to open all .k
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

;; Use `M-x q` to start an inferior q shell. Or use `M-x qcon` to
;; create an inferior qcon shell to communicate with an existing q
;; process.  Both can be prefixed with the universal-argument `C-u` to
;; customize the arguments used to start the processes.

;; The first q or qcon session opened becomes the activated buffer.
;; To open a new session and send code to the new buffer, it must be
;; actived.  Switch to the desired buffer and type `C-c M-RET` to
;; activate it.

;; The following commands are available to interact with an inferior q
;; or qcon process/buffer. `C-c C-l` sends a single line, `C-c C-f`
;; sends the surrounding function, `C-c C-r` sends the selected region
;; and `C-c C-b` sends the whole buffer.  If the source file exists on
;; the same machine as the q process, `C-c M-l` can be used to load
;; the file associated with the active buffer.

;; `M-x customize-group` can be used to customize the `q` group.
;; Specifically, the `inferior-q-program-name` and
;; `inferior-qcon-program-name` variables can be changed depending on
;; your environment.


(require 'comint)

(defgroup q nil "Major mode for editing q code" :group 'languages)

(defcustom inferior-q-program-name "q"
  "Program name for invoking an inferior q."
  :type 'file
  :group 'q)

(defcustom q-server ""
  "If non-nil, Q-Shell will ssh to the remote server before executing q."
  :type 'string
  :group 'q)

(defcustom q-user ""
  "User to use when 'ssh'-ing q to theremote server."
  :type 'string
  :group 'q)

(defcustom q-comment-start "/"
  "String to insert to start a new comment (some prefer a double forward slash)."
  :type 'string
  :group 'q)

(defgroup q-init nil "Q initialization variables" :group 'q)

(defcustom q-init-port 0
  "If non-zero, Q-Shell will start with the specified server port."
  :type 'integer
  :group 'q-init)

(defcustom q-init-slaves 0
  "If non-zero, Q-Shell will start with the specified number of slaves."
  :type 'integer
  :group 'q-init)

(defcustom q-init-workspace 0
  "If non-zero, Q-Shell will start with the specified workspace limit."
  :type 'integer
  :group 'q-init)

(defcustom q-init-file ""
  "If non-empty, Q-Shell will load the specified file."
  :type 'file
  :group 'q-init)

(defgroup q-qcon nil "Q qcon arguments" :group 'q)

(defcustom inferior-qcon-program-name "qcon"
  "Program name for invoking an inferior qcon."
  :type 'file
  :group 'q-qcon)

(defcustom q-qcon-server ""
  "Remote q servern"
  :type 'string
  :group 'q-qcon)

(defcustom q-qcon-port 5000
  "Port for remote q server."
  :type 'integer
  :group 'q-qcon)

(defcustom q-qcon-user ""
  "If non-nil, qcon will log in to remove q server with this id."
  :type 'string
  :group 'q-qcon)

(defcustom q-qcon-password ""
  "Password for remote q server."
  :type 'string
  :group 'q-qcon)

(defun q-customize ()
  "Customize 'q-mode'."
  (interactive)
  (customize-group "q"))

(defvar q-active-buffer nil
  "The name of the q-shell buffer to send q commands")

(defun q-activate-this-buffer ()
  (interactive)
  (q-activate-buffer (current-buffer)))

(defun q-activate-buffer (buffer)
  (interactive "bactivate buffer: ")
  (setq q-active-buffer buffer))

(defun q-default-args ()
  "Build a list of default args out of the q-init customizable variables."
  (let ((args ""))
    (unless (equal q-init-file "") (setq args (format " %s" q-init-file)))
    (unless (equal q-init-port 0) (setq args (concat args (format " -p %s" q-init-port))))
    (unless (equal q-init-slaves 0) (setq args (concat args (format " -s %s" q-init-slaves))))
    (unless (equal q-init-workspace 0) (setq args (concat args (format " -w %s" q-init-workspace))))
    args))

(defun q-qcon-default-args ()
  "Build a list of default args out of the q-init customizable variables."
  (let ((args (format "%s:%s" q-qcon-server q-qcon-port)))
    (unless (equal q-qcon-user "") (setq args (concat args (format ":%s:%s" q-qcon-user q-qcon-password))))
    args))

(defun q-shell-name (server port)
  "Build name of q-shell based on server and port."
  (concat "q-"
          (if (equal server "") "localhost" server)
          (unless (equal port "") (format ":%s" port))))

(defun q (&optional svr usr args)
  "Start a new q process.  Optional argument ARGS specifies the
command line args to use when executing q; the default ARGS are
obtained from the q-init customization variables.

In interactive use, a prefix argument directs this command
to read the command line arguments from the minibuffer."
  (interactive (let* ((args (q-default-args))
                      (usr  q-user)
                      (svr  q-server))
                 (if current-prefix-arg
                     (list (read-string "Server: " svr)
                           (read-string "User: " usr)
                           (read-string "Q command line args: " args))
                   (list svr usr args))))

  (unless (equal usr "") (setq svr (format "%s@%s" usr svr)))
  (let* ((cmd inferior-q-program-name)
         (cmd (if (equal args "") cmd (concat cmd args)))
         (qs (not (equal svr "")))
         (port (if (with-temp-buffer (setq case-fold-search nil)(string-match "-p *\\([0-9]+\\)" args)) (match-string 1 args) ""))
         (buffer (get-buffer-create (format "*%s*" (q-shell-name svr port))))
         (command (if qs "ssh" (getenv "SHELL")))
         (switches (append (if qs (list "-t" svr) (list "-c")) (list cmd)))
         process)
    (pop-to-buffer buffer)
    (when (or current-prefix-arg (not (comint-check-proc buffer)))
      (message "Starting q with the following command: \"%s\"" cmd)
      (inferior-q-mode)
      (setq args (list buffer "q" command nil switches))
      (setq process (get-buffer-process (apply `comint-exec args)))

      (setq comint-input-ring-file-name "~/.q_history")
      (comint-read-input-ring t)
      (set-process-sentinel process 'q-process-sentinel)
      (unless q-active-buffer (q-activate-buffer (buffer-name buffer)))
      process)))

(defun qcon (&optional args)
  "Connect to a pre-existing q process.
Optional argument ARGS specifies the command line args to use
when executing qcon; the default ARGS are obtained from the
q-server and q-init-port customization variables.

In interactive use, a prefix argument directs this command
to read the command line arguments from the minibuffer."
  (interactive (let* ((args (q-qcon-default-args)))
                 (list (if current-prefix-arg
                           (read-string "qcon command line args: " args)
                         args))))
  (let* ((buffer (get-buffer-create (format "*%s*" (format "qcon-%s" args))))
         process)
    (pop-to-buffer buffer)
    (when (or current-prefix-arg (not (comint-check-proc buffer)))
      (message "Starting qcon with the following cmd: \"%s\"" (concat inferior-qcon-program-name " " args))
      (inferior-q-mode)
      (setq comint-process-echoes nil)
      (setq process (get-buffer-process (comint-exec buffer "qcon" inferior-qcon-program-name nil (list args))))
      (setq comint-input-ring-file-name (concat (getenv "HOME") "/.qcon_history"))
      (comint-read-input-ring)
      (set-process-sentinel process 'q-process-sentinel)
      (unless q-active-buffer (q-activate-buffer (buffer-name buffer)))
      process)))

(defun q-show-q-buffer ()
  "Switch to the active q process, or start a new one (passing in args)."
  (interactive)
  (unless (comint-check-proc (pop-to-buffer (get-buffer-create q-active-buffer)))
    (q)))

(defun q-kill-q-buffer ()
  "Kill the q process and its buffer"
  (interactive)
  (let* ((buffer (get-buffer q-active-buffer))
         (process (get-buffer-process buffer)))
    (if (comint-check-proc buffer) (kill-process process))
    (if buffer (kill-buffer buffer))))

(defun q-process-sentinel (process message)
  "Sentinel for use with q processes.
   This marks the process with a message, at a particular time point."
  (comint-write-input-ring)
  (let ((buffer (process-buffer process)))
    (setq message (substring message 0 -1)) ; strip newline
    (setq message (format "\nProcess %s %s at %s\n"
                          (process-name process) message (current-time-string)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert-before-markers message)))))

(defun q-strip (text)                 ; order matters, don't rearrange
  (while (string-match "[ \t\n;]/.*" text)(setq text (replace-match "" t t text))) ; / comments
  (while (string-match "[ \t]+$" text) (setq text (replace-match "" t nil text))) ; excess white space
  (while (string-match "\n[ \t]+" text) (setq text (replace-match " " t t text))) ; fold functions
  text)

(defun q-send-string (string)
  (unless (cdr (assoc `comint-process-echoes (buffer-local-variables (get-buffer q-active-buffer))))
      (with-current-buffer (get-buffer q-active-buffer)
        (goto-char (point-max))
        (insert-before-markers (concat string "\n"))))
  (comint-simple-send (get-buffer-process q-active-buffer) string))

(defun q-eval-region (start end)
  "Send the current region to the inferior q process."
  (interactive "r")
  (q-send-string (q-strip (buffer-substring start end))))

(defun q-eval-line ()
  "Send the current line to the inferior q process."
  (interactive)
  (q-eval-region (point-at-bol) (point-at-eol)))

(defun q-eval-buffer ()
  "Load current buffer into the inferior q process."
  (interactive)
  (q-eval-region (point-min) (point-max)))

(defun q-eval-function ()
  "Send the current function to the inferior q process."
  (interactive)
  (save-excursion
    (goto-char (point-at-eol))          ; go to end of line
    (let ((start (re-search-backward q-function-regex)) ; find beinning of function
          (end   (re-search-forward ":")) ; find end of function name
          (fun   (thing-at-point `sexp))) ; find function body
      (q-send-string (q-strip (concat (buffer-substring start end) fun))))))

(defun q-load-file()
  "Load current buffer's file into the inferior q process after saving."
  (interactive)
  (save-buffer)
  (q-send-string (format "\\l %s" (buffer-file-name))))

;; keymaps

(defvar inferior-q-mode-map
  (let ((inferior-q-mode-map (make-sparse-keymap)))
    (define-key inferior-q-mode-map (kbd "C-c M-RET") 'q-activate-this-buffer)
    (set-keymap-parent inferior-q-mode-map comint-mode-map)
    inferior-q-mode-map)
  "Keymap for inferior q mode")

(defvar q-mode-map
  (let ((q-mode-map (make-sparse-keymap)))
    (define-key q-mode-map "\C-c\C-l"    'q-eval-line)
    (define-key q-mode-map "\C-c\C-f"    'q-eval-function)
    (define-key q-mode-map "\C-c\C-r"    'q-eval-region)
    (define-key q-mode-map "\C-c\C-b"    'q-eval-buffer)
    (define-key q-mode-map "\C-c\M-l"    'q-load-file)
    (define-key q-mode-map (kbd "C-c M-RET") 'q-activate-buffer)
    (define-key q-mode-map "\C-c\C-q"   'q-show-q-buffer)
    (define-key q-mode-map "\C-c\C-\\"  'q-kill-q-buffer)
    (define-key q-mode-map "\C-c\C-z"   'q-customize)
    (define-key q-mode-map "\C-c\C-c"   'comment-region)
    q-mode-map)
  "Keymap for q major mode")

;; menu bars
(easy-menu-define q-menu q-mode-map
  "Menubar for q script commands"
  '("Q"
    ["Eval Line"      q-eval-line t]
    ["Eval Function"  q-eval-function t]
    ["Eval Region"    q-eval-region t]
    ["Eval Buffer"    q-eval-buffer t]
    ["Load File"      q-load-file t]
    "---"
    ["Comment Region" comment-region t]
    "---"
    ["Customize Q"    q-customize t]
    ["Show Q Shell"   q-show-q-buffer t]
    ["Kill Q Shell"   q-kill-q-buffer t]
    ))

(easy-menu-define inferior-q-menu inferior-q-mode-map
  "Menubar for q shell commands"
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

(defvar q-keywords
  (eval-when-compile
    (concat "\\_<"
            (regexp-opt
             '("abs" "acos" "asin" "atan" "avg" "bin" "binr" "by" "cor" "cos" "cov" "dev" "delete"
               "div" "do" "exec" "exit" "exp" "from" "getenv" "if" "in" "insert" "last"
               "like" "log" "max" "min" "prd" "select" "setenv" "sin" "sqrt" "ss"
               "sum" "tan" "update" "var" "wavg" "while" "within" "wsum" "xexp") t) "\\_>"))
  "Keywords for q mode")

(defvar q-type-words
  (eval-when-compile
    (concat "\\(`\\_<"
            (regexp-opt '("boolean" "byte" "short" "long" "real" "int" "float" "char" "symbol"
                          "month" "date" "datetime" "minute" "second" "time" "timespan" "timestamp"
                          "year" "mm" "dd" "hh" "uu" "ss" "week") t) "\\_>\\)\\s *[$]"))
  "Types for q mode")

(defvar q-builtin-words
  (eval-when-compile
    (concat "\\_<\\(?:[.]q[.]\\)?"
            (regexp-opt
             '( "aj" "aj0" "all" "and" "any" "asc" "asof" "attr" "avgs" "ceiling"
                "cols" "count" "cross" "csv" "cut" "deltas" "desc"
                "differ" "distinct" "dsave" "each" "ej" "enlist" "eval" "except" "fby" "fills"
                "first" "fkeys" "flip" "floor" "get" "group" "gtime" "hclose" "hcount"
                "hdel" "hopen" "hsym" "iasc" "idesc" "ij" "inter" "inv" "key" "keys"
                "lj" "ljf" "load" "lower" "lsq" "ltime" "ltrim" "mavg" "maxs" "mcount" "md5"
                "mdev" "med" "meta" "mins" "mmax" "mmin" "mmu" "mod" "msum" "neg"
                "next" "not" "null" "or" "over" "parse" "peach" "pj" "plist" "prds" "prior"
                "prev" "rand" "rank" "ratios" "raze" "read0" "read1" "reciprocal" "reval"
                "reverse" "rload" "rotate" "rsave" "rtrim" "save" "scan" "scov" "sdev" "set" "show"
                "signum" "ssr" "string" "sublist" "sums" "sv" "svar" "system" "tables" "til"
                "trim" "txf" "type" "uj" "ungroup" "union" "upper" "upsert" "value"
                "view" "views" "vs" "where" "wj" "wj1" "ww" "xasc" "xbar" "xcol" "xcols" "xdesc"
                "xgroup" "xkey" "xlog" "xprev" "xrank") t) "\\_>"))
  "Builtin functions defined in q.k")

(defvar q-constant-words
  (eval-when-compile
    (concat "\\_<"
            (regexp-opt '(".z.D" ".z.K" ".z.T" ".z.Z" ".z.N" ".z.P" ".z.a" ".z.b" ".z.d" ".z.exit" ".z.f"
                          ".z.h" ".z.i" ".z.k" ".z.l" ".z.o" ".z.pc" ".z.pg" ".z.ph" ".z.pi"
                          ".z.po" ".z.pp" ".z.ps" ".z.pw" ".z.s" ".z.t" ".z.ts" ".z.u" ".z.vs"
                          ".z.w" ".z.x" ".z.z" ".z.n" ".z.p" ".z.ws" ".z.bm") t) "\\_>"))
  "Constants for q mode")

(defvar q-font-lock-keywords
  (list '("^\\\\\\_<.*?$" 0 font-lock-constant-face keep) ; lines starting with a '\' are compile time
        )
  "Minimal highlighting expressions for q mode")

(defvar q-font-lock-keywords-1
  (append q-font-lock-keywords
          (list
           '("`:\\(?:\\w\\|[/:._]\\)*" . font-lock-preprocessor-face) ; files
           (list q-type-words 1 font-lock-type-face nil) ; `minute`year
           '("\\(`\\_<[gpsu]\\)#" 1 font-lock-type-face nil) ; attributes
           '("`\\(?:\\(?:\\w\\|[.]\\)\\(?:\\w\\|\\s_\\)*\\)?" . font-lock-constant-face) ; symbols
           (cons q-constant-words 'font-lock-constant-face) ; .z.*
           (cons q-keywords  'font-lock-keyword-face)   ; select from
           '("\\b[0-2]:" . font-lock-preprocessor-face) ; IO/IPC
           ))
  "More highlighting expressions for q mode")

(defvar q-function-regex
  "\\_<\\([.]?[a-zA-Z]\\(?:\\s_\\|\\w\\)*\\s *\\):\\s *{"
  "Regular expression used to find function declarations")

(defvar q-variable-regex
  "\\_<\\([.]?[a-zA-Z]\\(?:\\s_\\|\\w\\)*\\s *\\)[-.~=!@#$%^&*_+|,<>?]?::?"
  "Regular expression used to find variable declarations")

(defvar q-font-lock-keywords-2
  (append q-font-lock-keywords-1
          (list
           (list q-function-regex 1 font-lock-function-name-face nil) ; functions
           (list q-variable-regex 1 font-lock-variable-name-face nil) ; variables
           (cons q-builtin-words 'font-lock-builtin-face) ; q.k
           ))
  "Even More highlighting expressions for q mode")

(defvar q-font-lock-keywords-3
  (append q-font-lock-keywords-2
          (list
           '("^'.*?$" 0 font-lock-warning-face t)   ; error
           '("'`\\w*" 0 font-lock-warning-face t) ; signal
           '("\\_<[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\(?:m\\|\\.[0-9][0-9]\\(?:T\\(?:[0-9][0-9]:[0-9][0-9]\\(?:[:][0-9][0-9]\\(?:\\.[0-9]*\\)?\\)?\\)?\\)?\\)\\_>" . font-lock-constant-face) ; month/date/datetime
           '("\\_<\\(?:[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]\\|[0-9]+\\)D\\(?:[0-9][0-9]\\(?:[:][0-9][0-9]\\(?:[:][0-9][0-9]\\(?:\\.[0-9]*\\)\\)?\\)?\\)?\\_>" . font-lock-constant-face) ; timespan/timestamp
           '("\\_<[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\_>" . font-lock-constant-face) ; guid
           '("\\_<[0-9][0-9]:[0-9][0-9]\\(?:[:][0-9][0-9]\\(?:\\.[0-9]\\|\\.[0-9][0-9]\\|\\.[0-9][0-9][0-9]\\)?\\)?\\_>" . font-lock-constant-face) ; time
           '("\\<[0-9]*[0-9.][0-9]*\\(?:[eE]-?[0-9]+\\)?[ef]?\\>" . font-lock-constant-face) ; floats/reals
           '("\\_<[0-9]+[efhijD]?\\_>" . font-lock-constant-face) ; short/long/real/float/timespan
           '("\\_<[01]+b\\_>" . font-lock-constant-face) ; bool
           '("\\_<0x[0-9a-fA-F]+\\_>" . font-lock-constant-face) ; bytes
           '("\\_<0[nNwW][cefghijmndzuvtp]?\\_>" . font-lock-constant-face) ; null/infinity
           '("\\(?:TODO\\|NOTE\\)\\:?" 0 font-lock-warning-face t) ; TODO
           ))
  "Most highlighting expressions for q mode")

(defvar q-font-lock-defaults
  '((q-font-lock-keywords q-font-lock-keywords-1 q-font-lock-keywords-2 q-font-lock-keywords-3)
    nil nil nil nil
    (font-lock-syntactic-keywords . (("^\\(\/\\)\\s *$"     1 "< b") ; begin multiline comment /
                                     ("^\\(\\\\\\)\\s *$"   1 "> b") ; end multiline comment   \
                                     ("\\(?:^\\(?:[a-z])\\)?\\|[ \t\n;]\\)\\(\/\\)"    1 "<  ") ; comments start with ' ' or ';'
                                     ("\\(\"\\)\\(?:[^\"\\\\]\\|\\\\.\\)*?\\(\"\\)" (1 "\"") (2 "\""))
                                     )))
  "List of Font lock keywords to properly highlight q syntax")


;; syntax table

(defvar q-mode-syntax-table
  (let ((q-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" ".  " q-mode-syntax-table) ; treat " as punctuation
    (modify-syntax-entry ?\/ ".  " q-mode-syntax-table) ; treat / as punctuation
    (modify-syntax-entry ?\n ">  " q-mode-syntax-table) ; comments are ended by a new line
    (modify-syntax-entry ?\r ">  " q-mode-syntax-table) ; comments are ended by a new line
    (modify-syntax-entry ?\. "_  " q-mode-syntax-table) ; treat . as a symbol
    (modify-syntax-entry ?\\ ".  " q-mode-syntax-table) ; treat \ as punctuation
    (modify-syntax-entry ?\$ ".  " q-mode-syntax-table) ; treat $ as punctuation
    (modify-syntax-entry ?\% ".  " q-mode-syntax-table) ; treat % as punctuation
    (modify-syntax-entry ?\& ".  " q-mode-syntax-table) ; treat & as punctuation
    (modify-syntax-entry ?\+ ".  " q-mode-syntax-table) ; treat + as punctuation
    (modify-syntax-entry ?\, ".  " q-mode-syntax-table) ; treat , as punctuation
    (modify-syntax-entry ?\- ".  " q-mode-syntax-table) ; treat - as punctuation
    (modify-syntax-entry ?\= ".  " q-mode-syntax-table) ; treat < as punctuation
    (modify-syntax-entry ?\* ".  " q-mode-syntax-table) ; treat * as punctuation
    (modify-syntax-entry ?\< ".  " q-mode-syntax-table) ; treat < as punctuation
    (modify-syntax-entry ?\> ".  " q-mode-syntax-table) ; treat > as punctuation
    (modify-syntax-entry ?\| ".  " q-mode-syntax-table) ; treat | as punctuation
    (modify-syntax-entry ?\` "'  " q-mode-syntax-table) ; treat ` as expression prefix
    q-mode-syntax-table)
  "Syntax table for q-mode")

;; modes

(define-derived-mode inferior-q-mode comint-mode "Q-Shell"
  "Major mode for interacting with a q interpreter"
  :syntax-table q-mode-syntax-table
  (add-hook (make-local-variable `comint-output-filter-functions) 'comint-strip-ctrl-m)
  (setq comint-prompt-regexp "^\\(q)+\\|[^:]*:[0-9]+>\\)")
  (set (make-local-variable 'font-lock-defaults) q-font-lock-defaults)
  (set (make-local-variable 'comint-process-echoes) nil)
  ;;  (set (make-local-variable 'comint-process-echoes) (not (equal q-server "")))
  (set (make-local-variable 'comint-password-prompt-regexp) "[Pp]assword")
  (font-lock-mode t)
  (setq truncate-lines t)
  (easy-menu-add inferior-q-menu)
  )

(defvar q-imenu-generic-expression
  (list (list nil (concat "^" q-variable-regex) 1))
  "Regular expresions to get q expressions into imenu")

(define-derived-mode q-mode nil "Q-Script"
  "Major mode for editing q language files"
  :group `q
  (set (make-local-variable 'font-lock-defaults) q-font-lock-defaults)
  (set (make-local-variable 'comment-start) q-comment-start)
  (set (make-local-variable 'comment-start-skip) "\\(/+\\)\\s *")
  (set (make-local-variable 'comment-end) "")
  ;; Keep <tabs> out of the code.
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tab-stop-list) '(1))
  ;; (set (make-local-variable 'indent-line-function) 'q-indent-line)
  (easy-menu-add q-menu)
  ;; enable imenu
  (set (make-local-variable 'imenu-generic-expression) q-imenu-generic-expression)
  )

;; enable hide-show minor mode
(add-to-list 'hs-special-modes-alist '(q-mode "{" "}" "^\\s */" nil hs-c-like-adjust-block-beginning))

(provide 'q-mode)

;;; q-mode.el ends here
