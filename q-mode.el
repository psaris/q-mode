;;; q-mode.el --- modes for editing and running Q scripts -*- Emacs-Lisp -*-

;; Copyright (C) 1997-2002 Free Software Foundation, Inc.
;; Copyright (C) 1999-2002 Albert Graef

;; Author/Maintainer: Albert Graef
;; <ag@muwiinfa.geschichte.uni-mainz.de, Dr.Graef@t-online.de>

;; This file is part of the Q programming system.

;; The Q programming system is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; The Q programming system is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:

;; This version is for recent releases of the Q programming system (V3.0 or
;; later).

;; See "The Q Programming Language", Section "Running Scripts in GNU Emacs"
;; for a brief description.

;; To a large extent, this code has been pilfered from other language modes,
;; most notably elisp and octave mode (therefore the FSF copyright at the
;; beginning of this file). Preliminary support for autoindent and comment
;; filling is now also included (taken from Prolog and C++ modes).

;; KNOWN BUGS (Q 7.7 and later): Autoindent of equations doesn't work very
;; well with left-hand qualifiers right now; it easily gets confused by
;; 'where' clauses on the left-hand side of an equation. There's no easy way
;; to get this right without doing a full parse of the equation to be
;; indented. Thus, for the time being, the code immediately following a
;; left-hand qualifier will often have to be indented manually.

;; INSTALLATION: If necessary, edit the values of the `q-prog', `q-data-dir'
;; and `q-lib-dir' variables below.

(defvar q-prog (expand-file-name "$QHOME/l64/q"))
(defvar q-data-dir (expand-file-name "$QHOME/l64/q"))
(defvar q-lib-dir (expand-file-name "$QHOME/l64/q"))

;; Then copy this file to your site-lisp directory. The easiest way to make Q
;; mode available in emacs is to add the following to your emacs startup file:

;; (require 'q-mode)

;; Alternatively, you may specify the following autoloads:

;; (autoload 'q-mode "q-mode")
;; (autoload 'q-help "q-mode")
;; (autoload 'run-q "q-mode")

;; To enable Q mode for *.q files, add the following to your emacs startup
;; file:

;; (setq auto-mode-alist (cons '("\\.q$" . q-mode) auto-mode-alist))

;; Furthermore, you can enable font lock (syntax highlighting) as follows:

;; (add-hook 'q-mode-hook 'turn-on-font-lock)
;; (add-hook 'q-eval-mode-hook 'turn-on-font-lock)

;; Well, that's the way it works with XEmacs and newer GNU Emacs versions. For
;; older versions of GNU Emacs you might have to try something like:

;; (global-font-lock-mode t)
;; (add-hook 'q-mode-hook (lambda () (font-lock-mode 1)))
;; (add-hook 'q-eval-mode-hook (lambda () (font-lock-mode 1)))

;; Using the Q-Eval hook you can also rebind the cursor up and down keys to
;; the history cycling commands:

;; (add-hook 'q-eval-mode-hook
;;	   (lambda ()
;;	     (define-key q-eval-mode-map [up] 'comint-previous-input)
;;	     (define-key q-eval-mode-map [down] 'comint-next-input)))

;; Finally, you might wish to add some global key bindings, e.g.:

;; (global-set-key "\C-h\C-q" 'q-help)
;; (global-set-key "\C-c\C-q" 'run-q)

;; GNUSERV SUPPORT: (Good stuff. ;-) This option becomes available when
;; setting the `q-gnuclient' variable to `t'. Next time an interpreter is
;; started, emacs will try to ensure that a gnuserv process is running. If
;; successful, the interpreter will be invoked with the --gnuclient option, in
;; order to synchronize between emacs and the interpreter. This means, in
;; particular, that emacs will track commands like path, cd, run, etc., and
;; update the state of the Q eval buffer accordingly. Moreover, edit and help
;; commands will be executed directly in emacs. This helps a lot, since
;; without this synchronization the interpreter will have its own idea of its
;; current directory, search path, history, running script, etc., which can
;; lead to some confusion. Therefore I strongly recommend enabling this
;; option, although it is disabled by default, because FSF Emacs still comes
;; without the gnuserv package. If you want to install the gnuserv package
;; yourself, you can retrieve it from the usual elisp archives.

;; Note that gnuserv, by default, pops up a new frame when a client command is
;; received. To avoid this, set the `gnuserv-frame' variable to `t'. Another
;; potential obstacle is that, with the current gnuserv version, only one
;; gnuserv process can be active at any time. Thus, if you already invoked
;; gnuserv with another emacs instance, then gnuserv will not work with the
;; present instance (in this case running a script will produce a message from
;; gnuserv saying that the gnuserv process has exited).

(require 'comint)

;; customizable variables

(defgroup q nil "Major modes for editing and running Q scripts."
  :group 'languages)

(defgroup q-options nil
  "Q interpreter options. Note that changing any of these options does
not affect a running instance of the interpreter; rather they denote
the settings to be used when a new interpreter is started."
  :group 'q)

(defcustom q-default-rhs-indent 32
  "*Default indentation of the right-hand side of a rule."
  :type 'integer
  :group 'q )

(defcustom q-extra-decl-indent 2
  "*Extra indentation of continuation lines in declarations."
  :type 'integer
  :group 'q )

(defcustom q-extra-qual-indent 2
  "*Extra indentation of qualifiers in rules."
  :type 'integer
  :group 'q )

(defcustom q-hanging-comment-ender-p t
  "*Controls what \\[fill-paragraph] does to Q block comment enders.
When set to nil, Q block comment enders are left on their own line.
When set to t, block comment enders will be placed at the end of the
previous line (i.e. they `hang' on that line)."
  :type 'boolean
  :group 'q)

(defcustom q-hanging-comment-starter-p t
  "*Controls what \\[fill-paragraph] does to Q block comment starters.
When set to nil, Q block comment starters are left on their own line.
When set to t, text that follows a block comment starter will be
placed on the same line as the block comment starter (i.e. the text
`hangs' on that line)."
  :type 'boolean
  :group 'q)

(defcustom q-prog-name q-prog
  "*Name of the interpreter executable."
  :type 'string
  :group 'q)

(defcustom q-prog-opts nil
  "*List of extra command line options the interpreter is invoked with.
These options will be prepended to the options set in the Options group below.
See the Q online documentation for a list of available options."
  :type '(repeat string)
  :group 'q)

(defcustom q-prog-args nil
  "*List of extra arguments the interpreter is invoked with."
  :type '(repeat string)
  :group 'q)

(defcustom q-path-separator path-separator
  "*Separator to be used to delimit directories in `q-path' when passing the
path to the interpreter. Usually this is just identical to `path-separator'.
You only have to set this value if your Emacs assumes another path separator
than the interpreter."
  :type 'string
  :group 'q)

(defcustom q-path 
  (let ((qpath (getenv "QPATH")))
    (if qpath (split-string qpath q-path-separator)
      (if (string= q-data-dir q-lib-dir)
	  (list "." q-lib-dir)
	(list "." q-data-dir q-lib-dir))))
  "*A list of directories to be searched by the interpreter for scripts and
code files. Initialized from the QPATH environment variable if set."
  :type '(repeat string)
  :group 'q-options)

(defcustom q-histfile "~/.q_history"
  "*Name of the command history file."
  :type 'string
  :group 'q-options)

(defcustom q-histsize 500
  "*Size of the command history."
  :type 'integer
  :group 'q-options)

(defcustom q-int-format "dec"
  "*Integer output format."
  :type '(choice (const :tag "Decimal" "dec")
		 (const :tag "Hexadecimal" "hex")
		 (const :tag "Octal" "oct"))
  :group 'q-options)

(defcustom q-float-format "std"
  "*Floating point output format."
  :type '(choice (const :tag "Standard" "std")
		 (const :tag "Scientific" "sci")
		 (const :tag "Fixed" "fix"))
  :group 'q-options)

(defcustom q-float-prec 15
  "*Floating point output precision."
  :type 'integer
  :group 'q-options)

(defcustom q-cstacksize 256
  "*Maximum C stack size of the interpreter (KB)."
  :type 'integer
  :group 'q-options)

(defcustom q-stacksize 1024000
  "*Maximum stack size of the interpreter."
  :type 'integer
  :group 'q-options)

(defcustom q-memsize 4096000
  "*Maximum memory size of the interpreter."
  :type 'integer
  :group 'q-options)

(defcustom q-debug nil
  "*Enable debugging."
  :type 'boolean
  :group 'q-options)

(defcustom q-debug-options ""
  "*Debugger options."
  :type 'string
  :group 'q-options)

(defcustom q-break nil
  "*Enable debugging after break."
  :type 'boolean
  :group 'q-options)

(defcustom q-echo nil
  "*Enable command echoing in batch mode."
  :type 'boolean
  :group 'q-options)

(defcustom q-quiet t
  "*Disable sign-on at startup."
  :type 'boolean
  :group 'q-options)

(defcustom q-gnuclient nil
  "*Run the interpreter as a gnuserv client. (Before you enable this, make
sure that your Emacs supports the gnuserv package.)"
  :type 'boolean
  :group 'q-options)

(defcustom q-query-before-kill nil
  "*Indicates that the user should be prompted before zapping an existing
interpreter process when starting a new one."
  :type 'boolean
  :group 'q)

;; I disabled this stuff because it does not work reliably with the new \\x
;; escape sequences. Thus if you set a customized prompt, e.g., in the qinitrc
;; file, you probably have to change the q-prompt-regexp customization
;; variable accordingly. - AG

;;;; default prompts of the interpreter; make sure that the command prompt is
;;;; the first in this list, and that not all these strings are empty
;;(defvar q-prompts '("\n==> " ": " "> "))

;;(defun lastline (x)
;;  (let ((l (split-string x "\n")))
;;    (nth (1- (length l)) l)))

;;(defun q-make-prompt-regexp ()
;;  (mapconcat
;;   'identity
;;   (let ((l (mapcar (lambda (x) (concat "^" (regexp-quote x)))
;;		    (cdr q-prompts)))
;;	 (s (lastline (car q-prompts))))
;;     (if (string= s "")
;;	 l
;;       (cons (concat "^" (regexp-quote s)) l)))
;;   "\\|"))
;;
;;(defcustom q-prompt-regexp (q-make-prompt-regexp)

(defcustom q-prompt-regexp "^==> \\|^[A-Za-z_0-9-]*>> \\|^: \\|^> "
  "*Regexp to match prompts in the Q interpreter. If you customize the
interpreter's default prompt, you will have to change this value accordingly."
  :type 'regexp
  :group 'q)

(defcustom q-msg-regexp
  "^[ \t]*\\([0-9]+>\\|\\sw+\\)?[ \t]+\\(\\([^ \t\n]+\\), line \\([0-9]+\\)\\):"
"*Regexp to match compiler and debugger messages with source line references
in the Q eval buffer.  Expression 1 denotes the optional message prefix
(usually either \"Error\", \"Warning\" or a stack index), expression 2 the
whole source line info, expression 3 the file name and expression 4 the
corresponding line number."
  :type 'regexp
  :group 'q)

(defcustom q-help-files '("qdoc")
  "*List of info files to scan for online documentation about Q.
Default: (\"qdoc\").")

(defcustom q-mode-hook nil
  "*Hook for customising Q mode.
For instance, add `turn-on-font-lock' to enable syntax highlighting."
  :type 'hook
  :group 'q)

(defcustom q-eval-mode-hook nil
  "*Hook for customising Q eval mode.
For instance, add `turn-on-font-lock' to enable syntax highlighting."
  :type 'hook
  :group 'q)

;; the following are used internally

(defvar q-output-list nil)
(defvar q-output-string nil)
(defvar q-receive-in-progress nil)
(defvar q-last-dir nil)
(defvar q-last-script nil)
(defvar q-other-script nil)
(defvar q-last-path nil)
(defvar q-gnuclient-active nil)

;; font-lock support

(defvar q-eval-font-lock-keywords
  (list
   ;; make sure the prompt is the first pattern, since it is updated by
   ;; q_prompt_cmd
   (list q-prompt-regexp 0 'font-lock-type-face t)
   (list "^!.*" 0 'font-lock-warning-face t)
   (list "^Warning:" 0 'font-lock-warning-face t)
;; (list "\\(//\\|^#!\\).*" 0 'font-lock-comment-face t)
   (list q-msg-regexp 0 'font-lock-warning-face t)
   (list "\\<\\(if\\|else\\|then\\|where\\)\\>\\|  ==>  " 0 'font-lock-keyword-face))
  "Rules for fontifying in Q-Eval mode.")

;; FIXME: need support for unicode identifiers here

(defvar q-font-lock-keywords
  (list
;; (list "\\(//\\|^#!\\).*" 0 'font-lock-comment-face t)
   (list "\\([A-Za-z_][A-Za-z_0-9]*\\)::" 1 'font-lock-constant-face)
;; (list "[^:]: *\\([A-Za-z_][A-Za-z_0-9]*\\)" 1 'font-lock-type-face)
;; (list "[^:]: *\\([A-Za-z_][A-Za-z_0-9]*\\)::\\([A-Za-z_][A-Za-z_0-9]*\\)"
;;	 2 'font-lock-type-face)
;; (list "\\<type\\>[ \t\n]+\\([A-Za-z_][A-Za-z_0-9]*\\)"
;;	 1 'font-lock-type-face)
   (list (concat "\\<\\("
		 "as\\|const\\|def\\|extern\\|i\\(f\\|mport\\|nclude\\)"
		 "\\|otherwise\\|p\\(rivate\\|ublic\\)\\|special\\|type\\|"
		 "undef\\|var\\|virtual\\|where"
		 "\\)\\>")
	 0 'font-lock-keyword-face)
;; additional operator keywords
   (list "\\<\\(and\\|div\\|else\\|in\\|mod\\|not\\|or\\|then\\)\\>" 0
	 'font-lock-keyword-face)
   (list "\\<\\(_\\|[A-Z][A-Za-z_0-9]*\\)\\>" 1 'font-lock-variable-name-face)
   )
  "Rules for fontifying Q scripts.")

;; keymaps

(defvar q-mode-map nil)
(cond ((not q-mode-map)
       (setq q-mode-map (make-sparse-keymap))
       (define-key q-mode-map "\C-c\C-h" 'q-help)
       (define-key q-mode-map "\C-c\C-l" 'q-run-script)
       (define-key q-mode-map "\C-c\C-x" 'q-quit-eval)
       (define-key q-mode-map "\C-c\C-m" 'q-run-main)
       (define-key q-mode-map "\C-c\C-u" 'q-current-msg)
       (define-key q-mode-map "\C-c\C-n" 'q-next-msg)
       (define-key q-mode-map "\C-c\C-p" 'q-prev-msg)
       (define-key q-mode-map "\C-c\C-z" 'q-last-msg)
       (define-key q-mode-map "\C-c\C-c" 'q-do-cmd)
       (define-key q-mode-map "\C-c\C-b" 'q-do-buf)
       (define-key q-mode-map "\C-c\C-a" 'q-first-msg)
       (define-key q-mode-map "\C-c\C-f" 'q-find-script)
       (define-key q-mode-map "\C-c\C-v" 'q-goto-input-line)
       (define-key q-mode-map "\t" 'q-indent-line)
       (define-key q-mode-map "(" 'q-electric-delim)
       (define-key q-mode-map ")" 'q-electric-delim)
       (define-key q-mode-map "[" 'q-electric-delim)
       (define-key q-mode-map "]" 'q-electric-delim)
       (define-key q-mode-map ";" 'q-electric-delim)
       (define-key q-mode-map "=" 'q-electric-delim)
       (define-key q-mode-map "|" 'q-electric-delim)
       (define-key q-mode-map "\e\C-i" 'q-move-to-indent-column)
       (define-key q-mode-map "\e\C-q" 'q-indent-current-rule)))

(defvar q-eval-mode-map nil)
(cond ((not q-eval-mode-map)
       (setq q-eval-mode-map (copy-keymap comint-mode-map))
       (define-key q-eval-mode-map "\C-c\C-h" 'q-help)
       (define-key q-eval-mode-map "\t" 'comint-dynamic-complete)
       (define-key q-eval-mode-map "\C-a" 'comint-bol)
       (define-key q-eval-mode-map [home] 'comint-bol)
;;       (define-key q-eval-mode-map [up] 'comint-previous-input)
;;       (define-key q-eval-mode-map [down] 'comint-next-input)
       (define-key q-eval-mode-map [return] 'q-current-msg-or-send)
       (if (string-match "XEmacs\\|Lucid" emacs-version)
	   (define-key q-eval-mode-map [button2] 'q-mouse-msg)
	 (define-key q-eval-mode-map [mouse-2] 'q-mouse-msg))
       (define-key q-eval-mode-map "\C-c\C-u" 'q-current-msg)
       (define-key q-eval-mode-map "\C-c\C-n" 'q-next-msg)
       (define-key q-eval-mode-map "\C-c\C-p" 'q-prev-msg)
       (define-key q-eval-mode-map "\C-c\C-e" 'q-last-msg)
       (define-key q-eval-mode-map "\C-c\C-a" 'q-first-msg)
       (define-key q-eval-mode-map "\C-c\C-f" 'q-find-script)
       (define-key q-eval-mode-map "\C-c\C-v" 'q-goto-input-line)))

;; menus

(defsubst q-region-is-active-p ()
  ;; Return t when the region is active.  The determination of region
  ;; activeness is different in both Emacs and XEmacs.
  (cond
   ;; XEmacs
   ((and (fboundp 'region-active-p)
	 zmacs-regions)
    (region-active-p))
   ;; Emacs
   ((boundp 'mark-active) mark-active)
   ;; fallback; shouldn't get here
   (t (mark t))))

(defvar ses-mode-menu
  '("SES"
    ["Insert row" ses-insert-row (ses-in-print-area)]
    ["Delete row" ses-delete-row (ses-in-print-area)]
    ["Insert column" ses-insert-column (ses-in-print-area)]
    ["Delete column" ses-delete-column (ses-in-print-area)]
    ["Set column printer" ses-read-column-printer t]
    ["Set column width" ses-set-column-width t]
    ["Set default printer" ses-read-default-printer t]
    ["Jump to cell" ses-jump t]
    ["Set cell printer" ses-read-cell-printer t]
    ["Recalculate cell" ses-recalculate-cell t]
    ["Truncate cell display" ses-truncate-cell t]
    ["Export values" ses-export-tsv t]
    ["Export formulas" ses-export-tsf t]))

(defvar q-mode-menu
  '("Q"
    ["Help Topics..."		q-help t]
    ["Describe Q Mode"		describe-mode t]
    ["Customize"			(customize-group 'q) t]
    "-"
    ["Move to `=' Column"		q-move-to-indent-column t]
    ["Indent Current Rule"		q-indent-current-rule t]
    ["Indent Line or Region"	q-indent-line-or-region t]
    ["Comment Out Region"		comment-region (q-region-is-active-p)]
    ["Uncomment Region"		uncomment-region (q-region-is-active-p)]
    ["Fill Comment Paragraph"	q-fill-paragraph t]
    "-"
    ["Run Script"			q-run-script t]
    ["Run Buffer"			q-do-buf t]
    ["Run Group"			q-do-cmd t]
    ["Find Main Script"		q-find-script q-last-script]
    ["Find Last Script"		q-find-script q-other-script]
    ["Run Main Script"		q-run-main t]
    ["Goto Input Line"		q-goto-input-line
     (get-process "q-eval")]
    "-"
    ["Quit Q"				q-quit-eval t]
    ["Current Message"		q-current-msg
     (get-buffer "*q-eval*")]
    ["First Message"		q-first-msg
     (get-buffer "*q-eval*")]
    ["Next Message"			q-next-msg
     (get-buffer "*q-eval*")]
    ["Previous Message"		q-prev-msg
     (get-buffer "*q-eval*")]
    ["Last Message"			q-last-msg
     (get-buffer "*q-eval*")]))

(defvar q-eval-mode-menu
  '("Q-Eval"
	["Help Topics..."		q-help t]
	["Describe Q-Eval Mode"		describe-mode t]
	["Customize"			(customize-group 'q) t]
	"-"
	["Find Main Script"		q-find-script q-last-script]
	["Goto Input Line"		q-goto-input-line
					(get-process "q-eval")]
	"-"
	["Current Message"		q-current-msg
					(get-buffer "*q-eval*")]
	["First Message"		q-first-msg
					(get-buffer "*q-eval*")]
	["Next Message"			q-next-msg
					(get-buffer "*q-eval*")]
	["Previous Message"		q-prev-msg
					(get-buffer "*q-eval*")]
	["Last Message"			q-last-msg
					(get-buffer "*q-eval*")]
	"-"
	["Complete Symbol"		comint-dynamic-complete
					(q-at-command-prompt-p)]))

;; some helper functions for q/q-eval-mode: check that we're on the command
;; resp. debugger prompt

(defun q-at-pmark-p ()
  (and (get-buffer "*q-eval*")
       (get-process "q-eval")
       (progn (set-buffer "*q-eval*") (comint-after-pmark-p))))

(defun q-at-command-prompt-p ()
  (and
   (q-at-pmark-p)
   (save-excursion
     (forward-line 0)
     (looking-at q-prompt-regexp))))
		   
(defun q-at-debug-prompt-p ()
  (and
   (q-at-pmark-p)
   (save-excursion
     (forward-line 0)
     (looking-at ":"))))

;; Q mode

;;;###autoload
(defun q-mode ()
  "Major mode for editing Q scripts.

Provides the `q-run-script' (\\[q-run-script]) command to run the interpreter
on the script in the current buffer. It will be verified that the buffer has a
file associated with it, and you will be prompted to save edited buffers when
invoking this command. Special commands to quickly locate the main script and
the input line of the Q eval buffer, and to visit the source lines shown in
compiler/debugger messages are provided as well (see `q-eval-mode').

These operations can be selected from the Q mode menu (accessible from
the menu bar), which also provides commands for reading the online
help and customizing the Q/Q-Eval mode setup.

Command list:

\\{q-mode-map}
Entry to this mode calls the value of q-mode-hook if that value is
non-nil."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "_")
  (modify-syntax-entry ?\.  "_")
  (modify-syntax-entry ?\+  ".")
  (modify-syntax-entry ?\-  ".")
  (modify-syntax-entry ?\=  ".")
  (modify-syntax-entry ?\<  ".")
  (modify-syntax-entry ?\>  ".")
  (modify-syntax-entry ?\$  ".")
  (modify-syntax-entry ?\|  ".")
  ;; comment syntax a la C++ mode
;  (cond
;   ;; XEmacs 19 & 20
;   ((memq '8-bit c-emacs-features)
;    (modify-syntax-entry ?/  ". 1456")
;    (modify-syntax-entry ?*  ". 23"))
;   ;; Emacs 19 & 20
;   ((memq '1-bit c-emacs-features)
;    (modify-syntax-entry ?/  ". 124b")
;    (modify-syntax-entry ?*  ". 23"))
;   ;; incompatible
;   (t (error "Q Mode is incompatible with this version of Emacs")))
  (cond
   ((string-match "XEmacs\\|Lucid" emacs-version)
    (modify-syntax-entry ?/  ". 1456")
    (modify-syntax-entry ?*  ". 23"))
   (t
    (modify-syntax-entry ?/  ". 124b")
    (modify-syntax-entry ?*  ". 23")))
  (modify-syntax-entry ?\n "> b")
  (modify-syntax-entry ?\^m "> b")
  (setq major-mode 'q-mode)
  (setq mode-name "Q")
  (use-local-map q-mode-map)
  (make-local-variable 'paragraph-start)
;;  (setq paragraph-start (concat "^$\\|" page-delimiter))
;;  (setq paragraph-start (concat "^//\\|^$\\|" page-delimiter))
  (setq paragraph-start (concat page-delimiter "\\|$"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (if (boundp 'fill-paragraph-function)
      (progn
	(make-local-variable 'fill-paragraph-function)
	(setq fill-paragraph-function 'q-fill-paragraph)))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'q-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'q-indent-region)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'q-prog-args)

  (require 'easymenu)  
  (easy-menu-define q-mode-menu-map q-mode-map
		    "Menu keymap for Q mode." q-mode-menu)

  (easy-menu-add q-mode-menu-map q-mode-map)

  (run-hooks 'q-mode-hook))

;; (define-derived-mode q-mode kdbp-mode "Q"

(defun q-minor-mode ()

  "Minor mode for editing Q scripts.

Provides the `q-run-script' (\\[q-run-script]) command to run the interpreter
on the script in the current buffer. It will be verified that the buffer has a
file associated with it, and you will be prompted to save edited buffers when
invoking this command. Special commands to quickly locate the main script and
the input line of the Q eval buffer, and to visit the source lines shown in
compiler/debugger messages are provided as well (see `q-eval-mode').

These operations can be selected from the Q mode menu (accessible from
the menu bar), which also provides commands for reading the online
help and customizing the Q/Q-Eval mode setup.

Command list:

\\{q-mode-map}
Entry to this mode calls the value of q-mode-hook if that value is
non-nil."
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (if (boundp 'fill-paragraph-function)
      (progn
	(make-local-variable 'fill-paragraph-function)
	(setq fill-paragraph-function 'q-fill-paragraph)))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'q-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'q-indent-region)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (setq comment-column 48
	comment-start "// "
	comment-end ""
	comment-start-skip "/\\*+ *\\|// *\\|^#! *"
	comment-multi-line nil
	)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'q-prog-args)
  (setq comment-indent-function 'q-comment-indent)
  ;; (make-local-variable 'font-lock-defaults)
  ;; (setq font-lock-defaults '(q-font-lock-keywords nil nil ((?_ . "w"))))

  (require 'easymenu)  
  (easy-menu-define q-mode-menu-map q-mode-map
		    "Menu keymap for Q mode." q-mode-menu)
  (easy-menu-add q-mode-menu-map q-mode-map)

  (run-hooks 'q-minor-mode-hook))

;; Q eval mode

(defun q-eval-mode ()

  "Major mode for interacting with the Q interpreter, based on comint-mode.

Provides the `q-current-msg-or-send' (\\[q-current-msg-or-send]) command,
which, when point is at a compiler or debugger message describing a source
reference, visits the given line in the corresponding source file in another
window. Otherwise it runs the `comint-send-input' command, which usually
submits a command line to the interpreter, or copies it to the command prompt
when point is not at the current command line.

Compiler and debugger messages are indicated with a special font, and in
XEmacs they will also be highlighted when the mouse passes over
them. Moreover, pressing the middle mouse button (button2) over such a message
visits the corresponding source line in another window (`q-mouse-msg'
command); anywhere else, the middle mouse button invokes the usual
`mouse-yank' command, so that you can also use the mouse to perform xterm-like
cut and paste in the Q-Eval buffer.

You can also use the `q-first-msg' (\\[q-first-msg]), `q-next-msg'
(\\[q-next-msg]), `q-prev-msg' (\\[q-prev-msg]) and `q-last-msg'
(\\[q-last-msg]) commands to scan through the compiler and debugger messages
found in the buffer. The `q-find-script' (\\[q-find-script]) command lets you
visit the script that is currently running, and `q-goto-input-line'
(\\[q-goto-input-line]) quickly takes you to the prompt at the current input
line in the Q eval buffer. (These commands are also provided in Q mode. If you
like, you can bind them globally, so that you can invoke them from other kinds
of buffers as well.)

Besides this, you can use the usual comint commands, see the description of
`comint-mode' for details. Some important commands are listed below:

\\[comint-previous-input] and \\[comint-next-input] cycle through the command history.
\\[comint-previous-matching-input] and \\[comint-next-matching-input] search the command history.
\\[comint-interrupt-subjob] sends a Ctl-C to the interpreter.
\\[comint-send-eof] sends a Ctl-D to the interpreter.
\\[comint-dynamic-list-input-ring] lists the command history.
\\[comint-dynamic-complete] performs symbol and filename completion.

Note that in difference to standard comint mode, the C-a/home keys are rebound
to `comint-bol', to mimic the behaviour of the default binding of these keys
in the interpreter.

Most of these operations can also be selected from the Comint and Q-Eval mode
menus accessible from the menu bar. The Q-Eval menu also provides operations
for reading the online help and customizing Q/Q-Eval mode setup. Moreover, a
History menu is provided from which the most recent commands can be selected.

The interpreter's prompt and lines containing compiler and debugger messages
are described by the variables `q-prompt-regexp' and `q-msg-regexp'. The
history file and size is given by the `q-histfile' and `q-histsize'
variables. Note that when the `q-gnuclient' customization option is enabled,
then Q-Eval mode automatically tracks the current prompt string and hence you
can safely use the `prompt' command in the interpreter.

A complete command list is given below:

\\{q-eval-mode-map}
Entry to this mode runs the hooks on `comint-mode-hook' and `q-eval-mode-hook'
(in that order)."

  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (set-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "_")
  (modify-syntax-entry ?\.  "_")
  (modify-syntax-entry ?\+  ".")
  (modify-syntax-entry ?\-  ".")
  (modify-syntax-entry ?\=  ".")
  (modify-syntax-entry ?\<  ".")
  (modify-syntax-entry ?\>  ".")
  (modify-syntax-entry ?\|  ".")
  (modify-syntax-entry ?\$  ".")
  (modify-syntax-entry ?\/  ". 12")
  (modify-syntax-entry ?\*  ".")
  (modify-syntax-entry ?\n  ">")
  (modify-syntax-entry ?\^m ">")
  (setq major-mode 'q-eval-mode)
  (setq mode-name "Q-Eval")
  (use-local-map q-eval-mode-map)
  (setq comint-prompt-regexp q-prompt-regexp)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (setq comment-column 48
	comment-start-skip "// *\\|^#! *"
	comment-multi-line nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(q-eval-font-lock-keywords nil nil ((?_ . "w"))))
  (setq comint-input-ring-file-name q-histfile
	comint-input-ring-size q-histsize
	comint-dynamic-complete-functions
	'(q-complete comint-dynamic-complete-filename))
  ;; mouse-sensitive messages (requires XEmacs)
  (cond
   ((string-match "XEmacs\\|Lucid" emacs-version)
    (require 'mode-motion)
    (setq mode-motion-hook 'q-motion-hook)))
  (comint-read-input-ring t)
  (require 'easymenu)  
  (easy-menu-define q-eval-mode-menu-map q-eval-mode-map
		    "Menu keymap for Q mode." q-eval-mode-menu)
  (easy-menu-add q-eval-mode-menu-map q-eval-mode-map)
  (run-hooks 'q-eval-mode-hook))

(if (string-match "XEmacs" emacs-version)
(defun q-motion-hook (event)
  (mode-motion-highlight-internal
    event
    #'beginning-of-line
    #'(lambda () 
	(if (looking-at q-msg-regexp)
	    (end-of-line))))
))

;; run a Q script in a Q eval buffer (with gnuserv if requested)

(defun q-gnuclient-option ()
  (if (not q-gnuclient)
      (progn (setq q-gnuclient-active nil) nil)
    (if (string-match "XEmacs\\|Lucid" emacs-version)
	(require 'gnuserv)
      ;; GNU emacs apparently requires the gnuserv-compat package
      (require 'gnuserv-compat)
      (require 'gnuserv))
    (if (not (eq (process-status "gnuserv") 'run))
	;; give the gnuserv process some time to get ready
	(progn (gnuserv-start) (sit-for 0.1 1)))
    (if (not (eq (process-status "gnuserv") 'run))
	nil
      (setq q-gnuclient-active t)
      (list "--gnuclient"))))

;; make sure win32 XEmacs quotes arguments containing whitespace

(if (string-match "XEmacs.*-win32" (emacs-version))
    (defun q-quote-arg (x)
      (if (string-match "[ \t]" x) (concat "\"" x "\"") x))
  (defun q-quote-arg (x) x))

;;;###autoload
(defun run-q (&rest args)

  "Run the interpreter with given arguments, in buffer *q-eval*.

The interpreter is invoked in the directory of the current buffer (current
default directory if no file is associated with the current buffer).
If buffer exists but process is not running, make new process.
If buffer exists and process is running, kill it and start a new one.

Program used comes from variable `q-prog-name'. Extra options and arguments
can be specified in the variables `q-prog-opts' and `q-prog-args'. The buffer
is put in Q eval mode, giving commands for visiting source files, sending
input, manipulating the command history, etc. See `q-eval-mode'.

If the `q-gnuclient' option is set, the interpreter will be run as a `gnuserv'
client (`gnuserv' is invoked if necessary). This means, in particular, that
emacs will track interpreter commands like `path', `cd', `run', etc., and
update the state of the Q eval buffer accordingly. Moreover, `edit' and `help'
commands will be executed directly in emacs. For this to work, you must have
the `gnuserv' package and no other emacs instance running `gnuserv' must be
active.

Note that `gnuserv', by default, pops up a new frame when a client command is
received. To avoid this, set the `gnuserv-frame' variable to `t'.

\(Type \\[describe-mode] in the Q eval buffer for a list of commands.)"

  (interactive)
  (let* ((dir (if buffer-file-name
		  (file-name-directory (buffer-file-name))
		default-directory))
	 (q-eval-active (not (null (get-buffer "*q-eval*"))))
	 (q-eval-running (comint-check-proc "*q-eval*"))
	 (q-eval-buffer (get-buffer-create "*q-eval*")))
    (if (and q-eval-running
	     q-query-before-kill
	     (not
	      (y-or-n-p
	       "An interpreter process is still running. Start a new one? ")))
	(message "Aborted")
      (set-buffer q-eval-buffer)
      ;; give process some time to terminate, then blast it away
      (if q-eval-running
	  (progn
	    (comint-send-eof)
	    (sleep-for .5)))
      (if (comint-check-proc "*q-eval*")
	  (progn
	    (comint-kill-subjob)
	    (sleep-for .1)))
      (cd dir)
      (if (not q-eval-active)
	  (q-eval-mode)
	(if (and q-eval-running
		 (or (not (string-equal
			   comint-input-ring-file-name q-histfile))
		     (not (= comint-input-ring-size q-histsize))))
	    ;; reset history in case any of the options have changed
	    (progn
	      (comint-write-input-ring)
	      (setq comint-input-ring-file-name q-histfile
		    comint-input-ring-size q-histsize)
	      (comint-read-input-ring t))))
      ;; show the directory and other parameters with which we start the
      ;; new process
      (goto-char (point-max))
      ;;(if q-eval-active (insert "\n"))

      (setq default-args (mapconcat 'identity (default-value 'q-prog-args) " "))
      (setq input-args (mapconcat 'identity args " "))
      ;; (insert "\n// [" (directory-file-name dir) "] "
      ;;	(mapconcat 'identity (list input-args default-args) ":") "\n")
      (setq largs (if (car (cdr args)) (split-string (car (cdr args))) (list "")))
      (setq dntarg (directory-file-name dir))
      (setq fntarg (list (car args)))
      (setq targ (list (mapconcat 'identity (append (list dntarg) fntarg ) "/")))
      ;; (insert "\n// [" (directory-file-name dir) "] "
      ;;     (mapconcat 'identity (append targ largs) ":") "\n")

      ;; we pass q-path in an environment variable since it can be long;
      ;; hopefully, this prevents probs on systems with small maximum command
      ;; line length
      (setenv "QPATH" (mapconcat 'identity q-path q-path-separator))
      ;; invoke the interpreter
      (let ((qargs
	     (append q-prog-opts
		     targ largs)))
	(comint-exec q-eval-buffer "q-eval" q-prog-name nil qargs))
      ;; set up process parameters
      (setq q-output-list nil
	    q-output-string nil
	    q-receive-in-progress nil
	    q-last-script nil
	    q-last-dir dir
	    q-last-path nil)
      (set-process-sentinel (get-process "q-eval") 'q-eval-sentinel)
      (if (not q-query-before-kill)
	  (process-kill-without-query (get-process "q-eval")))
      ;; switch to and go to the end of the eval buffer
      (pop-to-buffer "*q-eval*")
      (goto-char (point-max))))
  )

(defun q-quit-eval ()
  "Depending on whether point is at a compiler/debugger message, either
execute a `q-current-msg' or a `comint-send-input' command. This must be
invoked from the Q-Eval buffer."
  (interactive)
  (if (save-excursion (forward-line 0) (looking-at q-msg-regexp))
      (q-current-msg)
    (comint-simple-send "*q-eval*" "\\\\")))

(defun q-run-script ()
  "Run the interpreter with the script in the current buffer, in buffer
*q-eval*. See `run-q' for details."
  (interactive)
  (let ((script-file
	 (if (buffer-file-name)
	     (file-name-nondirectory (buffer-file-name))
	   (error "Buffer is not associated with any file"))))
    (save-some-buffers 1)
    (revert-buffer t) ; forces a re-read of the local variables
    (run-q script-file q-prog-args)
    (setq q-last-script script-file)))

(defun q-run-main () 
  "Run the interpreter with the script in the current buffer, in buffer
*q-eval*. See `run-q' for details."
  (interactive)
  (let ((script-file
	 (if (buffer-file-name)
	     (file-name-nondirectory (buffer-file-name))
	   (error "Buffer is not associated with any file"))))
    (q-find-script)
    (q-run-script)
    (switch-to-buffer-other-window script-file)
    (setq q-other-script script-file)))

;; find a script on the Q library path

(defun q-locate-script (file)
  (let ((script (locate-library file t q-path)))
    (if script
	script
      (error (concat "File " file " not found on search path")))))

;; visit source lines of error and debugging messages

(defun q-current-msg ()
  "Show the source line referenced by a compiler/debugger message on the
current line in the Q eval buffer."
  (interactive)
  (let ((actwindow (selected-window)))
    (if (get-buffer "*q-eval*")
	(pop-to-buffer "*q-eval*")
      (error "No script is running"))
    (cond
     ((save-excursion (forward-line 0) (looking-at q-msg-regexp))
      (forward-line 0) (recenter 0)
      (let (visit-buffer
	    visit-line
	    (file (match-string 3)) (line (match-string 4)))
	(setq visit-buffer (find-file-noselect (q-locate-script file)))
	(setq visit-line (string-to-number line))
	(message "%s, line %s" file line)
	(switch-to-buffer-other-window visit-buffer)
	(goto-line visit-line)))
     (t
      (select-window actwindow)
      (error "No message found")))))

(defun q-current-msg-or-send ()
  "Depending on whether point is at a compiler/debugger message, either
execute a `q-current-msg' or a `comint-send-input' command. This must be
invoked from the Q-Eval buffer."
  (interactive)
  (if (save-excursion (forward-line 0) (looking-at q-msg-regexp))
      (q-current-msg)
    (comint-send-input)))

(defun q-next-msg (&optional count)
  "Advance to the next Q compiler/debugger message below the current line in
the Q eval buffer, and show the referenced source line in another
window. When used with a numeric argument n, advance to the nth message below
the current line (move backwards if numeric argument is negative).

Note that this command can easily be fooled if the running script produces
some output, or you insert some text, which looks like a compiler or debugger
message, so you should take care what you're doing."
  (interactive "P")
  (if (and (numberp count) (< count 0))
      (q-prev-msg (- count))
    (if (null count) (setq count 1))
    (let ((actwindow (selected-window)))
      (if (get-buffer "*q-eval*")
	  (pop-to-buffer "*q-eval*")
	(error "No script is running"))
      (forward-line 0)
      (if (looking-at q-msg-regexp)
	  (if (save-excursion (end-of-line) (not (eobp)))
	      (forward-line 1)
	    (error "No more messages")))
      (let ((pos (re-search-forward q-msg-regexp nil t count)))
	(if pos
	    (let ((file (match-string 3)) (line (match-string 4)))
	      (goto-char pos)
	      (recenter 0)
	      (find-file-other-window (q-locate-script file))
	      (goto-line (string-to-number line))
	      (message "%s, line %s" file line))
	  (select-window actwindow)
	  (error "No more messages"))))))

(defun q-prev-msg (&optional count)
  "Advance to previous Q compiler/debugger messages above the current line in
the Q eval buffer, and show the referenced source line in another
window. Like `q-next-msg', but moves backward."
  (interactive "P")
  (if (and (numberp count) (< count 0))
      (q-next-msg (- count))
    (if (null count) (setq count 1))
    (let ((actwindow (selected-window)))
      (if (get-buffer "*q-eval*")
	  (pop-to-buffer "*q-eval*")
	(error "No script is running"))
      (forward-line 0)
      (let ((pos (re-search-backward q-msg-regexp nil t count)))
	(if pos
	    (let ((file (match-string 3)) (line (match-string 4)))
	      (goto-char pos)
	      (recenter 0)
	      (find-file-other-window (q-locate-script file))
	      (goto-line (string-to-number line))
	      (message "%s, line %s" file line))
	  (select-window actwindow)
	  (error "No more messages"))))))

(defun q-last-msg ()
  "Advance to the last message in a contiguous sequence of compiler/debugger
messages at or below the current line, and show the referenced source line
in another window."
  (interactive)
  (let ((actwindow (selected-window)))
    (if (get-buffer "*q-eval*")
	(pop-to-buffer "*q-eval*")
      (error "No script is running"))
    (forward-line 0)
    (let ((pos
	   (if (looking-at q-msg-regexp)
	       (point)
	     (re-search-forward q-msg-regexp nil t))))
      (if pos
	  (progn
	    (goto-char pos)
	    (while (and (save-excursion (end-of-line) (not (eobp)))
			(save-excursion (forward-line 1)
					(looking-at q-msg-regexp)))
	      (forward-line 1))
	    (let ((file (match-string 3)) (line (match-string 4)))
	      (recenter 0)
	      (find-file-other-window (q-locate-script file))
	      (goto-line (string-to-number line))
	      (message "%s, line %s" file line)))
	(select-window actwindow)
	(error "No more messages")))))

(defun q-first-msg ()
  "Advance to the first message in a contiguous sequence of compiler/debugger
messages at or above the current line, and show the referenced source line
in another window."
  (interactive)
  (let ((actwindow (selected-window)))
    (if (get-buffer "*q-eval*")
	(pop-to-buffer "*q-eval*")
      (error "No script is running"))
    (forward-line 0)
    (let ((pos
	   (if (looking-at q-msg-regexp)
	       (point)
	     (re-search-backward q-msg-regexp nil t))))
      (if pos
	  (progn
	    (goto-char pos)
	    (while (and (not (bobp))
			(save-excursion (forward-line -1)
					(looking-at q-msg-regexp)))
	      (forward-line -1))
	    (let ((file (match-string 3)) (line (match-string 4)))
	      (recenter 0)
	      (find-file-other-window (q-locate-script file))
	      (goto-line (string-to-number line))
	      (message "%s, line %s" file line)))
	(select-window actwindow)
	(error "No more messages")))))

(defun q-mouse-msg (event)
  "Show the source line referenced by a compiler/debugger message under the
mouse."
  (interactive "e")
  (mouse-set-point event)
  (if (save-excursion (forward-line 0) (looking-at q-msg-regexp))
      (progn (forward-line 0) (q-current-msg))
    (mouse-yank event)))

;; visit main script and the eval buffer

(defun q-find-script ()
  "Visit the script currently running in the Q eval buffer."
  (interactive)
  (if (and q-last-dir q-last-script)
      (if (not (string-equal (concat q-last-dir q-last-script)
			     (buffer-file-name)))
	  (find-file (concat q-last-dir q-last-script)))
    (error "No script is running")))

(defun q-goto-input-line ()
  "Move to the prompt in the Q eval buffer."
  (interactive)
  (if (get-buffer "*q-eval*")
      (progn (pop-to-buffer "*q-eval*") (goto-char (point-max)))
    (error "No script is running")))

;; completion

(defun q-complete ()
  "Perform completion on the token preceding point."
  (interactive)
  (if (q-at-command-prompt-p)
      (let* ((end (point))
	     (command
	      (save-excursion
		;; skip over anything but whitespace, quotes and parentheses
		(skip-syntax-backward "w_.\\$'<>")
		(and (looking-at q-prompt-regexp)
		     (goto-char (match-end 0)))
		(buffer-substring-no-properties (point) end))))
	(q-send-list-and-digest
	 (list (concat "completion_matches \"" command "\"\n")))
	;; Sort the list
	(setq q-output-list
	      (sort q-output-list 'string-lessp))
	;; Remove duplicates
	(let* ((x q-output-list)
	       (y (cdr x)))
	  (while y
	    (if (string-equal (car x) (car y))
		(setcdr x (setq y (cdr y)))
	      (setq x y
		    y (cdr y)))))
	;; And let comint handle the rest
	(comint-dynamic-simple-complete command q-output-list))))

;; send commands to the Q interpreter and digest their results

(defun q-output-digest (proc string)
  (setq string (concat q-output-string string))
  (while (string-match "\n" string)
    (setq q-output-list
	  (append q-output-list
		  (list (substring string 0 (match-beginning 0))))
	  string (substring string (match-end 0))))
  (if (string-match q-prompt-regexp string)
      (setq q-receive-in-progress nil))
  (setq q-output-string string))

(defun q-send-list-and-digest (list)
  (let* ((q-eval-buffer (get-buffer "*q-eval*"))
	 (proc (get-buffer-process q-eval-buffer))
	 (filter (process-filter proc))
	 string)
    (set-process-filter proc 'q-output-digest)
    (setq q-output-list nil)
    (unwind-protect
	(while (setq string (car list))
	  (setq q-output-string nil
		q-receive-in-progress t)
	  (comint-send-string proc string)
	  (while q-receive-in-progress
	    (accept-process-output proc))
	  (setq list (cdr list)))
      (set-process-filter proc filter))))

;; online help

;; The following is a slightly modified version of the help command
;; from octave mode; thanks to Kurt Hornik and John Eaton.

(require 'info)

(defvar q-help-lookup-alist nil
  "Alist of Q info files index entries for lookup.")

(defvar q-help-completion-alist nil
  "Alist of Q info files index entries for completion.
The entries are of the form (VAR . VAR), where VAR runs through all
different keys in `q-help-lookup-alist'.")

;;;###autoload
(defun q-help (&optional key)
  "Get help from the Q info files.
Look up KEY in the function, operator and variable indices of the files
specified by `q-help-files'.
If KEY is not a string, prompt for it with completion."
  (interactive
   (list
    (completing-read (format "Q info topic: ")
		     (q-help-get-completion-alist)
		     nil t)))
  (if (zerop (length key))
      (Info-find-node (car q-help-files) "Top")
    (Info-index key)))

(defun q-help-get-lookup-alist ()
  "Build the index lookup alist from all Q info files.
The files specified by `q-help-files' are searched."
  (if q-help-lookup-alist
      ()
    (message "Building help lookup alist...")    
    (let ((files q-help-files) file key node)
      (save-window-excursion
	(while files
	  (setq file (car files))
 	  (Info-goto-node (concat "(" file ")"))
	  (condition-case nil
	      (progn
		(Info-index "")
		(while
		    (progn
		      (while (re-search-forward
			      "^\\* \\([^(:]+\\)[^:]*: *\\([^.]+\\)\\."
			      nil t)
			(setq key (match-string 1)
			      node (concat "(" file ")" (match-string 2)))
			(and (string-match "\\(.*\\>\\) *$" key)
			     (setq key (replace-match "\\1" t nil key)))
			(add-to-list 'q-help-lookup-alist
				     (list key
					   node
					   (concat (concat "(" file ")")
						   Info-current-node)
					   0)))
		      (and (setq node (Info-extract-pointer "next" t))
			   (string-match
			    (concat "\\(Function\\|Operator\\|Variable\\) "
				    "\\<Index\\>")
			    node)))
		  (Info-goto-node node)))
	    (error nil))
	  (setq files (cdr files)))))
    (message "Building help lookup alist...done"))
  q-help-lookup-alist)

(defun q-help-get-completion-alist ()
  "Build the index completion alist from all Q info files.
The files specified by `q-help-files' are searched."
  (if q-help-completion-alist
      ()
    (message "Building help completion alist...")
    (let ((alist (q-help-get-lookup-alist)) entry)
      (while alist
	(setq entry (car alist))
	(add-to-list 'q-help-completion-alist
		     (cons (car entry) (car entry)))
	(setq alist (cdr alist))))
    (message "Building help completion alist...done"))    
  q-help-completion-alist)

;; interface function for gnuclient

;; These functions are used to track the built-in interpreter commands
;; cd/chdir, histfile/histsize, path, prompt, run, edit and help, when the
;; interpreter is invoked with the --gnuclient option (see the operations in
;; qmparse.y).

;; check that we're providing a gnuclient server, and make sure that the eval
;; buffer is current
(defun q-gnuclient-p ()
  (and q-gnuclient-active
       (if (get-buffer "*q-eval*")
	   (progn (set-buffer "*q-eval*") t)
	 nil)))

(defun q-cd-cmd (dir)
  (if (q-gnuclient-p)
      (cd (expand-file-name dir))))

(defun q-histfile-cmd (file)
  (if (q-gnuclient-p)
      (progn 
	(comint-write-input-ring)
	(setq q-histfile file
	      comint-input-ring-file-name q-histfile)
	(comint-read-input-ring t))))

(defun q-dec-cmd ()
  (if (q-gnuclient-p)
      (setq q-int-format "dec")))

(defun q-hex-cmd ()
  (if (q-gnuclient-p)
      (setq q-int-format "hex")))

(defun q-oct-cmd ()
  (if (q-gnuclient-p)
      (setq q-int-format "oct")))

(defun q-std-cmd (x)
  (if (q-gnuclient-p)
      (progn
	(setq q-float-format "std")
	(setq q-float-prec x))))

(defun q-sci-cmd (x)
  (if (q-gnuclient-p)
      (progn
	(setq q-float-format "sci")
	(setq q-float-prec x))))

(defun q-fix-cmd (x)
  (if (q-gnuclient-p)
      (progn
	(setq q-float-format "fix")
	(setq q-float-prec x))))

(defun q-cstacksize-cmd (x)
  (if (q-gnuclient-p)
      (setq q-cstacksize x)))

(defun q-stacksize-cmd (x)
  (if (q-gnuclient-p)
      (setq q-stacksize x)))

(defun q-memsize-cmd (x)
  (if (q-gnuclient-p)
      (setq q-memsize x)))

(defun q-debug-cmd (x)
  (if (q-gnuclient-p)
      (setq q-debug x)))

(defun q-debug-options-cmd (x)
  (if (q-gnuclient-p)
      (setq q-debug-options x)))

(defun q-break-cmd (x)
  (if (q-gnuclient-p)
      (setq q-break x)))

(defun q-echo-cmd (x)
  (if (q-gnuclient-p)
      (setq q-echo x)))

;; looks like writing and rereading is the only "official" way to resize the
;; input ring
(defun q-histsize-cmd (size)
  (if (q-gnuclient-p)
      (progn
	(comint-write-input-ring)
	(setq q-histsize size
	      comint-input-ring-size q-histsize)
	(comint-read-input-ring t))))

(defun q-path-cmd (path)
  (if (q-gnuclient-p)
      (setq q-last-path (split-string path q-path-separator))))

(defun q-prompt-cmd (prompt) nil)

;;(defun q-prompt-cmd (prompt)
;;  (if (q-gnuclient-p)
;;      (progn
;;	(setcar q-prompts prompt)
;;	(setq q-prompt-regexp (q-make-prompt-regexp))
;;	(setq q-eval-font-lock-keywords
;;	      (cons (list q-prompt-regexp 0 'font-lock-type-face t)
;;		    (cdr q-eval-font-lock-keywords)))
;;	;; make sure buffer is refontified (a simple font-lock-fontify-buffer
;;	;; does not work here since the keyword patterns have changed)
;;	(if (and (boundp 'font-lock-fontified) font-lock-fontified)
;;	    (progn (font-lock-mode) (font-lock-mode)))
;;	)))

(defun q-run-cmd (file)
  (if (q-gnuclient-p)
      (setq q-last-dir (file-name-directory (expand-file-name file))
	    q-last-script (file-name-nondirectory file))
    (if q-last-path
	(setq q-path q-last-path))))

(defun q-do-cmd ()
  "Advance to the last message in a contiguous sequence of compiler/debugger
messages at or below the current line, and show the referenced source line
in another window."
  (interactive)
  (let ((actwindow (selected-window)))
    (let ((beg (progn
	       (forward-line 0)
	       (point)))
	(end (progn
	       (forward-paragraph)
	       (point))))

      (copy-region-as-kill beg end))
    (if (get-buffer "*q-eval*")
	(pop-to-buffer "*q-eval*")
      (error "No script is running"))
    (end-of-buffer)
    (yank)
    (comint-send-input)
    (select-window actwindow)
    (forward-line 1)))

(defun q-do-buf ()
  "Advance to the last message in a contiguous sequence of compiler/debugger
messages at or below the current line, and show the referenced source line
in another window."
  (interactive)
  (let ((actwindow (selected-window)))
    (let ((beg (progn
	       (goto-char (point-min))
	       (point)))
	(end (progn
	       (goto-char (point-max))
	       (point))))

      (copy-region-as-kill beg end))
    (if (get-buffer "*q-eval*")
	(pop-to-buffer "*q-eval*")
      (error "No script is running"))
    (end-of-buffer)
    (yank)
    (comint-send-input)
    (select-window actwindow)
    (forward-line 1)))

(defun q-edit-cmd (file)
  (if (q-gnuclient-p)
      (find-file-other-window file)))
  
(defun q-help-cmd (&optional key)
  (if (q-gnuclient-p)
      ;; hmm, looks like we always have to invoke (q-help) before we can
      ;; access the index with (q-help key) (strange >.;)
      (progn
	(q-help)
	(if key (q-help key)))))

;; perform cleanup when the interpreter process is killed

(defun q-eval-sentinel (proc msg)
  (if (null (buffer-name (process-buffer proc)))
      ;; buffer has been killed
      (set-process-buffer proc nil)
    (set-buffer (process-buffer proc))
    (comint-write-input-ring)
    (setq q-last-dir nil
	  q-last-script nil
	  q-other-script nil)
    (goto-char (point-max))
    (insert "\n*** Process Q-Eval finished ***\n")))

;; make sure that the history is written when exiting emacs
(add-hook 'kill-emacs-hook
	  (lambda ()
	    (let ((q-eval-buffer (get-buffer "*q-eval*")))
	      (cond
	       (q-eval-buffer
		(set-buffer q-eval-buffer)
		(comint-write-input-ring))))))

;; autoindent and fill support (preliminary)

;; I quickly hacked this together from an improved Prolog mode by Kenichi
;; Handa, and the paragraph filling in the C/C++ mode included in the Xemacs
;; distribution. So don't expect miracles. It seems to work pretty well,
;; though.

(defun q-electric-delim (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if (and (not arg)
	   (save-excursion
	     (skip-chars-backward " \t")
	     (bolp)))
      (progn
	(insert last-command-char)
	(q-indent-line)
	(delete-char -1)))
  (self-insert-command (prefix-numeric-value arg)))

;; find the position of the previous rule's rhs (`=' delimiter)
;; XXXFIXME: this doesn't work properly with lhs 'where' clauses
(defun q-prev-rhs ()
  (if (not (q-backward-to-delim "="))
      nil
    ;; back up to beginning of rule, then find 1st `=' at toplevel
    (beginning-of-rule)
    (if (not (q-forward-to-delim "="))
	nil ; this shouldn't happen
      (backward-char)
      (point))))

(defvar q-qual-keywords "\\<\\(else\\|if\\|otherwise\\|where\\)\\>")

(defun q-at-qual ()
  (and (looking-at q-qual-keywords)
       (or (not (looking-at "else"))
	   (save-excursion
	     (backward-word 1)
	     (not (looking-at "or"))))))

;; find the position of the previous qualifier or conditional keyword (if,
;; else, otherwise, etc.)
(defun q-prev-qual ()
  (if (not (q-backward-to-regexp q-qual-keywords)) nil
    (let ((success t) (done nil))
      (while (and success (not done))
	(setq done (q-at-qual))
	(setq success (or done (q-backward-to-regexp q-qual-keywords))))
      (if (not done) nil
	(let* ((p0 (point))
	       (p (progn (beginning-of-line)
			 (if (q-forward-to-regexp q-qual-keywords)
			     (backward-word 1))
			 (if (q-at-qual) (point) p0))))
	  (goto-char p))))))

(defun q-move-to-indent-column () 
 "At end of line, move forward to the current `=' indentation column, as
given by the most recent rule or the \\[q-default-rhs-indent] variable."
  (interactive)
  (if (save-excursion
	(skip-chars-forward " \t")
	(eolp))
      (let ((col (current-column))
	    (icol (save-excursion
		    (if (q-prev-rhs)
			(current-column)
		      q-default-rhs-indent))))
	(if (> icol col)
	    (move-to-column icol t)))))

(defun q-comment-indent ()
  "Compute Q comment indentation."
  (cond ((looking-at "^#!") 0)
	((looking-at "/[/*]")
	 (let ((indent (q-calculate-indent)))
	   (if (consp indent) (car indent) indent)))
	(t
	 (save-excursion
	       (skip-chars-backward " \t")
	       (max (current-column)
;;	       (max (1+ (current-column)) ;Insert one space at least
		    comment-column)))
	))

;; FIXME: This stuff (beginning-of-rule, end-of-rule) is broken. It gets
;; caught in block comments easily -- unfortunately, Q definitions may look a
;; lot like plain comment text ;-). There really seems to be no good way of
;; doing this, because these routines need to be fast, so we can't just parse
;; the whole file any time they are invoked.

;; As implemented, beginning-of-rule looks for a line starting with a
;; word/symbol constituent, open parentheses, string, or optional whitespace
;; followed by a `=' character, whereas end-of-rule searches for a semicolon
;; at line end (with maybe some single-line comments and whitespace in
;; between). So reasonable formatting styles should all be parsed correctly.

(defun beginning-of-rule ()
  "Move backward to beginning of current or previous rule."
  (interactive)
  (if (or
       (if (and (> (current-column) 0)
		(save-excursion
		  (beginning-of-line)
		  (looking-at "[ \t]*=")))
	   (progn (beginning-of-line) t)
	 nil)
       (re-search-backward "^\\w\\|^\\s_\\|^\\s(\\|^\\s\"\\|^[ \t]*="
			   (point-min) 'mv))
      (let ((p (point)))
	(q-backward-to-noncomment (point-min))
	(if (and (not (bobp))
		 (/= (preceding-char) ?\;)
		 (/= (preceding-char) ?\:))
	    (beginning-of-rule)
	  (goto-char p)))))

(defun end-of-rule ()
  "Move forward to end of current or next rule."
  (interactive)
  (let ((p (point)))
    (while (and (re-search-forward
;;; match ";" + whitespace/comment sequence + "\n"
";\\([ \t]+\\|/\\*+\\([^\n\\*]\\|\\*[^\n/]\\)*\\*+/\\)*\\(//.*\\)?\n"
		 nil 'move)
		(/= (1+ (match-beginning 0))
		   (save-excursion
		     (q-backward-to-noncomment p)
		     (point)))))))

(defun q-indent-line ()
  "Indent current line as Q code.
Return the amount the indentation changed by."
  (interactive)
  (let ((indent (q-calculate-indent nil))
	start-of-block
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (if (listp indent)
	(progn
	  (setq start-of-block (cdr indent))
	  (setq indent (car indent)))
      (setq start-of-block 0))
    (beginning-of-line)
    (setq beg (point))
    (setq indent
	  (cond ((eq indent nil) (current-indentation))
		((eq indent t) (q-calculate-indent-within-comment))
		(t
		 (skip-chars-forward " \t")
		 (cond ((looking-at "^#!") 0)
		       ((= (following-char) ?\)) start-of-block)
		       (t indent)))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defvar q-decl-keywords
  (concat "\\<\\("
	  "const\\|def\\|extern\\|import\\|include\\|"
	  "p\\(rivate\\|ublic\\)\\|special\\|type\\|"
	  "undef\\|var"
	  "\\)\\>"))

(defun q-indent-col (col pos)
  (if pos
      (let ((col2 (save-excursion (goto-char pos) (current-column))))
	(cons col col2))
    col)
)

;; TODO: proper indentation of parenthesized if-then-else constructs
(defun q-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as Q code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment,
\(indent . start-of-block\) if line is within a paren block."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp
	  (at-decl nil)
	  (lhs-extra-indent 0)
	  (rhs-extra-indent 
	   (save-excursion
	     (skip-chars-forward " \t")
	     (if (q-at-qual) q-extra-qual-indent 0)))
	  (following-character
	   (save-excursion (skip-chars-forward " \t") (following-char))))
      (if parse-start
	  (goto-char parse-start)
	(let ((p (point)))
	  (q-backward-to-noncomment (point-min))
	  (if (and (not (bobp))
		   (/= (preceding-char) ?\;))
	      (beginning-of-rule)
	    (goto-char p))))
      ;; extra indent for continuation lines in declarations
      (if (and (< (point) indent-point)
	       (looking-at q-decl-keywords))
	  (setq at-decl t
		lhs-extra-indent q-extra-decl-indent))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      ;; the above sometimes craps out even if we're inside a balanced pair
      ;; of parens, but the following should work in any case
      (if (null containing-sexp)
	  (setq containing-sexp
		(condition-case nil
		    (scan-lists indent-point -1 1)
		  (error nil))))
      (if (or (nth 3 state) (nth 4 state))
	  ;; return nil or t if should not change this line
	  (nth 4 state)
	;; Check to see whether we are inside a sexp, on the lhs, rhs,
	;; qualifier, or at the = of a rule.
	(goto-char indent-point)
	(q-backward-to-noncomment (or parse-start (point-min)))
	(let (p0 p1 p2 p3 col1 col2 col3)
	  (setq p0 containing-sexp
		p1 (save-excursion
		     (q-backward-to-delim ";")
		     (point))
		p2 (save-excursion
		     (if (q-prev-rhs) (point) 0))
		p3 (save-excursion
		     (if (q-prev-qual) (point) 0)))
	  (if (> p2 0)
	      (setq col1 (save-excursion
			   (goto-char p2)
			   (current-column))
		    col2 (save-excursion
			   (goto-char p2)
			   (forward-char)
			   (skip-chars-forward " \t")
			   (current-column))
		    col3 (save-excursion
			   (goto-char p3)
			   (current-column)))
	    (setq col1 q-default-rhs-indent
		  col2 q-default-rhs-indent
		  col3 q-default-rhs-indent))
	  (cond
	   ((and (not (null p0)) (>= p0 (max p1 p2 p3)))
	    ;; inside a sexp (pair of balanced parens): indent at the column
	    ;; to the right of the paren
	    (let ((col (save-excursion (goto-char p0) (current-column))))
	      (cons (1+ col) col)))
	   ((or (= following-character ?=)
		(= following-character ?\;)
		(and at-decl (= following-character ?|)))
	    ;; followup eqns (initial =), initial semi, and initial |
	    ;; in declarations are indented at preceding =
	    (q-indent-col col1 p0))
	   ((or at-decl (> p1 p2))
	    ;; lhs: indent at lhs-extra-indent
	    (q-indent-col lhs-extra-indent p0))
	   ((> p3 p2)
	    ;; qualifier/conditional: indent at column of previous qualifier
	    ;; keyword plus q-extra-qual-indent if no keyword at bol
	    (q-indent-col
	     (+ col3 (if (= 0 rhs-extra-indent) q-extra-qual-indent 0)) p0))
	   (t
	    ;; rhs: indent at first token behind preceding =
	    ;; add rhs-extra-indent for initial qualifier keyword
	    (q-indent-col (+ col2 rhs-extra-indent) p0))))))))

(defun q-calculate-indent-within-comment ()
  "Return the indentation amount for line, assuming that
the current line is to be regarded as part of a block comment."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq star-start (= (following-char) ?\*))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (re-search-forward "/\\*[ \t]*" end t)
	   star-start
	   (goto-char (1+ (match-beginning 0))))
      (current-column))))

(defun q-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (if (and (>= (point) (+ 2 lim))
	       (= (preceding-char) ?/) (= (char-after (- (point) 2)) ?*))
	  (search-backward "/*" lim 'mv)
	(let ((p (max lim (save-excursion (beginning-of-line) (point)))))
	  (if (nth 4 (parse-partial-sexp p (point)))
	      (re-search-backward "^#!\\|//" p 'mv)
	    (goto-char opoint)
	    (setq stop t)))))))

(defun q-forward-to-noncomment (lim)
  (forward-char 1)
  (while (progn
	   (skip-chars-forward " \t\n" lim)
	   (looking-at "^#!\\|//\\|/\\*"))
    ;; Skip over comments and labels following openparen.
    (if (looking-at "^#!\\|//")
	(forward-line 1)
      (forward-char 2)
      (search-forward "*/" lim 'mv))))

;; some added stuff for finding = and ; delimiters in rules

(defun q-at-toplevel-p ()
  (let (p state)
    (save-excursion
      (setq p (save-excursion
		(beginning-of-rule)
		(point)))
      (setq state (parse-partial-sexp p (point)))
      (not (or (nth 1 state)
	       (nth 3 state)
	       (nth 4 state))))))

(defun q-backward-to-delim (delim-str)
  (let ((success nil))
    (while (and (search-backward delim-str nil 'mv)
		(progn
		  (setq success (q-at-toplevel-p))
		  (not success))
		(not (bobp))))
    (if success (point) nil)))

(defun q-forward-to-delim (delim-str)
  (let ((success nil))
    (while (and (search-forward delim-str nil 'mv)
		(progn
		  (setq success (q-at-toplevel-p))
		  (not success))
		(not (eobp))))
    (if success (point) nil)))

(defun q-backward-to-regexp (delim-str)
  (let ((success nil))
    (while (and (re-search-backward delim-str nil 'mv)
		(progn
		  (setq success (q-at-toplevel-p))
		  (not success))
		(not (bobp))))
    (if success (point) nil)))

(defun q-forward-to-regexp (delim-str)
  (let ((success nil))
    (while (and (re-search-forward delim-str nil 'mv)
		(progn
		  (setq success (q-at-toplevel-p))
		  (not success))
		(not (eobp))))
    (if success (point) nil)))

(defun q-indent-current-rule ()
  "Indent all lines in the current rule."
  (interactive)
  (let (p)
    (save-excursion
      (end-of-rule)
      (setq p (point-marker))
      (beginning-of-rule)
      (while (< (point) p)
	(q-indent-line)
	(forward-line 1)))))

;; this stuff is from (XEmacs) cc-mode

(defun q-indent-region (start end)
  ;; Indent every line whose first char is between START and END inclusive.
  (let (p)
    (save-excursion
      (goto-char start)
      (setq p (copy-marker end))
      (while (and (bolp)
		  (not (eobp))
		  (< (point) p))
	(q-indent-line)
	(forward-line 1)))))

(defun q-indent-line-or-region ()
  "When the region is active, indent it.  Otherwise indent the current line."
  (interactive)
  (if (q-region-is-active-p)
      (q-indent-region (region-beginning) (region-end))
    (q-indent-line)))

;; paragraph fill from (XEmacs) cc-mode, boiled down for Q mode

(defmacro q-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))

(defmacro q-forward-sexp (&optional arg)
  ;; like forward-sexp except
  ;;   1. this is much stripped down from the XEmacs version
  ;;   2. this cannot be used as a command, so we're insulated from
  ;;      XEmacs' losing efforts to make forward-sexp more user
  ;;      friendly
  ;;   3. Preserves the semantics most of CC Mode is based on
  (or arg (setq arg 1))
  `(goto-char (or (scan-sexps (point) ,arg)
		  ,(if (numberp arg)
		       (if (> arg 0) `(point-max) `(point-min))
		     `(if (> ,arg 0) (point-max) (point-min))))))

(defmacro q-backward-sexp (&optional arg)
  ;; See q-forward-sexp and reverse directions
  (or arg (setq arg 1))
  `(q-forward-sexp ,(if (numberp arg) (- arg) `(- ,arg))))

(defsubst q-point (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;; 
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; 
  ;; This function does not modify point or mark.
  (let ((here (point)))
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     (t (error "unknown buffer position requested: %s" position))
     )
    (prog1
	(point)
      (goto-char here))))

(defun q-literal-limits (&optional lim near)
  ;; Returns a cons of the beginning and end positions of the comment
  ;; or string surrounding point (including both delimiters), or nil
  ;; if point isn't in one.  If LIM is non-nil, it's used as the
  ;; "safe" position to start parsing from.  If NEAR is non-nil, then
  ;; the limits of any literal next to point is returned.  "Next to"
  ;; means there's only [ \t] between point and the literal.  The
  ;; search for such a literal is done first in forward direction.
  ;;
  ;; This is the Emacs 19 version.
  (save-excursion
    (let* ((pos (point))
;;; FIXME: need a reasonable replacement for `beginning-of-defun' (bod) here.
;;;	   (lim (or lim (q-point 'bod)))
	   (lim (or lim (point-min)))
	   (state (parse-partial-sexp lim (point))))
      (cond ((nth 3 state)
	     ;; String.  Search backward for the start.
	     (while (nth 3 state)
	       (search-backward (make-string 1 (nth 3 state)))
	       (setq state (parse-partial-sexp lim (point))))
	     (cons (point) (or (q-safe (q-forward-sexp 1) (point))
			       (point-max))))
	    ((nth 7 state)
	     ;; Line comment.  Search from bol for the comment starter.
	     (beginning-of-line)
	     (setq state (parse-partial-sexp lim (point))
		   lim (point))
	     (while (not (nth 7 state))
	       (search-forward "//")	; Should never fail.
	       (setq state (parse-partial-sexp
			    lim (point) nil nil state)
		     lim (point)))
	     (backward-char 2)
	     (cons (point) (progn (forward-comment 1) (point))))
	    ((nth 4 state)
	     ;; Block comment.  Search backward for the comment starter.
	     (while (nth 4 state)
	       (search-backward "/*")	; Should never fail.
	       (setq state (parse-partial-sexp lim (point))))
	     (cons (point) (progn (forward-comment 1) (point))))
	    ((q-safe (nth 4 (parse-partial-sexp ; Can't use prev state due
			     lim (1+ (point))))) ; to bug in Emacs 19.34.
	     ;; We're standing in a comment starter.
	     (backward-char 2)
	     (cons (point) (progn (forward-comment 1) (point))))
	    (near
	     (goto-char pos)
	     ;; Search forward for a literal.
	     (skip-chars-forward " \t")
	     (cond
	      ((eq (char-syntax (or (char-after) ?\ )) ?\") ; String.
	       (cons (point) (or (q-safe (q-forward-sexp 1) (point))
				 (point-max))))
	      ((looking-at q-comment-start-regexp) ; Line or block comment.
	       (cons (point) (progn (forward-comment 1) (point))))
	      (t
	       ;; Search backward.
	       (skip-chars-backward " \t")
	       (let ((end (point)) beg)
		 (cond
		  ((eq (char-syntax (or (char-before) ?\ )) ?\") ; String.
		   (setq beg (q-safe (q-backward-sexp 1) (point))))
		  ((and (q-safe (forward-char -2) t)
			(looking-at "*/"))
		   ;; Block comment.  Due to the nature of line
		   ;; comments, they will always be covered by the
		   ;; normal case above.
		   (goto-char end)
		   (forward-comment -1)
		   ;; If LIM is bogus, beg will be bogus.
		   (setq beg (point))))
		 (if beg (cons beg end))))))
	    ))))

(defconst q-comment-start-regexp "\\(/[/*]\\|^#!\\)")

;; FIXME: I'm wondering why this code messes up the fontification of comment
;; paragraphs since the same code apparently works in C/C++ mode, and the
;; comment syntax is also the same. :( This only happens with XEmacs
;; (21.1p10), no problems with GNU Emacs. Maybe the XEmacs font-lock stuff is
;; broken, or has some special built-in support for the C modes? Anyway, if
;; anyone knows how to fix this please let me know. -AG

(defun q-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handles Q (i.e., C/C++) style
comments. If any of the current line is a comment or within a comment,
fill the comment or the paragraph of it that point is in,
preserving the comment indentation or line-starting decorations.

If point is inside multiline string literal, fill it.  This currently
does not respect escaped newlines, except for the special case when it
is the very first thing in the string.  The intended use for this rule
is in situations like the following:

description = \"\\
A very long description of something that you want to fill to make
nicely formatted output.\"\;

If point is in any other situation, i.e. in normal code, do nothing.

Optional prefix ARG means justify paragraph as well."
  (interactive "*P")
  (let* ((point-save (point-marker))
	 limits
	 comment-start-place
	 (first-line
	  ;; Check for obvious entry to comment.
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t")
	    (and (looking-at comment-start-skip)
		 (setq comment-start-place (point)))))
	 (re1 "\\|\\([ \t]*/\\*[ \t]*\\|[ \t]*\\*/[ \t]*\\|[ \t/*]*\\)"))
    (if (save-excursion
	  (beginning-of-line)
	  (looking-at "#!\\|.*//"))
	(let ((fill-prefix fill-prefix)
	       ;; Lines containing just a comment start or just an end
	       ;; should not be filled into paragraphs they are next
	       ;; to.
	      (paragraph-start (concat paragraph-start re1 "$"))
	      (paragraph-separate (concat paragraph-separate re1 "$")))
	  (save-excursion
	    (beginning-of-line)
	    ;; Move up to first line of this comment.
	    (while (and (not (bobp))
 			(looking-at "[ \t]*//[ \t]*[^ \t\n]"))
	      (forward-line -1))
 	    (if (not (looking-at ".*//[ \t]*[^ \t\n]"))
		(forward-line 1))
	    ;; Find the comment start in this line.
	    (re-search-forward "[ \t]*//[ \t]*")
	    ;; Set the fill-prefix to be what all lines except the first
	    ;; should start with.  But do not alter a user set fill-prefix.
	    (if (null fill-prefix)
		(setq fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))
	    (save-restriction
	      ;; Narrow down to just the lines of this comment.
	      (narrow-to-region (q-point 'bol)
				(save-excursion
				  (forward-line 1)
				  (while
				      (looking-at (regexp-quote fill-prefix))
				    (forward-line 1))
				  (point)))
	      (or (q-safe
		   ;; fill-paragraph sometimes fails to detect when we
		   ;; are between paragraphs.
		   (beginning-of-line)
		   (search-forward fill-prefix (q-point 'eol))
		   (looking-at paragraph-separate))
		  ;; Avoids recursion
		  (let (fill-paragraph-function)
		    (fill-paragraph arg))))))
      ;; else C style comments
      (if (or first-line
	      ;; t if we enter a comment between start of function and
	      ;; this line.
	      (save-excursion
		(setq limits (q-literal-limits))
		(and (consp limits)
		     (save-excursion
		       (goto-char (car limits))
		       (looking-at q-comment-start-regexp))))
	      ;; t if this line contains a comment starter.
	      (setq first-line
		    (save-excursion
		      (beginning-of-line)
		      (prog1
			  (re-search-forward comment-start-skip
					     (save-excursion (end-of-line)
							     (point))
					     t)
			(setq comment-start-place (point)))))
	      ;; t if we're in the whitespace after a comment ender
	      ;; which ends its line.
	      (and (not limits)
		   (when (and (looking-at "[ \t]*$")
			      (save-excursion
				(beginning-of-line)
				(looking-at ".*\\*/[ \t]*$")))
		     (save-excursion
		       (forward-comment -1)
		       (setq comment-start-place (point)))
		     t)))
	  ;; Inside a comment: fill one comment paragraph.
	  (let ((fill-prefix
		 (or
		  ;; Keep user set fill prefix if any.
		  fill-prefix
		  ;; The prefix for each line of this paragraph
		  ;; is the appropriate part of the start of this line,
		  ;; up to the column at which text should be indented.
		  (save-excursion
		    (beginning-of-line)
		    (if (looking-at ".*/\\*.*\\*/")
			(progn (re-search-forward comment-start-skip)
			       (make-string (current-column) ?\ ))
		      (if first-line
			  (forward-line 1)
			(if (and (looking-at "[ \t]*\\*/")
				 (not (save-excursion
					(forward-line -1)
					(looking-at ".*/\\*"))))
			    (forward-line -1)))

		      (let ((line-width (progn (end-of-line)
					       (current-column))))
			(beginning-of-line)
			(prog1
			    (buffer-substring
			     (point)

			     ;; How shall we decide where the end of the
			     ;; fill-prefix is?
			     (progn
			       (skip-chars-forward " \t*" (q-point 'eol))
			       ;; kludge alert, watch out for */, in
			       ;; which case fill-prefix should *not*
			       ;; be "*"!
			       (if (and (eq (char-after) ?/)
					(eq (char-before) ?*))
				   (forward-char -1))
			       (point)))

			  ;; If the comment is only one line followed
			  ;; by a blank line, calling move-to-column
			  ;; above may have added some spaces and tabs
			  ;; to the end of the line; the fill-paragraph
			  ;; function will then delete it and the
			  ;; newline following it, so we'll lose a
			  ;; blank line when we shouldn't.  So delete
			  ;; anything move-to-column added to the end
			  ;; of the line.  We record the line width
			  ;; instead of the position of the old line
			  ;; end because move-to-column might break a
			  ;; tab into spaces, and the new characters
			  ;; introduced there shouldn't be deleted.

			  ;; If you can see a better way to do this,
			  ;; please make the change.  This seems very
			  ;; messy to me.
			  (delete-region (progn (move-to-column line-width)
						(point))
					 (progn (end-of-line) (point)))))))))

		;; Lines containing just a comment start or just an end
		;; should not be filled into paragraphs they are next
		;; to.
		(paragraph-start (concat paragraph-start re1 "$"))
		(paragraph-separate (concat paragraph-separate re1 "$"))
		(chars-to-delete 0)
		)
	    (save-restriction
	      ;; Don't fill the comment together with the code
	      ;; following it.  So temporarily exclude everything
	      ;; before the comment start, and everything after the
	      ;; line where the comment ends.  If comment-start-place
	      ;; is non-nil, the comment starter is there.  Otherwise,
	      ;; point is inside the comment.
	      (narrow-to-region (save-excursion
				  (if comment-start-place
				      (goto-char comment-start-place)
				    (search-backward "/*"))
				  (if (and (not q-hanging-comment-starter-p)
					   (looking-at
					    (concat q-comment-start-regexp
						    "[ \t]*$")))
				      (forward-line 1))
				  ;; Protect text before the comment
				  ;; start by excluding it.  Add
				  ;; spaces to bring back proper
				  ;; indentation of that point.
				  (let ((column (current-column)))
				    (prog1 (point)
				      (setq chars-to-delete column)
				      (insert-char ?\  column))))
				(save-excursion
				  (if comment-start-place
				      (goto-char (+ comment-start-place 2)))
				  (search-forward "*/" nil 'move)
				  (if (and (not q-hanging-comment-ender-p)
					   (save-excursion
					     (beginning-of-line)
					     (looking-at "[ \t]*\\*/")))
				      (beginning-of-line)
				    (forward-line 1))
				  (point)))
	      (or (q-safe
		   ;; fill-paragraph sometimes fails to detect when we
		   ;; are between paragraphs.
		   (beginning-of-line)
		   (search-forward fill-prefix (q-point 'eol))
		   (looking-at paragraph-separate))
		  ;; Avoids recursion
		  (let (fill-paragraph-function)
		    (fill-paragraph arg)))
	      (save-excursion
		;; Delete the chars we inserted to avoid clobbering
		;; the stuff before the comment start.
		(goto-char (point-min))
		(if (> chars-to-delete 0)
		    (delete-region (point) (+ (point) chars-to-delete)))
		;; Find the comment ender (should be on last line of
		;; buffer, given the narrowing) and don't leave it on
		;; its own line, unless that's the style that's desired.
		(goto-char (point-max))
		(forward-line -1)
		(search-forward "*/" nil 'move)
		(beginning-of-line)
		(if (and q-hanging-comment-ender-p
			 (looking-at "[ \t]*\\*/"))
		    ;(delete-indentation)))))
		    (let ((fill-column (+ fill-column 9999)))
		      (forward-line -1)
		      (fill-region-as-paragraph (point) (point-max))
		      ;; If fill-prefix ended with a `*', it may be
		      ;; taken away from the comment ender.  We got to
		      ;; check this and put it back if that is the
		      ;; case.
		      (goto-char (- (point-max) 2))
		      (if (not (= (char-before) ?*))
			  (insert ?*))
		      )))))
	;; Else maybe a string.  Fill it if it's a multiline string.
	;; FIXME: This currently doesn't handle escaped newlines.
	;; Doing that correctly is a bit tricky.
	(if (and limits
		 (eq (char-syntax (char-after (car limits))) ?\")
		 (save-excursion
		   (goto-char (car limits))
		   (end-of-line)
		   (< (point) (cdr limits))))
	    (let (fill-paragraph-function)
	      (save-restriction
		(narrow-to-region (save-excursion
				    (goto-char (1+ (car limits)))
				    (if (looking-at "\\\\$")
					;; Some DWIM: Leave the start
					;; line if it's nothing but an
					;; escaped newline.
					(1+ (match-end 0))
				      (point)))
				  (save-excursion
				    (goto-char (1- (cdr limits)))
				    ;; Inserting a newline and
				    ;; removing it again after
				    ;; fill-paragraph makes it more
				    ;; predictable.
				    (insert ?\n)
				    (point)))
		;; Do not compensate for the narrowed column.  This
		;; way the literal will always be filled at the same
		;; column internally.
		(fill-paragraph arg)
		(goto-char (1- (point-max)))
		(delete-char 1)))
	  )))
    (goto-char (marker-position point-save))
    (set-marker point-save nil)
    ;; Always return t.  This has the effect that if filling isn't
    ;; done above, it isn't done at all, and it's therefore
    ;; effectively disabled in normal code.
    t))

(provide 'q-mode)
