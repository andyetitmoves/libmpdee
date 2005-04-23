;;; LIBMPDEE.EL --- Client end library for mpd, a music playing daemon

;; Copyright (C) 2004, 2005 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	10 May 2004
;; Version: 	1.0
;; Keywords:	music, mpd

;; This file is *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to andyetitmoves@gmail.com)
;; or from the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is a client end library to the wonderful music playing daemon by
;; name mpd. Hence, an obvious prerequisite for this package is mpd itself,
;; which you can get at http://www.musicpd.org. For those who haven't heard of
;; mpd, all I can say is that it is definitely worth a try, and is surely
;; different from the rest. This package is aimed at developers, and though
;; there are some interactive functions, much of the power of the library lies
;; away from the user.

;; This package started off to implement libmpdclient.c, which came with mpd, in
;; elisp. But as it stands of now, this package is not an exact translation.
;; Notable amongst the deviations are -
;;
;;      - This package contains quite a bit of higher level functionality as
;;      compared with the original. An action or a query needs only a single
;;      call and the library user can choose to get either the raw output, the
;;      formatted output, or hook on a callback for each logical token of the
;;      output. However, dig deeper, and you will find the lower level
;;      functionality available as well.
;;      - The error throwing scheme consistent with what is expected of elisp
;;      programs.
;;      - Command list mode is transparent in most cases, as wherever
;;      appropriate, functions taking arguments can accept either a single item
;;      or a list of it for each argument.
;;      - Apart from this, command list functionality is limited to actions
;;      rather than queries, as it is anyway not that easy to parse out the
;;      individual outputs from command-list queries (it is firstly possible
;;      only from 0.11, which allows for a list_OK to be output at the end of
;;      each command in the list) and the design of the package makes it even
;;      tougher.
;;      - Hence, the command_list_ok_begin isn't implemented. In the unlikely
;;      event of you needing this facility, it is still possible to explicitly
;;      use "command_list_(ok)begin\n..\ncommand_list_end" for
;;      `mpd-execute-command' and get the output tokens for queries.
;;      - There is a small interactive interface as well, but this is
;;      intentionally incomplete. The point is that this exists only to the
;;      extent of adding an interactive part to the functions, without modifying
;;      the functions per se.

;; Most of the functions below require a connection object as an argument, which
;; you can create by a call to `mpd-new-connection'. Refer to the `*-data'
;; variables for descriptions of return values of some of the functions.

;; As this package caters to developers, it should be helpful to browse the file
;; in order for atleast the functions and the documentation. The file is well
;; structured and documented, so go for it. The impatient could do a selective
;; display to 3 (C-u 3 C-x $) before proceeding.

;;; Installation:

;; Put this file somewhere on your load-path. Then, you could use
;; (require 'libmpdee) whenever the services of this package are needed.
;; To use this package for interactive purposes, you will have to, in addition,
;; call `mpd-start-interactive-connection', either interactively before you
;; start using the package, or preferably somewhere in your startup scripts.
;; Typically the call would be something like -
;;
;;      (mpd-start-interactive-connection "localhost" 6600 10.0)
;;
;; Please read the function documentation for details.

;;; Code:

(defvar libmpdee-version "1.0"
  "libmpdee version information.")

;;;; User serviceable variable(s).

(require 'custom)

(defun widget-mpd-format-handler (widget esc)
  (cond
   ((eq esc ?l)
    (widget-create-child-and-convert widget 'documentation-string "\
This variable specifies a MPD connection.
The value is a list of the mpd host name, port number, and the timeout for
server replies. See `mpd-conn-new' for more details.")
    (insert "To know more about libmpdee, read ")
    (widget-create 'emacs-commentary-link :tag "this" "libmpdee"))
   (t (funcall (widget-get (widget-convert 'lazy) :format-handler)
	       widget esc))))

(defun mpd-connection-tidy (val)
  (or val (setq val '(nil nil nil)))
  (let ((val val))
    (and (listp val)
	 (or (car val)
	     (setcar val (or (getenv "MPD_HOST") "127.0.0.1")))
	 (setq val (cdr val))
	 (or (car val) (setcar val 6600))
	 (setq val (cdr val))
	 (or (car val) (setcar val 10.0)))) val)

(define-widget 'mpd-connection 'lazy
  "A widget for a MPD connection."
  :tag "MPD Connection"
  :format "%{%t%}:\n\n%l\n\n%v"
  :format-handler 'widget-mpd-format-handler
  :value-to-internal '(lambda (wid val) (mpd-connection-tidy val))
  :type '(list :format "%v"
	       (string :format "%t: %v\n" :tag "Hostname" :size 15)
	       (integer :format "%t:     %v\n" :tag "Port" :size 5
			:match (lambda (widget value) (> value 0))
			:type-error "Port number must be a natural number")
	       (float :format "%t:  %v\n\n" :tag "Timeout" :size 10
		      :match (lambda (widget value) (> value 0))
		      :type-error "Timeout must be a positive number.")))

(defgroup mpd nil
  "The client end library for MPD, the music playing daemon."
  :group 'external :group 'multimedia
  :link '(emacs-commentary-link "libmpdee"))

(defcustom mpd-db-root (getenv "MPD_DB_ROOT")
  "*MPD database directory root"
  :type 'directory :group 'mpd)

(defcustom mpd-interactive-connection-parameters nil
  "Parameters for the interactive mpd connection.
These determine the connection used by interactive functions in `libmpdee'."
  :type 'mpd-connection :group 'mpd)

(defface mpd-separator-face '((((background dark)) (:foreground "lightyellow"))
			      (((background light)) (:foreground "darkgreen")))
  "Face for display of separator lines in interactive mpd queries."
  :group 'mpd)

(defface mpd-header-face '((((background dark)) (:foreground "gold"))
			   (((background light)) (:foreground "brown")))
  "Face for display of header lines in interactive mpd queries."
  :group 'mpd)

(defface mpd-first-field-face '((((background dark)) (:foreground "cyan"))
				(((background light)) (:foreground "orange")))
  "Face for display of the first field in interactive mpd queries.
Most lines in interactive displays are split into two fields."
  :group 'mpd)

(defface mpd-second-field-face
  '((((background dark)) (:foreground "lightgreen"))
    (((background light)) (:foreground "blue")))
  "Face for display of the second field in interactive mpd queries.
Most lines in interactive displays are split into two fields."
  :group 'mpd)

;;;; Constants and internal variables.

(eval-and-compile (defconst mpd-welcome-message " MPD "))
(defmacro mpd-welcome-length () (length mpd-welcome-message))
(defconst mpd-ver-string-length 3)

;;;; Package independent helper functions.

(defmacro assert-type (obj func)
  "Ensure that OBJ tests succeeds for type checking predicate FUNC.
The function emits a \"Wrong type argument\" signal on failure.
Note that the arguments are evalled twice in this process."
  `(or (,func ,obj) (signal 'wrong-type-argument (list (quote ,func) ,obj))))

(defmacro assert-string (obj) `(assert-type ,obj stringp))
(defmacro assert-wholenump (obj) `(assert-type ,obj wholenump))
(defmacro assert-numberp (obj) `(assert-type ,obj numberp))

(defun string-to-number-strict (str &optional allowneg)
  "Convert string STR to a number strictly.
By strict, we mean that nil is returned if there are any unmatched
characters. ALLOWNEG, if non-nil, allows for negative numbers."
  (let ((num (string-to-number str)))
    (and (if allowneg (numberp num) (wholenump num))
	 (string= str (number-to-string num)) num)))
(put 'string-to-number-strict 'side-effect-free t)

(defun get-lines (str)
  "Split STR into newline separated lines.
Differ from `split-string' in that tokens are created
for leading and trailing newlines."
  (let ((packets (split-string str "\n")))
    (when packets
      (if (= (aref str 0) ?\n)
	  (setq packets (cons "" packets)))
      (if (= (aref str (1- (length str))) ?\n)
	  (nconc packets (list ""))
	packets))))

(defun find-key-field (key desc)
  (let ((offset 0) (len (length desc)))
    (while (and (< offset len) (not (string= (aref desc offset) key)))
      (setq offset (1+ offset)))
    (and (not (= offset len)) offset)))

(defsubst vput (vect desc key val)
  "Set element in sequence based on description sequence and key.
Set the first element in VECT, for which the corresponding element in DESC
matches KEY, to VAL, calling REFILL with VECT if the element at that position is
already non-nil.  Return non-nil if the search succeeds."
  (let ((offset (find-key-field key desc)))
    (and offset (aset vect offset val))))

;;; Modified from the pcomplete package.
(defun sort-uniq-list (l lessp eqp)
  "Sort and remove multiples in list L.
LESSP and EQP are predicates for the \"lesser\" and \"equal\" operations."
  (setq l (sort l lessp))
  (let ((m l))
    (while m
      (while (and (cdr m) (funcall eqp (car m) (cadr m)))
	(setcdr m (cddr m)))
      (setq m (cdr m)))) l)

;;; For edebug macro specifications.
(eval-when-compile (require 'edebug))

(defmacro with-temp-widen (&rest args)
  "Evaluate ARGS while temporarily widening the current buffer."
  `(save-restriction
     (widen)
     ,@args))
(put 'with-temp-widen 'lisp-indent-function 0)
(def-edebug-spec with-free-buffer (body))

(defmacro with-free-buffer (&rest args)
  "Evaluate ARGS with temporary widening and saved excursion."
  `(save-excursion
     (with-temp-widen ,@args)))
(put 'with-free-buffer 'lisp-indent-function 0)
(def-edebug-spec with-free-buffer (body))

(defmacro safe-nreverse (list)
  "Reverse LIST if it is a list, leave alone otherwise.
Note that LIST is evaluated thrice."
  `(if (listp ,list) (nreverse ,list) ,list))

(defun mpd-seq-add (seq &optional spec &rest args)
  "Operate the sequence SEQ on SPEC depending on its type.
Add a copy of SEQ to SPEC, if it's a list and call it with SEQ as an argument
followed by other ARGS specified, if it is a function. Return SPEC."
  (if (not (functionp spec))
      (cons (copy-sequence seq) spec)
    (apply spec seq args) spec))

(defun mpd-elt-add (elt &optional spec &rest args)
  "Operate the object ELT on SPEC depending on its type.
Add ELT to SPEC, if it's a list and call it with ELT as an argument
followed by other ARGS specified, if it is a function. Return SPEC."
  (if (not (functionp spec))
      (cons elt spec)
    (apply spec elt args) spec))

(defun mpd-parse-line (str)
  "Parse line STR of form \"KEY: VALUE\" to a cons (KEY . VALUE).
Return (STR . nil) on a parse failure."
  (if (string-match "^\\([^:]*\\): ?\\(.*\\)$" str)
      (cons (match-string 1 str) (match-string 2 str))
    (cons str nil)))

(defun mpd-safe-string (str)
  "Quote and escape string STR for sending to the mpd server."
  (if str
      (let ((start 0))
	(while (string-match "[\\\"]" str start)
	  ;; We add an extra character,
	  ;; so place start a character beyond end of match.
	  (setq start (1+ (match-end 0)))
	  (setq str (replace-match "\\\\\\&" t nil str)))
	(if (string-match " " str) (concat " \"" str "\"") str))))

;;;; Connection object internals, library users... please close ur eyes ;)

;;; A connection to mpd is represented by the vector whose elements are below:
;;; This is just for the hackers amongst you, and for my reference :)
;;; *WARNING* No program using this package should depend on this description.
;;; 0 : vector of mpd-ver-string-length version numbers
;;; 1 : process object for the connection to the server.
;;; 2 : transaction-buffer used by the process filter to handle partial recvs.
;;; 3 : list-mode - refer `mpd-execute-command'
;;; 4 : connection-status: should be t at the beginning of a command.
;;;     stringp -> Result of command, contains the message after OK/ACK
;;;                till the next end of line. No filtering occurs after this
;;;                stage is reached.
;;;     numberp -> Intermediate state, when OK/ACK has been got
;;;                but no and of line found. Then store start pos of the OK/ACK.
;;;                Hope that eol will come in a subsequent packet.
;;;     listp   -> In commandlist mode, the list is what is to be sent to the
;;;                server after the commandlist gets over.
;;; 5 : last-command-result-flag - t if OK, nil if ACK.
;;; 6 : timeout - The timeout used for replies from server.
;;; 7 : host - The name of the host in this connection, used for auto-reconnecting.
;;; 8 : port number - The port number, also for auto-reconnecting.
;;; 9 : noreconn - nil if reconnection should be tried on a broken connection.

;; Don't expose these macros, unless required so.
(eval-when-compile
  (defmacro _mpdgv () `(aref conn 0))
  (defmacro _mpdsv (val) `(aset conn 0 ,val))
  (defmacro _mpdgo () `(aref conn 1))
  (defmacro _mpdso (val) `(aset conn 1 ,val))
  (defmacro _mpdgb () `(aref conn 2))
  (defmacro _mpdsb (val) `(aset conn 2 ,val))
  (defmacro _mpdgl () `(aref conn 3))
  (defmacro _mpdsl (val) `(aset conn 3 ,val))
  (defmacro _mpdgs () `(aref conn 4))
  (defmacro _mpdss (val) `(aset conn 4 ,val))
  (defmacro _mpdgf () `(aref conn 5))
  (defmacro _mpdsf (val) `(aset conn 5 ,val))
  (defmacro _mpdgt () `(aref conn 6))
  (defmacro _mpdst (val) `(aset conn 6 ,val))
  (defmacro _mpdgh () `(aref conn 7))
  (defmacro _mpdsh (val) `(aset conn 7 ,val))
  (defmacro _mpdgp () `(aref conn 8))
  (defmacro _mpdsp (val) `(aset conn 8 ,val))
  (defmacro _mpdgr () `(aref conn 9))
  (defmacro _mpdsr (val) `(aset conn 9 ,val)))

;;;; Sanity check functions.

;;; (defun mpd-log (fmt &rest args)
;;;   (write-region (concat (apply 'format fmt args) "\n") nil "~/mpd.log" t 1))

;;; (write-region "" nil "~/mpd.log" nil 1)

(defun mpd-connp (conn)
  "Return t if CONN is a connection to the mpd server."
  (and (vectorp conn) (= (length conn) 10)))
(put 'mpd-connp 'side-effect-free 'error-free)

(defmacro assert-mpd-conn (conn) `(assert-type ,conn mpd-connp))

(defun mpd-assert-idle (conn)
  "Assert mpd connection CONN to be free to receive a command."
  (assert-mpd-conn conn)
  (or (stringp (_mpdgs))
      (error (if (listp (_mpdgs)) "Command list mode has not ended"
	       "Not done processing current command"))))

(defun mpd-end-conn (conn fmt &rest args)
  "Abort mpd conection CONN and signal error.
The error message is `format'ted using FMT and ARGS."
  (delete-process (_mpdgo))
  (_mpdso nil)
  (signal 'error (list (apply 'format fmt args))))

(defun mpd-end-cmd (conn fmt &rest args)
  "Abort current mpd command for connection CONN and signal error.
The error message is `format'ted using FMT and ARGS."
  (_mpdsf nil)
  (_mpdss (apply 'format fmt args))
  (signal 'error (list (apply 'format fmt args))))

;;;; Internal functions.

(defun mpd-process-filter (conn str)
  "Filter used to receive replies from the mpd server.
CONN represents the connection object for which the filter is invoked.
STR is the packet received from the server to be processed.
This is an internal function, do not use this in your code."
  (cond
   ((eq (_mpdgs) t)
    (let ((start (length (_mpdgb))) status (case-fold-search nil))
      ;; Can be 4 if not for the : as mentioned below
      (setq start (if (< start 5) 0 (- start 5)))
      (_mpdsb (concat (_mpdgb) str))
      ;; The optional : in ACK accomodates the ACK: reply to a failed `add'
      ;; given by MPD servers till version 0.10.*
      (setq status (string-match "^\\(ACK:? \\|OK\\)"
				 (substring (_mpdgb) start)))
      (if status (setq status (+ status start)))
      (or (eq (_mpdgl) t)
	  (let ((packets
		 (if (not (equal status 0))
		     (get-lines (substring (_mpdgb) 0
					   (and status (1- status)))))))
	    (if status
		(progn
		  (_mpdsb (substring (_mpdgb) status))
		  (setq status 0))
	      (if (cdr packets)
		  (let ((last (last packets 2)))
		    (_mpdsb (cadr last))
		    (setcdr last nil))
		(_mpdsb (car packets))
		(setq packets nil)))
	    (cond
	     ((functionp (_mpdgl))
	      (mapcar '(lambda (str)
			 (funcall (_mpdgl) conn (mpd-parse-line str)))
		      packets))
	     ((listp (_mpdgl))
	      (_mpdsl (nconc (mapcar 'mpd-parse-line packets) (_mpdgl))))
	     (t (error "Invalid line mode filter")))))
      (when status
	(_mpdss status)
	(mpd-process-filter conn ""))))
   ((numberp (_mpdgs))
    (_mpdsb (concat (_mpdgb) str))
    (let* ((resp-end (_mpdgs))
	   (bufend (string-match "\n" (_mpdgb) resp-end)))
      (when bufend
	(_mpdss (substring
		 (_mpdgb)
		 ;; The `4` below should be `5` if ACK: is found, see above.
		 ;; This may then leave a space before the error message.
		 (+ resp-end (if (_mpdsf (eq (aref (_mpdgb) resp-end)?O)) 2 4))
		 bufend))
	(_mpdsb (substring (_mpdgb) 0 resp-end))
	(throw 'mpd-output-over nil))))))

(defun mpd-transact (conn &optional mode input)
  "Do an I/O transacton with the mpd server.
Use connection CONN for contacting the server. MODE is as described in
`mpd-execute-command'. INPUT, if non-nil, is sent to the server before waiting
for the output. This is an internal function, do not use this in your code."
  (_mpdsb "")
  (_mpdsl mode)
  (_mpdss t)
  (unwind-protect
      (let ((timeout (_mpdgt)))
	(and input (process-send-string (_mpdgo) input))
	(while (not (stringp (_mpdgs)))
	  (catch 'mpd-output-over
	    (or (accept-process-output
		 (_mpdgo) (and timeout (/ timeout 1000))
		 (and timeout (mod timeout 1000)))
		(error "Timed out getting a response from the mpd server")))))
    (unless (stringp (_mpdgs))
      (_mpdsf nil)
      (_mpdss "")
      (_mpdsb ""))))

;;;; Low level public interface.

(defun mpd-conn-new (host port &optional timeout noreconn)
  "Construct a mpd connection object from given parameters.
No connection is established, use `mpd-connect' for the same. Connections made
using this object are made to host HOST at port number PORT. TIMEOUT is a
floating-point value specifying the number of seconds to wait before giving up
waiting for a reply from the server. Unspecified or zero TIMEOUT correspond to
infinite timeout. If NORECONN is non-nil, no reconnection is attempted if the
connection breaks."
  (or (and (stringp host) (wholenump port) (or (not timeout) (numberp timeout)))
      (error "Invalid parameters passed for making new connection"))
  (and timeout
       (if (= timeout 0)
	   (setq timeout nil)
	 (or (> timeout 0) (error "Invalid (negative) timeout value"))))
  ;; Declaration conn object structure dependent.
  (vector (make-vector mpd-ver-string-length nil) nil
	  "" nil "" nil (and timeout (floor (* timeout 1000)))
	  host port noreconn))

(eval-when-compile
  (or (fboundp 'set-process-query-on-exit-flag)
      (defmacro set-process-query-on-exit-flag (process flag)
	`(process-kill-without-query process flag))))

(defun mpd-connect (conn)
  "Connect to the mpd server using the connection object CONN.
A connection object is constructed using `mpd-conn-new'."
  (let (proc rt welc)
    (setq proc (or (open-network-stream "mpd" nil (_mpdgh) (_mpdgp))
		   (error "Unable to open connection with mpd")))
    (and (_mpdgo) (delete-process (_mpdgo)))
    (_mpdso proc)
    (set-process-query-on-exit-flag proc nil)
    (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
    (set-process-filter proc
			`(lambda (proc str)
			   (mpd-process-filter ,conn str)))
    (unwind-protect
	(mpd-transact conn t)
      (or (_mpdgf)
	  (mpd-end-conn conn "Handshake failed, server returned: %s" (_mpdgs))))
    (setq welc (_mpdgs))
    (or (string= (substring welc 0 (mpd-welcome-length)) mpd-welcome-message)
	(mpd-end-conn conn "Process mpd not running on port %d in host \"%s\""
		      (_mpdgp) (_mpdgh)))
    (let ((verlist (split-string (substring welc (mpd-welcome-length))
				 "[\\.\n]")))
      (or (= (length verlist) mpd-ver-string-length)
	  (mpd-end-conn conn "Error parsing version information from server"))
      (let ((i 0))
	(while (< i mpd-ver-string-length)
	  (or (aset (_mpdgv) i (string-to-number-strict
				(pop verlist)))
	      (mpd-end-conn
	       conn "Error parsing version information from server"))
	  (setq i (1+ i))))))
  (message "Opened connection with mpd"))

(defun mpd-new-connection (host port &optional timeout noreconn)
  "Create a new connection with the mpd server.
Return a connection object, which could be used for further transactions with
the server. See `mpd-conn-new' for a description of the parameters. Close the
connection using `mpd-close-connection' when you no longer need it."
  (let ((conn (mpd-conn-new host port timeout noreconn)))
    (mpd-connect conn) conn))

(defun mpd-execute-command (conn cmd &optional mode)
  "Send the command CMD to the mpd server using CONN.
A newline is further appended to CMD by the function.  Mode MODE is used to
receive the output to the command, and could be one of the following:
 - A list, usually nil, to which cons-cells got by formatting each line
   of the output, except the last one, using `mpd-parse-line', are appended.
   The new list thus got is the output of the function.
 - t, by which all output before the last line, as a string,
   is the output of the function.
 - A function, by which each cons-cell, got as described above, is sent to
   this function.  Two parameters are passed to the function, the connection
   object and this cons-cell.  An empty string is the output.
The return value is a cons-cell, whose car is non-nil if the server succeeds.
The cdr is the output as specified in the description of MODE above."
  (mpd-assert-idle conn)
  (unless (or (and (_mpdgo) (eq (process-status (_mpdgo)) 'open)) (_mpdgr))
    (and (_mpdgo) (message "Connection with mpd broken, attempting reconnect"))
    (mpd-connect conn))
  (mpd-transact conn mode (concat cmd "\n"))
  (prog1
      (cons (_mpdgf)
	    ;; Declaration conn object structure dependent.
	    (aref conn (if (_mpdgf) (if (or (eq (_mpdgl) 't)
					    (functionp (_mpdgl))) 2 3) 4)))
    (_mpdsb "")))

(defun mpd-simple-exec (conn cmd)
  "Execute mpd command CMD using CONN ignoring output.
Note that an OK/ACK message still has to come.
Return value is non-nil iff the command succeeds.
`mpd-get-last-error' gives the server error message in case of failure.
See also `mpd-execute-command'."
  (if (not (listp (_mpdgs)))
      (car (mpd-execute-command conn cmd t))
    (_mpdss (cons cmd (_mpdgs))) t))

(defun mpd-close-connection (conn)
  "Close the mpd server connection given by CONN."
  (mpd-assert-idle conn)
  (delete-process (_mpdgo))
  (_mpdso nil))

(defvar mpd-inter-conn
  (apply 'mpd-conn-new `(,@(mpd-connection-tidy
			    mpd-interactive-connection-parameters) nil))
  "The global mpd connection used for interactive queries.")

(defsubst mpd-get-version (conn)
  "Get version information for the mpd server CONN is connected to.
Return a vector of three numbers, for the major, minor and patch levels."
  (assert-mpd-conn conn)
  (_mpdgv))
(put 'mpd-get-version 'side-effect-free t)

(defsubst mpd-get-last-error (conn)
  "Get the last server error message for mpd connection CONN.
Return nil in case of a successful last command."
  (assert-mpd-conn conn)
  (and (not (_mpdgf)) (_mpdgs)))
(put 'mpd-get-last-error 'side-effect-free t)

(defsubst mpd-get-connection-timeout (conn)
  "Get the timeout of mpd connection CONN.
Return nil if CONN isn't a mpd connection object."
  (assert-mpd-conn conn)
  (_mpdgt))
(put 'mpd-get-connection-timeout 'side-effect-free t)

;;;###autoload
(defun mpd-set-connection-timeout (conn timeout)
  "Set the timeout of mpd connection object CONN to TIMEOUT.
See also `mpd-new-connection'."
  (interactive
   (list mpd-inter-conn
	 (string-to-number
	  (read-string "Connection timeout (None): "
		       (let ((conn mpd-inter-conn))
			 (or conn (error "Connection not yet started"))
			 (and (_mpdgt) (number-to-string (_mpdgt))))))))
  (if (or (not (mpd-connp conn)) (and timeout (not (numberp timeout))))
      (error "Invalid parameters used to set connection timeout")
    (and (= timeout 0) (setq timeout nil))
    (_mpdst timeout)))

(defsubst mpd-get-reconnectible (conn)
  "Return t if CONN reconnects when its mpd connection breaks."
  (assert-mpd-conn conn)
  (not (_mpdgr)))
(put 'mpd-get-reconnectible 'side-effect-free t)

(defsubst mpd-make-reconnectible (conn &optional noreconn)
  "Make CONN reconnectible when its mpd connection breaks.
Unset the reconnectibility on non-nil prefix arg NORECONN."
  (assert-mpd-conn conn)
  (_mpdsr noreconn))

(defun mpd-force-accept-command (conn)
  "Force the mpd connection CONN to accept the next command.
*WARNING* DON'T use this unless you are really desperate.  Shelf this off for
debugging purposes. Normally, the package should signal correctly whether it is
safe to receive the next command. Doing this would mean losing out on the output
of the current command, and worse, the output of the aborted command could creep
into the current one."
  (if (mpd-command-list-mode-p conn)
      (error "Command list mode has not ended")
    (_mpdsf nil)
    (_mpdss "")
    (_mpdsb "")))

(defun mpd-command-list-begin (conn)
  "Start the command-list mode for CONN.
Only commands that can be used with `mpd-simple-exec' are allowed in
command-list mode. Commands can be issued as they are usually done. Return for
the commands is always t, as the commands are just queued up instead of being
sent to the server. `mpd-command-list-end' ends the command-list and execute the
list built up."
  (mpd-assert-idle conn)
  (_mpdsf nil)				; FIXME: Why is this here ??
  (_mpdss nil))

(defun mpd-command-list-mode-p (conn)
  "Return non-nil if mpd connection CONN is in command list mode."
  (assert-mpd-conn conn)
  (listp (_mpdgs)))
(put 'mpd-command-list-mode-p 'side-effect-free t)

(defun mpd-command-list-end (conn)
  "End command-list mode for CONN.
This function needs to be preceded by a call to `mpd-command-list-begin'."
  (or (mpd-command-list-mode-p conn)
      (error "The connection is not in command-list mode"))
  (let (str)
    (setq str (concat "command_list_begin\n"
		      (mapconcat '(lambda (item) item) (nreverse (_mpdgs)) "\n")
		      "\ncommand_list_end"))
    (_mpdss "")
    (mpd-simple-exec conn str)))

(defun mpd-connection-status (conn)
  "Get the status of mpd connection CONN.
Return one of 'busy for being in the midst of a request, 'ready for the ready
state, and 'command-list to indicate being in command-list mode."
  (assert-mpd-conn conn)
  (cond
   ((listp (_mpdgs)) 'command-list)
   ((stringp (_mpdgs)) 'ready)
   (t 'busy)))
(put 'mpd-connection-status 'side-effect-free t)

;;; Returned data descriptions for high level functions.

(eval-and-compile
  (defconst mpd-status-data ["volume" "repeat" "random" "playlist" "playlistlength"
			     "bit-rate" "song" "xfade" "state" "time-elapsed"
			     "time-total" "sample-rate" "bits-per-sample"
			     "channels" "error"]
    "Descriptions of the status vector received by a call to `mpd-get-status'.
Some of the less obvious descriptions are:
  - playlist : A \"checksum\" for the current playlist, guaranteed to change
    after a change in the playlist.
  - xfade : The number of seconds over which crossfade between two songs occurs.
    This value could be zero as well.
  - error : A description of the error, if one occurs, could be nil.
  - song : Position of the current song in the playlist.
  - state : Could be one of 'play, 'pause or 'stop.
All fields except error and state are whole numbers.  \"repeat\" and \"random\"
are in addition, bi-state variables (0/1)")

  (defconst mpd-stats-data ["artists" "albums" "songs" "uptime"
			    "playtime" "db_playtime" "db_update"]
    "Description of the stats vector received by a call to `mpd-get-stats'.
\"artists\", \"albums\", \"songs\", \"db_playtime\" correspond to the number of
artists, albums and songs and the total time in seconds of all songs in the
database respectively. \"db_update\" is the time stamp of the last update to the
database. \"playtime\" is the total time for which music has been played and
\"uptime\", the server uptime, both in seconds.")

  (defconst mpd-song-data ["file" "Artist" "Album" "Title" "Track" "Time"]
    "Description of a song got from the mpd server.
All fields except time, which is the total number of seconds in the song, are
strings.  All fields could be nil, in the absence of information pertaining to
the request sent to the server.")

  (defconst mpd-directory-data ["file" "playlist" "directory"]
    "Description of the data corresponding to a directory in the mpd database.
\"file\" is a list of songs, each of which is represented as described by
`mpd-song-data' in the directory. \"playlist\" and \"directory\" is the list of
playlists and subdirectories respectively."))

(defmacro mpd-song-data-length () (length mpd-song-data))

;;; High level public interface helper functions.

(defun mpd-status-receiver (vect cell)
  "Output handler for the \"status\" command to the mpd server.
See `mpd-execute-command' for a description of output handlers.
This is an internal function, do not use this in your code."
  (let ((sym (car cell)))
    (cond
     ((string= sym "state")
      (aset vect 8
	    (cond
	     ((string= (cdr cell) "play") 'play)
	     ((string= (cdr cell) "pause") 'pause)
	     ((string= (cdr cell) "stop") 'stop))))
     ((string= sym "time")
      (when (string-match "^\\([0-9]*\\):\\([0-9]*\\)$" (cdr cell))
	(aset vect 9 (string-to-number (match-string 1 (cdr cell))))
	(aset vect 10 (string-to-number (match-string 2 (cdr cell))))))
     ((string= sym "audio")
      (when (string-match "^\\([0-9]*\\):\\([0-9]*\\):\\([0-9]*\\)$" (cdr cell))
	(aset vect 11 (string-to-number (match-string 1 (cdr cell))))
	(aset vect 12 (string-to-number (match-string 2 (cdr cell))))
	(aset vect 13 (string-to-number (match-string 3 (cdr cell))))))
     ((string= sym "error")
      (aset vect 14 (cdr cell)))
     (t (vput vect mpd-status-data sym (string-to-number (cdr cell)))))))

;;; Silence the compiler.
(defvar mpd-song-receiver)
(defvar foreach)
(defvar mpd-song-receiver-args)

(defun mpd-song-receiver (conn cell)
  "Handle song data output from the mpd server.
See `mpd-execute-command' for a description of output handlers.
This is an internal function, do not use this in your code."
  (let ((offset (find-key-field (car cell) mpd-song-data)))
    (when offset
      (when (aref mpd-song-receiver offset)
	(setq foreach (apply 'mpd-seq-add mpd-song-receiver
			     foreach mpd-song-receiver-args))
	(let ((i (1- (mpd-song-data-length))))
	  (while (>= i 0)
	    (aset mpd-song-receiver i nil)
	    (setq i (1- i)))))
      (aset mpd-song-receiver offset
	    (if (string= (car cell) "Time")
		(string-to-number (cdr cell)) (cdr cell))))))

(defun mpd-get-songs (conn cmd &optional foreach)
  "Get details of songs from the mpd server using connection CONN.
Command CMD is used to get the songs. FOREACH is the function to call for each
song provided as the argument. Return list of all songs if FOREACH is not
specified and FOREACH otherwise. `mpd-song-data' describes the song vector."
  (or (functionp foreach) (setq foreach nil))
  (let ((mpd-song-receiver (make-vector (mpd-song-data-length) nil))
	mpd-song-receiver-args)
    (mpd-execute-command conn cmd 'mpd-song-receiver)
    (or (equal mpd-song-receiver (make-vector (mpd-song-data-length) nil))
	(setq foreach (mpd-seq-add mpd-song-receiver foreach)))
    (safe-nreverse foreach)))

(defun mpd-make-cmd-concat (cmd arg &optional normal-nil)
  "Make mpd command string using command CMD and argument ARG.
ARG could be a string or a list of strings. Unless NORMAL-NIL is non-nil,
a value of `nil' for ARG outputs CMD rather than not doing anything.
Command list mode is implicitly used for lists. Arguments are sanitized before
composition. This is an internal function, do not use this in your code."
  (cond
   ((not (or arg normal-nil)) cmd)
   ((listp arg)
    (concat "command_list_begin\n"
	    (mapconcat '(lambda (item) (concat cmd " " (mpd-safe-string item)))
		       arg "\n") "\ncommand_list_end"))
   (t (concat cmd " " (mpd-safe-string arg)))))
(put 'mpd-make-cmd-concat 'side-effect-free t)

(defun mpd-make-cmd-format (cmd validate arg1 &optional arg2)
  "Make mpd command string with format CMD and arguments ARG1, ARG2.
ARG1/ARG2 could be a list of arguments each. Command list mode is implicitly
used for lists. Each of the arguments pairs are sent to VALIDATE before
composition. This is an internal function, do not use this in your code."
  (if (listp arg1)
      (let ((tail2 arg2))
	(and arg2 (or (= (length arg1) (length arg2))
		      (error "Argument lists are of unequal lengths")))
	(concat "command_list_begin\n"
		(mapconcat '(lambda (item)
			      (and validate (funcall validate item (car tail2)))
			      (prog1
				  (format cmd item (car tail2))
				(setq tail2 (cdr tail2)))) arg1 "\n")
		"\ncommand_list_end"))
    (and validate (funcall validate arg1 arg2))
    (format cmd arg1 arg2)))

;;; Helper functions for interactive display.

(defun mpd-line-to-buffer (str)
  "Insert STR as a line to the mpd output buffer."
  (with-current-buffer (get-buffer-create "*mpd-output*")
    (with-free-buffer
      (goto-char (point-max))
      (insert (concat str "\n"))
      (display-buffer (current-buffer)))))

(defsubst mpd-separator-line (&optional num)
  "Make a separator line for insertion to the mpd output buffer."
  (propertize (concat (make-string (or num 80) ?*) "\n")
	      'face 'mpd-separator-face))

(defun mpd-init-buffer (&optional str1 str2 str3)
  "Initialize the mpd output buffer using strings STR1, STR2, STR3.
The layout is done as follows:
	STR1 		if non-empty
	***...*** 	if STR1 is non-nil
	STR2 		if non-empty
	***...*** 	if STR2 is non-nil
	STR3		if non-empty"
  (let ((max (max (length str1) (length str2) (length str3))))
    (with-current-buffer (get-buffer-create "*mpd-output*")
      (erase-buffer)
      (insert
       (concat "\n" (and str1 (not (string= str1 ""))
			 (propertize (concat str1 "\n") 'face 'mpd-header-face))
	       (and str1 (mpd-separator-line max))
	       (and str2 (propertize (concat str2 "\n") 'face 'mpd-header-face))
	       (and str3 (mpd-separator-line max))
	       (and str3 (not (string= str3 ""))
		    (propertize (concat str3 "\n")
				'face 'mpd-header-face)) "\n")))))

(defun mpd-render-field (desc val &optional nosep)
  "Format to a colorized line of form \"\\nDESC: VAL\".
The separating colon is output unless NOSEP is non-nil."
  (and val (not (string= val ""))
       (concat (propertize desc 'face 'mpd-first-field-face)
	       (and (not nosep) ": ")
	       (propertize val 'face 'mpd-second-field-face) "\n")))
(put 'mpd-render-field 'side-effect-free t)

(defun mpd-display-song (song)
  "Display mpd song data SONG in the output buffer."
  (or (equal song (eval-when-compile (make-vector (mpd-song-data-length) nil)))
      (mpd-line-to-buffer
       (concat (mpd-render-field "Title       " (aref song 3))
	       (mpd-render-field "Artist      " (aref song 1))
	       (mpd-render-field "Album       " (aref song 2))
	       (mpd-render-field "Track       " (aref song 4))
	       (mpd-render-field
		"Song Length "
		(and (aref song 5) (concat (number-to-string (aref song 5))
					   " seconds.")))
	       (mpd-render-field "Filename    " (aref song 0))
	       "\n" (mpd-separator-line)))))

(defun mpd-display-playlist-item (title num)
  "Display playlist item with TITLE and index NUM in mpd buffer."
  (mpd-line-to-buffer
   (concat (propertize (format "%4d  " (1+ num)) 'face 'mpd-first-field-face)
	   (propertize title 'face 'mpd-second-field-face))))

(defun mpd-display-dir-info (item type)
  "Display mpd directory information to the output buffer."
  (if (eq type 'file)
      (mpd-display-song item)
    (mpd-line-to-buffer
     (concat (mpd-render-field
	      (if (eq type 'playlist) "Playlist    " "Directory   ") item)
	     "\n" (mpd-separator-line)))))

(defun mpd-display-dir-listing (item dir)
  "Display mpd directory listing to the output buffer."
  (mpd-line-to-buffer
   (concat (mpd-render-field (if dir "Directory " "File      " ) item)
	   "\n" (mpd-separator-line))))

(defsubst mpd-display-bullet (str)
  "Display a bulleted line to the mpd output buffer"
  (mpd-line-to-buffer (mpd-render-field "o " str t)))

(defun read-item (prompt &optional default zero)
  "Read a number from the minibuffer.
PROMPT is the prompt string prefix to display. Append the DEFAULT value,
if present, in brackets. Return the number read. Unless ZERO is non-nil,
add default value by one before operation, and decrement number read
by 1 before returning."
  (let (num str)
    (and default (setq str (number-to-string (if zero default (1+ default)))))
    (setq
     num (string-to-number-strict
	  (read-string (concat prompt (if default (concat " (" str ")")) ": ")
		       nil nil str)))
    (if (and num (not zero)) (1- num) num)))

;;;; High level public interface.

;;;  These functions require output, and hence cannot be queued by using the
;;;  command-line mode.

(defun mpd-get-status (conn)
  "Get status of the mpd server, using connection CONN.
See `mpd-status-data' for a description of the vector returned."
  (let ((vect (make-vector (eval-when-compile (length mpd-status-data)) nil)))
    (mpd-execute-command
     conn "status" '(lambda (conn cell) (mpd-status-receiver vect cell))) vect))

(defun mpd-get-stats (conn)
  "Get statistics for the mpd server connected using CONN.
See `mpd-stats-data' for a description of the vector returned."
  (let ((stat (make-vector (eval-when-compile (length mpd-stats-data)) nil)))
    (mpd-execute-command conn "stats"
			 '(lambda (conn cell)
			    (vput stat mpd-stats-data (car cell)
				  (string-to-number (cdr cell)))))
    (message (prin1-to-string stat))
    stat))

;;;###autoload
(defun mpd-get-playlist (conn &optional foreach)
  "Get all songs in the current playlist managed by the mpd server.
CONN and FOREACH are as in `mpd-get-songs'."
  (interactive (progn (mpd-init-buffer "" "Current MPD Playlist" "")
		      (list mpd-inter-conn 'mpd-display-playlist-item)))
  (or (functionp foreach) (setq foreach nil))
  (mpd-execute-command
   conn "playlist" '(lambda (conn cell)
		      (setq foreach (mpd-elt-add
				     (cdr cell) foreach
				     (string-to-number (car cell))))))
  (safe-nreverse foreach))

;;;###autoload
(defun mpd-get-playlist-entry (conn &optional item foreach)
  "Get song data for entr(y/ies) ITEM in the current mpd playlist.
CONN and FOREACH are as in `mpd-get-songs'. ITEM is the item position or a list
of it. Note that ITEM as `nil' outputs for all entries in the current playlist
rather than not doing this. Return the list of songs or FOREACH depending on
FOREACH."
  (interactive
   (let ((item (read-item "Enter item number"
			  (aref (mpd-get-status mpd-inter-conn) 6))))
     (mpd-init-buffer "" (format "MPD Playlist Item # %d" (1+ item)) "")
     (list mpd-inter-conn item 'mpd-display-song)))
  (mpd-get-songs
   conn (if item
	    (mpd-make-cmd-format
	     "playlistinfo %d" '(lambda (item item2) (assert-wholenump item))
	     item) "playlistinfo") foreach))

;;;###autoload
(defun mpd-get-directory-songs (conn &optional directory foreach)
  "Get all songs in a directory of the mpd database.
CONN and FOREACH are as in `mpd-get-songs'. DIRECTORY is the relative directory
path wrt the database root. DIRECTORY could be a list as well, the action then
corresponds to all songs in all the directories. Note that the `nil' value for
DIRECTORY corresponds to the database toplevel rather than an empty list."
  (interactive
   (let ((str (read-string "Enter relative directory: ")))
     (progn (mpd-init-buffer "" (concat "Songs in directory " str) "")
	    (list mpd-inter-conn str 'mpd-display-song))))
  (mpd-get-songs
   conn (mpd-make-cmd-concat "listallinfo" directory) foreach))

;;;###autoload
(defun mpd-get-directory-info (conn &optional directory foreach)
  "Get directory info for DIRECTORY in the mpd database.
CONN is the mpd connection to use for this purpose and FOREACH, if specified as
a function, is called for each information field. The arguments passed to
FOREACH is a song object, directory string or playlist string and one of the
symbols 'file, 'playlist or 'directory describing the data sent. DIRECTORY
could be a list as well, the action then corresponds information for all the
directories. Note that a `nil' for DIRECTORY corresponds to the database
toplevel rather than an empty list. The return value is the directory info as
specified by `mpd-directory-data', if FOREACH is not used, and FOREACH
otherwise."
  (interactive
   (let ((str (read-string "Enter relative directory: ")))
     (progn (mpd-init-buffer "" (concat "Information on directory " str) "")
	    (list mpd-inter-conn str 'mpd-display-dir-info))))
  (or (functionp foreach) (setq foreach nil))
  (let (filemode (pl foreach) (dir foreach) (mpd-song-receiver-args '(file))
		 (mpd-song-receiver (make-vector (length mpd-song-data) nil)))
    (mpd-execute-command
     conn (mpd-make-cmd-concat "lsinfo" directory)
     '(lambda (conn cell)
	(if (string= (car cell) "directory")
	    (setq dir (mpd-elt-add (cdr cell) dir 'directory))
	  (if (string= (car cell) "playlist")
	      (setq pl (mpd-elt-add (cdr cell) pl 'playlist))
	    (mpd-song-receiver conn cell)))))
    (or (equal mpd-song-receiver (make-vector (length mpd-song-data) nil))
	(setq foreach (mpd-seq-add mpd-song-receiver foreach 'file)))
    (if (functionp foreach) foreach
      (vector (nreverse foreach) (nreverse pl) (nreverse dir)))))

;;;###autoload
(defun mpd-list-directory-recursive (conn foreach &optional directory)
  "Get the file-directory hierarchy of a directory in the mpd database.
CONN is the connection used and FOREACH is the function with which each entry is
reported along with a non-nil second argument if the entry is a
directory. DIRECTORY could be a list as well, the action then corresponds to
listing of all the directories. Note that the `nil' value for DIRECTORY
corresponds to the database toplevel rather than an empty list."
  (interactive
   (let ((str (read-string "Enter relative directory: ")))
     (progn (mpd-init-buffer "" (concat "Recursive listing of directory " str) "")
	    (list mpd-inter-conn 'mpd-display-dir-listing str))))
  (assert-type foreach functionp)
  (mpd-execute-command conn (mpd-make-cmd-concat "listall" directory)
		       '(lambda (conn cell)
			  (funcall foreach (cdr cell) (string= (car cell)
							       "directory")))))

;;;###autoload
(defun mpd-search (conn by for &optional foreach)
  "Search for songs in the mpd database.
CONN is the mpd connection used, the valid values for BY are 'artist, 'album
and 'title, indicating the field to search for, and FOR is the search string.
FOREACH and the return values are as in `mpd-get-songs'. FOR could be a
non-empty list as well, the action then is to search by BY for all FOR.
Note that duplicates could occur for a list of FOR,
use `sort-uniq-list' to sort and remove duplicates."
  (interactive
   (let ((reqb (intern-soft
		(completing-read "Search by: "
				 '(("artist") ("album") ("title")) nil t)))
	 (reqf (read-string "Search for: ")))
     (mpd-init-buffer "" (format "Search results for %s %s" reqb reqf) "")
     (list mpd-inter-conn reqb reqf 'mpd-display-song)))
  (or (eq by 'artist) (eq by 'album) (eq by 'title)
      (error "Invalid mpd search field %s" by))
  (mpd-get-songs
   conn
   (mpd-make-cmd-concat (concat "find " (symbol-name by)) for t) foreach))

;;;###autoload
(defun mpd-get-artists (conn &optional foreach)
  "Get the names of all artists whose songs are in the mpd database.
CONN is the connection to use and FOREACH is the optional function to call with
each name. Unless FOREACH is a function, when it is returned, a list of artist
names is the return value."
  (interactive
   (progn
     (mpd-init-buffer "" "List of artists" "")
     (list mpd-inter-conn 'mpd-display-bullet)))
  (or (functionp foreach) (setq foreach nil))
  (mpd-execute-command
   conn "list artist"
   '(lambda (conn cell)
      (and (string= (car cell) "Artist")
	   (setq foreach (mpd-elt-add (cdr cell) foreach)))))
  (safe-nreverse foreach))

;;;###autoload
(defun mpd-get-artist-albums (conn &optional artist foreach)
  "Get all albums in the mpd database featuring artist(s) ARTIST.
If ARTIST is not specified, all albums are provided. CONN is the connection to
use and FOREACH is the optional function to call with each name.
Unless FOREACH is a function, when it is returned, a list of artist names
is the return value. ARTIST could be a list as well, the action then is to
output the albums of all the artists."
  (interactive
   (let ((str (read-string "Name of the artist (All): ")))
     (and (string= str "") (setq str nil))
     (mpd-init-buffer "" (if str (concat "Albums of artist " str)
			   "List of albums") "")
     (list mpd-inter-conn str 'mpd-display-bullet)))
  (or (functionp foreach) (setq foreach nil))
  (mpd-execute-command
   conn (mpd-make-cmd-concat "list album" artist)
   '(lambda (conn cell)
      (and (string= (car cell) "Album")
	   (setq foreach (mpd-elt-add (cdr cell) foreach)))))
  (safe-nreverse foreach))

;;; These are command functions. These functions can be queued by using the
;;; command-list mode. See `mpd-command-list-begin' and `mpd-command-list-end'.

;;;###autoload
(defun mpd-enqueue (conn file)
  "Enqueue filename FILE (or list of FILE) to the mpd playlist."
  (interactive
   (list mpd-inter-conn
	 (if (and (stringp mpd-db-root) (not (string= mpd-db-root "")))
	     (file-relative-name
	      (file-truename
	       (read-file-name "Enqueue what: " mpd-db-root)) mpd-db-root)
	   (read-string "Enqueue what: "))))
  (mpd-simple-exec conn (mpd-make-cmd-concat "add" file)))

;;;###autoload
(defun mpd-delete (conn pos &optional sort)
  "Delete song at position POS from the mpd playlist.
POS could be a list of positions to delete as well. If POS is a list, and SORT
is non-nil, it is sorted in descending order and duplicates removed before
proceeding. Note that this is necessary for the correctness of deletion.
SORT is optional to provide for arguments already satisfying the condition."
  (interactive (list mpd-inter-conn (read-item "Enter item to be deleted")))
  ;; Deletion changes the playlist ordering of all those below the deleted item.
  ;; Hence, sort and uniquify the list in descending order.
  (and (listp pos) sort (setq pos (sort-uniq-list pos '> '=)))
  (mpd-simple-exec
   conn
   (mpd-make-cmd-format "delete %d"
			'(lambda (item ig) (assert-wholenump item)) pos)))

;;;###autoload
(defun mpd-save-playlist (conn file)
  "Save current mpd playlist to FILE (or list of FILE)."
  (interactive (list mpd-inter-conn (read-string "Save playlist to: ")))
  (mpd-simple-exec conn (mpd-make-cmd-concat "save" file t)))

;;;###autoload
(defun mpd-load-playlist (conn plname)
  "Load playlist PLNAME (or list of PLNAME) to the mpd server."
  (interactive (list mpd-inter-conn (read-string "Load playlist: ")))
  (mpd-simple-exec conn (mpd-make-cmd-concat "load" plname t)))

;;;###autoload
(defun mpd-remove-playlist (conn plname)
  "Remove playlist PLNAME from the mpd playlist directory.
PLNAME could as well be a list of playlist names."
  (interactive (list mpd-inter-conn (read-string "Remove playlist: ")))
  (mpd-simple-exec conn (mpd-make-cmd-concat "rm" plname t)))

;;;###autoload
(defun mpd-shuffle-playlist (conn)
  "Shuffle current mpd playlist using connection CONN."
  (interactive (list mpd-inter-conn))
  (mpd-simple-exec conn "shuffle"))

;;;###autoload
(defun mpd-clear-playlist (conn)
  "Clear current mpd playlist using connection CONN."
  (interactive (list mpd-inter-conn))
  (mpd-simple-exec conn "clear"))

;;;###autoload
(defun mpd-play (conn &optional pos)
  "Play song at position POS (default first) in the mpd playlist."
  (interactive (list mpd-inter-conn (read-item "Enter item to play" 0)))
  (and pos (assert-wholenump pos))
  (mpd-simple-exec
   conn (concat "play" (and pos (concat " " (number-to-string pos))))))

;;;###autoload
(defun mpd-stop (conn)
  "Stop playing the current mpd playlist."
  (interactive (list mpd-inter-conn))
  (mpd-simple-exec conn "stop"))

;;;###autoload
(defun mpd-pause (conn)
  "Pause playing the current mpd playlist."
  (interactive (list mpd-inter-conn))
  (mpd-simple-exec conn "pause"))

;;;###autoload
(defun mpd-next (conn)
  "Play next song in the current mpd playlist."
  (interactive (list mpd-inter-conn))
  (mpd-simple-exec conn "next"))

;;;###autoload
(defun mpd-prev (conn)
  "Play previous song in the current mpd playlist."
  (interactive (list mpd-inter-conn))
  (mpd-simple-exec conn "previous"))

;;;###autoload
(defun mpd-move (conn from to)
  "Move item from position FROM in the current mpd playlist to TO.
For lists of FROM and TO, do action in that order for each pair of items.
If sending a list for FROM and TO, note that every move changes the order
of items in the playlist."
  (interactive (list mpd-inter-conn (read-item "Source item number")
		     (read-item "Destination item number")))
  (and from to
       (mpd-simple-exec conn (mpd-make-cmd-format "move %d %d"
						  '(lambda (i j)
						     (assert-wholenump i)
						     (assert-wholenump j))
						  from to))))

;;;###autoload
(defun mpd-swap (conn first second)
  "Swap positions FIRST and SECOND in the current mpd playlist.
For lists of FROM and TO, do action in that order for each pair of items.
See also `mpd-move'."
  (interactive (list mpd-inter-conn (read-item "Swap item at number")
		     (read-item "With item at number")))
  (and first second
       (mpd-simple-exec conn (mpd-make-cmd-format "swap %d %d"
						  '(lambda (i j)
						     (assert-wholenump i)
						     (assert-wholenump j))
						  first second))))

;;;###autoload
(defun mpd-seek (conn song &optional time)
  "Seek to song position SONG and time TIME in the mpd playlist."
  (interactive
   (let (status)
     (and (eq (mpd-connection-status mpd-inter-conn) 'ready)
	  (setq status (mpd-get-status mpd-inter-conn)))
     (list mpd-inter-conn
	   (read-item "Seek to song" (and status (aref status 6)))
	   (read-item "Time in seconds" (and status (aref status 9)) t))))
  (assert-wholenump song)
  (if time (assert-wholenump time) (setq time 0))
  (mpd-simple-exec conn (format "seek %d %d" song time)))

;;;###autoload
(defun mpd-toggle-random (conn &optional arg)
  "Change random mode of mpd using connection CONN.
With ARG, set random on iff ARG is positive."
  (interactive (list mpd-inter-conn current-prefix-arg))
  (setq arg (if arg (> (prefix-numeric-value arg) 0)
	      (= (aref (mpd-get-status conn) 2) 0)))
  (mpd-simple-exec conn (concat "random " (if arg "1" "0"))))

;;;###autoload
(defun mpd-toggle-repeat (conn &optional arg)
  "Change repeat mode of mpd using connection CONN.
With ARG, set repeat on iff ARG is positive."
  (interactive (list mpd-inter-conn current-prefix-arg))
  (setq arg (if arg (> (prefix-numeric-value arg) 0)
	      (= (aref (mpd-get-status conn) 1) 0)))
  (mpd-simple-exec conn (concat "repeat " (if arg "1" "0"))))

;;;###autoload
(defun mpd-set-volume (conn vol)
  "Set the volume for the mpd player to volume VOL."
  (interactive
   (list mpd-inter-conn
	 (read-item "New volume"
		    (and (eq (mpd-connection-status mpd-inter-conn) 'ready)
			 (aref (mpd-get-status mpd-inter-conn) 0)) t)))
  (assert-wholenump vol)
  (mpd-simple-exec conn (format "setvol %d" vol)))

;;;###autoload
(defun mpd-adjust-volume (conn vol)
  "Adjust the volume for the mpd player by volume VOL.
VOL could be positive for a volume increase and negative otherwise."
  (interactive (list mpd-inter-conn
		     (string-to-number-strict
		      (read-string "Adjust volume by: ") t)))
  (assert-numberp vol)
  (mpd-simple-exec conn (format "volume %d" vol)))

;;;###autoload
(defun mpd-set-crossfade (conn time)
  "Set cross-fading time for the mpd player to TIME in seconds.
Zero TIME turns off cross-fading."
  (interactive (list mpd-inter-conn
		     (read-item "New crossfade time in seconds"
				(aref (mpd-get-status mpd-inter-conn) 7) t)))
  (assert-wholenump time)
  (mpd-simple-exec conn (format "crossfade %d" time)))

;;;###autoload
(defun mpd-set-password (conn pass)
  "Set the password for access to the mpd server.
*WARNING* The password is sent to the server in plaintext.
The processing done by libmpdee to send the command for
setting the password also has its data as plaintext."
  (interactive (list mpd-inter-conn (read-passwd "Enter password: ")))
  (assert-string pass)
  (mpd-simple-exec conn (concat "password" (mpd-safe-string pass))))

;;;###autoload
(defun mpd-update (conn &optional enforce-timeout)
  "Instruct the mpd server using CONN to update its database.
Unless ENFORCE-TIMEOUT is non-nil or the command is executed in
command list mode, the timeout for the connection is ignored."
  (interactive (list mpd-inter-conn))
  (assert-mpd-conn conn)
  (if (or enforce-timeout (listp (_mpdgs)))
      (mpd-simple-exec conn "update")
    (let ((timeout (_mpdgt)))
      (_mpdst nil)
      (unwind-protect
	  (mpd-simple-exec conn "update")
	(_mpdst timeout)))))

(defun mpd-ping (conn)
  "Use connection CONN to ping the mpd server.
Return non-nil on success."
  (mpd-simple-exec conn "ping"))

(defun mpd-conn-strongp (conn)
  (and (mpd-connp conn)
       (vectorp (_mpdgv))
       (= (length (_mpdgv)) mpd-ver-string-length)
       (or (not (_mpdgo)) (processp (_mpdgo)))
       (stringp (_mpdgb))
       (or (not (_mpdgt)) (numberp (_mpdgt)))
       (stringp (_mpdgh))
       (wholenump (_mpdgp))))
(put 'mpd-conn-strongp 'side-effect-free 'error-free)

;;; Adapted from bbdb.el
;;;###autoload
(defun mpd-libmpdee-submit-bug-report ()
  (interactive)
  (eval-and-compile
    (require 'reporter)
    (require 'sendmail))
  (delete-other-windows)
  (reporter-submit-bug-report
   "andyetitmoves@gmail.com"
   (concat "libmpdee " libmpdee-version)
   (append
    ;; all mpd connections
    (apropos-internal
     "" '(lambda (sym)
	   (and (boundp sym) (mpd-conn-strongp (symbol-value sym)))))
    ;; some variables
    '(emacs-version features))
   nil nil
   "Please change the Subject header to a concise bug description.
In this report, remember to cover the basics, that is,
what you expected to happen and what in fact did happen.
If you got an error, please enable debugging by
	M-x set-variable debug-on-error t
or if you have `dbfrobs', M-x debug-on-interesting-errors
Then reproduce the error and immediately call
	M-x mpd-libmpdee-submit-bug-report
The backtrace will be automatically included with the report.
Please remove these instructions from your message.")

  ;; insert the backtrace buffer content if present
  (let ((backtrace (get-buffer "*Backtrace*")))
    (when backtrace
      (goto-char (point-max))
      (insert "\nPossible backtrace for libmpdee:\n\n")
      (insert-buffer-substring backtrace)))

  (goto-char (point-min))
  (mail-position-on-field "Subject"))

(provide 'libmpdee)

;;; LIBMPDEE.EL ends here
