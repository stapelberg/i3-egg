 ;; We need this lookup table for event type to event name, because i3
 ;; uses event names when subscribing but event types when
 ;; delivering. Hopefully we can get rid of this in future versions :).
 (define-constant
   i3-event-name-to-type
   '(("workspace" . 0)
     ("output" . 1)
     ("mode" . 2)))

 ;; tell medea to always use lists for arrays, not vectors
 (json-parsers (alist-cons 'array identity (json-parsers)))

 (define (i3-socket-path)
   (string-trim-right (capture "i3 --get-socketpath")))

 ;; writes a 32-bit unsigned integer
 (define (write-u32 val)
   (write-u8vector (blob->u8vector/shared (u32vector->blob/shared (u32vector val)))))

 (define (read-u32)
   (u32vector-ref (blob->u32vector (u8vector->blob (read-u8vector 4))) 0))

 (define-record i3-conn
   cmd-fd
   event-fd
   evmutex
   event-thread
   callbacks)

 ;; Alias for i3-conn? because the module can be used with a
 ;; user-specified prefix.
 (define connection?
   i3-conn?)

 (define (connect)
   @("Connects to i3 running on the display specified by the environment variable {{DISPLAY}}."
     (@to "connection"))
   (let ((cmd-fd (socket af/unix sock/stream))
	 (event-fd (socket af/unix sock/stream)))
     (socket-connect cmd-fd (unix-address (i3-socket-path)))
     (socket-connect event-fd (unix-address (i3-socket-path)))
     (let ((conn (make-i3-conn
		  cmd-fd
		  event-fd
		  (make-mutex)
		  ;; Thread will be filled in after the record is
		  ;; created, because it uses the connection.
		  #f
		  '())))
       (i3-conn-event-thread-set!
	conn
	(thread-start! (lambda () (read-events conn))))
       conn)))

 (define (process-events-forever conn)
   (thread-join! (i3-conn-event-thread conn)))
 
 (define (i3-format-ipc-message payload type)
   (with-output-to-string
     (lambda ()
       (display "i3-ipc")
       (write-u32 (string-length payload))
       (write-u32 type)
       (display payload))))

 ;; Reads one message from the specified socket, then returns the
 ;; reply and its type.
 (define (i3-read-one-message sock)
   (socket-receive sock (string-length "i3-ipc"))
   (let* ((reply-length (with-input-from-string (socket-receive sock 4) read-u32))
	  (reply-type (with-input-from-string (socket-receive sock 4) read-u32))
	  (reply (read-json (socket-receive sock reply-length))))
     (values reply reply-type)))

 (define (cmd conn msg #!optional (type 0))
   @("Sends the given MSG to i3, by default as command."
     (conn "A connection to i3, created with [[#connect|(connect)]].")
     (msg "The payload of the message, e.g. a command when {{type}} is 0.")
     (type "The numeric message type, “COMMAND” (0) by default.

See also [[http://i3wm.org/docs/ipc.html#_sending_messages_to_i3]] for message types.")
     (@to "reply + reply-type")
     (@example-no-eval "Change focus to the window to the right:"
	       (cmd (connect) "focus right")))
   (let ((sock (i3-conn-cmd-fd conn)))
     (socket-send-all sock (i3-format-ipc-message msg type))
     (i3-read-one-message sock)))

 ;; Forever reads events from the event file descriptor and dispatches
 ;; them to callback handlers.
 (define (read-events conn)
   (let ((events-fd (i3-conn-event-fd conn))
	 (mutex (i3-conn-evmutex conn)))
     (let loop ()
       (thread-wait-for-i/o! (socket-fileno events-fd) #:input)
       (mutex-lock! mutex)
       (receive
	(reply reply-type)
	(i3-read-one-message events-fd)
	;; For events, the highest bit is 1, the rest is the event ID.
	(let* ((event-type (bitwise-and #x7F reply-type))
	       (callbacks (i3-conn-callbacks conn))
	       (callback (alist-ref
			  event-type
			  callbacks
			  eqv?
			  (lambda (unused) (format #t "no callback for event")))))
	  (callback reply)))
       (mutex-unlock! mutex)
       (loop))))

 (define (subscribe conn event thunk)
   @("Subscribes to the specified EVENT (e.g. \"workspace\") and calls
THUNK when an event arrives.")
   (i3-conn-callbacks-set!
    conn
    (alist-update!
     ;; Map the event name to its type here so that comparisons are
     ;; easier later on.
     (alist-ref event i3-event-name-to-type string=?)
     thunk
     (i3-conn-callbacks conn)))
   ;; We cannot use i3-command here since that uses the wrong
   ;; connection.
   (let ((sock (i3-conn-event-fd conn))
	 (mutex (i3-conn-evmutex conn)))
     (mutex-lock! mutex)
     (socket-send-all sock (i3-format-ipc-message (json->string (vector event)) 2))
     (i3-read-one-message sock)
     (mutex-unlock! mutex)))

 (define (tree conn)
   @("Convenience function to get the layout tree from i3.

See [[http://i3wm.org/docs/ipc.html#_tree_reply]] for the reply
format."
     (conn "A connection to i3, created with [[#connect|(connect)]].")
     (@to "reply + reply-type")
     (@example-no-eval "Return a list of all Chrome windows:"
		       (filter-containers
			  (lambda (con)
			    (string-suffix? " - Google Chrome" (alist-ref 'name con)))
			  (tree (connect)))))
   (cmd conn "" 4))

 (define (workspaces conn)
   @("Convenience function to get the workspaces from i3.

See [[http://i3wm.org/docs/ipc.html#_workspaces_reply]] for the reply
format."
     (conn "A connection to i3, created with [[#connect|(connect)]]")
     (@to "reply + reply-type"))
   (cmd conn "" 1))

 ;; returns the name of the currently focused workspace by filtering
 ;; the list of workspaces for the one which has focused == true
 (define (i3-get-focused-workspace-name conn)
   (alist-ref 'name
	      (find (cut alist-ref 'focused <>) (workspaces conn))))

 (define (filter-containers predicate tree)
   @("Returns a list containing all containers for which the given
predicate returns #t."
     (predicate "Predicate which is evaluated for each i3
container. Only containers for which the predicate returns #t are
included in the return list")
     (tree "(Part of) a list of containers as returned by [[#tree|(tree)]].")
     (@to "list")
     (@example-no-eval "Return a list of all Chrome windows:"
		       (filter-containers
			(lambda (con)
			  (string-suffix? " - Google Chrome" (alist-ref 'name con)))
			(tree (connect)))))
   (if (null? tree)
       '()
       (append (if (predicate tree) (list tree) (list))
	       (apply append
		      (filter-map (lambda (node) (filter-containers predicate node))
				  (cdr (assoc 'nodes tree)))))))

 ;; besser: (define i3-output-containers (by-type …))
 (define-syntax container-getter-by-type
   (syntax-rules ()
     ((_ name con-type)
      (define (name tree)
	(filter-containers (lambda (con) (= (alist-ref 'type con) con-type)) tree)))))

 ;; XXX: comparing type with the magic number 4 is unclean and will
 ;; break once this field is properly exported by i3
 (container-getter-by-type i3-output-containers 1)
 (container-getter-by-type i3-workspace-containers 4)

 (define (descend-focused stop-predicate tree)
   @("Descends the focused containers of the given TREE, stopping
at the first container which satisfies STOP-PREDICATE."
     (stop-predicate "Processing stops when this predicate first
returns true. The return value is the container with which this
predicate was evaluated.")
     (tree "(Part of) a list of containers as returned by [[#tree|(tree)]].")
     (@to "alist"))
   (if (stop-predicate tree)
       tree
       (if (null? (alist-ref 'focus tree))
	   tree
	   (descend-focused
	    stop-predicate
	    (let ((focused-id (first (alist-ref 'focus tree))))
	      (find (lambda (con) (= (alist-ref 'id con) focused-id))
		    (append (alist-ref 'nodes tree)
			    (alist-ref 'floating_nodes tree))))))))

 (define (focused-con tree)
   @("Returns the currently focused container."
     (tree "(Part of) a list of containers as returned by [[#tree|(tree)]].")
     (@to "alist")
     (@example-no-eval "Print the name of the currently focused window:"
		       (format #t "~A~N"
			       (alist-ref 'name (focused-con (tree (connect))))))
     (@example-no-eval "Print the name of the focused window of workspace 1:"
		       (let ((ws-1 (first (filter-containers
					   (lambda (con)
					     (and
					      (= (alist-ref 'type con) 4)
					      (string= (alist-ref 'name con) "1")))
					   (tree (connect))))))
			 (format #t "~A~N" (focused-con ws-1)))))
   (descend-focused
    (lambda (con) (null? (alist-ref 'nodes con)))
    tree))
