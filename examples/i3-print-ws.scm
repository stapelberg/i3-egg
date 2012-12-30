;;;; Prints the current workspace name upon every workspace change
;;;; event.
;;;;
;;;; © 2012 Michael Stapelberg
(use i3)

(let ((conn (connect)))
  (subscribe conn
	     "workspace"
	     (lambda (event)
	       ;; We only care about "change":"focus" events, the others don’t
	       ;; contain the workspace name.
	       (if (string=? "focus"
			     (alist-ref 'change event eqv? ""))
		   (let ((ws-name (alist-ref 'name (alist-ref 'current event))))
		     (format #t "Now on workspace ~A~N" ws-name)))))
  (process-events-forever conn))
