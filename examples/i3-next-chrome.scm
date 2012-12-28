;;;; Small binary to focus the chrome window on the current workspace
;;;; or start a new chrome instance.
;;;;
;;;; © 2012 Michael Stapelberg
(use i3)

(define (i3-next-chrome)
  ;; TODO: provide container type functions in i3-ipc so that we don’t have to use magic numbers
  (let* ((conn (connect))
	 (tree (tree conn))
	 (workspace (descend-focused (lambda (con) (= (alist-ref 'type con) 4)) tree))
	 (chrome-windows (filter-containers
			  workspace
			  (lambda (con)
			    (string-suffix? " - Google Chrome" (alist-ref 'name con))))))
    (if (null? chrome-windows)
	(cmd conn "exec google-chrome")
	;; XXX: It’d be better (but significantly more complex) if
	;; we’d chose the nearest chrome window.
	(cmd conn (format "[con_id=\"~A\"] focus" (alist-ref 'id (first chrome-windows)))))))

(i3-next-chrome)
