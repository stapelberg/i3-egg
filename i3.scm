;;;; i3 IPC interface in Chicken Scheme
;;;; © 2012 Michael Stapelberg <michael@i3wm.org>

@(egg "i3")
@(title "i3")
@(description "Extends the i3 window manager via its IPC interface.")
@(author "Michael Stapelberg")
@(email "michael@i3wm.org")

@(text "Note that there are more examples than what is documented here
at [[http://code.stapelberg.de/git/i3-egg/tree/examples]]. That
repository is also the canonical location for this egg’s source
code.")

(module
 i3
 (connect
  cmd
  subscribe
  tree
  workspaces
  filter-containers
  descend-focused
  focused-con
  connection?
  process-events-forever)
 (import scheme)
 (import chicken)
 (use srfi-1)
 (use data-structures)
 (use extras)
 (use ports)
 (use srfi-18)
 (use srfi-13)
 (use medea)
 (use shell)
 ;; vector datatypes
 (use srfi-4)
 (use socket)
 (use cock)

 (include "i3-core.scm")
)
