(asdf:load-system :footsoldiers)
(ql:quickload "footsoldiers")
(sb-ext:save-lisp-and-die #p"footsoldiers-runner" :compression 9 :toplevel #'footsoldiers:run-footsoldiers :executable t)