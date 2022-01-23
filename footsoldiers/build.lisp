(load "footsoldiers.asd")
(ql:quickload "footsoldiers")
(sb-ext:save-lisp-and-die #p"footsoldiers" :toplevel #'footsoldiers:run-footsoldiers :executable t)
