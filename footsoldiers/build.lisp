(asdf:load-system :footsoldiers)
(ql:quickload "footsoldiers")
(sb-ext:save-lisp-and-die #p"footsoldiers-runner" :compression 9 :save-runtime-options t :toplevel #'footsoldiers:run-footsoldiers :executable t)
