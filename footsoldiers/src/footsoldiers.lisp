(defpackage footsoldiers
  (:use :cl :herodotus :bind
        :alexandria :try
        :iterate :trivia
        :local-time
        :trivia.ppcre
        :metabang-bind
        :yason
        :monad 
        :n-player-game
        :anaphora
        :either)
  (:export :distance :neighbours
           :make-soldier :reachable-positions
           :soldier-pos :closest-reachable-position
           :soldier-destination
           :move-soldier
           :move-soldiers
           :eligible-target
           :make-base
           :find-target
           :soldier-health
           :attack-soldier
           :attack-base
           :player-health
           :make-game
           :make-player
           :attack-target
           :make-soldier-attack
           :make-soldiers-attack
           :game-player1
           :game-player2
           :game-map
           :close-enough-to-base
           :build-soldier
           :apply-move
           :make-build
           :apply-moves
           :make-move-result
           :move-result-updated-game
           :move-result-errors
           :game-over
           :determine-result
           :get-players-input-for-turn
           :step-game
           :parse-move
           :advance-turn
           :coord-alist
           :game-alist
           :start-game
           :determine-result
           :run-footsoldiers
           :change-destination-soldier-position
           :change-soldier-destination
           :format-position
           :make-change-destination
           :format-command
           :make-rock
           :*default-game-config*
           :game-config
           :game-config-money-per-turn
           :construct-bot-paths))

(in-package :footsoldiers)

(defstruct rock)

(defun rock-alist (coord r)
  (declare (ignore r))
  (list (cons "position" (coord-alist coord))
        (cons "type" "ROCK")))

(defstruct soldier pos health type team destination)

(defun coord-alist (coord)
  (list (cons "x" (car coord))
        (cons "y" (cdr coord))))

(defun soldier-alist (soldier)
  (list
   (cons "position" (coord-alist (soldier-pos soldier)))
   (cons "health" (soldier-health soldier))
   (cons "type" (format nil "~a" (soldier-type soldier)))
   (cons "team" (soldier-team soldier))
   (cons "destination" (coord-alist (soldier-destination soldier)))))

(defstruct base team)

(defun base-alist (position base)
  (list
   (cons "type" "BASE")
   (cons "position" (coord-alist position))
   (cons "team" (base-team base))))

(defun map-to-alist (mp)
  (map 'vector (lambda (e) 
                 (match (cdr e)
                   ((type rock) (rock-alist (car e) (cdr e)))
                   ((type soldier) (soldier-alist (cdr e)))
                   ((type base) (base-alist (car e) (cdr e)))))
       (sort (hash-table-alist mp) #'pair-less :key #'car)))

(defstruct player team money base health)

(defun player-alist (player)
  (list 
   (cons "team" (player-team player))
   (cons "money" (player-money player))
   (cons "base" (coord-alist (player-base player)))
   (cons "health" (player-health player))))

(defstruct build soldier-type start destination)

(defun build-alist (build)
  (list 
   (cons "soldier-type" (build-soldier-type build))
   (cons "start" (build-start build))
   (cons "destination" (build-destination build))))

(defstruct move-result errors updated-game)

(defstruct speed-config scout assassin tank)
(defstruct damage-config scout assassin tank)
(defstruct cost-config scout assassin tank)
(defstruct health-config scout assassin tank)

(defstruct game-config 
  initial-money
  money-per-turn
  max-distance-from-base
  health
  speed
  damage
  cost)

(defparameter *soldier-types* '(:scout :tank :assassin))
(defparameter *default-health-config* 
  (make-health-config :scout 6 :assassin 6 :tank 6))
(defparameter *default-speed-config*
  (make-speed-config :scout 3 :assassin 5 :tank 2))
(defparameter *default-damage-config* 
  (make-damage-config :scout 2 :assassin 5 :tank 2))
(defparameter *default-cost-config*
  (make-cost-config :scout 10 :tank 12 :assassin 14))
(defparameter *default-game-config*
  (make-game-config 
   :initial-money 10
   :money-per-turn 3 
   :max-distance-from-base 5 
   :health *default-health-config*
   :speed *default-speed-config*
   :damage *default-damage-config*
   :cost *default-cost-config*))

(defstruct game map turns-remaining player1 player2 (config *default-game-config*))

(defun game-alist (game)
  (list
   (cons "map" (map-to-alist (game-map game)))
   (cons "turns-remaining" (game-turns-remaining game))
   (cons "player1" (player-alist (game-player1 game)))
   (cons "player2" (player-alist (game-player2 game)))))

(defmethod initial-health (soldier-type (game-config game-config))
  (let ((config (game-config-health game-config)))
    (case soldier-type
      (:scout (health-config-scout config))
      (:assassin (health-config-assassin config))
      (:tank (health-config-tank config)))))

(defmethod soldier-speed (soldier-type (game-config game-config))
  (let ((config (game-config-speed game-config)))
    (case soldier-type
      (:scout (speed-config-scout config))
      (:assassin (speed-config-assassin config))
      (:tank (speed-config-tank config)))))

(defmethod damage (soldier-type (game-config game-config))
  (let ((config (game-config-damage game-config)))
    (case soldier-type
      (:scout (damage-config-scout config))
      (:assassin (damage-config-assassin config))
      (:tank (damage-config-tank config)))))

(defmethod cost (soldier-type (game-config game-config))
  (let ((config (game-config-cost game-config)))
    (case soldier-type
      (:scout (cost-config-scout config))
      (:assassin (cost-config-assassin config))
      (:tank (cost-config-tank config)))))

(defun sqr (x) (* x x))

(defun x (pos) (car pos))
(defun y (pos) (cdr pos))

(defmethod distance (p1 p2) 
  (+ (+ (abs (- (x p1) (x p2))) (abs (- (y p1) (y p2))))))

(defun neighbours (pos mp)
  (let ((ns (list (cons (x pos) (+ (y pos) 1))
                  (cons (- (x pos) 1) (y pos))
                  (cons (x pos) (- (y pos) 1))
                  (cons (+ (x pos) 1) (y pos)))))
    (remove-if (lambda (p) (gethash p mp)) ns)))

(defun close-enough (range p1 p2)
  (<= (distance p1 p2) range))

(defun reachable-positions (range p1 mp)
  (let ((seen (make-hash-table :test 'equal)))
    (setf (gethash p1 seen) t)
    (labels ((rec (current)
               (let ((nbrs (remove-if-not (lambda (nbr) (and (not (gethash nbr seen))
                                                             (close-enough range nbr p1)))
                                          (neighbours current mp))))
                 (mapc (lambda (nbr) (setf (gethash nbr seen) t)) nbrs)
                 (mapc #'rec nbrs))))
      (rec p1)
      seen)))

(defmethod closest-reachable-position ((s soldier) mp (game-config game-config))
  (let ((range (soldier-speed (soldier-type s) game-config))
        (origin (soldier-pos s))
        (dest (soldier-destination s)))
    (iter (for p in (sort (hash-table-keys (reachable-positions range origin mp)) #'pair-less))
          (reducing p by (lambda (a b) (if (< (distance b dest) (distance a dest)) b a)) 
                    initial-value origin))))

(defmethod move-soldier (mp (s soldier) (game-config game-config))
  (when (not (equal (soldier-pos s) (soldier-destination s)))
    (let* ((destination (closest-reachable-position s mp game-config))
           (new-soldier (copy-structure s)))
      (setf (soldier-pos new-soldier) destination)
      (remhash (soldier-pos s) mp)
      (setf (gethash destination mp) new-soldier))))

(defmethod is-soldier ((s soldier)) t)
(defmethod is-soldier ((non-soldier t)) nil)
(defmethod is-soldier ((r rock)) nil)

(defun pair-less (p1 p2)
  (or (< (car p1) (car p2))
      (and (= (car p1) (car p2))
           (< (cdr p1) (cdr p2)))))

(defun move-soldiers (mp game-config)
  (let ((es (sort (hash-table-alist mp) #'pair-less :key #'car)))
    (iter (for (p . e) in es)
          (when (is-soldier e)
            (move-soldier mp e game-config)))))

(defmethod eligible-target ((s soldier) e) 
  (match e
    ((type rock) nil)
    ((soldier team) 
     (not (equalp team (soldier-team s))))
    ((base team)
     (not (equalp team (soldier-team s))))))

(defmethod find-target ((s soldier) mp)
  (bind (((x . y) (soldier-pos s))
         (search-candidates (list (cons x (+ y 1))
                                  (cons (- x 1) y)
                                  (cons x (- y 1))
                                  (cons (+ x 1) y))))
    (awhen (find-if (lambda (p) 
                (and (gethash p mp)
                     (eligible-target s (gethash p mp)))) search-candidates)
      (gethash it mp))))

(defmethod attack-soldier ((s1 soldier) (s2 soldier) mp config)
  (let ((new-health (max 0 (- (soldier-health s2) (damage (soldier-type s1) config)))))
    (if (= new-health 0)
        (remhash (soldier-pos s2) mp)
        (setf (gethash (soldier-pos s2) mp)
              (let ((new-soldier (copy-structure s2)))
                (setf (soldier-health new-soldier) new-health)
                new-soldier)))
    mp))

(defmethod attack-base ((s soldier) (b base) (gm game))
  (let ((new-player (if (equalp (base-team b) (player-team (game-player1 gm)))
                        (copy-structure (game-player1 gm))
                        (copy-structure (game-player2 gm)))))
    (setf (player-health new-player) (max 0 (- (player-health new-player)
                                               (damage (soldier-type s) (game-config gm)))))
    (if (equalp (base-team b) (player-team (game-player1 gm)))
        (setf (game-player1 gm) new-player)
        (setf (game-player2 gm) new-player))
    gm))

(defmethod attack-target ((s soldier) e (gm game))
  (match e
    ((type rock) gm)
    ((type soldier) 
     (let ((new-gm (copy-structure gm)))
       (setf (game-map new-gm)
             (attack-soldier s e (game-map gm) (game-config gm)))
       new-gm))
    ((type base) 
     (attack-base s e gm))))

(defmethod make-soldier-attack ((gm game) (s soldier))
  (let ((target (find-target s (game-map gm))))
    (if target
        (attack-target s target gm)
        gm)))

(defmethod make-soldiers-attack ((gm game))
  (let ((entries (sort (hash-table-alist (game-map gm)) #'pair-less :key #'car)))
    (iter (for (p . e) in entries)
          (for new-gm first (match e
                              ((type rock) gm)
                              ((type soldier) 
                               (make-soldier-attack gm e))
                              ((type base) gm)) 
               then (match e 
                      ((type rock) new-gm)
                      ((type soldier) 
                       (make-soldier-attack new-gm e))
                      ((type base) new-gm)))
          (finally (return new-gm)))))

(defmethod close-enough-to-base (pos (player player) (game-config game-config))
  (<= (distance pos (player-base player))
      (game-config-max-distance-from-base game-config)))

(defun format-position (pos)
  (format nil "(~a, ~a)" (car pos) (cdr pos)))

(defmethod build-soldier (team soldier-type start destination (new-gm game))
  (let ((player (copy-structure (if (equalp team (player-team (game-player1 new-gm)))
                                    (game-player1 new-gm)
                                    (game-player2 new-gm)))))
    (if (< (player-money player) (cost soldier-type (game-config new-gm)))
        (left (format nil "Player ~a with money ~a doesn't have enough money for ~a with cost ~a" 
                      (player-team player) (player-money player) soldier-type (cost soldier-type (game-config new-gm))))
        (if (not (close-enough-to-base start player (game-config new-gm)))
            (left (format nil "Player ~a tried to build ~a at position ~a, too far from base ~a"
                          (player-team player) soldier-type 
                          (format-position start)
                          (format-position (player-base player))))
            (if (gethash start (game-map new-gm))
                (left (format nil "Player ~a tried to build ~a at position ~a which is occupied" 
                              (player-team player) soldier-type (format-position start)))
                (let* ((s (make-soldier :pos start
                                        :health (initial-health soldier-type (game-config new-gm))
                                        :type soldier-type
                                        :destination destination
                                        :team (player-team player))))
                  (setf (player-money player) (- (player-money player) (cost soldier-type (game-config new-gm))))
                  (setf (gethash start (game-map new-gm)) s)
                  (if (equalp team (player-team (game-player1 new-gm)))
                      (setf (game-player1 new-gm) player)
                      (setf (game-player2 new-gm) player))
                  (right new-gm)))))))

(defstruct change-destination soldier-position new-destination)

(defmethod change-soldier-destination (team soldier-position new-destination (new-gm game))
  (let ((soldier-lookup (gethash soldier-position (game-map new-gm))))
    (if (not soldier-lookup)
        (left (format nil "Player ~a tried to change destination, but position ~a is not occupied"
                      team (format-position soldier-position)))
        (match soldier-lookup
          ((type rock) (left (format nil "Player ~a tried to change destination, but position ~a has a rock, not a soldier"
                              team (format-position soldier-position))))
          ((type base) (left (format nil "Player ~a tried to change destination, but position ~a has a base, not a soldier"
                              team (format-position soldier-position))))
          ((type soldier) 
           (if (not (equal (soldier-team soldier-lookup) team))
               (left (format nil "Player ~a tried to change destination, but position ~a has an enemy soldier"
                             team (format-position soldier-position)))
               (let ((new-soldier (copy-structure soldier-lookup)))
                 (setf (soldier-destination new-soldier) new-destination)
                 (setf (gethash soldier-position (game-map new-gm)) new-soldier)
                 (right new-gm))))))))

(defmethod apply-move (team move (new-gm game))
  (match move
    ((type build) 
     (build-soldier team 
                    (build-soldier-type move) 
                    (build-start move)
                    (build-destination move) new-gm))
    ((type change-destination)
     (change-soldier-destination team
                                 (change-destination-soldier-position move)
                                 (change-destination-new-destination move)
                                 new-gm))
    (:no-op (right new-gm))))

(defun combine-results (one other)
  (match (cons one other)
         ((cons (type move-result) (type move-result))
          (make-move-result :errors (append (move-result-errors one) (move-result-errors other)) 
                             :updated-game (move-result-updated-game other)))
         ((cons (type move-result) (type right))
          (make-move-result :errors (move-result-errors one)
                            :updated-game (right-value other)))
         ((cons (type right) (type move-result))
          (make-move-result :errors (move-result-errors other)
                            :updated-game (move-result-updated-game other)))
         ((cons (type right) (type right))
          (make-move-result :errors nil :updated-game (right-value other)))))

(defmethod apply-moves (moves (new-gm game))
  (if (null moves)
      (make-move-result :errors nil :updated-game new-gm)
      (bind (((team . move) (car moves)))
        (match (apply-move team move new-gm)
           ((left left-err) 
            (combine-results (make-move-result :errors (list left-err) :updated-game new-gm)
                             (apply-moves (cdr moves) new-gm)))
           ((right right-value) 
            (combine-results (right right-value)
                             (apply-moves (cdr moves) new-gm)))))))

(defmethod game-over ((game game))
  (or (= (game-turns-remaining game) 0) 
      (= (player-health (game-player1 game)) 0)
      (= (player-health (game-player2 game)) 0)))

(defmethod determine-result ((game game))
  (if (and (= (player-health (game-player1 game)) 0)
           (> (player-health (game-player2 game)) 0)) 
      (cons :winner (player-team (game-player2 game)))
      (if (and (= (player-health (game-player2 game)) 0)
               (> (player-health (game-player1 game)) 0))
          (cons :winner (player-team (game-player1 game)))
          :draw)))

(defmethod step-game (moves (gm game))
  (let ((new-mp (copy-hash-table (game-map gm)))
        (new-gm (copy-structure gm)))
    (setf (game-map new-gm) new-mp)
    (bind ((result (apply-moves moves new-gm))
           (after-attack (progn (move-soldiers (game-map (move-result-updated-game result)) 
                                               (game-config gm))
                                (make-soldiers-attack (move-result-updated-game result)))))
      (let* ((ticked-game (copy-structure after-attack))
             (player1 (copy-structure (game-player1 ticked-game)))
             (player2 (copy-structure (game-player2 ticked-game))))
        (incf (player-money player1) (game-config-money-per-turn (game-config gm)))
        (incf (player-money player2) (game-config-money-per-turn (game-config gm)))
        (setf (game-turns-remaining ticked-game)
              (- (game-turns-remaining ticked-game) 1))
        (setf (game-player1 ticked-game) player1)
        (setf (game-player2 ticked-game) player2)
        (make-move-result :errors (move-result-errors result) :updated-game ticked-game)))))

(defmethod is-finished? ((game game)) (game-over game))

(defmethod game-result ((game game)) (determine-result game))

(defun game-to-json (player-name game)
  (let ((yason:*list-encoder* #'yason:encode-alist))
    (yason:with-output-to-string* (:stream-symbol s)
      (yason:encode (cons (cons "you" player-name) (game-alist game)) s))))

(defmethod get-players-input-for-turn ((game game))
  (list (cons (player-team (game-player1 game)) (game-to-json "player1" game))
        (cons (player-team (game-player2 game)) (game-to-json "player2" game))))

(defmethod turn-time-limit ((game game)) 1)

(defun parse-move (player-move)
  (match (cdr player-move)
    ((ppcre "BUILD (SCOUT|ASSASSIN|TANK) \\((\\d+), (\\d+)\\) \\((\\d+), (\\d+)\\)" name 
            (read start-x) (read start-y) (read dest-x) (read dest-y))     
     (right (cons (car player-move)
                  (make-build :soldier-type (intern (string-upcase name) "KEYWORD") 
                              :start (cons start-x start-y)
                              :destination (cons dest-x dest-y)))))
    ((ppcre "MOVE \\((\\d+), (\\d+)\\) TO \\((\\d+), (\\d+)\\)"
            (read position-x) (read position-y) (read dest-x) (read dest-y))
     (right (cons (car player-move)
                  (make-change-destination :soldier-position (cons position-x position-y)
                                :new-destination (cons dest-x dest-y)))))
    ((ppcre "NO-OP") (right (cons (car player-move) :no-op)))
    (nil (left (format nil "Player ~a didn't provide a move" (car player-move))))
    (otherwise (left (format nil "Player ~a provided invalid move '~a'" 
                     (car player-move) (cdr player-move))))))

(defun rights (results)
  (mapcar #'right-value (remove-if-not (lambda (r) (match r ((type right) t)
                                                          (otherwise nil))) results)))

(defun lefts (results)
  (mapcar #'left-err (remove-if-not (lambda (r) (match r ((type left) t) 
                                                       (otherwise nil))) results)))

(defun format-command (command)
  (match command
    ((type change-destination)
     (format nil "MOVE ~a TO ~a"
             (format-position (change-destination-soldier-position command))
             (format-position (change-destination-new-destination command))))
    ((type build)
     (format nil "BUILD ~a ~a ~a" 
             (build-soldier-type command) 
             (format-position (build-start command))
             (format-position (build-destination command))))
    (:no-op (format nil "NO-OP"))))

(defun format-parsed-move (parsed-move)
  (match parsed-move
    ((left left-err) left-err)
    ((right right-value) 
     (format nil "Player ~a, Move ~a" (car right-value) 
             (format-command (cdr right-value))))))

(defmethod advance-turn (player-moves (game game))
  (bind ((parsed-moves (mapcar #'parse-move player-moves))
         (valid-moves (rights parsed-moves))
         (move-result (step-game valid-moves game)))
    (make-game-turn-result :game (move-result-updated-game move-result)
                           :move-log (mapcar #'format-parsed-move parsed-moves))))

(defmethod game-state ((game game)) 
  (let ((yason:*list-encoder* #'yason:encode-alist))
    (yason:with-output-to-string* (:stream-symbol s)
      (yason:encode (game-alist game) s))))

(defmethod input-parser ((game game)) #'read-line)

(defun construct-bot-paths (bot-relative-paths &optional (current-directory nil))
  (bind (((bot-1-relative-path . bot-2-relative-path) bot-relative-paths))
    (labels ((bot-absolute-path (bot-relative-path)
               (if current-directory 
                   (merge-pathnames (cl-fad:pathname-as-directory bot-relative-path)
                                    (cl-fad:pathname-as-directory current-directory))
                   (merge-pathnames (cl-fad:pathname-as-directory bot-relative-path)))))
      (cons (bot-absolute-path bot-1-relative-path)
            (bot-absolute-path bot-2-relative-path)))))


(defun run-bots (bot-absolute-paths turns-log-stream)
  (bind (((bot1-path . bot2-path) bot-absolute-paths)
         (bot-1-def (runtime:read-bot-definition (merge-pathnames "definition.json" bot1-path)))
         (bot-2-def (runtime:read-bot-definition (merge-pathnames "definition.json" bot2-path))))
    (list (runtime:start-bot-from-definition bot-1-def (format nil "~a" bot1-path) turns-log-stream)
          (runtime:start-bot-from-definition bot-2-def (format nil "~a" bot2-path) turns-log-stream))))

(defun start-game (bot-relative-paths logging-config 
                   &optional (current-directory nil)
                     (game-config *default-game-config*))
  (format t "Running footsoldiers~%")
  (let* ((runtime:*bot-initialisation-time* 15)
         (bots (alist-hash-table 
                (pairlis '("player1" "player2") 
                         (run-bots (construct-bot-paths bot-relative-paths current-directory)
                                   (logging-config-turns logging-config)))))
         (game (make-game :map (alist-hash-table 
                                (list (cons (cons 0 0) (make-base :team "player1"))
                                      (cons (cons 20 0) (make-base :team "player2"))
                                      (cons (cons 10 0) (make-rock))
                                      (cons (cons 10 -1) (make-rock))
                                      (cons (cons 10 -2) (make-rock))
                                      (cons (cons 10 1) (make-rock))
                                      (cons (cons 10 2) (make-rock))) 
                                :test 'equal)
                          :turns-remaining 100
                          :player1 (make-player :team "player1" 
                                                :money (game-config-initial-money game-config)
                                                :base (cons 0 0)
                                                :health 40)
                          :player2 (make-player :team "player2"
                                                :money (game-config-initial-money game-config)
                                                :base (cons 20 0)
                                                :health 40)
                          :config *default-game-config*)))
    (n-player-game bots game logging-config)))

(opts:define-opts 
    (:name :help
           :description "Print this help dialogue"
           :long "help"
           :short #\h)
    (:name :bot-dir-1
           :description "The directory where the first bot can be found"
           :long "bot-dir-1"
           :arg-parser #'identity
           :meta-var "DIR")
    (:name :bot-dir-2
           :description "The directory where the second bot can be found"
           :long "bot-dir-2"
           :arg-parser #'identity
           :meta-var "DIR"))

(defun run-footsoldiers ()
  (handler-case 
      (multiple-value-bind (options free-args) (opts:get-opts)
        (declare (ignore free-args))
       (let ((bot-1-relative-path (or (getf options :bot-dir-1) "~/bot1/"))
             (bot-2-relative-path (or (getf options :bot-dir-2) "~/bot2/")))
         (if (getf options :help)
             (opts:describe 
              :prefix "Footsoldiers game runner"
              :suffix "Hope you enjoy!"
              :usage-of "footsoldiers-runner"
              :args     "[FREE-ARGS]")
             (progn 
               (start-game (cons bot-1-relative-path bot-2-relative-path) 
                           (make-logging-config :turns *standard-output*
                                                :moves *standard-output*
                                                :states *standard-output*))))))
    (sb-sys:interactive-interrupt () (progn (format t "User interrupt. Exiting.~%") (sb-ext:exit :code 0)))
    (error (e) (progn (format t "Error occurred: ~%~a~%" e) (sb-ext:exit :code 1)))))
