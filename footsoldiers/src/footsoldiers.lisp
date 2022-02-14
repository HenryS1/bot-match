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
           :money-per-turn
           :construct-bot-paths
           :*default-health-config*
           :*default-speed-config*
           :*default-damage-config*
           :*default-cost-config*
           :map-from-lines
           :map-from-file
           :make-map-details
           :map-details-map
           :bot-memory-limit-kib))

(in-package :footsoldiers)

(defstruct rock)

(defun rock-alist (coord r)
  (declare (ignore r))
  (list (cons "position" (coord-alist coord))
        (cons "type" "Rock")))

(defstruct soldier pos health type team destination)

(defun coord-alist (coord)
  (list (cons "x" (car coord))
        (cons "y" (cdr coord))))

(defun soldier-alist (soldier)
  (list
   (cons "position" (coord-alist (soldier-pos soldier)))
   (cons "health" (soldier-health soldier))
   (cons "type" (format nil "~@(~a~)" (soldier-type soldier)))
   (cons "team" (soldier-team soldier))
   (cons "destination" (coord-alist (soldier-destination soldier)))))

(defstruct base team)

(defun base-alist (position base)
  (list
   (cons "type" "Base")
   (cons "position" (coord-alist position))
   (cons "team" (base-team base))))

(defun compare-map-entries (turns-remaining)
  (lambda (one other)
    (match (cdr one)
      ((type rock) t)
      ((type soldier)
       (match (cdr other) 
         ((type rock) nil)
         ((type soldier) 
          (if (equal (soldier-team (cdr one)) (soldier-team (cdr other)))
              (pair-less (car one) (car other))
              (if (= (mod turns-remaining 2) 0)
                  (string< (soldier-team (cdr one)) (soldier-team (cdr other)))
                  (string> (soldier-team (cdr one)) (soldier-team (cdr other)))))))))))

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

(define-json-model speed-config (scout assassin tank) :kebab-case)
(define-json-model damage-config (scout assassin tank) :kebab-case)
(define-json-model cost-config (scout assassin tank) :kebab-case)
(define-json-model health-config (scout assassin tank) :kebab-case)

(define-json-model game-config 
    ((initial-money)
     (money-per-turn)
     (max-distance-from-base)
     (total-turns)
     (bot-memory-limit-kib)
     (allowed-commands)
     (health health-config)
     (speed-config speed-config "speed")
     (damage damage-config)
     (cost cost-config))
  :kebab-case)

(defparameter *soldier-types* '(:scout :tank :assassin))
(defparameter *default-health-config* 
  (make-instance 'health-config :scout 6 
                 :assassin 6 
                 :tank 6))
(defparameter *default-speed-config*
  (make-instance 'speed-config 
                 :scout 3 
                 :assassin 5 
                 :tank 2))
(defparameter *default-damage-config* 
  (make-instance 'damage-config 
                 :scout 2 
                 :assassin 5 
                 :tank 2))
(defparameter *default-cost-config*
  (make-instance 'cost-config 
                 :scout 10 
                 :tank 12 
                 :assassin 14))
(defparameter *default-game-config*
  (make-instance 'game-config 
   :initial-money 10
   :money-per-turn 3 
   :total-turns 100
   :allowed-commands (alist-hash-table 
                      (list (cons "lisp-ros" "ros +Q -- <bot-file>"))
                      :test 'equal)
   :bot-memory-limit-kib 2000000
   :max-distance-from-base 5 
   :health *default-health-config*
   :speed-config *default-speed-config*
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
  (let ((config (health game-config)))
    (case soldier-type
      (:scout (scout config))
      (:assassin (assassin config))
      (:tank (tank config)))))

(defmethod soldier-speed (soldier-type (game-config game-config))
  (let ((config (speed-config game-config)))
    (case soldier-type
      (:scout (scout config))
      (:assassin (assassin config))
      (:tank (tank config)))))

(defmethod soldier-damage (soldier-type (game-config game-config))
  (let ((config (damage game-config)))
    (case soldier-type
      (:scout (scout config))
      (:assassin (assassin config))
      (:tank (tank config)))))

(defmethod soldier-cost (soldier-type (game-config game-config))
  (let ((config (cost game-config)))
    (case soldier-type
      (:scout (scout config))
      (:assassin (assassin config))
      (:tank (tank config)))))

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

(defun make-queue () (cons nil nil))

(defun add-to-front (e q) (cons (cons e (car q)) (cdr q)))

(defun remove-from-back (q) 
  (if (not (null (cdr q)))
      (values (cadr q) (cons (car q) (cddr q)))
      (let ((reversed-front (reverse (car q))))
        (values (car reversed-front) (cons nil (cdr reversed-front))))))

(defun add-all (es q)
  (cons (append (reverse es) (car q)) (cdr q)))

(defun reachable-positions (range p1 mp)
  (let ((seen (make-hash-table :test 'equal)))
    (setf (gethash p1 seen) 0)
    (labels ((rec (q)
               (bind (((:values current next-q) (remove-from-back q)))
                 (when current 
                   (let* ((distance-to (gethash current seen)))
                     (when (< distance-to range)
                      (let ((nbrs (remove-if (lambda (nbr) (gethash nbr seen)) 
                                             (neighbours current mp))))
                        
                        (mapc (lambda (nbr) (setf (gethash nbr seen) (+ 1 distance-to))) nbrs)
                        (rec (add-all nbrs next-q)))))))))
      (rec (add-to-front p1 (make-queue)))
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

(defun move-soldiers (mp game-config turns-remaining)
  (let ((es (sort (hash-table-alist mp) (compare-map-entries turns-remaining))))
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
  (let ((new-health (max 0 (- (soldier-health s2) (soldier-damage (soldier-type s1) config)))))
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
                                               (soldier-damage (soldier-type s) (game-config gm)))))
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
  (let ((entries (sort (hash-table-alist (game-map gm)) 
                       (compare-map-entries (game-turns-remaining gm)))))
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
      (max-distance-from-base game-config)))

(defun format-position (pos)
  (format nil "(~a, ~a)" (car pos) (cdr pos)))

(defmethod build-soldier (team soldier-type start destination (new-gm game))
  (let ((player (copy-structure (if (equalp team (player-team (game-player1 new-gm)))
                                    (game-player1 new-gm)
                                    (game-player2 new-gm)))))
    (if (< (player-money player) (soldier-cost soldier-type (game-config new-gm)))
        (left (format nil "Player ~a with money ~a doesn't have enough money for ~a with cost ~a" 
                      (player-team player) (player-money player) soldier-type (soldier-cost soldier-type (game-config new-gm))))
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
                  (setf (player-money player) (- (player-money player) (soldier-cost soldier-type (game-config new-gm))))
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
      (format nil "Winner ~a" (player-team (game-player2 game)))
      (if (and (= (player-health (game-player2 game)) 0)
               (> (player-health (game-player1 game)) 0))
          (format nil "Winner ~a" (player-team (game-player1 game)))
          "Draw")))

(defmethod step-game (moves (gm game))
  (let ((new-mp (copy-hash-table (game-map gm)))
        (new-gm (copy-structure gm)))
    (setf (game-map new-gm) new-mp)
    (bind ((result (apply-moves moves new-gm))
           (after-attack (progn (move-soldiers (game-map (move-result-updated-game result)) 
                                               (game-config gm)
                                               (game-turns-remaining gm))
                                (make-soldiers-attack (move-result-updated-game result)))))
      (let* ((ticked-game (copy-structure after-attack))
             (player1 (copy-structure (game-player1 ticked-game)))
             (player2 (copy-structure (game-player2 ticked-game))))
        (incf (player-money player1) (money-per-turn (game-config gm)))
        (incf (player-money player2) (money-per-turn (game-config gm)))
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
                                    (cl-fad:pathname-as-directory (parse-namestring current-directory)))
                   (merge-pathnames (cl-fad:pathname-as-directory bot-relative-path)))))
      (cons (bot-absolute-path bot-1-relative-path)
            (bot-absolute-path bot-2-relative-path)))))

(defun run-bots (bot-absolute-paths turns-log-stream allowed-commands memory-limit)
  (bind (((bot1-path . bot2-path) bot-absolute-paths)
         (bot-1-def (runtime:read-bot-definition (merge-pathnames "definition.json" bot1-path)))
         (bot-2-def (runtime:read-bot-definition (merge-pathnames "definition.json" bot2-path))))
    (let* ((bots (list (runtime:start-bot-from-definition
                        bot-1-def
                        (format nil "~a" bot1-path) 
                        turns-log-stream
                        :allowed-commands allowed-commands
                        :memory-limit memory-limit)
                       (runtime:start-bot-from-definition
                        bot-2-def
                        (format nil "~a" bot2-path)
                        turns-log-stream
                        :allowed-commands allowed-commands
                        :memory-limit memory-limit)))
           (errors (lefts bots))
           (successes (rights bots)))
      (if (null errors)
          (right successes)
          (left errors)))))

(defstruct map-details base1 base2 map)

(defparameter *default-game-map* 
  (make-map-details :base1 (cons 0 0) :base2 (cons 20 0)
                    :map (alist-hash-table 
                          (list (cons (cons 0 0)
                                      (make-base :team "player1"))
                                (cons (cons 20 0) 
                                      (make-base :team "player2"))
                                (cons (cons 10 0) (make-rock))
                                (cons (cons 10 -1) (make-rock))
                                (cons (cons 10 -2) (make-rock))
                                (cons (cons 10 1) (make-rock))
                                (cons (cons 10 2) (make-rock))) 
                          :test 'equal)))

(defun start-game (bot-relative-paths logging-config 
                   &key (current-directory nil)
                     (game-config *default-game-config*)
                     (game-map *default-game-map*))
  (format t "Running footsoldiers~%")
  (let ((runtime:*bot-initialisation-time* 15)
        (base1-lookup (gethash (map-details-base1 game-map) (map-details-map game-map)))
        (base2-lookup (gethash (map-details-base2 game-map) (map-details-map game-map))))
    (cond ((or (not base1-lookup) (not (equalp (base-team base1-lookup) "player1")))
           (left "player1 base was not in the expected position"))
          ((or (not base2-lookup) (not (equalp (base-team base2-lookup) "player2")))
           (left "player2 base was not in the expected position"))
          (t (fmap (lambda (bs)
                     (let* ((bots (alist-hash-table (pairlis '("player1" "player2") bs)
                                                    :test 'equal))
                            (game (make-game :map (map-details-map game-map)
                                             :turns-remaining (total-turns game-config)
                                             :player1 (make-player 
                                                       :team "player1" 
                                                       :money (initial-money game-config)
                                                       :base (map-details-base1 game-map)
                                                       :health 40)
                                             :player2 (make-player 
                                                       :team "player2"
                                                       :money (initial-money game-config)
                                                       :base (map-details-base2 game-map)
                                                       :health 40)
                                             :config game-config)))
                       (n-player-game bots game logging-config))) 
                   (run-bots (construct-bot-paths bot-relative-paths current-directory)
                             (logging-config-turns logging-config)
                             (allowed-commands game-config)
                             (bot-memory-limit-kib game-config)))))))

(opts:define-opts 
    (:name :help
           :description "Print this help dialogue"
           :long "help"
           :short #\h)
    (:name :result-file
           :description "File to write the game result to"
           :long "result-file"
           :arg-parser #'identity
           :meta-var "RESULT-FILE")
    (:name :turn-logs
           :description "File to write the game turns to"
           :long "turn-logs"
           :arg-parser #'identity
           :meta-var "TURN-LOGS")
    (:name :move-logs
           :description "File to write the game moves to"
           :long "move-logs"
           :arg-parser #'identity
           :meta-var "MOVE-LOGS")
    (:name :state-logs
           :description "File to write game states to"
           :long "state-logs"
           :arg-parser #'identity
           :meta-var "STATE-LOGS")
    (:name :print-turns
           :description "Write bot turn logs to stdout"
           :long "print-turns"
           :meta-var "PRINT-TURNS")
    (:name :print-moves
           :description "Write player moves to stdout"
           :long "print-moves"
           :meta-var "PRINT-MOVES")
    (:name :print-states
           :description "Write the game state to stdout"
           :long "print-states"
           :meta-var "PRINT-STATES")
    (:name :bot-dir-1
           :description "The directory where the first bot can be found"
           :long "bot-dir-1"
           :arg-parser #'identity
           :meta-var "DIR1")
    (:name :bot-dir-2
           :description "The directory where the second bot can be found"
           :long "bot-dir-2"
           :arg-parser #'identity
           :meta-var "DIR2")
    (:name :config-file-path
           :description "The path of the game runner config file"
           :long "config-file-path"
           :arg-parser #'identity
           :meta-var "CONF-FILE")
    (:name :map-file-path
           :description "The path of the file with the game map"
           :long "map-file-path"
           :arg-parser #'identity
           :meta-var "MAP-FILE"))

(defun normalise-path (path-string)
  (merge-pathnames (parse-namestring path-string)))

(defun only-allowed-characters (lines)
  (every (lambda (line) (every (lambda (c) (member c '(#\1 #\2 #\X #\space))) line)) lines))

(defun map-from-lines (lines)
  (if (not (only-allowed-characters lines))
      (left "The provided map contains characters which are not allowed. Only '1', '2', 'X' and ' ' are allowed.")
      (let ((map (make-hash-table :test 'equal))
            base1 base2
            (base1-seen-count 0) (base2-seen-count 0))
        (loop for line in lines
           for row from 0
           do (loop for ch across line
                 for col from 0
                 if (char= ch #\1)
                 do (setf (gethash (cons col row) map) 
                          (make-base :team "player1"))
                   (setf base1 (cons col row))
                   (incf base1-seen-count)
                 else if (char= ch #\2)
                 do (setf (gethash (cons col row) map)
                          (make-base :team "player2"))
                   (setf base2 (cons col row))
                   (incf base2-seen-count)
                 else if (char= ch #\X)
                 do (setf (gethash (cons col row) map)
                          (make-rock))))
        (cond ((null base1) (left "No base for player1 was found on the map"))
              ((null base2) (left "No base for player2 was found on the map"))
              ((> base1-seen-count 1)
               (left "More than one base position was specified for player1"))
              ((> base2-seen-count 1) 
               (left "More than one base position was specified for player2"))
              (t (right (make-map-details :base1 base1 :base2 base2 :map map)))))))

(defun map-from-file (filepath)
  (let ((lines (with-open-file (f filepath)
                 (when f
                   (loop for line = (read-line f nil nil)
                      while line collect line)))))
    (map-from-lines lines)))

(defun determine-map-bounds (map)
  (loop for (x . y) being the hash-keys of map
     using (hash-value v) 
     minimizing x into x-min
     minimizing y into y-min
     maximizing x into x-max
     maximizing y into y-max
     finally (return (list x-min y-min x-max y-max))))

(defun draw-map (map)
  (bind (((x-min y-min x-max y-max) (determine-map-bounds map)))
    (with-output-to-string (s)
      (loop for r from y-min to y-max
         do (loop for c from x-min to x-max
               do (let ((entry (gethash (cons c r) map)))
                    (match entry
                      ((type soldier)
                       (case (soldier-type entry)
                         (:scout (if (string= (soldier-team entry) "player1")
                                     (format s "]")
                                     (format s "[")))
                         (:assassin (if (string= (soldier-team entry) "player1")
                                        (format s ">")
                                        (format s "<")))
                         (:tank (if (string= (soldier-team entry) "player1")
                                    (format s "}")
                                    (format s "{")))))
                      ((type base)
                       (if (string= (base-team entry) "player1")
                           (format s "1")
                           (format s "2")))
                      ((type rock)
                       (format s "X"))
                      (nil (format s " ")))))
           (format s "~%")))))

(defmethod game-visualisation ((game game))
  (if (is-finished? game)
      (format nil "~a~%" (game-result game))
      (with-output-to-string (s)
           (format s "Player 1: health ~a, money ~a~%" 
                   (player-health (game-player1 game))
                   (player-money (game-player1 game)))
           (format s "Player 2: health ~a, money ~a~%"
                   (player-health (game-player2 game))
                   (player-money (game-player2 game)))
           (format s (draw-map (game-map game))))))

(defclass wrapped-stream (sb-gray:fundamental-stream)
  ((stream :initarg :stream :reader stream-of)))

(defmethod stream-element-type ((stream wrapped-stream))
  (stream-element-type (stream-of stream)))

(defmethod close ((stream wrapped-stream) &key abort)
  (close (stream-of stream) :abort abort))

(defclass with-stdout (wrapped-stream sb-gray:fundamental-character-output-stream) ())

(defmethod sb-gray:stream-write-char ((stream with-stdout) char)
  (write-char char (stream-of stream))
  (write-char char *standard-output*))

(defmethod sb-gray:stream-write-string ((stream with-stdout)
                                string &optional (start 0) end)
  (write-string string (stream-of stream) :start start :end end)
  (write-string string *standard-output* :start start :end end))

(defun run-footsoldiers ()
  (handler-case 
      (multiple-value-bind (options free-args) (opts:get-opts)
        (declare (ignore free-args))
       (let ((bot-1-relative-path (or (getf options :bot-dir-1) "~/bot1/"))
             (bot-2-relative-path (or (getf options :bot-dir-2) "~/bot2/"))
             (config-file-path (normalise-path 
                                (or (getf options :config-file-path) "./game-config.json")))
             (result-filepath (or (getf options :result-file) "./game-result"))
             (turns-filepath (or (getf options :turn-logs) "./turn-logs"))
             (moves-filepath (or (getf options :move-logs) "./move-logs"))
             (states-filepath (or (getf options :state-logs) "./state-logs"))
             (map-filepath (or (getf options :map-file-path) "./game-map")))
         (if (getf options :help)
             (opts:describe 
              :prefix "Footsoldiers game runner"
              :suffix "Hope you enjoy!"
              :usage-of "footsoldiers-runner"
              :args     "[FREE-ARGS]")
             (bind ((config (if (and config-file-path (probe-file config-file-path))
                                (with-open-file (f config-file-path)
                                  (game-config-json:from-json f))
                                *default-game-config*))
                    (parsed-map (map-from-file map-filepath)))
               (match parsed-map 
                 ((left (left-err e)) (format t "~a" e))
                 ((right (right-value map-details))
                  (with-open-file (turns turns-filepath :if-does-not-exist 
                                         :create :if-exists :supersede :direction :output)
                    (with-open-file (moves moves-filepath :if-does-not-exist
                                           :create :if-exists :supersede :direction :output)
                      (with-open-file (states states-filepath :if-does-not-exist
                                              :create :if-exists :supersede :direction :output)
                        (match (start-game (cons bot-1-relative-path bot-2-relative-path) 
                                           (make-logging-config :turns (if (getf options :print-turns)
                                                                           (make-instance 'with-stdout :stream turns)
                                                                           turns)
                                                                :moves (if (getf options :print-moves)
                                                                           (make-instance 'with-stdout :stream moves)
                                                                           moves)
                                                                :states (if (getf options :print-states)
                                                                            (make-instance 'with-stdout :stream states)
                                                                            states)
                                                                :visualisation *standard-output*)
                                           :game-map map-details
                                           :game-config config)
                          ((left (left-err errs)) (mapc (lambda (e) (format t "~a~%" e)) errs))
                          ((right (right-value end-game)) 
                           (with-open-file (result result-filepath 
                                                   :if-does-not-exist
                                                   :create :if-exists
                                                   :supersede :direction :output)
                             (format result "~a~%" (game-result end-game))))))))))))))
    (sb-sys:interactive-interrupt () (progn (format t "User interrupt. Exiting.~%") (sb-ext:exit :code 0)))
    (error (e) (progn (format t "Error occurred: ~%~a~%" e) (sb-ext:exit :code 1)))))
