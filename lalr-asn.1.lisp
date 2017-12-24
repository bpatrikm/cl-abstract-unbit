;;Copyright 2017 Patrik Magnusson

;;This file is part of cl-abstract-unbit.

;;cl-abstract-unbit is free software: you can redistribute it and/or modify
;;it under the terms of the GNU General Public License as published by
;;the Free Software Foundation, either version 3 of the License, or
;;(at your option) any later version.

;;cl-abstract-unbit is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.

;;You should have received a copy of the GNU General Public License
;;along with cl-abstract-unbit.  If not, see <http://www.gnu.org/licenses/>.

(in-package :lalr)

(defmethod present ((list list) out)
  (dolist (element list)
    (present element out)))

(defclass rule ()
  ((non-terminal :initarg :non-terminal :reader non-terminal)
   (production :initarg :production :reader production)
   (prio :initform 0 :initarg :prio :reader prio);transferred to items during maching building in case of conflict at reduction (of this rule). shifts counts as prio 0 in the comparison
   (collator :initform nil :initarg :collator :reader collator)));function used for parse-tree collation. Takes a list as an argument, the car being rule and the cdr a list of collated (sub-)parse-trees corresponding to each symbol in the production (the tree returned by parse-file/run-state-machine is collated by backwards recursion), and returns an object which contains whatever information therein deemed to be of interest. If it is missing, a nil-returning function is effectively used instead. Sub-parse-trees corresponding to terminals are always returned as they are, (cons type value), but can be treated by higher-level collation.

(defun nullable-non-terminals (rules found-nullable)
  (flet ((nullable-production (production)
           (every (lambda (symbol);;true for empy productions
                    (find symbol found-nullable))
                  production)))
    (let ((next-found-nullable
           (remove-duplicates
            (mapcar #'non-terminal
                    (remove-if-not (lambda (rule)
                                     (when (nullable-production (production rule))
                                       (non-terminal rule)))
                                   rules)))))
      (if (eql (length next-found-nullable) (length found-nullable))
          next-found-nullable
          (nullable-non-terminals rules next-found-nullable)))))

(defun immediate-first-set (non-terminal rules non-terminals nullable-non-terminals)
  ;;terminaler som inleder produktion, när nollbara tagits bort från denna
  (flet ((rule-first (rule)
           (let ((symbol-after-nullables
                  (do ((sub-production (production rule) (cdr sub-production)))
                      ((or (not sub-production)
                           (not (find (car sub-production) nullable-non-terminals)))
                       (when sub-production
                         (car sub-production))))))
             (when (and symbol-after-nullables
                        (not (find symbol-after-nullables non-terminals)))
               symbol-after-nullables))))
    (remove-duplicates
     (remove nil
             (mapcar #'rule-first
                     (remove-if-not (lambda (rule)
                                      (eql (non-terminal rule) non-terminal))
                                    rules))))))

(defun first-set-reachable (non-terminal rules non-terminals nullable-non-terminals found-reachable)
  ;;icke-terminaler (inklusive de nollbara) som inleder produktion efter att nollbara kan ha tagits bort
  (flet ((rule-reachables (rule)
           (do ((sub-production (production rule) (cdr sub-production))
                (pos 0 (+ 1 pos)))
               ((or (not sub-production)
                    (not (find (car sub-production) nullable-non-terminals)))
                (if (and sub-production
                         (find (car sub-production) non-terminals))
                    (subseq (production rule) 0 (+ 1 pos))
                    (when (> pos 0)
                      (subseq (production rule) 0 pos)))))))
    (let ((reachables (remove-duplicates
                       (reduce
                        #'append
                        (mapcar #'rule-reachables
                                (remove-if-not (lambda (rule)
                                                 (eql (non-terminal rule)
                                                      non-terminal))
                                               rules))))))
      (do ((found-reachable (union reachables found-reachable)
                            (first-set-reachable (car new-reachables)
                                                 rules
                                                 non-terminals
                                                 nullable-non-terminals
                                                 found-reachable))
           (new-reachables (set-difference reachables found-reachable)
                           (cdr new-reachables)))
          ((not new-reachables)
           found-reachable)))))

;;immediate first set of self and all reachable
(defun first-set (symbol rules non-terminals nullable-non-terminals)
  (if (find symbol non-terminals)
      (remove-duplicates
       (reduce
        #'append
        (mapcar (lambda (reachable)
                  (immediate-first-set reachable rules non-terminals nullable-non-terminals))
                (first-set-reachable symbol rules non-terminals nullable-non-terminals (list symbol)))))
      (list symbol)))

(defun first-set-sub-production (sub-production rules non-terminals nullable-non-terminals)
  ;;terminals in sub-production
  (do ((sub-production sub-production (cdr sub-production))
       (first-set nil (union
                       first-set
                       (first-set (car sub-production) rules non-terminals nullable-non-terminals))))
      ((or (not sub-production)
           (not (find (car sub-production) nullable-non-terminals)))
       (if sub-production
           (union first-set
                  (first-set (car sub-production) rules non-terminals nullable-non-terminals))
           first-set))))

(defclass item ()
  ((rule :initarg :rule :reader rule)
   (completed-p :initform nil :initarg :completed-p :reader completed-p)
   (dot-position :initform 0 :initarg :dot-position :reader dot-position);(elt (production rule) dot-position) ger vad som är bakom punkten
   (lookaheads :initform nil :accessor lookaheads)
   (prio :initform 0 :accessor prio)));see break-conflicts

(defmethod present ((item item) out)
  (if (completed-p item)
      (format out "~a ~a -> ~{~a ~}. << ~{~a ~}" (prio (rule item)) (non-terminal (rule item)) (production (rule item)) (lookaheads item))
      (format out "~a ~a -> ~{~a ~}. ~{~a ~}"
              (prio (rule item)) (non-terminal (rule item))
              (subseq (production (rule item)) 0 (dot-position item))
              (subseq (production (rule item)) (dot-position item)))))

(defun eq-item (item-1 item-2)
  (and (eql (rule item-1) (rule item-2))
       (or (and (completed-p item-1)
                (completed-p item-2))
           (and (not (completed-p item-1))
                (not (completed-p item-2))
                (eql (dot-position item-1) (dot-position item-2))))))

(defun next-symbols (items)
  (remove nil
          (remove-duplicates
           (mapcar (lambda (item)
                     (unless (or (completed-p item)
                                 (not (production (rule item))))
                       (elt (production (rule item)) (dot-position item))))
                   items))))

(defun fresh-items-from-symbol (symbol rules);;symbol may be terminal, giving a nil result
  (let ((matching-rules
         (remove-if-not (lambda (rule)
                          (eql (non-terminal rule) symbol))
                        rules)))
    (mapcar (lambda (rule)
              (make-instance 'item :rule rule :completed-p (not (production rule))))
            matching-rules)))
          
(defun close-item-set (items rules)
  (let ((next-items
         (remove-duplicates
          (append
           items
           (reduce #'append (mapcar (lambda (symbol)
                                      (fresh-items-from-symbol symbol rules))
                                    (next-symbols items))))
          :test #'eq-item)))
    (if (eql (length items) (length next-items))
        next-items
        (close-item-set next-items rules))))

(defun new-open-item-set (transition-symbol closed-item-set)
  (mapcar (lambda (item)
            (if (eql (length (production (rule item))) (+ 1 (dot-position item)))
                (make-instance 'item
                               :rule (rule item)
                               :completed-p t)
                (make-instance 'item
                               :rule (rule item)
                               :dot-position (+ 1 (dot-position item)))))
          (remove-if-not (lambda (item)
                           (and (not (completed-p item))
                                (production (rule item))
                                (eql transition-symbol (elt (production (rule item)) (dot-position item)))))
                         closed-item-set)))
(defparameter *lr0-id* nil)
(defclass lr0-state ()
  ((id :initform (incf *lr0-id*) :reader id)
   (item-set :initarg :item-set :reader item-set)
   (transitions :accessor transitions :initform nil)));lista av (cons transition-symbol target-state)

(defun conflicted-state-p (state)
  (and (find-if #'completed-p (item-set state))
       (> (length (item-set state)) 1)))

(defmethod present ((state lr0-state) out)
  (if (conflicted-state-p state)
    (format out "~&Conflicted state: ~a~&" (id state))
    (format out "~&State: ~a~&" (id state)))
  (dolist (item (item-set state))
    (present item out)
    (format out "~&"))
  (format out "~%")
  (dolist (transition (transitions state))
    (format out "~a => ~a~&" (car transition) (id (cdr transition))))
  (format out "-------------~%"))

(defun eq-state (state-1 state-2)
  (not (or (set-difference (item-set state-1) (item-set state-2) :test #'eq-item)
           (set-difference (item-set state-2) (item-set state-1) :test #'eq-item))))

;;två rekursiva funktioner, add-states-from-transitions är den inre, den lägger till nya tillstånd från övergångssymboler, build-states rekurserar över tillstånd (och är den som behandlar new-closed-state till done-closed-state

(defun add-states-from-transitions (nexts current-closed-state done-closed-states rules new-closed-states)
  (let ((transition-symbol (car nexts))
        (nexts (cdr nexts)))
    (let ((new-closed-state (make-instance 'lr0-state
                                           :item-set
                                           (close-item-set
                                            (new-open-item-set transition-symbol (item-set current-closed-state))
                                            rules))))
      (let ((existing-matching-state (find new-closed-state (append done-closed-states new-closed-states) :test #'eq-state)))
        (let ((transition-target (or existing-matching-state new-closed-state)))
          (push (cons transition-symbol transition-target) (transitions current-closed-state)))
        (let ((new-closed-states
               (if existing-matching-state new-closed-states (cons new-closed-state new-closed-states))))
          (if (not nexts)
              new-closed-states
              (add-states-from-transitions nexts current-closed-state done-closed-states rules new-closed-states)))))))

;;returvärde är en ny (utökad) done-closed-states
(defun build-states (remaining-closed-states rules done-closed-states)
  (let ((nexts (next-symbols (item-set (car remaining-closed-states))))
        (done-closed-states (cons (car remaining-closed-states) done-closed-states))
        (current-closed-state (car remaining-closed-states)))
    ;;varje next ger en potentiellt ny state, men det för efter slutningen som man kan avgöra hur många nya unika det är. och fler än en next kan då peka mot samma ny eller gammal state
    (let ((remaining-closed-states (if nexts
                                       (append
                                        (cdr remaining-closed-states)
                                        (add-states-from-transitions nexts current-closed-state done-closed-states rules nil))
                                       (cdr remaining-closed-states))))
      (if (not remaining-closed-states)
          done-closed-states
          (build-states remaining-closed-states rules done-closed-states)))))

;;excluding the case where non-terminal is the start-non-terminal
(defun state-immediate-follow-set (non-terminal state rules non-terminals nullable-non-terminals)
  (remove
   nil
   (reduce
    #'union
    (mapcar (lambda (item)
              (first-set-sub-production (subseq (production (rule item)) (+ 1 (dot-position item)))
                                        rules non-terminals nullable-non-terminals))
            ;;items in state with non-terminal just after dot, with symbols following non-terminal
            (remove-if-not (lambda (item)
                             (and (not (completed-p item))
                                  (production (rule item))
                                  (eql (elt (production (rule item)) (dot-position item)) non-terminal)
                                  (< (+ 1 (dot-position item)) (length (production (rule item))))))
                           (item-set state)))
    :initial-value nil)));reduce

;;if there is an item in a given state, we want to find the possible preceding states where the corresponding item was fresh (dot all the way to the left)
;;current is (cons item state), result is list of state
(defun starting-states (current states);;loops are not possible, since dot-position is being strictly reduced and rule never changes
  ;;if current item is fresh, we have reached end of recursion
  (if (or (not (production (rule (car current))));the null productions are in the starting state while complete
          (and (not (completed-p (car current)))
               (eql 0 (dot-position (car current)))))
      (list (cdr current))
      ;;which states can reach this one
      (let ((earlier-states
             (remove-if-not (lambda (state)
                              (find-if (lambda (transition)
                                         (eql (cdr current) (cdr transition)))
                                       (transitions state)))
                            states)))
        (reduce
         #'append
         (mapcar (lambda (earlier-state)
                   ;;in each state, find the item which has same rule, one dot-position less (the specific item which gives the transition we are reversing)
                   (let ((earlier (cons (find-if (lambda (item)
                                                   (and (not (completed-p item))
                                                        (eql (rule item) (rule (car current)))
                                                        (if (completed-p (car current))
                                                            (eql (+ 1 (dot-position item)) (length (production (rule item))))
                                                            (eql (+ 1 (dot-position item)) (dot-position (car current))))))
                                                 (item-set earlier-state))
                                        earlier-state)))
                     (when (car earlier)
                       (starting-states earlier states))))
                 earlier-states)))))

;;elements of remaining-/done-reachable are pairs of non-terminal and state
(defun state-follow-reachable (non-terminal state states nullable-non-terminals &optional remaining-reachable done-reachable)
  ;;in each recursive step, we look in the current state for all items where the current non-terminal is placed just after the dot, having only nullable non-terminals after that. for each such item, and for each starting state of that item, we have a propagation step to (non-terminal (rule item)) in that state (it is backwards reachable), then we recurse over that non-terminal and state
  (let ((reachable
         (reduce
          (lambda (list-1 list-2) (union list-1 list-2 :test #'equal))
          (remove
           nil
           (mapcar (lambda (item)
                     (mapcar (lambda (reachable-state)
                               (cons (non-terminal (rule item)) reachable-state))
                             (starting-states (cons item state) states)))
                   (remove-if-not (lambda (item);;items in this state containing non-terminals as described
                                    (and (not (completed-p item))
                                         (production (rule item))
                                         (eql non-terminal
                                              (elt (production (rule item))
                                                   (dot-position item)))
                                         (every (lambda (symbol);;also true if the subseq is empty
                                                  (find symbol nullable-non-terminals))
                                                (subseq (production (rule item)) (+ 1 (dot-position item))))))
                                  (item-set state))))
          :initial-value nil)))
    (let ((remaining-reachable (append remaining-reachable
                                      (set-difference reachable (append remaining-reachable done-reachable) :test #'equal)))
          (done-reachable (cons (cons non-terminal state) done-reachable)))
      (if (not remaining-reachable)
          done-reachable
          (state-follow-reachable (caar remaining-reachable) (cdar remaining-reachable)
                                  states nullable-non-terminals
                                  (cdr remaining-reachable) done-reachable)))))

(defun state-follow (non-terminal state rules states non-terminals nullable-non-terminals)
  (remove-duplicates
   (reduce
    #'append
    (mapcar (lambda (reachable)
              (state-immediate-follow-set (car reachable) (cdr reachable) rules non-terminals nullable-non-terminals))
            (state-follow-reachable non-terminal state states nullable-non-terminals)))))

(defun import-asn.1-grammer (asn.1-grammer)
  (mapcar (lambda (row)
            (destructuring-bind (rule-desc &optional collator prio) row
              (make-instance 'rule :non-terminal (car rule-desc) :production (cdr rule-desc) :prio (if prio prio 0) :collator (when collator (eval collator)))))
          asn.1-grammer))

;;prio appliceras när det finns en annars olösbar konflikt vid reduktion av regel. shift räknas som prio 0
(defun break-conflict (conflicted-state states rules)
  ;;for every conflicted state, for every completed item in that state, find the follow set by taking the union of state-follow of the (non-terminal (rule item)) in every state where that item is fresh (dot-position=0/completed-p=nil) (and from which this current state is reachable through transitions)
  (let ((non-terminals (remove-duplicates (mapcar #'non-terminal rules)))
        (nullable-non-terminals (nullable-non-terminals rules nil)))
    (flet ((state-follow (non-terminal starting-state)
             (state-follow non-terminal starting-state
                           rules states non-terminals nullable-non-terminals)))
      (let ((conflicted-items (remove-if-not #'completed-p (item-set conflicted-state))))
        (dolist (conflicted-item conflicted-items)
          (setf (lookaheads conflicted-item)
                (remove-duplicates
                 (reduce
                  #'append
                  (mapcar (lambda (starting-state)
                            (state-follow (non-terminal (rule conflicted-item)) starting-state))
                          (starting-states (cons conflicted-item conflicted-state) states))))))
        (when (not (every #'lookaheads conflicted-items))
          (present conflicted-state t)
          (error "did not find lookaheads at all"))
        ;;check each conflicted item against transitions and other conflicted items
        (let ((transition-symbols (mapcar #'car (transitions conflicted-state))))
          (dolist (conflicted-item conflicted-items)
            (when (intersection (lookaheads conflicted-item) transition-symbols)
              (unless (and (prio (rule conflicted-item))
                           (not (eql 0 (prio (rule conflicted-item)))))
                (present conflicted-state t)
                (error "still shift-reduce error"))
              (setf (prio conflicted-item)
                    (prio (rule conflicted-item))))))
        (when (some
               #'identity
               (maplist (lambda (conflicted-items-subset);;pairwise check for overlap of lookaheads among reductions
                          (some (lambda (second-conflicted)
                                  (when (intersection (lookaheads (car conflicted-items-subset))
                                                      (lookaheads second-conflicted))
                                    (when (prio (rule (car conflicted-items-subset)))
                                      (setf (prio (car conflicted-items-subset)) (prio (rule (car conflicted-items-subset)))))
                                    (when (prio (rule second-conflicted))
                                      (setf (prio second-conflicted) (prio (rule second-conflicted))))
                                    (eql (prio (car conflicted-items-subset)) (prio second-conflicted))))
                                (cdr conflicted-items-subset)))
                        conflicted-items))
          (error "still reduce-reduce conflict"))))))

(defun break-conflicts (states rules)
  (dolist (conflicted-state (remove-if-not #'conflicted-state-p states))
    (break-conflict conflicted-state states rules)))

(defun present-conflict (conflicted-state states rules out)
  (let ((non-terminals (remove-duplicates (mapcar #'non-terminal rules)))
        (nullable-non-terminals (nullable-non-terminals rules nil)))
    (dolist (conflicted-item (remove-if-not #'completed-p (item-set conflicted-state)))
      (dolist (starting-state (starting-states (cons conflicted-item conflicted-state) states))
        (mapcar (lambda (reachable)
                  (format out "Now looking at the non-terminal ~a in the state:~&" (car reachable))
                  (present (cdr reachable) out)
                  (format out "~&the follow set contribution is:~&")
                  (format out "~a~&"
                          (state-immediate-follow-set (car reachable) (cdr reachable) rules non-terminals nullable-non-terminals)))
                (state-follow-reachable (non-terminal (rule conflicted-item)) starting-state states nullable-non-terminals))))))

(defun filter-rules (non-terminal rules)
  (remove-if-not (lambda (rule)
                   (eql non-terminal (non-terminal rule)))
                 rules))

;;presentera de tillstånd en viss regel har reduktioner i

(defun present-rule-items (rule states out)
  (dolist (state (remove-if-not (lambda (state)
                                  (find-if (lambda (item)
                                             (and (completed-p item)
                                                  (eql rule (rule item))))
                                           (item-set state)))
                                states))
    (present state out)))

(defun id-to-state (id states)
  (find id states :key #'id))
                  
(defun build-state-machine (rules start-non-terminal)
  (let ((*lr0-id* 0))
    (let ((starting-closed-state
           (let ((added-rule (make-instance 'rule :non-terminal (cons :augmented-start (gensym)) :production (list start-non-terminal))))
             (make-instance 'lr0-state
                            :item-set
                            (close-item-set (list (make-instance 'item :rule added-rule)) rules)))))
      (let ((states (build-states (list starting-closed-state) rules nil)))
        (break-conflicts states rules)
        states))))

(defun build-lr0-state-machine (rules start-non-terminal)
  (let ((*lr0-id* 0))
    (let ((starting-closed-state
           (let ((added-rule (make-instance 'rule :non-terminal (cons :augmented-start (gensym)) :production (list start-non-terminal))))
             (make-instance 'lr0-state
                            :item-set
                            (close-item-set (list (make-instance 'item :rule added-rule)) rules)))))
      (let ((states (build-states (list starting-closed-state) rules nil)))
        ;;(break-conflicts states rules)
        states))))

;;lookahead is (cons symbol value)
;;entry of parse-stack is list, where car is rule and cdr are values, or (cons symbol value). (production rule) and values are the semantics
(defun run-state-machine (state-stack parse-stack lookahead lexer)
  (if (equal lookahead '(nil))
      (let ((start-rule (rule (car (item-set (car state-stack))))))
        (cons start-rule (nreverse parse-stack)))
      (let ((initial-state (car state-stack)))
        ;;;(format t "~%New recursive step, lookahead is ~a~&" (car lookahead))
        ;;;(present initial-state t)
        ;;dessa situationer är 'normala'
        ;;1 reduce-fall 0 shift-fall
        ;;>1 reduce-fall 0 shift-fall, bara en matchar lookaheads
        ;;>1 reduce-fall 0 shift-fall, flera matchar lookaheads, en har högre prio än andra
        ;;>0 shift-fall 0 reduce-fall
        ;;>0 shift-fall >0 reduce-fall, inget reduce-fall matchar lookaheads
        ;;>0 shift-fall >0 reduce-fall, inget reduce-fall matchar lookaheads och har prio>0
        ;;>0 shift-fall 1 reduce-fall, reduce-fallet matchar lookaheads men inget av shift-fallen
        ;;>0 shift-fall 1 reduce-fall, reduce-fallet och 1 shift-fall matchar lookahead, reduce-fallet har prio>0
        ;;>0 shift-fall 1 reduce-fall, reduce-fallet och 1 shift-fall matchar lookahead, reduce-fallet har prio<0
        ;;>0 shift-fall >0 reduce-fall, >0 reduce-fall och 1 shift-fall matchar lookahead, ett reduce-fall har unikt högst prio>0
        ;;>0 shift-fall >0 reduce-fall, >0 reduce-fall matchar lookahead men inte något shift-fall, ett reduce-fall har unikt högst prio>0
        
        ;;titta först på reduce-fallen, beskär dem genom att titta på först på lookaheads och sedan prio så länge de är fler än ett, lookaheads kan ge noll och pga delat högst prio kan de fortfarande vara flera än ett
        ;;om det också finns shift-fall, beskär reduce-fallen genom att jämföra lookaheads och prio högst över noll (raden ovan har inte gjort det ifall det var få nog fall där)
        ;;om det nu finns exakt ett reduce-fall kvar, välj det, annars välj shift-fall genom transition-symbol
    
        ;;0 reduce och 0 shift är också normalt, i den meningen att felet kan ligga i strömmen och inte maskinen

        ;;kan jag filtrera bort icke-matchande shifts redan i början? då triggas kanske inte andra raden, kanske godtas ett fel i strömmen som lookaheads hade förhindrat - men det är alltid risken i de fall som inte hade konflikter vid maskin-bygget, eftersom inget läses från strömmen vid reduktion har inget godtagits från den och felet triggas ändå

        (let* ((shifts (remove-if-not (lambda (transition)
                                        (eql (car lookahead) (car transition)))
                                      (transitions initial-state)))
               (reductions
                (let ((reductions (remove-if-not #'completed-p (item-set initial-state))))
                  (let ((reductions (if (< (length reductions) 2) reductions
                                        (remove-if-not (lambda (reduction)
                                                         (find (car lookahead) (lookaheads reduction)))
                                                       reductions))))
                    (let ((reductions
                           (if (< (length reductions) 2) reductions
                               (let ((high-prio (car (sort (mapcar #'prio reductions) #'>))))
                                 (remove-if-not (lambda (reduction) (eql high-prio (prio reduction))) reductions)))))
                      (if (not shifts) reductions ;inga shift-fall
                          (let ((reductions
                                 (remove-if-not (lambda (reduction)
                                                  (find (car lookahead) (lookaheads reduction)))
                                                reductions)))
                            (when reductions
                              (let ((high-prio (car (sort (mapcar #'prio reductions) #'>))))
                                (when (> high-prio 0)
                                  (remove-if-not (lambda (reduction) (eql high-prio (prio reduction))) reductions)))))))))))
          (when (> (length reductions) 1) (error "reduce-reduce"))
          (let ((reduction (when reductions (car reductions))))
            (if reduction
                (progn
                  ;;pop the state-/parse-stack corresponding to the rule of reduction
                  ;;push the new parse-entry back
                  ;;om (length production) är 0, tas inget från parse-stack, men något läggs till - likaså kan något i production var null, och då är längden också fel
                  (let ((plength (length (production (rule reduction)))))
                    ;;;(format t "popping ~a states~&" plength)
                    (let ((parse-entry (cons (rule reduction) (nreverse (copy-seq (subseq parse-stack 0 plength))))))
                      (setf state-stack (subseq state-stack plength))
                      (setf parse-stack (subseq parse-stack plength))
                      (push parse-entry parse-stack)))
                  ;;lookup goto in newly exposed top of state-stack
                  (let ((transitions (remove-if-not (lambda (transition)
                                                      (eql (non-terminal (rule reduction)) (car transition)))
                                                    (transitions (car state-stack)))))
                    (unless (eql 1 (length transitions))
                      (error "goto error"))
                    (push (cdar transitions) state-stack)))
                ;;shift
                (progn
                  ;;;(format t "shifting ~a~&" (car lookahead))
                  (unless (eql 1 (length shifts))
                    (error "shift error"))
                  (push (cdar shifts) state-stack)
                  (push lookahead parse-stack)
                  (multiple-value-bind (symbol value) (funcall lexer)
                    (setf lookahead (cons symbol value)))))
            (run-state-machine state-stack parse-stack lookahead lexer))))))

;;the state maching simply collects the parse into a tree. Each rule has a collation-function. These are executed bottom-up by collate-parse
