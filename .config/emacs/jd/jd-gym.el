;;; jd-gym.el ---  Gym workout manager.
;;; Commentary:
;; It will be a gym workout manager in the future.
;;; code:

(require 'emacsql-sqlite)

(defconst jd-gym/db--tables-schema
  '((workout-plans
     [(id integer :primary-key :autoincrement)
      (name text)])

    (workouts
     [(id integer :primary-key :autoincrement)
      (workout-plan-id integer :not-null)
      (day integer :not-null)
      (month integer :not-null)
      (year integer :not-null)]
     (:foreign-key [workout-plan-id] :references workout-plans [id] :on-delete :cascade))

    (muscle-part
     [(id integer :primary-key :autoincrement)
      (name text :not-null)])

    (exercises
     [(id integer :primary-key :autoincrement)
      (name text :not-null)
      (muscle-part-id integer :not-null)]
     (:foreign-key [muscle-part-id] :references muscle-part [id] :on-delete :cascade))
    
    (workout-plan-exercise-map
     [(id integer :primary-key :autoincrement)
      (week-day integer :not-null)
      (workout-plan-id integer :not-null)
      (exercise-id integer :not-null)]
     (:foreign-key [workout-plan-id] :references workout-plans [id] :on-delete :cascade)
     (:foreign-key [exercise-id] :references exercises [id] :on-delete :cascade))

    (workout-sets
     [(id integer :primary-key :autoincrement)
      (reps integer :not-null)
      (weight integer :not-null)
      (workout-plan-exercise-map-id integer :not-null)
      (comment text)]
     (:foreign-key [workout-plan-exercise-map-id] :references workout-plan-exercise-map [id] :on-delete :cascade))))

(defconst jd-gym/db--test-data
  '((workout-plans
     ([nil "Push pull"]))
    (workouts
     ([nil 1 3 1 2023]))
    (muscle-part
     ([nil "glutes"]
      [nil "chest"]))
    (exercises
     ([nil "Bench press" 2]
      [nil "RDL" 1]))
    (workout-plan-exercise-map
     ([nil 0 1 1] ; Monday, Push pull, Bench press
      [nil 1 1 2] ; Tuesday, Push pull, RDL
      [nil 3 1 1] ; Thursday, Push pull, Bench press
      [nil 4 1 2] ; Friday, Push pull, RDL
      ))
    (workout-sets
     ([nil 7 70 1 "New PR!"]
      [nil 8 110 2 "It was hard!"]))))

(defun jd-gym/db--test-data-insert ()
  "Insert test data to jd-gym database."
  (mapc (lambda (table)
	  (let ((table-name (car table))
		(table-data (cdr table)))
	    (emacsql jd-gym/db--conn [:insert :into $i1
				      :values $v2]
		     table-name table-data)))
	jd-gym/db--test-data))

(defvar jd-gym/db-path "~/Documents/Gym/gym.sqlite"
  "Path can be relative or absolute.")

(defvar jd-gym/db--conn nil
  "Store connection to jd-gym database.")

(defun jd-gym/db--conn-p ()
  "Check if jd-gym is connected to db."
  (and (emacsql-sqlite-connection-p jd-gym/db--conn)
       (emacsql-live-p jd-gym/db--conn)))

(defun jd-gym/db--connect ()
  "Connect to db if there is no connection yet."
  (unless (jd-gym/db--conn-p)
    (setq jd-gym/db--conn (emacsql-sqlite jd-gym/db-path))))

(defun jd-gym/db--close ()
  "Close db connection."
  (when (jd-gym/db--conn-p)
    (emacsql-close jd-gym/db--conn)))

(defun jd-gym/db--init ()
  "Initialize database structure."
  (when (jd-gym/db--conn-p)
    (emacsql jd-gym/db--conn [:pragma (= foreign_keys ON)])
    (emacsql jd-gym/db--conn "PRAGMA foreign_keys=ON")
    (mapc (lambda (table)
	    (let ((table-name (car table))
		  (table-schema (cdr table)))
	      (emacsql jd-gym/db--conn [:create-table $i1 $S2] table-name table-schema)))
	  jd-gym/db--tables-schema)))

(defun jd-gym/db ()
  "Entrypoint to jd-gym db."
  (unless (file-exists-p jd-gym/db-path)
    (jd-gym/db--close)
    (jd-gym/db--connect) ; Restart connection
    (jd-gym/db--init)))


(provide 'jd-gym)

;;; jd-gym.el ends here
