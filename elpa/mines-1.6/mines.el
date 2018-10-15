;;; mines.el --- Minesweeper game -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>
;; Created: 2017-10-28
;; Keywords: games
;; Version: 1.6
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; url: https://github.com/calancha/Minesweeper

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an elisp implementation of the classical minesweeper game.
;; The target is localize all hidden mines (bombs) in a rectangular board
;; without detonating them.  You reveal the content of a cell with the
;; command `mines-dig'.
;;
;; 1. Cells with a bomb contain the character 'x'; if you call `mines-dig'
;;    in these cells then you lost the game.

;; 2. Cells without bomb at distance 1 from any cell with a mine
;;    contain a number: the number of bombs at distance 1 from this cell.
;;    If you reveal the content of this cell, then this number is shown.
;;
;; 3. Cells without a bomb at distance > 1 from any bomb contain '@'.
;;    If you reveal the content of this cell, then '@' is shown and
;;    all adjacent cells are recursively revealed.
;;
;;
;; If you think that an uncovered cell has a mine, you might flag it
;; with `mines-flag-cell'; if you call this command again in the same
;; cell the cell is unflagged.  This is useful to visualize your
;; progress in the game.
;;
;; The game is completed once all mine-free cells are revealed, that is,
;; when the only uncovered cells equals the number of hidden mines.
;;

;;; Code:

(require 'gamegrid)
(require 'cl-lib)
(require 'cookie1) ; For `cookie-shuffle-vector'.


;;; Internal variables.
(defgroup mines nil
  "Play minessweeper."
  :group 'games
  :prefix "mines-")

(defcustom mines-protect-first-move t
  "Non-nil avoid game over in the first cell revealed."
  :type 'boolean
  :version "27.1")

(defcustom mines-mode-hook nil
  "Hook run by mines mode."
  :type 'hook
  :group 'mines
  :version "27.1")

(defvar mines-uncover-cell-char ?.
  "Char to display uncover cells.")

(defvar mines-flagged-cell-char ?!
  "Char to display flagged cells as maybe having a mine.")

(defvar mines-empty-cell-char ?@
  "Char to display a cell without mine nor numbers.")

(defvar mines-empty-cell-mine ?x
  "Char to display a cell with a mine.")

(defvar mines-buffer nil "Buffer where play minesweeper.")
(defvar mines-start-pos 2 "Initial prompt position.")
(defvar mines-number-mines 10 "Number of mines.")
(defvar mines-number-rows 8 "Nmber of rows.")
(defvar mines-number-cols 8 "Number of columns.")
(defvar mines-number-cells (* mines-number-rows mines-number-cols)
  "Number of cells.")

(defcustom mines-difficulty-level 'easy
  "Level of difficulty.
If `easy' we have 8 columns x 8 columns and 10 mines.
If `medium' we have 16 columns x 16 columns and 40 mines.
If `hard' we have 30 columns x 16 columns and 99 mines.
If `custom' then ask user for these numbers."
  :type '(choice (const :tag "Easy" easy)
                 (const :tag "Medium" medium)
                 (const :tag "Hard" hard)
                 (const :tag "Custom" custom))
  :group 'games
  :set (lambda (sym val)
         (if (not (eq val 'custom))
             (set sym val)
           (setq mines-number-cols (read-number "Number of columns: ")
                 mines-number-rows (read-number "Number of rows: ")
                 mines-number-mines (read-number "Number of mines: "))
           (set sym val)))
  :version "27.1")

(defvar mines-grid (make-vector mines-number-cells nil)
  "Game configuration.")

(defvar mines-state (make-vector mines-number-cells nil)
  "Game state.")

(defvar mines-mine-positions nil "Mine positions.")
(defvar mines-gap-positions nil "Empty cell positions.")
(defvar mines-init-time nil "Initial time of the game.")
(defvar mines-end-time nil "End time of the game.")
(defvar mines-undone-neighbours nil
  "List of uncovered neighbours for the current cell.")

(defvar-local mines-game-over nil
  "Non-nil if the game in current buffer has ended.")

(defmacro mines-init (cond1 cond2 cond3 cond4 &rest body)
  (declare (debug (form form form form &rest body)))
  `(progn
     (cond (,cond1
            (setq mines-number-cols 8
                  mines-number-rows 8
                  mines-number-mines 10
                  mines-difficulty-level 'easy))
           (,cond2
            (setq mines-number-cols 16
                  mines-number-rows 16
                  mines-number-mines 40
                  mines-difficulty-level 'medium))
           (,cond3
            (setq mines-number-cols 30
                  mines-number-rows 16
                  mines-number-mines 99
                  mines-difficulty-level 'hard))
           (,cond4 (setq mines-difficulty-level 'custom) ,@body))
     (setq mines-number-cells (* mines-number-rows mines-number-cols))))


;;; Moving.
(defun mines-index-2-matrix (idx)
  "Translate 1-D array index into 2-D matrix indices."
  (let* ((col (% idx mines-number-cols))
         (row (/ idx mines-number-cols)))
    (list row col)))

(defun mines-matrix-2-index (row col)
  "Translate 2-D matrix indices into 1-D array index."
  (+ col (* row mines-number-cols)))

(defun mines-get-neighbours (idx)
  "Return cell neighbour indices for cell at IDX."
  (let* ((row-col (mines-index-2-matrix idx))
         (row (car row-col))
         (col (cadr row-col))
         res)
    (cl-flet ((add-fn (to-row)
                      (or (= row to-row) (push (list to-row col) res))
                      (and (< col (1- mines-number-cols))
                           (push (list to-row (1+ col)) res))
                      (or (zerop col) (push (list to-row (1- col)) res))))
      (progn
        (add-fn row) ; Horizontal neighburs.
        (unless (zerop row) (add-fn (1- row))) ; Up neighbours.
        (when (< row (1- mines-number-rows)) ; Below neighbours.
          (add-fn (1+ row)))))
    (mapcar (lambda (x) (mines-matrix-2-index (car x) (cadr x))) res)))

(defun mines-goto (idx)
  "Move to cell at IDX."
  (goto-char 1)
  (let ((cidx (mines-current-pos)))
    (ignore-errors
      (while (not (= cidx idx))
        (goto-char (next-single-property-change (point) 'idx))
        (setq cidx (mines-current-pos)))
      (goto-char (1+ (point))))))

(defun mines-go-right ()
  "Move 1 cell to the right."
  (interactive)
  (if (= (point) (point-max))
      (progn
        (forward-line -1)
        (goto-char (1+ (point))))
    (let* ((idx (mines-current-pos))
           (row-col (mines-index-2-matrix idx))
           (row (car row-col))
           (col (cadr row-col)))
      (if (= col (1- mines-number-cols))
          (mines-goto (apply #'mines-matrix-2-index (list row 0)))
        (mines-goto (1+ (mines-current-pos)))))))

(defun mines-go-left ()
  "Move 1 cell to the left."
  (interactive)
  (if (= (point) (point-max))
      (goto-char (1- (point)))
    (let* ((idx (mines-current-pos))
           (row-col (mines-index-2-matrix idx))
           (row (car row-col))
           (col (cadr row-col)))
      (if (zerop col)
          (mines-goto (apply #'mines-matrix-2-index
                             (list row (1- mines-number-cols))))
        (mines-goto (1- (mines-current-pos)))))))

(defun mines-go-down ()
  "Move to the cell under the current one."
  (interactive)
  (if (= (point) (point-max))
      (goto-char mines-start-pos)
    (let* ((idx (mines-current-pos))
           (row-col (mines-index-2-matrix idx))
           (row (car row-col))
           (col (cadr row-col)))
      (if (= row (1- mines-number-rows))
          (mines-goto (apply #'mines-matrix-2-index (list 0 col)))
        (mines-goto (apply #'mines-matrix-2-index (list (1+ row) col)))))))

(defun mines-go-up ()
  "Move to the cell over the current one."
  (interactive)
  (if (= (point) (point-max))
      (progn
        (forward-line -1)
        (goto-char (1+ (point))))
    (let* ((idx (mines-current-pos))
           (row-col (mines-index-2-matrix idx))
           (row (car row-col))
           (col (cadr row-col)))
      (if (zerop row)
          (mines-goto (apply #'mines-matrix-2-index (list (1- mines-number-rows) col)))
        (mines-goto (apply #'mines-matrix-2-index (list (1- row) col)))))))


;;; Main Functions.

(defun mines--find-pos (elt vec)
  (let ((pos 0) res)
    (while (setq pos
                 (cl-position-if
                  (lambda (x)
                    (cond ((null elt)
                           ;; Check if the cell is empty or flagged.
                           (or (null x) (eq mines-flagged-cell-char x)))
                          (t (eq elt x))))
                  vec :start pos))
      (push pos res)
      (cl-incf pos))
    (nreverse res)))

(defun mines-start ()
  "Set mine positions for a new game."
  ;; Erase vector.
  (setq mines-grid (make-vector mines-number-cells nil))
  (setq mines-state (make-vector mines-number-cells nil))
  (let ((numbers (append
                  (cookie-shuffle-vector
                   (vconcat (number-sequence 0 (1- mines-number-cells)))) nil)))
    (dotimes (_ mines-number-mines)
      (aset mines-grid (pop numbers) t))
    (setq mines-mine-positions (mines--find-pos t mines-grid))))

(defun mines--near-bombs (i j)
  (let ((numb 0))
    ;; Horizontal neighbours.
    (when (> j 0)
      (and (aref mines-grid (mines-matrix-2-index i (1- j))) (cl-incf numb)))
    (when (< j (1- mines-number-cols))
      (and (aref mines-grid (mines-matrix-2-index i (1+ j))) (cl-incf numb)))
    ;; Previous row neighbours.
    (when (> i 0)
      (and (aref mines-grid (mines-matrix-2-index (1- i) j)) (cl-incf numb))
      (when (> j 0)
        (and (aref mines-grid (mines-matrix-2-index (1- i) (1- j))) (cl-incf numb)))
      (when (< j (1- mines-number-cols))
        (and (aref mines-grid (mines-matrix-2-index (1- i) (1+ j))) (cl-incf numb))))
    ;; Next row neighbours.
    (when (< i (1- mines-number-rows))
      (and (aref mines-grid (mines-matrix-2-index (1+ i) j)) (cl-incf numb))
      (when (> j 0)
        (and (aref mines-grid (mines-matrix-2-index (1+ i) (1- j))) (cl-incf numb)))
      (when (< j (1- mines-number-cols))
        (and (aref mines-grid (mines-matrix-2-index (1+ i) (1+ j))) (cl-incf numb))))
    numb))

(defun mines-set-numbers ()
  "Set numbers for cells adjacent to cells with bombs."
  (let ((tmp-grid (copy-sequence mines-grid)))
    (dotimes (i mines-number-rows)
      (dotimes (j mines-number-cols)
        (let ((idx (mines-matrix-2-index i j)))
          (unless (aref mines-grid idx)
            (let ((numb (mines--near-bombs i j)))
              (unless (zerop numb) (aset tmp-grid idx numb)))))))
    (setq mines-grid tmp-grid)))

(defun mines-list-game-conditions ()
  "Return number of rows, columns and mines for current game."
  (interactive)
  (when (mines-mines-mode-p)
    (let ((rows mines-number-rows)
          (cols mines-number-cols)
          (mines mines-number-mines))
      (message "%d rows x %d columns with %d mines"
               rows cols mines)
      (list rows cols mines))))

(defun mines--insert (elt idx &optional props null-str flag-or-unflag)
  (let* ((face nil)
         (str (cond ((null elt)
                     (if (null null-str)
                         (format " %c " mines-uncover-cell-char)
                       ;; Uncover all its uncovered neighbours.
                       (save-excursion
                         (dolist (x (mines-get-neighbours idx))
                           (mines-goto x)
                           (unless (get-text-property (point) 'done)
                             (push x mines-undone-neighbours))))
                       (format " %s " null-str)))
                    ((eq flag-or-unflag 'unflag)
                     (format " %c " mines-uncover-cell-char))
                    ((and (memq 'flag props) (eq flag-or-unflag 'flag))
                     (setq face 'warning)
                     (format " %c " mines-flagged-cell-char))
                    ((integerp elt) (format " %d " elt))
                    (t (format " %c " mines-empty-cell-mine))))
         (pos (point))
         (inhibit-read-only t))
    (if face
        (insert (propertize str 'font-lock-face face))
      (insert str))
    (when (= (cadr (mines-index-2-matrix idx)) (1- mines-number-cols))
      (backward-delete-char 1)
      (insert "\n"))
    (add-text-properties pos (point) props)
    (goto-char (1+ (point)))))

(defun mines-show ()
  "Display the board for a new game."
  (with-current-buffer (or (and (buffer-live-p mines-buffer) mines-buffer)
                           (setq mines-buffer (get-buffer-create "*Mines*")))
    (read-only-mode 1)
    (setq mines-game-over nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (unless (derived-mode-p 'mines-mode)
        (mines-mode))
      (dotimes (i mines-number-rows)
        (dotimes (j mines-number-cols)
          (let* ((idx (+ (* i mines-number-cols) j))
                 (elt (aref mines-state idx))
                 (pos (point)))
            (mines--insert elt idx)
            (put-text-property pos (point) 'idx idx)
            (when (= j (1- mines-number-cols))
              (delete-char -1)
              (insert "\n"))
            (put-text-property (1- (point)) (point) 'idx idx))))))
  (display-buffer mines-buffer '(display-buffer-same-window))
  (set-window-point (get-buffer-window mines-buffer) mines-start-pos))

(defun mines-current-pos ()
  "Return the index of the cell at point."
  (get-text-property (point) 'idx))

(defun mines--show-all ()
  "Show all mines after game over."
  (dolist (to mines-mine-positions)
    (save-excursion
      (mines-goto to)
      ;; Drop all flags before show the mines; that drop the flag faces.
      (when (eq (following-char) mines-flagged-cell-char)
        (mines--update-cell to mines-uncover-cell-char 'unflag))
      (mines-dig 'show-mines))))

(defun mines-game-over ()
  "Offer play a new game after uncover a bomb."
  (let ((inhibit-read-only t))
    (put-text-property (point) (1+ (point)) 'face 'error)
    (mines--show-all)
    (if (yes-or-no-p "Game over! Play again? ")
        (mines)
      (setq mines-game-over t))))

;; Extracted from `gamegrid-add-score-with-update-game-score'.
(defun mines--score-file (file)
  "Return full filename of score file."
  (let ((gamegrid-shared-game-dir
	     (not (zerop (logand (or (file-modes
				                  (expand-file-name "update-game-score"
						                            exec-directory))
				                 0)
			                 #o6000)))))
    (cond ((file-name-absolute-p file) file)
	      ((and gamegrid-shared-game-dir
		        (file-exists-p (expand-file-name file shared-game-score-directory)))
           (expand-file-name file shared-game-score-directory))
	      ;; Else: Use a score file in the user's home directory.
	      (t
	       (unless (file-exists-p
		            (directory-file-name gamegrid-user-score-file-directory))
	         (make-directory gamegrid-user-score-file-directory t))
           (expand-file-name file gamegrid-user-score-file-directory)))))

(defun mines--number-of-records (file)
  "Return number of records in FILE."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (count-lines (point-min) (point-max)))
    0))

(defun mines--worst-score (file)
  "Return worst score in FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (when (/= (point-min) (point-max))
        (goto-char (point-max))
        (while (and (looking-at "^$") (/= (point) (point-min)))
          (forward-line -1))
        (unless (looking-at "^$")
          (read (current-buffer)))))))

(defun mines--sort-score-file (file &optional limit)
  "Sort FILE lexicographically using the score column.
Keep LIMIT number of records in the file; default to
`gamegrid-score-file-length'.

Note that `gamegrid-add-score' assumes that the score file is a
high-score-file, i.e., getting a higher score means a better result.

Instead, in this file the score is the number of seconds to complete
the game, i.e., getting a shorter score means a better result.
After sorting, games completed with shorter times appear first."
  (when (file-exists-p file)
    (let ((buf (get-file-buffer file)))
      (with-temp-file file
        (insert
         (with-temp-buffer
           (insert-file-contents file) (buffer-string)))
        (sort-fields 1 (point-min) (point-max))
        (goto-char (point-min))
        (forward-line (or limit gamegrid-score-file-length))
        (delete-region (point) (point-max)))
      ;; If there is a buffer visiting FILE, then revert it.
      (when buf
        (with-current-buffer buf
          (revert-buffer nil 'noconfirm)
          (read-only-mode 1))))))

(defun mines-game-completed ()
  (setq mines-end-time (current-time))
  (let* ((score (time-to-seconds
                 (time-subtract mines-end-time mines-init-time)))
         (elapsed-time (format-seconds "%Y, %D, %H, %M, %z%S"
                                       score))
         (score-file (mines--score-file
                      (format "mines-rows-%d-cols-%d-mines-%d-scores"
                              mines-number-rows
                              mines-number-cols
                              mines-number-mines)))
         (worst-score (mines--worst-score score-file)))
    ;; Do not save RECORD if we already have `gamegrid-score-file-length'
    ;; records and RECORD > than the largest one.
    (when (or (/= gamegrid-score-file-length
                  (mines--number-of-records score-file))
              (not worst-score)
              (<= score worst-score))
      ;; Sort `score-file' and prepare space for a new record.
      (mines--sort-score-file score-file (1- gamegrid-score-file-length))
      ;; save score
      (gamegrid-add-score score-file score)
      ;; Sort `score-file' again and update the buffer visiting it.
      (mines--sort-score-file score-file))
    (message (format "Well done %s, you have completed it in %s!"
                     user-login-name elapsed-time))))

(defun mines-flag-cell ()
  "Flag current cell as having a mine.
If called again then unflag it."
  (interactive)
  (let* ((idx (mines-current-pos))
         (done (get-text-property (point) 'done))
         (flagged (get-text-property (point) 'flag)))
    (unless idx (user-error "Wrong position!"))
    (unless done
      (cond (flagged
             (mines--update-cell idx mines-uncover-cell-char 'unflag))
            (t (mines--update-cell idx mines-flagged-cell-char 'flag))))))

(defun mines--update-cell (idx elt &optional flag-or-unflag)
  (if (zerop idx)
      (goto-char 1)
    (goto-char (previous-single-property-change (point) 'idx)))
  (let ((to (or (next-single-property-change (point) 'idx) (point-max)))
        (prop (append (text-properties-at (point))
                      (if flag-or-unflag
                          `(flag ,(eq flag-or-unflag 'flag))
                        '(done t))))
        (inhibit-read-only t))
    (when (eq flag-or-unflag 'unflag)
      (setq prop `(idx ,idx)))
    ;; If unflagging, then remove additional text properties.
    (when (eq flag-or-unflag 'unflag)
      (remove-text-properties (point) to '(font-lock-face flag)))
    (delete-region (point) to)
    (mines--insert elt idx prop (string mines-empty-cell-char) flag-or-unflag)
    (unless flag-or-unflag (aset mines-state idx '@))
    (mines-goto idx)))

(defun mines-dig (&optional show-mines)
  "Reveal the content of the cell at point."
  (interactive)
  (when (mines-mines-mode-p)
    (if mines-game-over
        (user-error "Current game is over.  Try `%s' to start a new one"
                    (substitute-command-keys "\\[mines]"))
      (skip-chars-forward "[:blank:]") ; Set point in the center of the cell.
      (cl-labels ((uncover-fn
                   ()
                   (let ((idx (mines-current-pos))
                         (inhibit-read-only t)
                         (done (get-text-property (point) 'done)))
                     (cond ((null idx) (user-error "Wrong position!"))
                           (done nil) ; Already updated.
                           (t
                            (let ((elt (aref mines-grid idx)))
                              (cl-flet ((game-end-fn
                                         ()
                                         ;; Check for end of game.
                                         (cond ((and (not show-mines) (eq elt t))
                                                ;; We lost the game; show all the mines.
                                                (mines-game-over))
                                               (t
                                                (when (and (not show-mines) (mines-end-p))
                                                  (mines-game-completed))))))
                              ;; Don't end the game in the first trial when
                              ;; `mines-protect-first-move' is non-nil.
                              (when (and (eq elt t) mines-protect-first-move (mines-first-move-p))
                                (let ((ok-pos (cl-position-if-not (lambda (x) (eq t x)) mines-grid)))
                                  (message "Avoided game over in the first move")
                                  ;; Update mine positions.
                                  (setf (nth (cl-position idx mines-mine-positions)
                                             mines-mine-positions) ok-pos)
                                  ;; We must update `mines-grid' further: the neighbour cells
                                  ;; to IDX must show now a lower number of near bombs; the
                                  ;; cells near the new position of the bomb must increase their
                                  ;; numbers.
                                  (setq mines-grid (make-vector mines-number-cells nil))
                                  ;; Add the mine positions.
                                  (dolist (pos mines-mine-positions)
                                    (aset mines-grid pos t))
                                  ;; Update the numbers on neighbour cells.
                                  (mines-set-numbers)
                                  ;; Update current element.
                                  (setq elt (aref mines-grid idx))))
                              (cond ((and (not show-mines) (eq (following-char) mines-flagged-cell-char))
                                     ;; If the cell is flagged ask for confirmation.
                                     (cond ((yes-or-no-p "This cell is flagged as having a bomb.  Uncover it? ")
                                            ;; Unflag first.
                                            (mines--update-cell idx mines-uncover-cell-char 'unflag)
                                            (mines--update-cell idx elt)
                                            (game-end-fn))
                                           (t (message "OK, canceled"))))
                                    (t
                                     (mines--update-cell idx elt)
                                     (game-end-fn))))))))))
        (uncover-fn)
        (when mines-undone-neighbours
          (while mines-undone-neighbours
            (let ((to (pop mines-undone-neighbours)))
              (save-excursion
                (mines-goto to)
                (uncover-fn)))))))))

;; `read-multiple-choice' requires Emacs > 25.
(defun mines--read-multiple-choice ()
  (let (choice)
    (if (> emacs-major-version 25)
        (setq choice
              (read-multiple-choice "Choose difficulty level: "
                                    '((?e "Easy" "8 columns x 8 rows and 10 mines")
                                      (?m "Medium" "16 columns x 16 rows and 40 mines")
                                      (?h "Hard" "30 columns x 16 rows and 99 mines")
                                      (?c "Custom" "C columns x R rows and M mines"))))
      (let ((help-msg "Choose difficulty level: 

e: [e] Easy              m: Medium                h: [h] Hard              c: [c] Custom
8 columns x 8 rows       16 columns x 16 rows     30 columns x 16 rows     C columns x R rows
and 10 mines             and 40 mines             and 99 mines             and M mines
")                                                                           
            (answer
             (read-char "Choose difficulty level:  ([e] Easy, [m] Medium, [h] Hard, [c] Custom, [?]): ")))
        (cl-flet ((show-help ()
                             (when (eq answer ??)
                               (let ((help-buf (get-buffer-create "*Multiple Choice Help*")))
                                 (setq answer nil)
                                 (with-current-buffer help-buf
                                   (and (zerop (buffer-size)) (insert help-msg))
                                   (display-buffer help-buf))))))
          (if (eq answer ??) (show-help))
          (while (not (memq answer '(?e ?m ?h ?c ??)))
            (setq answer (read-char "Choose difficulty level:  ([e] Easy, [m] Medium, [h] Hard, [c] Custom, [?]): "))
            (show-help))
          (cond ((eq answer ?e) (list ?e "Easy" "8 columns x 8 rows and 10 mines"))
                ((eq answer ?m) (list ?m "Medium" "16 columns x 16 rows and 40 mines"))
                ((eq answer ?h) (list ?h "Hard" "30 columns x 16 rows and 99 mines"))
                ((eq answer ?c) (list ?c "Custom" "C columns x R rows and M mines"))))))))

;;;###autoload
(defun mines (&optional arg)
  "Play the minesweeper game.
Called with a prefix prompt for the difficulty level."
  (interactive
   (let* ((prefix current-prefix-arg)
          (choice (and prefix (mines--read-multiple-choice))))
     (when choice
       (mines-init (eq ?e (car choice))
                   (eq ?m (car choice))
                   (eq ?h (car choice))
                   (eq ?c (car choice))
                   (setq mines-number-cols (read-number "Number of columns: ")
                         mines-number-rows (read-number "Number of rows: ")
                         mines-number-mines (read-number "Number of mines: "))))
     (list prefix)))
  (unless arg
    (mines-init (eq mines-difficulty-level 'easy)
                (eq mines-difficulty-level 'medium)
                (eq mines-difficulty-level 'hard)
                t))
  (setq mines-init-time (current-time))
  (mines-start)
  (mines-set-numbers)
  (mines-show))

(define-derived-mode mines-mode special-mode "mines"
  "Major mode for playing Minesweeper.

The target of the game is discover which cells contain mines.
You reveal the content of the mine at point with \\[mines-dig\].
1. If you look at one cell containing a mine you lost.

2. A cell without a mine with N neighbour cells containing mines
   shows N when you look at it.

3. A cell without a mine and without neighbour cells having mines
   shows the character `@' when you look at it; all adjacent cells
   are recursively revealed.

For instance, following is a possible configuration:

@ @ @ @ @
1 2 2 1 @
1 x x 1 @
1 2 2 1 @
@ @ @ @ @

You can move between cells using the arrow keys, or using vi
or Emacs keystrokes (↑↓→←) = (kjlh) = (pnfb).

You can flag a cell as having a mine with \\[mines-flag-cell\]; if you
call this command again, the cell is unflagged."
  (let ((map mines-mode-map))
    (define-key map [right] 'mines-go-right)
    (define-key map "f" 'mines-go-right)
    (define-key map "l" 'mines-go-right)
    (define-key map [left] 'mines-go-left)
    (define-key map "b" 'mines-go-left)
    (define-key map "h" 'mines-go-left)
    (define-key map "p" 'mines-go-up)
    (define-key map "k" 'mines-go-up)
    (define-key map [up] 'mines-go-up)
    (define-key map [down] 'mines-go-down)
    (define-key map "n" 'mines-go-down)
    (define-key map "j" 'mines-go-down)
    (define-key map "x" 'mines-dig)
    (define-key map "c" 'mines-dig)
    ;; (define-key map "a" 'mines-flag-cell)
    (define-key map "1" 'mines-flag-cell)
    (define-key map "m" 'mines-flag-cell)
    (define-key map "r" 'mines)))


;;; Predicates

(defun mines-mines-mode-p ()
  "Return non-nil if the current buffer is in `mines-mode'."
  (derived-mode-p 'mines-mode))

(defun mines-end-p ()
  "Return non-nil when the game is completed."
  (= mines-number-mines (length (mines--find-pos nil mines-state))))

(defun mines-first-move-p ()
  "Return non-nil if any cell has been revealed yet."
  (cl-every 'null mines-state))


(provide 'mines)
;;; mines.el ends here
