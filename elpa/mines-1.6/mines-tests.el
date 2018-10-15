;;; mines-tests.el --- Tests for mines.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>,
;; Keywords:

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; code:

(require 'ert)
(require 'mines)
(require 'cl-lib)

(ert-deftest mines-test-gameover ()
  (mines)
  (let ((buf (get-buffer "*Mines*"))
        (grid-orig (copy-sequence mines-grid))
        (idx-bomb (cl-position t mines-grid))
        idx-ok grid-new)
    (with-current-buffer buf
      (mines-goto idx-bomb) ; Go to first mine.
      (setq idx-ok (cl-position-if-not (lambda (x) (eq t x)) mines-grid))
      (should (mines-first-move-p))
      (should-not (mines-dig)) ; Not gameover in first trial.
      (setq grid-new (copy-sequence mines-grid))
      ;; Adjacent cells to bombs must be updated as well.
      (should-not (equal grid-orig
                         (cl-rotatef (aref grid-new idx-bomb)
                                     (aref grid-new idx-ok))))
      (mines-goto (cl-position t mines-grid))
      (should-not (mines-first-move-p))
      ;; 2nd trial might end the game.
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (error "Fail"))))
        (should-error (mines-dig))))
    (when (buffer-live-p buf) (kill-buffer buf))))

(ert-deftest mines-test-find-pos ()
  (mines)
  (let* ((buf (get-buffer "*Mines*"))
         (empty-pos (mines--find-pos nil mines-grid))
         (non-empty-pos (cl-set-difference
                         (number-sequence 0 (1- mines-number-cells))
                         empty-pos)))
    (with-current-buffer buf
      (dolist (idx empty-pos)
        (should-not (aref mines-grid idx)))
      (dolist (idx non-empty-pos)
        (should (aref mines-grid idx))))
    (when (buffer-live-p buf) (kill-buffer buf))))

(ert-deftest mines-test-game-completed ()
  (mines)
  (let ((buf (get-buffer "*Mines*"))
        (empty-pos (mines--find-pos nil mines-grid))
        uncover-pos)
    (with-current-buffer buf
      (should (mines-mines-mode-p))
      ;; Uncover all empty cells.
      (dolist (idx empty-pos)
        (mines-goto idx)
        (mines-dig))
      (setq uncover-pos (mines--find-pos nil mines-state))
      ;; Exclude mines from `uncover-pos'.
      (dolist (idx mines-mine-positions)
        (setq uncover-pos (delete idx uncover-pos)))
      ;; Uncover all but the first element in `uncover-pos'.
      (dolist (idx (cdr uncover-pos))
        (mines-goto idx)
        (mines-dig))
      (should-not (mines-end-p))
      (mines-goto (car uncover-pos))
      (mines-dig) ; Uncover the last one.
      (should (mines-end-p)) ; Game completed.
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest mines-test-indices ()
  (should (= 0 (mines-matrix-2-index 0 0)))
  (should (= 1 (mines-matrix-2-index 0 1)))
  (should (= (1- mines-number-cols)
             (mines-matrix-2-index 0 (1- mines-number-cols))))
  (should (= (* mines-number-cols (1- mines-number-rows))
             (mines-matrix-2-index (1- mines-number-rows) 0)))
  (should (= (1- mines-number-cells)
             (mines-matrix-2-index (1- mines-number-rows)
                                   (1- mines-number-cols))))
  ;; `mines-matrix-2-index' consistent with `mines-index-2-matrix'.
  (dolist (idx (list 0 1 (1- mines-number-cols)
                     (* mines-number-cols (1- mines-number-rows))
                     (1- mines-number-cells)))
    (should (= idx (apply #'mines-matrix-2-index (mines-index-2-matrix idx))))))

(ert-deftest mines-test-neighbours ()
  (cl-flet ((set-equalp (x y)
                        (and (null (cl-set-difference x y))
                             (= (length x) (length y)))))
    (set-equalp (list 1 mines-number-cols (1+ mines-number-cols))
                (mines-get-neighbours 0))
    (set-equalp (list 0 2 mines-number-cols (1+ mines-number-cols) (+ 2 mines-number-cols))
                (mines-get-neighbours 1))
    (set-equalp (list 1 2 3 mines-number-cols (1+ mines-number-cols) (+ 3 mines-number-cols)
                      (+ 2 (* 2 mines-number-cols)) (1+ (* 2 mines-number-cols)) (+ 3 (* 2 mines-number-cols)))
                (mines-get-neighbours (+ 2 mines-number-cols)))
    (set-equalp (list (- mines-number-cells 2)
                      (1- (* (1- mines-number-rows) mines-number-cols))
                      (- (* (1- mines-number-rows) mines-number-cols) 2))
                (mines-get-neighbours (1-  mines-number-cells)))))

(ert-deftest mines-test-neighbours ()
  (mines)
  (let ((buf (get-buffer "*Mines*")))
    (with-current-buffer buf
      (mines-goto (1- mines-number-cols))
      (should (eq (following-char) mines-uncover-cell-char))
      (mines-flag-cell) ; First time flag cell.
      (should (eq (following-char) mines-flagged-cell-char))
      (mines-flag-cell) ; Second time unflag.
      (should (eq (following-char) mines-uncover-cell-char))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest mines-test-goto ()
  (mines)
  (let ((buf (get-buffer "*Mines*")))
    (with-current-buffer buf
      (dolist (idx (list 0 1 (1- mines-number-cols) (1- mines-number-rows)
                         (1- mines-number-cells)))
        (mines-goto idx)
        (should (= idx (mines-current-pos))))
      (let ((idx (+ 100 mines-number-cells)))
        (mines-goto idx) ; No error
        (should-not (= idx (mines-current-pos))))
      ;; Test right. left, up, down movements.
      (dotimes (idx mines-number-cells)
        (mines-goto idx)
        (let* ((row (car (mines-index-2-matrix idx)))
               (col (cadr (mines-index-2-matrix idx)))
               (idx-right (if (= col (1- mines-number-cols))
                              (mines-matrix-2-index row 0)
                            (mines-matrix-2-index row (1+ col))))
               (idx-left (if (zerop col)
                             (mines-matrix-2-index row (1- mines-number-cols))
                           (mines-matrix-2-index row (1- col))))
               (idx-up (if (zerop row)
                           (mines-matrix-2-index (1- mines-number-rows) col)
                         (mines-matrix-2-index (1- row) col)))
               (idx-down (if (= row (1- mines-number-rows))
                             (mines-matrix-2-index 0 col)
                           (mines-matrix-2-index (1+ row) col))))
          (save-excursion ; Right
            (mines-go-right)
            (should (= idx-right (mines-current-pos))))
          (save-excursion ; Left
            (mines-go-left)
            (should (= idx-left (mines-current-pos))))
          (save-excursion ; Up
            (mines-go-up)
            (should (= idx-up (mines-current-pos))))
          (save-excursion ; Down
            (mines-go-down)
            (should (= idx-down (mines-current-pos)))))))
    (when (buffer-live-p buf) (kill-buffer buf))))

;; `mines--near-bombs' is used when `mines-grid' just contains nil or t.
(ert-deftest mines-test-near-bombs ()
  (mines)
  (let ((buf (get-buffer "*Mines*"))
        (grid (make-vector mines-number-cells nil)))
    (dolist (idx mines-mine-positions)
      (aset grid idx t))
    (with-current-buffer buf
      (let ((mines-grid grid))
        (dotimes (idx mines-number-cells)
          (unless (aref grid idx)
            (let ((row (car (mines-index-2-matrix idx)))
                  (col (cadr (mines-index-2-matrix idx)))
                  (numb 0))
              (mines-goto idx)
              ;; Right
              (when (< col (1- mines-number-cols))
                (when (aref grid (mines-matrix-2-index row (1+ col)))
                  (cl-incf numb)))
              ;; Left
              (unless (zerop col)
                (when (aref grid (mines-matrix-2-index row (1- col)))
                  (cl-incf numb)))
              ;; Up
              (unless (zerop row)
                (when (aref grid (mines-matrix-2-index (1- row) col))
                  (cl-incf numb)))
              ;; Up-Right
              (when (and (not (zerop row)) (< col (1- mines-number-cols)))
                (when (aref grid (mines-matrix-2-index (1- row) (1+ col)))
                  (cl-incf numb)))
              ;; Up-Left
              (when (and (not (zerop row)) (not (zerop col)))
                (when (aref grid (mines-matrix-2-index (1- row) (1- col)))
                  (cl-incf numb)))
              ;; Down
              (unless (= row (1- mines-number-rows))
                (when (aref grid (mines-matrix-2-index (1+ row) col))
                  (cl-incf numb)))
              ;; Down-Right
              (when (and (not (= row (1- mines-number-rows))) (< col (1- mines-number-cols)))
                (when (aref grid (mines-matrix-2-index (1+ row) (1+ col)))
                  (cl-incf numb)))
              ;; Down-Left
              (when (and (not (= row (1- mines-number-rows))) (not (zerop col)))
                (when (aref grid (mines-matrix-2-index (1+ row) (1- col)))
                  (cl-incf numb)))
              (should (= numb (mines--near-bombs row col))))))))
    (when (buffer-live-p buf) (kill-buffer buf))))

(ert-deftest mines-test-read-multiple-choice ()
  (if (> emacs-major-version 25)
      (dolist (char '(?e ?m ?h ??))
        (cl-letf* (((symbol-function #'read-char) (lambda () char))
                   (str (cdr (assoc char '((?e . "Easy") (?m . "Medium") (?h . "Hard") (?? . "Help"))))))
          (should (equal (list char str)
                         (read-multiple-choice "Choose level "
                                               '((?e "Easy") (?m "Medium") (?h "Hard") (?? "Help")))))))
    (cl-block :help
      ;; When char is ? `mines--read-multiple-choice' enter a loop until char
      ;; is a different character.  Exit the loop.
      (dolist (char '(?e ?m ?h ??))
        (cl-letf (((symbol-function #'read-char)
                   (lambda (&rest _) (if (eq char ??) (cl-return-from :help nil) char))))
          (should (mines--read-multiple-choice)))))))


(provide 'mines-tests)
;;; mines-tests.el ends here

