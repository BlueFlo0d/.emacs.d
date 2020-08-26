;;; k-useless.el --- Useless Emacs effects -*- lexical-binding: t; -*-

;; Copyright 2020 Qiantan Hong

;; Author: Qiantan Hong <qhong@mit.edu>

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; balabala.

;;; Code:
(require 'cl)
(defgroup k-useless nil
  "Useless Emacs effects."
  :prefix "k-useless-")
(defcustom k-useless-animate-frame t
  "Let the frame shake when typing or deleting.")
(defcustom k-useless-animate-background nil
  "Let the background blink when typing or deleting.")
(defun k-useless--oscillator-impact (oscillator delta-velocity)
  "Add DELTA-VELOCITY to OSCILLATOR."
  (incf (cdr oscillator) delta-velocity))
(defun k-useless--oscillator-step (oscillator oscillator-spec)
  "Let OSCILLATOR evolves for one time step using OSCILLATOR-SPEC.
OSCILLATOR-SPEC is expressed in natural units."
  (setf (car oscillator) (min 10.0 (max -10.0 (car oscillator))))
  (setf (cdr oscillator) (min 100.0 (max -100.0 (cdr oscillator))))
  (decf (cdr oscillator)
        (+ (* (car oscillator-spec) (car oscillator))
           (* (cdr oscillator-spec) (cdr oscillator))))
  (incf (car oscillator) (cdr oscillator)))
(defvar k-useless--frame-oscillator-x '(0 . 0)
  "The oscillator for X direction movement of current frame.")
(defvar k-useless--frame-oscillator-y '(0 . 0)
  "The oscillator for Y direction movement of current frame.")
(defvar k-useless--frame-oscillator-spec '(4000.0 . 0.1)
  "The oscillator spec for K-USELESS--FRAME-OSCILLATOR-{X,Y} in S.I.
The CAR cell stores the square of angular frequency.
The CDR cell stores damping factor (should in [0.0, 1.0)).")

(defvar k-useless--frame-oscillator-impact-x-range 20
  "The range of random X direction impact to current frame.")
(defvar k-useless--frame-oscillator-impact-x-offset 0
  "The offset of random X direction impact to current frame.")
(defvar k-useless--frame-oscillator-impact-y-range 100
  "The range of random Y direction impact to current frame.")
(defvar k-useless--frame-oscillator-impact-y-offset -50
  "The offset of random Y direction impact to current frame.")
(defvar k-useless--timer-step 0.04
  "Time step for the main animation timer.")

(defun k-useless--oscillator-evolve (oscillator oscillator-spec)
  "Let OSCILLATOR evolves for K-USELESS--TIMER-STEP using OSCILLATOR-SPEC.
OSCILLATOR-SPEC is expressed in S.I.
Number of steps is automatically chosen to guanrantee numerical stability."
  (let* ((sqk (sqrt (car oscillator-spec)))
         (nsteps (ceiling (/ (* 0.5 k-useless--timer-step sqk) (- 1 (cdr oscillator-spec)))))
         (h (/ k-useless--timer-step nsteps)))
    (dotimes (i nsteps)
      (k-useless--oscillator-step oscillator (cons (* h h (car oscillator-spec))
                                                   (* 2 h (cdr oscillator-spec) sqk))))
    (setq test (cons (* h h (car oscillator-spec))
                     (* h (cdr oscillator-spec) sqk))))
  (car oscillator))
(defvar k-useless--old-pos (frame-position (selected-frame))
  "The equilibrium position for current frame.")
(defun k-useless--animate-frame ()
  "Animate current frame position."
  (let ((current-pos (frame-position (selected-frame))))
    (setq k-useless--old-pos (cons
                              (- (car current-pos) (car k-useless--frame-oscillator-x))
                              (- (cdr current-pos) (car k-useless--frame-oscillator-y)))))
  (let* ((old-x (car k-useless--old-pos))
         (old-y (cdr k-useless--old-pos))
         (new-x (+ old-x (k-useless--oscillator-evolve
                          k-useless--frame-oscillator-x
                          k-useless--frame-oscillator-spec)))
         (new-y (+ old-y (k-useless--oscillator-evolve
                          k-useless--frame-oscillator-y
                          k-useless--frame-oscillator-spec))))
    (set-frame-position (selected-frame) (round new-x) (round new-y))))

(require 'colir)
(defvar k-useless--background-blink-alpha 0.0
  "Current background alpha value.")
(defvar k-useless--background-old (face-attribute 'default :background)
  "Equilibrium background color.")
(defvar k-useless--foreground-old (face-attribute 'default :foreground)
  "Excited background color.")
(defcustom k-useless-background-decay-time 0.1
  "Time constant for background color to relax from excited to equilibrium.")
(defun k-useless--blend-color (x y alpha)
  "Blend color (standard string format) X and Y using float ALPHA."
  (if (= alpha 0.0)
      y
    (apply #'color-rgb-to-hex
           (cl-mapcar
            (lambda (xc yc)
              (+ (* alpha xc)
                 (* (- 1 alpha) yc)))
            (colir-color-parse x)
            (colir-color-parse y)))))

(defun k-useless--animate-background ()
  "Animate background color."
  (set-face-attribute
   'default nil :background
   (k-useless--blend-color
    k-useless--foreground-old k-useless--background-old
    k-useless--background-blink-alpha))
  (setq k-useless--background-blink-alpha
        (let ((alpha (* k-useless--background-blink-alpha
                        (- 1 (/ k-useless--timer-step
                                k-useless-background-decay-time)))))
          (if (< alpha 0.1) 0.0 alpha))))
(defvar k-useless--timer)
(defun k-useless--animate ()
  "Do every enabled animation."
  (when k-useless-animate-frame
    (k-useless--animate-frame))
  (when k-useless-animate-background
    (k-useless--animate-background)))
(defvar k-useless--timer nil
  "The main animation timer.")

(defun k-useless--trigger (&rest args)
  "Initiate enabled animations once.
ARGS are ignored."
  (interactive)
  (when k-useless-animate-frame
    (k-useless--oscillator-impact
     k-useless--frame-oscillator-x
     (+ k-useless--frame-oscillator-impact-x-offset
        (* 0.01 (random 100) k-useless--frame-oscillator-impact-x-range)))
    (k-useless--oscillator-impact
     k-useless--frame-oscillator-y
     (+ k-useless--frame-oscillator-impact-y-offset
        (* 0.01 (random 100) k-useless--frame-oscillator-impact-y-range))))
  (when k-useless-animate-background
    (setq k-useless--background-blink-alpha 0.5)))

(define-minor-mode k-useless-mode
  "A minor mode to make emacs high."
  nil " K-Useless!" nil
  (if k-useless-mode
      (progn
        (add-hook 'after-change-functions
                  #'k-useless--trigger)
        (setq k-useless--timer (run-at-time nil k-useless--timer-step
                                            'k-useless--animate)))
    (cancel-timer k-useless--timer)
    (setq k-useless--timer nil)))


(defun k-useless--ease-in (init-val end-val progress)
  "Ease-in function used by window animations.
When PROGRESS varies from 0.0 to 1.0, its value varies from INIT-VAL to END-VAL."
  (+ init-val (* (- end-val init-val) progress (- 2 progress))))
(defcustom k-useless-window-animation-time 0.4
  "Duration of the window animations.")
(defmacro k-useless--safe-timer-thunk (timer &rest body)
  "Wrap BODY as a thunk to be executed in TIMER, canceling the timer in case there's any error."
  `(lambda ()
     (condition-case error-description
         (progn ,@body)
       (error
        (message "Error occured in a timer used by K-USELESS. Canceling it.")
        (print error-description)
        (cancel-timer ,timer)))))
(defun k-useless--split-window-advice (cont &optional window size side pixelwise)
  "Use as an around advice for SPLIT-WINDOW, adding animation effect.
Function SPLIT-WINDOW should be passed as CONT.
WINDOW SIZE SIDE PIXELWISE have the same meaning as for SPLIT-WINDOW."
  (let* ((horizontal
          (not (memq side '(up down above below nil))))
         (init-size (window-min-pixel-size window horizontal))
         (new-window (funcall cont window (- init-size) side t))
         (size (if (numberp size) size
                 (/ (window-size window horizontal) 2)))
         (size-in-pixel (window--size-to-pixel new-window size horizontal pixelwise))
         (timer nil)
         (step 0))
    (setq timer
          (run-at-time k-useless--timer-step k-useless--timer-step
                       (k-useless--safe-timer-thunk
                        timer
                        (incf step)
                        (let ((progress (/ (* step k-useless--timer-step) k-useless-window-animation-time))
                              (current-size (window-size new-window horizontal t)))
                          (if (< progress 1.0)
                              (window-resize new-window (- (k-useless--ease-in init-size size-in-pixel progress)
                                                           current-size)
                                             horizontal nil t)
                            (window-resize new-window (- size-in-pixel
                                                         current-size)
                                           horizontal)
                            (cancel-timer timer))))))
    new-window))

(defun k-useless--delete-window-advice (cont &optional window)
  "Use as an around advice for DELETE-WINDOW, adding animation effect.
Function DELETE-WINDOW should be passed as CONT.
WINDOW has the same meaning as for SPLIT-WINDOW."
  (let* ((horizontal
          (window-left-child (window-parent window)))
         (final-size (window-min-pixel-size window horizontal))
         (init-size (window-size window horizontal t))
         (timer nil)
         (step 0))
    (setq timer
          (run-at-time k-useless--timer-step k-useless--timer-step
                       (k-useless--safe-timer-thunk
                        timer
                        (incf step)
                        (let ((progress (/ (* step k-useless--timer-step) k-useless-window-animation-time))
                              (current-size (window-size window horizontal t)))
                          (if (< progress 1.0)
                              (window-resize window (- (k-useless--ease-in final-size init-size (- 1.0 progress))
                                                       current-size)
                                             horizontal nil t)
                            (funcall cont window)
                            (cancel-timer timer))))))
    nil))

(define-minor-mode k-useless-animate-window-mode
  "Smooth window manipulations!"
  nil nil nil
  :global t
  (if k-useless-animate-window-mode
      (progn
        (advice-add 'split-window :around #'k-useless--split-window-advice)
        (advice-add 'delete-window :around #'k-useless--delete-window-advice))
    (advice-remove 'split-window  #'k-useless--split-window-advice)
    (advice-remove 'delete-window #'k-useless--delete-window-advice)))

(defun k-useless-delete-window (&optional window)
  "Animated delete window.
WINDOW has the same meaning as for DELETE-WINDOW."
  (if k-useless-animate-window-mode
      (delete-window window)
    (k-useless--delete-window-advice #'delete-window window)))
(defun k-useless-split-window (&optional window size side pixelwise)
  "Animated split window.
WINDOW SIZE SIDE PIXELWISE have the same meaning as for DELETE-WINDOW."
  (if k-useless-animate-window-mode
      (split-window window size side pixelwise)
    (k-useless--split-window-advice #'split-window window size side pixelwise)))

(provide 'k-useless)
;;; k-drop.el ends here
