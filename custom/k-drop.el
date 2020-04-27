(defcustom k-drop-animate-frame t
  "Let the frame shake when typing or deleting.")
(defcustom k-drop-animate-background t
  "Let the background blink when typing or deleting.")
(defun k-drop--oscillator-impact (oscillator delta-velocity)
  "Add DELTA-VELOCITY to OSCILLATOR."
  (incf (cdr oscillator) delta-velocity))
(defun k-drop--oscillator-step (oscillator oscillator-spec)
  "Let OSCILLATOR evolves for one time step using OSCILLATOR-SPEC.
OSCILLATOR-SPEC is expressed in natural units."
  (setf (car oscillator) (min 10.0 (max -10.0 (car oscillator))))
  (setf (cdr oscillator) (min 100.0 (max -100.0 (cdr oscillator))))
  (decf (cdr oscillator)
        (+ (* (car oscillator-spec) (car oscillator))
           (* (cdr oscillator-spec) (cdr oscillator))))
  (incf (car oscillator) (cdr oscillator)))
(defvar k-drop--frame-oscillator-x '(0 . 0)
  "The oscillator for X direction movement of current frame.")
(defvar k-drop--frame-oscillator-y '(0 . 0)
  "The oscillator for Y direction movement of current frame.")
(defvar k-drop--frame-oscillator-spec '(4000.0 . 0.1)
  "The oscillator spec for K-DROP--FRAME-OSCILLATOR-{X,Y} in S.I.
The CAR cell stores the square of angular frequency.
The CDR cell stores damping factor (should in [0.0, 1.0)).")

(defvar k-drop--frame-oscillator-impact-x-range 20
  "The range of random X direction impact to current frame.")
(defvar k-drop--frame-oscillator-impact-x-offset 0
  "The offset of random X direction impact to current frame.")
(defvar k-drop--frame-oscillator-impact-y-range 100
  "The range of random Y direction impact to current frame.")
(defvar k-drop--frame-oscillator-impact-y-offset -50
  "The offset of random Y direction impact to current frame.")
(defvar k-drop--timer-step 0.04
  "Time step for the main animation timer.")

(defun k-drop--oscillator-evolve (oscillator oscillator-spec)
  "Let OSCILLATOR evolves for K-DROP--TIMER-STEP using OSCILLATOR-SPEC.
OSCILLATOR-SPEC is expressed in S.I.
Number of steps is automatically chosen to guanrantee numerical stability."
  (let* ((sqk (sqrt (car oscillator-spec)))
         (nsteps (ceiling (/ (* 0.5 k-drop--timer-step sqk) (- 1 (cdr oscillator-spec)))))
         (h (/ k-drop--timer-step nsteps)))
    (dotimes (i nsteps)
      (k-drop--oscillator-step oscillator (cons (* h h (car oscillator-spec))
                                                (* 2 h (cdr oscillator-spec) sqk))))
    (setq test (cons (* h h (car oscillator-spec))
                     (* h (cdr oscillator-spec) sqk))))
  (car oscillator))
(defvar k-drop--old-pos (frame-position (selected-frame))
  "The equilibrium position for current frame.")
(defun k-drop--animate-frame ()
  "Animate current frame position."
  (let ((current-pos (frame-position (selected-frame))))
    (setq k-drop--old-pos (cons
                           (- (car current-pos) (car k-drop--frame-oscillator-x))
                           (- (cdr current-pos) (car k-drop--frame-oscillator-y)))))
  (let* ((old-x (car k-drop--old-pos))
         (old-y (cdr k-drop--old-pos))
         (new-x (+ old-x (k-drop--oscillator-evolve
                          k-drop--frame-oscillator-x
                          k-drop--frame-oscillator-spec)))
         (new-y (+ old-y (k-drop--oscillator-evolve
                          k-drop--frame-oscillator-y
                          k-drop--frame-oscillator-spec))))
    (set-frame-position (selected-frame) (round new-x) (round new-y))))

(require 'colir)
(defvar k-drop--background-blink-alpha 0.0
  "Current background alpha value.")
(defvar k-drop--background-old (face-attribute 'default :background)
  "Equilibrium background color.")
(defvar k-drop--foreground-old (face-attribute 'default :foreground)
  "Excited background color.")
(defcustom k-drop-background-decay-time 0.1
  "Time constant for background color to relax from excited to equilibrium.")
(defun k-drop--blend-color (x y alpha)
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

(defun k-drop--animate-background ()
  "Animate background color."
  (set-face-attribute
   'default nil :background
   (k-drop--blend-color
    k-drop--foreground-old k-drop--background-old
    k-drop--background-blink-alpha))
  (setq k-drop--background-blink-alpha
        (let ((alpha (* k-drop--background-blink-alpha
                        (- 1 (/ k-drop--timer-step
                                k-drop-background-decay-time)))))
          (if (< alpha 0.1) 0.0 alpha))))
(defvar k-drop--timer)
(cancel-timer k-drop--timer)
(defun k-drop--animate ()
  "Do every enabled animation."
  (when k-drop-animate-frame
    (k-drop--animate-frame))
  ;;(k-drop--animate-overlay)
  (when k-drop-animate-background
    (k-drop--animate-background))
  (when k-drop-glooming-cursor
    (k-drop--animate-glooming-cursor)))
(defvar k-drop--timer nil
  "The main animation timer.")
(setq k-drop--timer (run-at-time nil 0.04
                                 'k-drop--animate))
sadfjdfiasdf
(defcustom k-drop-glooming-cursor nil
  "Turn on glooming cursor tail.")
(defcustom k-drop-glooming-cursor-decay-time 1.0
  "Time constant for cursor tail to decay.")
(defvar k-drop--glooming-cursor-overlays nil
  "Overlays for the glooming tail effect.")

(defun k-drop--animate-glooming-cursor ()
  "Animate cursor tail."
  (cl-remove-if
   (lambda (overlay)
     (let* ((current-color (cadr (overlay-get overlay 'face)))
            (background-color
             (face-attribute 'default :background))
            (blended-color (k-drop--blend-color
                            background-color
                            current-color
                            (/ k-drop--timer-step k-drop-glooming-cursor-decay-time))))
       (overlay-put overlay 'face
                    `(:background
                      ,blended-color))
       (when (equal blended-color background-color)
         (delete-overlay overlay)
         t)))
   k-drop--glooming-cursor-overlays))
(define-minor-mode k-drop-mode
  "A minor mode to make emacs high."
  :lighter " K-Drop!")
(defvar k-drop-blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65"))
(defun k-drop--command ()
  "Initiate enabled animations once."
  (interactive)
  (when k-drop-animate-frame
    (k-drop--oscillator-impact
     k-drop--frame-oscillator-x
     (+ k-drop--frame-oscillator-impact-x-offset
        (* 0.01 (random 100) k-drop--frame-oscillator-impact-x-range)))
    (k-drop--oscillator-impact
     k-drop--frame-oscillator-y
     (+ k-drop--frame-oscillator-impact-y-offset
        (* 0.01 (random 100) k-drop--frame-oscillator-impact-y-range))))
  (when k-drop-animate-background
    (setq k-drop--background-blink-alpha 0.5))
  (when k-drop-glooming-cursor
    (let ((new-overlay (make-overlay (1- (point)) (point))))
      (overlay-put new-overlay 'face `(:background ,(car k-drop-blink-cursor-colors)))
      (push new-overlay
            k-drop--glooming-cursor-overlays))))
(defun k-drop--hook (&rest ARGS)
  "Call K-DROP--COMMAND if K-DROP mode is enabled, but ignore ARGS."
  (when k-drop-mode
    (k-drop--command)))
(add-hook 'after-change-functions
          #'k-drop--hook)
