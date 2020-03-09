;;; flames-of-freedom.el --- The flames of freedom  -*-coding: utf-8-dos; lexical-binding: t; -*-

;; Copyright (C) 2019 Stéphane Champailler

;; Author: Stéphane Champailler <schampailler@skynet.be>
;; Version: 1.2
;; Package-Version: 20191202.1637
;; Package-Requires: ((emacs "25.1"))
;; Keywords: multimedia
;; URL: https://github.com/wiz21b/FlamesOfFreedom

;; This code is covered by the GNU's Affero General Public License Version 3.

;;; Commentary:

;; These are the eternal flames of freedom (and an homage to RMS who
;; is having tough times in this year 2019).


;;; Code:

;; Although this project started as reflection on the free software
;; movement it ended up being an exercise in optimization. The
;; question became how fast can I draw those flames ? You'll find
;; notes below explaining the various things I've tried.
;; Interestingly, many attempts to make the code faster didn't result
;; in significant performance improvements. This leads me to think
;; that Emacs lisp is very predictable and that the functions provided
;; by Emacs (buffer, text,...) are not optimization friendly (1/ they
;; can't be combined in clever way; 2/ as stated in the documentation,
;; calling functions is slow). IMHO, elisp is a language that is
;; designed to program Emacs (and a very mature one at that), not to
;; make real time graphics. What a surprise :-)

;; Things that improved speed :

;; * In the end, the main optimization revolves around a scheme.
;;   First, compute all the chars of the buffer and transform that
;;   into a big string, at once (that's "at once" that makes things
;;   fast).  Second, set "face" properties directly in the buffer, not
;;   on the string.

;; * A bit of profiling indicates that removing the call to
;;   set-text-properties increases the speed by 50%.

;; * Byte compiling provides a significant improvement.

;; * Using set-text-properties directly on buffer seems faster than
;;   putting properties on strings and then inserting the string+props
;;   in the buffer.

;; Things that didn't improve speed :

;; * set-text-properties is not faster than put-text-property
;;   in my case (but set-text-properties is cleaner).

;; * I tried to defsubst some lambda expressions => no noticeable
;;   speed improvement.

;; * I tried to transform some variables into global variables
;;   (defvar) => no noticeable speed improvement. (hypothesis was :
;;   global variable are easier to reach than local variables).

;; * I tried double buffering the buffers (hypothesis was Emacs
;;   displays a buffer more efficiently when it's a window
;;   refresh). This made things significantly slower.

;; * Moved face construction outside the loop. Cleaner code,
;;   performance are the same.

;; * Replacing the buffer (setf (buffer-substring ...)) instead of
;;   erasing/recreating it did not make things faster.

;; Things I don't quite understand :

;; * On my Linux machine, sometimes, the animation is choppy.
;;   Interestingly it makes little pauses in a very predictable way
;;   (about twice a second). Restarting Emacs fixes the issue
;;   sometimes.

;; * The font size affects performance in some unpredictable
;;   ways. Changing the font size by a single point can cut the speed
;;   in two (where I'd expect a few percent loss). Also, using
;;   smaller fonts sometimes results in faster (!) speed...

;; * Calling (set-window-start window 1) before redisplay increases
;;   performance significantly.


(require 'seq)

(defun flames-of-freedom-make-vector-by-step (steps)
  "Build a vector by STEPS.

A step is a pair ( what .  how many).  STEPS is a list of steps.

Example : (( 'a' . 1 ) ( 'b' . 2 )) will give
          [ 'a' 'b' 'b' ]"

  (cond ((null steps) [ ] )
	(1 (let ((step (car steps)))
	     (vconcat (make-vector (car step) (cdr step))
		      (flames-of-freedom-make-vector-by-step (cdr steps)))))))


(defun flames-of-freedom-dups (l)
  "Count repeated elements in vector L.

Output : a list of pair (element . count).

Example : '(10 10 10 20 20 30) will give
          '((10 . 3) (20 . 2) (30. 1))"

  ;; This was originally written as a tail recursive function.
  ;; However this brought Emacs to its limits (and it raised error
  ;; because stack depth). So I rewrote it in a more imperative way.

  ;; We go from right to left.

  (let ( (i (- (length l) 2))
	 (current-elt (aref l (- (length l) 1))) ; last one
	 (current-cnt 1)
	 (ret-list '()))

    (while (>= i 0)
      (let ( (first-of-rest (aref l i)) )
	(if (eq current-elt first-of-rest)
	    (setq current-cnt (1+ current-cnt))
	  (progn
	    (setq ret-list (cons (cons current-elt current-cnt) ret-list))
	    (setq current-elt first-of-rest)
	    (setq current-cnt 1))))

      (setq i (- i 1)))

    (if (> current-cnt 0)
	(progn
	  (setq ret-list (cons (cons current-elt current-cnt) ret-list))))))

(defconst flames-of-freedom-message-separator "|")

;; How long each sentence of the message will be displayed, in seconds.
(defconst flames-of-freedom-message-show-time 3)

(defconst flames-of-freedom-int-to-blocks
  (flames-of-freedom-make-vector-by-step '( (1  . ?\ )
					    (2  . ?.)
					    (2  . #x2591)
					    (23 . #x2592)
					    (10 . #x2593))))

(defconst flames-of-freedom-int-to-faces
  (flames-of-freedom-make-vector-by-step
   '((5  . (face (:foreground "grey"   :background "black")))
     (2  . (face (:foreground "orange" :background "grey")))
     (3  . (face (:foreground "orange" :background "red")))
     (4  . (face (:foreground "red"    :background "yellow")))
     (4  . (face (:foreground "yellow" :background "yellow")))
     (10 . (face (:foreground "white"  :background "yellow")))
     (10 . (face (:foreground "white"  :background "white"))))))


(defun flames-of-freedom-write-line (text pos)
  "Write a TEXT line at some POS in the current buffer."
  (goto-char (point-min))
  (forward-line pos)
  (delete-region (point) (+ (point) (length text)))
  (insert (propertize text 'face '(:foreground "white" :background "black"))))

(defun flames-of-freedom-make-flames-buffer (flame-buffer-width flame-buffer-height)
  "The buffer in which the flames are computed is a grid of integers.

It's represented by a vector of vectors.  This function
initializes such a grid.  The grid size is FLAME-BUFFER-WIDTH x
FLAME-BUFFER-HEIGHT."

  (let ((l (make-vector flame-buffer-height nil)))
    (dotimes (i flame-buffer-height)
      (aset l i (make-vector flame-buffer-width 0)))
    l))

(defun flames-of-freedom-update-flames (l flame-buffer-width time)
  "Update the flame grid in L.

The grid has width FLAME-BUFFER-WIDTH.  The TIME gives an
indication of how much time has passed.  It's useful to throttle
some effects."

  ;; The lowest line of the flames is random.  I don't update it
  ;; too often so that the flames look nicer.

  (if (= 0 (% time 5))
      (let ((factor (if (< time flame-buffer-width)
			(truncate (+ 1 (* 35 (/ (float time) flame-buffer-width))))
		      36))
	    (last-line (- (length l) 1)))
	(dotimes (i flame-buffer-width)
	  (aset (aref l last-line)
		i (random factor)))))

  ;; Just compute the flames as usual. Add a bit of randomness to
  ;; make it look better.

  (dotimes (i (- (length l) 1))
    (let ( (v (aref l (+ i 1)))
	   (v2 (aref l i))
	   (width (- flame-buffer-width 2)))
      (dotimes (z width)
	(aset v2 (1+ z)
	      (/
	       (+ (aref v (+ 2 z)) (aref v (1+ z)) (aref v z)
		  (aref v2 (1+ z)) (- (random 4) 1))
	       4))))))

(defun flames-of-freedom-flames-to-string (l)
  "Build a big string representing the whole flame grid L.

The string is made of graphical characters.  The colors should
be added later."

  (let* ((width-base (length (aref l 1)))
	 (width (+ 1 (length (aref l 1))))
	 (bigv (make-vector (* (length l) width) 0)) ; Recreating the vector is not significantly slower.
	 (i 0))

    (dotimes (y (length l))
      (let ((line (aref l y)))
	(dotimes (x width-base)
	  (aset bigv (+ i x) (aref flames-of-freedom-int-to-blocks (aref line x)))))

      (setq i (+ width-base i))
      (aset bigv i ?\n )
      (setq i (1+ i)))

  (concat bigv)))


(defun flames-of-freedom-flames-to-string-props (l)
  "Make appropriate (i.e. with nice flame colors) text properties out of the flame grid L."

  (let ((i 1))
    (dotimes (y (length l))

      (let ((line-props (flames-of-freedom-dups (aref l y))))
	;; Using mapc because it's made for "side effects" (see Emacs doc.)
	(mapc (lambda (p)
		(set-text-properties i (+ i (cdr p)) (aref flames-of-freedom-int-to-faces (car p)))
		(setq i (+ i (cdr p))))
	      line-props))
      ;; skip \n at the end of each line
      (setq i (+ i 1)))))

;;;###autoload
(defun flames-of-freedom-my-message (&optional the-message testing)
  "Displays the Flames of Freedom.

These are the eternal flames of freedom (and an homage to RMS who
is having tough times in this year 2019).

THE-MESSAGE is displayed.  It is a list of sentences separated by
\"|\".  If you just want to stare at a comforting fire, just
leave the message empty.

If TESTING is set, then some debugging information is displayed."

  (interactive "sMessage to show (sentences separated by |):")

  (let* ((window (selected-window))

	 ;; -1 seems necessary for this to work in emacs-nw
	 ;; but that's totally empirical.
	 (flame-buffer-width (- (window-body-width window) 1))
	 (flame-buffer-height (+ 3 (window-total-size)))
	 (l (flames-of-freedom-make-flames-buffer flame-buffer-width flame-buffer-height))
	 (buffer1 (get-buffer-create "Flames Of Freedom"))
	 (messages (vconcat (split-string the-message flames-of-freedom-message-separator)))
	 (current-msg 0)
	 (drawn-frames 0)
	 (drawn-frames-benchmarking 0)
	 (start-time-benchmarking (float-time))
	 (last-time-benchmarking (float-time)))

    (cond ( (< (window-total-size) 6) (message "I need a taller window to be shining"))
	  ( (> (apply #'max (mapcar #'length messages)) flame-buffer-width) (message "I need a wider window to be shining"))
      (t (progn

	(if testing
	    (progn
	      (message "Grid is %d x %d = %d cells"
		       flame-buffer-width flame-buffer-height (* flame-buffer-width flame-buffer-height))
	      ;; help testing by making execution results repeatable.
	      (random "alphabravo")))

	(buffer-disable-undo buffer1)
	(set-window-buffer window buffer1)
	(set-buffer buffer1)

	;; Make the cursor disappears
	(setq cursor-type nil)

	(while (not (input-pending-p))

	  ;; Computing the flames

	  (flames-of-freedom-update-flames l flame-buffer-width drawn-frames)

	  ;; Displaying the flames in the buffer.

	  (let* ((sub-vec (seq-take l (min (length l) (window-total-size))))
		 (big-string (flames-of-freedom-flames-to-string sub-vec)))
	    (erase-buffer)
	    (insert big-string)
	    (flames-of-freedom-flames-to-string-props sub-vec))

	  ;; Display the messages in the middle of the screen

	  (if (and the-message (> (length the-message) 0))

	      (progn
		(if (> (float-time) (+ flames-of-freedom-message-show-time last-time-benchmarking))
		    (progn
		      (setq last-time-benchmarking (float-time))
		      (setq current-msg (% (+ 1 current-msg) (length messages)))))

		(let* ((pos (/ (window-total-size) 2))
		       (blank-line (make-string (- flame-buffer-width 1) ?\  ))
		       (msg (aref messages current-msg))
		       (half (/ (- flame-buffer-width (length msg)) 2))
		       (centered-text (concat (make-string half ?\ )
					      msg
					      (make-string half ?\ )
					      (if (< (+ (* half 2) (length msg)) flame-buffer-width) " "))))

		  (flames-of-freedom-write-line blank-line (- pos 1))
		  (flames-of-freedom-write-line centered-text pos)
		  (flames-of-freedom-write-line blank-line (+ pos 1)))))

	  ;; Remove useless information

	  (set-buffer-modified-p nil) ;; with-silent-modification shows buffer modified indicator anyway

	  ;; Make sure the window will display the buffer from its top

	  (set-window-start window 1)   ; !!! This results in a big performance
					; improvement, must be done before each
					; call to redisplay. Dont't know why.

	  ;; Remember Emacs favor processing/input over display so if I
	  ;; don't ask, re-display never gets a chance to occur.

	  (redisplay)

	  (if testing
	      (let ((passed-time (- (float-time) start-time-benchmarking)))
		(if (> passed-time 10)
		    (progn
		      (message "%d frames drawn in %.1f seconds, %.1f fps"
			       drawn-frames-benchmarking
			       passed-time
			       (/ drawn-frames-benchmarking passed-time))
		      (setq start-time-benchmarking (float-time))
		      (setq drawn-frames-benchmarking 0)))
		(setq drawn-frames-benchmarking (+ 1 drawn-frames-benchmarking))))

	  (setq drawn-frames (+ 1 drawn-frames)))

	(kill-buffer buffer1)

	;; Make sure that the key the user has typed to exit our program
	;; doesn't show up in its buffer
	(discard-input))))))


;;;###autoload
(defun flames-of-freedom-default ()
  "Displays the Flames of Freedom.

These are the eternal flames of freedom (and an homage to RMS who
is having tough times in this year 2019).

A little poem is displayed."

  (interactive)
  (flames-of-freedom-my-message
   (mapconcat #'identity
	      '("These are the eternal flames of freedom,"
		"Showing us light in darkness"
		"beyond the thought police."
		"Software is our sword,"
		"GPL the great ultimate.")
	      flames-of-freedom-message-separator)
   1))


(mapc 'byte-compile '(flames-of-freedom-dups
		      flames-of-freedom-update-flames
		      flames-of-freedom-flames-to-string
		      flames-of-freedom-flames-to-string-props))


;;(flames-of-freedom-default)

(provide 'flames-of-freedom)

;;; flames-of-freedom.el ends here
