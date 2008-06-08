;;
;; Library for generating identicons from 32-bit numbers. Based on an idea by
;; Don Park (see <http://www.docuverse.com/blog/donpark/2007/01/18/visual-security-9-block-ip-identification>).
;;

#lang scheme/gui
(require (only-in srfi/26 cut)
         "util.ss")

;; Patch is a graphical object that has a specific shape enclosed in a
;; square area of given size, colorized with foreground and background colors.
;;
;; To draw a patch in a specific location on a dc<%> call its draw method.
(define patch%
  (class object%
    (init-field size)
    (init-field (shape null))
    (init-field (foreground-color "black"))
    (init-field (background-color "white"))

    ;; Covert (x y) point expressed in patch size multiples to real
    ;; coordinates.
    (define (point-relative->absolute point)
      (map (cut * size <>) point))
    (define (shape->absolute-points shape)
      (map point-relative->absolute shape))

    (define (build-path shape)
      (let ((path (make-object dc-path%))
            (points (shape->absolute-points shape)))
        (unless (null? points)
          (send/apply path move-to (first points))
          (for ((point (rest points)))
               (send/apply path line-to point)))
        path))

    (field (path (build-path shape)))

    ;; Call the given code on translated path by x and y. Translation is
    ;; reverted after that.
    (define-syntax with-translation
      (syntax-rules ()
        ((_ ?x ?y ?body ...)
         (begin
           (send path translate ?x ?y)
           (begin ?body ...)
           (send path translate (- ?x) (- ?y))))))

    ;; Fill the patch spot under position (x y) with its background color.
    (define (clear-absolute dc x y)
      (send dc set-brush background-color 'solid)
      (send dc draw-rectangle x y size size))

    ;; Draw the patch path under position (x y) using its foreground color.
    (define (draw-absolute dc x y)
      (send dc set-brush foreground-color 'solid)
      (with-translation x y
        (send dc draw-path path)))

    ;; Draw the patch on given dc under position (x y) expressed in patch size
    ;; multiples.
    (define/public (draw dc x y)
      (match-let (((list x y) (point-relative->absolute (list x y))))
        (clear-absolute dc x y)
        (draw-absolute dc x y)))

    ;; Draw a series of patches, each placed at the coordinate from list of points,
    ;; rotated -90 degrees each time.
    (define/public (draw-rotating dc positions)
      (for ((x-and-y positions))
           (draw dc (first x-and-y) (second x-and-y))
           (rotate -90)))

    ;; Rotate the patch by given number of degrees.
    (define/public (rotate degrees)
      (with-translation (- (/ size 2)) (- (/ size 2))
        (send path rotate (degrees->radians degrees))))

    (super-new)))

;; 16 basic patch shapes from Don Park's description at
;; <http://www.docuverse.com/blog/donpark/2007/01/19/identicon-updated-and-source-released>.
(define patch-shapes
  '(((0 0) (1 0) (1 1) (0 1))
    ((0 0) (1 0) (0 1))
    ((0 1) (1/2 0) (1 1))
    ((0 0) (1/2 0) (1/2 1) (0 1))
    ((1/2 0) (1 1/2) (1/2 1) (0 1/2))
    ((0 0) (1 1/2) (1 1) (1/2 1))
    ((1/2 0) (1 1) (0 1) (1/4 1/2) (1/2 1) (3/4 1/2) (1/4 1/2))
    ((0 0) (1 1/2) (1/2 1))
    ((1/4 1/4) (3/4 1/4) (3/4 3/4) (1/4 3/4))
    ((1/2 0) (1 0) (0 1) (0 1/2) (1/2 1/2) (1/2 0))
    ((0 0) (1/2 0) (1/2 1/2) (0 1/2))
    ((0 1/2) (1 1/2) (1/2 1))
    ((0 1) (1/2 1/2) (1 1))
    ((1/2 0) (1/2 1/2) (0 1/2))
    ((0 0) (1/2 0) (0 1/2))
    ()))
(define center-patch-shapes
  (list-refs patch-shapes 0 4 8 15))

;; Identicon is a graphical object with properties generated from a single
;; 32-bit number.
;;
;; Please note that 32 bits may not be enough for bigger sets of data. In
;; a community of no more than about 9.000 users chance of an overlap is less
;; than 1%. Chance of an overlap reaches 50% with about 77.000 users.
;; See <http://scott.sherrillmix.com/blog/programmer/wp_monsterid-and-statistics/>
;; for an explanation.
(define identicon%
  (class object%
    (init-field seed)

    (field (32-bits            (make-bit-stream seed))
           (center-patch-shape (list-ref center-patch-shapes (32-bits 2)))
           (side-patch-shape   (list-ref patch-shapes (32-bits 4)))
           (side-rotation      (* -90 (32-bits 2)))
           (corner-patch-shape (list-ref patch-shapes (32-bits 4)))
           (corner-rotation    (* -90 (32-bits 2)))
           (color              (make-object color% (* 8 (32-bits 5)) (* 8 (32-bits 5)) (* 8 (32-bits 5))))
           (center-inversion   (zero? (32-bits 1)))
           (side-inversion     (zero? (32-bits 1)))
           (corner-inversion   (zero? (32-bits 1))))

    ;; Calculate optimal patch size based on the size of the dc<%>.
    (define (get-patch-size dc)
      (define identicon-size
        (let-values (((width height) (send dc get-size)))
          (min width height)))
      (/ identicon-size 3))

    ;; Initialize a new patch% object using given size, shape and inversion
    ;; attributes.
    (define (make-patch size shape inversion)
      (make-object patch% size shape (if inversion "white" color) (if inversion color "white")))

    ;; Draw the identicon on given dc using all available space. Since
    ;; identicons are square, that will be dc's width or height, whichever is
    ;; smaller.
    ;;
    ;; Note: dc will be cleared before drawing and its properties will be
    ;; altered.
    (define/public (draw dc)
      (define patch-size (get-patch-size dc))

      (define center-patch (make-patch patch-size center-patch-shape center-inversion))
      (define side-patch   (make-patch patch-size side-patch-shape   side-inversion))
      (define corner-patch (make-patch patch-size corner-patch-shape corner-inversion))

      (send side-patch   rotate side-rotation)
      (send corner-patch rotate corner-rotation)

      (send dc clear)
      (send dc set-smoothing 'smoothed)
      (send dc set-pen "black" 1 'transparent)

      (send corner-patch draw-rotating dc '((0 0) (2 0) (2 2) (0 2)))
      (send side-patch   draw-rotating dc '((1 0) (2 1) (1 2) (0 1)))
      (send center-patch draw dc 1 1))

    ;; Return a bitmap of given size with this identicon drawn on it.
    (define/public (on-bitmap size)
      (let ((bitmap (make-object bitmap% size size)))
        (draw (new bitmap-dc% (bitmap bitmap)))
        bitmap))

    ;; Save this identicon of the given size under the file-path.
    (define/public (save-to-file file-path size)
      (send (on-bitmap size)
            save-file
            (expand-user-path file-path)
            (file-extension->kind file-path)))

    ;; Display this identicon in a square frame of a given size.
    (define/public (display-in-frame size)
      (define (make-frame-dc)
        (let* ((frame (new frame% (label "Identicon") (width size) (height size)))
               (canvas (new canvas% (parent frame))))
          (send frame show #t)
          (sleep/yield 1)
          (send canvas get-dc)))
      (draw (make-frame-dc)))

    (super-new)))

;; Create an identicon using a random seed value.
(define (random-identicon)
  (make-object identicon% (random 4294967087)))

;; Save a random identicon under a filename derived from its seed value.
;; Procedure returns the generated filename.
(define (save-random-identicon)
  (let* ((identicon (random-identicon))
         (file-path (string-append "0x" (number->string (get-field seed identicon) 16) ".png")))
    (send identicon save-to-file file-path 30)
    file-path))

;; Display a random identicon in a new 300px frame.
(define (display-random-identicon)
  (send (random-identicon) display-in-frame 300))

(provide/contract
 [identicon% class?]
 [random-identicon (-> (is-a?/c identicon%))]
 [save-random-identicon (-> string?)]
 [display-random-identicon (-> void?)])
