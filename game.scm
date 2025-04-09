;;; Copyright (C) 2025 Dark Overlord of Data <darkoverlordofdata@gmail.com>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Shmup Wars
;;; Oh Give Me Those Shmup Wars
;;;
;;; Code:

(use-modules (dom canvas)
             (dom document)
             (dom element)
             (dom event)
             (dom image)
             (dom media)
             (dom window)
             (hoot ffi)
             (hoot hashtables)
             (rnrs bytevectors)
             (ice-9 match)
             (math)
             (math rect)
             (math vector)
             (srfi srfi-9))

;;
;; Data types
;;
;;
(define-record-type <game>
  (make-game image rect)
  game?
  (image  game-image)
  (rect   game-rect))

(define-record-type <player>
  (make-player image rect)
  player?
  (image  player-image)
  (rect   player-rect))

(define f64-ref  bytevector-ieee-double-native-ref)
(define f64-set! bytevector-ieee-double-native-set!)

;;
;; Globals
;;
;;
(define image:background  (make-image "assets/images/background.png"))
(define image:player      (make-image "assets/images/spaceshipspr.png"))

(define key:left          "ArrowLeft")
(define key:right         "ArrowRight")
(define key:up            "ArrowUp")
(define key:down          "ArrowDown")
(define key:confirm       "Enter")

(define dt                (/ 1000.0 60.0)) ; aim for updating at 60Hz
(define game-width        380.0)
(define game-height       380.0)
(define *player-x*          0.0)
(define *player-y*          0.0)
(define *velocity*          2)

(define *canvas-scale*      0.0)
(define *canvas-width*    380.0)
(define *canvas-height*   380.0)


(define canvas            (get-element-by-id "canvas"))
(define context           (get-context canvas "2d")) 
;; TODO -                 (get-context canvas "webgl"))
(define game              (make-game image:background (make-rect 0.0 0.0 (inexact->exact game-width) (inexact->exact game-height))))
(define player            (make-player image:player (make-rect 202.0 170.0 54.0 87.0)))

;;
;; update
;;
;;
(define (update)
  (let ((r (player-rect player)))
    (set-rect-x! r (+ (rect-x r) *player-x*))
    (set-rect-y! r (+ (rect-y r) *player-y*)))

  (timeout update-callback dt))
(define update-callback (procedure->external update))

;;
;; draw
;;
;;
(define (draw prev-time)
  (let ((g (game-rect game)))
    (draw-image context image:background
                    0.0 0.0 (rect-width g) (rect-height g)
                    (rect-x g) (rect-y g) (rect-width g) (rect-height g))

    (let ((r (player-rect player)))
      (draw-image context image:player
                    0.0 0.0 (rect-width r) (rect-height r)
                    (rect-x r) (rect-y r) (rect-width r) (rect-height r)))
  )
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;;
;; on-key-down event handler
;;
;;
(define (on-key-down event)
  (let ((key (keyboard-event-code event)))
    (cond
    ((string=? key key:left)
      (set! *player-x* (- 0 *velocity*)))
    ((string=? key key:right)
      (set! *player-x* *velocity*))
    ((string=? key key:up)
      (set! *player-y* (- 0 *velocity*)))
    ((string=? key key:down)
      (set! *player-y* *velocity*)))
    ))

;;
;; on-key-up event handler
;;
;;
(define (on-key-up event)
  (let ((key (keyboard-event-code event)))
    (cond
    ((string=? key key:left)
      (set! *player-x* 0))
    ((string=? key key:right)
      (set! *player-x* 0))
    ((string=? key key:up)
      (set! *player-y* 0))
    ((string=? key key:down)
      (set! *player-y* 0)))
    ))

;;
;; on-input-down event handler
;;
;;
(define (on-input-down input)
     (match input
        ('left (
          (set! *player-x* (- 0 *velocity*))))
        ('right (
          (set! *player-x* *velocity*)))
        ('up (
          (set! *player-y* (- 0 *velocity*))))
        ('down (
          (set! *player-y* *velocity*)))))

;;
;; on-input-up event handler
;;
;;
(define (on-input-up input)
     (match input
        ('left (
          (set! *player-x* 0)))
        ('right (
          (set! *player-x* 0)))
        ('up (
          (set! *player-y* 0)))
        ('down (
          (set! *player-y* 0)))))

;;
;; resize-canvas event handler
;;
;;
(define (resize-canvas)
  (let* ((win (current-window))
         (w (window-inner-width win))
         (h (window-inner-height win))
         (gw (inexact->exact game-width))
         (gh (inexact->exact game-height))
         (scale (max (min (quotient w gw) (quotient h gh)) 1))
         (cw (* gw scale))
         (ch (* gh scale)))
    (set-element-width! canvas cw)
    (set-element-height! canvas ch)
    (set-image-smoothing-enabled! context 0)
    (set! *canvas-scale* (exact->inexact scale))
    (set! *canvas-width* (* game-width *canvas-scale*))
    (set! *canvas-height* (* game-height *canvas-scale*))))

;;
;; add event handlers
;;
;;
(add-event-listener! (current-window) "resize"
                     (procedure->external (lambda (_) (resize-canvas))))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))


(define (register-mouse-control-down elem-id input-id)
  (add-event-listener! (get-element-by-id elem-id) "mousedown"
                       (procedure->external
                        (lambda (e) (on-input-down input-id)))))

(define (register-touch-control-start elem-id input-id)
  (add-event-listener! (get-element-by-id elem-id) "touchstart"
                       (procedure->external
                        (lambda (e) (on-input-down input-id)))))

(define (register-mouse-control-up elem-id input-id)
  (add-event-listener! (get-element-by-id elem-id) "mouseup"
                       (procedure->external
                        (lambda (e) (on-input-up input-id)))))

(define (register-touch-control-cancel elem-id input-id)
  (add-event-listener! (get-element-by-id elem-id) "touchcancel"
                       (procedure->external
                        (lambda (e) (on-input-up input-id)))))

(define (register-touch-control-end elem-id input-id)
  (add-event-listener! (get-element-by-id elem-id) "touchend"
                       (procedure->external
                        (lambda (e) (on-input-up input-id)))))

(define (register-events registration-id)
  (registration-id    "dpad-left"   'left)
  (registration-id    "dpad-right"  'right)
  (registration-id    "dpad-down"   'down)
  (registration-id    "dpad-up"     'up)
  (registration-id    "button-a"    'undo))
  
(register-events register-mouse-control-down)
(register-events register-touch-control-start)
(register-events register-mouse-control-up)
(register-events register-touch-control-cancel)
(register-events register-touch-control-end)

(set-element-width! canvas (inexact->exact game-width))
(set-element-height! canvas (inexact->exact game-height))
(resize-canvas)

(request-animation-frame draw-callback)
(timeout update-callback dt)
