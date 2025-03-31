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
(define game-width        512.0)
(define game-height       512.0)

(define game-x            0.0)
(define game-y            0.0)

(define canvas            (get-element-by-id "canvas"))
(define context           (get-context canvas "2d"))
(define game              (make-game image:background (make-rect 0.0 0.0 512.0 512.0)))
(define player            (make-player image:player (make-rect 202.0 170.0 54.0 87.0)))

;;
;; update
;;
;;
(define (update)
  (let ((r (player-rect player)))
    (set-rect-x! r (+ (rect-x r) game-x))
    (set-rect-y! r (+ (rect-y r) game-y)))

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
      (set! game-x (- 0 1)))
      
    ((string=? key key:right)
      (set! game-x 1))
    ((string=? key key:up)
      (set! game-y (- 0 1)))
    ((string=? key key:down)
      (set! game-y 1)))
    ))

;;
;; on-key-up event handler
;;
;;
(define (on-key-up event)
  (let ((key (keyboard-event-code event)))
    (cond
    ((string=? key key:left)
      (set! game-x 0)
      (set! game-y 0))
    ((string=? key key:right)
      (set! game-x 0)
      (set! game-y 0))
    ((string=? key key:up)
      (set! game-x 0)
      (set! game-y 0))
    ((string=? key key:down)
      (set! game-x 0)
      (set! game-y 0)))
    ))

;;
;; wire up the events and start
;;
;;
(set-element-width! canvas (inexact->exact game-width))
(set-element-height! canvas (inexact->exact game-height))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))
(request-animation-frame draw-callback)
(timeout update-callback dt)
