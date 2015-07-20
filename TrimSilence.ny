;nyquist plug-in
;version 3
;type process
;categories "http://lv2plug.in/ns/lv2core/#UtilityPlugin"
;name "Trim Silence..."
;action "Trimming..."
;info "by Steve Daulton (www.easyspacepro.com). Released under GPL v2.\n\nTrims silence from the beginning and end of the selection.\n"

;control thresh "Silence Threshold (dB)" real "" -48 -100 0
;control beginning "Leave at start (s)" real "" 0 -100 100
;control ending "Leave at end (s)" real "" 0 -100 100

;; TrimSilence.ny by Steve Daulton. Aug 2011.
;; Updated 20 July 2015.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;; Requires Audacity 1.3.8 or later.

;; RAM USAGE:
;; This plug-in requires the audio to be loaded into RAM.
;; If there is insufficient free RAM Audacity may freeze or crash.
;; The line below limits RAM usage to 1.0 GB (about 47 minutes for
;; a stereo track at 44.1 kHz)
;; If your computer has more than 1GB of physical RAM available, the
;; limit may be increased.

(setq RAM-Limit 1.0) ; RAM limit in GB

; convert threhold to linear
(setq thresh (db-to-linear (min 0 thresh)))

;; Limit of duration in seconds
(setq limit
  (/ (* ram-limit 1000000000)
    (* 4.0 *sound-srate*)))
(when (arrayp s)(setq limit (/ limit 2.0)))

;;; modulo
(defun mod (x y)
  (setq y (float y))
  (round (* y
    (- (/ x y)
      (truncate (/ x y))))))
  
;;; convert to hh:mm:ss
(defun to-hhmmss (seconds)
  (let* ((hh (truncate (/ seconds 3600)))
        (mm (truncate (/ (mod seconds 3600) 60)))
        (ss (mod seconds 60)))
    (format nil "~ah:~am:~as" hh mm ss)))

;;; convert to mono and limit sample rate
(defun convert (sig ratio)
  (if (arrayp s)
      (snd-avg 
        (s-max (snd-abs (aref sig 0))
          (snd-abs (aref sig 1)))
        ratio ratio op-peak)
      (snd-avg sig ratio ratio op-peak)))

;;; find silences
(defun find-sil (sig &aux (start 0)(end 0))
  (do ((new (snd-fetch sig) (snd-fetch sig))
       (flag 0))
      ((not new))
    (if (= flag 0)
        ;; count initial silence
        (if (<= new thresh)
            (setq start (1+ start))
            (setq flag 1))
        ;; count final silence
        (if (<= new thresh)
            (setq end (1+ end))
            (setq end 0))))
  (list start end))


(if (< len (* limit *sound-srate*)) ;max length in samples
  (let* ((start 0)
         (end 0)
         (flag 0)
         ;; ratio provides tighter trimming for short selections
         ;; while maintaining reasonable speed for long selections
         (ratio (max 10 (min 200 (round (/ len 100000.0)))))
         (my-srate (/ *sound-srate* ratio))
         (mysound (convert s ratio)))

    ;loop through samples and mark start and end
    (setf result (find-sil mysound))

    (let ((start (/ (first result) my-srate))
          (end (- (get-duration 1)(/ (second result) my-srate))))
      (setq start (- start beginning))
      ;; ensure at least 1 sample remains
      (if (>= start (get-duration 1))
        (setq start (/ (1- len) *sound-srate*)))
      ; try to leave desired ending silence and trim
      (if (<= (+ end ending) (get-duration 1))
          (setq end (+ end ending))
          (setq end (get-duration 1)))
      (multichan-expand #'extract-abs start end (cue s))))

  ;; OR print error message
  (format nil "Error.\nMax RAM usage by Trim Silence is set to ~a GB.~%This allows a maximum duration ~
              for a ~a~%track at ~a Hz of ~a.~%Selected track is ~a.~%"
              RAM-limit
              (if (arrayp s) "stereo" "mono")
              (round *sound-srate*)
              (to-hhmmss limit)
              (to-hhmmss (get-duration 1))))
