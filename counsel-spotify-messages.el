(require 'counsel-spotify-search)

(cl-defgeneric counsel-spotify-format (element)
  "Format an ELEMENT to be shown in the minibuffer.")

(cl-defmethod counsel-spotify-format :around (element)
  (propertize (decode-coding-string (string-make-unibyte (cl-call-next-method)) 'utf-8)
              'spotify-object element))

(cl-defmethod counsel-spotify-format ((playable counsel-spotify-playable))
  (name playable))

(cl-defmethod counsel-spotify-format ((track counsel-spotify-track))
  (let* ((seconds-of-song (/ (duration-in-ms track) 1000.0))
         (second-left-in-song (% (round seconds-of-song) 60))
         (minutes-in-song (truncate (/ seconds-of-song 60))))
    (format "(%d:%0.2d) %s - %s [%s]"
            minutes-in-song
            second-left-in-song
            (name (artist track))
            (name track)
            (name (album track)))))

(cl-defmethod counsel-spotify-format ((album counsel-spotify-album))
  (format "%s - %s" (artist-name album) (name album)))

(provide 'counsel-spotify-messages)
