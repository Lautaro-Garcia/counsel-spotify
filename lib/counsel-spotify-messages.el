(require 'counsel-spotify-search)

(cl-defgeneric counsel-spotify-format (element)
  "Format an ELEMENT to be shown in the minibuffer.")

(cl-defmethod counsel-spotify-format :around (element)
  (decode-coding-string (string-make-unibyte (cl-call-next-method)) 'utf-8))

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

(cl-defmethod counsel-spotify-format ((artist counsel-spotify-artist))
  (name artist))

(cl-defmethod counsel-spotify-format ((playlist counsel-spotify-playlist))
  (name playlist))

(cl-defmethod counsel-spotify-format ((album counsel-spotify-album))
  (concat (artist-name album) " - " (name album)))

(defun counsel-spotify-get-formatted-object (element)
  "Return a string representation and it's corresponding ELEMENT as a property."
  (propertize (counsel-spotify-format element) 'property element))

(provide 'counsel-spotify-messages)
