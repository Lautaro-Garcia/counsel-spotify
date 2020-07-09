(require 'counsel-spotify-messages)
(require 'counsel-spotify-backends)

(defun as-utf8 (a-string)
  (decode-coding-string (string-make-unibyte a-string) 'utf-8))

(ert-deftest it-formats-an-album-object ()
  (let* ((album (make-instance 'counsel-spotify-album :name "Album" :artist-name "Artist"))
         (album-as-string (counsel-spotify-format album)))
    (should (equal album-as-string "Artist - Album"))
    (should (equal (counsel-spotify-unwrap-spotify-object album-as-string) album))))

(ert-deftest it-formats-a-track-object ()
  (let* ((track (make-instance 'counsel-spotify-track
                              :name "Track"
                              :artist (make-instance 'counsel-spotify-playable :name "Artist")
                              :album (make-instance 'counsel-spotify-album :name "Album")
                              :duration 61000))
         (track-as-string (counsel-spotify-format track)))
    (should (equal track-as-string "(1:01) Artist - Track [Album]"))
    (should (equal (counsel-spotify-unwrap-spotify-object track-as-string) track))))

(ert-deftest it-formats-a-playable-object ()
  (let* ((playable (make-instance 'counsel-spotify-playable :name "Something"))
         (playable-as-string (counsel-spotify-format playable)))
    (should (equal playable-as-string "Something"))
    (should (equal (counsel-spotify-unwrap-spotify-object playable-as-string) playable))))

(ert-deftest it-formats-objects-displaying-accents ()
  (let ((playable (make-instance 'counsel-spotify-playable :name "Você"))
        (album (make-instance 'counsel-spotify-album :name "Niño" :artist-name "Não")))
    (should (equal (counsel-spotify-format playable) (as-utf8 "Você")))
    (should (equal (counsel-spotify-format album) (as-utf8 "Não - Niño")))))
