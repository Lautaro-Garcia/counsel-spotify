(require 'counsel-spotify-messages)

(ert-deftest it-formats-an-album-object ()
  (let ((album (make-instance 'counsel-spotify-album :name "Album" :artist-name "Artist")))
    (should (equal (counsel-spotify-format album)
                   "Artist - Album"))))

(ert-deftest it-formats-a-track-object ()
  (let ((track (make-instance 'counsel-spotify-track
                              :name "Track"
                              :artist (make-instance 'counsel-spotify-playable :name "Artist")
                              :album (make-instance 'counsel-spotify-album :name "Album")
                              :duration 61000)))
    (should (equal (counsel-spotify-format track)
                   "(1:01) Artist - Track [Album]"))))

(ert-deftest it-formats-a-playable-object ()
  (should (equal (counsel-spotify-format (make-instance 'counsel-spotify-playable :name "Something"))
                 "Something")))
