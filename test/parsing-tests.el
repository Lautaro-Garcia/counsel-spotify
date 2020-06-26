(require 'counsel-spotify-search)
(require 'spotify-responses-tests)

(ert-deftest it-parses-an-artist-response ()
  (should (equal (counsel-spotify-parse-response (json-read-from-string artist-spotify-response))
                 (list (make-instance 'counsel-spotify-playable
                                      :uri "spotify:artist:3Ujv6sa60JRiaxS8RVuNOj"
                                      :name "Tuyo")))))

(ert-deftest it-parses-an-album-response ()
  (should (equal (counsel-spotify-parse-response (json-read-from-string album-spotify-response))
                 (list (make-instance 'counsel-spotify-album
                                      :uri "spotify:album:3qzmmmRmVBiOuMvrerfW4z"
                                      :name "Choose Your Weapon"
                                      :artist-name "Hiatus Kaiyote")))))

(ert-deftest it-parses-a-track-response ()
  (let ((jacob-collier (make-instance 'counsel-spotify-playable :name "Jacob Collier" :uri "spotify:artist:0QWrMNukfcVOmgEU0FEDyD"))
        (in-my-room (make-instance 'counsel-spotify-album :name "In My Room" :artist-name "Jacob Collier" :uri "spotify:album:4yVaTn71IU28NA0Et4bRED")))
    (should (equal (counsel-spotify-parse-response (json-read-from-string track-spotify-response))
                   (list (make-instance 'counsel-spotify-track
                                        :uri "spotify:track:5ciCKiLGTIgfHKV4MxjInt"
                                        :name "Hajanga"
                                        :artist jacob-collier
                                        :album in-my-room
                                        :duration 362996))))))

(ert-deftest it-parses-a-playlist-response ()
  (let ((atilla-playlist (make-instance 'counsel-spotify-playable :name "Atilla" :uri "spotify:playlist:6ROecAB4Ksv6MJKyhFchKH")))
    (should (equal (counsel-spotify-parse-response (json-read-from-string playlist-spotify-response))
                   (list atilla-playlist)))))

(ert-deftest it-parses-an-episode-response ()
  (let ((episode-414 (make-instance 'counsel-spotify-playable
                                    :name "414: The Front-End Ceiling, Emacs, Permissions, and Writing Better Words"
                                    :uri "spotify:episode:4v7TCaTDO9US5bgY24D4ks")))
    (should (equal (counsel-spotify-parse-response (json-read-from-string episode-spotify-response))
                   (list episode-414)))))

(ert-deftest it-parses-a-show-object ()
  (let ((emacscast (make-instance 'counsel-spotify-playable
                                  :name "EmacsCast"
                                  :uri "spotify:show:55FtiTIlDijMqPPa6rGUti")))
    (should (equal (counsel-spotify-parse-response (json-read-from-string show-spotify-response))
                   (list emacscast)))))

(ert-deftest it-parses-multiple-objects-of-the-same-type-in-a-single-response ()
  (let ((emacscast (make-instance 'counsel-spotify-playable
                                  :name "EmacsCast"
                                  :uri "spotify:show:55FtiTIlDijMqPPa6rGUti"))
        (emack (make-instance 'counsel-spotify-playable
                              :name "Emack (El Brody ) Ramírez "
                              :uri "spotify:show:1M7pvExXz0Se3vmrtTHNb9")))
    (should (equal (counsel-spotify-parse-response (json-read-from-string two-shows-spotify-response))
                   (list emacscast emack)))))

(ert-deftest it-parses-multiple-objects-of-multiple-types ()
  (let ((nakamura (make-instance 'counsel-spotify-album
                                 :name "NAKAMURA"
                                 :uri "spotify:album:3jqQFIXUakuDXdhFVvI7Ko"
                                 :artist-name "Aya Nakamura"))
        (nakamarra (make-instance 'counsel-spotify-playable
                                  :name "Nakamarra"
                                  :uri "spotify:artist:6dX1kzzeak2jp50G2vtKx1")))
    (should (equal (counsel-spotify-parse-response (json-read-from-string artist-and-album-spotify-response))
                   (list nakamura nakamarra)))))
