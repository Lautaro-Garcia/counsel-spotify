(require 'counsel-spotify-search)

(ert-deftest it-can-make-a-query-with-a-term-and-a-single-type ()
  (should (equal (counsel-spotify-make-query "foo" :type '(track))
                 (concat counsel-spotify-spotify-api-url "/search?q=foo&type=track"))))

(ert-deftest it-can-make-a-query-with-a-term-and-multiple-types ()
  (should (equal (counsel-spotify-make-query "foo" :type '(track album))
                 (concat counsel-spotify-spotify-api-url "/search?q=foo&type=track,album"))))

(ert-deftest it-errors-when-no-type-is-provided ()
  (should-error (counsel-spotify-make-query "foo")))

(ert-deftest it-adds-a-filter-to-queried-term ()
  (should (equal (counsel-spotify-make-query "foo" :type '(track) :filter 'album)
                 (concat counsel-spotify-spotify-api-url "/search?q=album:foo&type=track"))))
