;;; counsel-spotify.el --- Control Spotify search and select music with Ivy.

;; Copyright (C)
;; Author: Lautaro García <https://github.com/Lautaro-Garcia>
;; Package: helm-spotify-plus
;; Package-Requires: ((emacs "24.4") (ivy "0.9.0"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Makes it easier to browse Spotify API from Emacs.
;;; Code:

(require 'url)
(require 'json)
(require 'ivy)

(defcustom counsel-spotify-spotify-api-url "http://api.spotify.com/v1"
  "Variable to define spotify API url."
  :type 'string :group 'counsel-spotify)


;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(defun counsel-spotify-alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (counsel-spotify-alist-get (cdr symbols)
                             (assoc (car symbols) alist))
    (cdr alist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spotify app integration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun counsel-spotify-action (action)
  "Tell Soptify app to perform the given ACTION."
  (shell-command (format "osascript -e 'tell application \"Spotify\" to %s'" action)))

(defun counsel-spotify-play-uri (uri)
  "Tell Spotify app to play the given URI."
  (counsel-spotify-action (format "play track %S" uri)))

(defun counsel-spotify-play-album (track)
  "Get the Spotify app to play the album for this TRACK."
  (counsel-spotify-play-uri (counsel-spotify-alist-get '(album uri) (get-text-property 0 'property track))))

(defun counsel-spotify-play-track (track)
  "Get the Spotify app to play the TRACK."
  (counsel-spotify-play-uri (cdr (assoc 'uri (get-text-property 0 'property track)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spotify API integration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun counsel-spotify-search-by-type (search-term type)
  "Return a query for SEARCH-TERM in the corresponding TYPE (track, album)."
  (format "%s/search?q=%s:%s&type=track" counsel-spotify-spotify-api-url type search-term))

(defun counsel-spotify-request (a-url)
  "Function to request an json given a correct A-URL."
  (with-current-buffer
      (url-retrieve-synchronously a-url)
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun counsel-spotify-decode-utf8 (string)
  "Function to decode the STRING due to the errors in some symbols."
   (decode-coding-string (string-make-unibyte string) 'utf-8))


;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting output ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun counsel-spotify-format-track (track)
  "Given a TRACK, return a a formatted string suitable for display."
  (let ((track-name   (counsel-spotify-decode-utf8 (counsel-spotify-alist-get '(name) track)))
        (track-length (/ (counsel-spotify-alist-get '(duration_ms) track) 1000))
        (album-name   (counsel-spotify-decode-utf8 (counsel-spotify-alist-get '(album name) track)))
        (artist-names (mapcar
                       (lambda (artist) (counsel-spotify-decode-utf8 (counsel-spotify-alist-get '(name) artist)))
                       (counsel-spotify-alist-get '(artists) track))))
    (format "(%d:%0.2d) %s - %s [%s]"
            (/ track-length 60) (mod track-length 60)
            track-name
            (mapconcat 'identity artist-names "/")
            album-name)))


;;;;;;;;;;;;;;;;;
;; Controllers ;;
;;;;;;;;;;;;;;;;;

(defun counsel-spotify-play ()
  "Start playing current track."
  (interactive)
  (counsel-spotify-action "play track"))

(defun counsel-spotify-toggle-play-pause ()
  "Toggle play or pause of the current track."
  (interactive)
  (counsel-spotify-action "playpause"))

(defun counsel-spotify-previous ()
  "Start playing previous song."
  (interactive)
  (counsel-spotify-action "previous track"))

(defun counsel-spotify-next ()
  "Start playing next song."
  (interactive)
  (counsel-spotify-action "next track"))

(defun counsel-spotify-do-search-artist (search-term)
  "Queries spotify API for the artist in SEARCH-TERM."
  (let ((url (counsel-spotify-search-by-type search-term "artist")))
    (counsel-spotify-request url)))

(defun counsel-spotify-do-search-track (search-term)
  "Queries spotify API for the track in SEARCH-TERM."
  (let ((url (counsel-spotify-search-by-type search-term "track")))
    (counsel-spotify-request url)))

(defun counsel-spotify-do-search-album (search-term)
  "Queries spotify API for the album in SEARCH-TERM."
  (let ((url (counsel-spotify-search-by-type search-term "album")))
    (counsel-spotify-request url)))

(defun counsel-spotify-search-track-formatted (search-term)
  "Helper function to format the output due to SEARCH-TERM."
  (if (< (length search-term) 3)
      '("More input required" . nil)
    (mapcar (lambda (track) (propertize (counsel-spotify-format-track track) 'property track))
            (counsel-spotify-alist-get '(tracks items) (counsel-spotify-do-search-track search-term)))))

(defun counsel-spotify-search-artist-formatted (search-term)
  "Helper function to format the output due to SEARCH-TERM."
  (if (< (length search-term) 3)
      '("More input required" . nil)
    (mapcar (lambda (track) (propertize (counsel-spotify-format-track track) 'property track))
            (counsel-spotify-alist-get '(tracks items) (counsel-spotify-do-search-artist search-term)))))

(defun counsel-spotify-search-album-formatted (search-term)
  "Helper function to format the output due to SEARCH-TERM."
  (if (< (length search-term) 3)
      '("More input required" . nil)
    (let ((albums (aref (counsel-spotify-alist-get '(tracks items) (counsel-spotify-do-search-album search-term)) 0)))
      (when (> (length albums) 0)
        (mapcar (lambda (album) (propertize (counsel-spotify-alist-get '(name) album) 'property album))
                (list (counsel-spotify-alist-get '(album) albums)))))))


;;;;;;;;;;;;;;;;;;;
;; Ivy interface ;;
;;;;;;;;;;;;;;;;;;;

(defun counsel-spotify-search-track ()
  "Bring Ivy frontend to choose and play a track."
  (interactive)
  (ivy-read "Search track: " #'counsel-spotify-search-track-formatted
            :dynamic-collection t
            :action 'counsel-spotify-play-track))

(defun counsel-spotify-search-artist ()
  "Bring Ivy frontend to choose and play an artist (sort of)."
  (interactive)
  (ivy-read "Search artist: " #'counsel-spotify-search-artist-formatted
            :dynamic-collection t
            :action 'counsel-spotify-play-track))

(defun counsel-spotify-search-album ()
  "Bring Ivy frontend to choose and play an album."
  (interactive)
  (ivy-read "Search album: " #'counsel-spotify-search-album-formatted
            :dynamic-collection t
            :action 'counsel-spotify-play-track))

(provide 'counsel-spotify)
;;; counsel-spotify.el ends here
