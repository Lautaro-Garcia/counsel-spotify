;;; counsel-spotify.el --- Control Spotify search and select music with Ivy.

;; Copyright (C)
;; Author: Lautaro García <https://github.com/Lautaro-Garcia>
;; Package: counsel-spotify
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

(defcustom counsel-spotify-backends-alist
  '((gnu/linux . ((action . counsel-spotify-action-linux)
                  (play-track . counsel-spotify-format-play-linux)
                  (play . "Play")
                  (toggle . "PlayPause")
                  (next . "Next")
                  (previous . "Previous")))
    (darwin . ((action . counsel-spotify-action-darwin)
               (play-track . counsel-spotify-format-play-darwin)
               (play . "play track")
               (toggle . "playpause")
               (next . "next track")
               (previous . "previous track"))))
  "Variable to map each platform with its backend functions.
Every patform should define a function that, given an string action,
should call Spotify to execute that action.
PLAY-TRACK is a function that, given an uri, returns a play action for that uri.
Every other entry in the alist is an action,
\(the action function will be called with it as a parameter)"
  :type 'alist :group 'counsel-spotify)


;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(defun counsel-spotify-alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (counsel-spotify-alist-get (cdr symbols)
                             (assoc (car symbols) alist))
    (cdr alist)))


;;;;;;;;;;;;;;;;;
;; OS backends ;;
;;;;;;;;;;;;;;;;;

(defun counsel-spotify-action-darwin (action)
  "Tell Soptify app to perform the given ACTION."
  (shell-command (format "osascript -e 'tell application \"Spotify\" to %s'" action)))

(defun counsel-spotify-format-play-darwin (uri)
  "Tell Spotify app to play the given URI."
  (format "play track %S" uri))

(defvar counsel-spotify-dbus-call "dbus-send  --session --type=method_call --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 "
  "Variable to hold the dbus call string.")

(defun counsel-spotify-action-linux (action)
  "Tell Soptify app to perform the given ACTION."
  (shell-command (format (concat counsel-spotify-dbus-call "org.mpris.MediaPlayer2.Player.%s") action)))

(defun counsel-spotify-format-play-linux (uri)
  "Tell Spotify app to play the given URI."
  (format "OpenUri \"string:%s\"" uri))

(defun counsel-spotify-action (action &optional uri)
  "Tell Soptify app to perform the given ACTION.
If it's a play-track action will play the corresponding URI."
  (let* ((backend (alist-get system-type counsel-spotify-backends-alist))
         (action-fn (alist-get 'action backend))
         (parameter (alist-get action backend)))
    (if (eq action 'play-track)
        (apply action-fn (list (apply parameter (list uri))))
      (apply action-fn (list parameter)))))

(defun counsel-spotify-play-album (track)
  "Get the Spotify app to play the album for this TRACK."
  (counsel-spotify-action 'play-track (counsel-spotify-alist-get '(album uri) (get-text-property 0 'property track))))

(defun counsel-spotify-play-track (track)
  "Get the Spotify app to play the TRACK."
  (counsel-spotify-action 'play-track (alist-get 'uri (get-text-property 0 'property track))))


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


;;;;;;;;;;;;;;;;;
;; Controllers ;;
;;;;;;;;;;;;;;;;;

(defun counsel-spotify-play ()
  "Start playing current track."
  (interactive)
  (counsel-spotify-action 'play))

(defun counsel-spotify-toggle-play-pause ()
  "Toggle play or pause of the current track."
  (interactive)
  (counsel-spotify-action 'toggle))

(defun counsel-spotify-previous ()
  "Start playing previous song."
  (interactive)
  (counsel-spotify-action 'previous))

(defun counsel-spotify-next ()
  "Start playing next song."
  (interactive)
  (counsel-spotify-action 'next))


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
