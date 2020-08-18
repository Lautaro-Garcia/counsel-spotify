;;; counsel-spotify.el --- Control Spotify search and select music with Ivy -*- lexical-binding: t; -*-

;; Copyright (C)
;; Author: Lautaro García <https://github.com/Lautaro-Garcia>
;; URL: https://github.com/Lautaro-Garcia/counsel-spotify
;; Package: counsel-spotify
;; Package-Requires: ((emacs "25.1") (ivy "0.13.0"))
;; Version: 0.5.2

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

(require 'ivy)
(require 'counsel-spotify-search)
(require 'counsel-spotify-backends)
(require 'counsel-spotify-notifications)
(require 'counsel-spotify-messages)

(defgroup  counsel-spotify nil
  "Customs for `counsel-spotify'"
  :group 'applications)


;;;;;;;;;;;;;;;;;
;; Controllers ;;
;;;;;;;;;;;;;;;;;

;;;###autoload
(defun counsel-spotify-play ()
  "Start playing current track."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'play))

;;;###autoload
(defun counsel-spotify-toggle-play-pause ()
  "Toggle play or pause of the current track."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'playpause))

;;;###autoload
(defun counsel-spotify-previous ()
  "Start playing previous song."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'previous))

;;;###autoload
(defun counsel-spotify-next ()
  "Start playing next song."
  (interactive)
  (counsel-spotify-tell-backend-to counsel-spotify-current-backend #'next))


;;;;;;;;;;;;;;;;;;;
;; Ivy interface ;;
;;;;;;;;;;;;;;;;;;;

(defun counsel-spotify-update-ivy-candidates (list-of-counsel-spotify-objects)
  "Tell Ivy to update the minibuffer candidates with the LIST-OF-COUNSEL-SPOTIFY-OBJECTS."
  (ivy-update-candidates (mapcar #'counsel-spotify-format list-of-counsel-spotify-objects)))

(defmacro counsel-spotify-search-by (&rest search-args)
  "Create the function to search by SEARCH-KEYWORD and other SEARCH-ARGS."
  `(lambda (search-term)
     (counsel-spotify-search #'counsel-spotify-update-ivy-candidates search-term ,@search-args)
     0))

;;;###autoload
(defun counsel-spotify-search-track ()
  "Bring Ivy frontend to choose and play a track."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search track: " (counsel-spotify-search-by :type '(track))
            :dynamic-collection t
            :action '(1
                      ("p" counsel-spotify-play-string "Play track")
                      ("a" (lambda (elem) (counsel-spotify-do-play counsel-spotify-current-backend (album (counsel-spotify-unwrap-spotify-object elem)))) "Play album")
                      ("A" (lambda (elem) (counsel-spotify-do-play counsel-spotify-current-backend (artist (counsel-spotify-unwrap-spotify-object elem)))) "Play artist"))))

;;;###autoload
(defun counsel-spotify-search-artist ()
  "Bring Ivy frontend to choose and play an artist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search artist: " (counsel-spotify-search-by :type '(artist)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-search-playlist ()
  "Bring Ivy frontend to choose and play a playlist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search playlist: " (counsel-spotify-search-by :type '(playlist)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-search-album ()
  "Bring Ivy frontend to choose and play an album."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search album: " (counsel-spotify-search-by :type '(album)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-search-tracks-by-artist ()
  "Bring Ivy frontend to search for all tracks for a given artist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search tracks by artist: " (counsel-spotify-search-by :filter 'artist :type '(track)) :dynamic-collection t :action #'counsel-spotify-play-string))

;;;###autoload
(defun counsel-spotify-search-tracks-by-album ()
  "Bring Ivy frontend to search for all track on a given album."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search tracks by album: " (counsel-spotify-search-by :filter 'album :type '(track)) :dynamic-collection t :action #'counsel-spotify-play-string))

(provide 'counsel-spotify)
;;; counsel-spotify.el ends here
