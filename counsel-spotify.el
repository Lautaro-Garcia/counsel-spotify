;;; counsel-spotify.el --- Control Spotify search and select music with Ivy -*- lexical-binding: t; -*-

;; Copyright (C)
;; Author: Lautaro García <https://github.com/Lautaro-Garcia>
;; URL: https://github.com/Lautaro-Garcia/counsel-spotify
;; Package: counsel-spotify
;; Package-Requires: ((emacs "25.1") (ivy "0.13.0"))
;; Version: 0.4.0

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
(require 'dbus)

(defgroup  counsel-spotify nil
  "Customs for `counsel-spotify'"
  :group 'applications)

(defcustom counsel-spotify-spotify-api-url "https://api.spotify.com/v1"
  "Variable to define spotify API url."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-api-authentication-url "https://accounts.spotify.com/api/token"
  "Variable to define spotify API url for getting the access token."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-client-id ""
  "Spotify application client ID."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-client-secret ""
  "Spotify application client secret."
  :type 'string :group 'counsel-spotify)


(defcustom counsel-spotify-service-name "spotify"
  "Name of the DBUS service used by the client we talk to.

The official Spotify client uses `spotify', but one can also use
alternative clients such as mopidy or spotifyd."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-use-system-bus-p nil
  "Whether to access the spotify client using the system DBUS.

Some clients, such as mopidy, can run as system services."
  :type 'boolean :group 'counsel-spotify)


(defcustom counsel-spotify-use-notifications t
  "Notify playback changes via DBUS (only for backends that support DBUS)."
  :type 'boolean :group 'counsel-spotify)

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(defun counsel-spotify-verify-credentials ()
  "Tell the user that the credentials are not set."
  (when (or (string= counsel-spotify-client-id "") (string= counsel-spotify-client-secret ""))
    (error "The variables counsel-spotify-client-id or counsel-spotify-client-secret are undefined and both are required to authenticate to the Spotify API.  See https://developer.spotify.com/my-applications")))


(defun counsel-spotify-basic-auth-credentials ()
  "Return the Basic auth string that should be sent to ask for an auth token."
  (concat "Basic " (base64-encode-string (concat counsel-spotify-client-id ":" counsel-spotify-client-secret) t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spotify API integration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass counsel-spotify-playable ()
  ((uri :initarg :uri :initform "" :reader uri)))

(defclass counsel-spotify-album (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)
   (artist-name :initarg :artist-name :initform "" :reader artist-name)))

(defclass counsel-spotify-track (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)
   (artist :initarg :artist :initform "" :reader artist)
   (album :initarg :album :initform "" :reader album)
   (duration-in-ms :initarg :duration :initform 0 :reader duration-in-ms)))

(defclass counsel-spotify-artist (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)))

(defclass counsel-spotify-playlist (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)))

(cl-defmacro counsel-spotify-with-auth-token ((auth-variable) &body body)
  "Execute the BODY with the AUTH-TOKEN-VARIABLE bound to the Spotify's auth token for the current user."
  `(let ((url-request-method "POST")
         (url-request-data "&grant_type=client_credentials")
         (url-request-extra-headers (list (cons "Content-Type" "application/x-www-form-urlencoded")
                                          (cons "Authorization" ,(counsel-spotify-basic-auth-credentials)))))
     (url-retrieve counsel-spotify-spotify-api-authentication-url
                   (lambda (status)
                     (goto-char url-http-end-of-headers)
                     (let ((,auth-variable (alist-get 'access_token (json-read))))
                       ,@body)))))

(cl-defmacro counsel-spotify-with-query-results ((auth-token query-url results-variable) &body body)
  "Execute the BODY with the results of an api call to QUERY-URL with an AUTH-TOKEN bound to RESULTS-VARIABLE."
  `(let ((url-request-extra-headers (list (cons "Authorization" (concat "Bearer " ,auth-token)))))
     (url-retrieve ,query-url
                   (lambda (status)
                     (goto-char url-http-end-of-headers)
                     (let ((,results-variable (json-read)))
                       ,@body)))))

(cl-defun counsel-spotify-make-query (&key album artist playlist track (type 'track))
  "Create a new query url."
  (if (or artist album playlist track)
      (concat counsel-spotify-spotify-api-url
              "/search?q="
              (when artist (format "artist:%s" artist))
              (when album (format " album:%s" album))
              (when track (format " track:%s" track))
              (when playlist (format "%s" playlist))
              (when type (format "&type=%s" (symbol-name type))))
    (error "Must supply at least an artist or an album or a track to search for")))

(cl-defun counsel-spotify-search (&rest rest)
  "Search something in Spotify, based on the query described in REST."
  (let ((type (or (plist-get rest :type) 'track))
        (query-url (apply #'counsel-spotify-make-query rest)))
    (counsel-spotify-with-auth-token (auth-token)
      (counsel-spotify-with-query-results (auth-token query-url results)
        (counsel-spotify-update-ivy-candidates type results)))))

(cl-defgeneric counsel-spotify-builder (type spotify-alist-response)
  "Builds the TYPE object from the SPOTIFY-ALIST-RESPONSE")

(cl-defmethod counsel-spotify-builder ((type (eql track)) spotify-alist-response)
  (mapcar
   (lambda (tr)
     (let ((artist (elt (alist-get 'artists tr) 0))
           (album (alist-get 'album tr)))
       (make-instance 'counsel-spotify-track
                      :name (alist-get 'name tr)
                      :uri (alist-get 'uri tr)
                      :artist (make-instance 'counsel-spotify-artist :name (alist-get 'name artist) :uri (alist-get 'uri artist))
                      :album (make-instance 'counsel-spotify-album :name (alist-get 'name album) :artist-name (alist-get 'name artist) :uri (alist-get 'uri album))
                      :duration (alist-get 'duration_ms tr))))
   (alist-get 'items (alist-get 'tracks spotify-alist-response))))

(cl-defmethod counsel-spotify-builder ((type (eql album)) spotify-alist-response)
  (mapcar
   (lambda (a) (make-instance 'counsel-spotify-album
                              :name (alist-get 'name a)
                              :artist-name (alist-get 'name (elt (alist-get 'artists a) 0))
                              :uri (alist-get 'uri a)))
   (alist-get 'items (alist-get 'albums spotify-alist-response))))

(cl-defmethod counsel-spotify-builder ((type (eql artist)) spotify-alist-response)
  (mapcar
   (lambda (a) (make-instance 'counsel-spotify-artist
                              :name (alist-get 'name a)
                              :uri (alist-get 'uri a)))
   (alist-get 'items (alist-get 'artists spotify-alist-response))))

(cl-defmethod counsel-spotify-builder ((type (eql playlist)) spotify-alist-response)
  (mapcar
   (lambda (a) (make-instance 'counsel-spotify-playlist
                              :name (alist-get 'name a)
                              :uri (alist-get 'uri a)))
   (alist-get 'items (alist-get 'playlists spotify-alist-response))))


;;;;;;;;;;;;;;;;;
;; OS backends ;;
;;;;;;;;;;;;;;;;;

(defclass counsel-spotify-backend-commands ()
  ((play-command :initarg :play :initform "" :reader play)
   (playpause-command :initarg :playpause :initform "" :reader playpause)
   (next-command :initarg :next :initform "" :reader next)
   (previous-command :initarg :previous :initform "" :reader previous)))

(defclass counsel-spotify-backend ()
  ((commands :initarg :commands :reader commands)))

(defclass counsel-spotify-darwin-backend (counsel-spotify-backend)
  ((commands :initform (make-instance 'counsel-spotify-backend-commands :play "play track" :playpause "playpause" :next "next track" :previous "previous track"))))

(defclass counsel-spotify-linux-backend (counsel-spotify-backend)
  ((commands :initform (make-instance 'counsel-spotify-backend-commands :play "Play" :playpause "PlayPause" :next "Next" :previous "Previous"))))

(defvar counsel-spotify-current-backend
  (pcase system-type
    ('gnu/linux (make-instance 'counsel-spotify-linux-backend))
    ('darwin    (make-instance 'counsel-spotify-darwin-backend))))

(cl-defun counsel-spotify-call-spotify-via-dbus (method &rest args)
  (apply #'dbus-call-method `(,(if counsel-spotify-use-system-bus-p :system :session)
                              ,(format "org.mpris.MediaPlayer2.%s" counsel-spotify-service-name)
                              "/org/mpris/MediaPlayer2"
                              "org.mpris.MediaPlayer2.Player"
                              ,method
                              ,@args)))

(cl-defgeneric counsel-spotify-tell-backend-to (backend action)
  "Tells the given BACKEND to execute the given ACTION")

(cl-defmethod counsel-spotify-tell-backend-to ((backend counsel-spotify-darwin-backend) action)
  (shell-command (concat "osascript -e 'tell application \"Spotify\" to '" (shell-quote-argument (funcall action (commands backend))))))

(cl-defmethod counsel-spotify-tell-backend-to ((backend counsel-spotify-linux-backend) action)
  (counsel-spotify-call-spotify-via-dbus (funcall action (commands backend))))

(cl-defgeneric counsel-spotify-do-play (backend playable)
  "Tells the BACKEND to play the PLAYABLE object")

(cl-defmethod counsel-spotify-do-play ((backend counsel-spotify-darwin-backend) (playable counsel-spotify-playable))
  (shell-command (concat "osascript -e 'tell application \"Spotify\" to play track \"" (uri playable) "\"'")))

(cl-defmethod counsel-spotify-do-play ((backend counsel-spotify-linux-backend) (playable counsel-spotify-playable))
  (counsel-spotify-call-spotify-via-dbus "OpenUri" (uri playable)))

(defun counsel-spotify-unwrap-property (elem)
  "Unwrap the property of ELEM."
  (get-text-property 0 'property elem))

(defun counsel-spotify-play-property (elem)
  "Call play on an unwrapped ELEM."
  (counsel-spotify-do-play counsel-spotify-current-backend (counsel-spotify-unwrap-property elem)))


;;;;;;;;;;;;;;;;;;;
;; Notifications ;;
;;;;;;;;;;;;;;;;;;;

(defun counsel-spotify-get-dbus-spotify-metadata-for (event property)
  "Scrap Spotify's dbus EVENT for information about PROPERTY."
  (caadr (assoc (format "xesam:%s" property) (caadr (assoc "Metadata" event)))))


(defun counsel-spotify-handle-player-change (interface-name changed-properties invalidated-properties)
  "DBUS event handler for Spotify's Player ProertiesChanged event. The signal has an INTERFACE-NAME, an CHANGED-PROPERTIES and an INVALIDATED-PROPERTIES."
  (let* ((playback-status (caadr (assoc "PlaybackStatus" changed-properties)))
         (track-name (counsel-spotify-get-dbus-spotify-metadata-for changed-properties "title"))
         (artist     (car (counsel-spotify-get-dbus-spotify-metadata-for changed-properties "artist")))
         (album      (counsel-spotify-get-dbus-spotify-metadata-for changed-properties "album")))
    (message "(Spotify %s) %s - %s [%s]" playback-status artist track-name album)))

(cl-defgeneric counsel-spotify-notify-playback-changes (backend)
  "Notify playback changes")

(cl-defmethod counsel-spotify-notify-playback-changes ((backend counsel-spotify-backend))
  :backend-doesnt-support-notifications)

(cl-defmethod counsel-spotify-notify-playback-changes ((backend counsel-spotify-linux-backend))
  (dbus-register-signal :session "org.mpris.MediaPlayer2.spotify" "/org/mpris/MediaPlayer2" "org.freedesktop.DBus.Properties" "PropertiesChanged" #'counsel-spotify-handle-player-change)
  :notifications-set-up)

(counsel-spotify-notify-playback-changes counsel-spotify-current-backend)


;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting output ;;
;;;;;;;;;;;;;;;;;;;;;;;

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

(defun counsel-spotify-update-ivy-candidates (type-of-response completions)
  "Tell Ivy to update the minibuffer candidates with the COMPLETIONS list of playable objects of type TYPE-OF-RESPONSE."
  (ivy-update-candidates (mapcar #'counsel-spotify-get-formatted-object (counsel-spotify-builder type-of-response completions))))

(defmacro counsel-spotify-search-by (search-keyword &rest search-args)
  "Create the function to search by SEARCH-KEYWORD and other SEARCH-ARGS."
  `(lambda (search-term) (counsel-spotify-search ,search-keyword search-term ,@search-args) 0))

;;;###autoload
(defun counsel-spotify-search-track ()
  "Bring Ivy frontend to choose and play a track."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search track: " (counsel-spotify-search-by :track)
            :dynamic-collection t
            :action '(1
                      ("p" counsel-spotify-play-property "Play track")
                      ("a" (lambda (elem) (counsel-spotify-do-play counsel-spotify-current-backend (album (counsel-spotify-unwrap-property elem)))) "Play album")
                      ("A" (lambda (elem) (counsel-spotify-do-play counsel-spotify-current-backend (artist (counsel-spotify-unwrap-property elem)))) "Play artist"))))

;;;###autoload
(defun counsel-spotify-search-artist ()
  "Bring Ivy frontend to choose and play an artist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Seach artist: " (counsel-spotify-search-by :artist :type 'artist) :dynamic-collection t :action #'counsel-spotify-play-property))

;;;###autoload
(defun counsel-spotify-search-playlist ()
  "Bring Ivy frontend to choose and play a playlist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Seach playlist: " (counsel-spotify-search-by :playlist :type 'playlist) :dynamic-collection t :action #'counsel-spotify-play-property))

;;;###autoload
(defun counsel-spotify-search-album ()
  "Bring Ivy frontend to choose and play an album."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search album: " (counsel-spotify-search-by :album :type 'album) :dynamic-collection t :action #'counsel-spotify-play-property))

;;;###autoload
(defun counsel-spotify-search-tracks-by-artist ()
  "Bring Ivy frontend to search for all tracks for a given artist."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search tracks by artist: " (counsel-spotify-search-by :artist :type 'track) :dynamic-collection t :action #'counsel-spotify-play-property))

;;;###autoload
(defun counsel-spotify-search-tracks-by-album ()
  "Bring Ivy frontend to search for all track on a given album."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search tracks by album: " (counsel-spotify-search-by :album :type 'track) :dynamic-collection t :action #'counsel-spotify-play-property))

(provide 'counsel-spotify)
;;; counsel-spotify.el ends here
