
;;; counsel-spotify.el --- Control Spotify search and select music with Ivy -*- lexical-binding: t; -*-

;; Copyright (C)
;; Author: Lautaro García <https://github.com/Lautaro-Garcia>
;; URL: https://github.com/Lautaro-Garcia/counsel-spotify
;; Package: counsel-spotify
;; Package-Requires: ((emacs "25.1") (ivy "0.13.0") (elisp-oauth-2 "0.0.1") (dash "2.17.0"))
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
(require 'dash)
(require 'json)
(require 'ivy)
(require 'dbus)
(require 'elisp-oauth-2)

(defgroup  counsel-spotify nil
  "Customs for `counsel-spotify'"
  :group 'applications)

(defcustom counsel-spotify-spotify-api-url "https://api.spotify.com/v1"
  "Variable to define spotify API url."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-api-token-url "https://accounts.spotify.com/api/token"
  "Variable to define spotify API url for getting the access token."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-api-auth-url "https://accounts.spotify.com/authorize"
  "Variable to define spotify API url for authorization."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-scopes "playlist-read-private user-read-private user-read-email user-read-currently-playing user-read-playback-state"
  "Spotify application oauth scope."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-client-id ""
  "Spotify application client ID."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-client-secret ""
  "Spotify application client secret."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-oauth-refresh-token nil
  ""
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
    (error "The variables counsel-spotify-client-id or counsel-spotify-client-secret are undefined and both are required to authenticate to the Spotify API. See https://developer.spotify.com/my-applications")))

(defun counsel-spotify-basic-auth-credentials ()
  "Return the Basic auth string that should be sent to ask for an auth token."
  (concat "Basic " (base64-encode-string (concat counsel-spotify-client-id ":" counsel-spotify-client-secret) t)))

;; integrate elisp oauth-2 wip
(defun init-oauth-2 ()
  (elisp-oauth-2-init
   counsel-spotify-spotify-api-token-url
   counsel-spotify-spotify-api-auth-url
   counsel-spotify-client-id
   counsel-spotify-client-secret
   counsel-spotify-scopes
   counsel-spotify-oauth-refresh-token))

(defun counsel-spotify-build-result (type &rest data &allow-other-keys)
  (counsel-spotify-update-ivy-candidates type (plist-get data :data)))

(cl-defun elisp-oauth-2-search (&rest rest)
  (let ((type (or (plist-get rest :type) 'track))
        (query-url (apply #'counsel-spotify-make-query rest)))
    (elisp-oauth-2-request
     :method "GET"
     :url (url-encode-url query-url)
     :callback (-partial 'counsel-spotify-build-result type))))

(defmacro counsel-spotify-authorization-search (search-keyword &rest search-args)
  "This uses authorizatoin flow to obtain refreshable user authorization code.
With this we can query for APIs that returns user data."
  `(lambda (search-term)
     (init-oauth-2) ;; this only runs once in a session so we can place it here
     (when search-term
       (elisp-oauth-2-search ,search-keyword search-term ,@search-args))
     0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spotify API integration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass counsel-spotify-playable ()
  ((uri :initarg :uri :initform "" :reader uri)))

(defclass counsel-spotify-album (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)
   (artist-name :initarg :artist-name :initform "" :reader artist-name)))

(defclass counsel-spotify-episode (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)))

(defclass counsel-spotify-track (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)
   (artist :initarg :artist :initform "" :reader artist)
   (album :initarg :album :initform "" :reader album)
   (duration-in-ms :initarg :duration :initform 0 :reader duration-in-ms)))

(defclass counsel-spotify-artist (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)))

(defclass counsel-spotify-user-playlist (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)))

(defclass counsel-spotify-new-releases (counsel-spotify-playable)
  ;; same as album - refactor
  ((name :initarg :name :initform "" :reader name)
   (artist-name :initarg :artist-name :initform "" :reader artist-name)))

(defclass counsel-spotify-playlist (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)))

(defclass counsel-spotify-show (counsel-spotify-playable)
  ((name :initarg :name :initform "" :reader name)))

(cl-defun counsel-spotify-make-query (&key album artist episode playlist show track new-releases user-playlist (type 'track))
  "Create a new query url."
  (cond
   ((or artist
        album
        episode
        playlist
        show
        track) (concat counsel-spotify-spotify-api-url
                       "/search?q="
                       (when artist (format "artist:%s" artist))
                       (when album (format "album:%s" album))
                       (when track (format "track:%s" track))
                       (when episode (format "name:%s" episode))
                       (when playlist (format "%s" playlist))
                       (when show (format "%s" show))
                       (when type (format "&type=%s" (symbol-name type)))))
   (user-playlist (concat counsel-spotify-spotify-api-url "/me/playlists"))
   (new-releases (concat counsel-spotify-spotify-api-url "/browse/new-releases"))
   (t (error "Must supply at least an artist or an album or a track to search for"))))

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

(cl-defmethod counsel-spotify-builder ((type (eql episode)) spotify-alist-response)
  (mapcar
   (lambda (a) (make-instance 'counsel-spotify-episode
                              :name (alist-get 'name a)
                              :uri (alist-get 'uri a)))
   (alist-get 'items (alist-get 'episodes spotify-alist-response))))

(cl-defmethod counsel-spotify-builder ((type (eql playlist)) spotify-alist-response)
  (mapcar
   (lambda (a) (make-instance 'counsel-spotify-playlist
                              :name (alist-get 'name a)
                              :uri (alist-get 'uri a)))
   (alist-get 'items (alist-get 'playlists spotify-alist-response))))

(cl-defmethod counsel-spotify-builder ((type (eql user-playlist)) spotify-alist-response)
  (mapcar
   (lambda (a) (make-instance 'counsel-spotify-user-playlist
                              :name (alist-get 'name a)
                              :uri (alist-get 'uri a)))
   (alist-get 'items spotify-alist-response)))

;; dupe of album - refactor
(cl-defmethod counsel-spotify-builder ((type (eql new-releases)) spotify-alist-response)
  (mapcar
   (lambda (a) (make-instance 'counsel-spotify-new-releases
                              :name (alist-get 'name a)
                              :artist-name (alist-get 'name (elt (alist-get 'artists a) 0))
                              :uri (alist-get 'uri a)))
   (alist-get 'items (alist-get 'albums spotify-alist-response))))

(cl-defmethod counsel-spotify-builder ((type (eql show)) spotify-alist-response)
  (mapcar
   (lambda (a) (make-instance 'counsel-spotify-show
                              :name (alist-get 'name a)
                              :uri (alist-get 'uri a)))
   (alist-get 'items (alist-get 'shows spotify-alist-response))))


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

(cl-defmethod counsel-spotify-format ((episode counsel-spotify-episode))
  (name episode))

(cl-defmethod counsel-spotify-format ((playlist counsel-spotify-playlist))
  (name playlist))

(cl-defmethod counsel-spotify-format ((user-playlist counsel-spotify-user-playlist))
  (name user-playlist))

;; dupe of album - refactor
(cl-defmethod counsel-spotify-format ((new-releases counsel-spotify-new-releases))
  (concat (artist-name new-releases) " - " (name new-releases)))

(cl-defmethod counsel-spotify-format ((show counsel-spotify-show))
  (name show))

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

;;;###autoload
(defun build-authorization-search (prompt search-keyword type)
  ;; note that lexical binding needs to be non-nil
  ;; for this higher order function to work
  ;; I had to manually set this (setq lexical-binding t)
  ;; during development
  (setq lexical-binding t)
  (lambda ()
    (counsel-spotify-verify-credentials)
    (ivy-read prompt
              (counsel-spotify-authorization-search search-keyword :type type)
              :dynamic-collection t
              :action #'counsel-spotify-play-property)))

;;;###autoload
(defun counsel-spotify-search-track ()
  "Bring Ivy frontend to choose and play a track."
  (interactive)
  (counsel-spotify-verify-credentials)
  (ivy-read "Search track: " (counsel-spotify-authorization-search :track)
            :dynamic-collection t
            :action '(1
                      ("p" counsel-spotify-play-property "Play track")
                      ("a" (lambda (elem) (counsel-spotify-do-play counsel-spotify-current-backend (album (counsel-spotify-unwrap-property elem)))) "Play album")
                      ("A" (lambda (elem) (counsel-spotify-do-play counsel-spotify-current-backend (artist (counsel-spotify-unwrap-property elem)))) "Play artist"))))

;;;###autoload
(defun counsel-spotify-search-artist ()
  "Bring Ivy frontend to choose and play an artist."
  (interactive)
  (funcall (build-authorization-search "Seach artist: " :artist 'artist)))

;;;###autoload
(defun counsel-spotify-search-album ()
  "Bring Ivy frontend to choose and play an album."
  (interactive)
  (funcall (build-authorization-search "Search album: " :album 'album)))

;;;###autoload
(defun counsel-spotify-search-episode ()
  "Bring Ivy frontend to choose and play an episode."
  (interactive)
  (funcall (build-authorization-search "Search episode: " :episode 'episode)))

;;;###autoload
(defun counsel-spotify-search-playlist ()
  "Bring Ivy frontend to choose and play a playlist."
  (interactive)
  (funcall (build-authorization-search "Seach playlist: " :playlist 'playlist)))

;;;###autoload
(defun counsel-spotify-search-show ()
  "Bring Ivy frontend to choose and play an show"
  (interactive)
  (funcall (build-authorization-search "Search show: " :show 'show)))

;;;###autoload
(defun counsel-spotify-search-user-playlist ()
  "Bring Ivy frontend to choose and play a playlist for the current user.
Current user is the user that you used to log in to spotify api console to get the client id."
  (interactive)
  (funcall (build-authorization-search "Seach user playlist: " :user-playlist 'user-playlist)))

;;;###autoload
(defun counsel-spotify-new-releases ()
  "Bring Ivy frontend to choose and play a playlist for the current user.
Current user is the user that you used to log in to spotify api console to get the client id."
  (interactive)
  (funcall (build-authorization-search "New releases: " :new-releases 'new-releases)))

;;;###autoload
(defun counsel-spotify-search-tracks-by-artist ()
  "Bring Ivy frontend to search for all tracks for a given artist."
  (interactive)
  (funcall (build-authorization-search "Search tracks by artist: " :artist 'track)))

;;;###autoload
(defun counsel-spotify-search-tracks-by-album ()
  "Bring Ivy frontend to search for all track on a given album."
  (interactive)
  (funcall (build-authorization-search "Search tracks by album: " :album 'track)))

;;; New functionalities - WIP

;;; currently playing
(defun format-currently-playing (data)
  (let* ((item (->> (plist-get data :data)
                    (alist-get 'item)))
         (artist-name (-as-> item <> (alist-get 'artists <>) (elt <> 0) (alist-get 'name <>)))
         (track-name (->> item (alist-get 'name))))
    (concat track-name " - " artist-name)))

(defun currently-playing-cb (&rest data)
  (if data
    (message (format-currently-playing data))
    (message "No currently playng song.")))

;;;###autoload
(defun counsel-spotify-currently-playing ()
  (interactive)
  (init-oauth-2)
  (elisp-oauth-2-request
   :method "GET"
   :url "https://api.spotify.com/v1/me/player/currently-playing"
   :callback 'currently-playing-cb))


(provide 'counsel-spotify)
;;; counsel-spotify.el ends here
