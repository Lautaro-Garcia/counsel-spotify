;;; counsel-spotify-backends.el --- Backends to comunicate with the native Spotify app

;; Copyright (C)

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
;; The Linux backend communicates via DBus
;; The OSX one uses Applescript to control Spotify
;;; Code:

(require 'dbus)
(require 'counsel-spotify-search)

(defcustom counsel-spotify-service-name "spotify"
  "Name of the DBUS service used by the client we talk to.

The official Spotify client uses `spotify', but one can also use
alternative clients such as mopidy or spotifyd."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-use-system-bus-p nil
  "Whether to access the spotify client using the system DBUS.

Some clients, such as mopidy, can run as system services."
  :type 'boolean :group 'counsel-spotify)

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
    ('darwin    (make-instance 'counsel-spotify-darwin-backend)))
  "Current counsel-spotify backend based on the OS.")

(cl-defun counsel-spotify-call-spotify-via-dbus (method &rest args)
  "Tell Spotify to execute METHOD with ARGS through Dbus."
  (apply #'dbus-call-method `(,(if counsel-spotify-use-system-bus-p :system :session)
                              ,(format "org.mpris.MediaPlayer2.%s" counsel-spotify-service-name)
                              "/org/mpris/MediaPlayer2"
                              "org.mpris.MediaPlayer2.Player"
                              ,method
                              ,@args)))

(cl-defgeneric counsel-spotify-tell-backend-to (backend action)
  "Tell the given BACKEND to execute the given ACTION.")

(cl-defmethod counsel-spotify-tell-backend-to ((backend counsel-spotify-darwin-backend) action)
  "Tell Darwin BACKEND to execute the given ACTION."
  (shell-command (concat "osascript -e 'tell application \"Spotify\" to '" (shell-quote-argument (funcall action (commands backend))))))

(cl-defmethod counsel-spotify-tell-backend-to ((backend counsel-spotify-linux-backend) action)
  "Tell Linux BACKEND to execute the given ACTION."
  (counsel-spotify-call-spotify-via-dbus (funcall action (commands backend))))

(cl-defgeneric counsel-spotify-do-play (backend playable)
  "Tell the BACKEND to play the PLAYABLE object.")

(cl-defmethod counsel-spotify-do-play ((backend counsel-spotify-darwin-backend) (playable counsel-spotify-playable))
  "Tell Darwin BACKEND to play the PLAYABLE object."
  (shell-command (concat "osascript -e 'tell application \"Spotify\" to play track \"" (uri playable) "\"'")))

(cl-defmethod counsel-spotify-do-play ((backend counsel-spotify-linux-backend) (playable counsel-spotify-playable))
  "Tell Linux BACKEND to play the PLAYABLE object."
  (counsel-spotify-call-spotify-via-dbus "OpenUri" (uri playable)))

(defun counsel-spotify-unwrap-spotify-object (spotify-object-as-string)
  "Unwrap the spotify-object property of the SPOTIFY-OBJECT-AS-STRING."
  (get-text-property 0 'spotify-object spotify-object-as-string))

(defun counsel-spotify-play-string (spotify-object-as-string)
  "Call play on an the spotify object held as a property of the SPOTIFY-OBJECT-AS-STRING."
  (counsel-spotify-do-play counsel-spotify-current-backend (counsel-spotify-unwrap-spotify-object spotify-object-as-string)))

(provide 'counsel-spotify-backends)
;;; counsel-spotify-backends.el ends here
