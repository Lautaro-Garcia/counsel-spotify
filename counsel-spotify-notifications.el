;;; counsel-spotify-notifications.el --- Notify the user of playback eventse

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
;; Notify the user of playback eventse
;; Right now, the only backend supported is Linux via sbus
;;; Code:

(require 'dbus)
(require 'counsel-spotify-backends)

(defcustom counsel-spotify-use-notifications t
  "Notify playback changes via DBUS (only for backends that support DBUS)."
  :type 'boolean :group 'counsel-spotify)

(defun counsel-spotify-get-dbus-spotify-metadata-for (event property)
  "Scrap Spotify's dbus EVENT for information about PROPERTY."
  (caadr (assoc (format "xesam:%s" property) (caadr (assoc "Metadata" event)))))

(defun counsel-spotify-handle-player-change (interface-name changed-properties invalidated-properties)
  "DBUS event handler for Spotify's Player ProertiesChanged event.  The signal has an INTERFACE-NAME, an CHANGED-PROPERTIES and an INVALIDATED-PROPERTIES."
  (let* ((playback-status (caadr (assoc "PlaybackStatus" changed-properties)))
         (track-name (counsel-spotify-get-dbus-spotify-metadata-for changed-properties "title"))
         (artist     (car (counsel-spotify-get-dbus-spotify-metadata-for changed-properties "artist")))
         (album      (counsel-spotify-get-dbus-spotify-metadata-for changed-properties "album")))
    (message "(Spotify %s) %s - %s [%s]" playback-status artist track-name album)))

(cl-defgeneric counsel-spotify-notify-playback-changes (backend)
  "Notify a playback change via the BACKEND.")

(cl-defmethod counsel-spotify-notify-playback-changes ((backend counsel-spotify-backend))
  "Do nothing for a generic BACKEND."
  :backend-doesnt-support-notifications)

(when (featurep 'dbusbind)
  (cl-defmethod counsel-spotify-notify-playback-changes ((backend counsel-spotify-linux-backend))
    "Notify via Dbus (available for the Linux BACKEND) a playback change."
    (dbus-register-signal :session "org.mpris.MediaPlayer2.spotify" "/org/mpris/MediaPlayer2" "org.freedesktop.DBus.Properties" "PropertiesChanged" #'counsel-spotify-handle-player-change)
    :notifications-set-up))

(when counsel-spotify-use-notifications
  (counsel-spotify-notify-playback-changes counsel-spotify-current-backend))

(provide 'counsel-spotify-notifications)
;;; counsel-spotify-notifications.el ends here
