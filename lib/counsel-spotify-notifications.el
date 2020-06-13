(require 'dbus)
(require 'counsel-spotify-backends)

(defcustom counsel-spotify-use-notifications t
  "Notify playback changes via DBUS (only for backends that support DBUS)."
  :type 'boolean :group 'counsel-spotify)

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

(provide 'counsel-spotify-notifications)
