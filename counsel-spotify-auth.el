;;; counsel-spotify-auth.el --- Provides a function to make authenticated requests to Spotify API -*- lexical-binding: t; -*-

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
;; Provides a function to make authenticated requests to Spotify API
;;; Code:

(require 'json)
(require 'oauth2)
(require 'simple-httpd)

(defcustom counsel-spotify-spotify-api-url "https://api.spotify.com/v1"
  "Variable to define spotify API url."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-authorization-url "https://accounts.spotify.com/authorize"
  "Spotify Authorization Code endpoint."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-spotify-api-token-url "https://accounts.spotify.com/api/token"
  "Spotify access token endpoint."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-client-id ""
  "Spotify application client ID."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-client-secret ""
  "Spotify application client secret."
  :type 'string :group 'counsel-spotify)

(defcustom counsel-spotify-local-http-server-port "5566"
  "Local HTTP server direction to handle the authorization callback."
  :type 'string :group 'counsel-spotify)

(defvar counsel-spotify--spotify-scopes "user-read-currently-playing")

(defun counsel-spotify-verify-credentials ()
  "Tell the user that the credentials are not set."
  (when (or (string= counsel-spotify-client-id "") (string= counsel-spotify-client-secret ""))
    (error "The variables counsel-spotify-client-id or counsel-spotify-client-secret are undefined and both are required to authenticate to the Spotify API.  See https://developer.spotify.com/my-applications")))

(defun counsel-spotify-make-api-url (endpoint)
  (format "%s%s" counsel-spotify-spotify-api-url endpoint))

(defclass authorization-flow () ())

(cl-defgeneric counsel-spotify-parse-spotify-object (a-spotify-object type)
  "Parse A-SPOTIFY-OBJECT knowing it has the type TYPE.")

(cl-defgeneric counsel-spotify-request (authorization-flow url callback)
  "Call CALLBACK with the result of parsing as counsel-spotify objects the response of the authorized request to URL via the corresponding AUTHORIZATION-FLOW.")


;;;;;;;;;;;;;;;;;;;:::::::::::
;; Authorization Code  Flow ;;
;;;;;;;;;;;;;;;;;;;:::::::::::

(defclass counsel-spotify-authorization-code-flow (authorization-flow) ())

(defun counsel-spotify-oauth2-token ()
  "Ask oauth2 to get an access token, performing all the refreshing steps needed."
  (oauth2-auth-and-store counsel-spotify-spotify-authorization-url
                         counsel-spotify-spotify-api-token-url
                         counsel-spotify--spotify-scopes
                         counsel-spotify-client-id
                         counsel-spotify-client-secret
                         (concat "http://localhost:" counsel-spotify-local-http-server-port)))

(defmethod counsel-spotify-request ((_authorization-flow counsel-spotify-authorization-code-flow) endpoint callback)
  "Call CALLBACK with the result of parsing as counsel-spotify objects the response of the authorized request to ENDPOINT."
  (setq oauth--url-advice t)
  (let ((token (counsel-spotify-oauth2-token))
        (spotify-url (counsel-spotify-make-api-url endpoint)))
    (setq oauth--token-data (cons token spotify-url))
    (oauth2-url-retrieve token
                         (counsel-spotify-make-api-url spotify-url)
                         (lambda (_status)
                           (goto-char url-http-end-of-headers)
                           (funcall callback (counsel-spotify-parse-response (json-read)))))))


;;;;;;;;;;;;;;;;;;;::::::::::
;; Client Credentials Flow ;;
;;;;;;;;;;;;;;;;;;;::::::::::

(defclass counsel-spotify-client-credentials-flow (authorization-flow) ())

(defun counsel-spotify-basic-auth-credentials ()
  "Return the Basic auth string that should be sent to ask for an auth token."
  (concat "Basic " (base64-encode-string (concat counsel-spotify-client-id ":" counsel-spotify-client-secret) t)))

(cl-defmacro counsel-spotify-with-auth-token ((auth-variable) &body body)
  "Execute with AUTH-VARIABLE bound to the Spotify's auth token for the current user the BODY."
  `(let ((url-request-method "POST")
         (url-request-data "&grant_type=client_credentials")
         (url-request-extra-headers (list (cons "Content-Type" "application/x-www-form-urlencoded")
                                          (cons "Authorization" (counsel-spotify-basic-auth-credentials)))))
     (url-retrieve counsel-spotify-spotify-api-token-url
                   (lambda (_status)
                     (goto-char url-http-end-of-headers)
                     (let ((,auth-variable (alist-get 'access_token (json-read))))
                       ,@body)))))

(cl-defmacro counsel-spotify-with-query-results ((auth-token query-url results-variable) &body body)
  "Execute the BODY with the results of an api call to QUERY-URL with an AUTH-TOKEN bound to RESULTS-VARIABLE."
  `(let ((url-request-extra-headers (list (cons "Authorization" (concat "Bearer " ,auth-token)))))
     (url-retrieve ,query-url
                   (lambda (_status)
                     (goto-char url-http-end-of-headers)
                     (let ((,results-variable (json-read)))
                       ,@body)))))

(defmethod counsel-spotify-request ((_authorization-flow counsel-spotify-client-credentials-flow) endpoint callback)
  "Make a HTTP requqest to ENDPOINT with the proper _AUTHORIZATION-FLOW and execute CALLBACK with its result parsed as counsel-spotify objects."
  (counsel-spotify-with-auth-token (credentials)
    (counsel-spotify-with-query-results (credentials (counsel-spotify-make-api-url endpoint) results)
      (funcall callback (counsel-spotify-parse-response results)))))

;;;;;;;;;;;;;;;;;;;::::::::
;; Oauth callback server ;;
;;;;;;;;;;;;;;;;;;;::::::::

(defun counsel-spotify-start-http-auth-server ()
  "Start server to function like oauth redirect URI."
  (setq httpd-root "auth-server/"
        httpd-port counsel-spotify-local-http-server-port)
  (httpd-start)
  (set-process-query-on-exit-flag (get-process "httpd") nil))

(defun counsel-spotify-stop-http-auth-server ()
  "Stop auth server"
  (httpd-stop))

(counsel-spotify-start-http-auth-server)

(provide 'counsel-spotify-auth)
;;; counsel-spotify-auth.el ends here
