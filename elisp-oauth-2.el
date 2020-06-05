;;; elisp-oauth-2.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Ryuei Sasaki
;;
;; Author: Ryuei Sasaki <https://github.com/louixs> <https://github.com/ryueisasaki>
;; Maintainer: Ryuei Sasaki
;; Created: May 17, 2020
;; Modified: May 17, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/louixs/elisp-oauth-2
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:
(require 'request)
(require 'simple-httpd)

(defvar elisp-oauth-2-api-token-url ""
  "Variable to define spotify API url for getting the access token.")

(defvar elisp-oauth-2-api-auth-url ""
  "Variable to define spotify API url for authorization.")

(defvar elisp-oauth-2-redirect-uri-port "8080"
  "Variable to define spotify API url for authorization.")

(defvar elisp-oauth-2-redirect-uri (concat "http://localhost:"
                                           elisp-oauth-2-redirect-uri-port)
  "Variable to define spotify API url for authorization.")

(defvar elisp-oauth-2-client-id ""
  "Spotify application client ID.")

(defvar elisp-oauth-2-client-secret ""
  "Spotify application client secret.")

(defvar elisp-oauth-2-oauth-refresh-token nil
  "")

(defvar elisp-oauth-2-oauth-access-token nil
  "")

(defvar elisp-oauth-2-oauth-code nil
  "")

(defvar elisp-oauth-2-scopes ""
  "Spotify application oauth scope.")

(defvar elisp-oauth-2-oauth-access-token-expires-at 0
  "The time that the current oauth access token will expire in unix timestamp.")

;; http server
(defun start-redirect-server ()
  (setq httpd-root "www/"
        httpd-port "8080")
  (httpd-start))

(defun stop-redirect-server ()
  (print "Stopping web server...")
  (httpd-stop)
  (kill-buffer "*httpd*"))

(defun elisp-oauth-2-set-auth-code ()
  "TODO: add doc."
  (setq elisp-oauth-2-oauth-code (read-string "Enter the code your browser displayed: ")))

(defvar elisp-oauth-2-oauth-url
  (concat "%s"
          "?client_id=%s"
          "&response_type=code"
          "&redirect_uri=%s"
          "&scope=%s")
  "The OAuth URL/endpoint.")

(defun elisp-oauth-2-build-oauth-url ()
  "TODO: add doc."
  (format elisp-oauth-2-oauth-url
          elisp-oauth-2-api-auth-url
          elisp-oauth-2-client-id
          elisp-oauth-2-redirect-uri
          elisp-oauth-2-scopes))

(defun elisp-oauth-2-fetch-oauth-token ()
  "TODO: add doc."
  (start-redirect-server)
  ;; endpoint /authorize
  ;; query params
  ;; client_id, resonse_type, redirect_uri, scope
  (browse-url (elisp-oauth-2-build-oauth-url))
  (elisp-oauth-2-set-auth-code)
  (stop-redirect-server))

;; refresh token
(defun elisp-oauth-2-get-current-time ()
  (string-to-number (shell-command-to-string "date +%s")))

(defun elisp-oauth-2-oauth-set-access-token-expires-at (expires-in)
  (let ((expires-at (+ (elisp-oauth-2-get-current-time) expires-in)))
    (setq elisp-oauth-2-oauth-access-token-expires-at expires-at)))

(defun elisp-oauth-2-refresh-oauth-token? ()
  "Return non-nil if oauth access token needs to be refreshed."
  (< elisp-oauth-2-oauth-access-token-expires-at
     (elisp-oauth-2-get-current-time)))

(cl-defun elisp-oauth-2-oauth-fetch-callback (&rest data &allow-other-keys)
  "Callback to run when the oauth code fetch is complete."
  (let-alist (plist-get data :data)
    (unless (and .access_token .refresh_token .expires_in)
      (message "elisp-oauth-2: Failed to fetch OAuth access_token and refresh_token values!")
      (error "elisp-oauth-2: Failed to fetch OAuth access_token and refresh_token values!"))
    (setq elisp-oauth-2-oauth-access-token .access_token)
    (setq elisp-oauth-2-oauth-refresh-token .refresh_token)
    (elisp-oauth-2-oauth-set-access-token-expires-at .expires_in)
    ;; @todo Handle expires_in value (should be ~1 hour, so refresh before then)
    (message
     "Tokens set - consider adding elisp-oauth-2-oauth-refresh-token values to your init file to avoid signing in again in the future sessions.")))

(defun elisp-oauth-2-oauth-fetch-authorization-token ()
  "Make the initial code request for OAuth."
  (request elisp-oauth-2-api-token-url
    :complete #'elisp-oauth-2-oauth-fetch-callback
    :sync t
    :data (concat "client_id=" elisp-oauth-2-client-id
                  "&client_secret=" elisp-oauth-2-client-secret
                  "&code=" elisp-oauth-2-oauth-code
                  "&redirect_uri=" elisp-oauth-2-redirect-uri
                  "&grant_type=authorization_code")
    :type "POST"
    :parser 'json-read
    :headers '(("Content-Type" . "application/x-www-form-urlencoded"))))

;; TODO: refactor
(cl-defun elisp-oauth-2-oauth-refresh-callback (&rest data &allow-other-keys)
  "Callback to run when the oauth code fetch is complete."
  (let-alist (plist-get data :data)
    (unless (and .access_token .expires_in)
      (message "counsel-spotify: Failed to fetch OAuth access_token values!")
      (error "counsel-spotify: Failed to fetch OAuth access_token values!"))
    (setq elisp-oauth-2-oauth-access-token .access_token)
    (elisp-oauth-2-oauth-set-access-token-expires-at .expires_in)
    (message
     "Refresh token successful!")))

(defun elisp-oauth-2-oauth-refresh-authorization-token ()
  "Get new oauth access and refresh token using refresh token."
  (request elisp-oauth-2-api-token-url
    :complete #'elisp-oauth-2-oauth-refresh-callback
    :sync t
    :data (concat "client_id=" elisp-oauth-2-client-id
                  "&client_secret=" elisp-oauth-2-client-secret
                  "&refresh_token=" elisp-oauth-2-oauth-refresh-token
                  "&grant_type=refresh_token")
    :type "POST"
    :parser 'json-read
    :headers '(("Content-Type" . "application/x-www-form-urlencoded"))))

(defun elisp-oauth-2-fetch-and-set-tokens ()
  ""
  (elisp-oauth-2-fetch-oauth-token)
  (elisp-oauth-2-oauth-fetch-authorization-token))

;;;###autoload
(defun elisp-oauth-2-handle-oauth ()
  "Retrieve oauth access token and refresh token if they are nil.
Access token gets refreshed if necessary."
  (cond ((and elisp-oauth-2-oauth-refresh-token
              (elisp-oauth-2-refresh-oauth-token?))
         (elisp-oauth-2-oauth-refresh-authorization-token))
        ((or (not elisp-oauth-2-oauth-refresh-token)
             (not elisp-oauth-2-oauth-access-token))
         (elisp-oauth-2-fetch-and-set-tokens))
        (t nil)))

;; once implementation
;;(setq lexical-binding t)

(defun once (fn)
  (let ((ran? nil))
    (lambda (&rest args)
      (when (not ran?)
          (progn (setq ran? t)
                 (apply fn args))))))

(defun elisp-oauth-2-set-vars (token-url oauth-url client-id client-secret scopes refresh-token)
  ""
  (setq elisp-oauth-2-api-token-url token-url
        elisp-oauth-2-api-auth-url oauth-url
        elisp-oauth-2-client-id client-id
        elisp-oauth-2-client-secret client-secret
        elisp-oauth-2-scopes scopes
        elisp-oauth-2-oauth-refresh-token refresh-token)
  (message "set vars finished"))

(setq _elisp-oauth-2-set-vars (once 'elisp-oauth-2-set-vars))

;;;###autoload
(defun elisp-oauth-2-init (token-url oauth-url client-id client-secret scopes refresh-token)
  "TODO: add doc"
  (funcall _elisp-oauth-2-set-vars token-url oauth-url client-id client-secret scopes refresh-token))

;;;###autoload
(defun elisp-oauth-2-request (url callback)
  "Wrapper for request to do http calls.
URL - api endpoint
CALLBACK - callback function
TODO: make request type configurable."
  (elisp-oauth-2-handle-oauth)
  (request url
    :complete callback
    :type "GET"
    :parser 'json-read
    :headers `(("Authorization" . ,(format  "Bearer %s" elisp-oauth-2-oauth-access-token)))))

(provide 'elisp-oauth-2)
;;; elisp-oauth-2.el ends here
