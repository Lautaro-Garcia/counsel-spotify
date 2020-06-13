;;; -*- lexical-binding: t; -*-
(require 'json)
(require 'url)

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

(defun counsel-spotify-verify-credentials ()
  "Tell the user that the credentials are not set."
  (when (or (string= counsel-spotify-client-id "") (string= counsel-spotify-client-secret ""))
    (error "The variables counsel-spotify-client-id or counsel-spotify-client-secret are undefined and both are required to authenticate to the Spotify API.  See https://developer.spotify.com/my-applications")))

(defun counsel-spotify-basic-auth-credentials ()
  "Return the Basic auth string that should be sent to ask for an auth token."
  (concat "Basic " (base64-encode-string (concat counsel-spotify-client-id ":" counsel-spotify-client-secret) t)))

(defclass counsel-spotify-playable ()
  ((name :initarg :name :initform "" :reader name)
   (uri :initarg :uri :initform "" :reader uri)))

(defclass counsel-spotify-album (counsel-spotify-playable)
  ((artist-name :initarg :artist-name :initform "" :reader artist-name)))

(defclass counsel-spotify-track (counsel-spotify-playable)
  ((artist :initarg :artist :initform "" :reader artist)
   (album :initarg :album :initform "" :reader album)
   (duration-in-ms :initarg :duration :initform 0 :reader duration-in-ms)))

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

(cl-defun counsel-spotify-search (a-callback &rest rest)
  "Search something in Spotify, based on the query described in REST."
  (let ((type (or (plist-get rest :type) 'track))
        (query-url (apply #'counsel-spotify-make-query rest)))
    (counsel-spotify-with-auth-token (auth-token)
      (counsel-spotify-with-query-results (auth-token query-url results)
        (funcall a-callback (counsel-spotify-parse-response results))))))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-object type)
  (let ((name (alist-get 'name a-spotify-object))
        (uri (alist-get 'uri a-spotify-object)))
    (make-instance 'counsel-spotify-playable :name name :uri uri)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-album-object (type (eql albums)))
  (let ((name (alist-get 'name a-spotify-album-object))
        (artist-name (alist-get 'name (elt (alist-get 'artists a-spotify-album-object) 0)))
        (uri (alist-get 'uri a-spotify-album-object)))
    (make-instance 'counsel-spotify-album :name name :uri uri :artist-name artist-name)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-track-object (type (eql tracks)))
  (let ((name (alist-get 'name a-spotify-track-object))
        (uri (alist-get 'uri a-spotify-track-object))
        (duration-in-ms (alist-get 'duration_ms a-spotify-track-object))
        (main-artist (counsel-spotify-parse-spotify-object (elt (alist-get 'artists a-spotify-track-object) 0) 'artists))
        (album (counsel-spotify-parse-spotify-object (alist-get 'album a-spotify-track-object) 'albums)))
    (make-instance 'counsel-spotify-track
                   :name name
                   :uri uri
                   :artist main-artist
                   :duration duration-in-ms
                   :album album)))

(defun counsel-spotify-parse-items (a-type a-spotify-alist-response)
  (let ((items (alist-get 'items (alist-get a-type a-spotify-alist-response))))
    (mapcar (lambda (item) (counsel-spotify-parse-spotify-object item a-type))
            items)))

(defun counsel-spotify-parse-response (a-spotify-alist-response) ;
  (mapcan (lambda (category) (counsel-spotify-parse-items (car category) a-spotify-alist-response))
          a-spotify-alist-response))

(provide 'counsel-spotify-search-api)
