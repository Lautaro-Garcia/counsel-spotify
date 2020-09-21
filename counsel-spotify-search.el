;;; counsel-spotify-search.el --- Search things through the Spotify Search API -*- lexical-binding: t; -*-

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
;; In this file there are functions to search things using Spotify Search API
;; and decoders for all the different types of things that Spotify can respond back
;;; Code:

(require 'json)
(require 'url)
(require 'oauth2)

(defclass counsel-spotify-playable ()
  ((name :initarg :name :initform "" :reader name)
   (uri :initarg :uri :initform "" :reader uri)))

(defclass counsel-spotify-album (counsel-spotify-playable)
  ((artist-name :initarg :artist-name :initform "" :reader artist-name)))

(defclass counsel-spotify-track (counsel-spotify-playable)
  ((artist :initarg :artist :initform "" :reader artist)
   (album :initarg :album :initform "" :reader album)
   (duration-in-ms :initarg :duration :initform 0 :reader duration-in-ms)))

(cl-defun counsel-spotify-make-query (term &key type filter)
  "Make a Spotify query to search for TERM of type TYPE with a FILTER."
  (when (null type) (error "Must supply a type of object to search for"))
  (format "/search?q=%s&type=%s"
          (if filter (format "%s:%s" filter term) term)
          (mapconcat #'symbol-name type ",")))

(cl-defun counsel-spotify-search (a-callback &rest rest)
  "Call A-CALLBACK with the parsed result of the query described by REST."
  (let ((query-url (apply #'counsel-spotify-make-query rest)))
    (counsel-spotify-request (make-instance 'counsel-spotify-client-credentials-flow) query-url a-callback)))

(cl-defmethod counsel-spotify-parse-spotify-object (spotify-object _type)
  "Parse a generic SPOTIFY-OBJECT of type _TYPE."
  (let ((name (alist-get 'name spotify-object))
        (uri (alist-get 'uri spotify-object)))
    (make-instance 'counsel-spotify-playable :name name :uri uri)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-album-object (_type (eql albums)))
  "Parse A-SPOTIFY-ALBUM-OBJECT of _TYPE album."
  (let ((name (alist-get 'name a-spotify-album-object))
        (artist-name (alist-get 'name (elt (alist-get 'artists a-spotify-album-object) 0)))
        (uri (alist-get 'uri a-spotify-album-object)))
    (make-instance 'counsel-spotify-album :name name :uri uri :artist-name artist-name)))

(cl-defmethod counsel-spotify-parse-spotify-object (a-spotify-track-object (_type (eql tracks)))
  "Parse A-SPOTIFY-TRACK-OBJECT of _TYPE track."
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

(defun counsel-spotify-parse-items (a-spotify-alist-response a-type)
  "Parse every item in A-SPOTIFY-ALIST-RESPONSE as being of the type A-TYPE."
  (let ((items (alist-get 'items (alist-get a-type a-spotify-alist-response))))
    (mapcar (lambda (item) (counsel-spotify-parse-spotify-object item a-type))
            items)))

(defun counsel-spotify-parse-response (a-spotify-alist-response)
  "Parse A-SPOTIFY-ALIST-RESPONSE iterating through every category."
  (cl-mapcan (lambda (category) (counsel-spotify-parse-items a-spotify-alist-response  (car category)))
             a-spotify-alist-response))

(provide 'counsel-spotify-search)
;;; counsel-spotify-search.el ends here
