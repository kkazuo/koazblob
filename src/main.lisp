;;;; Copyright (c) 2019 Koga Kazuo <obiwanko@me.com>
;;;;
;;;; Permission to use, copy, modify, and distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage koazblob
  (:use :cl :alexandria)
  (:export
   :az-storage-account
   :az-list-containers
   :az-get-blob))
(in-package :koazblob)

(defstruct storage-account
  protocol
  account
  secret
  endpoint)

(defun parse-connection-string (s)
  (declare (type string s))
  (loop for entry in (split-sequence:split-sequence #\; s)
        for piv = (search "=" entry)
        collect (cons (subseq entry 0 piv)
                      (subseq entry (1+ piv)))))

(defun az-storage-account (connection-string)
  "Instanciate new Storage Account from connection-string"
  (let ((info (parse-connection-string connection-string)))
    (make-storage-account
     :protocol (cdr (assoc "DefaultEndpointsProtocol" info :test #'string=))
     :account (cdr (assoc "AccountName" info :test #'string=))
     :secret (cl-base64:base64-string-to-usb8-array
              (cdr (assoc "AccountKey" info :test #'string=)))
     :endpoint (cdr (assoc "EndpointSuffix" info :test #'string=)))))

(defconstant +newline+
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element 10))

(defconstant +slash+
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element 47))

(defconstant +colon+
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element 58))

(defconstant +verb-get+
  (make-array 4 :element-type '(unsigned-byte 8) :initial-contents #(71 69 84 10)))

(defconstant +verb-post+
  (make-array 5 :element-type '(unsigned-byte 8) :initial-contents #(80 79 83 84 10)))

(defconstant +verb-put+
  (make-array 4 :element-type '(unsigned-byte 8) :initial-contents #(80 85 84 10)))

(defconstant +verb-delete+
  (make-array 7 :element-type '(unsigned-byte 8) :initial-contents #(68 69 76 69 84 69 10)))

(defun update-verb (mac verb)
  (case verb
    (:get (ironclad:update-mac mac +verb-get+))
    (:post (ironclad:update-mac mac +verb-post+))
    (:put (ironclad:update-mac mac +verb-put+))
    (:delete (ironclad:update-mac mac +verb-delete+))
    (otherwise
     (ironclad:update-mac mac (trivial-utf-8:string-to-utf-8-bytes
                               (string-upcase verb)))
     (ironclad:update-mac mac +newline+))))

(defun update-canonical-headers (mac headers)
  (loop for a in (sort (loop for a in headers
                             for h = (string-downcase (car a))
                             when (and (string< "x-ms-" h) (string< h "x-ms."))
                               collect (cons h (cdr a)))
                       #'string< :key #'car)
        for h = (car a)
        for v = (cdr a)
        do (ironclad:update-mac mac (trivial-utf-8:string-to-utf-8-bytes h))
           (ironclad:update-mac mac +colon+)
           (ironclad:update-mac mac (trivial-utf-8:string-to-utf-8-bytes v))
           (ironclad:update-mac mac +newline+)))

(defun update-canonical-resource (mac account container resource)
  (ironclad:update-mac mac +slash+)
  (ironclad:update-mac mac (trivial-utf-8:string-to-utf-8-bytes account))
  (ironclad:update-mac mac +slash+)
  (ironclad:update-mac mac (trivial-utf-8:string-to-utf-8-bytes container))
  (when resource
    (ironclad:update-mac mac (trivial-utf-8:string-to-utf-8-bytes resource))))

(defun update-canonical-params (mac query)
  (flet ((update-canonical-param (mac k v)
           (ironclad:update-mac mac +newline+)
           (ironclad:update-mac mac (trivial-utf-8:string-to-utf-8-bytes k))
           (ironclad:update-mac mac +colon+)
           (ironclad:update-mac mac (trivial-utf-8:string-to-utf-8-bytes v))))
    (loop for a in (sort query #'string< :key #'car)
          for k = (car a)
          for v = (cdr a)
          do (when k (update-canonical-param mac k v)))))

(defun produce-sig (&key secret
                      verb headers query
                      account container resource)
  (let ((mac (ironclad:make-mac :hmac secret :sha256)))
    (update-verb mac verb)
    ;; Content-Encoding
    (ironclad:update-mac mac +newline+)
    ;; Content-Language
    (ironclad:update-mac mac +newline+)
    ;; Content-Length (empty string when zero)
    (ironclad:update-mac mac +newline+)
    ;; Content-MD5
    (ironclad:update-mac mac +newline+)
    ;; Content-Type
    (ironclad:update-mac mac +newline+)
    ;; Date
    (ironclad:update-mac mac +newline+)
    ;; If-Modified-Since
    (ironclad:update-mac mac +newline+)
    ;; If-Match
    (ironclad:update-mac mac +newline+)
    ;; If-None-Match
    (ironclad:update-mac mac +newline+)
    ;; If-Unmodified-Since
    (ironclad:update-mac mac +newline+)
    ;; Range
    (ironclad:update-mac mac +newline+)

    (update-canonical-headers mac headers)
    (update-canonical-resource mac account container resource)
    (update-canonical-params mac query)

    (concatenate
     'string
     "SharedKey " account ":"
     (cl-base64:usb8-array-to-base64-string (ironclad:produce-mac mac)))))

(defun format-rfc1123 (universal-time)
  (substitute #\space #\- (trivial-rfc-1123:as-rfc-1123 universal-time)))

(defmacro az-blob-api ((account &key container resource verb query) &body body)
  (once-only (account container resource verb query)
    (with-unique-names (protocol endpoint ac-name secret headers sig)
      `(let ((,ac-name (storage-account-account ,account))
             (,protocol (storage-account-protocol ,account))
             (,endpoint (storage-account-endpoint ,account))
             (,secret (storage-account-secret ,account))
             (,headers `(("x-ms-date" . ,(format-rfc1123 (get-universal-time)))
                         ("x-ms-version" . "2019-02-02"))))
         (let* ((,sig (produce-sig
                       :account ,ac-name
                       :secret ,secret
                       :verb ,verb
                       :headers ,headers
                       :query ,query
                       :container ,container
                       :resource ,resource))
                (headers (acons "Authorization" ,sig ,headers))
                (uri (quri:make-uri
                      :scheme ,protocol
                      :host (concatenate 'string ,ac-name ".blob." ,endpoint)
                      :path (if ,resource
                                (concatenate 'string "/" ,container ,resource)
                                "/")
                      :query ,query)))
           ,@body)))))

(defun az-list-containers (account)
  "List Containers
https://docs.microsoft.com/en-us/rest/api/storageservices/list-containers2"
  (az-blob-api (account
                :verb :get :query `(("comp" . "list")))
    (let ((body (dex:get uri :headers headers :force-binary t)))
      (cxml:parse-octets body (cxml-xmls:make-xmls-builder)))))

(defun az-get-blob (account &key container path)
  "Get Blob
https://docs.microsoft.com/en-us/rest/api/storageservices/get-blob"
  (az-blob-api (account
                :verb :get
                :container container
                :resource path)
    (dex:get uri :headers headers)))
