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
     :protocol (cdr (assoc "DefaultEndpointsProtocol" info :test 'equal))
     :account (cdr (assoc "AccountName" info :test 'equal))
     :secret (cl-base64:base64-string-to-usb8-array
              (cdr (assoc "AccountKey" info :test 'equal)))
     :endpoint (cdr (assoc "EndpointSuffix" info :test 'equal)))))

(define-constant +az-blob-type-block+ "BlockBlob" :test 'string-equal)
(define-constant +az-blob-type-append+ "AppendBlob" :test 'string-equal)

(define-constant +empty+
    (make-array 0 :element-type '(unsigned-byte 8))
  :test 'equalp)

(define-constant +newline+
    (make-array 1 :element-type '(unsigned-byte 8) :initial-element 10)
  :test 'equalp)

(define-constant +slash+
    (make-array 1 :element-type '(unsigned-byte 8) :initial-element 47)
  :test 'equalp)

(define-constant +colon+
    (make-array 1 :element-type '(unsigned-byte 8) :initial-element 58)
  :test 'equalp)

(define-constant +verb-get+
    (make-array 4 :element-type '(unsigned-byte 8) :initial-contents #(71 69 84 10))
  :test 'equalp)

(define-constant +verb-post+
    (make-array 5 :element-type '(unsigned-byte 8) :initial-contents #(80 79 83 84 10))
  :test 'equalp)

(define-constant +verb-put+
    (make-array 4 :element-type '(unsigned-byte 8) :initial-contents #(80 85 84 10))
  :test 'equalp)

(define-constant +verb-delete+
    (make-array 7 :element-type '(unsigned-byte 8) :initial-contents #(68 69 76 69 84 69 10))
  :test 'equalp)

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

(defun update-std-header (mac headers key &key ignore-when)
  (when-let (v (cdr (assoc key headers :test 'equal)))
    (when (or (not ignore-when)
              (string/= ignore-when v))
      (ironclad:update-mac mac (trivial-utf-8:string-to-utf-8-bytes v))))
  (ironclad:update-mac mac +newline+))

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
    (update-std-header mac headers "content-encoding")
    (update-std-header mac headers "content-language")
    ;; Content-Length (empty string when zero)
    (update-std-header mac headers "content-length" :ignore-when "0")
    (update-std-header mac headers "content-md5")
    (update-std-header mac headers "content-type")
    (update-std-header mac headers "date")
    (update-std-header mac headers "if-modified-since")
    (update-std-header mac headers "if-match")
    (update-std-header mac headers "if-none-match")
    (update-std-header mac headers "if-unmodified-since")
    (update-std-header mac headers "range")
    (update-canonical-headers mac headers)
    (update-canonical-resource mac account container resource)
    (update-canonical-params mac query)
    (concatenate
     'string
     "SharedKey " account ":"
     (cl-base64:usb8-array-to-base64-string (ironclad:produce-mac mac)))))

(defun format-rfc1123 (universal-time)
  (substitute #\space #\- (trivial-rfc-1123:as-rfc-1123 universal-time)))

(defun merge-headers (base xs)
  (alist-merge (alist-map-key base 'string-downcase)
               xs))

(defmacro az-blob-api ((account &key container resource verb headers query)
                       &body body)
  (once-only (account container resource verb headers query)
    (with-unique-names (protocol endpoint ac-name secret hdrs sig)
      `(let ((,ac-name (storage-account-account ,account))
             (,protocol (storage-account-protocol ,account))
             (,endpoint (storage-account-endpoint ,account))
             (,secret (storage-account-secret ,account))
             (,hdrs (alist-merge
                     ,headers
                     `(("x-ms-date" . ,(format-rfc1123 (get-universal-time)))
                       ("x-ms-version" . "2019-02-02")))))
         (let* ((,sig (produce-sig
                       :account ,ac-name
                       :secret ,secret
                       :verb ,verb
                       :headers ,hdrs
                       :query ,query
                       :container ,container
                       :resource ,resource))
                (headers (acons "Authorization" ,sig ,hdrs))
                (uri (quri:make-uri
                      :scheme ,protocol
                      :host (concatenate 'string ,ac-name ".blob." ,endpoint)
                      :path (concatenate 'string "/" ,container ,resource)
                      :query ,query)))
           ,@body)))))

(defun az-list-containers (account &key headers)
  "List Containers
https://docs.microsoft.com/en-us/rest/api/storageservices/list-containers2"
  (az-blob-api (account
                :verb :get :query `(("comp" . "list"))
                :headers (merge-headers headers '()))
    (let ((body (dex:get uri :headers headers :force-binary t)))
      (cxml:parse-octets body (cxml-xmls:make-xmls-builder)))))

(defun az-create-container (account &key container headers)
  "Create Container
https://docs.microsoft.com/en-us/rest/api/storageservices/create-container"
  (az-blob-api (account
                :verb :put :query `(("restype" . "container"))
                :container container
                :headers (merge-headers headers '()))
    (dex:put uri :headers headers)))

(defun az-delete-container (account &key container headers)
  "Delete Container
https://docs.microsoft.com/en-us/rest/api/storageservices/delete-container"
  (az-blob-api (account
                :verb :delete :query `(("restype" . "container"))
                :container container
                :headers (merge-headers headers '()))
    (dex:delete uri :headers headers)))

(defun az-get-blob (account &key container path headers)
  "Get Blob
https://docs.microsoft.com/en-us/rest/api/storageservices/get-blob"
  (az-blob-api (account
                :verb :get
                :container container
                :resource path
                :headers (merge-headers headers '()))
    (dex:get uri :headers headers)))

(defun az-get-blob-props (account &key container path headers)
  "Get Blob Properties
https://docs.microsoft.com/en-us/rest/api/storageservices/get-blob-properties"
  (az-blob-api (account
                :verb :head
                :container container
                :resource path
                :headers (merge-headers headers '()))
    (dex:head uri :headers headers)))

(defun header-of-delete-snapshots (v)
  (acons "x-ms-delete-snapshots"
         (ccase v
           (:include "include")
           (:only "only"))
         ()))

(defun az-delete-blob (account &key container path headers
                                 (delete-snapshots :include))
  "Delete Blob
https://docs.microsoft.com/en-us/rest/api/storageservices/delete-blob"
  (az-blob-api (account
                :verb :delete
                :container container
                :resource path
                :headers (merge-headers
                          headers
                          (header-of-delete-snapshots delete-snapshots)))
    (dex:delete uri :headers headers)))

(defun az-put-blob (account
                    &key container path content headers
                      (blob-type +az-blob-type-block+))
  "Put Blob
https://docs.microsoft.com/en-us/rest/api/storageservices/put-blob"
  (let* ((content
           (typecase content
             (string (trivial-utf-8:string-to-utf-8-bytes content))
             (otherwise content)))
         (content-length (length content))
         (headers (merge-headers
                   headers
                   `(("x-ms-blob-type" . ,blob-type)
                     ("content-length" . ,(write-to-string content-length)))))
         (content-type-p (assoc "content-type" headers :test 'equal)))
    (az-blob-api (account
                  :verb :put
                  :container container
                  :resource path
                  :headers headers)
      (dex:put uri :headers headers
                   :content (cond
                              ((and content-type-p (not content)) +empty+)
                              (t content))))))

(defun az-append-block (account
                        &key container path content headers)
  "Append Block
https://docs.microsoft.com/en-us/rest/api/storageservices/append-block"
  (let* ((content
           (typecase content
             (string (trivial-utf-8:string-to-utf-8-bytes content))
             (otherwise content)))
         (content-length (length content))
         (headers (merge-headers
                   headers
                   `(("content-length" . ,(write-to-string content-length))
                     ;; Don't output Content-Type header for request auth sign.
                     ("content-type" . nil)))))
    (az-blob-api (account
                  :verb :put
                  :query `(("comp" . "appendblock"))
                  :container container
                  :resource path
                  :headers headers)
      (dex:put uri :headers headers
                   :content content))))
