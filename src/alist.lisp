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

(defun alist-map-key (alist f)
  (loop for a in alist
        for k = (car a)
        for v = (cdr a)
        collect (cons (funcall f k) v)))

(defun alist-join (alist x &key (test 'eql))
  (if x
      (cons x (loop with ak = (car x)
                    for a in alist
                    for k = (car a)
                    unless (funcall test ak k)
                      collect a))
      alist))

(defun alist-merge (alist xs &key (test 'eql))
  (loop with a = alist
        for x in xs
        do (setq a (alist-join a x :test test))
        finally (return a)))
