;; Copyright (C) 2026 darlopvil (soprano-fork)
;;
;; This file is part of Soprano
;;
;; Soprano is free software: you can redistribute it and/or modify it under the
;; terms of the GNU Affero General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of  MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License
;; for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Small in-memory cache of rendered pages, so that loading the same page again
;; does not cost another request to Tenor.  It holds Soprano's own HTML (a few
;; KB per page, not Tenor's 100+ KB), with a hard cap on the number of entries
;; so it stays small on low-memory hosts.

(define-module (soprano cache)
  #:use-module (ice-9 threads)
  #:export (cache-ref cache-put!))

(define max-entries 500)

(define lock (make-mutex))
(define table (make-hash-table))   ; key -> (expiry . value)

;; The value stored under KEY, or #f if there is none or it has expired.
(define (cache-ref key)
  (with-mutex lock
    (let ((entry (hash-ref table key)))
      (cond ((not entry) #f)
            ((> (car entry) (current-time)) (cdr entry))
            (else (hash-remove! table key) #f)))))

;; Stores VALUE under KEY for TTL seconds.
(define (cache-put! key value ttl)
  (with-mutex lock
    (unless (hash-ref table key)
      (when (>= (hash-count (const #t) table) max-entries)
        (make-room!)))
    (hash-set! table key (cons (+ (current-time) ttl) value))))

;; Called with the lock held.  Drops expired entries, then, if the cache is
;; still full, the quarter of entries closest to expiring.
(define (make-room!)
  (let* ((now (current-time))
         (entries (hash-map->list cons table)))
    (for-each (lambda (entry)
                (when (<= (cadr entry) now)
                  (hash-remove! table (car entry))))
              entries)
    (when (>= (hash-count (const #t) table) max-entries)
      (let ((by-expiry (sort (hash-map->list cons table)
                             (lambda (a b) (< (cadr a) (cadr b))))))
        (for-each (lambda (entry) (hash-remove! table (car entry)))
                  (list-head by-expiry (quotient max-entries 4)))))))
