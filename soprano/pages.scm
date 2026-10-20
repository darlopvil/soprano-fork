;; Copyright (C) 2024-2025 Skylar Astaroth <cobra@vern.cc>
;; Copyright (C) 2026 darlopvil (modified for soprano-fork)
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

(define-module (soprano pages)
  #:use-module (soprano scraping)
  #:use-module (soprano templates)
  #:use-module (soprano upstream)
  #:use-module (soprano cache)
  #:use-module (htmlprag)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 string-fun)
  #:export (locale?
            error-page
            index-page
            gif-page
            search-page
            search-redirect-page
            proxy-page))

(define good-response
  (build-response #:code 200
                  #:headers `((content-type . (text/html)))))

(define (redirect response code)
  (values (build-response #:code code
                          #:headers `((location . ,(parse-header
                                                     'location
                                                     (uri-path
                                                       (response-location
                                                         response))))))
          "Redirect"))

(define (error-page code)
  (values (build-response
            #:code code
            #:headers `((content-type . (text/html))))
          (shtml->html (error-template code))))

;; Raw (undecoded) value of KEY in a query string, or #f.  Tolerates a
;; missing query, empty parts and parameters without '='.
(define (query-ref query key)
  (let ((prefix (string-append key "=")))
    (and query
         (let loop ((parts (string-split query #\&)))
           (cond ((null? parts) #f)
                 ((string-prefix? prefix (car parts))
                  (substring (car parts) (string-length prefix)))
                 (else (loop (cdr parts))))))))

;; Runs THUNK, which talks to Tenor and builds a page.  Any failure becomes
;; an error page (504 on timeout, 502 otherwise) instead of an uncaught
;; exception, which Guile answers with a blank 500.
(define (with-upstream-errors thunk)
  (catch #t
    thunk
    (lambda (key . args)
      (display (string-append
                 ";\tUpstream: " (symbol->string key)
                 (if (pair? args)
                   (string-append " (" (object->string (car args)) ")")
                   "")))
      (error-page (if (eq? key 'upstream-timeout) 504 502)))))

;; Tenor locales are path prefixes: two letters ("es") or two letters, a
;; dash and two letters ("es-es"; Tenor redirects "es-ES" there).
(define (locale? segment)
  (let ((n (string-length segment)))
    (and (or (= n 2) (and (= n 5) (char=? (string-ref segment 2) #\-)))
         (char-alphabetic? (string-ref segment 0))
         (char-alphabetic? (string-ref segment 1))
         (or (= n 2)
             (and (char-alphabetic? (string-ref segment 3))
                  (char-alphabetic? (string-ref segment 4)))))))

;; "/es/view/..." -> "/es", anything else -> ""
(define (locale-prefix path)
  (let ((parts (string-split path #\/)))
    (if (and (>= (length parts) 3) (locale? (cadr parts)))
      (string-append "/" (cadr parts))
      "")))

;; How long a rendered page stays cached, in seconds: the values Tenor's own
;; pages advertise in their config (VIEW_PAGE_TTL, SEARCH_PAGE_TTL).
(define gif-page-ttl 3600)
(define search-page-ttl 600)

;; Pages are cached by path: the same path always maps to the same Tenor page,
;; and the locale is part of the path.
(define (generic-page procedure path ttl)
  (let ((cached (cache-ref path)))
    (if cached
      (begin
        (display ";\tCache: hit")
        (values good-response cached))
      (parameterize ((current-locale (locale-prefix path)))
        (generic-page* procedure path ttl)))))

(define (generic-page* procedure path ttl)
  (with-upstream-errors
    (lambda ()
      (let ((resp "")
            (body ""))
        (receive (_resp _body)
                 (fetch (string-append "https://tenor.com" path))
                 (set! resp _resp)
                 (set! body _body))
        (display (string-append ";\tStatus: "
                                (number->string (response-code resp))))
        (cond ((equal? (response-code resp) 200)
               (let ((html (string-replace-substring
                             (shtml->html (procedure body))
                             "&amp;" "&")))
                 (cache-put! path html ttl)
                 (values good-response html)))
              ((and (>= (response-code resp) 300)
                    (<= (response-code resp) 399))
               (redirect resp (response-code resp)))
              (else (error-page (response-code resp))))))))

(define (gif-page path)
  (display ";\tHandler: gif-page")
  (generic-page gif-template path gif-page-ttl))

(define (search-page path)
  (display ";\tHandler: search-page")
  (generic-page search-template path search-page-ttl))

;; Search box text -> Tenor's search slug, or #f when there are no words.
;; Forms encode spaces as '+', which becomes a space before percent-decoding,
;; so an actual plus sign (sent as %2B) is kept.
(define (search-slug raw)
  (let* ((spaced (string-map (lambda (c) (if (char=? c #\+) #\space c)) raw))
         (text (catch #t (lambda () (uri-decode spaced)) (lambda _ spaced))))
    (words->slug text)))

;; PREFIX is the locale prefix ("" or e.g. "/es") the search came from.
(define (search-redirect-page query prefix)
  (display ";\tHandler: search-redirect-page")
  (let ((slug (search-slug (or (query-ref query "q") ""))))
    (values (build-response
              #:code 301
              #:headers `((location .
                            ,(parse-header
                               'location
                               (if slug
                                 (string-append prefix "/search/"
                                                (uri-encode slug) "-gifs")
                                 "/")))))
            "Redirect")))

(define (index-page host)
  (display ";\tHandler: index-page") (newline)
  (values good-response (shtml->html (index-template host))))

(define (proxy-page query)
  (display ";\tHandler: proxy-page")
  (let ((resp "")
        (body "")
        (url (uri-decode (or (query-ref query "url") ""))))
    (if (or (string-prefix? "https://media1.tenor.com/" url)
            (string-prefix? "https://media.tenor.com/" url))
      (with-upstream-errors
        (lambda ()
          (receive (_resp _body) (fetch url)
                   (set! resp _resp)
                   (set! body _body))
          (cond
            ((equal? (response-code resp) 200)
             (values (build-response
                       #:code 200
                       #:headers `((content-type .
                                    ,(response-content-type resp))
                                   (cache-control .
                                    ,(parse-header 'cache-control
                                                   "max-age=604800"))))
                     body))
            ((and (>= (response-code resp) 300)
                  (<= (response-code resp) 399))
             (values (build-response
                       #:code (response-code resp)
                       #:headers `((location . ,(parse-header
                                                  'location
                                                  (uri-path
                                                    (response-location
                                                      resp))))
                                   (content-type .
                                    ,(response-content-type resp))))
                     "Redirect"))
            (else (error-page 404)))))
      (error-page 400))))
