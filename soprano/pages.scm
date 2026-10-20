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
  #:use-module (htmlprag)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 string-fun)
  #:export (error-page
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

(define (generic-page procedure path)
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
               (values good-response
                       (string-replace-substring
                         (shtml->html (procedure body))
                         "&amp;" "&")))
              ((and (>= (response-code resp) 300)
                    (<= (response-code resp) 399))
               (redirect resp (response-code resp)))
              (else (error-page (response-code resp))))))))

(define (gif-page path)
  (display ";\tHandler: gif-page")
  (generic-page gif-template path))

(define (search-page path)
  (display ";\tHandler: search-page")
  (generic-page search-template path))

(define (search-redirect-page query)
  (display ";\tHandler: search-redirect-page")
  (let ((q (or (query-ref query "q") "")))
    (values (build-response
              #:code 301
              #:headers `((location .
                            ,(parse-header
                               'location
                               (if (string-null? q)
                                 "/"
                                 (string-append
                                   "/search/"
                                   (string-replace-substring q " " "-")
                                   "-gifs"))))))
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
