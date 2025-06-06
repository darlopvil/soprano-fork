;; Copyright (C) 2024-2025 Skylar Astaroth <cobra@vern.cc>
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

(define (generic-page procedure path)
  (let ((resp "")
        (body ""))
    (receive (_resp _body)
             (http-request (string-append "https://tenor.com" path))
             (set! resp _resp)
             (set! body _body))
    (display (string-append ";\tStatus: "
                            (number->string (response-code resp))))
    (newline)
    (cond ((equal? (response-code resp) 200)
           (let ((doc (html->sxml-0nf body #:strict? #t)))
             (values good-response
                     (string-replace-substring
                       (shtml->html (procedure doc))
                       "&amp;" "&"))))
          ((and (>= (response-code resp) 300) (<= (response-code resp) 399))
           (redirect resp (response-code resp)))
          (else (error-page (response-code resp))))))

(define (gif-page path)
  (display ";\tHandler: gif-page")
  (generic-page gif-template path))

(define (search-page path)
  (display ";\tHandler: search-page")
  (generic-page search-template path))

(define (search-redirect-page query)
  (display ";\tHandler: search-redirect-page")
  (let ((q ""))
    (map (lambda (p)
           (if (equal? (car p) "q")
             (set! q (cadr p))))
         (map (lambda (s)
                (string-split s #\=))
              (string-split query #\&)))
    (values (build-response
              #:code 301
              #:headers `((location .
                            ,(parse-header
                               'location
                               (string-append
                                 "/search/"
                                 (string-replace-substring q " " "-")
                                 "-gifs")))))
            "Redirect")))

(define (index-page host)
  (display ";\tHandler: index-page") (newline)
  (values good-response (shtml->html (index-template host))))

(define (proxy-page query)
  (display ";\tHandler: proxy-page") (newline)
  (let ((resp "")
        (body "")
        (url "")
        (sub ""))
    (map (lambda (p)
           (if (equal? (car p) "url")
             (set! url (cadr p)))
           (if (equal? (car p) "sub")
             (set! sub (cadr p))))
         (map (lambda (s)
                (string-split s #\=))
              (string-split query #\&)))
    (if (or
          (and
            (>= (string-length url) 25)
            (equal? (substring url 0 25) "https://media1.tenor.com/"))
          (and
            (>= (string-length url) 24)
            (equal? (substring url 0 24) "https://media.tenor.com/")))
      (begin
        (receive (_resp _body) (http-request url)
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
          ((and (>= (response-code resp) 300) (<= (response-code resp) 399))
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
          (else (error-page 404))))
      (error-page 400))))
