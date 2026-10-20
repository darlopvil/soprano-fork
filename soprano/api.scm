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

;; More search results, beyond the 50 a Tenor search page holds.
;;
;; Tenor's public API is discontinued: it now answers 403 "Tenor API is
;; discontinued" unless the request comes from Tenor's own web client, which
;; loads further results through it as people scroll.  Tenor's pages publish
;; that client's API key and client id in their config.  Soprano learns them
;; from the pages it fetches anyway, and uses them for one thing only: the
;; next page of a search, one request per click on "Next page", with the same
;; parameters tenor.com sends for its own infinite scroll.
;;
;; This may stop working at any time.  If a call fails, more results are
;; switched off for an hour: searches keep working, they just show no
;; "Next page" link.  Then it is tried again.

(define-module (soprano api)
  #:use-module (soprano upstream)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:export (learn-api-config!
            api-config-known?
            more-results-available?
            search-more))

(define retry-after 3600)          ; seconds without more results after a failure

(define lock (make-mutex))
(define api-key #f)
(define client-key #f)
(define off-until 0)

(define base64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;; Standard base64 -> bytevector.  Padding and anything outside the alphabet
;; are skipped.
(define (base64-decode text)
  (let loop ((chars (string->list text)) (acc 0) (bits 0) (out '()))
    (if (null? chars)
      (u8-list->bytevector (reverse out))
      (let ((value (string-index base64-alphabet (car chars))))
        (if (not value)
          (loop (cdr chars) acc bits out)
          (let ((acc (logior (ash acc 6) value))
                (bits (+ bits 6)))
            (if (>= bits 8)
              (loop (cdr chars)
                    (logand acc (1- (ash 1 (- bits 8))))
                    (- bits 8)
                    (cons (ash acc (- 8 bits)) out))
              (loop (cdr chars) acc bits out))))))))

;; Tenor pages carry their config as base64 JSON in <script id="data">.
;; Reads the API key and client id from it, if the page has them.
(define (learn-api-config! body)
  (let* ((tag (string-contains body "<script id=\"data\""))
         (start (and tag (string-index body #\> tag)))
         (end (and start (string-contains body "</script>" start))))
    (when end
      (catch #t
        (lambda ()
          (let* ((config (json-string->scm
                          (utf8->string
                           (base64-decode (substring body (1+ start) end)))))
                 (key (assoc-ref config "API_V2_KEY"))
                 (client (assoc-ref config "API_V2_CLIENT_KEY")))
            (when (and (string? key) (string? client))
              (with-mutex lock
                (set! api-key key)
                (set! client-key client)))))
        (const #f)))))

(define (api-config-known?)
  (with-mutex lock
    (and api-key client-key #t)))

(define (more-results-available?)
  (with-mutex lock
    (and api-key client-key (>= (current-time) off-until) #t)))

(define (switch-off!)
  (with-mutex lock
    (set! off-until (+ (current-time) retry-after))))

;; The formats tenor.com asks for.
(define media-filter
  "gif,gif_transparent,mediumgif,tinygif,tinygif_transparent,webp,webp_transparent,tinywebp,tinywebp_transparent,tinymp4,mp4,webm,originalgif,gifpreview")

(define (query-string params)
  (string-join (map (lambda (param)
                      (string-append (car param) "=" (uri-encode (cdr param))))
                    params)
               "&"))

;; The next page of results for search Q, from cursor POS.  LOCALE is #f or
;; Tenor's locale tag.  -> the parsed JSON response (results and next).
;; Throws 'more-unavailable when more results are switched off or the call
;; fails; a failure also switches them off.
(define (search-more q pos locale)
  (let ((params (with-mutex lock
                  (and api-key client-key (>= (current-time) off-until)
                       `(("q" . ,q)
                         ("key" . ,api-key)
                         ("client_key" . ,client-key)
                         ("limit" . "50")
                         ("contentfilter" . "low")
                         ("searchfilter" . "none")
                         ("media_filter" . ,media-filter)
                         ,@(if locale `(("locale" . ,locale)) '())
                         ("pos" . ,pos))))))
    (unless params
      (throw 'more-unavailable 'off))
    (catch #t
      (lambda ()
        (receive (response body)
            (fetch (string-append "https://tenor.googleapis.com/v2/search?"
                                  (query-string params)))
          (unless (= (response-code response) 200)
            (throw 'more-unavailable (response-code response)))
          (let ((data (json-string->scm
                       (if (bytevector? body) (utf8->string body) body))))
            (unless (vector? (assoc-ref data "results"))
              (throw 'more-unavailable 'no-results))
            data)))
      (lambda (key . args)
        (switch-off!)
        (throw 'more-unavailable (if (eq? key 'more-unavailable)
                                     (if (pair? args) (car args) key)
                                     key))))))
