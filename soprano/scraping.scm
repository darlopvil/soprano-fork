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

(define-module (soprano scraping)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (proxy get-gif get-search))

(define (proxy url)
  (string-append "/proxy?url=" url))

;; Tenor pages embed the data they render as JSON, in
;; <script id="store-cache" type="text/x-cache">.  Reading it is far more
;; robust than walking the DOM through absolute element paths, and it holds
;; every result, every media format, tags and related GIFs.
(define (store-cache body)
  (let* ((tag (string-contains body "<script id=\"store-cache\""))
         (start (and tag (string-index body #\> tag)))
         (end (and start (string-contains body "</script>" start))))
    (if end
      (json-string->scm (substring body (1+ start) end))
      (throw 'store-cache-missing))))

;; Nested lookup: string keys index JSON objects (alists), numbers index
;; arrays (vectors).  Returns #f as soon as something is missing.
(define (ref data . keys)
  (fold (lambda (key data)
          (cond ((not data) #f)
                ((and (string? key) (list? data)) (assoc-ref data key))
                ((and (integer? key) (vector? data) (< key (vector-length data)))
                 (vector-ref data key))
                (else #f)))
        data
        keys))

;; Value of the only (or first) member of a JSON object.
(define (first-value data)
  (and (pair? data) (pair? (car data)) (cdar data)))

(define (media-url gif . formats)
  (any (lambda (name) (ref gif "media_formats" name "url")) formats))

(define (sticker? gif)
  (let ((flags (ref gif "flags")))
    (and (vector? flags) (member "sticker" (vector->list flags)) #t)))

(define (media-width gif name)
  (ref gif "media_formats" name "dims" 0))

;; Thumbnails are always images, never videos: hardened browsers block video
;; autoplay, even muted, and a frontend must not ask people to loosen their
;; browser settings.  Stickers keep a transparent image.  Everything else
;; prefers tinywebp, much lighter than tinygif, but only when it is wide
;; enough for a ~200px column: tinywebp fits a 200x200 box, so tall images
;; come out very narrow.  Those use the full-size webp instead, which is
;; still lighter than tinygif.  -> (url width height), or #f.
(define (thumbnail gif)
  (let ((name (find (lambda (name) (ref gif "media_formats" name "url"))
                    (cond ((sticker? gif)
                           '("tinywebp_transparent" "tinygif_transparent"
                             "tinygif"))
                          ((let ((width (media-width gif "tinywebp")))
                             (and (real? width) (>= width 180)))
                           '("tinywebp"))
                          (else
                           '("webp" "tinygif"))))))
    (and name
         (list (ref gif "media_formats" name "url")
               (media-width gif name)
               (ref gif "media_formats" name "dims" 1)))))

;; "https://tenor.com/view/..." -> "/view/..."
(define (local-path url)
  (and (string? url)
       (string-prefix? "https://tenor.com/" url)
       (substring url (string-length "https://tenor.com"))))

;; -> (title media-url)
(define (get-gif body)
  (let* ((gif (ref (first-value (ref (store-cache body) "gifs" "byId"))
                   "results" 0))
         (title (ref gif "h1_title"))
         (url (media-url gif "mediumgif" "gif")))
    (if (and (string? title) url)
      (list title url)
      (throw 'gif-missing))))

;; Tenor lays search results out in columns and lists them column by column
;; in its HTML.  Sorting by that layout keeps the order Soprano always showed;
;; without layout details, results keep Tenor's ranking order (sort is stable).
(define (layout-key gif)
  (let ((column (ref gif "details" "column"))
        (y (ref gif "details" "y")))
    (cons (if (real? column) column 0)
          (if (real? y) y 0))))

(define (layout<? a b)
  (let ((ka (layout-key a))
        (kb (layout-key b)))
    (or (< (car ka) (car kb))
        (and (= (car ka) (car kb))
             (< (cdr ka) (cdr kb))))))

;; -> list of (local-link url width height), see thumbnail
(define (get-search body)
  (let ((results (ref (first-value (ref (store-cache body) "universal" "search"))
                      "results")))
    (filter-map
      (lambda (gif)
        (let ((href (local-path (ref gif "itemurl")))
              (thumb (thumbnail gif)))
          (and href thumb (cons href thumb))))
      (if (vector? results)
        (sort (vector->list results) layout<?)
        '()))))
