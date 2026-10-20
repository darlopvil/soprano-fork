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
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:export (proxy get-gif get-search search-cursor more-results))

;; Characters left as they are in the url parameter of proxy links.  ':' and
;; '/' are safe inside a query and keep the links readable; everything else
;; outside this set is percent-encoded, so a media URL with its own '?', '&',
;; '+' or '%' reaches the proxy unchanged.
(define url-parameter-chars
  (string->char-set
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/"))

(define (proxy url)
  (string-append "/proxy?url="
                 (uri-encode url #:unescaped-chars url-parameter-chars)))

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

(define (sticker? gif)
  (let ((flags (ref gif "flags")))
    (and (vector? flags) (member "sticker" (vector->list flags)) #t)))

(define (media-width gif name)
  (ref gif "media_formats" name "dims" 0))

;; The first available format among NAMES -> (url width height), or #f.
(define (media gif . names)
  (let ((name (find (lambda (name) (ref gif "media_formats" name "url"))
                    names)))
    (and name
         (list (ref gif "media_formats" name "url")
               (media-width gif name)
               (ref gif "media_formats" name "dims" 1)))))

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

;; Short text describing a GIF: Tenor's content description, else its title.
(define (alt-text gif)
  (let ((description (ref gif "content_description"))
        (title (ref gif "h1_title")))
    (cond ((and (string? description) (not (string-null? description)))
           description)
          ((string? title) title)
          (else ""))))

;; -> list of (local-link url width height alt), see thumbnail.  Results that
;; cannot be linked or shown are skipped.
(define (thumbnails results)
  (filter-map
    (lambda (gif)
      (let ((href (local-path (ref gif "itemurl")))
            (thumb (thumbnail gif)))
        (and href thumb `(,href ,@thumb ,(alt-text gif)))))
    results))

(define (strings data)
  (if (vector? data)
    (filter string? (vector->list data))
    '()))

;; The GIF page shows an animated webp: the same animation as mediumgif at the
;; same or a higher resolution, about 6 times lighter.  It is an image, so it
;; plays in every browser, including those that block video autoplay.
;; Tenor's "hasaudio" flag is ignored: none of the media files it serves,
;; video ones included, carry an audio track.
;; Everything else on the page comes from the same embedded data, so it
;; costs no extra request to Tenor.  Tenor lists the GIF itself first among
;; its related GIFs; it is left out.
;; -> alist with title, description (or #f), image (url width height),
;; tags, author (or #f), tenor-url (or #f), downloads, a list of
;; (label . url), and related, see thumbnails.
(define (get-gif body)
  (let* ((store (store-cache body))
         (entry (first-value (ref store "gifs" "byId")))
         (gif (ref entry "results" 0))
         (id (ref gif "id"))
         (title (ref gif "h1_title"))
         (description (ref gif "content_description"))
         (author (ref gif "user" "username"))
         (image (media gif "webp" "mediumgif" "gif"))
         (related (ref (first-value (ref store "gifs" "related")) "results")))
    (if (and (string? title) image)
      `((title . ,title)
        (description . ,(and (string? description)
                             (not (string-null? description))
                             description))
        (image . ,image)
        (tags . ,(strings (ref gif "tags")))
        (author . ,(and (string? author)
                        (not (string-null? author))
                        author))
        (tenor-url . ,(let ((url (ref gif "itemurl")))
                        (and (string? url)
                             (string-prefix? "https://tenor.com/" url)
                             url)))
        (downloads
         . ,(filter-map (lambda (format)
                          (let ((found (media gif (cdr format))))
                            (and found (cons (car format) (car found)))))
                        '(("GIF" . "gif") ("MP4" . "mp4") ("WebM" . "webm"))))
        (related
         . ,(thumbnails
              (filter (lambda (other) (not (equal? (ref other "id") id)))
                      (if (vector? related) (vector->list related) '())))))
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

;; -> list of (local-link url width height alt), see thumbnails
(define (get-search body)
  (let ((results (ref (first-value (ref (store-cache body) "universal" "search"))
                      "results")))
    (thumbnails
      (if (vector? results)
        (sort (vector->list results) layout<?)
        '()))))

;; Cursor for the next page of a search, or #f.
(define (search-cursor body)
  (let ((next (ref (first-value (ref (store-cache body) "universal" "search"))
                   "next")))
    (and (string? next) (not (string-null? next)) next)))

;; A page of results from Tenor's API (see (soprano api)), in its ranking
;; order: there is no layout to follow.  -> (thumbnails . next-cursor-or-#f)
(define (more-results data)
  (let ((results (ref data "results"))
        (next (ref data "next")))
    (cons (thumbnails (if (vector? results) (vector->list results) '()))
          (and (string? next) (not (string-null? next)) next))))
