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

(define-module (soprano templates)
  #:use-module (soprano scraping)
  #:use-module (htmlprag)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:export (current-locale
            words->slug
            error-template
            index-template
            gif-template
            search-template))

(define (html-head title)
  `(head (title ,(if title (string-append title " - Soprano") "Soprano"))
         (meta (@ (charset "UTF-8")))
         (meta (@ (name "viewport") (content "width=device-width")))
         (meta (@ (name "robots") (content "noindex, nofollow")))
         (link (@ (rel "stylesheet") (href "/style.css")))
         (link (@ (rel "icon") (type "image/png") (href "/favicon.png")))))

;; Locale prefix of the page being built ("" or e.g. "/es"), so searching
;; from a localized page stays in that locale.
(define current-locale (make-parameter ""))

;; Every page: a real doctype (without one, browsers render in quirks mode),
;; a language and the shared head.  TITLE #f is the plain "Soprano".
(define (page title . body)
  `(*TOP* (*DECL* DOCTYPE html)
          (html (@ (lang "en"))
                ,(html-head title)
                (body ,@body))))

;; Only the index focuses the search box: elsewhere it would pop up the
;; on-screen keyboard on phones every time a GIF is opened.
(define* (heading #:key autofocus?)
  `(header (a (@ (href "/"))
              (img (@ (src "/favicon.png"))))
           (form (@ (id "search")
                    (role "search")
                    (method "get")
                    (action ,(string-append (current-locale) "/search")))
                 (input (@ (type "search")
                           (id "q")
                           (name "q")
                           (placeholder "Search")
                           ,@(if autofocus? '((autofocus)) '())))
                 (button "Go"))))

(define (env-or name default)
  (let ((value (getenv name)))
    (if (and value (not (string-null? value)))
      value
      default)))

(define source-url
  (env-or "SOURCE_URL" "https://github.com/darlopvil/soprano-fork"))

(define footer
  `(footer
     (p (a (@ (href ,source-url))
           "Source Code"))
     ,(if (getenv "PATCHES_URL")
        `(p (a (@ (href ,(getenv "PATCHES_URL")))
               "Patches"))
        `())
     (p "Based on "
        (a (@ (href "https://codeberg.org/cobra/Soprano"))
           "Soprano")
        " by Skylar Astaroth")))

(define (index-template host)
  (page #f
        (heading #:autofocus? #t)
        '(h1 "Soprano")
        '(p "Soprano is a privacy-respecting front-end to Tenor.")
        '(br)
        '(h2 "Usage")
        '(p "You can use Soprano by replacing " (code "tenor.com")
            " with this website's domain")
        '(h3 "Example")
        '(pre (code "https://tenor.com/view/a-on-the-test-gif"
                    "-16733124990588327175"))
        '(p "becomes")
        `(pre (code "https://" ,host "/view/a-on-the-test-gif"
                    "-16733124990588327175"))
        footer))

;; Width and height let the browser reserve space before the image loads, so
;; the page does not jump around while it arrives.
(define (size-attributes width height)
  (if (and (integer? width) (integer? height))
    `((width ,(number->string width)) (height ,(number->string height)))
    '()))

;; Last path segment of a media URL, decoded: the name for a downloaded file.
(define (file-name url)
  (uri-decode (car (last-pair (string-split url #\/)))))

(define (download-links downloads)
  `(p (@ (class "downloads"))
      "Download: "
      ,@(let loop ((downloads downloads) (first? #t))
          (if (null? downloads)
            '()
            (let ((label (caar downloads))
                  (url (cdar downloads)))
              `(,@(if first? '() '(" · "))
                (a (@ (href ,(proxy url)) (download ,(file-name url)))
                   ,label)
                ,@(loop (cdr downloads) #f)))))))

;; Text -> Tenor search slug: its words joined with '-' (Tenor reads '-' as
;; a space), or #f when there are no words.
(define (words->slug text)
  (let ((words (string-tokenize text (char-set-complement char-set:whitespace))))
    (and (pair? words) (string-join words "-"))))

;; Search page of a tag, in the locale of the current page.
(define (tag-path tag)
  (let ((slug (words->slug tag)))
    (and slug
         (string-append (current-locale) "/search/" (uri-encode slug) "-gifs"))))

(define (tag-links tags)
  `(p (@ (class "tags"))
      ,@(filter-map (lambda (tag)
                      (let ((path (tag-path tag)))
                        (and path `(span (a (@ (href ,path)) "#" ,tag) " "))))
                    tags)))

;; "Uploaded by ... · View on Tenor"
(define (credits author tenor-url)
  (let ((parts (filter pair?
                       (list (if author `("Uploaded by " ,author) '())
                             (if tenor-url
                               `((a (@ (href ,tenor-url) (rel "noreferrer"))
                                    "View on Tenor"))
                               '())))))
    (if (null? parts)
      '()
      `(p (@ (class "credits"))
          ,@(car parts)
          ,@(if (pair? (cdr parts)) `(" · " ,@(cadr parts)) '())))))

;; Grid of result thumbnails, as in search results.
(define (thumbnail-grid results)
  `(div (@ (class "search list"))
        ,(map (lambda (result)
                (let ((href (car result))
                      (src (proxy (cadr result)))
                      (size (size-attributes (caddr result)
                                             (cadddr result))))
                  (if (not (string-prefix? "/gif-maker" href))
                    `(a (@ (href ,href))
                        (img (@ (src ,src)
                                (alt ,(list-ref result 4))
                                ,@size
                                (loading "lazy"))))
                    '())))
              results)))

(define (gif-template d)
  (let* ((gif (get-gif d))
         (title (assq-ref gif 'title))
         (image (assq-ref gif 'image))
         (tags (assq-ref gif 'tags))
         (downloads (assq-ref gif 'downloads))
         (related (assq-ref gif 'related)))
    (page title
          (heading)
          `(h1 ,title)
          `(img (@ (alt ,(or (assq-ref gif 'description) title))
                   (src ,(proxy (car image)))
                   ,@(size-attributes (cadr image) (caddr image))))
          (if (pair? tags) (tag-links tags) '())
          (credits (assq-ref gif 'author) (assq-ref gif 'tenor-url))
          (if (pair? downloads) (download-links downloads) '())
          (if (pair? related)
            `((h2 "Related") ,(thumbnail-grid related))
            '())
          footer)))

(define (search-template d)
  (page "Search"
        (heading)
        '(h1 "Search")
        (thumbnail-grid (get-search d))
        footer))

(define (error-message code)
  (case code
    ((400) "Bad request.")
    ((404) "Not found.")
    ((502) "Tenor could not be reached, or sent something Soprano could not read.")
    ((504) "Tenor took too long to answer.")
    (else #f)))

(define (error-template code)
  (page (number->string code)
        (heading)
        `(h1 ,(number->string code))
        (let ((message (error-message code)))
          (if message `(p ,message) '()))
        footer))
