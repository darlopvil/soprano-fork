;; Copyright (C) 2024 Skylar Astaroth <cobra@vern.cc>
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
  #:export (error-template
            index-template
            gif-template
            search-template))

(define (html-head title)
  `(head (title ,(string-append title " - Soprano"))
         (meta (@ (charset "UTF-8")))
         (meta (@ (name "viewport") (content "width=device-width")))
         (link (@ (rel "stylesheet") (href "/style.css")))
         (link (@ (rel "icon") (type "image/png") (href "/favicon.png")))))

(define heading
  `(header (a (@ (href "/"))
              (img (@ (src "/favicon.png"))))
           (form (@ (id "search")
                    (role "search")
                    (method "get")
                    (action "/search"))
                 (input (@ (type "search")
                           (id "q")
                           (name "q")
                           (placeholder "Search")
                           (autofocus)))
                 (button "Go"))))

(define footer
  `(footer
     (p (a (@ (href "https://git.vern.cc/cobra/Soprano"))
           "Source Code"))
     ,(if (getenv "PATCHES_URL")
        `(p (a (@ (href ,(getenv "PATCHES_URL")))
               "Patches"))
        `())))

(define (index-template host)
  `(html ,(html-head "Soprano")
         (body
           ,heading
           (h1 "Soprano")
           (p "Soprano is a privacy-respecting front-end to Tenor.")
           (br)
           (h2 "Usage")
           (p "You can use Soprano by replacing " (code "tenor.com")
              " with this website's domain")
           (h3 "Example")
           (pre (code "https://tenor.com/view/a-on-the-test-gif"
                      "-16733124990588327175"))
           (p "becomes")
           (pre (code "https://" ,host "/view/a-on-the-test-gif"
                      "-16733124990588327175"))
           ,footer)))

(define (gif-template d)
  (let ((gif (get-gif d)))
    `(html ,(html-head (car gif))
           (body
             ,heading
             (h1 ,(car gif))
             (img (@ (alt ,(car gif))
                     (src ,(proxy (cadr gif)))
                     (loading "lazy")))
             ,footer))))

(define (search-template d)
  `(html ,(html-head "Search")
         (body
           ,heading
           (h1 "Search")
           (div (@ (class "search list"))
                ,(map (lambda (p)
                        (if (not (equal? (substring (car p) 0 10) "/gif-maker"))
                          `(a (@ (href ,(car p)))
                              (img (@ (src ,(proxy (cdr p)))
                                      (loading "lazy"))))
                          '()))
                      (get-search d)))
           ,footer)))

(define (error-template code)
  `(html ,(html-head (number->string code))
         (body
           ,heading
           (h1 ,(number->string code))
           ,footer)))
