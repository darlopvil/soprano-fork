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

(define-module (soprano scraping)
  #:use-module (sxml xpath)
  #:export (proxy get-gif get-search))

(define (proxy url)
  (string-append "/proxy?url=" url))

(define (get-gif d)
  (let ((pref '(html body div div div div)))
    (append
      ((sxpath (append pref '(h1 *text*))) d)
      ((sxpath (append pref '(div div div div div img @ src *text*))) d))))

(define (get-search d)
  (define pref '(// figure a))
  (map cons
    ((sxpath (append pref '(@ href *text*))) d)
    ((sxpath (append pref '(div // img @ src *text*))) d)))
