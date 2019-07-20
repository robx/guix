;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix import elm)
  #:use-module (ice-9 match)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (guix base32)
  #:use-module (guix build-system elm)
  #:export (elm.json->guix-package elm->guix-package))

(define (elm-package-name name)
  "Given the NAME of an Elm package, return a Guix-compliant name for
the package."
  (match (string-split name #\/)
    ((author project) (if (string-prefix? "elm-" project)
                          (snake-case project)
                          (string-append "elm-" (snake-case project))))
    (_                (error "invalid elm package name: " name))))

(define (make-elm-package-sexp name version summary license)
;  "Return the `package' s-expression for a Ruby package with the given NAME,
;VERSION, HASH, HOME-PAGE, DESCRIPTION, DEPENDENCIES, and LICENSES."
  `(package
     (name ,(elm-package-name name))
     (version ,version)
     (source (origin
               (method url-fetch)
               (uri (elm-package-uri ,name version))
               (sha256
                (base32
                 ,(guix-hash-url (elm-package-uri name version))))))
     (build-system elm-package-build-system)
     (synopsis ,summary)
     (description ,summary)
     (home-page #f)
     (license ,(spdx-string->license license))))

(define (make-elm-app-sexp dependencies)
;  "Return the `package' s-expression for a Ruby package with the given NAME,
;VERSION, HASH, HOME-PAGE, DESCRIPTION, DEPENDENCIES, and LICENSES."
  (define (bind-dependency dep)
    (match dep
      ((name . pkgsexp)
         (list (string->symbol name) pkgsexp))))
  (define (add-dependency  dep)
    (match dep
      ((name . _)
         (list name (list 'unquote (string->symbol name))))))
  `((let
      (,@(map bind-dependency dependencies))
      (package
        (name #f)
        (version #f)
        (source #f)
        (build-system elm-application-build-system)
        (native-inputs
         ,(list 'quasiquote (map add-dependency dependencies)))
        (synopsis #f)
        (description #f)
        (home-page #f)
        (license #f)))))

(define (get-dependencies elm.json)
  (let* ((deps     (assoc-ref elm.json "dependencies"))
         (direct   (assoc-ref deps "direct"))
         (indirect (assoc-ref deps "indirect")))
    (append direct indirect)))

(define (elm.json->guix-package elm.json)
  "Read package metadata from the given ELM.JSON file, and return
the `package' s-expression corresponding to that package."
  (let ((type    (assoc-ref elm.json "type")))
    (cond
      ((equal? type "package")
         (let* ((name    (assoc-ref elm.json "name"))
                (version (assoc-ref elm.json "version"))
                (license (assoc-ref elm.json "license"))
                (summary (assoc-ref elm.json "summary")))
           (make-elm-package-sexp name version summary license)))
      ((equal? type "application")
         (make-elm-app-sexp
           (map (match-lambda ((name . version)
                               `(,(elm-package-name name) . ,(elm->guix-package name version))))
                (get-dependencies elm.json))))
      (else
         (error "unsupported elm package type: " type)))))

(define (elm-fetch-json name version)
  "Return an alist representation of the elm.json metadata for the package NAME,
at version VERSION."
  (or
    (json-fetch
      (string-append "https://package.elm-lang.org/packages/"
                     name "/" version "/elm.json"))
    (error "elm.json not found")))

(define* (elm->guix-package name #:optional (version "latest"))
  (elm.json->guix-package (elm-fetch-json name version)))
