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

(define-module (guix build-system elm)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix build-system trivial) ; guile-for-build
  #:use-module ((guix build-system gnu)
                #:select (standard-packages)
                #:prefix gnu:)
  #:use-module (ice-9 match)
  #:export (elm-package-uri elm-package-build-system elm-application-build-system))

(define (elm-compiler)
  (let ((elm (resolve-interface '(gnu packages elm))))
    (module-ref elm 'elm-compiler)))

;(define (tar)
;  (let ((base (resolve-interface '(gnu packages base))))
;    (module-ref base 'tar)))

;(define (gzip)
;  (let ((compression (resolve-interface '(gnu packages compression))))
;    (module-ref compression 'gzip)))

;(define (xz)
;  (let ((compression (resolve-interface '(gnu packages compression))))
;    (module-ref compression 'xz)))

(define (elm-package-uri name version)
  "Return a URI string for the elm source archive for the release corresponding to
NAME and VERSION."
  (string-append "https://github.com/" name "/archive/" version ".tar.gz"))

(define* (lower-package name
                #:key source inputs native-inputs outputs system target
                guile builder modules allowed-references)
  "Return a bag for NAME."
  (bag
    (name name)
    (system system)
    (target target)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs
                   ,@(gnu:standard-packages)))
    (build-inputs `())
    (outputs outputs)
    (build elm-package-build)
    (arguments `(#:guile ,guile
                 #:modules ,modules
                 #:allowed-references ,allowed-references))))

(define* (lower-application name
                #:key source inputs native-inputs outputs system target
                guile builder modules allowed-references
                (elm-modules '((("Main.elm") . "main.js"))))
  "Return a bag for NAME."
  (bag
    (name name)
    (system system)
    (target target)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ("elm-compiler" ,(elm-compiler))
                   ,@inputs
                   ,@(gnu:standard-packages)))
    (build-inputs `(,@native-inputs))
    (outputs outputs)
    (build elm-application-build)
    (arguments `(#:guile ,guile
                 #:modules ,modules
                 #:elm-modules ,elm-modules
                 #:allowed-references ,allowed-references))))

(define (extract-source inputs)
  (match (assoc-ref inputs "source")
         (((? derivation? source))
          (derivation->output-path source))
         ((source)
          source)
         (source
          source)))

(define* (elm-package-build store name inputs
                            #:key
                            outputs guile system (modules '())
                            search-paths allowed-references)
  (define builder
    `(begin
       (use-modules (guix build elm-build-system))
       (elm-package-build ,(extract-source inputs) %build-inputs %output)))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:outputs outputs
                                #:modules '((guix build utils) (guix build elm-build-system) (guix build json))
                                #:allowed-references #f
                                #:guile-for-build
                                (guile-for-build store guile system)))

(define* (elm-application-build store name inputs
                                #:key elm-modules
                                outputs guile system (modules '())
                                search-paths allowed-references)

  (define builder
    `(begin
       (use-modules (guix build elm-build-system))
       (elm-application-build ,(extract-source inputs)
                              %build-inputs
                              %output
                              (quote ,elm-modules))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:outputs outputs
                                #:modules '((guix build utils) (guix build elm-build-system) (guix build json))
                                #:allowed-references #f
                                #:guile-for-build
                                (guile-for-build store guile system)))

(define elm-package-build-system
  (build-system
    (name 'elm-package)
    (description
     "Elm package build system, merely unpacking the source archive.")
    (lower lower-package)))

(define elm-application-build-system
  (build-system
    (name 'elm-application)
    (description
     "Elm application build system.")
    (lower lower-application)))
