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

(define-module (gnu packages elm)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system elm)
  #:use-module (guix build-system haskell)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;; The full elm build calls out to itself via Template Haskell to
;; compile the elm reactor web app. elm reactor isn't required to
;; compile elm applications, so we take this part out of this
;; bootstrap package.
(define-public elm-compiler
  (package
    (name "elm-compiler")
    (version "0.19.0")
    (source
     (origin
       (method git-fetch)
       (file-name (git-file-name name version))
       (uri (git-reference
             (url "https://github.com/elm/compiler/")
             (commit version)))
       (sha256
        (base32 "0s93z9vr0vp5w894ghc5s34nsq09sg1msf59zfiba87sid5vgjqy"))
       (patches
        (search-patches "elm-compiler-disable-reactor.patch"
                        "elm-compiler-fix-map-key.patch"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'update-constraints
           (lambda _
             (substitute* "elm.cabal"
               (("ansi-terminal >= 0\\.8 && < 0\\.9,")
                "ansi-terminal >= 0.8 && < 0.10,")
               (("containers >= 0\\.5\\.8\\.2 && < 0\\.6,")
                "containers >= 0.5.8.2 && < 0.7,")
               (("http-client >= 0\\.5 && < 0\\.6,")
                "http-client >= 0.5 && < 0.7,")
               (("language-glsl >= 0\\.0\\.2 && < 0\\.3,")
                "language-glsl >= 0.0.2 && < 0.4,")
               (("network >= 2\\.4 && < 2\\.7,")
                "network >= 2.4 && < 2.9,"))
             #t)))))
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-edit-distance" ,ghc-edit-distance)
       ("ghc-file-embed" ,ghc-file-embed)
       ("ghc-http" ,ghc-http)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-language-glsl" ,ghc-language-glsl)
       ("ghc-logict" ,ghc-logict)
       ("ghc-network" ,ghc-network)
       ("ghc-raw-strings-qq" ,ghc-raw-strings-qq)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-sha" ,ghc-sha)
       ("ghc-snap-core" ,ghc-snap-core)
       ("ghc-snap-server" ,ghc-snap-server)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-vector" ,ghc-vector)
       ("ghc-zip-archive" ,ghc-zip-archive)))
    (home-page "https://elm-lang.org")
    (synopsis "Programming language for Web applications")
    (description
     "This package provides Elm, a statically-typed functional programming
language for the browser.  It includes commands for developers such as
@command{elm make} and @command{elm repl}.")
    (license license:bsd-3)))

(define-public elm
  (package
    (name "elm")
    (version "0.19.0")
    (source
     (origin
       (method git-fetch)
       (file-name (git-file-name name version))
       (uri (git-reference
             (url "https://github.com/elm/compiler/")
             (commit version)))
       (sha256
        (base32 "0s93z9vr0vp5w894ghc5s34nsq09sg1msf59zfiba87sid5vgjqy"))
       (patches
        (search-patches "elm-include-reactor.patch"
                        "elm-compiler-relax-glsl-bound.patch"
                        "elm-compiler-fix-map-key.patch"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-reactor
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-file
              (string-append (assoc-ref inputs "elm-reactor") "/elm.js")
              "ui/browser/assets/elm.js"))))))
    (inputs
     `(("elm-reactor" ,elm-reactor)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-edit-distance" ,ghc-edit-distance)
       ("ghc-file-embed" ,ghc-file-embed)
       ("ghc-http" ,ghc-http)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-language-glsl" ,ghc-language-glsl)
       ("ghc-logict" ,ghc-logict)
       ("ghc-network" ,ghc-network)
       ("ghc-raw-strings-qq" ,ghc-raw-strings-qq)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-sha" ,ghc-sha)
       ("ghc-snap-core" ,ghc-snap-core)
       ("ghc-snap-server" ,ghc-snap-server)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-vector" ,ghc-vector)
       ("ghc-zip-archive" ,ghc-zip-archive)))
    (home-page "https://elm-lang.org")
    (synopsis "The `elm` command line interface, including `elm reactor`.")
    (description
     "This package provides Elm, a statically-typed functional programming
language for the browser.  It includes commands for developers such as
@command{elm make} and @command{elm repl}.")
    (license license:bsd-3)))

(define elm-reactor
  (let
      ((elm-virtual-dom
        (package
          (name "elm-virtual-dom")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri "elm/virtual-dom" version))
             (sha256
              (base32
               "0hm8g92h7z39km325dlnhk8n00nlyjkqp3r3jppr37k2k13md6aq"))))
          (build-system elm-package-build-system)
          (synopsis
           "Core virtual DOM implementation, basis for HTML and SVG libraries")
          (description
           "Core virtual DOM implementation, basis for HTML and SVG libraries")
          (home-page #f)
          (license license:bsd-3)))
       (elm-parser
        (package
          (name "elm-parser")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri "elm/parser" version))
             (sha256
              (base32
               "0k4zlq30lrvawqvzwbvsl0hrmwf9s832mb41z7fdspm4549dj7wc"))))
          (build-system elm-package-build-system)
          (synopsis
           "a parsing library, focused on simplicity and great error messages")
          (description
           "a parsing library, focused on simplicity and great error messages")
          (home-page #f)
          (license license:bsd-3)))
       (elm-time
        (package
          (name "elm-time")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri "elm/time" version))
             (sha256
              (base32
               "0vch7i86vn0x8b850w1p69vplll1bnbkp8s383z7pinyg94cm2z1"))))
          (build-system elm-package-build-system)
          (synopsis
           "Work with POSIX times, time zones, years, months, days, hours, seconds, etc.")
          (description
           "Work with POSIX times, time zones, years, months, days, hours, seconds, etc.")
          (home-page #f)
          (license license:bsd-3)))
       (elm-url
        (package
          (name "elm-url")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri "elm/url" version))
             (sha256
              (base32
               "0av8x5syid40sgpl5vd7pry2rq0q4pga28b4yykn9gd9v12rs3l4"))))
          (build-system elm-package-build-system)
          (synopsis
           "Create and parse URLs. Use for HTTP and \"routing\" in single-page apps (SPAs)")
          (description
           "Create and parse URLs. Use for HTTP and \"routing\" in single-page apps (SPAs)")
          (home-page #f)
          (license license:bsd-3)))
       (elm-svg
        (package
          (name "elm-svg")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri "elm/svg" version))
             (sha256
              (base32
               "08x0v8p9wm699jjmsnbq69pxv3jh60j4f6fg7y6hyr7xxj85y390"))))
          (build-system elm-package-build-system)
          (synopsis
           "Fast SVG, rendered with virtual DOM diffing")
          (description
           "Fast SVG, rendered with virtual DOM diffing")
          (home-page #f)
          (license license:bsd-3)))
       (elm-core
        (package
          (name "elm-core")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri "elm/core" version))
             (sha256
              (base32
               "10kr86h4v5h4p0586q406a5wbl8xvr1jyrf6097zp2wb8sv21ylw"))))
          (build-system elm-package-build-system)
          (synopsis "Elm's standard libraries")
          (description "Elm's standard libraries")
          (home-page #f)
          (license license:bsd-3)))
       (elm-http
        (package
          (name "elm-http")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri "elm/http" version))
             (sha256
              (base32
               "1igmm89ialzrjib1j8xagkxalq1x2gj4l0hfxcd66mpwmvg7psl8"))))
          (build-system elm-package-build-system)
          (synopsis "Make HTTP requests")
          (description "Make HTTP requests")
          (home-page #f)
          (license license:bsd-3)))
       (elm-html
        (package
          (name "elm-html")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri "elm/html" version))
             (sha256
              (base32
               "1n3gpzmpqqdsldys4ipgyl1zacn0kbpc3g4v3hdpiyfjlgh8bf3k"))))
          (build-system elm-package-build-system)
          (synopsis
           "Fast HTML, rendered with virtual DOM diffing")
          (description
           "Fast HTML, rendered with virtual DOM diffing")
          (home-page #f)
          (license license:bsd-3)))
       (elm-json
        (package
          (name "elm-json")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri "elm/json" version))
             (sha256
              (base32
               "1g0hafkqf2q633r7ir9wxpb1lnlzskhpsyi0h5bkzj0gl072zfnb"))))
          (build-system elm-package-build-system)
          (synopsis "Encode and decode JSON values")
          (description "Encode and decode JSON values")
          (home-page #f)
          (license license:bsd-3)))
       (elm-markdown
        (package
          (name "elm-markdown")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri
                   "elm-explorations/markdown"
                   version))
             (sha256
              (base32
               "0k3110ixa4wwf3vkkdplagwah9ypr965qxr1y147rnsc1xsxmr6y"))))
          (build-system elm-package-build-system)
          (synopsis "Fast markdown parsing and rendering")
          (description
           "Fast markdown parsing and rendering")
          (home-page #f)
          (license license:bsd-3)))
       (elm-project-metadata-utils
        (package
          (name "elm-project-metadata-utils")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri
                   "elm/project-metadata-utils"
                   version))
             (sha256
              (base32
               "1d4rd4grrnbdvj9gf00h7dr6hbkjzawgkzpizfrkp1z1pyr3mvq9"))))
          (build-system elm-package-build-system)
          (synopsis
           "Work with elm.json and docs.json files in Elm")
          (description
           "Work with elm.json and docs.json files in Elm")
          (home-page #f)
          (license license:bsd-3)))
       (elm-browser
        (package
          (name "elm-browser")
          (version "1.0.0")
          (source
           (origin
             (method url-fetch)
             (uri (elm-package-uri "elm/browser" version))
             (sha256
              (base32
               "1apmvyax93nvmagwj00y16zx10kfv640cxpi64xgqbgy7d2wphy4"))))
          (build-system elm-package-build-system)
          (synopsis
           "Run Elm in browsers, with access to browser history for single-page apps (SPAs)")
          (description
           "Run Elm in browsers, with access to browser history for single-page apps (SPAs)")
          (home-page #f)
          (license license:bsd-3))))
    (package
      (name "elm-reactor")
      (version "0.19.0")
      (source
       (origin
         (method url-fetch)
         (file-name "elm-0.19.0.tar.gz")
         (uri "https://github.com/elm/compiler/archive/0.19.0.tar.gz")
         (sha256
          (base32 "0g4risrjrvngz3j4wf432j82gjcc8i1b7l5lwbb0fhr24hvz6ka9"))
                                        ; FIXME: extract reactor subdirectory, there must be a better way to do this
         (snippet #~(begin
                      (use-modules (guix build utils) (ice-9 ftw))
                      (let ((files (scandir "." (lambda (f) (not (or (equal? f ".")
                                                                     (equal? f "..")
                                                                     (equal? f "ui")))))))
                        (for-each delete-file-recursively files)
                        (copy-recursively "ui/browser" ".")
                        (delete-file-recursively "ui"))))))
      (build-system elm-application-build-system)
      (arguments '(#:elm-modules ((("Errors.elm" "Index.elm" "NotFound.elm") . "elm.js"))))
      (native-inputs
       `(("elm-virtual-dom" ,elm-virtual-dom)
         ("elm-parser" ,elm-parser)
         ("elm-time" ,elm-time)
         ("elm-url" ,elm-url)
         ("elm-svg" ,elm-svg)
         ("elm-core" ,elm-core)
         ("elm-http" ,elm-http)
         ("elm-html" ,elm-html)
         ("elm-json" ,elm-json)
         ("elm-markdown" ,elm-markdown)
         ("elm-project-metadata-utils"
          ,elm-project-metadata-utils)
         ("elm-browser" ,elm-browser)))
      (synopsis "Elm's reactor, internal to elm")
      (description "Elm's reactor")
      (home-page "https://elm-lang.org")
      (license bsd-3))))

(define ghc-indents-0.3
  (package
    (name "ghc-indents")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/indents/indents-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "16lz21bp9j14xilnq8yym22p3saxvc9fsgfcf5awn2a6i6n527xn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-concatenative" ,ghc-concatenative)))
    (home-page
     "http://patch-tag.com/r/salazar/indents")
    (synopsis
     "indentation sensitive parser-combinators for parsec")
    (description
     "This library provides functions for use in parsing indentation sensitive contexts.")
    (license bsd-3)))

(define-public elm-format
  (package
    (name "elm-format")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/avh4/elm-format/archive/"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "1dywzhdp25iki9kk9fvwz6d9zyxzapnk1fmj03082ca71r40mpzx"))
       (patches
        (search-patches "elm-format-setup.patch"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f
       #:haddock? #f))
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-free" ,ghc-free)
       ("ghc-indents" ,ghc-indents-0.3)
       ("ghc-json" ,ghc-json)
       ("ghc-optparse-applicative"
        ,ghc-optparse-applicative)
       ("ghc-split" ,ghc-split)))
    (home-page "https://github.com/avh4/elm-format")
    (synopsis "Source code formatter for Elm")
    (description
     "A simple way to format your Elm code according to the official style guide.")
    (license bsd-3)))
