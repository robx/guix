;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2014 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014 Sylvain Beucler <beuc@beuc.net>
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (gnu packages games)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages check)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tcl)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial))

(define-public gnubg
  (package
    (name "gnubg")
    (version "1.02")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://files.gnubg.org/media/sources/gnubg-release-"
                           version ".000-sources." "tar.gz"))
       (sha256
        (base32
         "015mvjk2iw1cg1kxwxfnvp2rxb9cylf6yc39i30fdy414k07zkky"))))
    (build-system gnu-build-system)
    (inputs `(("glib" ,glib)
              ("readline" ,readline)
              ("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("gtkglext" ,gtkglext)
              ("sqlite" ,sqlite)
              ("libcanberra" ,libcanberra)))
    (native-inputs `(("python-2" ,python-2)
                     ("pkg-config" ,pkg-config)))
    (home-page "http://gnubg.org")
    (synopsis "Backgammon game")
    (description "The GNU backgammon application can be used for playing, analyzing and
teaching the game.  It has an advanced evaluation engine based on artificial
neural networks suitable for both beginners and advanced players.  In
addition to a command-line interface, it also features an attractive, 3D
representation of the playing board.")
    (license license:gpl3+)))

(define-public gnubik
  (package
    (name "gnubik")
    (version "2.4.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnubik/gnubik-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0mfpwz341i1qpzi2qgslpc5i7d4fv7i01kv392m11pczqdc7i7m5"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("libx11" ,libx11)
              ("guile" ,guile-2.0)
              ("gtkglext" ,gtkglext)))
    (native-inputs `(("gettext" ,gnu-gettext)
                     ("pkg-config" ,pkg-config)))
    (home-page "https://www.gnu.org/software/gnubik/")
    (synopsis "3d Rubik's cube game")
    (description
     "GNUbik is a puzzle game in which you must manipulate a cube to make
each of its faces have a uniform color.  The game is customizable, allowing
you to set the size of the cube (the default is 3x3) or to change the colors.
You may even apply photos to the faces instead of colors.  The game is
scriptable with Guile.")
    (license license:gpl3+)))

(define-public abbaye
  (package
    (name "abbaye")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://abbaye-for-linux.googlecode.com/files/abbaye-for-linux-src-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1wgvckgqa2084rbskxif58wbb83xbas8s1i8s7d57xbj08ryq8rk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases (alist-cons-after
                 'set-paths 'set-sdl-paths
                 (lambda* (#:key inputs outputs (search-paths '()) #:allow-other-keys)
                   (define input-directories
                     (match inputs
                       (((_ . dir) ...)
                        dir)))
                   ;; This package does not use pkg-config, so modify CPATH
                   ;; variable to point to include/SDL for SDL header files.
                   (set-path-environment-variable "CPATH"
                                                  '("include/SDL")
                                                  input-directories))
                 (alist-cons-after
                  'patch-source-shebangs 'patch-makefile
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; Replace /usr with package output directory.
                    (for-each (lambda (file)
                                (substitute* file
                                  (("/usr") (assoc-ref outputs "out"))))
                              '("makefile" "src/pantallas.c" "src/comun.h")))
                  (alist-cons-before
                   'install 'make-install-dirs
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((prefix (assoc-ref outputs "out")))
                       ;; Create directories that the makefile assumes exist.
                       (mkdir-p (string-append prefix "/bin"))
                       (mkdir-p (string-append prefix "/share/applications"))
                       (mkdir-p (string-append prefix "/share/pixmaps"))))
                   ;; No configure script.
                   (alist-delete 'configure %standard-phases))))
       #:tests? #f)) ;; No check target.
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("sdl" ,sdl)
              ("sdl-gfx" ,sdl-gfx)
              ("sdl-image" ,sdl-image)
              ("sdl-mixer" ,sdl-mixer)
              ("sdl-ttf" ,sdl-ttf)))
    (home-page "http://code.google.com/p/abbaye-for-linux/")
    (synopsis "GNU/Linux port of the indie game \"l'Abbaye des Morts\"")
    (description "L'Abbaye des Morts is a 2D platform game set in 13th century
France.  The Cathars, who preach about good Christian beliefs, were being
expelled by the Catholic Church out of the Languedoc region in France.  One of
them, called Jean Raymond, found an old church in which to hide, not knowing
that beneath its ruins lay buried an ancient evil.")
    (license license:gpl3+)))

(define-public pingus
  (package
    (name "pingus")
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://pingus.googlecode.com/files/pingus-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0q34d2k6anzqvb0mf67x85q92lfx9jr71ry13dlp47jx0x9i573m"))
       (patches (list (search-patch "pingus-sdl-libs-config.patch")))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("scons" ,scons)))
    (inputs `(("sdl" ,sdl)
              ("sdl-image" ,sdl-image)
              ("sdl-mixer" ,sdl-mixer)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("libpng" ,libpng)
              ("boost" ,boost)))
    (arguments
     '(#:tests? #f                      ;no check target
       #:phases
       (alist-delete
        'configure
        (alist-replace
         'install
         (lambda* (#:key outputs #:allow-other-keys)
           (zero? (system* "make" "install"
                           (string-append "PREFIX="
                                          (assoc-ref outputs "out")))))
         %standard-phases))))
    (home-page "http://pingus.seul.org/welcome.html")
    (synopsis "Lemmings clone")
    (description
     "Pingus is a free Lemmings-like puzzle game in which the player takes
command of a bunch of small animals and has to guide them through levels.
Since the animals walk on their own, the player can only influence them by
giving them commands, like build a bridge, dig a hole, or redirect all animals
in the other direction.  Multiple such commands are necessary to reach the
level's exit.  The game is presented in a 2D side view.")
    ;; Some source files are under bsd-3 and gpl2+ licenses.
    (license license:gpl3+)))

(define-public talkfilters
  (package
    (name "talkfilters")
    (version "2.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.hyperrealm.com/" name "/"
                           name  "-" version  ".tar.gz"))
       (sha256
        (base32 "19nc5vq4bnkjvhk8srqddzhcs93jyvpm9r6lzjzwc1mgf08yg0a6"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/talkfilters")
    (synopsis "Convert English text to humorous dialects")
    (description "The GNU Talk Filters are programs that convert English text
into stereotyped or otherwise humorous dialects.  The filters are provided as
a C library, so they can easily be integrated into other programs.")
    (license license:gpl2+)))

(define-public cmatrix
  (package
    (name "cmatrix")
    (version "1.2a")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.asty.org/cmatrix/dist/cmatrix-" version
                           ".tar.gz"))
       (sha256
        (base32
         "0k06fw2n8nzp1pcdynhajp5prba03gfgsbj91bknyjr5xb5fd9hz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (alist-replace 'configure
                      (lambda* (#:key outputs #:allow-other-keys)
                        ;; This old `configure' script doesn't support
                        ;; variables passed as arguments.
                        (let ((out (assoc-ref outputs "out")))
                          (setenv "CONFIG_SHELL" (which "bash"))
                          (zero?
                           (system* "./configure"
                                    (string-append "--prefix=" out)))))
                      %standard-phases)))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://www.asty.org/cmatrix")
    (synopsis "Simulate the display from \"The Matrix\"")
    (description "CMatrix simulates the display from \"The Matrix\" and is
based on the screensaver from the movie's website.  It works with terminal
settings up to 132x300 and can scroll lines all at the same rate or
asynchronously and at a user-defined speed.")
    (license license:gpl2+)))

(define-public chess
  (package
    (name "chess")
    (version "6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/chess/gnuchess-" version
                           ".tar.gz"))
       (sha256
        (base32
         "1jckpg1qi1vjr3pqs0dnip3rmn0mgklx63xflrpqiv3cx2qlz8kn"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/chess")
    (synopsis "Full chess implementation")
    (description "GNU Chess is a chess engine.  It allows you to compete
against the computer in a game of chess, either through the default terminal
interface or via an external visual interface such as GNU XBoard.")
    (license license:gpl3+)))

(define freedink-engine
  (package
    (name "freedink-engine")
    (version "108.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "08c51imfjfcydm7h0va09z8qfw5nc837bi2x754ni2z737hb5kw2"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags '("--disable-embedded-resources")))
    (native-inputs `(("gettext" ,gnu-gettext)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("sdl" ,sdl)
              ("sdl-image" ,sdl-image)
              ("sdl-mixer" ,sdl-mixer)
              ("sdl-ttf" ,sdl-ttf)
              ("sdl-gfx" ,sdl-gfx)
              ("fontconfig" ,fontconfig)
              ("check" ,check)))
    (home-page "http://www.gnu.org/software/freedink/")
    (synopsis "Twisted adventures of young pig farmer Dink Smallwood")
    (description
     "GNU FreeDink is a free and portable re-implementation of the engine
for the role-playing game Dink Smallwood.  It supports not only the original
game data files but it also supports user-produced game mods or \"D-Mods\".
To that extent, it also includes a front-end for managing all of your D-Mods.")
    (license license:gpl3+)))

(define freedink-data
  (package
    (name "freedink-data")
    (version "1.08.20140901")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-data-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "04f1aa8gfz30qkgv7chjz5n1s8v5hbqs01h2113cq1ylm3isd5sp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-delete 'configure (alist-delete 'check %standard-phases))
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (home-page "http://www.gnu.org/software/freedink/")
    (synopsis "Game data for GNU Freedink")
    (description
     "This package contains the game data of GNU Freedink.")
    (license license:gpl3+)))

;; TODO: Add freedink-dfarc when there's a wxWidgets package.

(define-public freedink
  ;; This is a wrapper that tells the engine where to find the data.
  (package (inherit freedink-engine)
    (name "freedink")
    (build-system trivial-build-system)
    (arguments
     '(#:builder (begin
                   (use-modules (guix build utils))

                   (let* ((output     (assoc-ref %outputs "out"))
                          (bin        (string-append output "/bin"))
                          (executable (string-append bin "/freedink")))
                     (mkdir-p bin)
                     (call-with-output-file executable
                       (lambda (port)
                         (format port "#!~a/bin/sh
exec ~a/bin/freedink -refdir ~a/share/dink\n"
                                 (assoc-ref %build-inputs "bash")
                                 (assoc-ref %build-inputs "engine")
                                 (assoc-ref %build-inputs "data"))
                         (chmod port #o777)))))
       #:modules ((guix build utils))))
    (inputs `(("engine" ,freedink-engine)
              ("data" ,freedink-data)
              ("bash" ,bash)))
    (native-inputs '())))

(define-public xboard
  (package
    (name "xboard")
    (version "4.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/xboard/xboard-" version
                           ".tar.gz"))
       (sha256
        (base32
         "05rdj0nyirc4g1qi5hhrjy45y52ihp1j3ldq2c5bwrz0gzy4i3y8"))))
    (build-system gnu-build-system)
    (inputs `(("cairo" ,cairo)
              ("librsvg" ,librsvg)
              ("libxt" ,libxt)
              ("libxaw" ,libxaw)))
    (native-inputs `(("texinfo" ,texinfo)
                     ("pkg-config" ,pkg-config)))
    (home-page "http://www.gnu.org/software/xboard")
    (synopsis "Graphical user interface for chess programs")
    (description "GNU XBoard is a graphical board for all varieties of chess,
including international chess, xiangqi (Chinese chess), shogi (Japanese chess)
and Makruk.  Several lesser-known variants are also supported.  It presents a
fully interactive graphical interface and it can load and save games in the
Portable Game Notation.")
    (license license:gpl3+)))


(define-public xboing
  (package
    (name "xboing")
    (version "2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.techrescue.org/xboing/xboing"
                           version ".tar.gz"))
       (sha256
        (base32 "16m2si8wmshxpifk861vhpqviqxgcg8bxj6wfw8hpnm4r2w9q0b7"))))
    (arguments
     `(#:tests? #f
       #:phases
       (alist-replace
        'configure
        (lambda* (#:key outputs #:allow-other-keys)

          (substitute* "Imakefile"
            (("XPMINCLUDE[\t ]*= -I/usr/X11/include/X11")
             (string-append "XPMINCLUDE = -I" (assoc-ref %build-inputs "libxpm")
                            "/include/X11")))

          (substitute* "Imakefile"
            (("XBOING_DIR = \\.") "XBOING_DIR=$(PROJECTROOT)"))

          ;; FIXME: HIGH_SCORE_FILE should be set to somewhere writeable

          (zero? (system* "xmkmf" "-a"
                          (string-append "-DProjectRoot="
                                         (assoc-ref outputs "out")))))
        (alist-replace 'install
                       (lambda* (#:key outputs #:allow-other-keys)
                         (and
                          (zero? (system* "make" "install.man"))
                          (zero? (system* "make" "install"))))
                       %standard-phases))))
    (inputs `(("libx11" ,libx11)
              ("libxext" ,libxext)
              ("libxpm" ,libxpm)))
    (native-inputs `(("imake" ,imake)
                     ("inetutils" ,inetutils)
                     ("makedepend" ,makedepend)))
    (build-system gnu-build-system)
    (home-page "http://www.techrescue.org/xboing")
    (synopsis "Ball and paddle game")
    (description "XBoing is a blockout type game where you have a paddle which
you control to bounce a ball around the game zone destroying blocks with a
proton ball.  Each block carries a different point value.  The more blocks you
destroy, the better your score.  The person with the highest score wins.")
    (license (license:x11-style "file://COPYING"
                                "Very similar to the X11 licence."))))

(define-public gtypist
  (package
    (name "gtypist")
    (version "2.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gtypist/gtypist-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0xzrkkmj0b1dw3yr0m9hml2y634cc4h61im6zwcq57s7285z8fn1"))
              (modules '((guix build utils)))
              (snippet
               ;; We do not provide `ncurses.h' within an `ncursesw'
               ;; sub-directory, so patch the source accordingly.  See
               ;; <http://bugs.gnu.org/19018>.
               '(for-each (lambda (file)
                            (substitute* file
                              (("ncursesw/ncurses.h")
                               "ncurses.h")))
                          (find-files "." "configure$|\\.c$")))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("perl" ,perl)))
    (home-page "http://www.gnu.org/software/gtypist/")
    (synopsis "Typing tutor")
    (description
     "GNU Typist is a universal typing tutor.  It can be used to learn and
practice touch-typing.  Several tutorials are included; in addition to
tutorials for the standard QWERTY layout, there are also tutorials for the
alternative layouts Dvorak and Colemak, as well as for the numpad.  Tutorials
are primarily in English, however some in other languages are provided.")
    (license license:gpl3+)))

(define-public irrlicht
  (package
    (name "irrlicht")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/irrlicht/Irrlicht%20SDK/"
                    (version-major+minor version)
                    "/" version "/irrlicht-" version ".zip"))
              (sha256
               (base32
                "0yz9lvsc8aqk8wj4rnpanxrw90gqpwn9w5hxp94r8hnm2q0vjjw1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-after
                 'unpack 'fix-build-env
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "Makefile"
                       (("INSTALL_DIR = /usr/local/lib")
                        (string-append "INSTALL_DIR = " out "/lib")))
                     ;; The Makefile assumes these directories exist.
                     (mkdir-p (string-append out "/lib"))
                     (mkdir-p (string-append out "/include"))))
                 (alist-replace
                  'unpack
                  (lambda* (#:key source #:allow-other-keys)
                    (and (zero? (system* "unzip" source))
                         ;; The actual source is buried a few directories deep.
                         (chdir "irrlicht-1.8.1/source/Irrlicht/")))
                  (alist-cons-after
                   'unpack 'apply-patch/mesa-10-fix
                   (lambda* (#:key inputs #:allow-other-keys)
                     (zero? (system* "patch" "--force" "-p3" "-i"
                                     (assoc-ref inputs "patch/mesa-10-fix"))))
                   ;; No configure script
                   (alist-delete 'configure %standard-phases))))
       #:tests? #f ; no check target
       #:make-flags '("CC=gcc" "sharedlib")))
    (native-inputs
     `(("patch/mesa-10-fix" ,(search-patch "irrlicht-mesa-10.patch"))
       ("unzip" ,unzip)))
    (inputs
     `(("mesa" ,mesa)
       ("glu" ,glu)))
    (synopsis "3D game engine written in C++")
    (description
     "The Irrlicht Engine is a high performance realtime 3D engine written in
C++.  Features include an OpenGL renderer, extensible materials, scene graph
management, character animation, particle and other special effects, support
for common mesh file formats, and collision detection.")
    (home-page "http://irrlicht.sourceforge.net/")
    (license license:zlib)))

(define minetest-data
  (package
    (name "minetest-data")
    (version "0.4.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/minetest/minetest_game/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0642vy6r6sv96mz6wbs9qvyr95vd69zj4fxiljdg9801javrmm9p"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("source" ,source)
       ("tar" ,tar)
       ("gzip" ,(@ (gnu packages compression) gzip))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((tar (string-append (assoc-ref %build-inputs "tar")
                                             "/bin/tar"))
                         (install-dir (string-append
                                       %output
                                       "/share/minetest/games/minetest_game"))
                         (path (string-append (assoc-ref %build-inputs
                                                         "gzip")
                                              "/bin")))
                     (setenv "PATH" path)
                     (system* tar "xvf" (assoc-ref %build-inputs "source"))
                     (chdir (string-append "minetest_game-" ,version))
                     (mkdir-p install-dir)
                     (copy-recursively "." install-dir)))))
    (synopsis "Main game data for the Minetest game engine")
    (description
     "Game data for the Minetest infinite-world block sandox game.")
    (home-page "http://minetest.net")
    (license license:lgpl2.1+)))

(define-public minetest
  (package
    (name "minetest")
    (version "0.4.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/minetest/minetest/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pqp8hfwg5wkimig8j5jrihzgjjgplxr24w5xisrxvx1hlvnczdk"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
         (list "-DRUN_IN_PLACE=0"
               "-DENABLE_FREETYPE=1"
               "-DENABLE_GETTEXT=1"
               (string-append "-DIRRLICHT_INCLUDE_DIR="
                              (assoc-ref %build-inputs "irrlicht")
                              "/include/irrlicht")
               (string-append "-DCURL_INCLUDE_DIR="
                              (assoc-ref %build-inputs "curl")
                              "/include/curl"))
       #:tests? #f)) ; no check target
    (native-search-paths
     (list (search-path-specification
            (variable "MINETEST_SUBGAME_PATH")
            (files '("share/minetest/games")))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("irrlicht" ,irrlicht)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)
       ("libxxf86vm" ,libxxf86vm)
       ("mesa" ,mesa)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("openal" ,openal)
       ("freetype" ,(@ (gnu packages fontutils) freetype))
       ("curl" ,curl)
       ("luajit" ,luajit)
       ("gettext" ,gnu-gettext)
       ("sqlite" ,sqlite)))
    (propagated-inputs
     `(("minetest-data" ,minetest-data)))
    (synopsis "Infinite-world block sandbox game")
    (description
     "Minetest is a sandbox construction game.  Players can create and destroy
various types of blocks in a three-dimensional open world.  This allows
forming structures in every possible creation, on multiplayer servers or as a
single player.  Mods and texture packs allow players to personalize the game
in different ways.")
    (home-page "http://minetest.net")
    (license license:lgpl2.1+)))

(define glkterm
  (package
   (name "glkterm")
   (version "1.0.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://www.ifarchive.org/if-archive/programming/"
                         "glk/implementations/glkterm-104.tar.gz"))
     (sha256
      (base32
       "0zlj9nlnkdlvgbiliczinirqygiq8ikg5hzh5vgcmnpg9pvnwga7"))))
   (build-system gnu-build-system)
   (propagated-inputs `(("ncurses" ,ncurses))) ; required by Make.glkterm
   (arguments
    '(#:tests? #f ; no check target
      #:phases
      (alist-replace
       'install
       (lambda* (#:key outputs #:allow-other-keys)
         (let* ((out (assoc-ref outputs "out"))
                (inc (string-append out "/include")))
           (mkdir-p inc)
           (for-each
            (lambda (file)
              (copy-file file (string-append inc "/" file)))
            '("glk.h" "glkstart.h" "gi_blorb.h" "gi_dispa.h" "Make.glkterm"))
           (mkdir (string-append out "/lib"))
           (copy-file "libglkterm.a" (string-append out "/lib/libglkterm.a"))))
       (alist-delete 'configure %standard-phases))))
   (home-page "http://www.eblong.com/zarf/glk/")
   (synopsis "Curses Implementation of the Glk API")
   (description
    "Glk defines a portable API for applications with text UIs.  It was
primarily designed for interactive fiction, but it should be suitable for many
interactive text utilities, particularly those based on a command line.
This is an implementation of the Glk library which runs in a terminal window,
using the curses.h library for screen control.")
   (license (license:fsf-free "file://README"))))

(define-public glulxe
  (package
   (name "glulxe")
   (version "0.5.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://www.ifarchive.org/if-archive/programming/"
                         "glulx/interpreters/glulxe/glulxe-052.tar.gz"))
     (sha256
      (base32
       "19iw6kl8ncqcy9pv4gsqfh3xsa1n94zd234rqavvmxccnf3nj19g"))))
   (build-system gnu-build-system)
   (inputs `(("glk" ,glkterm)))
   (arguments
    '(#:tests? #f ; no check target
      #:make-flags
      (let* ((glk (assoc-ref %build-inputs "glk")))
        (list (string-append "GLKINCLUDEDIR=" glk "/include")
              (string-append "GLKLIBDIR=" glk "/lib")
              (string-append "GLKMAKEFILE=" "Make.glkterm")))
      #:phases
      (alist-replace
       'install
       (lambda* (#:key outputs #:allow-other-keys)
         (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
           (mkdir-p bin)
           (copy-file "glulxe" (string-append bin "/glulxe"))))
       (alist-delete 'configure %standard-phases))))
   (home-page "http://www.eblong.com/zarf/glulx/")
   (synopsis "Interpreter for Glulx VM")
   (description
    "Glulx is a 32-bit portable virtual machine intended for writing and
playing interactive fiction.  It was designed by Andrew Plotkin to relieve
some of the restrictions in the venerable Z-machine format.  This is the
reference interpreter, using Glk API.")
   (license (license:fsf-free "file://README"))))

(define-public retroarch
  (package
    (name "retroarch")
    (version "1.0.0.3-beta")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libretro/RetroArch/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1iqcrb076xiih20sk8n1w79xsp4fb8pj4vkmdc1xn562h56y4nxx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:phases
       (alist-replace
        'configure
        (lambda _
          (substitute* "qb/qb.libs.sh"
            (("/bin/true") (which "true")))
          (zero? (system*
                  "./configure"
                  (string-append "--prefix=" %output)
                  (string-append "--global-config-dir=" %output "/etc"))))
        %standard-phases)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ffmpeg" ,ffmpeg)
       ("freetype" ,freetype)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python)
       ("sdl" ,sdl2)
       ("udev" ,eudev)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (home-page "http://www.libretro.com/")
    (synopsis "Reference frontend for the libretro API")
    (description
     "Libretro is a simple but powerful development interface that allows for
the easy creation of emulators, games and multimedia applications that can plug
straight into any libretro-compatible frontend.  RetroArch is the official
reference frontend for the libretro API, currently used by most as a modular
multi-system game/emulator system.")
    (license license:gpl3+)))

(define-public gnugo
  (package
    (name "gnugo")
    (version "3.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gnugo/gnugo-" version
                                 ".tar.gz"))
             (sha256
              (base32
               "0wkahvqpzq6lzl5r49a4sd4p52frdmphnqsfdv7gdp24bykdfs6s"))))
    (build-system gnu-build-system)
    (inputs `(("readline" ,readline)))
    (synopsis "Play the game of Go")
    (description "GNU Go is a program that plays the game of Go, in which
players place stones on a grid to form territory or capture other stones.
While it can be played directly from the terminal, rendered in ASCII
characters, it is also possible to play GNU Go with 3rd party graphical
interfaces or even in Emacs.  It supports the standard game storage format
(SGF, Smart Game Format) and inter-process communication format (GMP, Go
Modem Protocol).")
    (home-page "http://www.gnu.org/software/gnugo/")
    (license license:gpl3+)))

(define-public extremetuxracer
  (package
    (name "extremetuxracer")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://downloads.sourceforge.net/project/extremetuxracer/releases/"
                    version "/etr-" version ".tar.xz"))
              (sha256
               (base32
                "0fl9pwkywqnsmgr6plfj9zb05xrdnl5xb2hcmbjk7ap9l4cjfca4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freetype" ,freetype)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("libice" ,libice)
       ("libpng" ,libpng)
       ("sdl" ,sdl)
       ("sdl-mixer" ,sdl-mixer)
       ("sdl-image" ,sdl-image)
       ("libsm" ,libsm)
       ("libunwind" ,libunwind)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxmu" ,libxmu)
       ("libxt" ,libxt)
       ("tcl" ,tcl)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-makefile
           (lambda _
             (substitute* "Makefile"
               (("CXXFLAGS =") "CXXFLAGS = ${CFLAGS}")))))))
    (synopsis "High speed arctic racing game based on Tux Racer")
    ;; Snarfed straight from Debian
    (description "Extreme Tux Racer, or etracer as it is called for short, is
a simple OpenGL racing game featuring Tux, the Linux mascot.  The goal of the
game is to slide down a snow- and ice-covered mountain as quickly as possible,
avoiding the trees and rocks that will slow you down.

Collect herrings and other goodies while sliding down the hill, but avoid fish
bones.

This game is based on the GPL version of the famous game TuxRacer.")
    (home-page "http://sourceforge.net/projects/extremetuxracer/")
    (license license:gpl2+)))
