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

(define-module (guix build elm-build-system)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ftw) ; scandir
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-26)
  #:use-module (guix build json)
  #:use-module (guix build utils)

  #:export(build-versions.dat
           elm-package-and-version
           elm-package-build
           elm-application-build))

(define (first-subdirectory dir)
  "Return the path of the first sub-directory of DIR."
  (file-system-fold (lambda (path stat result)
                      (string=? path dir))
                    (lambda (path stat result) result) ; leaf
                    (lambda (path stat result) result) ; down
                    (lambda (path stat result) result) ; up
                    (lambda (path stat result)         ; skip
                      (or result path))
                    (lambda (path stat errno result)   ; error
                      (error "first-subdirectory" (strerror errno)))
                    #f
                    dir))

(define* (unpack #:key source #:allow-other-keys)
  "Unpack SOURCE in the working directory, and change directory within the
source.  When SOURCE is a directory, copy it in a sub-directory of the current
working directory."
  (if (file-is-directory? source)
      (begin
        (mkdir "source")
        (chdir "source")

        ;; Preserve timestamps (set to the Epoch) on the copied tree so that
        ;; things work deterministically.
        (copy-recursively source "."
                          #:keep-mtime? #t))
      (begin
        (if (string-suffix? ".zip" source)
            (invoke "unzip" source)
            (invoke "tar" "xvf" source))
        (chdir (first-subdirectory "."))))
  #t)

(define* (put-packages port pkgs)
  "Writes an elm package database to PORT based on the
list PKGS of packages. Each package should be a list
of the form '(\"elm\" \"core\" (1 0 0))."

  (define (put-int64 port x)
    (let ((vec (make-bytevector 8)))
      (bytevector-s64-set! vec 0 x (endianness big))
      (put-bytevector port vec)))

  (define (put-text port s)
    (put-int64 port (string-length s)) ; this should be utf8 length, utf8
    (put-string port s))

  (define (put-version port v)
    (map (cut put-u8 port <>) v)) ; there's a different encoding for very large versions

  (define (put-package port pkg)
    (match pkg
      ((author project version)
       (begin
         (put-text port author)
         (put-text port project)
         (put-int64 port 1)
         (put-version port version)))))

  (put-int64 port (length pkgs)) ; total number of versions
  (put-int64 port (length pkgs)) ; number of packages
  (for-each
   (cut put-package port <>) 
   (sort pkgs (match-lambda*
                (((auth1 proj1 _) (auth2 proj2 _))
                 (or (string<? auth1 auth2)
                     (and (string=? auth1 auth2)
                          (string<? proj1 proj2))))))))

(define* (parse-version version)
  "Parse an elm package version from string to a list of
integer components."
  (map string->number (string-split version #\.)))

(define* (build-versions.dat)
  "Build an elm package database in the file versions.dat
in the current directory to match the existing unpacked elm
modules."
  (format #t "building versions.dat in ~a~%" (getcwd))
  (let ((packages (string-split
                   (get-line (open-input-pipe "echo */*/*"))
                   #\ ))
        (out (open-output-file "versions.dat")))
    (format #t "packages: ~a~%" packages)
    (put-packages
     out
     (map
      (lambda (path)
        (match (string-split path #\/)
          ((author project version)
           (list author project (parse-version version)))))
      packages))
    (close out)))

(define* (elm-package-and-version pkg)
  "Read the package name and version from an elm package."
  (catch 'system-error
    (lambda _
      (let* ((filename (string-append pkg "/elm.json"))
             (json (read-json (open-input-file filename)))
             (type (assoc-ref json "type")))
        (when
          (string=? type "package")
          `(,(assoc-ref json "name") . ,(assoc-ref json "version")))))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          #f
          (apply throw args)))))

(define (elm-package-build source inputs output)
  (let ((tar (assoc-ref inputs "tar"))
        (gzip (assoc-ref inputs "gzip")))
    (setenv "PATH" (string-append (getenv "PATH") ":"
                                  (string-append tar "/bin") ":"
                                  (string-append gzip "/bin")))
    (mkdir "source")
    (with-directory-excursion "source"
      (invoke "tar" "xzf" source)
      (let ((dir (car (scandir "." (lambda (f) (> (string-length f) 2))))))
        (copy-recursively dir output)))))

(define (elm-application-build source inputs output elm-modules)
  (let ((elm (string-append (assoc-ref inputs "elm-compiler") "/bin/elm"))
        (deps ".elm/0.19.0/package")
        (tar (assoc-ref inputs "tar"))
        (xz (assoc-ref inputs "xz")))
    (setenv "PATH" (string-append (getenv "PATH") ":"
                                  (string-append tar "/bin") ":"
                                  (string-append xz "/bin")))
    (format #t "collecting dependencies~%")
    (mkdir-p deps)
    (for-each
      (match-lambda
        ((n . pkg)
         (match (elm-package-and-version pkg)
           ((name . version)
            (begin (format #t "  ~a: ~a/~a" n name version)
             (copy-recursively pkg
                               (string-append deps "/" name "/" version))))
           (_ #f))))
      inputs)
    (format #t "generating versions.dat~%")
    (with-directory-excursion deps (build-versions.dat))
    (format #t "setting up elm env: cwd=~a~%" (getcwd))
    (setenv "HOME" (getcwd))
    (setenv "HTTP_PROXY" ".")
    (mkdir "src") ; extra level of src directory because unpack uses the first subdir...
    (with-directory-excursion "src"
      (format #t "extracting source~%")
      (unpack #:source (assoc-ref inputs "source")) ; changes directory
      (mkdir "elm-stuff")
      (chmod "elm-stuff" #o775)
      (for-each
        (match-lambda
          ((srcs . dst)
           (begin
             (format #t "building ~a from ~a~%" dst srcs)
             (apply invoke elm "make" (string-append "--output=" output "/" dst)
                    (map (lambda (src) (string-append "src/" src)) srcs)))))
        elm-modules))))
