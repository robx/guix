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

(define-module (gnu services scron)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:autoload   (gnu packages suckless) (scron)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (scron-configuration
            scron-configuration?
            scron-configuration-scron
            scron-configuration-jobs

            scron-job
            scron-job?
            scron-job-schedule
            scron-job-command

            scron-service-type
            scron-service))

;;; Commentary:
;;;
;;; This module implements a service to run instances of scron, a
;;; periodic job execution daemon.  Example of a service:
;;
;;  (service scron-service-type
;;           (scron-configuration
;;            (jobs (list (scron-job (schedule "*/15 * * * *")
;;                                   (command  "echo hello!"))))))
;;;
;;; Code:

(define-record-type* <scron-configuration> scron-configuration
  make-scron-configuration
  scron-configuration?
  (scron             scron-configuration-scron    ;package
                     (default scron))
  (jobs              scron-configuration-jobs     ;list of <scron-job>
                     (default '())))

(define-record-type* <scron-job> scron-job
  make-scron-job
  scron-job?
  (schedule scron-job-schedule)
  (command  scron-job-command))

(define (crontab jobs)
  (apply mixed-text-file "crontab"
    (concatenate
      (map
        (match-lambda
          (($ <scron-job> schedule command)
            (list schedule " " command "\n")))
        jobs))))

(define scron-shepherd-services
  (match-lambda
    (($ <scron-configuration> scron jobs)
     (list
       (shepherd-service
        (provision '(scron))
        (requirement '(user-processes))
        (start #~(make-forkexec-constructor
                  (list (string-append #$scron "/bin/crond")
                        "-n" ; don't fork
                        "-f" #$(crontab jobs))
                  #:log-file "/var/log/scron.log"))
        (stop #~(make-kill-destructor)))))))

(define scron-service-type
  (service-type (name 'scron)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          scron-shepherd-services)))
                (compose concatenate)
                (extend (lambda (config jobs)
                          (scron-configuration
                           (inherit config)
                           (jobs (append (scron-configuration-jobs config)
                                         jobs)))))
                (default-value (scron-configuration))))
