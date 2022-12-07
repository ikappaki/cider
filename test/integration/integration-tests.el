;;; integration-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2022 Ioannis Kappas

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; Integration tests

;; This file is part of CIDER

;;; Code:

(require 'buttercup)
(require 'cider)
(require 'nrepl-dict)
(require 'nrepl-tests-utils "test/utils/nrepl-tests-utils")

;; https://emacs.stackexchange.com/a/55031
(defmacro with-temp-dir (temp-dir &rest body)
  "Create a temporary directory and bind it to TEMP-DIR while evaluating BODY.
Remove the temp directory at the end of evaluation."
  `(let ((,temp-dir (make-temp-file "" t)))
    (unwind-protect
      (progn
        ,@body)
      (condition-case err
          (delete-directory ,temp-dir t)
        (error
         (message ":with-temp-dir-error :cannot-remove-temp-dir %S" err))))))

(defun nrepl-client-cider-connected-hook-ref-add! ()
  "Return a gv ref to signal when the client is connected to the nREPL server.
This is done by adding a hook to `cider-connected-hook`.  Please remember to
restore the hook to its original value after your test finishes.

Use `gv-deref' (of which see) to deref the variable.

The generalized variable can take the following values

'!is-connected the client has not yet connected to the nREPL server.
'is-connected  the client has connected to the nREPL server."
  (let ((is-connected '!is-connected))
    (add-hook 'cider-connected-hook
              (lambda ()
                (setq is-connected 'is-connected)))
    (gv-ref is-connected)))


(describe "jack in"
  ;; See "babashka" case for commentary of the base template.
  ;;
  ;; It has been observed that some REPLs (Clojure cli, shadow) might take a
  ;; very long time to bring up/respond/shutdown, and thus sleep duration values
  ;; are set rather high.

  ;; Restore global state after each test invocation.
  ;;
  ;; The convention here is that variables starting with `-' are used to store
  ;; the original value of the global variable.
  :var (-cider-connected-hook)
  (before-each
   (setq -cider-connected-hook cider-connected-hook))
  (after-each
   (setq cider-connected-hook -cider-connected-hook))

  (it "to babashka"
      (with-temp-dir temp-dir
        ;; set up a project directory in temp
        (let* ((project-dir temp-dir)
               (bb-edn (expand-file-name "bb.edn" project-dir)))
          (write-region "{}" nil bb-edn)

          (with-temp-buffer
            ;; set default directory to temp project
            (setq-local default-directory project-dir)

            (unwind-protect
                ;; jack in and get repl buffer
                (let* ((client-is-connected* (nrepl-client-cider-connected-hook-ref-add!))
                       (nrepl-proc (cider-jack-in-clj '()))
                       (nrepl-buf (process-buffer nrepl-proc)))

                  ;; wait until the client has successfully connected to the
                  ;; nREPL server.
                  (nrepl-tests-sleep-until 5 (eq (gv-deref client-is-connected*)
                                                  'is-connected))
                  (expect (gv-deref client-is-connected*) :to-equal 'is-connected)

                  ;; give it some time to setup the clj REPL
                  (nrepl-tests-sleep-until 5 (cider-repls 'clj nil))

                  ;; send command to the REPL, and stdout/stderr to
                  ;; corresponding eval- variables.
                  (let ((repl-buffer (cider-current-repl))
                        (eval-err '())
                        (eval-out '()))
                    (expect repl-buffer :not :to-be nil)

                    ;; send command to the REPL
                    (cider-interactive-eval
                     ;; ask REPL to return a string that uniquely identifies it.
                     "(print :bb? (some? (System/getProperty \"babashka.version\")))"
                     (lambda (return)
                       (nrepl-dbind-response
                           return
                           (out err)
                         (when err (push err eval-err))
                         (when out (push out eval-out)))) )

                    ;; wait for the response to come back.
                    (nrepl-tests-sleep-until 5 eval-out)

                    ;; ensure there are no errors and response is as expected.
                    (expect eval-err :to-equal '())
                    (expect eval-out :to-equal '(":bb? true"))

                    ;; exit the REPL.
                    (cider-quit repl-buffer)

                    ;; wait for the REPL to exit
                    (nrepl-tests-sleep-until 5 (not (eq (process-status nrepl-proc) 'run)))
                    (expect (member (process-status nrepl-proc) '(exit signal)))))

              ;; useful for debugging on errors
              (when-let ((nrepl-error-buffer (get-buffer "*nrepl-error*")))
                (with-current-buffer nrepl-error-buffer
                  (message ":*nrepl-error* %S" (substring-no-properties (buffer-string))))))))))

  (it "to clojure tools cli"
      (with-temp-dir temp-dir
        (let* ((project-dir temp-dir)
               (deps-edn (expand-file-name "deps.edn" project-dir)))
          (write-region "{}" nil deps-edn)
          (with-temp-buffer
            (setq-local default-directory project-dir)
            (unwind-protect
                (let* ((client-is-connected* (nrepl-client-cider-connected-hook-ref-add!))
                       (nrepl-proc (cider-jack-in-clj `()))
                       (nrepl-buf (process-buffer nrepl-proc)))
                  ;; high duration since on windows it takes a long time to startup
                  (nrepl-tests-sleep-until 90 (eq (gv-deref client-is-connected*)
                                                  'is-connected))
                  (expect (gv-deref client-is-connected*) :to-equal 'is-connected)
                  (nrepl-tests-sleep-until 90 (cider-repls 'clj nil))
                  (let ((repl-buffer (cider-current-repl))
                        (eval-err '())
                        (eval-out '()))
                    (expect repl-buffer :not :to-be nil)
                    (cider-interactive-eval
                     "(print :clojure? (some? (clojure-version)))"
                     (lambda (return)
                       (nrepl-dbind-response
                           return
                           (out err)
                         (when err (push err eval-err))
                         (when out (push out eval-out)))) )
                    (nrepl-tests-sleep-until 10 eval-out)
                    (expect eval-err :to-equal '())
                    (expect eval-out :to-equal '(":clojure? true"))
                    (cider-quit repl-buffer)
                    (nrepl-tests-sleep-until 15 (not (eq (process-status nrepl-proc) 'run)))
                    (expect (member (process-status nrepl-proc) '(exit signal)))))
              (when-let ((nrepl-error-buffer (get-buffer "*nrepl-error*")))
                (with-current-buffer nrepl-error-buffer
                  (message ":*nrepl-error* %S" (substring-no-properties (buffer-string))))))))))

  (it "to leiningen"
      (with-temp-dir temp-dir
        (let* ((project-dir temp-dir)
               (project-clj (expand-file-name "project.clj" project-dir)))
          (write-region "(defproject cider/integration \"test\"
                           :dependencies [[org.clojure/clojure \"1.10.3\"]])"
                        nil project-clj)
          (with-temp-buffer
            (setq-local default-directory project-dir)
            (unwind-protect
                (let* ((client-is-connected* (nrepl-client-cider-connected-hook-ref-add!))
                       (nrepl-proc (cider-jack-in-clj `()))
                       (nrepl-buf (process-buffer nrepl-proc)))
                  (nrepl-tests-sleep-until 90 (eq (gv-deref client-is-connected*)
                                                  'is-connected))
                  (expect (gv-deref client-is-connected*) :to-equal 'is-connected)
                  (nrepl-tests-sleep-until 90 (cider-repls 'clj nil))
                  (let ((repl-buffer (cider-current-repl))
                        (eval-err '())
                        (eval-out '()))
                    (expect repl-buffer :not :to-be nil)
                    (cider-interactive-eval
                     "(print :clojure? (some? (clojure-version)))"
                     (lambda (return)
                       (nrepl-dbind-response
                           return
                           (out err)
                         (when err (push err eval-err))
                         (when out (push out eval-out)))) )
                    (nrepl-tests-sleep-until 10 eval-out)
                    (expect eval-err :to-equal '())
                    (expect eval-out :to-equal '(":clojure? true"))
                    (cider-quit repl-buffer)
                    (nrepl-tests-sleep-until 15 (not (eq (process-status nrepl-proc) 'run)))
                    (expect (member (process-status nrepl-proc) '(exit signal)))
                    (sleep-for 0.5)))
              (when-let ((nrepl-error-buffer (get-buffer "*nrepl-error*")))
                (with-current-buffer nrepl-error-buffer
                  (message ":*nrepl-error* %S"
                           (substring-no-properties (buffer-string))))))))))

  (it "to shadow"
      ;; shadow asks user whether they want to open a browser, force to no
      (spy-on 'y-or-n-p)

      (with-temp-dir temp-dir
        (let* ((project-dir temp-dir)
               (shadow-cljs-edn (expand-file-name "shadow-cljs.edn" project-dir))
               (package-json    (expand-file-name "package.json"    project-dir)))
          (write-region "{}" nil shadow-cljs-edn)
          (write-region "{\"dependencies\":{\"shadow-cljs\": \"^2.20.13\"}}" nil package-json)
          (let ((default-directory project-dir))
            (message ":npm-install...")
            (shell-command "npm install")
            (message ":npm-install :done"))
          (let ((cider-preferred-build-tool 'shadow-cljs)
                ;; request for a node repl, so that shadow forks one.
                (cider-shadow-default-options ":node-repl"))
            (with-temp-buffer
              (setq-local default-directory project-dir)
              (unwind-protect
                  (let* ((client-is-connected* (nrepl-client-cider-connected-hook-ref-add!))
                         (nrepl-proc (cider-jack-in-cljs '(:cljs-repl-type shadow)))
                         (nrepl-buf (process-buffer nrepl-proc)))
                    (nrepl-tests-sleep-until 120 (eq (gv-deref client-is-connected*)
                                                    'is-connected))
                    (expect (gv-deref client-is-connected*) :to-equal 'is-connected)
                    (nrepl-tests-sleep-until 120 (cider-repls 'cljs nil))
                    (expect (cider-repls 'cljs nil) :not :to-be nil)
                    (let ((repl-buffer (cider-current-repl))
                          (eval-err '())
                          (eval-out '()))
                      (expect repl-buffer :not :to-be nil)
                      (sleep-for 2)
                      (cider-interactive-eval
                       "(print :cljs? (some? *clojurescript-version*))"
                       (lambda (return)
                         (nrepl-dbind-response
                             return
                             (out err)
                           (when err (push err eval-err))
                           (when out (push out eval-out)))) )
                      (nrepl-tests-sleep-until 10 eval-out)
                      (expect eval-err :to-equal '())
                      (expect eval-out :to-equal '(":cljs? true\n"))
                      (cider-quit repl-buffer)
                      (nrepl-tests-sleep-until 15 (not (eq (process-status nrepl-proc) 'run)))
                      (expect (member (process-status nrepl-proc) '(exit signal)))))
                (when-let ((nrepl-error-buffer (get-buffer "*nrepl-error*")))
                  (with-current-buffer nrepl-error-buffer
                    (message ":*nrepl-error* %S"
                             (substring-no-properties (buffer-string))))))))))))

(provide 'integration-tests)

;;; integration-tests.el ends here

