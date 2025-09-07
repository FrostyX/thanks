;;; thanks.el --- Say thanks to the authors of all your installed packages   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Jakub Kadlčík

;; Author: Jakub Kadlčík <frostyx@email.cz>
;; URL: https://github.com/FrostyX/thanks
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (gh "1.0.1"))
;; Keywords: tools

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows you to give a GitHub stars to all your installed
;; packages.  More info in the project README


;;; Code:

;;;; Requirements

(require 'gh)
(require 'package)

;;;; Customization

(defgroup thanks nil
  "Thanks package."
  :prefix "thanks-"
  :group 'thanks)

(defcustom thanks-excluded-packages
  '()
  "A list of package names that are not to be starred."
  :type '(repeat string)
  :group 'thanks)

;;;; Modes

;;;###autoload
(define-minor-mode thanks-mode
  "Automatically give thanks after installing a package."
  :global t
  :group 'thanks
  (if thanks-mode
      (advice-add 'package-install :after #'thanks--after-install)
    (advice-remove 'package-install #'thanks--after-install)))

;;;; Variables

;;;; Commands

;;;###autoload
(defun thanks ()
  "Say thanks to the authors of all your installed packages."
  (interactive)
  (let* ((packages (thanks--installed-packages))
         (packages (seq-filter
                    (lambda (p) (not (member p thanks-excluded-packages)))
                    packages))
         (urls (mapcar #'thanks--package-homepage packages))
         (urls (seq-filter #'thanks--github-project-url? urls)))
    (when (y-or-n-p
           (format "Do you want to say thanks to %d projects? " (length urls)))
      (seq-do-indexed
       (lambda (url i)
         (let ((i (1+ i)))
           (when (= (mod i 30) 0)
             (message "[%d/%d] Waiting a minute to not exceed GitHub rate limits"
                      i (length urls))
             (sit-for 60))
           (message "[%d/%d] Saying thanks to %s" i (length urls) url)
           (when (thanks--github-project-exists? url)
             (unless (thanks--github-star-url url)
               (error "Failed to star a GitHub project %s" url)))))
       urls)
      (message "Said thanks to %s projects! You just made somebody's day better."
               (length urls)))))

;;;###autoload
(defun thanks-test-github-auth ()
  "A best effort read-only test whether GitHub authentication works."
  (interactive)
  (let* ((api (gh-repos-api))
         (response (gh-repos-user-list api))
         (status (slot-value response 'http-status)))
    (if (and (>= status 200) (< status 300))
        (message "GitHub authentication OK.")
      (message
       (concat "GitHub authentication failed. "
               "See the troubleshooting section in the project README")))))

;;;; Functions

;;;;; Public

;;;;; Private

(defun thanks--installed-packages ()
  "Get names of all installed packages."
  (mapcar
   (lambda (package)
     (symbol-name (car package)))
   package-alist))

(defun thanks--package-homepage (name)
  "Find a package by its NAME and return its homepage URL."
  (let* ((symbol (intern name))
         (desc (cadr (assoc symbol package-alist)))
         (extras (package-desc-extras desc)))
    (cdr (assoc :url extras))))

(defun thanks--github-project-url? (url)
  "Is this URL a github project?"
  (let ((prefix "https://github.com/"))
    (and (string-prefix-p prefix url)
         (let* ((path (string-remove-prefix prefix url))
                (path (string-remove-suffix "/" path))
                (args (split-string path "/")))
           (= (length args) 2)))))

(defun thanks--github-project-exists? (url)
  "Does this a GitHub project with this URL exist?
The `gh.el' library cannot handle renamed projects (at least when giving stars),
and bombs out.  See https://github.com/sigma/gh.el/issues/106

For this reason, we need to always make sure that a project exists before
modifying it in any way."
  (let* ((inhibit-message t)
         (owner-project (thanks--github-url-to-owner-project url))
         (owner (car owner-project))
         (project (cdr owner-project))
         (repo (gh-repos-repo-get (gh-repos-api) project owner)))
    (equal url (slot-value (slot-value repo 'data) 'html-url))))

(defun thanks--github-star (owner project)
  "Give a star to a GitHub PROJECT by this OWNER."
  (let* ((inhibit-message t)
         (api (gh-repos-api))
         (owner (gh-user :login owner))
         (repo (gh-repos-repo :name project :owner owner))
         (response (gh-repos-star api repo))
         (status (slot-value response 'http-status)))
    (and (>= status 200) (< status 300))))

(defun thanks--github-star-url (url)
  "Give a star to a GitHub project identified by its URL."
  (let* ((args (split-string
                (string-remove-prefix "https://github.com/" url)
                "/"))
         (owner (car args))
         (project (cadr args)))
    (thanks--github-star owner project)))

(defun thanks--github-url-to-owner-project (url)
  "Parse `(owner . project)' pair from a GitHub project URL."
  (let* ((args (split-string
                (string-remove-prefix "https://github.com/" url)
                "/"))
         (owner (car args))
         (project (cadr args)))
    (cons owner project)))

(defun thanks--after-install (package)
  "Call this after installing a PACKAGE to give thanks."
  (let* ((name (if (package-desc-p package)
                   (package-desc-name package)
                 package))
         (name (symbol-name name))
         (url (thanks--package-homepage name)))
    (when (thanks--github-project-url? url)
      ;; Since this is a non-critical, more like for-fun package, I think we
      ;; should not error-out but rather only show a message if something went
      ;; wrong. We don't want to bother users too much.
      (unless (thanks--github-star-url url)
        (message "Failed to star a GitHub project %s" url)))))

;;;; Footer

(provide 'thanks)

;;; thanks.el ends here
