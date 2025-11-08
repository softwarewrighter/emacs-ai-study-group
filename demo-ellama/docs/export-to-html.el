;;; export-to-html.el --- Export ellama demo to HTML -*- lexical-binding: t; -*-

;; This script exports demo.org to HTML

;;; Commentary:

;; Usage:
;;   emacs --batch -l export-to-html.el
;; Or interactively:
;;   M-x load-file RET export-to-html.el RET
;;   M-x ellama-demo-export-html RET

;;; Code:

(require 'ox-html)

(defun ellama-demo-export-html (&optional file)
  "Export demo.org to HTML.
If FILE is provided, export that file instead."
  (interactive)
  (let* ((org-file (or file
                      (expand-file-name "demo.org"
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))
         (html-file (concat (file-name-sans-extension org-file) ".html")))

    (unless (file-exists-p org-file)
      (error "Org file not found: %s" org-file))

    (message "Exporting %s to HTML..." org-file)

    (with-current-buffer (find-file-noselect org-file)
      ;; Configure export settings
      (let ((org-html-htmlize-output-type 'css)
            (org-html-html5-fancy t)
            (org-html-doctype "html5")
            (org-export-with-toc t)
            (org-export-with-section-numbers t)
            (org-html-postamble t)
            (org-confirm-babel-evaluate nil)
            (org-export-use-babel nil)  ; Don't evaluate blocks during export
            (org-html-postamble-format
             '(("en" "<p class=\"author\">Author: %a</p>
<p class=\"date\">Created: %d</p>
<p class=\"creator\">%c</p>
<p>Part of the <a href=\"../../README.html\">Emacs AI Study Group</a></p>"))))

        ;; Export to HTML
        (org-html-export-to-html)
        (message "✓ Exported to: %s" html-file)
        html-file))))

(defun ellama-demo-export-and-open ()
  "Export demo.org to HTML and open in browser."
  (interactive)
  (let ((html-file (ellama-demo-export-html)))
    (when (file-exists-p html-file)
      (browse-url-of-file html-file)
      (message "✓ Opened in browser: %s" html-file))))

;; If running in batch mode, export automatically
(when noninteractive
  (ellama-demo-export-html))

(provide 'export-to-html)
;;; export-to-html.el ends here
