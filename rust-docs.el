;;; rust-docs --- Search and read docs.rs from GNU Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Parses HTML pages from docs.rs.

;;; Code:
(require 'dom)
(require 'cl-lib)

(cl-defstruct
 (rust-docs--entry (:constructor rust-docs--make-entry)) name href)

(defconst rust-docs--doc-entry-type '("mod" "struct" "trait" "macro")
  "All possible types of doc entries.")


(defun rust-docs--search-crate (crate-name version)
  "Searches for the CRATE-NAME with VERSION on the docs.rs."
  (with-current-buffer (url-retrieve-synchronously
                        (url-encode-url
                         (format "https://docs.rs/%s/%s/%s"
                                 crate-name version crate-name)))
    (let* ((dom (libxml-parse-html-region))
           result)
      (dolist (entry-type rust-docs--doc-entry-type)
        (dolist (entry-node
                 (rust-docs--search-nodes-by-entry-type
                  dom entry-type))
          (push (rust-docs--entry-from-node entry-node) result)))
      result)))

(defun rust-docs-search-entry (name &optional version href)
  "Returns details of crate with NAME and VERSION for entry with HREF."
  (with-current-buffer (url-retrieve-synchronously
                        (url-encode-url
                         (format "https://docs.rs/%s/%s/%s/%s"
                                 name version name href)))
    (let ((dom (libxml-parse-html-region)))
      (dom-by-id dom "main-content"))))

(defun rust-docs--search-nodes-by-entry-type (dom entry-type)
  "Returns all nodes from DOM which have ENTRY-TYPE."
  (dom-search
   dom
   (lambda (node)
     (rust-docs--filter node (format "^%s$" entry-type)))))

(defun rust-docs--filter (node class)
  "Fitlers NODE by CLASS."
  (and (dom-by-tag node 'a)
       (dom-by-class node class)
       (dom-attr node 'title)))

(defun rust-docs--entry-from-node (node)
  "Creates `rust-docs--entry' from dom NODE."
  (rust-docs--make-entry
   :name (dom-attr node 'title)
   :href (dom-attr node 'href)))

(defun rust-docs--dom-to-org (node)
  "Reqursively converts NODE to org."
  (cond
   ((stringp node)
    nil)
   ((eq (dom-tag node) 'h1)
    (rust-docs--h1-to-org node))
   ((eq (dom-tag node) 'h2)
    (insert "\n")
    (rust-docs--h2-to-org node))
   ((eq (dom-tag node) 'code)
    (rust-docs--code-to-org node))
   ((eq (dom-tag node) 'p)
    (rust-docs--p-to-org node))
   (t
    (mapc #'rust-docs--dom-to-org (dom-children node)))))

(defun rust-docs--h1-to-org (node)
  "Converts h1 NODE to org."
  (insert
   "* " (dom-text node) "~" (dom-text (dom-by-tag node 'span)) "~\n"))

(defun rust-docs--h2-to-org (node)
  "Converts h2 NODE to org."
  (insert "** " (dom-texts node) "\n"))

(defun rust-docs--code-to-org (node)
  "Converts code NODE to org."
  (insert "#+begin_src rust\n" (dom-texts node) "\n" "#+end_src\n"))

(defun rust-docs--p-to-org (node)
  "Converts paragraph NODE to org."
  (insert (dom-texts node) "\n"))

(defun rust-docs--parse-cargo-toml (path)
  "Parses dependencies from Cargo.toml under PATH.
Returns alist of (dependency-name . version)"
  (with-temp-buffer
    (insert-file-contents path)
    (let ((begin (re-search-forward "^\\[dependencies\\]$"))
          (end
           (or (re-search-forward "^\\[.*\\]$" nil t 1) (point-max))))
      (goto-char begin)
      (while (and
              (< (point) end)
              (re-search-forward
               "^\\([a-z-_0-9]+\\)\\ +?=\\(.*\"\\([0-9.]+\\)\"\\)?.*$"
               nil t))
        (replace-match "(\"\\1\" . \"\\3\")" nil nil))
      (eval
       (car
        (read-from-string
         (format "'(%s)" (buffer-substring begin (point)))))))))

(defun rust-docs--find-all-cargo-files ()
  "Searches for the Cargo.toml files."
  (split-string
   (shell-command-to-string
    (format "find %s -name Cargo.toml -print" default-directory))))

(defun rust-docs--collect-dependencies ()
  "Collects dependencies from the project."
  (let (result)
    (dolist (path (rust-docs--find-all-cargo-files))
      (dolist (dep (rust-docs--parse-cargo-toml path))
        (unless (alist-get (car dep) result)
          (push dep result))))
    result))

;;; Choose document entry
;; (completing-read
;;  "Entry: "
;;  (mapcar #'rust-docs--entry-name (rust-docs--search-crate "tower" "latest")))

;;; Get dom for the entry
;; (with-current-buffer (get-buffer-create "*org-docs.rs*")
;;   (erase-buffer)
;;   (org-mode)
;;   (let ((dom
;;          (rust-docs-search-entry "tower"
;;                                  "latest"
;;                                  "trait.Layer.html")))
;;     (rust-docs--dom-to-org dom)))

(message "%s"
         (rust-docs--parse-cargo-toml
          (expand-file-name "~/code/personal/wingdb/Cargo.toml")))


(provide 'rust-docs)
;;; rust-docs.el ends here
