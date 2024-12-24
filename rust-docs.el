;;; rust-docs --- Search and read docs.rs from GNU Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Parses HTML pages from docs.rs.

;;; Code:
(require 'dom)
(require 'cl-lib)
(require 'project)

; begin-region -- Domain

(cl-defstruct
 (rust-docs--entry (:constructor rust-docs--make-entry)) name href)

(defconst rust-docs--doc-entry-type '("mod" "struct" "trait" "macro")
  "All possible types of doc entries.")

(define-derived-mode
 rust-docs-mode
 org-mode
 "Rust-Docs"
 "Special org mode for the Rust docs."
 (keymap-local-set "q" #'quit-window))

; end-region   -- Domain

; begin-region -- Custom configuration

(defgroup rust-docs 'nil
  "Customization for Rust docs."
  :group 'frames)

(defcustom rust-docs-debug nil
  "Show debug messages."
  :type 'boolean
  :group 'rust-docs
  :options '(t nil))

(defcustom rust-docs-resource 'web
  "Resource to search for the documentation from."
  :type 'string
  :group 'rust-docs
  :options '(web local))

; end-region   -- Custom configuration

; begin-region -- Public API

(defun rust-docs-open ()
  "Show rust docs."
  (interactive)
  (let* ((dependencies (rust-docs--collect-dependencies))
         (dependency (completing-read "Crate: " dependencies))
         (version
          (alist-get dependency dependencies "latest" nil #'string=))
         (entries (rust-docs--search-crate dependency version))
         (entry-name
          (completing-read
           "Entry: " (mapcar #'rust-docs--entry-name entries)))
         (entry
          (seq-find
           (lambda (el)
             (string= entry-name (rust-docs--entry-name el)))
           entries))
         (dom
          (rust-docs--search-entry
           dependency version (rust-docs--entry-href entry))))
    (rust-docs--open dom dependency version)))

; end-region   -- Public API

; begin-region -- Reading docs

(defun rust-docs--search-crate (name version)
  "Searches for the crate NAME with VERSION on the docs.rs."
  (with-current-buffer (rust-docs--read-crate-contents name version)
    (let* ((dom (libxml-parse-html-region))
           result)
      (dolist (entry-type rust-docs--doc-entry-type)
        (dolist (entry-node
                 (rust-docs--search-nodes-by-entry-type
                  dom entry-type))
          (push (rust-docs--entry-from-node entry-node) result)))
      (rust-docs--debug "Found %s entries for %s@%s"
                        (length result)
                        name
                        version)
      result)))

(defun rust-docs--search-entry (name version href)
  "Returns details of crate with NAME and VERSION for entry with HREF."
  (with-current-buffer (rust-docs--read-crate-entry-content
                        name version href)
    (let ((dom (libxml-parse-html-region)))
      (dom-by-id dom "main-content"))))

(defun rust-docs--open (dom crate-name crate-version)
  "Creates or reuses docs buffer, parsed DOM and inserts result.
CRATE-NAME and CRATE-VERSION required for the links generation."
  (with-current-buffer (get-buffer-create "*docs.rs*")
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (rust-docs--dom-to-org dom crate-name crate-version)
    (goto-char 1)
    (rust-docs-mode)
    (setq-local buffer-read-only t)
    (setq-local org-link-elisp-confirm-function nil)
    (pop-to-buffer (current-buffer))))

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

(defun rust-docs--read-crate-contents (name version)
  "Reads contents of the crate NAME with VERSION."
  (cond
   ((eq rust-docs-resource 'local)
    (error "Local not supported yet"))
   ((eq rust-docs-resource 'web)
    (url-retrieve-synchronously (rust-docs--web-url name version)))))

(defun rust-docs--read-crate-entry-content (name version href)
  "Reads content of the HREF of crate NAME with VERSION."
  (rust-docs--debug "Reading content for the %s@%s with href=%s"
                    name
                    version
                    href)
  (cond
   ((eq rust-docs-resource 'local)
    (error "Local not supported yet"))
   ((eq rust-docs-resource 'web)
    (url-retrieve-synchronously
     (rust-docs--web-url name version href)))
   (t
    (error "Unknown rust-docs-resource %s" rust-docs-resource))))

; end-region   -- Reading docs

; begin-region -- docs.rs utils

(defun rust-docs--web-url (name version &optional href)
  "Creates url to docs.rs page.
Accepts crate NAME and it's VERSION.
HREF is optional and appended to the end."
  (url-encode-url
   (if (string= name "std")
       (format "https://doc.rust-lang.org/%s/%s%s"
               version name
               (if href
                   (format "/%s" href)
                 ""))
     (format "https://docs.rs/%s/%s/%s%s"
             name version name
             (if href
                 (format "/%s" href)
               "")))))

; end-region   -- docs.rs utils

; begin-region -- HTML DOM to Org

(defun rust-docs--dom-to-org
    (node crate-name crate-version &optional insert-text insert-links)
  "Reqursively converts NODE to org.
CRATE-NAME and CRATE-VERSION required for links generation.
Inserts string nodes if INSERT-TEXT
Inserts links as org links if INSERT-LINKS"
  (rust-docs--debug
   "Converting node=%s from crate=[%s@%s] insert-text=%s insert-links=%s"
   node crate-name crate-version insert-text insert-links)
  (cond
   ((stringp node)
    (and insert-text (insert node)))
   ((eq (dom-tag node) 'h1)
    (rust-docs--h1-to-org node))
   ((eq (dom-tag node) 'h2)
    (rust-docs--h2-to-org node))
   ((eq (dom-tag node) 'h4)
    (rust-docs--h4-to-org node))
   ((eq (dom-tag node) 'h5)
    (rust-docs--h5-to-org node))
   ((eq (dom-tag node) 'code)
    (rust-docs--code-to-org node))
   ((eq (dom-tag node) 'p)
    (rust-docs--p-to-org node crate-name crate-version))
   ((eq (dom-tag node) 'li)
    (rust-docs--li-to-org node crate-name crate-version))
   ((and (eq (dom-tag node) 'div)
         (dom-by-class node "docblock-short"))
    (rust-docs--docblock-short-to-org node))
   ((and insert-links (eq (dom-tag node) 'a))
    (rust-docs--a-to-org node crate-name crate-version))
   ((eq (dom-tag node) 'button)
    nil)
   (t
    (dolist (child (dom-children node))
      (rust-docs--dom-to-org child crate-name crate-version
                             insert-text
                             insert-links)))))

(defun rust-docs--h1-to-org (node)
  "Converts h1 NODE to org."
  (insert "* ")
  (dolist (child (dom-children node))
    (rust-docs--dom-to-org child nil nil t t)) ;; There is no links in h1
  (insert "\n"))

(defun rust-docs--h2-to-org (node)
  "Converts h2 NODE to org."
  (insert "** " (dom-texts node "") "\n"))

(defun rust-docs--h4-to-org (node)
  "Converts h4 NODE to org."
  (insert "*** " (dom-texts node "") "\n"))

(defun rust-docs--h5-to-org (node)
  "Converts h4 NODE to org."
  (insert "**** " (dom-texts node "") "\n"))

(defun rust-docs--code-to-org (node)
  "Converts code NODE to org."
  (let* ((children (dom-children node))
         (org
          (if (eq (length children) 1)
              `("=" ,(dom-text node) "=")
            `("#+begin_src rust\n"
              ,(dom-texts node "")
              "\n"
              "#+end_src\n\n"))))
    (rust-docs--debug "Code node=%s children-count=%s"
                      node
                      (length (dom-children node)))
    (apply #'insert org)))

(defun rust-docs--p-to-org (node crate-name crate-version)
  "Converts paragraph NODE to org.
There could be links in paragraph so CRATE-NAME and CRATE-VERSION required."
  (dolist (child (dom-children node))
    (rust-docs--dom-to-org child crate-name crate-version t t))
  (insert "\n\n"))

(defun rust-docs--li-to-org (node crate-name crate-version)
  "Converts li NODE to org.
There could be links in list so CRATE-NAME and CRATE-VERSION required."
  (insert "- ")
  (dolist (child (dom-children node))
    (rust-docs--dom-to-org child crate-name crate-version t t))
  (insert "\n"))

(defun rust-docs--a-to-org (node crate-name crate-version)
  "Converts a NODE to org.
CRATE-NAME and CRATE-VERSION describe current crate."
  (insert
   "[[elisp:"
   (format "%s"
           `(rust-docs--open
             (rust-docs-search-entry
              ,(format "\"%s\"" crate-name)
              ,(format "\"%s\"" crate-version)
              ,(format "\"%s\"" (dom-attr node 'href)))
             ,(format "\"%s\"" crate-name)
             ,(format "\"%s\"" crate-version))) ;; TODO: Rewrite these naive formats
   "][" (dom-texts node "") "]]"))

(defun rust-docs--docblock-short-to-org (node)
  "Converts a div NODE to org."
  (insert " " (dom-texts node "")))

; end-region   -- HTML DOM to Org

; begin-region -- Cargo.toml parsing

(defun rust-docs--collect-dependencies ()
  "Collects dependencies from the project."
  (let ((result (list (rust-docs--get-rust-dep))))
    (dolist (path (rust-docs--find-all-cargo-files))
      (dolist (dep (rust-docs--parse-cargo-toml path))
        (unless (alist-get (car dep) result)
          (push dep result))))
    (rust-docs--debug "Collected dependencies: %s" result)
    result))

(defun rust-docs--find-all-cargo-files ()
  "Searches for the Cargo.toml files."
  (let* ((default-directory
          (or (and (project-current) (project-root (project-current)))
              default-directory))
         (res
          (split-string
           (shell-command-to-string
            (format "find %s -name Cargo.toml -print"
                    default-directory)))))
    (rust-docs--debug "Found %d Cargo.toml files" (length res))
    res))

(defun rust-docs--parse-cargo-toml (path)
  "Parses dependencies from Cargo.toml under PATH.
Returns alist of (dependency-name . version)"
  (with-temp-buffer
    (insert-file-contents path)
    (let ((begin (re-search-forward "^\\[dependencies\\]$" nil t 1))
          (end
           (or (re-search-forward "^\\[.*\\]$" nil t 1) (point-max))))
      (when begin
        (goto-char begin)
        (while
            (and
             (< (point) end)
             (re-search-forward
              "^\\([a-z-_0-9]+\\)\\ +?=\\(.*\"\\([0-9.]+\\)\"\\)?.*$"
              nil t))
          (replace-match "(\"\\1\" . \"\\3\")" nil nil))
        (eval
         (car
          (read-from-string
           (format "'(%s)" (buffer-substring begin (point))))))))))


(defun rust-docs--get-rust-dep ()
  "Returns current rust std dependency."
  (cons
   "std"
   (nth
    1
    (split-string (shell-command-to-string "rustc --version")
                  "\\ "))))

; end-region   -- Cargo.toml parsing

; begin-region -- UI

(unless (alist-get "\\*docs.rs\\*" display-buffer-alist)
  (add-to-list
   'display-buffer-alist
   '("\\*docs.rs\\*"
     (display-buffer-reuse-window)
     (dedicated . t)
     (body-function . select-window))))

; end-region   -- UI

; begin-region -- Logging

(defun rust-docs--debug (format-string &rest args)
  "Debug message FORMAT-STRING with ARGS."
  (when rust-docs-debug
    (apply #'message (format "[rust-docs]: %s" format-string) args)))

; end-region   -- Logging

(provide 'rust-docs)
;;; rust-docs.el ends here
