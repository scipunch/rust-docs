;;; rust-docs --- Search and read docs.rs from GNU Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Parses HTML pages from docs.rs.

;;; Code:
(require 'dom)
(require 'cl-lib)
(require 'project)
(require 'org)

; begin-region -- Domain

(cl-defstruct
 (rust-docs--entry (:constructor rust-docs--make-entry)) name href)

(cl-defstruct
 (rust-docs--context
  (:constructor rust-docs--make-context))
 crate-name crate-version href)

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
         (context
          (rust-docs--make-context
           :crate-name dependency
           :crate-version version
           :href
           (rust-docs--entry-href entry)))
         (dom (rust-docs--search-entry context)))
    (rust-docs--open dom context)))

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

(defun rust-docs--search-entry (context)
  "Returns details of the current CONTEXT."
  (with-current-buffer (rust-docs--read-crate-entry-content context)
    (rust-docs--debug "Fetched %s symbols" (point-max))
    (let* ((dom (libxml-parse-html-region))
           (main-content (dom-by-id dom "main-content")))
      (unless main-content
        (error "Main content not found"))
      main-content)))

(defun rust-docs--open (dom context)
  "Creates or reuses docs buffer, parsed DOM and inserts result.
Updates CONTEXT"
  (with-current-buffer (get-buffer-create "*docs.rs*")
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (rust-docs--dom-to-org dom context)
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

(defun rust-docs--read-crate-entry-content (context)
  "Reads content of the href of crate name with version from CONTEXT."
  (rust-docs--debug "Reading content for the context=%s" context)
  (cond
   ((eq rust-docs-resource 'local)
    (error "Local not supported yet"))
   ((eq rust-docs-resource 'web)
    (let ((url
           (rust-docs--web-url (rust-docs--context-crate-name context)
                               (rust-docs--context-crate-version
                                context)
                               (rust-docs--context-href context))))
      (rust-docs--debug "Requesting url=%s" url)
      (url-retrieve-synchronously url)))
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
    (node context &optional insert-text insert-links)
  "Reqursively converts NODE to org.
CONTEXT required for the links.
Inserts string nodes if INSERT-TEXT
Inserts links as org links if INSERT-LINKS"
  (cond
   ((stringp node)
    (and insert-text (insert node)))
   ((eq (dom-tag node) 'h1)
    (rust-docs--h1-to-org node context))
   ((eq (dom-tag node) 'h2)
    (rust-docs--h2-to-org node))
   ((eq (dom-tag node) 'h4)
    (rust-docs--h4-to-org node))
   ((eq (dom-tag node) 'h5)
    (rust-docs--h5-to-org node))
   ((eq (dom-tag node) 'code)
    (rust-docs--code-to-org node))
   ((eq (dom-tag node) 'p)
    (rust-docs--p-to-org node context))
   ((eq (dom-tag node) 'li)
    (rust-docs--li-to-org node context))
   ((and (eq (dom-tag node) 'div)
         (dom-by-class node "docblock-short"))
    (rust-docs--dockblock-short-to-org node))
   ((and (eq (dom-tag node) 'div) (dom-by-class node "stab"))
    (rust-docs--stab-to-org node context))
   ((and insert-links (eq (dom-tag node) 'a))
    (rust-docs--a-to-org node context))
   ((eq (dom-tag node) 'button)
    nil)
   ((eq (dom-tag node) 'table)
    (rust-docs--table-to-org node context))
   (t
    (dolist (child (dom-children node))
      (rust-docs--dom-to-org child context
                             insert-text
                             insert-links)))))

(defun rust-docs--h1-to-org (node context)
  "Converts h1 NODE to org.
Owns CONTEXT."
  (insert "* ")
  (dolist (child (dom-children node))
    (rust-docs--dom-to-org child context t t)) ;; There is no links in h1
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
          (if (and (eq (length children) 1)
                   (eq (length (string-lines (dom-text node))) 1))
              `("~" ,(dom-text node) "~")
            `("#+begin_src rust\n"
              ,(dom-texts node "")
              "\n"
              "#+end_src\n\n"))))
    (rust-docs--debug "Code node=%s children-count=%s"
                      node
                      (length (dom-children node)))
    (apply #'insert org)))

(defun rust-docs--p-to-org (node context)
  "Converts paragraph NODE to org.
Owns CONTEXT."
  (dolist (child (dom-children node))
    (rust-docs--dom-to-org child context t t))
  (insert "\n\n"))

(defun rust-docs--li-to-org (node context)
  "Converts li NODE to org.
Owns CONTEXT."
  (insert "- ")
  (dolist (child (dom-children node))
    (rust-docs--dom-to-org child context t t))
  (insert "\n"))

(defun rust-docs--a-to-org (node context)
  "Converts a NODE to org.
Owns CONTEXT."
  (insert
   "[[elisp:"
   (format "%s"
           `(rust-docs--process-org-link
             ,(format "\"%s\"" (dom-attr node 'href))
             ,context)) ;; TODO: Rewrite these naive formats
   "][" (dom-texts node "") "]]"))

(defun rust-docs--process-org-link (href context)
  "Uses CONTEXT to redraw another docs entry from HREF."
  (setf
   (rust-docs--context-href context)
   (cond
    ((string-match-p "\\.\\.\\/.*" href) ;; https://doc.rust-lang.org/stable/std/convert/index.html + ../ops/trit.Deref.html -> doc.rust-lang.org/stable/std/ops/trait.Deref.html
     (concat
      (replace-regexp-in-string
       "[a-z0-9-_.]+\\/[a-z0-9-_.]+$"
       ""
       (symbol-name (rust-docs--context-href context)))
      (substring href 3 nil)))
    ((string-match-p "^.*\\.html$" href)
     (concat
      (replace-regexp-in-string
       "[a-z0-9_.]+.html$"
       ""
       (symbol-name (rust-docs--context-href context)))
      href))
    (t
     (error "Unsupported case href=%s" href))))
  (rust-docs--open (rust-docs--search-entry context) context))

(defun rust-docs--table-to-org (node context)
  "Convert a table NODE to org.
Owns CONTEXT."
  (rust-docs--debug "Table node=%s" node)
  (cond
   ((eq (dom-tag node) 'table)
    (dolist (child (dom-children node))
      (rust-docs--table-to-org child context))
    (org-table-align)
    (save-excursion
      (forward-line -1)
      (org-table-insert-hline) ;; TODO: Idk why hline in the end of the table wont to be inserted
      (org-table-goto-line 0)
      (org-table-insert-hline))
    (insert "\n"))
   ((or (eq (dom-tag node) 'thead) (eq (dom-tag node) 'tbody))
    (dolist (child (dom-children node))
      (rust-docs--table-to-org child context)))
   ((eq (dom-tag node) 'tr)
    (dolist (child (dom-children node))
      (rust-docs--table-to-org child context))
    (insert " |\n"))
   ((or (eq (dom-tag node) 'td) (eq (dom-tag node) 'th))
    (insert "| ")
    (rust-docs--dom-to-org node context t t))))

(defun rust-docs--dockblock-short-to-org (node)
  "Converts a div NODE to org."
  (insert " " (dom-texts node "")))

(defun rust-docs--stab-to-org (node context)
  "Convert a div stab NODE to org.
Owns CONTEXT."
  (insert "#+begin_comment\n")
  (dolist (child (dom-children node))
    (rust-docs--dom-to-org child context t t))
  (insert "\n#+end_comment\n\n"))

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
