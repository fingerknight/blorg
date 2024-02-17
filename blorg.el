;;; blorg.el --- Dust personal blog system -*- lexical-binding:t; -*-

(require 'denote)
(require 'dash)
(require 'f)
(require 's)
(require 'ht)
(require 'ox)
(require 'ox-publish)
(require 'ox-html)

;;; Basic variables
;;;

(defcustom blorg-base-plist
  (list
   :section-numbers nil
   :with-toc nil
   :with-entities nil
   :html-head-include-default-style nil
   :html-head (concat "<link rel=\"shortcut icon\" href=\"/static/favicon.ico\" type=\"image/x-icon\">\n"
                      "<link rel=\"stylesheet\" href=\"/static/Slippery/style.css\">"
                      "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@callmebill/lxgw-wenkai-web@latest/style.css\">"
                      "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/style.css\">\n")
   :html-preamble 'blorg-html-preamble
   :html-postamble 'blorg-html-postamble
   :with-sub-superscript nil
   :html-footnote-format "<sup class=\"fsup\">%s</sup>"
   :html-self-link-headlines t
   )
  "Common Property list of Blorg.")

(defcustom blorg-title "Ward of Dust"
  "Title of blog")

(defcustom blorg-post-tag "post"
  "Notes with this TAG needed to be published as a post.")

(defcustom blorg-page-tag "page"
  "Notes, said special, with this TAG needed to be published as a page.")

(defcustom blorg-naviation-list
  '(("Archives" . "/archive.html")
    ("Taxonomy" . "/taxonomy.html")
    ("About" . "/about.html"))
  "Navigation list")

(defcustom blorg-output-dir "~/blog"
  "Directory where published HTML files are stored.")

(defcustom blorg-index-post-num 5
  "Number of post items shown in Index Page")

(defcustom blorg-cache-dir org-publish-timestamp-directory
  "Cache dir")

(defcustom blorg-prism-js-mapping
  '((elisp . lisp)
    (emacs-lisp . lisp)
    (sh . bash)
    (js . javascript)
    (html . markup)
    (conf . editorconfig))
  
  "Language mapping from abrev to prism.js")

(defvar blorg-changed-files nil
  "ht: {tag: [tags], category: [categories]}")

(defvar blorg-extra-pkgs nil
  "List: [extra-pkg-names]")

(defvar blorg-math-chapter nil
  "Chapter os math chapter")

(defvar blorg-math-number nil
  "Number of current math prop.")

(defvar blorg-footnote-table nil
  "ht of footnote, with {footnote-num : num of references}")

(defun blorg-compose-project (proj-name &rest args)
  (let ((base-plist (ht<-plist blorg-base-plist)))
    (ht-update base-plist (ht<-plist args))
    (cons proj-name (ht->plist base-plist))))


;;; Utils
;;;

(defun blorg-get-post-title (filename)
  (denote-retrieve-title-value filename 'org))

(defun blorg-get-post-create-date (filename)
  (format-time-string "%Y-%m-%d"
                      (parse-iso8601-time-string
                       (denote-extract-id-from-string filename))))

(defun blorg-get-post-modify-date (filename)
  (format-time-string "%Y-%m-%d" (f-modification-time filename)))

(defun blorg-get-post-tag (filename)
  (denote-extract-keywords-from-path filename))

(defun blorg-get-post-category (filename)
  (car (f-split (f-relative filename denote-directory))))

(defun blorg-get-post-cover (filename)
  "NOTICE: cover should subordinates to `assets'"
  (when-let* ((cover (cadr (s-match "#\\+cover:\\s-*\\(.+\\)"
                                   (f-read filename))))
              (fbase (f-base cover))
              (fext (f-ext cover)))
    (unless (f-absolute-p cover)
      (setq cover (format "/assets/%s.%s"
                          (md5 fbase) fext)))
    cover))

(defun blorg-file-is-older-p (file other)
  (< (car (f-modification-time file 'second))
     (car (f-modification-time other 'second))))

(defun blorg-post-is-special-p (filename)
  (member blorg-page-tag (blorg-get-post-tag filename)))

(defun blorg-get-all-files (&optional without-special)
  (--filter (blorg-file-has-publish-tag it without-special)
   (denote--directory-get-files)))

(defun blorg-get-post-output-relative-filename (filename)
  (f-join "/"
          (f-dirname (f-relative filename denote-directory))
          (concat (denote-extract-id-from-string filename)
                  ".html")))

(defun blorg-file-has-publish-tag (filename &optional without-special)
  (and (denote-file-is-note-p filename)
        (-intersection (blorg-get-post-tag filename)
                       (if without-special (list blorg-post-tag)
                         (list blorg-post-tag blorg-page-tag )))))

(defun blorg-get-special-post-location (filename)
  (let (res)
    (with-temp-buffer
      (insert-file-contents filename)
      (setq res
            (cadr (s-match "^#\\+location:\\s-+\\(.*\\)$"
                           (buffer-substring-no-properties (point-min)
                                                           (point-max)))))
      (kill-buffer))
    res))

(defun blorg-try-get-math-chapter (filename)
  (cadr (s-match "\\([0-9]\\)+\\.\\s-"
                 (blorg-get-post-title filename))))


;;; Rewrite old functions
;;;

(defun org-publish-needed-p (filename &optional pub-dir pub-func _true-pub-dir base-dir)
  (let ((rtn (if (not org-publish-use-timestamps-flag) t
         (org-publish-cache-file-needs-publishing
          filename pub-dir pub-func base-dir))))
    (when (and (not rtn)
             org-publish-list-skipped-files))
    rtn))

(defun org-html-publish-to-html (plist filename pub-dir)
  "FIX: 1. backend
2. math chapter and number"
  (setq blorg-math-chapter
        (or (blorg-try-get-math-chapter filename) blorg-math-chapter)
        blorg-math-number 1)
  (org-publish-org-to 'blorg-backend filename
                    (concat (when (> (length org-html-extension) 0) ".")
                            (or (plist-get plist :html-extension)
                                org-html-extension
                                "html"))
                    plist pub-dir))

(defun denote-link-ol-export (link description format)
  "Fix HTML anchor, and output filename."
  (let* ((path-id (denote-link--ol-resolve-link-to-target link :path-id))
         (path (car path-id))
         (p (blorg-get-post-output-relative-filename path))
         (id (nth 1 path-id))
         (anchor (nth 2 path-id))
         (desc (or description (concat "denote:" id))))
    (cond
     ((eq format 'html) (format "<a href=\"%s%s\" target=\"_blank\">%s</a>"
                                p
                                (if anchor (format "#%s" anchor) "")
                                desc))
     (t path))))

(defun denote-link--ol-resolve-link-to-target (link &optional path-id)
  "FIX: html anchor"
  (let* ((search (and (string-match "::\\(.*\\)\\'" link)
                      (match-string 1 link)))
         (id (if (and search (not (string-empty-p search)))
                 (substring link 0 (match-beginning 0))
               link))
         (path (denote-get-path-by-id id)))
    (if path-id
        (list (format "%s" path) (format "%s" id) search)
      (concat path (when search "::") search))))

(defun blorg-advice-use-slug-output (args)
  (if-let* ((backend (car args))
            (file (cadr args))
            (_ (eq backend 'blorg-backend))
            (id (s-match denote-id-regexp file)))
      (append
       (list backend
             (f-expand
              (concat
               (if (member blorg-page-tag (blorg-get-post-tag file))
                   (let* ((origin-html-file (f-expand
                                             (f-relative file blorg-output-dir)
                                             denote-directory))
                          (origin-org-file (concat (f-no-ext origin-html-file)
                                                   ".org")))
                     (or (blorg-get-special-post-location
                          origin-org-file) ""))
                 (car id))
               ".html")
              (f-dirname file)))
       (cddr args))
    args))

(advice-add 'org-export-to-file :filter-args 'blorg-advice-use-slug-output)

(defun org-html--format-image (source attributes info)
  "Change the default arrtibute of `alt'"
  (org-html-close-tag
   "img"
   (org-html--make-attribute-string
    (org-combine-plists
     (list :src source
           :alt (concat (md5 (f-base source)) "." (f-ext source)))
     (if (string= "svg" (file-name-extension source))
         (org-combine-plists '(:class "org-svg") attributes '(:fallback nil))
       attributes)))
   info))

(defun blorg-advice-add-math-support (latex-frag processing-type info)
  (when (eq processing-type 'mathjax)
    (add-to-list 'blorg-extra-pkgs 'math)))

(advice-add 'org-html-format-latex :before 'blorg-advice-add-math-support)


;;; Preamble/Postamble

(defun blorg-html-preamble (plist)
  (concat "<div class=\"header\">\n"
          "<div class=\"title\">\n"
          "<a href=\"/\">" blorg-title "</a>\n"
          "</div>\n"
          "<div class=\"navigation\">\n"
          (s-join "\n"
                  (--map
                   (format "<a class=\"item\" href=\"%s\">%s</a>"
                           (cdr it) (car it))
                   blorg-naviation-list))
          "</div>\n"
          "</div>\n"))

(defun blorg-html-postamble (plist)
  (concat "<center>\n"
          "Powered by "
          "<a href=\"https://orgmode.org\">"
          "Org Mode"
          "</a>.\n"
          "</center>\n"))

;;; org-html backend
;;;

(defun blorg-html-build-meta (info)
  (let ((title (org-html-plain-text
                (org-element-interpret-data (plist-get info :title)) info)))
    (concat "<meta charset=\"utf-8\">\n"
            "<meta name=\"viewport\" content=\"width=device-width\">\n"
            (format "<title> %s </title>\n"
                    (if (string= title blorg-title)
                        title
                      (format "%s - %s" title blorg-title))
                    )
            "<meta name=\"author\" content=\"MrDust\">\n")))

(defun blorg-html-build-foot (info)
  (concat "<script src=\"/static/script.js\"></script>\n"))

(defun blorg-html-extra-head ()
  (mapconcat
   (lambda (it)
     (pcase it
       ('mermaid "")
       ('math "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css\">\n")
       ('highlight (concat "<link rel=\"stylesheet\" href=\"/static/Fantasque/style.css\">\n"
                           "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/dracula-prism@2.1.16/dist/css/dracula-prism.min.css\">\n"))))
   blorg-extra-pkgs))

(defun blorg-html-extra-foot ()
  (mapconcat
   (lambda (it)
     (pcase it
       ('mermaid "<script type=\"module\">
import mermaid from \"https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs\";
mermaid.initialize({ startOnLoad: true, securityLevel: 'loose' });
</script>")
       ('math (concat "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js\"></script>\n"
                      "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/contrib/auto-render.min.js\"></script>\n"
                      "<script src=\"/static/katex.js\"></script>"))
       ('highlight "<script src=\"/static/prism.js\"></script>\n")))
   blorg-extra-pkgs))

(defun blorg-html-inner-template (contents info)
  (let ((filename (plist-get info :input-file)))
    (concat
     (format "<div class=\"%s\">\n" (or (plist-get info :blorg-type) "post"))
     ;; Cover
     (when-let ((cover (blorg-get-post-cover (plist-get info :input-file))))
       (format "<img class=\"cover\" src=\"%s\">\n" cover))
     ;; Document title.
     (when (plist-get info :with-title)
       (let ((title (and (plist-get info :with-title)
                         (plist-get info :title))))
         (when title
           (format "<h1 class=\"title\">%s</h1>\n"
                   (org-export-data title info)))))
     ;; Posts Meta data
     (when (denote-file-is-note-p filename)
       ;; Posts
       (let* ((cdate (blorg-get-post-create-date filename))
              (mdate (blorg-get-post-modify-date filename))
              (tags (blorg-get-post-tag filename))
              (category (blorg-get-post-category filename)))
         ;; KEEP: modify date keeps here just in case
         (unless (blorg-post-is-special-p filename)
           (concat "<div class=\"metadata\">\n"
                   (format "<div class=\"date\"><span>%s</span></div>\n"
                           cdate)
                   "<div class=\"tags\">\n"
                   (s-join " "
                           (--map (format "<a class=\"tag\" href=\"/tag/%s.html\">#%s</a>" it it)
                         tags))
                   "</div>\n"
                   "<div class=\"category\">\n"
                   (format "<a href=\"/category/%s.html\">$%s</a>\n"
                           category category)
                   "</div>\n"
                   "</div>\n"))))
     ;; Table of contents.
     (let ((depth (plist-get info :with-toc)))
       (when depth (org-html-toc depth info)))
     ;; Document contents.
     contents
     ;; Footnotes section.
     (blorg-html-footnote-section info)
     "</div>\n")))

(defun blorg-html-template (contents info)
  (concat "<!DOCTYPE html>\n"
          "<html lang=\"zh\">\n"
          "<head>\n"
          (blorg-html-build-meta info)
          (blorg-html-extra-head)
          (org-html--build-head info)
          "</head>\n"
          "<body>\n"
          ;; Preamble.
          (org-html--build-pre/postamble 'preamble info)
          ;; Document contents.
          (let ((div (assq 'content (plist-get info :html-divs))))
            (format "<%s id=\"%s\" class=\"%s\">\n"
                    (nth 1 div)
                    (nth 2 div)
                    (plist-get info :html-content-class)))
          contents
          (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
          ;; Postamble.
          (org-html--build-pre/postamble 'postamble info)
          (blorg-html-build-foot info)
          (blorg-html-extra-foot)
          "</body>\n"
          "</html>"))

(defun blorg-html-src-block (src-block _contents info)
  "FIX: for integrating with prism.js"
  (let* ((lang (org-element-property :language src-block))
         (code-info (org-export-unravel-code src-block))
         (code (org-html-encode-plain-text (car code-info)))
         (lbl (org-html--reference src-block info t))
         (label (if lbl (format " id=\"%s\"" lbl) ""))
         (pre-attribute "")
         (contents ""))
    ;; Transfer `\t' to `    ' at the beginning or end
    (--each-r
        (s-matched-positions-all "^\\(\t\\|[ ]\\)+" code)
      (let ((start-pos (car it))
            (stop-pos (cdr it)))
        (setq code (concat (substring code 0 start-pos)
                           (s-replace "\t" "    " (substring code
                                                             start-pos
                                                             stop-pos))
                           (substring code stop-pos)))))
    (pcase lang
      ("mermaid"
       (add-to-list 'blorg-extra-pkgs 'mermaid)
       (setq contents code
             pre-attribute " class=\"mermaid\""))
      (otherwise
       (add-to-list 'blorg-extra-pkgs 'highlight)
       (setq contents (format "<code class=\"language-%s\">%s</code>\n"
                              lang code))))
    (format "<div class=\"org-src-container\">\n%s%s\n</div>"
            ;; Build caption.
            (let ((caption (org-export-get-caption src-block)))
              (if (not caption) ""
                (let ((listing-number
                       (format
                        "<span class=\"listing-number\">%s </span>"
                        (format
                         (org-html--translate "Listing %d:" info)
                         (org-export-get-ordinal
                          src-block info nil #'org-html--has-caption-p)))))
                  (format "<label class=\"org-src-name\">%s%s</label>"
                          listing-number
                          (org-trim (org-export-data caption info))))))
            ;; Contents.
            (format "<pre%s>\n%s</pre>\n" pre-attribute contents))))

(defun blorg-html-link (link desc info)
  "FIX: Links and Images"
  (let ((html-string (org-html-link link desc info)))
    ;; Handle normal links
    (cond
     ((s-starts-with-p "<a" html-string)
      (let ((url-match (s-match "href=\"\\([^\"]+\\)\"" html-string)))
        (if (and url-match (s-starts-with-p "#" (cadr url-match)))
            html-string
          (concat "<a target=\"_blank\"" (substring html-string 2)))))
     ;; Handle image
     ((s-starts-with-p "<img" html-string)
      (let ((url-match (s-match "src=\"\\([^\"]+\\)\"" html-string)))
        (if url-match
            (let* ((link (cadr url-match))
                   (uri (f-join "/" (f-dirname link) (concat (md5 (f-base link))
                                                             "."
                                                             (f-ext link))))
                   (html-string-1 (s-replace-regexp "src=\"\\([^\"]+\\)\""
                                                    (format "src=\"%s\"" uri)
                                                    html-string)))
              (concat (format "<a href=\"%s\" target=\"_blank\">\n" uri)
                      html-string-1
                      "</a>"))
          html-string)))
     (t html-string))))


(defun blorg-html-special-block (special-block contents info)
  "FIX: Math blocks"
  (let* ((block-type (org-element-property :type special-block))
         (html5-fancy (and (org-html--html5-fancy-p info)
                           (member block-type org-html-html5-elements)))
         (attributes (org-export-read-attribute :attr_html special-block))
         (name (org-element-property :name special-block))
         (math-block (car (member block-type '("definition"
                                               "proposition"
                                               "lemma"
                                               "theorem"
                                               "proof"
                                               "axiom"
                                               "remark"
                                               "solution"))))
         (is-proof-p (equal math-block "proof")))

    (when math-block
      (let* ((num-string (format "%s.%s"
                                blorg-math-chapter
                                blorg-math-number))
             (title-string (format "<span class=\"number\">%s %s </span>%s"
                                   (s-upper-camel-case math-block)
                                   (if is-proof-p "" num-string)
                                   (if name (format "(%s) " name) ""))))
        (setq attributes (plist-put attributes :class "mathblock")
              contents (if (s-starts-with-p "<p>" contents)
                           (concat "<p>"
                                   title-string
                                   (substring contents 3))
                         (concat title-string contents))
              blorg-math-number (+ blorg-math-number (if is-proof-p 0 1)))))
    
    (unless html5-fancy
      (let ((class (plist-get attributes :class)))
        (setq attributes (plist-put attributes :class
                                    (if class (concat class " " block-type)
                                      block-type)))))
    (let* ((contents (or contents ""))
           (reference (org-html--reference special-block info))
           (a (org-html--make-attribute-string
               (if (or (not reference) (plist-member attributes :id))
                   attributes
                 (plist-put attributes :id reference))))
           (str (if (org-string-nw-p a) (concat " " a) "")))
      (when is-proof-p
        (setq contents (concat (substring contents 0 -5)
                               "<span class=\"QED\">□</span>\n</p>")))
      (if html5-fancy
          (format "<%s%s>\n%s</%s>" block-type str contents block-type)
        (format "<div%s>\n%s\n</div>" str contents)))))

(defun blorg-html-quote-block (quote-block contents info)
  "FIX: Add author and reference. By using `#+attr_html: :from [ref] :by [author] before quote block.'
"
  (format "<blockquote%s>\n%s</blockquote>"
          (let* ((reference (org-html--reference quote-block info t))
                 (attributes
                  (let* ((attr (org-export-read-attribute :attr_html
                                                          quote-block))
                         (from (plist-get attr :from))
                         (by (plist-get attr :by)))
                    (plist-put attr :from nil)
                    (plist-put attr :by nil)
                    (when (or from by)
                      (setq contents
                            (s-append (concat  "<p class=\"source\">"
                                               " &#x2014;&#x2014; "
                                               by
                                               (when (and from by) ", ")
                                               from
                                               " </p>\n")
                                      contents)))
                    attr))
                 (a (org-html--make-attribute-string
                     (if (or (not reference) (plist-member attributes :id))
                         attributes
                       (plist-put attributes :id reference)))))
            (if (org-string-nw-p a) (concat " " a) ""))
          contents))

(defun blorg-get-footnote-reference-number (footnote-reference info &optional all)
  "Return current number of FOOTNOTE-REFERENCE. With `all', then return all current footnote's reference."
  (let ((label (org-element-property :label footnote-reference))
        (num 1))
    (when label
      (catch 'exit
        (org-export--footnote-reference-map
         (lambda (f)
           (let ((l (org-element-property :label f)))
             (when (and l label (string= l label))
               (when (and (eq f footnote-reference)
                          (not all))
                   (throw 'exit nil))
               (setq num (1+ num)))))
         (plist-get info :parse-tree) info)))
    num))

(defun blorg-html-footnote-section (info)
  "FIX: Add all references."
  (pcase (org-export-collect-footnote-definitions info)
    (`nil nil)
    (definitions
     (format
      (plist-get info :html-footnotes-section)
      (org-html--translate "Footnotes" info)
      (format
       "\n%s\n"
       (mapconcat
        (lambda (definition)
          (pcase definition
            (`(,n ,_ ,def)
             ;; `org-export-collect-footnote-definitions' can return
             ;; two kinds of footnote definitions: inline and blocks.
             ;; Since this should not make any difference in the HTML
             ;; output, we wrap the inline definitions within
             ;; a "footpara" class paragraph.
             (let ((inline? (not (org-element-map def org-element-all-elements
                                   #'identity nil t)))
                   (contents (org-trim (org-export-data def info))))
               (format "<div id=\"fn.%d\" class=\"footdef\">%s %s</div>\n"
                       n
                       (concat
                        (format "<span class=\"footnum\"> (%d) </span>" n)
                        (let ((s ""))
                          (--dotimes
                              (ht-get blorg-footnote-table n)
                            (setq s (concat s
                                            (format "<a class=\"foot2ref\" href=\"#fnr.%d.%d\" role=\"doc-backlink\"> ⇑ </a>"
                                                    n (1+ it)))))
                          s)
                        "\n")
                       (format "<div class=\"footpara\" role=\"doc-footnote\">%s</div>"
                           (if (not inline?) contents
                             (format "<p class=\"footpara\">%s</p>"
                                 contents))))))))
        definitions
        "\n"))))))

(defun blorg-html-headline (headline contents info)
  "FIX: no self link on lower headlines"
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
           (id (org-html--reference headline info))
           (formatted-text
            (if (and (plist-get info :html-self-link-headlines)
                     (<= level (1+ (plist-get info :headline-levels))))
                (format "<a href=\"#%s\">%s</a>" id full-text)
              full-text)))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((html-type (if numberedp "ol" "ul")))
            (concat
             (and (org-export-first-sibling-p headline info)
              (apply #'format "<%s class=\"org-%s\">\n"
                 (make-list 2 html-type)))
             (org-html-format-list-item
              contents (if numberedp 'ordered 'unordered)
              nil info nil
              (concat (org-html--anchor id nil nil info) formatted-text)) "\n"
             (and (org-export-last-sibling-p headline info)
              (format "</%s>\n" html-type))))
        ;; Standard headline.  Export it as a section.
        (let ((extra-class
               (org-element-property :HTML_CONTAINER_CLASS headline))
              (headline-class
               (org-element-property :HTML_HEADLINE_CLASS headline))
              (first-content (car (org-element-contents headline))))
          (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (format "outline-container-%s" id)
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
                  (format "\n<h%d id=\"%s\"%s>%s</h%d>\n"
                          level
                          id
                  (if (not headline-class) ""
                    (format " class=\"%s\"" headline-class))
                          (concat
                           (and numberedp
                                (format
                                 "<span class=\"section-number-%d\">%s</span> "
                                 level
                                 (concat (mapconcat #'number-to-string numbers ".") ".")))
                           formatted-text)
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))

(defun blorg-advice-headline-id (fn &rest args)
  "FIX: headline id"
  (let* ((maybe-headline (car args))
         (info (cadr args)))
    (if (memq (org-element-type maybe-headline) '(headline))
        (org-export-data (org-element-property :title maybe-headline) info)
      (apply fn args))))

(advice-add 'org-html--reference :around 'blorg-advice-headline-id)

(defun blorg-html-footnote-reference (footnote-reference _contents info)
  "FIX: correct reference number."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
          (id (format "fnr.%d.%d"
                      n
                      (blorg-get-footnote-reference-number footnote-reference
                                                           info))))
     (ht-set blorg-footnote-table n
             (1+ (ht-get blorg-footnote-table n 0)))
     (format
      (plist-get info :html-footnote-format)
      (org-html--anchor
       id (format "[%d]" n)
       (format " class=\"footref\" href=\"#fn.%d\" role=\"doc-backlink\"" n)
       info)))))




(defun blorg-html-remove-spaces (contents _backend info)
  (s-replace-regexp
   "\\([[:multibyte:]]\\)[ ]+\\(<b>[[:multibyte:]]\\)" ; Spaces before BOLD.
   "\\1\\2"
   
   (s-replace-regexp
    "\\([[:multibyte:]]</b>\\)[ ]+\\([[:multibyte:]]\\)" ; Spaces after BOLD
    "\\1\\2"
    contents)))


(org-export-define-derived-backend 'blorg-backend 'html
  :translate-alist '((template . blorg-html-template)
                     (inner-template . blorg-html-inner-template)
                     (src-block . blorg-html-src-block)
                     (link . blorg-html-link)
                     (special-block . blorg-html-special-block)
                     (quote-block . blorg-html-quote-block)
                     (footnote-reference . blorg-html-footnote-reference)
                     (headline . blorg-html-headline))
  :filters-alist '((:filter-final-output . (org-html-final-function
                                            blorg-html-remove-spaces))))


;;; Publishing Index Page
;;;

(defun blorg-org-index-item (filename)
  (concat "<div class=\"item\">\n"
          (format "<image class=\"cover\" src=\"%s\">\n"
                  (blorg-get-post-cover filename))
          (format "<a class=\"title\" href=\"%s\"> %s </a>\n"
                  (blorg-get-post-output-relative-filename filename)
                  (blorg-get-post-title filename))
          "<div class=\"metadata\">\n"
          ;; (format "<span class=\"date\"> %s </span>" (blorg-get-post-modify-date filename))
          (format "<span class=\"date\"> %s </span>" (blorg-get-post-create-date filename))
          "<div>\n"
          (s-join " "
                  (--map (format "<a class=\"tag\" href=\"/tag/%s.html\">#%s</a>" it it)
                         (blorg-get-post-tag filename)))
          ","
          (format "<a class=\"category\" href=\"/category/%s.html\">$%s</a>\n"
                  (blorg-get-post-category filename) (blorg-get-post-category filename))
          "</div>\n</div>\n"
          "</div>"))

(defun blorg-org-index (filenames)
  (concat (format "#+title: %s\n\n" blorg-title)
          "#+begin_center\n"
          "_I choose to see the beauty._"
          "\n#+end_center\n\n"

          "#+begin_export html\n"
          (s-join "\n"
                  (-map #'blorg-org-index-item
                        filenames))
          "\n#+end_export\n"
          ))

(defun blorg-assemble-index (plist)
  (when (or (ht-get blorg-changed-files "tag")
            (ht-get blorg-changed-files "category"))
    (message "Assembling index page")
    (let ((normal-posts (--sort (not (blorg-file-is-older-p it other))
                                (blorg-get-all-files t))))
      (with-temp-file (expand-file-name "index.org" blorg-cache-dir)
        (insert (blorg-org-index (-slice
                                  normal-posts
                                  0 blorg-index-post-num)))))))


;;; Publishing post
;;;

(defun blorg-advice-filter-files (project)
  (when (string= (org-publish-property :blorg-type project)
                 "post")
    (blorg-get-all-files)))

(defun blorg-post-add-tag-and-category-to-changed-files (filename _output)
  (when (denote-filename-is-note-p filename)
    (let ((tags (blorg-get-post-tag filename))
          (category (blorg-get-post-category filename)))
      (ht-set blorg-changed-files "tag"
              (-union (ht-get blorg-changed-files "tag")
                      tags))
      (ht-set blorg-changed-files "category"
              (-union (list category)
                      (ht-get blorg-changed-files "category"))))))


;;; Publishing taxonomy
;;;

(defun blorg-org-taxonomy (tags-list categoreis-list)
  (concat "#+title: Taxonomy\n\n"
          "* Categories:\n"
          "#+begin_export html\n"
          "<div class=\"wrapper\">\n"
          (s-join "\n"
                  (--map
                   (concat "<a href=\"/category/"
                           it
                           ".html\" class=\"taxonomies\">"
                           it
                           "</a><sup>"
                           (format "%d" (ht-get categoreis-list it))
                           "</sup>")
                   (-sort #'string<
                          (ht-keys categoreis-list))))
          "</div>\n"
          "\n#+end_export\n\n"
          "* Tags:\n"
          "#+begin_export html\n"
          "<div class=\"wrapper\">\n"
          (s-join "\n"
                  (--map
                   (concat "<a href=\"/tag/"
                           it ".html\" class=\"taxonomies\">"
                           it
                           "</a><sup>"
                           (format "%d" (ht-get tags-list it))
                           "</sup>")
                   (-sort #'string<
                          (ht-keys tags-list))))
          "</div>\n"
          "\n#+end_export\n"))

(defun blorg-assemble-taxonomy (plist)
  (let ((tags-list (ht-create))
        (categories-list (ht-create)))
    (when (ht-get blorg-changed-files "tag")
      (message "Assembling taxonomy page")
      (--map
       (let ((category (f-relative (f-dirname it) denote-directory))
             (tags (blorg-get-post-tag it)))
         ;; Collecting categories
         (unless (string= category ".")
           (ht-set categories-list category
                   (1+ (ht-get categories-list category 0))))
         ;; Collecting tags
         (dolist (tag tags)
             (unless (member tag '(blorg-post-tag blorg-page-tag))
               (ht-set tags-list tag
                       (1+ (ht-get tags-list tag 0))))))
       (blorg-get-all-files t))
      ;; Write to file
      (with-temp-file (expand-file-name "taxonomy.org" blorg-cache-dir)
        (insert (blorg-org-taxonomy tags-list categories-list))))))
  

;;; Publishing tag/category page
;;;

(defun blorg-org-each-taxonomy (what each filenames)
  (concat (format "#+title: %s: %s\n\n" (s-upper-camel-case what) each)
          "#+begin_export html\n"
          (s-join "\n"
                  (--map
                   (concat
                    "<div class=\"item\">\n"
                    "<a href=\""
                    (blorg-get-post-output-relative-filename it)
                    "\">"
                    (blorg-get-post-title it)
                    "</a>"
                    "<span class=\"date\">"
                    (blorg-get-post-create-date it)
                    "</span>\n"
                    "</div>")
                   filenames))
          "\n#+end_export"))

(defun blorg-assemble-each-taxonomy (what plist)
  (let ((each-list (ht-create))
        (normal-posts (blorg-get-all-files t))
        (each-input-dir (expand-file-name what blorg-cache-dir))
        (fn (pcase what
              ("tag" 'blorg-get-post-tag)
              ("category" 'blorg-get-post-category))))
    (unless (f-dir-p each-input-dir)
      (make-directory each-input-dir t))

    ;; Get all tags/categories among posts
    (--map
     (let ((eachs (funcall fn it)))
       (if (listp eachs)
           (dolist (each eachs)
             (ht-set each-list each (cons it (ht-get each-list each))))
         (ht-set each-list eachs (cons it (ht-get each-list eachs)))))
     normal-posts)
    
    ;;; Enumerate tags/categoires
    (ht-amap 
     (when (member key (ht-get blorg-changed-files what))
       (with-temp-file (expand-file-name (format "%s.org" key) each-input-dir)
         (message "Assembling %s: %s" what key)
         (insert (blorg-org-each-taxonomy
                  what key (--sort
                            (not (blorg-file-is-older-p it other))
                            value)))))
     each-list)))


;;; Publishing Archive
;;;

(defun blorg-org-archive (filenames)
  (let ((current-year "1999"))
    (concat "#+title: Archive\n\n"
            (s-join
             "\n"
             (--map (let* ((date-split (s-split "-"
                                                (blorg-get-post-create-date it)))
                           (year (car date-split))
                           (md (s-join "-" (cdr date-split)))
                           (res ""))
                      (unless (s-equals-p year current-year)
                        (unless (s-equals-p current-year "1999")
                          (setq res "#+end_export\n\n"))
                        (setq res (concat res "* " year "\n"
                                          "#+begin_export html\n")
                              current-year year))
                      (setq res
                            (concat
                             res
                             "<div class=\"item\">\n"
                             (format "<a class=\"title\" href=\"%s\">%s</a><span class=\"date\">%s</span>\n"
                                     (blorg-get-post-output-relative-filename it)
                                     (blorg-get-post-title it)
                                     md)

                             "</div>"))
                      res)
                    filenames))
            "\n#+end_export")))

(defun blorg-assemble-archive (plist)
  (when (or (ht-get blorg-changed-files "tag")
            (ht-get blorg-changed-files "category"))
    (message "Assembling archive page")
    (let ((normal-posts (--sort (string> (denote-extract-id-from-string it)
                                         (denote-extract-id-from-string other))
                                (blorg-get-all-files t))))
      (with-temp-file (expand-file-name "archive.org" blorg-cache-dir)
        (insert (blorg-org-archive normal-posts))))))


;;; Project settings
;;;

(defun blorg-post-publishing-function (plist filename pub-dir)
  "FIX: clearn `blorg-extra-pkgs', and footnote table."
  (let ((blorg-footnote-table (ht-create)))
    (org-html-publish-to-html plist filename pub-dir))
  (setq blorg-extra-pkgs nil))

(defun blorg-image-publishing-function (_plist filename pub-dir)
  "FIX: use slug for images"
  (unless (f-exists-p pub-dir)
    (make-directory pub-dir t))
  (let* ((filebase (f-base filename))
         (output-base (md5 filebase))
         (output (f-expand (concat output-base
                                   "."
                                   (f-ext filename))
                           pub-dir)))
    (unless (or (f-dir-p filename)
                (and (f-exists-p output)
                     (blorg-file-is-older-p output filename)))
      (copy-file filename output t))
    ;; Return file name.
    output))


;;;###autoload
(defun blorg (&optional force)
  "Publish blog.
With FORCE, then force project to republish."
  (interactive "P")
  (message "Start publishing files from %s to %s."
           denote-directory blorg-output-dir)
  (save-window-excursion
    (let ((blorg-changed-files (ht ("tag" nil) ("category" nil)))
          (blorg-extra-pkgs nil)
          (blorg-math-chapter 1)
          (blorg-math-number 1)
          (org-publish-use-timestamps-flag
           (and (not force) org-publish-use-timestamps-flag))

          (projects
           (list
            (blorg-compose-project
             "blorg-post"
             :base-directory denote-directory
             :publishing-directory blorg-output-dir
             :recursive t
             :with-toc t
             :blorg-type "post"
             :publishing-function 'blorg-post-publishing-function
             :preparation-function
             (lambda (plist)
               (advice-add 'org-publish-get-base-files :before-until
                           'blorg-advice-filter-files)
               (add-hook 'org-publish-after-publishing-hook
                         'blorg-post-add-tag-and-category-to-changed-files)))
            
            (blorg-compose-project
             "blorg-index"
             :base-directory blorg-cache-dir
             :publishing-directory blorg-output-dir
             :exclude ".*"
             :include '("index.org")
             :preparation-function 'blorg-assemble-index
             :blorg-type "index"
             :with-title nil)

            (blorg-compose-project
             "blorg-archive"
             :base-directory blorg-cache-dir
             :publishing-directory blorg-output-dir
             :exclude ".*"
             :include '("archive.org")
             :blorg-type "archive"
             :preparation-function 'blorg-assemble-archive)
            
            (blorg-compose-project
             "blorg-taxonomy"
             :base-directory blorg-cache-dir
             :publishing-directory blorg-output-dir
             :exclude ".*"
             :include '("taxonomy.org")
             :preparation-function 'blorg-assemble-taxonomy
             :blorg-type "taxonomy")

            (blorg-compose-project
             "blorg-category"
             :base-directory (f-expand "category" blorg-cache-dir)
             :publishing-directory (f-expand "category" blorg-output-dir)
             :blorg-type "each-taxonomy"
             :preparation-function
             (lambda (_)
               (blorg-assemble-each-taxonomy "category" _)))
            
            (blorg-compose-project
             "blorg-tag"
             :base-directory (f-expand "tag" blorg-cache-dir)
             :publishing-directory (f-expand "tag" blorg-output-dir)
             :blorg-type "each-taxonomy"
             :preparation-function
             (lambda (_)
               (blorg-assemble-each-taxonomy "tag" _)))

            (blorg-compose-project
             "blorg-asset"
             :base-directory (expand-file-name "assets" denote-directory)
             :publishing-directory (expand-file-name "assets" blorg-output-dir)
             :base-extension "jpg\\|gif\\|png"
             :publishing-function 'blorg-image-publishing-function)

            (blorg-compose-project
             "blorg-static"
             :base-directory (expand-file-name "static" denote-directory)
             :publishing-directory (expand-file-name "static" blorg-output-dir)
             :base-extension "ico\\|css\\|js\\|svg\\|woff\\|woff2\\|otf"
             :recursive t
             :publishing-function 'org-publish-attachment
             :preparation-function
             (lambda (_)
               (let ((in (f-expand "style.scss" denote-directory))
                     (out (f-expand "static/style.css" denote-directory)))
                 (when (or (not (f-exists-p out))
                           (blorg-file-is-older-p out in))
                   (shell-command-to-string
                    (format "sass %s %s" in out)))))))))
      (org-publish-projects projects))))

(provide 'blorg)
;;; init-publish.el ends
