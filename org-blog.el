;;; org-blog.el --- a simple org-mode based static blog generator

;;; An fork from: https://github.com/bastibe/org-static-blog
;; Author: Bastian Bechtold
;; Contrib: Shmavon Gazanchyan, Rafał -rsm- Marek, neeasade,
;; Michael Cardell Widerkrantz, Matthew Bauer, Winny, Yauhen Makei,
;; luhuaei, zngguvnf, Qiantan Hong, Jonas Bernoulli, Théo Jacquin,
;; K. Scarlet, zsxh
;; URL: https://github.com/bastibe/org-blog
;; Version: 1.6.0
;; Package-Requires: ((emacs "24.3"))

;;; Author: Finger Knight
;; version: 0.0.1
;; URL:
;; Package-Requires: ((emacs "28.2") (f.el) (dash.el) (s.el) (ts.el))

;;; Commentary:

;;; Personal fork of org-static-blog

;;; Code:

(require 'dash)
(require 'f)
(require 's)
(require 'ts)
(require 'org)
(require 'ox-html)

(defgroup org-blog nil
  "Settings for a static blog generator using org-mode"
  :version "1.6.0"
  :group 'applications)

;; (defcustom org-blog-publish-url "https://example.com/"
;;   "URL of the blog."
;;   :type '(string)
;;   :safe t)

(defcustom org-blog-publish-title "Example.com"
  "Title of the blog."
  :type '(string)
  :safe t)

(defcustom org-blog-publish-directory "~/blog/publish"
  "Directory where published HTML files are stored.

Only are files with non-nil `#+publish' contained"
  :type '(directory))

(defcustom org-blog-posts-directory "~/blog/posts/"
  "Directory where published ORG files are stored.
When publishing, posts are rendered as HTML, and included in the
index, archive, tags, and RSS feed."
  :type '(directory))

(defcustom org-blog-static-directory "~/blog/staic/"
  "Directory where static resoureces are stored, like css, js.
They will be copy to `static/' in publish directory."
  :type '(directory))

(defcustom org-blog-index-file "index.html"
  "File name of the blog landing page.
The index page contains the most recent
`org-blog-index-length` full-text posts."
  :type '(string)
  :safe t)

(defcustom org-blog-index-length 5
  "Number of articles to include on index page."
  :type '(integer)
  :safe t)

(defcustom org-blog-date-format "%Y-%m-%d"
  "Formatter of date."
  :type '(string)
  :safe t)

(defcustom org-blog-archive-file "archive/index.html"
  "File name of the list of all blog posts.
The archive page lists all posts as headlines."
  :type '(string)
  :safe t)

(defcustom org-blog-tags-file "tags/index.html"
  "File name of the list of all blog posts by tag.
The tags page lists all posts as headlines."
  :type '(string)
  :safe t)

;; (defcustom org-blog-enable-tags nil
;;   "Show tags below posts, and generate tag pages."
;;   :group 'org-blog
;;   :type '(boolean)
;;   :safe t)

(defcustom org-blog-rss-file "rss.xml"
  "File name of the RSS feed."
  :type '(string)
  :safe t)

(defcustom org-blog-rss-excluded-tag nil
  "Posts with this tag won't be included in the RSS feeds."
  :type '(choice (const :tag "None" nil)
                 (repeat string))
  :safe t)

(defcustom org-blog-rss-extra ""
  "Extra information for the RSS feed header.
This information is placed right before the sequence of posts.
You can add an icon for the feed, or advertise that you built
your blog with emacs, org-mode and org-blog.
"
  :type '(string)
  :safe t)

(defcustom org-blog-rss-max-entries nil
  "Maximum number of entries in the RSS feed.
If nil (the default), all existing posts are included."
  :type '(choice (const nil) integer)
  :safe t)

;; (defcustom org-blog-enable-tag-rss nil
;;   "Whether to generate per tag RSS feeds.

;; When this flag is set, an RSS file with name given by prefixing
;; `org-blog-rss-file' with '<tag>-' is created for each
;; existing tag.  The options `org-blog-rss-extra',
;; `org-blog-rss-max-entries' and
;; `org-blog-rss-excluded-tag' are also used to construct
;; per-tag RSS feeds."
;;   :type '(boolean))

(defcustom org-blog-page-head ""
  "HTML to put in the <head> of each page."
  :type '(string)
  :safe t)

(defcustom org-blog-page-preamble ""
  "HTML to put before the content of each page."
  :type '(string)
  :safe t)

(defcustom org-blog-page-postamble ""
  "HTML to put after the content of each page."
  :type '(string)
  :safe t)

(defcustom org-blog-index-front-matter ""
  "HTML to put at the beginning of the index page."
  :type '(string)
  :safe t)

;; (defcustom org-blog-post-comments ""
;;   "HTML code for comments to put after each blog post."
;;   :type '(string)
;;   :safe t)

;; (defcustom org-blog-langcode "en"
;;   "Language code for the blog content."
;;   :type '(string)
;;   :safe t)

;; (defcustom org-blog-use-preview nil
;;   "Use preview versions of posts on multipost pages.

;; See also `org-blog-preview-start',
;; `org-blog-preview-end', `org-blog-preview-ellipsis'
;; and `org-blog-preview-link-p'."
;;   :type '(boolean)
;;   :safe t)

;; (defcustom org-blog-preview-start nil
;;   "Marker indicating the beginning of a post's preview.

;; When set to nil, we look for the first occurence of <p> in the
;; generated HTML.  See also `org-blog-preview-end'."
;;   :type '(choice (const :tag "First paragraph" nil) (string))
;;   :safe t)

;; (defcustom org-blog-preview-end nil
;;   "Marker indicating the end of a post's preview.

;; When set to nil, we look for the first occurence of </p> after
;; `org-blog-preview-start' (or the first <p> if that is nil)
;; in the generated HTML."
;;   :type '(choice (const :tag "First paragraph" nil) (string))
;;   :safe t)

;; (defcustom org-blog-preview-convert-titles t
;;   "When preview is enabled, convert <h1> to <h2> for the previews."
;;   :type '(boolean)
;;   :safe t)

;; (defcustom org-blog-preview-ellipsis "(...)"
;;   "The HTML appended to the preview if some part of the post is hidden.

;; The contents shown in the preview is determined by the values of
;; the variables `org-blog-preview-start' and
;; `org-blog-preview-end'."
;;   :type '(string)
;;   :safe t)

(defcustom org-blog-no-post-tag "nonpost"
  "Do not pushlish the subtree with this tag or property."
  :type '(string)
  :safe t)

;; (defcustom org-blog-preview-link-p nil
;;   "Whether to make the preview ellipsis a link to the article's page."
;;   :type '(boolean)
;;   :safe t)

;; (defcustom org-blog-preview-date-first-p nil
;;   "If t, print post dates before title in the preview view."
;;   :type '(boolean)
;;   :safe t)

(defun concat-to-dir (dir filename)
  "Concat filename to another path interpreted as a directory."
  (concat (file-name-as-directory dir) filename))

(defun sort-file-recent (it other)
  (time-less-p (f-modification-time it)
               (f-modification-time other)))

(defun org-blog-template (title content &optional preamble postamble desc)
  "Create the template that is used to generate the static pages."
  (concat
   "<!DOCTYPE html>\n"
   "<html lang=\"zh\">\n"
   "<head>\n"
   "<meta charset=\"UTF-8\">\n"
   (format "<meta name=\"description\" content=\"%s\">\n"
           (or desc
               org-blog-publish-description))
   ;; "<link rel=\"alternate\"\n"
   ;; "      type=\"application/rss+xml\"\n"
   ;; "      href=\"" (org-blog-get-absolute-url org-blog-rss-file) "\"\n"
   ;; "      title=\"RSS feed for " org-blog-publish-url "\">\n"
   "<title>" title "</title>\n"
   org-blog-page-head
   "</head>\n"
   "<body>\n"
   "<div class=\"header\">\n"
   (org-blog-header-template)
   "</div>\n"
   "<div class=\"main\">\n"
   "<div id=\"preamble\" class=\"status\">"
   preamble
   "</div>\n"
   "<div id=\"content\">\n"
   content
   "</div>\n"
   "<div id=\"postamble\" class=\"status\">"
   postamble
   "</div>\n"
   "</div>\n"
   "<div class=\"footer\">\n"
   (org-blog-footer-template)
   "</div>\n"
   "</body>\n"
   "</html>\n"))

(defun org-blog-header-template ()
  (concat "<div class=\"home-page\">\n"
          (format "<a href=\"/\">%s</a>" org-blog-publish-title)
          "</div>"
          "<div class=\"nav\">\n"
          "<div class=\"nav item\">\n"
          "<a href=\"/tags\">标签</a>\n"
          "</div>\n"
          "<div class=\"nav item\">\n"
          "<a href=\"/archive\">归档</a>\n"
          "</div>\n"
          "<div class=\"nav item\">\n"
          "<a href=\"/about\">关于</a>\n"
          "</div>\n"
          "</div>\n"))

(defun org-blog-footer-template ()
  (concat "<center> The End </center>"))

;;;###autoload
(defun org-blog-publish (&optional force-render)
  "Render all blog posts, the index, archive, tags, and RSS feed.
Only blog posts that changed since the HTML was created are
re-rendered.

With a prefix argument, all blog posts are re-rendered
unconditionally."
  (interactive "P")
  (--each (org-blog-get-post-filenames)
    (let* ((n-p (org-blog-needs-publishing-p it))
           (need-p (car n-p))
           (special (cdr n-p)))
      (when (or force-render
                special)
        (org-blog-publish-file it special))))
  (message "All posts published")
  (f-delete (f-expand "assets" org-blog-publish-directory) t)
  (f-delete (f-expand "static" org-blog-publish-directory) t)
  (f-copy (f-expand "assets" org-blog-directory) (f-slash org-blog-publish-directory))
  (f-copy (f-expand "static" org-blog-directory) (f-slash org-blog-publish-directory))
  (message "All static files copied.")
  (org-blog-assemble-index)
    ;; (org-blog-assemble-rss)
  (org-blog-assemble-archive)
  (org-blog-assemble-tags))

(defun org-blog-needs-publishing-p (post-filename)
  "Check whether POST-FILENAME was changed since last render.
Return non-nil if post needs to be published.

Specially, return 't' means this post is `special'."
  (let ((header (nth 1 (s-match "^\\#\\+publish:[ ]*\\(.+\\)$"
                                (f-read post-filename))))
        (pub-filename (org-blog-matching-publish-filename
                       post-filename)))
    
    (when header
      (cons (not (and (file-exists-p pub-filename)
                      (file-newer-than-file-p pub-filename
                                              post-filename)))
         (s-equals-p (s-trim header)
                     "special")))))

(defun org-blog-matching-publish-filename (post-filename)
  "Generate HTML file name for POST-FILENAME."
  (f-expand (org-blog-get-post-public-path post-filename)
            org-blog-publish-directory))

(defun org-blog-get-post-filenames (&optional special)
  "Returns a list of all posts."
  (let ((file-list (f-files org-blog-posts-directory
                            (lambda (path)
                              (f-ext-p path "org"))
                            t)))
    (if special
      (--filter
       (not (cdr (org-blog-needs-publishing-p it)))
       file-list)
      file-list)))

(defun org-blog-file-buffer (file)
  "Return the buffer open with a full filepath, or nil."
  (require 'seq)
  (make-directory (file-name-directory file) t)
  (car (seq-filter
         (lambda (buf)
           (string= (with-current-buffer buf buffer-file-name) file))
         (buffer-list))))

;; This macro is needed for many of the following functions.
(defmacro org-blog-with-find-file (file contents &rest body)
  "Executes BODY in FILE. Use this to insert text into FILE.
The buffer is disposed after the macro exits (unless it already
existed before)."
  `(save-excursion
     (let ((current-buffer (current-buffer))
           (buffer-exists (org-blog-file-buffer ,file))
           (result nil)
	       (contents ,contents))
       (if buffer-exists
           (switch-to-buffer buffer-exists)
         (find-file ,file))
       (erase-buffer)
       (insert contents)
       (setq result (progn ,@body))
       (basic-save-buffer)
       (unless buffer-exists
         (kill-buffer))
       (switch-to-buffer current-buffer)
       result)))

(defun org-blog-get-date (post-filename)
  "Extract the `#+date:` from POST-FILENAME as date-time."
  (let ((date-string (nth 1 (s-match "^\\#\\+date:[ ]*[<[]\\([^]>]+\\)[]>]"
                                     (f-read post-filename)))))
    (if date-string
        (ts-parse-org date-string)
      (ts-now))))

(defun org-blog-get-title (post-filename)
  "Extract the `#+title:` from POST-FILENAME."
  (nth 1 (s-match "^\\#\\+title:[ ]*\\(.+\\)$"
                  (f-read post-filename))))

(defun org-blog-get-description (post-filename)
  "Extract the `#+description:` from POST-FILENAME."
  (nth 1 (s-match "^\\#\\+description:[ ]*\\(.+\\)$"
                  (f-read post-filename))))

(defun org-blog-get-tags (post-filename)
  "Extract the `#+filetags:` from POST-FILENAME as list of strings."
  (let ((tags-string-colon (nth 1 (s-match "^\\#\\+filetags:[ ]*:\\(.*\\):$"
                                           (f-read post-filename)))))
    (if tags-string-colon
        (s-split ":" tags-string-colon t)
      (s-split " " (nth 1 (s-match "^\\#\\+filetags:[ ]*\\(.+\\)$"
                                   (f-read post-filename)))))))

;; TODO
(defun org-blog-get-headlines (content)
  "Extract the all the headlines from HTML CONTENT

Return a list of HEADLINES, which consist of a list
(LEVEL TITLE ID-ANCHOR).
"
  (let ((now-level 0))
    (--map
     (list (string-to-number (nth 1 it))
           (nth 3 it)
           (nth 2 it))
     (s-match-strings-all
      "<h\\([0-9]\\)[^>]*id=\"\\([^\"]+\\)\"[^>]*>\\([^<]+\\)</h[0-9]>"
      content))))

(defun org-blog-build-toc (headlines)
  "Return innards of a table of contents, as a string.
HEADLINES a list containing headlines by order.
Each headline entry is list, whose first element is LEVEL,
second elemnt is TITLE, and third is ID-ANCHOR"
  (let* ((prev-level (1- (caar headlines)))
	     (start-level prev-level))
    (concat
     "<div class=\"toc\">\n"
     "<h2 class=\"toc-title\"> TOC </h2>\n"
     "<nav id=\"TableOfContents\">\n"
    (apply
     'concat
     (-snoc
      (--map
	   (seq-let (level title id) it
	     (concat
	      (let* ((cnt (- level prev-level))
		         (times (if (> cnt 0) (1- cnt) (- cnt))))
	        (setq prev-level level)
	        (concat
             (s-repeat times
	                   (cond ((> cnt 0) "\n<ul>\n<li>")
			                 ((< cnt 0) "</li>\n</ul>\n")))
	         (if (> cnt 0) "\n<ul>\n<li>" "</li>\n<li>")))
          (format "\n<a href=\"#%s\"> %s </a>" id title)))
       headlines)
     (s-repeat (- prev-level start-level) "</li>\n</ul>\n")))
    "</nav>\n</div>\n")))

(defun org-blog-get-tag-tree ()
  "Return an association list of tags to filenames.
e.g. `(('foo' 'file1.org' 'file2.org') ('bar' 'file2.org'))`"
  (let ((tag-tree '()))
    (dolist (post-filename (org-blog-get-post-filenames t))
      (let ((tags (org-blog-get-tags post-filename)))
        (dolist (tag (remove org-blog-rss-excluded-tag tags))
          (if (assoc-string tag tag-tree t)
              (push post-filename (cdr (assoc-string tag tag-tree t)))
            (push (cons tag (list post-filename)) tag-tree)))))
    tag-tree))

;; TODO
(defun org-blog--preview-region ()
  "Find the start and end of the preview in the current buffer."
  (goto-char (point-min))
  (when (search-forward "<p>" nil t)
    (let ((start (match-beginning 0)))
      (search-forward "</p>")
      (buffer-substring-no-properties start (point))))
  "")

(defun org-blog-get-post-content (post-filename &optional exclude-title)
  "Get the rendered HTML body without headers from POST-FILENAME.
Preamble and Postamble are excluded, too."
  ;; NB! The following code assumes the post is using default template.
  ;; See: org-blog-publish-file
  (with-temp-buffer
    (insert-file-contents (org-blog-matching-publish-filename post-filename))
    (buffer-substring-no-properties
     (progn
       (goto-char (point-min))
       (if exclude-title
           (progn (search-forward "<h1 class=\"post-title\">")
                  (search-forward "</h1>"))
         (search-forward "<div id=\"content\">"))
       (point))
     (progn
       (goto-char (point-max))
       ;; Search backward for the post content (by org-blog-render-post-content).
       ;; See: org-blog-template
       (search-backward "<div id=\"postamble\" class=\"status\">")
       (search-backward "</div>")
       ;; If comments section exists, it is then one div backward.
       ;; See: org-blog-post-postamble
       (search-backward "<div id=\"comments\">" nil t)
       (point)))))

(defun org-blog-get-absolute-url (relative-url)
  "Returns absolute URL based on the RELATIVE-URL passed to the function.

For example, when `org-blog-publish-url` is set to 'https://example.com/'
and `relative-url` is passed as 'archive.html' then the function
will return 'https://example.com/archive.html'."
  (concat-to-dir org-blog-publish-url relative-url))

(defun org-blog-get-post-relative-url (post-filename)
  "Returns absolute URL to the published POST-FILENAME.

This function concatenates publish URL and generated custom filepath to the
published HTML version of the post."
  (concat "/"
          (f-relative
           (f-dirname (org-blog-get-post-public-path post-filename))
           org-blog-publish-directory)))

(defun org-blog-get-post-public-path (post-filename)
  "Returns post relative filepath in public directory."
  (f-expand "index.html"
            (f-expand
             (f-no-ext (f-relative post-filename
                                   org-blog-posts-directory))
             org-blog-publish-directory)))

(defun org-blog-get-relative-path (post-filename)
  "Removes absolute directory path from POST-FILENAME and changes file extention
from `.org` to `.html`. Returns filepath to HTML file relative to posts or drafts directories.

Works with both posts and drafts directories.

For example, when `org-blog-posts-directory` is set to '~/blog/posts'
and `post-filename` is passed as '~/blog/posts/my-life-update.org' then the function
will return 'my-life-update.html'."
  (concat (file-name-sans-extension (file-relative-name post-filename org-blog-posts-directory))
	  ".html"))

(defun org-blog-generate-post-path (post-filename post-datetime)
  "Returns post public path based on POST-FILENAME and POST-DATETIME.

By default, this function returns post filepath unmodified, so script will
replicate file and directory structure of posts and drafts directories.

Override this function if you want to generate custom post URLs different
from how they are stored in posts and drafts directories.

For example, there is a post in posts directory with the
file path `hobby/charity-coding.org` and dated `<2019-08-20 Tue>`.

In this case, the function will receive following argument values:
- post-filename: 'hobby/charity-coding'
- post-datetime: datetime of <2019-08-20 Tue>

and by default will return 'hobby/charity-coding', so that the path
to HTML file in publish directory will be 'hobby/charity-coding.html'.

If this function is overriden with something like this:

(defun org-blog-generate-post-path (post-filename post-datetime)
  (concat (format-time-string \"%Y/%m/%d\" post-datetime)
          \"/\"
          (file-name-nondirectory post-filename)))

Then the output will be '2019/08/20/charity-coding' and this will be
the path to the HTML file in publish directory and the url for the post."
  post-filename)

;;;###autoload
(defun org-blog-publish-file (post-filename &optional special)
  "Publish a single POST-FILENAME.
The index, archive, tags, and RSS feed are not updated."
  (interactive "f")
  (message "Publishing: %s" post-filename)
  (let* ((content (org-blog-render-post-content post-filename))
         (toc (unless special
                (org-blog-build-toc
                 (org-blog-get-headlines content)))))
    (message "Content generated.")
    (org-blog-with-find-file
     (org-blog-matching-publish-filename post-filename)
     (org-blog-template
      (org-blog-get-title post-filename)
      content
      (org-blog-post-preamble post-filename toc)
      (org-blog-post-postamble post-filename)
      (org-blog-get-description post-filename)))))


(defun org-blog-render-post-content (post-filename)
  "Render blog content as bare HTML without header."
  (let ((org-html-doctype "html5")
        (org-html-html5-fancy t))
    (save-excursion
      (let ((current-buffer (current-buffer))
            (buffer-exists (org-blog-file-buffer post-filename))
            (result nil))
        (with-temp-buffer
          (if buffer-exists
              (insert-buffer-substring buffer-exists)
            (insert-file-contents post-filename))
          (org-mode)
          (goto-char (point-min))
          (org-map-entries
           (lambda ()
             (setq org-map-continue-from (point))
             (org-cut-subtree))
           org-blog-no-post-tag)
          (setq result
                (org-export-as 'org-blog-post-bare nil nil nil nil))
          (switch-to-buffer current-buffer)
          result)))))

(org-export-define-derived-backend 'org-blog-post-bare 'html
  :translate-alist '((template . (lambda (contents info) contents))
                     (footnote-reference . org-blog-html-footnote-reference)
                     (src-block . org-blog-html-src-block)
                     (quote-block . org-blog-html-quote-block)))

(defun org-blog-assemble-index ()
  "Assemble the blog index page.
The index page contains the last `org-blog-index-length`
posts as full text posts."
  (org-blog-assemble-multipost-page
   (f-expand org-blog-index-file org-blog-publish-directory)
   (last (-sort #'sort-file-recent
                (org-blog-get-post-filenames t))
         org-blog-index-length)
   org-blog-index-front-matter
   nil
   t))

(defun org-blog-assemble-multipost-page (pub-filename post-filenames &optional preamble postamble preview)
  "Assemble a page that contains multiple posts one after another.
Posts are sorted in descending time."
  (org-blog-with-find-file
   pub-filename
   (org-blog-template
    org-blog-publish-title
    (mapconcat
     (if preview
         'org-blog-get-preview
       'org-blog-get-post-summary)
     (-sort #'sort-file-recent
            post-filenames)
     "")
     preamble
     postamble)))

(defun org-blog-post-preamble (post-filename &optional toc)
  "Returns the formatted date and headline of the post.
This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline.

If TOC is nil, this post is `special',
in which date and tags won't be shown."
  (concat
   (mapconcat
    (lambda (it)
      (car (funcall (intern (concat "org-blog-config-"
                                    (symbol-name it))))))
    org-blog-extras
    "\n")
   toc
   "<h1 class=\"post-title\"> "
   (org-blog-get-title post-filename)
   " </h1>\n"
   (when toc
     (concat
      (ts-format
       (concat "<div class=\"post-date\">"
               org-blog-date-format
               "</div>\n")
       (org-blog-get-date post-filename))
      
      "<div class=\"taglist\">"
      (org-blog-post-taglist post-filename)
      "\n</div>\n"))))

(defun org-blog-post-taglist (post-filename)
  "Returns the tag list of the post.
This part will be attached at the end of the post, after
the taglist, in a <div id=\"taglist\">...</div> block."
  (let ((taglist-content "")
        (tags (-difference (org-blog-get-tags post-filename)
                           org-blog-rss-excluded-tag)))
    (when tags
      (mapconcat
       (lambda (tag)
         (format "<a href=\"%s\"> #%s </a>"
                 (concat "/tag-"
                         (downcase tag)
                         "/")
                 tag))
       tags "\n"))))

(defun org-blog-post-postamble (post-filename)
  "Returns the tag list and comment box at the end of a post.
This function is called for every post and the returned string is
appended to the post body, and includes the tag list generated by
followed by the HTML code for comments."
  (let ((cf "")
        (scripts ""))
    (--each org-blog-extras
      (let ((name (symbol-name it)))
        (seq-let (script config) 
            (cdr (funcall (intern (concat "org-blog-config-"
                                          name))))
          (setq cf (concat cf config ", ")
                scripts (concat scripts script "\n")))))
    (concat scripts
            "<script>\nwindow.config={"
            cf
            "}\n</script>\n"
            "<script src=\"/static/style/style.js\"></script>\n")))

(defun org-blog--prune-items (items)
  "Limit, if needed, the items to be included in an RSS feed."
  (if (and org-blog-rss-max-entries (> org-blog-rss-max-entries 0))
      (let ((excess (- (length items) org-blog-rss-max-entries)))
        (if (> excess 0) (butlast items excess) items))
    items))

(defun org-blog--rss-filename (&optional tag)
  "Full path to the RSS file for the given TAG."
  (concat-to-dir org-blog-publish-directory
                 (concat tag (when tag "-") org-blog-rss-file)))

(defun org-blog--write-rss (items &optional tag)
  "Generates an RSS file for the given TAG, or for all tags is TAG is nil."
  (let ((title (format "%s%s"
                       org-blog-publish-title
                       (if tag (concat " - " tag) "")))
        (url (format "%s%s"
                     org-blog-publish-url
                     (if tag (concat "/tag-" (downcase tag) ".html") "")))
        (items (sort items (lambda (x y) (time-less-p (car y) (car x))))))
    (org-blog-with-find-file
     (org-blog--rss-filename tag)
     (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	     "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
	     "<channel>\n"
	     "<title><![CDATA[" title "]]></title>\n"
	     "<description><![CDATA[" title "]]></description>\n"
	     "<link>" url "</link>\n"
	     "<lastBuildDate>" (let ((system-time-locale "C")) ; force dates to render as per RSS spec
				 (format-time-string "%a, %d %b %Y %H:%M:%S %z" (current-time)))
             "</lastBuildDate>\n"
             org-blog-rss-extra
	     (apply 'concat (mapcar 'cdr (org-blog--prune-items items)))
	     "</channel>\n"
	     "</rss>\n"))))

(defun org-blog-assemble-rss ()
  "Assemble the blog RSS feed.
The RSS-feed is an XML file that contains every blog post in a
machine-readable format."
  (let ((system-time-locale "en_US.utf-8") ; force dates to render as per RSS spec
        (rss-items nil)
        (rss-tag-items nil))
    (dolist (post-filename (org-blog-get-post-filenames t))
      (let ((rss-date (org-blog-get-date post-filename))
            (rss-text (org-blog-get-rss-item post-filename))
            (tags (org-blog-get-tags post-filename)))
        (when (or (null org-blog-rss-excluded-tag)
                  (not (member org-blog-rss-excluded-tag tags)))
          (let ((item (cons rss-date rss-text)))
            (add-to-list 'rss-items item)
            (when org-blog-enable-tag-rss
              (dolist (tag tags)
                (let ((items (cons item (cdr (assoc tag rss-tag-items)))))
                  (setf (alist-get tag rss-tag-items nil t 'string=) items))))))))
    (org-blog--write-rss rss-items)
    (message "%s" rss-tag-items)
    (dolist (x rss-tag-items) (org-blog--write-rss (cdr x) (car x)))))

(defun org-blog-get-rss-item (post-filename)
  "Assemble RSS item from post-filename.
The HTML content is taken from the rendered HTML post."
  (concat
   "<item>\n"
   "  <title><![CDATA[" (org-blog-get-title post-filename) "]]></title>\n"
   "  <description><![CDATA["
   (org-blog-get-post-content post-filename t) ; exclude headline!
   "]]></description>\n"
   (let ((categories ""))
     (when (and (org-blog-get-tags post-filename) org-blog-enable-tags)
       (dolist (tag (org-blog-get-tags post-filename))
         (setq categories (concat categories
                                  "  <category><![CDATA[" tag "]]></category>\n"))))
     categories)
   "  <link>"
   (url-encode-url (org-blog-get-post-relative-url post-filename))
   "</link>\n"
   "  <guid>"
   (url-encode-url (org-blog-get-post-relative-url post-filename))
   "</guid>\n"
   "  <pubDate>"
   (let ((system-time-locale "C")) ; force dates to render as per RSS spec
     (format-time-string "%a, %d %b %Y %H:%M:%S %z" (org-blog-get-date post-filename)))
   "</pubDate>\n"
   "</item>\n"))

(defun org-blog-assemble-archive ()
  "Re-render the blog archive page.
The archive page contains single-line links and dates for every
blog post, but no post body."
  (let ((archive-filename (f-expand org-blog-archive-file
                                    org-blog-publish-directory))
        (archive-entries nil)
        (post-filenames (-sort #'sort-file-recent
                               (org-blog-get-post-filenames t))))
    (org-blog-with-find-file
     archive-filename
     (org-blog-template
      (format "ARCHIVEs - %s" org-blog-publish-title)
      (mapconcat
       'org-blog-get-post-summary
       post-filenames
       "")
      (concat "<h1 class=\"post-title\"> ARCHIVEs </h1>")))))

(defun org-blog-get-post-summary (post-filename &optional with-tags)
  "Assemble post summary for an archive page.
This function is called for every post on the archive and
tags-archive page. Modify this function if you want to change an
archive headline."
  (concat
   "<div class=\"summary-item\">"
   "<div class=\"summary-title\">"
   (format "<a href=\"%s\"> %s </a>"
           (org-blog-get-post-relative-url post-filename)
           (org-blog-get-title post-filename))
   "</div>\n"
   "<div class=\"summary-date\">"
   (ts-format org-blog-date-format
              (org-blog-get-date post-filename))
   "</div>"
   
   "</div>\n"))

(defun org-blog-assemble-tags-archive-tag (tag)
  "Assemble single TAG for all filenames."
  (let ((tag-name (car tag))
        (post-filenames (cdr tag)))
    (format "<a class=\"tag-item\" href=\"%s\"> %s<sup>%s </sup></a>\n"
            (concat "/tag-" (downcase tag-name))
            tag-name
            (length post-filenames))))

(defun org-blog-assemble-tags-archive ()
  "Assemble the blog tag archive page.
The archive page contains single-line links and dates for every
blog post, sorted by tags, but no post body."
  (message "Assembling TAGs Page")
  (let ((tags-archive-filename (f-expand org-blog-tags-file
                                         org-blog-publish-directory))
        (tag-tree (org-blog-get-tag-tree)))
    ;; (setq tag-tree (sort tag-tree (lambda (x y) (string-greaterp (car y) (car x)))))
    (org-blog-with-find-file
     tags-archive-filename
     (org-blog-template
      (format "TAGs - %s" org-blog-publish-title)
      (mapconcat
       'org-blog-assemble-tags-archive-tag
       tag-tree "")
      "<h1 class=\"post-title\"> TAGs </h1>\n"))))

(defun org-blog-get-preview (post-filename)
  "Get title, date, tags from POST-FILENAME and get the first paragraph from the rendered HTML."
  (let ((post-title (org-blog-get-title post-filename))
        (post-date (org-blog-get-date post-filename))
        (post-taglist (org-blog-post-taglist post-filename))
        (preview-region
         (with-temp-buffer
           (insert-file-contents (org-blog-matching-publish-filename
                                  post-filename))
           (org-blog--preview-region))))
    ;; Put the substrings together.
    (concat
     "<div class=\"post-item\">\n"
     (format "<h2 class=\"post-title\"><a href=\"%s\"> %s </a></h2>"
             (org-blog-get-post-relative-url post-filename)
             post-title)
     preview-region
     (format "<div class=\"post-info\">\n%s\n%s\n</div>\n"
             (format "<div class=\"taglist\">%s</div>\n" post-taglist)
             (ts-format (concat "<div class=\"post-date\">"
                                org-blog-date-format
                                "</div>")
                        post-date))
     "</div>\n")))

(defun org-blog-assemble-tags ()
  "Render the tag archive and tag pages."
  (org-blog-assemble-tags-archive)

  (--each (org-blog-get-tag-tree)
    (org-blog-assemble-multipost-page
     (f-expand (concat "tag-" (downcase (car it)) "/index.html")
               org-blog-publish-directory)
     (cdr it)
     (concat "<h1 class=\"post-title\"> TAG: #" (car it) " </h1>"))))

(setq org-html-footnotes-section
      "<div id=\"footnotes\">
<hr>
<div class=\"text-footnotes\" role=\"doc-endnotes\">
<ol>
%s
</ol>
</div>
</div>\n"

      org-html-footnote-format "<sup>[%s]</sup>")

(defun org-blog-html-footnote-reference (footnote-reference contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
          (fkey (intern (format ":fn-%d" n)))
          (num (plist-get info fkey))
          (id (if (numberp num) (1+ num) 1)))
     (plist-put info fkey id)
     (format
      (plist-get info :html-footnote-format)
      (org-html--anchor
       (format "fnr:%d.%d" n id)
       n
       (format " class=\"footref\" href=\"#fn:%d\"" n) info)))))

(defun org-html-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let ((definitions (org-export-collect-footnote-definitions info)))
    (when definitions)
    (format
     (plist-get info :html-footnotes-section)
	 (mapconcat
	  (lambda (definition)
	    (pcase definition
	      (`(,n ,_ ,def)
	       (let* ((refs (plist-get info (intern (format ":fn-%d" n))))				  
		          (anchor "")
		         (contents (org-trim (org-export-data def info))))
             (when (numberp refs)
               (--dotimes refs
                 (setq anchor
                       (concat anchor
                               (format "<a href=\"#fnr:%d.%d\" class=\"footnote-backref\" role=\"doc-backlink\">↩︎</a>\n"
                                       n
                                       (1+ it))))))
		     (concat (format "<li id=\"fn:%d\">\n" n)
                     (substring contents 0 -4)
                     anchor
                     "</p>\n</li>")))))
	  definitions
	  "\n"))))

(defun org-blog-html-src-block (src-block _contents info)
  "SRC-BLOCK, for integrating with highlight.js"
  (let* ((lang (org-element-property :language src-block))
	     (code (org-html-format-code src-block info))
         (lbl (org-html--reference src-block info t))
	     (label (if lbl (format " id=\"%s\"" lbl) "")))
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
            (pcase lang
              ("mermaid"
               (add-to-list 'org-blog-extras 'mermaid)
               (format "<pre class=\"mermaid\"%s> %s </pre>"
                       label
                       code))

              (otherwise
               (add-to-list 'org-blog-extras 'code)
               (format "<pre><code class=\"language-%s\"%s%s> %s </code></pre>"
                       (or lang "plaintext")
                       label
                       (if (string= lang "html")
				           " data-editor-type=\"html\""
			             "")
                       code))))))


(defun org-blog-html-quote-block (quote-block contents info)
  "Add author."
  (setq string contents)
  (format "<blockquote%s>\n%s</blockquote>"
	  (let* ((reference (org-html--reference quote-block info t))
		     (attributes (org-export-read-attribute :attr_html quote-block))
		     (a (org-html--make-attribute-string
		         (if (or (not reference)
                         (plist-member attributes :id))
			         attributes
		           (plist-put attributes :id reference)))))
	    (if (org-string-nw-p a)
            (concat " " a) ""))
      (let ((p-list nil)
            (source nil))
        (with-temp-buffer
          (insert "\n"
                  contents)
          (beginning-of-buffer)
          (while (search-forward "<p>" nil t 1)
            (let ((start-point (point)))
              (search-forward "</p>" nil t 1)
              (backward-char 4)
              (push (s-trim (buffer-substring-no-properties start-point (point)))
                    p-list))))
        (when (s-starts-with-p "&#x2014;" (car p-list))
          (setq source (s-trim (s-replace "&#x2014;" "" (car p-list)))))
        (concat
         (mapconcat
          (lambda (it)
            (format "<p> %s </p>" it))
          (reverse (cdr p-list)) "\n")
         (if source
             (format "<p class=\"quote-source\"> - %s </p>\n"
                     source)
           (format "<p> %s </p>" (car p-list)))))))
        

;;; 3rd-party
(defvar org-blog-extras nil)

(defvar org-blog-current-org nil)

(defun org-blog-set-before (&rest args)
  (setq org-blog-extras (car args))
  (setq org-blog-extras nil))

(defun org-blog-set-after (&rest args)
  (setq org-blog-extras nil)
  (setq org-blog-extras nil))

(defun org-blog-config-math ()
  (list
   "<link rel=\"stylesheet\" href=\"/static/katex/katex.min.css\">"
   (concat "<script defer src=\"/static/katex/katex.min.js\"></script>\n"
           "<script defer src=\"/static/katex/auto-render.min.js\"></script>\n")
   "\"math\": {\"delimiters\": [
{\"display\": true,\"left\": \"$$\",\"right\": \"$$\"},
{\"display\": true,\"left\": \"\\\\[\",\"right\": \"\\\\]\"},
{\"display\": true,\"left\": \"\\\\begin{equation}\",\"right\": \"\\\\end{equation}\"},
{\"display\": true,\"left\": \"\\\\begin{equation*}\",\"right\": \"\\\\end{equation*}\"},
{\"display\": true,\"left\": \"\\\\begin{align}\",\"right\": \"\\\\end{align}\"},
{\"display\": true,\"left\": \"\\\\begin{align*}\",\"right\": \"\\\\end{align*}\"},
{\"display\": true,\"left\": \"\\\\begin{alignat}\",\"right\": \"\\\\end{alignat}\"},
{\"display\": true,\"left\": \"\\\\begin{alignat*}\",\"right\": \"\\\\end{alignat*}\"},
{\"display\": true,\"left\": \"\\\\begin{gather}\",\"right\": \"\\\\end{gather}\"},
{\"display\": true,\"left\": \"\\\\begin{CD}\",\"right\": \"\\\\end{CD}\"},
{\"display\": false,\"left\": \"$\",\"right\": \"$\"},
{\"display\": false,\"left\": \"\\\\(\",\"right\": \"\\\\)\"}
],
\"macros\": {
\"\\\\C\": \"\\\\mathbb{C}\",
\"\\\\N\": \"\\\\mathbb{N}\",
\"\\\\Q\": \"\\\\mathbb{Q}\",
\"\\\\R\": \"\\\\mathbb{R}\",
\"\\\\Z\": \"\\\\mathbb{Z}\",
\"’\": \"'\"
},
\"strict\": false
}"))


(defun org-blog-config-code ()
  (list
   "<link rel=\"stylesheet\" href=\"/static/highlight/dracula.css\">"
   "<script src=\"/static/highlight/highlight.min.js\"></script>\n"
   "\"code\": {\"header\": false,
\"exclude\": [\"mermaid\"]}"))

(defun org-blog-config-mermaid ()
  (list
   ""
   "<script src=\"/static/mermaid/mermaid.min.js\"></script>\n"
   "\"mermaid\": true"))

;; Tempororally
(advice-add 'org-html-format-latex
            :before
            (lambda (latex-frag processing-type info)
              (when (eq processing-type 'mathjax)
                (add-to-list 'org-blog-extras 'math))))

(advice-add 'org-blog-publish-file :before #'org-blog-set-before)
(advice-add 'org-blog-publish-file :after #'org-blog-set-after)

(advice-add 'org-html--format-image
            :filter-args
            (lambda (args)
              (cons (f-expand (car args) "/assets")
                    (cdr args))))

(advice-add 'org-html-link
            :filter-return
            (lambda (link)
              (let* ((href (nth 1
                                (s-match "<a.*href=\"\\(.*.org\\)\"[^>]*>"
                                         link)))
                     (root (if (s-starts-with-p ".." href)
                               (nth 1 (s-match "^\\(../\\)+" href))
                             "/"))
                     (url-p (--any-p
                             (s-starts-with-p it href)
                             '("http" "#"))))
                (when (and href
                           (not url-p))

                  (setq link
                        (s-replace href
                                   (f-expand (substring href 0 -4)
                                             root)
                                   link)))
                link)))

(setq org-html-link-org-files-as-html nil)

(provide 'org-blog)
;;; osb-blog.el ends
