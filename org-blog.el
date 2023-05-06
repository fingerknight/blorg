;;; org-blog.el -*- lexical-binding: t; -*-
;;; A simple org-mode based static blog generator 
;;; An fork from: https://github.com/bastibe/org-static-blog
;;; In order to integrate with `denote.el'
;; Author: Bastian Bechtold
;; Contrib: Shmavon Gazanchyan, Rafał -rsm- Marek, neeasade,
;; Michael Cardell Widerkrantz, Matthew Bauer, Winny, Yauhen Makei,
;; luhuaei, zngguvnf, Qiantan Hong, Jonas Bernoulli, Théo Jacquin,
;; K. Scarlet, zsxh
;; URL: https://github.com/bastibe/org-blog
;; Version: 1.6.0
;; Package-Requires: ((emacs "24.3"))

;;; Fork
;; Author: Finger Knight
;; version: 0.0.1
;; URL:
;; Package-Requires: ((emacs "28.2") (f.el) (dash.el) (s.el) (ts.el) (denote.el))

;;; Commentary:

;;; Personal fork of org-static-blog

;;; Code:

(require 'dash)
(require 'f)
(require 's)
(require 'ts)
(require 'org)
(require 'ox-html)
(require 'denote)

(defgroup org-blog nil
  "Settings for a static blog generator using org-mode"
  :version "1.6.0"
  :group 'applications)

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

(defcustom org-blog-static-directory "~/blog/static/"
  "Directory where static resoureces are stored, like css, js.
They will be copy to the same directory in publish directory."
  :type '(directory))

(defcustom org-blog-assets-directory "~/blog/assets/"
  "Directory where assets are stored, which are attachment
in org file

They will be copy to the same directory in publish directory."
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

(defcustom org-blog-no-post-tag "nonpost"
  "Do not pushlish the subtree with this tag or property."
  :type '(string)
  :safe t)

(defvar org-blog-post-filenames nil
  "A list contains all the posts' abs filename.

Set the value before `org-blog-publish',
then turn it to `nil' after finished.")

(defvar org-blog-post-filenames-without-special nil
  "A list contains all the posts' abs filename,
whose value of `publish' is not \"special\".")

(defvar org-blog-extras nil)

(defvar org-blog-math-num nil
  "A list with two elements:
One is the chapter number, which is a string.
Another is the item number, which is a integer")

(defun sort-file-recent (it other)
  (ts> (org-blog-get-date it)
       (org-blog-get-date other)))

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


(defun org-blog-get-post-filenames ()
  "Return a list of all posts with non-nil `publish' header."
  (--filter
   (-intersection '("blog" "blogsp")
                  (org-blog-get-tags it t))
   (f-files org-blog-posts-directory 'denote-file-is-note-p t)))

(defun org-blog-needs-publish-p (post-filename)
  "Check whether POST-FILENAME needs to publish."
  (let ((pub-filename (org-blog-get-post-public-path post-filename)))
    (or (not (f-exists-p pub-filename))
        (file-newer-than-file-p post-filename
                                pub-filename))))

(defun org-blog-is-special-p (post-filename)
  "Check whether POST-FILENAME is SPECIAL."
  (member "blogsp" (org-blog-get-tags post-filename t)))

(defun org-blog-matching-publish-filename (post-filename)
  "Generate HTML file name for POST-FILENAME."
  (f-expand (org-blog-get-post-public-path post-filename)
            org-blog-publish-directory))

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

(defun org-blog-get-id (post-filename)
  "Extract the `#+identifier:` from POST-FILENAME."
  (if (org-blog-is-special-p post-filename)
      (s-trim (nth 1 (s-match "^\\#\\+identifier:[ ]*\\(.+\\)$"
                              (f-read post-filename))))
    (denote-retrieve-filename-identifier (f-filename post-filename))))

(defun org-blog-get-description (post-filename)
  "Extract the `#+description:` from POST-FILENAME."
  (nth 1 (s-match "^\\#\\+description:[ ]*\\(.+\\)$"
                  (f-read post-filename))))

(defun org-blog-get-tags (post-filename &optional all)
  "Extract tags from POST-FILENAME's path
If ALL is `nil', then \"blog\" and \"blogsp\" are excluded."
  (-difference (denote-extract-keywords-from-path post-filename)
               (unless all '("blog" "blogsp"))))

(defun org-blog-get-math-chapter (post-filename)
  "Get chapter number for mathametics."
  (or
   (car (s-match "\\([0-9]+\.\\)+"
                 (org-blog-get-title post-filename)))
   ""))

(defun org-blog-build-toc (content)
  "Build TOC by HTML CONTENT"
  (let* ((headlines
          (--map
           (list (string-to-number (nth 1 it))
                 (nth 3 it)
                 (nth 2 it))
           (s-match-strings-all
            "<h\\([0-9]\\)[^>]*id=\"\\([^\"]+\\)\"[^>]*>\\([^<]+\\)</h[0-9]>"
            content)))
         (prev-level (if headlines
                       (1- (caar headlines))
                       0))
	     (start-level prev-level))
    (concat
     "<div class=\"toc\">\n"
     (if headlines
         (concat
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
          "</nav>\n")
       "\n")
    "</div>\n")))

(defun org-blog-get-tag-tree ()
  "Return an association list of tags to filenames.
e.g. `(('foo' 'file1.org' 'file2.org') ('bar' 'file2.org'))`"
  (let ((tag-tree '()))
    (dolist (post-filename org-blog-post-filenames-without-special)
      (dolist (tag (-difference (org-blog-get-tags post-filename)
                                org-blog-rss-excluded-tag))
        (if (assoc-string tag tag-tree t)
            (push post-filename (cdr (assoc-string tag tag-tree t)))
          (push (cons tag (list post-filename)) tag-tree))))
    tag-tree))

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
  (f-join org-blog-publish-directory
          (if (org-blog-is-special-p post-filename)
              ""
            "post")
          (org-blog-get-id post-filename)
          "index.html"))

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
                (org-export-as 'org-blog-post-backend nil nil nil nil))
          (switch-to-buffer current-buffer)
          result)))))

(defun org-blog-assemble-index ()
  "Assemble the blog index page.
The index page contains the last `org-blog-index-length`
posts as full text posts."
  (org-blog-assemble-multipost-page
   (f-expand org-blog-index-file org-blog-publish-directory)
   (-slice (-sort #'sort-file-recent
                  org-blog-post-filenames-without-special)
           0
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
    (mapconcat (if preview 'org-blog-get-preview 'org-blog-get-post-summary)
               (-sort #'sort-file-recent post-filenames)
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
      (car (assoc-default it org-blog-third-party)))
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
                 (concat "/tag/" (downcase tag))
                 tag))
       tags "\n"))))

(defun org-blog-post-postamble (post-filename)
  "Returns the tag list and comment box at the end of a post.
This function is called for every post and the returned string is
appended to the post body, and includes the tag list generated by
followed by the HTML code for comments."
  (mapconcat
    (lambda (it)
      (cdr (assoc-default it org-blog-third-party)))
    org-blog-extras
    "\n"))

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
  (org-blog-with-find-file
   (f-expand org-blog-archive-file org-blog-publish-directory)
   (org-blog-template
    (format "ARCHIVEs - %s" org-blog-publish-title)
    (mapconcat 'org-blog-get-post-summary
               (-sort #'sort-file-recent
                      org-blog-post-filenames-without-special)
               "")
    (concat "<h1 class=\"post-title\"> ARCHIVEs </h1>"))))

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
            (concat "/tag/" (downcase tag-name))
            tag-name
            (length post-filenames))))

(defun org-blog-assemble-tags-archive ()
  "Assemble the blog tag archive page.
The archive page contains single-line links and dates for every
blog post, sorted by tags, but no post body."
  (message "Assembling TAGs Page")
  (org-blog-with-find-file
   (f-expand org-blog-tags-file org-blog-publish-directory)
   (org-blog-template
    (format "TAGs - %s" org-blog-publish-title)
    (mapconcat
     'org-blog-assemble-tags-archive-tag
     (--sort (s-less-p (car it) (car other))
             (org-blog-get-tag-tree))
     "")
    "<h1 class=\"post-title\"> TAGs </h1>\n")))

(defun org-blog-get-preview (post-filename)
  "Get title, date, tags from POST-FILENAME and get the first paragraph from the rendered HTML."
  (let ((post-title (org-blog-get-title post-filename))
        (post-date (org-blog-get-date post-filename))
        (post-taglist (org-blog-post-taglist post-filename))
        (preview-region
         (with-temp-buffer
           (insert-file-contents (org-blog-get-post-public-path post-filename))
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
     (f-join org-blog-publish-directory
             "tag"
             (downcase (car it))
             "index.html")
     (cdr it)
     (concat "<h1 class=\"post-title\"> TAG: #" (car it) " </h1>"))))

(defun org-blog-copy-directory (string)
      (f-delete (f-expand (f-base string)
                          org-blog-publish-directory)
                t)
      (f-copy (f-expand string org-blog-posts-directory)
              org-blog-publish-directory))

;;; template
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

(defun org-blog-html-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((block-type (org-element-property :type special-block))
         (name (org-element-property :name special-block))
         (html5-fancy (and (org-html--html5-fancy-p info)
                           (member block-type org-html-html5-elements)))
         (attributes (org-export-read-attribute :attr_html special-block))
         (math-block (car (member block-type '("definition"
                                               "proposition"
                                               "lemma"
                                               "theorem"
                                               "proof"
                                               "axiom"
                                               "remark"
                                               "solution"))))
         (num (if (and math-block
                       (not (string= math-block "proof")))
                  (progn
                    (setcdr org-blog-math-num
                            (list (1+ (cadr org-blog-math-num))))
                    (format " %s%d"
                                 (car org-blog-math-num)
                                 (cadr org-blog-math-num)))
                "")))
    
    (unless html5-fancy
      (let ((class (plist-get attributes :class)))
        (setq attributes (plist-put attributes :class
                                    (if class (concat class " " block-type)
                                      block-type)))))
    (when math-block
      (setq attributes (plist-put attributes
                                  :class
                                  (format "mathblock %s" math-block))))
    (let* ((contents (if contents
                         (if math-block
                             (let* ((pattern "<\\([^ >]*\\)[^>]*>")
                                    (tag-name (cadr (s-match pattern
                                                             contents)))
                                    (start-pos (car (s-matched-positions-all pattern
                                                                             contents))))
                               (if (string= tag-name "p")
                                   (concat (substring contents 0 (cdr start-pos))
                                           "<span class=\"mathblock title\">"
                                           (s-capitalize math-block) num
                                           (if name
                                               (format "<span class=\"mathblock name\"> (%s)</span>"
                                                       name))
                                           ". </span>\n"
                                           (substring contents (cdr start-pos)))
                                 (concat "<span class=\"mathblock title\">"
                                         (s-capitalize math-block) num
                                         (if name
                                             (format "<span class=\"mathblock name\"> (%s)</span>"
                                                     name))
                                         ". </span>\n"
                                         contents)))
                             contents)
                       ""))
	       (reference (org-html--reference special-block info))
	       (a (org-html--make-attribute-string
	           (if (or (not reference) (plist-member attributes :id))
		           attributes
		         (plist-put attributes :id reference))))
	       (str (if (org-string-nw-p a) (concat " " a) "")))
      (when (string= math-block "proof")
        ;; TODO: find a better way to insert QED square.
        (setq contents (concat (substring contents 0 -5)
                               "<span class=\"QED\">□</span>\n</p>")))
      (if html5-fancy
          (format "<%s%s>\n%s</%s>" block-type str contents block-type)
        (format "<div%s>\n%s\n</div>" str contents))
       )))

(defun org-blog-html-src-block (src-block _contents info)
  "SRC-BLOCK, for integrating with highlight.js"
  (let* ((lang (org-element-property :language src-block))
	     (code (s-trim (car (org-export-unravel-code src-block))))
         (lbl (org-html--reference src-block info t))
	     (label (if lbl (format " id=\"%s\"" lbl) "")))
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
               (format "<pre class=\"mermaid\"%s>%s</pre>"
                       label
                       code))

              (otherwise
               (add-to-list 'org-blog-extras 'highlight)
               (format "<pre><code class=\"language-%s\"%s%s>%s</code></pre>"
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

(defun org-blog-html-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel.

ps. 1. Remove spaces at the side of strong cn words.
2. Remove `figure-number'."
  (let* ((parent (org-export-get-parent paragraph))
	     (parent-type (org-element-type parent))
	     (style '((footnote-definition " class=\"footpara\"")
		          (org-data " class=\"footpara\"")))
	     (attributes (org-html--make-attribute-string
		              (org-export-read-attribute :attr_html paragraph)))
	     (extra (or (cadr (assq parent-type style)) "")))
    ;; 删除粗体之前的空格
    (setq contents
          (s-replace-regexp "\\([[:multibyte:]]\\)[ ]+\\(<b>[[:multibyte:]]\\)"
                            "\\1\\2"
                            contents))
    ;; 删除粗体之后的空格
    (setq contents
          (s-replace-regexp "\\([[:multibyte:]]</b>\\)[ ]+\\([[:multibyte:]]\\)"
                            "\\1\\2"
                            contents))
    (cond
     ((and (eq parent-type 'item)
	       (not (org-export-get-previous-element paragraph info))
	       (let ((followers (org-export-get-next-element paragraph info 2)))
	         (and (not (cdr followers))
		          (memq (org-element-type (car followers)) '(nil plain-list)))))
      ;; First paragraph in an item has no tag if it is alone or
      ;; followed, at most, by a sub-list.
      contents)
     ((org-html-standalone-image-p paragraph info)
      ;; Standalone image.
      (let ((caption (org-export-data (org-export-get-caption paragraph)
                                      info))
	        (label (org-html--reference paragraph info)))
	    (org-html--wrap-image contents info caption label)))
     ;; Regular paragraph.
     (t (format "<p%s%s>\n%s</p>"
		        (if (org-string-nw-p attributes)
		            (concat " " attributes) "")
		        extra contents)))))

(defun org-blog-html-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information.

Remove `table-number'."
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (org-html-table--table.el-table table info)
    ;; Standard table.
    (let* ((caption (org-export-get-caption table))
	       (number (org-export-get-ordinal
		            table info nil #'org-html--has-caption-p))
	       (attributes
	        (org-html--make-attribute-string
	         (org-combine-plists
	          (list :id (org-html--reference table info t))
	          (and (not (org-html-html5-p info))
		           (plist-get info :html-table-attributes))
	          (org-export-read-attribute :attr_html table))))
	       (alignspec
	        (if (bound-and-true-p org-html-format-table-no-css)
		        "align=\"%s\""
	          "class=\"org-%s\""))
	       (table-column-specs
	        (lambda (table info)
	          (mapconcat
	           (lambda (table-cell)
		         (let ((alignment (org-export-table-cell-alignment
				                   table-cell info)))
		           (concat
		            ;; Begin a colgroup?
		            (when (org-export-table-cell-starts-colgroup-p
			               table-cell info)
		              "\n<colgroup>")
		            ;; Add a column.  Also specify its alignment.
		            (format "\n%s"
			                (org-html-close-tag
			                 "col" (concat " " (format alignspec alignment)) info))
		            ;; End a colgroup?
		            (when (org-export-table-cell-ends-colgroup-p
			               table-cell info)
		              "\n</colgroup>"))))
	           (org-html-table-first-row-data-cells table info) "\n"))))
      (format "<table%s>\n%s\n%s\n%s</table>"
	          (if (equal attributes "") "" (concat " " attributes))
	          (if (not caption) ""
		        (format (if (plist-get info :html-table-caption-above)
			                "<caption class=\"t-above\">%s</caption>"
			              "<caption class=\"t-bottom\">%s</caption>")
			            (org-export-data caption info)))
	          (funcall table-column-specs table info)
	          contents))))

(defun org-blog-link-handler (link)
  (let ((is-image (s-match
                   "\\(<img.*src=\"\\)\\([^\"]*\\)\\(\"[^>]*>.*\\)"
                   link))
        (is-a (s-match
               "\\(<a.*href=\"\\)\\([^\"]*\\)\\(\"[^>]*>.*\\)"
               link)))
    (cond
     (is-image
      (seq-let (_ before src after) is-image
        (let ((cp-src (f-expand src org-blog-assets-directory))
              (cp-dst (f-expand src (f-expand (f-relative org-blog-assets-directory
                                                         org-blog-posts-directory)
                                              org-blog-publish-directory))))
          (when (f-exists-p cp-dst)
            (f-delete cp-dst))
          (f-copy cp-src cp-dst))
        (setq src (f-expand src "/assets"))
        (format "<a href=\"%s\" target=\"_blank\">%s</a>"
                src
                (concat before src after))))
     
     (is-a
      (seq-let (_ before href after) is-a 
        (if (--any-p (s-starts-with-p it href)
                     '("http" "#"))
            ;; A hack way to add `target'
            (unless (s-starts-with-p "#" href)
              (setq href (concat href "\" target=\"_blank")))
          
          ;; local file
          ;; start with one or more `../'
          (setq href (s-replace-regexp "^\\(\.\./\\)+\\(.*\\)"
                                       "/post/\\2"
                                       href))
          
          ;; start with `org-blog-posts-directory'
          (setq href (s-replace-regexp
                      (format "^%s/\\(.*\\)"
                              (f-base org-blog-posts-directory))
                      "/post/\\1"
                      href))

          ;; Else, assume they are in the same directory
          (setq href (concat "/post/" href))
          
          (when (f-ext href "html")
            (setq href (f-no-ext href))))
        (concat before href after)))
     
     (t link))))

(advice-add 'org-html-link
            :filter-return
            'org-blog-link-handler)

;; Tempororally
(advice-add 'org-html-format-latex
            :before
            (lambda (latex-frag processing-type info)
              (when (eq processing-type 'mathjax)
                (add-to-list 'org-blog-extras 'math))))

;;; 3rd-party
(defvar org-blog-third-party
  (list
   (cons 'math
         (cons
          "<link rel=\"stylesheet\" href=\"/static/katex/katex.min.css\">"
          (concat "<script defer src=\"/static/katex/katex.min.js\"></script>\n"
                     "<script defer src=\"/static/katex/auto-render.min.js\"></script>\n"
                     "<script>\n"
                     "let macros = {
\"\\\\C\": \"\\\\mathbb{C}\",
\"\\\\N\": \"\\\\mathbb{N}\",
\"\\\\Q\": \"\\\\mathbb{Q}\",
\"\\\\R\": \"\\\\mathbb{R}\",
\"\\\\Z\": \"\\\\mathbb{Z}\",
\"’\": \"'\"
};\n"
                     "document.addEventListener(\"DOMContentLoaded\", function() {
renderMathInElement(document.getElementById(\"content\"), {
strict: false,
delimiters: [
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
macros});})\n"
                     "</script>")))

   (cons 'highlight
         (cons
          "<link rel=\"stylesheet\" href=\"/static/highlight/dracula.css\">"
          (concat "<script src=\"/static/highlight/highlight.min.js\"></script>\n"
                  "<script>\n"
                  "Array.from(document.querySelectorAll('#content pre')).forEach(
node => {
let codeElement = node.querySelector(\"code\"),
    excludes = [\"mermaid\"];

if (!codeElement){
return
}
let language = codeElement.getAttribute(\"class\").split(\"-\")[1];
if (excludes.includes(language)) {
return
}

hljs.highlightElement(codeElement);
});\n"
                  "</script>")))

   (cons 'mermaid
         (cons
          ""
          (concat "<script src=\"/static/mermaid/mermaid.min.js\"></script>\n"
                  "<script>\n"
                  "mermaid.initialize({theme: 'neutral', securityLevel: 'loose'});\n"
                  "</script>")))
    ))

(org-export-define-derived-backend 'org-blog-post-backend 'html
  :translate-alist '((template . (lambda (contents info) contents))
                     (footnote-reference . org-blog-html-footnote-reference)
                     (src-block . org-blog-html-src-block)
                     (quote-block . org-blog-html-quote-block)
                     (special-block . org-blog-html-special-block)
                     (paragraph . org-blog-html-paragraph)
                     (table . org-blog-html-table)))

;;;###autoload
(defun org-blog-publish-file (post-filename)
  "Publish a single POST-FILENAME.
The index, archive, tags, and RSS feed are not updated."
  (interactive "f")
  (message "Publishing: %s" post-filename)
  (let* ((org-blog-extras nil)
         (org-blog-math-num (list (org-blog-get-math-chapter post-filename)
                                  0))
         (content (org-blog-render-post-content post-filename))
         (toc (unless (org-blog-is-special-p post-filename)
                (org-blog-build-toc content))))
    (message "Content generated.")
    (org-blog-with-find-file
     (org-blog-get-post-public-path post-filename)
     (org-blog-template
      (org-blog-get-title post-filename)
      content
      (org-blog-post-preamble post-filename toc)
      (org-blog-post-postamble post-filename)
      (org-blog-get-description post-filename)))))

;;;###autoload
(defun org-blog-publish (&optional force-render)
  "Render all blog posts, the index, archive, tags, and RSS feed.
Only blog posts that changed since the HTML was created are
re-rendered.

With a prefix argument, all blog posts are re-rendered
unconditionally."
  (interactive "P")
  (let* ((org-blog-post-filenames (org-blog-get-post-filenames))
         (org-blog-post-filenames-without-special (--filter
                                                   (not (org-blog-is-special-p it))
                                                   org-blog-post-filenames)))
    (--map-when
        (or force-render
            (org-blog-needs-publish-p it))
      (org-blog-publish-file it)
      org-blog-post-filenames)
    
    (org-blog-assemble-index)
    ;; (org-static-blog-assemble-rss)
    (org-blog-assemble-archive)
    (org-blog-assemble-tags))

  (org-blog-copy-directory org-blog-static-directory)
  ;; (org-blog-copy-directory org-blog-assets-directory)
  )
 

(provide 'org-blog)
;;; osb-blog.el ends
