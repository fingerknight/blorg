;;; org-blog.el -*- lexical-binding: t -*-
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
;; Package-Requires: ((emacs "28.2") (f.el) (dash.el) (s.el) (denote.el))

;;; Commentary:

;;; Personal fork of org-static-blog

;;; Code:

(require 'dash)
(require 'f)
(require 's)
(require 'org)
(require 'ox-html)
(require 'denote)
(require 'json)

(defgroup org-blog nil
  "Settings for a static blog generator using org-mode"
  :version "0.0.1"
  :group 'applications)

(defcustom org-blog-meta-title "Org Blog"
  "Title of the blog."
  :type '(string)
  :safe t)

(defcustom org-blog-meta-author "Author"
  "Author of the blog."
  :type '(string)
  :safe t)

(defcustom org-blog-meta-slogan "Personal fork of org-static-blog"
  "Slogan showed in the home page."
  :type '(string)
  :safe t)

(defcustom org-blog-publish-directory "~/blog"
  "Directory where published HTML files are stored.

Only are files with non-nil `#+publish' contained"
  :type '(directory))

(defcustom org-blog-posts-directory "~/denote/"
  "Directory where ORG files are stored."
  :type '(directory))

(defcustom org-blog-publish-posts-directory "post/"
  "Directory where published posts are stored.

A relative path to `org-blog-publish-directory'"
  :type '(string))

(defcustom org-blog-publish-archive-directory "archive/"
  "Directory where save the index.html of archive page.

A relative path to `org-blog-publish-directory'")

(defcustom org-blog-publish-tag-directory "tags/"
  "Directory where save the index.html of tag archive page.

A relative path to `org-blog-publish-directory'")

(defcustom org-blog-publish-tags-directory "tag/"
  "Directory where published tags page are stored.

A relative path to `org-blog-publish-directory'"
  :type '(string))

(defcustom org-blog-static-directory "~/denote/static/"
  "Directory where static resoureces are stored, like css, js."
  :type '(directory))

(defcustom org-blog-publish-static-directory "static/"
  "Directory saving the copy of static resoureces.

A relative path to `org-blog-publish-directory'
"
  :type '(string))

(defcustom org-blog-assets-directory "~/denote/assets/"
  "Directory where assets are stored, which are attachment
in org file."
  :type '(directory))

(defcustom org-blog-publish-assets-directory "assets/"
  "Directory saving the copy of assets.

A relative path to `org-blog-publish-directory'"
  :type '(string))

(defcustom org-blog-index-length 5
  "Number of articles to include on index page."
  :type '(integer)
  :safe t)

(defcustom org-blog-date-format "%Y-%m-%d"
  "Formatter of date."
  :type '(string)
  :safe t)

(defcustom org-blog-use-pygmentize nil
  "Use `Pygments' to highlight src blocks.")

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

(defcustom org-blog-page-head-template
  (list "<meta charset=\"UTF-8\">"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
        "<meta name=\"robots\" content=\"noodp\">\n"
        (format "<meta name=\"author\" content=\" %s \">\n"
                org-blog-meta-author)
        "<meta name=\"referrer\" content=\"no-referrer\">")
  "HTML to put in the <head> of each page."
  :type '(list string)
  :safe t)

(defcustom org-blog-page-header-template ""
  "HTML put absolutely before of each page."
  :type '(string)
  :safe t)

(defcustom org-blog-page-preamble-template nil
  "HTML put before the content of each page."
  :type '(list string)
  :safe t)

(defcustom org-blog-page-postamble-template nil
  "HTML put after the content of each page."
  :type '(list string)
  :safe t)

(defcustom org-blog-page-footer-template "<center> The End </center>"
  "HTML put absolutely after the content of each page."
  :type '(string)
  :safe t)

(defcustom org-blog-no-post-tag "nonpost"
  "Do not pushlish the subtree with this tag or property."
  :type '(string)
  :safe t)

(defcustom org-blog-cache-file (f-expand "org-blog-cache.json"
                                         default-directory)
  "An JSON file to save the cache of publishing.

For increasingly publishing, removing out-of-date HTML, etc.

Default value is ~/.emacs.d/org-blog-cache.json"
  :type '(string)
  :safe t)

(defcustom org-blog-third-party nil
  "A list containing CONS object.

Each CONS is ('module CONS), with another CONS like:
(preamble-html . postamble-html).

MODULE now support `math', `highlight', `mermaid'.
PREAMBLE-HTML and POSTAMBLE-HTML are the scripts,
when module is triggered, put content.

PREAMBLE-HTML should contain style sheets, and
POSTAMBLE-HTML should contain js script.
")

(defcustom org-blog-pre-hook nil
  "Run before publishing.")

(defcustom org-blog-post-hook nil
  "Run after publishing.")

(defvar org-blog-cache nil
  "Cache for publishing")

(defvar org-blog-post-filenames nil
  "A list contains all the posts' abs filename.")

(defvar org-blog-post-filenames-without-special nil
  "A list contains all the posts' abs filename,
whose value of `publish' is not \"special\".")

(defvar org-blog-publishing-post nil
  "The post's filename that is publishing now.")

(defvar org-blog-publishing-post-assets nil
  "The assets of post's filename that is publishing now.")

(defvar org-blog-extras nil)

(defvar org-blog-publish-posts nil
  "Temporarily store posts needed to publish.")

(defvar org-blog-publish-posts-remove nil
  "Temporarily store posts needed to be removed.")

(defvar org-blog-publish-tags nil
  "Temporarily store tags needed to publish.")

(defvar org-blog-publish-tags-remove nil
  "Temporarily store tags needed to be removed.")

(defvar org-blog-publish-assets nil
  "Temporarily store assets needed to be copied.")

(defvar org-blog-publish-assets-remove nil
  "Temporarily store assets needed to be removed.")

(defvar cache-archive-table nil
  "A hash table containing all the posts,
with their tags and last modificatio time.")

(defvar cache-tags-table nil
  "A hash table containing all the tags,
with the posts' filename used it.")

(defvar cache-static-table nil
  "A hash table containing all the static files")

(defvar org-blog-math-num nil
  "A list with two elements:
One is the chapter number, which is a string.
Another is the item number, which is a integer")

(defun keyhash (table)
  "Return a list containing all the keys of `TABLE'."
  (let ((keys nil))
    (maphash #'(lambda (k v)
                 (push k keys))
             table)
    (reverse keys)))

(defun org-blog--sort-file-recent (it other)
  "Sort posts by recent date. Inputs are IDs."
  (> (org-blog-get-date it)
     (org-blog-get-date other)))

(defun org-blog-cache-read ()
  "Read cache from JSON file."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (unless (f-exists-p org-blog-cache-file)
      (with-temp-file org-blog-cache-file
        (insert "{"
                "\"archive\": {}, "
                " \"tags\": {}, "
                "\"static\": {} "
                "}")))
    (json-read-file org-blog-cache-file)))

(defun org-blog-cache-write ()
  "Write cache to JSON file"
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (setq hhh org-blog-cache)
    (f-write-text
     (json-encode org-blog-cache)
     ;; (json-serialize org-blog-cache)
     'utf-8
     org-blog-cache-file)))

(defun org-blog-message (S &rest args)
  "Output `S' into `*Org-Blog-Messages*' buffer with prefix `[Org Blog]'"
  (apply #'message (cons (concat "[Org Blog] " S) args)))

(defun org-blog--diff (actual-list cache-list)
  "compare two list, return UPDATE-LIST and REMOVE-LIST"
  (list
   (-difference actual-list cache-list)
   (-difference cache-list actual-list)))

(defun org-blog-template (title content preamble postamble &optional toc)
  "Create the template that is used to generate the static pages."
  (concat
   "<!DOCTYPE html>\n"
   "<html>\n"
   "<head>\n"
   ;; "<link rel=\"alternate\"\n"
   ;; "      type=\"application/rss+xml\"\n"
   ;; "      href=\"" (org-blog-get-absolute-url org-blog-rss-file) "\"\n"
   ;; "      title=\"RSS feed for " org-blog-publish-url "\">\n"
   "<title>" title "</title>\n"
   (apply #'s-concat org-blog-page-head-template)
   "</head>\n"
   "<body>\n"
   "<div id=\"header\">\n"
   org-blog-page-header-template
   "</div>\n"
   "<div id=\"main\">\n"
   "<div id=\"toc\">\n"
   toc
   "</div>\n"
   "<div id=\"page\">\n"
   "<div id=\"preamble\" class=\"status\">"
   (apply #'concat preamble org-blog-page-preamble-template)
   "</div>\n"
   "<div id=\"content\">\n"
   content
   "</div>\n"
   "<div id=\"postamble\" class=\"status\">\n"
   (apply #'concat postamble org-blog-page-postamble-template)
   "</div>\n"
   "</div>\n"
   "</div>\n"
   "<div id=\"footer\">\n"
   org-blog-page-footer-template
   "</div>\n"
   "</body>\n"
   "</html>\n"))

(defun org-blog-get-all-posts ()
  "Return a list of all posts' ID with `blog' or `blogsp' tag."
  (--filter (-intersection '("blog" "blogsp") (org-blog-get-tags it t))
            (-map #'denote-extract-id-from-string
                  (f-files org-blog-posts-directory 'denote-file-is-note-p t))))

(defun org-blog-get-post-filenames ()
  "Return a list of all posts with non-nil `publish' header."
  (--filter
   (-intersection '("blog" "blogsp")
                  (org-blog-get-tags it t))
   (f-files org-blog-posts-directory 'denote-file-is-note-p t)))

(defun org-blog-is-special-p (post-id)
  "Check whether post with POST-ID is SPECIAL."
  (member "blogsp" (org-blog-get-tags post-id t)))

(defun org-blog-matching-publish-filename (post-filename)
  "Generate HTML file name for POST-FILENAME."
  (f-expand (org-blog-get-post-publish-path post-filename)
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
(defmacro org-blog-with-find-file (file contents)
  "Insert CONTENTS itno FILE.
The buffer is disposed after the macro exits (unless it already
existed before)."
  `(save-excursion
     (let ((current-buffer (current-buffer))
           (buffer-exists (org-blog-file-buffer ,file))
           (contents ,contents))
       (if buffer-exists
           (with-current-buffer buffer-exists
             (erase-buffer)
             (insert contents)
             (basic-save-buffer))
         (f-write-text contents 'utf-8 ,file)))))

(defun org-blog-get-path (post-id)
  "Return the absolte path of post with POST-ID"
  (denote-get-path-by-id post-id))

(defun org-blog-get-title (post-id)
  "Return title of post with POST-ID"
  (denote-retrieve-filename-title (denote-get-path-by-id post-id)))

(defun org-blog-get-description (post-id)
  "TODO: Generate description for post with POST-ID"
  "")

(defun org-blog-get-location (post-id)
  "Get `LOCATION' in front matter from special post."
  (let* ((file (org-blog-get-path post-id))
         (case-fold-search t)
         (location (if file
                       (nth 1
                            (s-match "^\\#\\+location:\s*\\(.+\\)"
                                     (f-read file)))
                     (gethash "location" (gethash post-id cache-archive-table)))))
    (or location "")))

(defun org-blog-get-date (post-id)
  "Get the last modified date (TIMESTAMP, INTEGER) of post with POST-ID"
  (f-modification-time (org-blog-get-path post-id) 'seconds))

(defun org-blog-get-tags (post-id &optional all)
  "Extract tags from post with POST-iD.
If ALL is `nil', then \"blog\" and \"blogsp\" are excluded."
  (-difference (denote-extract-keywords-from-path
                (denote-get-path-by-id post-id))
               (unless all '("blog" "blogsp"))))

(defun org-blog-get-math-chapter (post-id)
  "Get chapter number for mathametics."
  (or
   (car (s-match "\\([0-9]+\.\\)+"
                 (org-blog-get-title post-id)))
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
      "\n")))

(defun org-blog-get-tag-tree (posts)
  "Return an association list of tags to posts.
e.g. `(('tag1' 'id1' 'id2') ('tag2' 'id2'))`"
  (let ((tag-tree '()))
    (dolist (post-id posts)
      (dolist (tag (-difference (org-blog-get-tags post-id)
                                org-blog-rss-excluded-tag))
        (if (assoc-string tag tag-tree t)
            (push post-id (cdr (assoc-string tag tag-tree t)))
          (push (cons tag (list post-id)) tag-tree))))
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

(defun org-blog-get-post-relative-url (post-id)
  "Returns absolute URL to the published POST-FILENAME.

This function concatenates publish URL and generated custom filepath to the
published HTML version of the post."
  (f-join "/"
          (f-relative
           (f-dirname (org-blog-get-post-publish-path post-id))
           org-blog-publish-directory)))

(defun org-blog-get-post-publish-path (post-id)
  "Returns post relative filepath in public directory."
  (f-join org-blog-publish-directory
          (if (org-blog-is-special-p post-id)
              (org-blog-get-location post-id)
            (format "%s/%s"
                    org-blog-publish-posts-directory
                    post-id))
          "index.html"))

(defun org-blog-render-post-content (post-id)
  "Render blog content as bare HTML without header."
  (let ((org-html-doctype "html5")
        (org-html-html5-fancy t))
    (save-excursion
      (let ((current-buffer (current-buffer))
            (buffer-exists (org-blog-file-buffer (org-blog-get-path post-id)))
            result
            org-moode-hook
            prog-mode-hook)
        (with-temp-buffer
          (if buffer-exists
              (insert-buffer-substring buffer-exists)
            (insert-file-contents (org-blog-get-path post-id)))
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

(defun org-blog-assemble-index (posts)
  "Assemble the blog index page with given POSTS.
The index page contains the last `org-blog-index-length`
posts as full text posts."
  (org-blog-message "Generating index.html...")
  (org-blog-assemble-multipost-page
   (f-expand "index.html" org-blog-publish-directory)
   (-slice (-sort #'org-blog--sort-file-recent
                  posts)
           0
           org-blog-index-length)
   (format "<div class=\"index-desc\">\n<center> %s </center>\n</div>\n"
           org-blog-meta-slogan)
   nil
   t))

(defun org-blog-assemble-multipost-page (pub-filename post-ids preamble postamble &optional preview)
  "Assemble a page that contains multiple posts one after another.
Posts are sorted in descending time."
  (org-blog-with-find-file
   pub-filename
   (org-blog-template
    org-blog-meta-title
    (mapconcat (if preview 'org-blog-get-preview 'org-blog-publish-post-summary)
               (-sort #'org-blog--sort-file-recent post-ids)
               "")
     preamble
     postamble)))

(defun org-blog-post-preamble (post-id &optional special)
  "Returns the formatted date and headline of the post.
This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline.

If SPECIAL is non-nil', date and tags won't be shown."
  (concat
   (mapconcat
    (lambda (it)
      (car (assoc-default it org-blog-third-party)))
    org-blog-extras
    "\n")
   "<h1 class=\"post-title\"> "
   (org-blog-get-title post-id)
   " </h1>\n"
   (unless special
     (concat
      (format-time-string
       (concat "<div class=\"post-date\">"
               org-blog-date-format
               "</div>\n")
       (org-blog-get-date post-id))
      
      "<div class=\"taglist\">"
      (org-blog-post-taglist post-id)
      "\n</div>\n"))))

(defun org-blog-post-taglist (post-id)
  "Returns the tag list of the post.
This part will be attached at the end of the post, after
the taglist, in a <div id=\"taglist\">...</div> block."
  (let ((taglist-content "")
        (tags (-difference (org-blog-get-tags post-id)
                           org-blog-rss-excluded-tag)))
    (when tags
      (mapconcat
       (lambda (tag)
         (format "<a href=\"%s\"> #%s </a>"
                 (concat "/tag/" (downcase tag))
                 tag))
       tags "\n"))))

(defun org-blog-post-postamble (post-id)
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
                       org-blog-meta-title
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
    (org-blog-message "%s" rss-tag-items)
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

(defun org-blog-assemble-archive (posts)
  "Render the blog archive page with given POSTS.
The archive page contains single-line links and dates for every
blog post, but no post body."
  (org-blog-message "Generating Archive page.")
  (org-blog-with-find-file
   (f-join org-blog-publish-directory
           org-blog-publish-archive-directory
           "index.html")
   (org-blog-template
    (format "ARCHIVEs - %s" org-blog-meta-title)
    (mapconcat 'org-blog-publish-post-summary
               (-sort #'org-blog--sort-file-recent
                      posts)
               "")
    (concat "<h1 class=\"post-title\"> ARCHIVEs </h1>")
    nil)))

(defun org-blog-publish-post-summary (post-id &optional with-tags)
  "Assemble post summary for an archive page.
This function is called for every post on the archive and
tags-archive page. Modify this function if you want to change an
archive headline."
  (concat
   "<div class=\"summary-item\">"
   "<div class=\"summary-title\">"
   (format "<a href=\"%s\"> %s </a>"
           (org-blog-get-post-relative-url post-id)
           (org-blog-get-title post-id))
   "</div>\n"
   "<div class=\"summary-date\">"
   (format-time-string org-blog-date-format
              (org-blog-get-date post-id))
   "</div>"
   
   "</div>\n"))

(defun org-blog-assemble-tags-archive-tag (tag)
  "Assemble single TAG for all filenames."
  (let ((tag-name (car tag))
        (post-ids (cdr tag)))
    (format "<a class=\"tag-item\" href=\"%s\"> %s<sup>%s </sup></a>\n"
            (concat "/tag/" (downcase tag-name))
            tag-name
            (length post-ids))))

(defun org-blog-get-preview (post-id)
  "Get title, date, tags from POST-FILENAME and get the first paragraph from the rendered HTML."
  (let ((post-title (org-blog-get-title post-id))
        (post-date (org-blog-get-date post-id))
        (post-taglist (org-blog-post-taglist post-id))
        (preview-region
         (with-temp-buffer
           (insert-file-contents (org-blog-get-post-publish-path post-id))
           (org-blog--preview-region))))
    ;; Put the substrings together.
    (concat
     "<div class=\"post-item\">\n"
     (format "<h2 class=\"post-title\"><a href=\"%s\"> %s </a></h2>"
             (org-blog-get-post-relative-url post-id)
             post-title)
     preview-region
     (format "<div class=\"post-info\">\n%s\n%s\n</div>\n"
             (format "<div class=\"taglist\">%s</div>\n" post-taglist)
             (format-time-string (concat "<div class=\"post-date\">"
                                org-blog-date-format
                                "</div>")
                        post-date))
     "</div>\n")))

(defun org-blog-assemble-tags (posts)
  "Assemble the blog tag archive page with given POSTS.
The archive page contains single-line links and dates for every
blog post, sorted by tags, but no post body."
  (org-blog-message "Generating Tags index page")
  (org-blog-with-find-file
   (f-join org-blog-publish-directory
             org-blog-publish-tag-directory
             "index.html")
   (org-blog-template
    (format "TAGs - %s" org-blog-meta-title)
    (mapconcat
     'org-blog-assemble-tags-archive-tag
     (--sort (s-less-p (car it) (car other))
             (org-blog-get-tag-tree posts))
     "")
    "<h1 class=\"post-title\"> TAGs </h1>\n"
    nil)))

(defun org-blog-publish-tag (tag)
  "Publish a single TAG page."
  (org-blog-assemble-multipost-page
   (f-join org-blog-publish-directory
           org-blog-publish-tags-directory
           (downcase tag)
           "index.html")
   (gethash tag cache-tags-table)
   (concat "<h1 class=\"post-title\"> TAG: #" tag " </h1>")
   nil))

(defun org-blog-handle-static ()
  "Copy or delete static files increasingly."
  (org-blog-message "Handling static files")
  (let ((cache-static-table (gethash "static" org-blog-cache))
        (static-files (--map
                       (f-relative it org-blog-static-directory)
                       (f-files org-blog-static-directory nil t))))
    (maphash
     #'(lambda (static-file lastmod)
         (let ((static-file-dst
                (f-join org-blog-publish-directory
                        org-blog-publish-static-directory
                        static-file)))
         
           (if (-elem-index static-file static-files)
               (let* ((static-file-src
                       (f-expand static-file org-blog-static-directory))
                      (new-lastmod (f-modification-time static-file-src
                                                        'seconds)))
                 (when (> new-lastmod
                          lastmod)
                   (puthash static-file new-lastmod cache-static-table)
                   (when (f-exists-p static-file-dst)
                     (f-delete static-file-dst))
                   (org-blog-message "Copying static: %s" static-file-dst)
                   (f-mkdir (f-dirname static-file-dst))
                   (copy-file static-file-src static-file-dst t)))

             ;; out-of-date
             (org-blog-message "Removing static: %s" static-file-dst)
             (when (f-exists-p static-file-dst)
               (f-delete static-file-dst))
             (remhash static-file cache-static-table))
           (setq static-files
                 (-remove-item static-file static-files))))
     cache-static-table)

    ;; new
    (--each static-files
      (let* ((src (f-expand it org-blog-static-directory))
             (dst (f-join org-blog-publish-directory
                          org-blog-publish-static-directory
                          it))
             (lastmod (f-modification-time src 'seconds)))
        (puthash it lastmod cache-static-table)
        (org-blog-message "Copying static: %s" dst)
        (f-mkdir (f-dirname dst))
        (copy-file src dst t)))))

(defun org-blog-handle-asset ()
  "Copy or delete asset files increasingly."
  (org-blog-message "Handling asset files")
  (let ((all-assets
         (let ((lst nil))
           (maphash #'(lambda (post data-table)
                        (setq lst (-union lst (gethash "asset" data-table))))
                    cache-archive-table)
           lst))
        (copy-assets org-blog-publish-assets)
        (remove-assets org-blog-publish-assets-remove)
        
        (published-assets
         (-map #'f-filename
               (f-files (f-expand org-blog-publish-assets-directory
                                  org-blog-publish-directory)))))

    ;; update assets
    (--each copy-assets
      (let ((src (f-expand it org-blog-assets-directory))
            (dst (f-join org-blog-publish-directory
                         org-blog-publish-assets-directory
                         (format "%s.%s"
                                 (md5 (f-no-ext it))
                                 (f-ext it)))))
        (org-blog-message "Copying asset: %s" it)
        (copy-file src dst t)))

    ;; remove out-of-date assets
    (--each (-difference remove-assets all-assets)
      (let ((abs-path (f-join org-blog-publish-directory
                        org-blog-publish-assets-directory
                        (format "%s.%s"
                                (md5 (f-no-ext it))
                                (f-ext it)))))
        (org-blog-message "Removing asset: %s" it)
        (when (f-exists-p abs-path)
          (f-delete abs-path))))))

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

(defun org-blog-pygmentize (lang code)
  "Use `Pygments' to highlight CODE with given LANGuage."
  (let* ((tmp-file "/tmp/org-blog-pygmentize")
         (qcmd (concat "pygmentize -L \"lexers\" --json |"
                       "jq '.lexers | map(.)[] | select(.aliases | index(\""
                       lang
                       "\"))'"))
         (lang-exists (shell-command-to-string qcmd))
         (lang (if (length= lang-exists 0)
                 "text" lang))
         (cmd (concat "cat "
                      tmp-file
                      " | pygmentize -l \""
                      lang
                      "\" -f html -O cssclass=org-src-container,encoding=utf-8,nowrap=True,ensurenl=False -s")))
    (f-write-text code 'utf-8 tmp-file)
    (shell-command-to-string cmd)))

(defun org-blog-html-src-block (src-block _contents info)
  "SRC-BLOCK, for integrating with highlight.js"
  (let* ((lang (org-element-property :language src-block))
         (code-info (org-export-unravel-code src-block))
	     (code (org-html-encode-plain-text (car code-info)))
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
               (if org-blog-use-pygmentize
                   (progn
                     (add-to-list 'org-blog-extras 'highlight-pygments)
                     (format "<pre class=\"%s\"> %s </pre>"
                             lang
                             (org-blog-pygmentize lang code)))
                   (add-to-list 'org-blog-extras 'highlight)
                 (format "<pre><code class=\"language-%s\"%s%s>%s</code></pre>"
                         (or lang "plaintext")
                         label
                         (if (string= lang "html")
				             " data-editor-type=\"html\""
			               "")
                         code)))))))

(defun org-blog-html-quote-block (quote-block contents info)
  "Add author and reference.

By using `#+attr_html: :from [ref] :by [author] before quote block.'
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
                        (s-append (concat  "<p class=\"quote-source\">"
                                           " &#x2014;&#x2014; "
                                           by
                                           (when (and from by)
                                             ", ")
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
             
(defun org-blog-html-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel.

ps. 1. Remove spaces at the side of strong Chinese words.
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

(defun org-blog--advice-link (link)
  (let ((a-matched (s-match
               "\\(<a.*href=\"\\)\\([^\"]*\\)\\(\"[^>]*>.*\\)"
               link)))
    (if a-matched
      (seq-let (_ before href after) a-matched
        ;; A hack way to add `target'
        (unless (s-starts-with-p "#" href)
          (setq after (concat "\" target=\"_blank" after)))
        ;; local file, only consider `.org' files
        (let ((post-id (denote-extract-id-from-string href)))
          (when (and post-id
                     (not (or (s-starts-with-p "#" href)
                              (org-url-p href))))
            (setq href (f-join "/"
                               org-blog-publish-posts-directory
                               post-id))))
        (concat before href after))
      link)))

(defun org-blog--advice-image (func &rest rest)
  "Fix image's src to be relative to`org-blog-publish-assets-directory',
and wrap <img> into a <a> source attributes info"
  (let* ((src (car rest))
         (image-abs-path (f-expand src
                                   (f-dirname org-blog-publishing-post)))
         (rel-src (f-relative image-abs-path
                              org-blog-assets-directory))
         (new-src (f-join "/"
                          org-blog-publish-assets-directory
                          (format "%s.%s"
                                  (md5 (f-no-ext rel-src))
                                  (f-ext rel-src)))))
    (push (f-relative image-abs-path
                      org-blog-assets-directory)
          org-blog-publishing-post-assets)
    
    (format "<a href=\"%s\" target=\"_blank\">%s</a>"
            new-src
            (apply func (cons new-src (cdr rest))))))

(advice-add 'org-html-link
            :filter-return
            'org-blog--advice-link)

(advice-add 'org-html--format-image
            :around
            'org-blog--advice-image)

;; Tempororally
(advice-add 'org-html-format-latex
            :before
            (lambda (latex-frag processing-type info)
              (when (eq processing-type 'mathjax)
                (add-to-list 'org-blog-extras 'math))))

(org-export-define-derived-backend 'org-blog-post-backend 'html
  :translate-alist '((template . (lambda (contents info) contents))
                     (footnote-reference . org-blog-html-footnote-reference)
                     (src-block . org-blog-html-src-block)
                     (quote-block . org-blog-html-quote-block)
                     (special-block . org-blog-html-special-block)
                     (paragraph . org-blog-html-paragraph)
                     (table . org-blog-html-table)))

(defun org-blog-remove-published-file (post-id)
  "Remove published post."
  (let* ((cache-post (gethash post-id cache-archive-table))
         (remove-tags (gethash "tag" cache-post))
         (remove-assets (gethash "asset" cache-post))
         (location (gethash "location" cache-post)))
    (org-blog-message "Removing post: %s" (gethash "title" cache-post))

    (remhash post-id cache-archive-table)
    (f-delete (f-join org-blog-publish-directory
                      (if (length= location 0)
                          (format "%s/%s"
                                  org-blog-publish-posts-directory
                                  post-id)
                        location))
              t)

    (--each remove-tags
      (let ((new-lst (-remove-item post-id
                                   (gethash it cache-tags-table))))
        (if new-lst
            (puthash it new-lst cache-tags-table)
          ;; empty tags
          (push it org-blog-publish-tags-remove))))

    (setq org-blog-publish-assets-remove
          (-union org-blog-publish-assets-remove
                  remove-assets))))

(defun org-blog-remove-published-tag (tag)
  "Remove published tag"
  (let* ((cache-tag (gethash tag cache-tags-table)))
    (remhash tag cache-tags-table)
    (org-blog-message "Removing tag: %s" tag)
    (f-delete (f-join org-blog-publish-directory
                      org-blog-publish-tags-directory
                      tag)
              t)))

(defun org-blog-publish-file (post-id)
  "Publish a single post with POST-ID."
  (org-blog-message "Publishing: %s" (org-blog-get-title post-id))
  
  (let* ((org-blog-extras nil)
         (org-blog-publishing-post (org-blog-get-path post-id))
         (org-blog-publishing-post-assets nil)
         (org-blog-math-num (list (org-blog-get-math-chapter post-id)
                                  0))
         (content (org-blog-render-post-content post-id))
         (toc (unless (org-blog-is-special-p post-id)
                (org-blog-build-toc content)))
         (cache-post (gethash post-id cache-archive-table)))

    (puthash "title" (org-blog-get-title post-id) cache-post)
    (puthash "lastmod" (org-blog-get-date post-id) cache-post)
    (puthash "location" (org-blog-get-location post-id) cache-post)

    ;; handler tags
    (let ((post-tags (org-blog-get-tags post-id)))
      (seq-let (update-tags remove-tags)
          (org-blog--diff post-tags (gethash "tag" cache-post))
        (--each update-tags
          (let ((cache-posts-in-tag (gethash it cache-tags-table)))
            (puthash it
                     (-snoc cache-posts-in-tag post-id)
                     cache-tags-table))
          (push it org-blog-publish-tags))

        (--each remove-tags
          (let* ((cache-posts-in-tag (-remove-item post-id
                                                   (gethash it cache-tags-table))))
            (unless cache-posts-in-tag
              (push it org-blog-publish-tags-remove))
            (puthash it
                     cache-posts-in-tag
                     cache-tags-table))))
      
      (puthash "tag" post-tags cache-post))
    
    ;; handler assets
    (seq-let (update-assets remove-assets)
        (org-blog--diff org-blog-publishing-post-assets
                        (gethash "asset" cache-post))
      (setq org-blog-publish-assets
            (-union org-blog-publish-assets
                    update-assets)
            
            org-blog-publish-assets-remove
            (-union org-blog-publish-assets-remove
                    remove-assets))
      (puthash "asset" org-blog-publishing-post-assets cache-post))

    ;; Writing output file
    (org-blog-with-find-file
     (org-blog-get-post-publish-path post-id)
     (org-blog-template
      (format "%s - %s"
              (org-blog-get-title post-id)
              org-blog-meta-title)
      content
      (org-blog-post-preamble post-id (not toc))
      (org-blog-post-postamble post-id)
	  toc))))

;;;###autoload
(defun org-blog-publish ()
  "Render all blog posts, the index, archive, tags, and RSS feed.
Only blog posts that changed since the HTML was created are
re-rendered."
  (interactive)
  (org-blog-message "Start to publish...")

  (apply #'run-hooks org-blog-pre-hook)
  
  ;; Preprocessing
  (f-mkdir org-blog-publish-directory
           (f-expand "archive" org-blog-publish-directory)
           (f-expand "assets" org-blog-publish-directory)
           (f-expand "static" org-blog-publish-directory)
           (f-expand "post" org-blog-publish-directory)
           (f-expand "tag" org-blog-publish-directory)
           (f-expand "tags" org-blog-publish-directory))
  (org-blog-message "Structure built")
  
  (let* ((org-blog-all-posts (org-blog-get-all-posts))
         (org-blog-normal-posts
          (--filter (not (org-blog-is-special-p it))
                    org-blog-all-posts))
         (org-blog-cache (org-blog-cache-read))
         (cache-archive-table (gethash "archive" org-blog-cache))
         (cache-tags-table (gethash "tags" org-blog-cache))
         (org-blog-publish-posts nil)
         (org-blog-publish-posts-remove nil)
         (org-blog-publish-tags nil)
         (org-blog-publish-tags-remove nil)
         (org-blog-publish-assets nil)
         (org-blog-publish-assets-remove nil)
         (new-posts org-blog-all-posts)

         (org-src-fontify-natively nil))

    (org-blog-message "Total posts: %d -- Normal posts: %d. Special posts: %d"
                      (length org-blog-all-posts)
                      (length org-blog-normal-posts)
                      (- (length org-blog-all-posts)
                         (length org-blog-normal-posts)))

    (--each (keyhash cache-archive-table)
      (let ((cache-post (gethash it cache-archive-table)))
        (if (-elem-index it org-blog-all-posts)
            ;; Existed posts
            (progn
              (when (> (org-blog-get-date it)
                       (gethash "lastmod" cache-post))
                ;; Newer
                (push it org-blog-publish-posts))
              ;; remove all exxisted posts from new-`posts'
              (setq new-posts (-remove-item it new-posts)))
          ;; out-of-date posts
          (push it org-blog-publish-posts-remove))))

    ;; new posts
    (--each new-posts
      (let ((cache-post (make-hash-table :test 'equal)))
        (push it org-blog-publish-posts)
        (puthash it cache-post cache-archive-table)))
    
    ;; make sure every thing shows only once.
    (setq org-blog-publish-posts (-distinct org-blog-publish-posts)
          org-blog-publish-posts-remove
          (-distinct org-blog-publish-posts-remove)

          org-blog-publish-tags (-distinct org-blog-publish-tags)
          org-blog-publish-tags-remove
          (-distinct org-blog-publish-tags-remove)

          org-blog-publish-assets (-distinct org-blog-publish-assets)
          org-blog-publish-assets-remove
          (-distinct org-blog-publish-assets-remove))

    (org-blog-message "%d posts publishing, %d posts are new"
                      (length org-blog-publish-posts)
                      (length new-posts))
    
    ;; publish posts' page
    (-each org-blog-publish-posts #'org-blog-publish-file)
    
    ;; clean out-of-date posts page
    (-each org-blog-publish-posts-remove #'org-blog-remove-published-file)
    
    ;; publish tags' page
    (-each org-blog-publish-tags #'org-blog-publish-tag)
    
    ;; clean out-of-date tags' page
    (-each org-blog-publish-tags-remove #'org-blog-remove-published-tag)
    
    (org-blog-assemble-index org-blog-normal-posts)
    ;; (org-static-blog-assemble-rss)
    (org-blog-assemble-archive org-blog-normal-posts)
    (org-blog-assemble-tags org-blog-normal-posts)

    (org-blog-handle-static)
    (org-blog-handle-asset)
    
    (org-blog-cache-write)
	(apply #'run-hooks org-blog-post-hook)
    (org-blog-message "Done.")))

(provide 'org-blog)
;;; osb-blog.el ends
