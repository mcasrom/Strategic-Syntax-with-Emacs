;;; blog.el --- Advanced blog publishing system with Org-mode, Git, and SEO -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 4.4
;; URL: https://yourblog.com
;; Package-Requires: ((emacs "27.1") (transient "0.3.7") (request "0.3.2") (json "1.4"))

;;; Commentary:
;; A complete blog management system for Emacs using Org-mode, org-publish, Git,
;; and SEO optimizations. Includes sitemap.xml, robots.txt, RSS feeds, and analytics.

;;; Requirements:
(require 'ox-publish)
(require 'ox-rss)
(require 'transient)
(require 'request nil t)
(require 'json)
(require 'url)
(require 'cl-lib)
(require 'subr-x)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar my-blog/posts-cache nil "Cache of posts for performance.")
(defvar my-blog/posts-cache-time nil "Last time posts were cached.")

(defun my-blog/get-posts (&optional force)
  "Get posts, with optional FORCE refresh of cache."
  (when (or force
            (null my-blog/posts-cache)
            (null my-blog/posts-cache-time)
            (time-less-p my-blog/posts-cache-time
                       (time-subtract (current-time) (seconds-to-time 300))))
    (setq my-blog/posts-cache (directory-files-recursively my-blog/posts-dir "\\.org$"))
    (setq my-blog/posts-cache-time (current-time)))
  my-blog/posts-cache)


;;; Chapter 1: Configuration and Setup
(defgroup my-blog nil
  "Settings for the blog publishing system."
  :group 'applications)

(defcustom my-blog/base-dir "~/org-blog/"
  "Base directory for blog source files."
  :type 'directory
  :group 'my-blog)

(defcustom my-blog/output-dir "~/public_html/"
  "Output directory for published blog."
  :type 'directory
  :group 'my-blog)

(defcustom my-blog/perplexity-api-key nil
  "API key for text analysis service (mocked in this version)."
  :type '(choice (const :tag "Not configured" nil)
                (string :tag "API Key"))
  :group 'my-blog)

(defcustom my-blog/blog-title "🔹 Strategic Syntax with Emacs"
  "Main title of the blog."
  :type 'string
  :group 'my-blog)

(defcustom my-blog/blog-subtitle "Org Mode, AI, and Defense in the Digital Age"
  "Subtitle of the blog."
  :type 'string
  :group 'my-blog)

(defcustom my-blog/author "M.Castillo"
  "Author name for blog posts."
  :type 'string
  :group 'my-blog)

(defcustom my-blog/contact-email "mybloggingnotes@gmail.com"
  "Contact email address displayed in the blog footer."
  :type 'string
  :group 'my-blog)


(defcustom my-blog/site-url "https://yourblog.com/"
  "Canonical URL of the blog."
  :type 'string
  :group 'my-blog)

(defcustom my-blog/templates
  '(("standard" . "#+TITLE: %s\n#+DATE: %s\n#+AUTHOR: %s\n#+OPTIONS: toc:nil num:nil\n#+CATEGORY: %s\n#+TAGS: %s\n#+DESCRIPTION: Write a brief description here...\n#+FEATURED_IMAGE: /images/default.jpg\n\n* Introduction\n\nWrite your content here...\n")
    ("review" . "#+TITLE: Review: %s\n#+DATE: %s\n#+AUTHOR: %s\n#+OPTIONS: toc:nil num:nil\n#+CATEGORY: reviews\n#+TAGS: %s\n#+DESCRIPTION: Write a brief description here...\n#+FEATURED_IMAGE: /images/default.jpg\n\n* Summary\n\n* Analysis\n\n* Rating\n\n* Conclusion\n")
    ("tutorial" . "#+TITLE: Tutorial: %s\n#+DATE: %s\n#+AUTHOR: %s\n#+OPTIONS: toc:t num:t\n#+CATEGORY: tutorials\n#+TAGS: %s\n#+DESCRIPTION: Write a brief description here...\n#+FEATURED_IMAGE: /images/default.jpg\n\n* Introduction\n\n* Requirements\n\n* Steps\n\n* Conclusion\n"))
  "Templates for different post types."
  :type '(alist :key-type string :value-type string)
  :group 'my-blog)

(defvar my-blog/posts-dir (concat my-blog/base-dir "posts/"))
(defvar my-blog/drafts-dir (concat my-blog/base-dir "drafts/"))
(defvar my-blog/pages-dir (concat my-blog/base-dir "pages/"))
(defvar my-blog/images-dir (concat my-blog/base-dir "images/"))
(defvar my-blog/assets-dir (concat my-blog/base-dir "assets/"))
(defvar my-blog/stats-file (concat my-blog/base-dir "stats.org"))
(defvar my-blog/categories '("blog" "technology" "tutorials" "reviews" "personal" "security" "orgmode"))
(defvar my-blog/common-tags '("emacs" "programming" "linux" "web" "org-mode" "security"))

(defun my-blog/ensure-directories ()
  "Create necessary directories if they don't exist."
  (dolist (dir (list my-blog/base-dir my-blog/posts-dir my-blog/drafts-dir
                     my-blog/pages-dir my-blog/images-dir my-blog/assets-dir
                     my-blog/output-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (message "Created directory: %s" dir))))

(defun my-blog/validate-directories ()
  "Verify that blog directories exist and are writable."
  (my-blog/ensure-directories)
  (dolist (dir (list my-blog/base-dir my-blog/posts-dir my-blog/drafts-dir
                     my-blog/pages-dir my-blog/images-dir my-blog/assets-dir
                     my-blog/output-dir))
    (unless (file-writable-p dir)
      (user-error "Directory %s is not writable" dir))))

(my-blog/validate-directories)

;;; Chapter 2: Publishing Configuration
(defun my-blog/safe-plist-string (plist key default)
  "Safely extract a string from PLIST for KEY, stripping properties, with DEFAULT."
  (let ((value (plist-get plist key)))
    (cond
     ((stringp value) (substring-no-properties value))
     ((and value (stringp (format "%s" value))) (substring-no-properties (format "%s" value)))
     (t (substring-no-properties (format "%s" default))))))


       (defun my-blog/html-head ()
  "Return HTML head with SEO and styling."
  (concat
   "<meta charset=\"UTF-8\">\n"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   "<meta name=\"author\" content=\"" my-blog/author "\">\n"
   "<link rel=\"canonical\" href=\"" my-blog/site-url "\">\n"
   "<link rel=\"stylesheet\" href=\"/assets/style.css\" type=\"text/css\"/>\n"
   "<link rel=\"alternate\" type=\"application/rss+xml\" href=\"/rss.xml\" title=\"RSS Feed\">\n"))

(defun my-blog/html-preamble ()
  "Return HTML preamble with navigation and branding."
  (format
   "<header class=\"blog-header\">\n"
   "<h1 class=\"blog-title\"><a href=\"/\">%s</a></h1>\n"
   "<h2 class=\"blog-subtitle\">%s</h2>\n"
   "<nav class=\"blog-nav\">\n"
   "<input type=\"checkbox\" id=\"menu-toggle\" class=\"menu-toggle\">\n"
   "<label for=\"menu-toggle\" class=\"menu-icon\">≡</label>\n"
   "<ul class=\"menu\">\n"
   "<li><a href=\"/\">Home</a></li>\n"
   "<li><a href=\"/categories/technology.html\">Technology</a></li>\n"
   "<li><a href=\"/categories/tutorials.html\">Tutorials</a></li>\n"
   "<li><a href=\"/categories/orgmode.html\">Org Mode</a></li>\n"
   "<li><a href=\"/categories/security.html\">Security</a></li>\n"
   "</ul>\n"
   "</nav>\n</header>"
   my-blog/blog-title my-blog/blog-subtitle))


(defun my-blog/html-postamble (post-plist)
  "Return HTML postamble with metadata and sharing buttons."
  (let* ((title (my-blog/safe-plist-string post-plist :title "Untitled"))
         (date (my-blog/safe-plist-string post-plist :date (format-time-string "%Y-%m-%d")))
         (author (my-blog/safe-plist-string post-plist :author my-blog/author))
         (description (my-blog/safe-plist-string post-plist :description ""))
         (file-name (my-blog/safe-plist-string post-plist :file-name ""))
         (url (concat my-blog/site-url file-name))
         ;; Fallback values
         (safe-title (if (org-string-nw-p title) title "Untitled"))
         (safe-date (if (org-string-nw-p date) date (format-time-string "%Y-%m-%d")))
         (safe-author (if (org-string-nw-p author) author my-blog/author))
         (safe-description (if (org-string-nw-p description) description safe-title)))
    (concat
     "<footer class=\"blog-footer\">\n"
     "<p class=\"author\">Author: " safe-author "</p>\n"
     "<p class=\"date\">Date: " safe-date "</p>\n"
     "<p class=\"contact\">Contact: <a href=\"mailto:mybloggingnotes@gmail.com\">mybloggingnotes@gmail.com</a></p>\n"
     "<p class=\"creator\">Generated with Emacs and Org-mode</p>\n"
    "<div class=\"share-buttons\">\n"
     "<a href=\"https://twitter.com/intent/tweet?url=" (url-encode-url url)
     "&text=" (url-encode-url safe-title) "\" target=\"_blank\">Share on Twitter</a>\n"
     "<a href=\"https://www.linkedin.com/shareArticle?url=" (url-encode-url url)
     "&title=" (url-encode-url safe-title) "\" target=\"_blank\">Share on LinkedIn</a>\n"
     "</div>\n"
     ;; Open Graph and Twitter Card metadata
     "<meta property=\"og:title\" content=\"" safe-title "\">\n"
     "<meta property=\"og:description\" content=\"" safe-description "\">\n"
     "<meta property=\"og:url\" content=\"" url "\">\n"
     "<meta name=\"twitter:card\" content=\"summary\">\n"
     "<meta name=\"twitter:title\" content=\"" safe-title "\">\n"
     "<meta name=\"twitter:description\" content=\"" safe-description "\">\n"
     "</footer>")))

(setq org-publish-project-alist
      `(("blog-posts"
         :base-directory ,my-blog/posts-dir
         :base-extension "org"
         :publishing-directory ,(concat blog-root "/posts")
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :section-numbers nil
         :with-toc nil
         :html-head ,(my-blog/html-head)
         :html-preamble ,(my-blog/html-preamble)
         :html-postamble my-blog/html-postamble
         :auto-sitemap nil
         :html-head-extra "<meta name=\"robots\" content=\"index,follow\">"
         :html-metadata t)
        ("blog-drafts"
         :base-directory ,my-blog/drafts-dir
         :base-extension "org"
         :publishing-directory ,(concat my-blog/output-dir "drafts/")
         :recursive t
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head ,(concat (my-blog/html-head) "<meta name=\"robots\" content=\"noindex,nofollow\">")
         :html-preamble ,(concat (my-blog/html-preamble) "<div class=\"draft-warning\">DRAFT</div>")
         :html-postamble my-blog/html-postamble)
        ("blog-pages"
         :base-directory ,(concat blog-org-root "/pages")
         :base-extension "org"
         :publishing-directory ,(concat blog-root "/pages")
         :recursive t
         :publishing-function org-html-publish-to-html
         :html-head ,blog-html-head
         :html-preamble t)
        ("blog-images"
         :base-directory ,my-blog/images-dir
         :base-extension "jpg\\|png\\|gif\\|webp\\|svg"
         :publishing-directory ,(concat my-blog/output-dir "images/")
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog-assets"
         :base-directory ,(concat blog-org-root "/images")
         :base-extension "jpg\|png\|gif\|svg"
         :publishing-directory ,(concat blog-root "/images")
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog-rss"
         :base-directory ,my-blog/posts-dir
         :base-extension "org"
         :publishing-directory ,my-blog/output-dir
         :publishing-function org-rss-publish-to-rss
         :html-link-home ,my-blog/site-url
         :html-link-use-abs-url t
         :rss-extension "xml"
         :rss-feed-title ,my-blog/blog-title
         :rss-feed-description ,my-blog/blog-subtitle
         :rss-feed-url ,(concat my-blog/site-url "rss.xml")
         :section-numbers nil
         :exclude ".*"
         :include ,(directory-files my-blog/posts-dir nil "^[^.].*\\.org$")
         :table-of-contents nil)
       ("blog-css"
         :base-directory ,(file-name-directory blog-css-path)
         :base-extension "css"
         :publishing-directory ,(concat blog-root "/css")
         :publishing-function org-publish-attachment)
	("blog-sitemap"
         :base-directory ,my-blog/output-dir
         :base-extension "html"
         :publishing-directory ,my-blog/output-dir
         :publishing-function my-blog/publish-sitemap
         :auto-sitemap t
         :sitemap-filename "sitemap.xml"
         :sitemap-title "Sitemap"
         :sitemap-style list)
        ("blog" :components ("blog-posts" "blog-pages" "blog-images" "blog-assets" "blog-css" "blog-rss" "blog-sitemap"))))

;;; Chapter 3: Post Management
(defun my-blog/generate-slug (title)
  "Generate a slug from TITLE."
  (downcase
   (replace-regexp-in-string
    "-+" "-"
    (replace-regexp-in-string
     "[^a-z0-9-]" "-"
     (replace-regexp-in-string
      "\\s-+" "-"
      (replace-regexp-in-string
       "[^\x00-\x7F]" ""  ; Remove non-ASCII characters
       title))))))


(defun my-blog/get-available-templates ()
  "Return list of available template names."
  (mapcar 'car my-blog/templates))

(defun my-blog/estimate-reading-time (text)
  "Estimate reading time for TEXT in minutes."
  (let ((words (count-words (point-min) (point-max))))
    (ceiling (/ words 200.0))))

(defun my-blog/create-new-post (title template-name category tags is-draft)
  "Create a new post with TITLE using TEMPLATE-NAME in CATEGORY with TAGS.
If IS-DRAFT is non-nil, create as a draft."
  (interactive
   (let* ((title (read-string "Post title: "))
          (template (completing-read "Template: "
                                     (my-blog/get-available-templates) nil t nil nil "standard"))
          (category (completing-read "Category: " my-blog/categories nil nil nil nil "blog"))
          (tags-default (mapconcat 'identity (seq-take my-blog/common-tags 3) ", "))
          (tags (read-string (format "Tags (comma-separated) [%s]: " tags-default)
                             nil nil tags-default))
          (is-draft (y-or-n-p "Create as draft? ")))
     (list title template category tags is-draft)))
  (let* ((slug (my-blog/generate-slug title))
         (date (format-time-string "%Y-%m-%d"))
         (template-content (cdr (assoc template-name my-blog/templates)))
         (target-dir (if is-draft my-blog/drafts-dir my-blog/posts-dir))
         (filename (expand-file-name (format "%s.org" slug) target-dir)))
    (if (file-exists-p filename)
        (user-error "File already exists: %s" filename)
      (with-temp-buffer
        (insert (format template-content title date my-blog/author category tags))
        (write-file filename))
      (find-file filename)
      (message "Post %s created: %s"
               (if is-draft "draft" "published") filename))))




(defun my-blog/publish-all ()
  "Publish all blog components."
  (interactive)
  ;; Temporarily disable recentf to reduce log noise
  (let ((recentf-mode (and (boundp 'recentf-mode) recentf-mode)))
    (when recentf-mode (recentf-mode -1))
    (unwind-protect
        (progn
          (my-blog/create-index-page)
          (my-blog/create-robots-txt)
          (org-publish-all t)
          (message "Blog published completely."))
      (when recentf-mode (recentf-mode 1)))))


     (defun my-blog/publish-post (filename)
  "Publish a specific post given by FILENAME."
  (interactive
   (list (read-file-name "Select post: " my-blog/posts-dir nil t nil
                         (lambda (f) (string-match-p "\\.org$" f)))))
  (unless (file-exists-p filename)
    (user-error "File not found: %s" filename))
  (org-publish-file filename t)
  (message "Post published: %s" filename))


(defun my-blog/promote-draft-to-post ()
  "Move a draft to posts and publish it."
  (interactive)
  (let* ((draft-file (read-file-name "Select draft: " my-blog/drafts-dir nil t nil
                                     (lambda (f) (string-match-p "\\.org$" f))))
         (draft-name (file-name-nondirectory draft-file))
         (target-file (expand-file-name draft-name my-blog/posts-dir)))
    (when (file-exists-p target-file)
      (user-error "A post with the same name already exists: %s" draft-name))
    (rename-file draft-file target-file)
    (find-file target-file)
    (save-buffer)
    (org-publish-file target-file t)
    (message "Draft promoted and published: %s" target-file)))

(defun my-blog/list-recent-posts ()
  "Show a list of recent posts for editing."
  (interactive)
  (let* ((files (directory-files-recursively my-blog/posts-dir "\\.org$"))
         (sorted-files (sort files
                             (lambda (a b)
                               (time-less-p (file-attribute-modification-time
                                             (file-attributes b))
                                            (file-attribute-modification-time
                                             (file-attributes a))))))
         (choice (completing-read "Select a recent post: "
                                  (mapcar 'file-name-nondirectory sorted-files))))
    (find-file (expand-file-name choice my-blog/posts-dir))))

;;; Chapter 4: Index Page and SEO
(defun my-blog/create-index-page ()
  "Create or update the blog's index page with recent posts."
  (interactive)
  (let* ((index-file (expand-file-name "index.org" my-blog/pages-dir))
         (posts (directory-files-recursively my-blog/posts-dir "\\.org$"))
         (sorted-posts (sort posts
                             (lambda (a b)
                               (time-less-p (file-attribute-modification-time
                                             (file-attributes b))
                                            (file-attribute-modification-time
                                             (file-attributes a))))))
         (recent-posts (seq-take sorted-posts 10)))
    (with-temp-buffer
      (insert (format "#+TITLE: %s\n" my-blog/blog-title))
      (insert (format "#+SUBTITLE: %s\n" my-blog/blog-subtitle))
      (insert "#+OPTIONS: toc:nil num:nil\n")
      (insert "#+DESCRIPTION: Welcome to Strategic Syntax with Emacs - Explore Emacs, Org Mode, and digital security.\n")
      (insert "#+KEYWORDS: emacs, org-mode, programming, security, tutorials\n")
      (insert "\n* Welcome to Strategic Syntax\n\n")
      (insert "Explore the universe of Emacs, Org Mode, and digital security through articles, tutorials, and strategic analyses.\n\n")
      (insert "* Recent Posts\n\n")
      (if recent-posts
          (dolist (post recent-posts)
            (with-temp-buffer
              (insert-file-contents post)
              (goto-char (point-min))
              (let ((title "Untitled")
                    (date (format-time-string "%Y-%m-%d"))
                    (excerpt "")
                    (description "")
                    (featured-image "/images/default.jpg"))
                ;; Extract title
                (when (re-search-forward "^#\\+TITLE:\\s-*\\(.*\\)$" nil t)
                  (setq title (match-string 1)))
                ;; Extract date
                (when (re-search-forward "^#\\+DATE:\\s-*\\(.*\\)$" nil t)
                  (setq date (match-string 1)))
                ;; Extract description
                (when (re-search-forward "^#\\+DESCRIPTION:\\s-*\\(.*\\)$" nil t)
                  (setq description (match-string 1)))
                ;; Extract featured image
                (when (re-search-forward "^#\\+FEATURED_IMAGE:\\s-*\\(.*\\)$" nil t)
                  (setq featured-image (match-string 1)))
                ;; Extract excerpt
                (goto-char (point-min))
                (when (re-search-forward "^\\*+ .*$" nil t)
                  (forward-line)
                  (when (looking-at "^$") (forward-line))
                  (setq excerpt
                        (replace-regexp-in-string
                         "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
                         (buffer-substring-no-properties
                          (point)
                          (min (+ (point) 200) (point-max))))))
                (with-current-buffer (get-buffer-create "*index-temp*")
                  (insert (format "** [[file:posts/%s][%s]]\n"
                                  (file-name-nondirectory (file-name-sans-extension post))
                                  title))
                  (insert (format "   /%s/\n\n" date))
                  (insert (format "   %s\n\n"
                                  (truncate-string-to-width
                                   (or description excerpt) 150 nil nil "...")))
                  (insert (format "   [[%s][View featured image]]\n\n" featured-image))))))
        (with-current-buffer (get-buffer-create "*index-temp*")
          (insert "No posts published yet.\n")))
      (insert-buffer "*index-temp*")
      (kill-buffer "*index-temp*")
      (insert "\n* Explore by Category\n\n")
      (dolist (cat my-blog/categories)
        (insert (format "- [[file:categories/%s.html][%s]]\n" cat (capitalize cat))))
      (write-file index-file))
    (message "Index page updated: %s" index-file)))

(defun my-blog/create-robots-txt ()
  "Generate robots.txt file."
  (let ((robots-file (expand-file-name "robots.txt" my-blog/output-dir)))
    (with-temp-buffer
      (insert "User-agent: *\n")
      (insert "Allow: /\n")
      (insert "Disallow: /drafts/\n")
      (insert (format "Sitemap: %ssitemap.xml\n" my-blog/site-url))
      (write-file robots-file))
    (message "robots.txt generated: %s" robots-file)))

(defun my-blog/publish-sitemap (filename project-plist &optional force)
  "Generate sitemap.xml for the blog.
FILENAME is the file being published (ignored).
PROJECT-PLIST contains project settings.
FORCE indicates whether to publish regardless of timestamps (ignored)."
  (ignore filename force)
  (let* ((output-dir (plist-get project-plist :publishing-directory))
         (sitemap-file (expand-file-name "sitemap.xml" output-dir))
         (posts (directory-files-recursively
                 (concat my-blog/output-dir "posts/") "\\.html$"))
         (pages (directory-files-recursively
                 my-blog/output-dir "\\.html$" nil
                 (lambda (f) (not (string-match-p "/posts\\|/drafts" f))))))
    (with-temp-buffer
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n")
      (dolist (file (append pages posts))
        (let ((rel-path (file-relative-name file my-blog/output-dir))
              (mtime (format-time-string
                      "%Y-%m-%d"
                      (file-attribute-modification-time (file-attributes file)))))
          (insert "  <url>\n")
          (insert (format "    <loc>%s%s</loc>\n" my-blog/site-url rel-path))
          (insert (format "    <lastmod>%s</lastmod>\n" mtime))
          (insert "    <changefreq>monthly</changefreq>\n")
          (insert "    <priority>0.8</priority>\n")
          (insert "  </url>\n")))
      (insert "</urlset>\n")
      (write-file sitemap-file))
    (message "Sitemap generated: %s" sitemap-file)))

;;; Chapter 5: Git Integration



(defun my-blog/git-status ()
  "Show the Git status of the blog."
  (interactive)
  (let ((default-directory my-blog/output-dir))
    (unless (file-exists-p (expand-file-name ".git" default-directory))
      (user-error "No Git repository found in %s" default-directory))
    (with-current-buffer (get-buffer-create "*Blog Git Status*")
      (erase-buffer)
      (insert "=== Blog Git Repository Status ===\n\n")
      (insert (shell-command-to-string "git status"))
      (insert "\n=== Recent Commits ===\n\n")
      (insert (shell-command-to-string "git log --oneline -n 5"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))



  (defun my-blog/git-commit-and-push (commit-msg)
  "Commit and push the generated blog with COMMIT-MSG."
  (interactive "sCommit message: ")
  (let ((default-directory my-blog/output-dir))
    (unless (file-exists-p (expand-file-name ".git" default-directory))
      (user-error "No Git repository found in %s" default-directory))
    (condition-case err
        (progn
          (shell-command "git add .")
          ;; Using shell-quote-argument to prevent command injection
          (shell-command (format "git commit -m %s" (shell-quote-argument commit-msg)))
          (shell-command "git push")
          (message "Blog pushed to Git with message: %s" commit-msg))
      (error
       (message "Git operation failed: %s" (error-message-string err))))))



(defun my-blog/git-init-repository ()
  "Initialize a new Git repository for the blog."
  (interactive)
  (let* ((default-directory my-blog/output-dir)
         (git-repo (read-string "Git repository URL (leave empty for local init): ")))
    (if (file-exists-p (expand-file-name ".git" default-directory))
        (message "Git repository already exists")
      (condition-case err
          (progn
            (shell-command "git init")
            (shell-command "git add .")
            (shell-command "git commit -m 'Initial blog setup'")
            (when (and git-repo (not (string-empty-p git-repo)))
              (shell-command (format "git remote add origin %s" git-repo))
              (shell-command "git push -u origin master"))
            (message "Git repository initialized"))
        (error
         (message "Failed to initialize Git repository: %s" (error-message-string err))))
      (my-blog/git-status))))



;;; Chapter 6: Text Analysis (Mock Perplexity)
(defun my-blog/analyze-perplexity ()
  "Mock text analysis for quality improvement."
  (interactive)
  (if (not (featurep 'request))
      (message "Library 'request' is not available. Install it to use this function.")
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (message "Analyzing text... (mock analysis)")
      (with-current-buffer (get-buffer-create "*Blog Analysis*")
        (erase-buffer)
        (insert "=== Text Analysis (Mock) ===\n\n")
        (insert (format "* Perplexity: %.2f/10 (lower is better)\n" (random 10.0)))
        (insert (format "* Readability: %.2f/10\n\n" (+ 5 (random 5.0))))
        (insert "* Suggestions:\n")
        (insert "  - Consider shorter sentences for better readability.\n")
        (insert "  - Add more specific examples to clarify points.\n")
        (insert "  - Check for consistent tone and style.\n")
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;; Chapter 7: Blog Statistics


(defun my-blog/collect-statistics ()
  "Collect detailed statistics about the blog."
  (interactive)
  (let ((posts (directory-files-recursively my-blog/posts-dir "\\.org$"))
        (drafts (directory-files-recursively my-blog/drafts-dir "\\.org$"))
        (pages (directory-files-recursively my-blog/pages-dir "\\.org$"))
        (categories (make-hash-table :test 'equal))
        (tags (make-hash-table :test 'equal))
        (total-words 0)
        (reading-times '())
        (publish-dates '())
        (stats-buffer (get-buffer-create "*Blog Statistics*")))
    ;; Analyze posts
    (dolist (post posts)
      (with-temp-buffer
        (insert-file-contents post)
        (let ((category nil)
              (post-tags nil)
              (words 0)
              (date nil))
          ;; Get category
          (goto-char (point-min))
          (when (re-search-forward "^#\\+CATEGORY: \\(.*\\)$" nil t)
            (setq category (match-string 1))
            (puthash category (1+ (gethash category categories 0)) categories))
          ;; Get tags
          (goto-char (point-min))
          (when (re-search-forward "^#\\+TAGS: \\(.*\\)$" nil t)
            (setq post-tags (split-string (match-string 1) "," t "[ \t]+"))
            (dolist (tag post-tags)
              (puthash tag (1+ (gethash tag tags 0)) tags)))
          ;; Get date
          (goto-char (point-min))
          (when (re-search-forward "^#\\+DATE: \\([0-9-]+\\)$" nil t)
            (setq date (match-string 1))
            (push date publish-dates))
          ;; Count words and estimate reading time
          (setq words (count-words (point-min) (point-max)))
          (setq total-words (+ total-words words))
          (push (my-blog/estimate-reading-time (buffer-string)) reading-times))))
    ;; Generate report
    (with-current-buffer stats-buffer
      (erase-buffer)
      (insert "#+TITLE: Blog Statistics\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
      (insert "* General Summary\n")
      (insert (format "- Published posts: %d\n" (length posts)))
      (insert (format "- Drafts: %d\n" (length drafts)))
      (insert (format "- Pages: %d\n" (length pages)))
      (insert (format "- Total words: %d\n" total-words))
      (insert (format "- Average words per post: %.1f\n"
                      (if (> (length posts) 0) (/ total-words (float (length posts))) 0)))
      (insert (format "- Average reading time: %.1f minutes\n"
                      (if reading-times
                          (/ (apply '+ reading-times) (float (length reading-times)))
                        0)))
      (insert "\n* Categories\n")
      (maphash (lambda (category count)
                 (insert (format "- %s: %d posts\n" category count)))
               categories)
      (insert "\n* Top Tags\n")
      (let ((sorted-tags nil))
        (maphash (lambda (tag count)
                   (push (cons tag count) sorted-tags))
                 tags)
        (setq sorted-tags (sort sorted-tags (lambda (a b) (> (cdr a) (cdr b)))))
        (dolist (tag-data (seq-take sorted-tags 10))
          (insert (format "- %s: %d times\n" (car tag-data) (cdr tag-data)))))
      (insert "\n* Publishing Frequency\n")
      (let ((dates (sort publish-dates 'string<)))
        (when dates
          (insert (format "- First post: %s\n" (car dates)))
          (insert (format "- Latest post: %s\n" (car (last dates))))
          (insert (format "- Posts in last 30 days: %d\n"
                          (length (seq-filter
                                   (lambda (d)
                                     (time-less-p
                                      (time-subtract (current-time)
                                                     (date-to-time d))
                                      (days-to-time 30)))
                                   dates))))))
      (goto-char (point-min))
      (write-file my-blog/stats-file)
      (message "Blog statistics generated in %s" my-blog/stats-file)
      (display-buffer stats-buffer))))

;;; Chapter 8: Preview and Search
  
   (defun my-blog/prepare-rss-feed ()
  "Prepare the list of files for RSS feed."
  (let* ((posts (my-blog/get-posts))
         (sorted-posts (sort (copy-sequence posts)
                            (lambda (a b)
                              (time-less-p (file-attribute-modification-time
                                          (file-attributes b))
                                         (file-attribute-modification-time
                                          (file-attributes a))))))
         (recent-posts (seq-take sorted-posts 15)))
    recent-posts))




(defun my-blog/preview ()
  "Publish the blog locally and open in browser."
  (interactive)
  (my-blog/publish-all)
  (let ((index-file (expand-file-name "index.html" my-blog/output-dir)))
    (if (file-exists-p index-file)
        (browse-url (concat "file://" index-file))
      (user-error "index.html not found in %s" my-blog/output-dir))))

(defun my-blog/preview-drafts ()
  "Publish and preview drafts."
  (interactive)
  (org-publish "blog-drafts" t)
  (let ((draft-index (expand-file-name "drafts/index.html" my-blog/output-dir)))
    (if (file-exists-p draft-index)
        (browse-url (concat "file://" draft-index))
      (browse-url (concat "file://" (expand-file-name "drafts/" my-blog/output-dir))))))

(defun my-blog/generate-search-index ()
  "Generate a JSON search index for client-side search."
  (interactive)
  (let ((posts (directory-files-recursively my-blog/posts-dir "\\.org$"))
        (index '()))
    (dolist (post posts)
      (with-temp-buffer
        (insert-file-contents post)
        (goto-char (point-min))
        (let ((title "Untitled")
              (slug (file-name-nondirectory (file-name-sans-extension post)))
              (content "")
              (tags nil)
              (category nil))
          ;; Extract title
          (when (re-search-forward "^#\\+TITLE:\\s-*\\(.*\\)$" nil t)
            (setq title (match-string 1)))
          ;; Extract category
          (when (re-search-forward "^#\\+CATEGORY:\\s-*\\(.*\\)$" nil t)
            (setq category (match-string 1)))
          ;; Extract tags
          (when (re-search-forward "^#\\+TAGS:\\s-*\\(.*\\)$" nil t)
            (setq tags (split-string (match-string 1) "," t "[ \t]+")))
          ;; Extract content
          (setq content (buffer-substring-no-properties (point-min) (point-max)))
          (push `((title . ,title)
                  (slug . ,slug)
                  (category . ,category)
                  (tags . ,tags)
                  (content . ,(truncate-string-to-width content 500)))
                index))))
    (with-temp-buffer
      (insert (json-encode index))
      (write-file (expand-file-name "assets/search-index.json" my-blog/output-dir)))
    (message "Search index generated")))

;;; Chapter 9: Transient Menu
(transient-define-prefix my-blog/menu ()
  "Menu for managing the blog."
  ["🔹 Strategic Syntax with Emacs"
   ["Create"
    ("n" "New post" my-blog/create-new-post)
    ("e" "Edit recent post" my-blog/list-recent-posts)
    ("i" "Update index page" my-blog/create-index-page)]
   ["Publish"
    ("a" "Publish all" my-blog/publish-all)
    ("p" "Publish single post" my-blog/publish-post)
    ("d" "Promote draft to post" my-blog/promote-draft-to-post)
    ("v" "Preview blog" my-blog/preview)
    ("w" "Preview drafts" my-blog/preview-drafts)]
   ["Tools"
    ("s" "Statistics" my-blog/collect-statistics)
    ("x" "Text analysis" my-blog/analyze-perplexity)
    ("j" "Generate search index" my-blog/generate-search-index)]
   ["Git"
    ("gs" "Git status" my-blog/git-status)
    ("gc" "Commit and push" my-blog/git-commit-and-push)
    ("gi" "Initialize repository" my-blog/git-init-repository)]
   ["Exit"
    ("q" "Exit" transient-quit-one)]])

(global-set-key (kbd "C-c b") #'my-blog/menu)

(provide 'blog)
;;; blog.el ends here
