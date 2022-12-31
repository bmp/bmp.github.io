(require 'package)

(use-package org)
(use-package org-contrib)
(use-package htmlize)

(require 'ox-publish)


;; Open external links in new tab
(defun my-org-export-add-target-blank-to-http-links (text backend info)
  "Add target=\"_blank\" to external links."
  (when (and
         (org-export-derived-backend-p backend 'html)
         (string-match "href=\"http[^\"]+" text)
         (not (string-match "target=\"" text)))
    (string-match "<a " text)
    (replace-match "<a target=\"_blank\" " nil nil text)))

(add-to-list 'org-export-filter-link-functions
             'my-org-export-add-target-blank-to-http-links)


;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-postamble "<footer>
<a title=\"Twitter\" href=\"https://twitter.com/bharathmp\" target=\"_blank\"><img class=\"logo-img\" src=\"img/twitter.svg\" style=\"width:18px;\" alt=\"Twitter\"></a>&nbsp;&nbsp;&nbsp;
<a title=\"LinkedIn\" href=\"https://www.linkedin.com/in/bharathmp/\" target=\"_blank\"><img class=\"logo-img\" src=\"img/linkedin.svg\" style=\"width:20px;\" alt=\"LinkedIn\"></a>&nbsp;&nbsp;&nbsp;
<a title=\"Mastodon\" rel=\"me\" href=\"https://mastodon.sdf.org/@bmp\" target=\"_blank\"><img class=\"logo-img\" src=\"img/mastodon.svg\" style=\"width:17px;\" alt=\"Mastodon\"></a>&nbsp;&nbsp;&nbsp;
<a title=\"XMPP\" href=\"xmpp:bmp@jabb3r.org\" target=\"_blank\"><img class=\"logo-img\" src=\"img/xmpp.svg\" style=\"width:18px;\" alt=\"XMPP\"></a>&nbsp;&nbsp;&nbsp;
<a title=\"Email\" href=\"&#109;&#97;&#105;&#108;&#116;&#111;&#58;&#109;&#97;&#105;&#108;&#64;&#98;&#104;&#97;&#114;&#97;&#116;&#104;&#112;&#97;&#108;&#97;&#118;&#97;&#108;&#108;&#105;&#46;&#99;&#111;&#109;\" target=\"_blank\"><img class=\"logo-img\" src=\"img/mail.svg\" style=\"width:20px;\" alt=\"Email\"></a>
<br>
<div class=\"generator\">
<a href=\"https://validator.w3.org/nu/?doc=https://www.bharathpalavalli.com/\">HTML5</a> | <a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> <a href=\"https://orgmode.org\">Org Mode</a> | <a href=\"https://www.bharathpalavalli.com/humans.txt\">Humans.txt</a>
<br>
© 2009-2022 Bharath M. Palavalli. Some rights reserved.
</div>
</footer>"
      org-export-with-footnotes t
      org-export-date-timestamp-format "%d %B %Y"
      org-html-metadata-timestamp-format "%d %B %Y"
      org-export-global-macros
      '(("timestamp" . "@@html:<time style=\"font-size:0.8em; margin-left:0.6em;\">$1</time>@@"))
      )

(defun sitemaplisting (entry style project)
  "Format ENTRY in org-publish PROJECT Sitemap format ENTRY ENTRY STYLE format that includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "[[file:%s][%s]] {{{timestamp(%s)}}}"
	      entry filename (format-time-string "%d %B %Y"
						 (org-publish-find-date entry project))))))

(setq org-publish-project-alist
      '(
	      ("content"
	       :base-directory "./content/"
	       :base-extension "org"
	       :publishing-directory "../"
	       :recursive nil
	       :publishing-function org-html-publish-to-html
	       :auto-preamble nil
	       :auto-sitemap t
	       :with-toc nil
	       :author "Bharath M. Palavalli"
	       :email "bmp@sdf.org"
	       :html-head "<link rel=\"stylesheet\" href=\"css/style.min.css\">
<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"img/apple-touch-icon.png\">
<link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"img/favicon-32x32.png\">
<link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"img/favicon-16x16.png\">"
	       :html-preamble "<header>
<h1>
<a href=\"./index.html\">Bharath M. Palavalli</a>
</h1>
</header>"
	       )
	      ("static"
	       :base-directory "./static/"
	       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|svg"
	       :publishing-directory "../"
	       :recursive t
	       :publishing-function org-publish-attachment
	       )
	      ("bmpWebsite"
	       :components ("content" "static")
	       )
        ))

;; Only to be used when metadata or generating functions for website changes
;; otherwise use M-x org-publish-project
(org-publish "bmpWebsite" t)
