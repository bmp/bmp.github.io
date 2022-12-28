(require 'package)

(use-package org)
(use-package org-contrib)
(use-package htmlize)

(require 'ox-publish)

;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-postamble "<footer>
<a title=\"Twitter\" href=\"https://twitter.com/bharathmp\" target=\"_blank\"><img src=\"img/twitter.svg\" width=\"18px\" alt=\"Twitter\"></a>&nbsp;&nbsp;&nbsp;
<a title=\"Twitter\" href=\"https://www.linkedin.com/in/bharathmp/\" target=\"_blank\"><img src=\"img/linkedin.svg\" width=\"20px\" alt=\"LinkedIn\"></a>&nbsp;&nbsp;&nbsp;
<a title=\"Mastodon\" href=\"https://mastodon.sdf.org/@bmp\" target=\"_blank\"><img src=\"img/mastodon.svg\" width=\"17px\" alt=\"Mastodon\"></a>&nbsp;&nbsp;&nbsp;
<a title=\"XMPP\" href=\"xmpp:bmp@jabb3r.org\" target=\"_blank\"><img src=\"img/xmpp.svg\" width=\"18px\" alt=\"XMPP\"></a>&nbsp;&nbsp;&nbsp;
<a title=\"Email\" href=\"\" target=\"_blank\"><img src=\"img/mail.svg\" width=\"20px\" alt=\"Email\"></a>
<br>
Made with %c
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
	       :publishing-directory "./public/"
	       :recursive nil
	       :publishing-function org-html-publish-to-html
	       :auto-preamble nil
	       :auto-sitemap t
	       :with-toc nil
	       :author "Bharath M. Palavalli"
	       :email "bmp@sdf.org"
	       :html-head "<link rel=\"stylesheet\" href=\"css/style.css\">
<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"img/apple-touch-icon.png\">
<link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"img/favicon-32x32.png\">
<link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"img/favicon-16x16.png\">"
	       :html-preamble "<header>
<h1>
<a href=\"../index.html\">Bharath M. Palavalli</a>
</h1>
</header>"
	       )
	      ("static"
	       :base-directory "./static/"
	       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|svg"
	       :publishing-directory "./public/"
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
