(require 'org-publish)
(setq org-publish-project-alist
      '(
        ("org-notes"
         :base-directory "~/Documents/workspace.clj/toxi2/org/"
         :base-extension "org"
         :publishing-directory "~/Documents/workspace.clj/toxi2/org/html/"
         :recursive f
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble f)

        ("org-static"
         :base-directory "~/Documents/workspace.clj/toxi2/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Documents/workspace.clj/toxi2/org/html/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("org" :components ("org-notes" "org-static"))

        ))

(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "~/org/"
         :base-extension "org"
         :publishing-directory "/ssh:user@host:~/html/notebook/"
         :publishing-function org-html-publish-to-html
         :exclude "PrivatePage.org"   ;; regexp
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\"
                       href=\"../other/mystyle.css\" type=\"text/css\"/>"
         :html-preamble t)
        ("images"
         :base-directory "~/images/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "/ssh:user@host:~/html/images/"
         :publishing-function org-publish-attachment)
        ("other"
         :base-directory "~/other/"
         :base-extension "css\\|el"
         :publishing-directory "/ssh:user@host:~/html/other/"
         :publishing-function org-publish-attachment)
        ("website" :components ("orgfiles" "images" "other"))))
