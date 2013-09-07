(require 'org-publish)
(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/Documents/workspace.clj/thing-geom/org/"
         :base-extension "org"
         :publishing-directory "~/Documents/workspace.clj/thing-geom/org/html/"
         :recursive f
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble f)
        ("org-static"
         :base-directory "~/Documents/workspace.clj/thing-geom/org/assets/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Documents/workspace.clj/thing-geom/org/html/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("all" :components ("org-notes" "org-static"))))
