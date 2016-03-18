(setq org-agenda-files
      (-mapcat
       (lambda (module)
         (let ((dir (concat default-directory "src/" module "/")))
           (-map (lambda (f) (concat dir f)) (directory-files dir nil "\\.org$"))))
       (list "core" "types" "mesh" "physics" "utils" "svg" "viz" "webgl" "")))
