(setq org-agenda-files
      (-mapcat
       (lambda (base)
         (-mapcat
          (lambda (module)
            (let ((dir (concat default-directory base "/" module "/")))
              (if (file-directory-p dir)
                  (directory-files dir t "\\.org$"))))
          (list "core" "mesh" "physics" "svg" "types" "utils" "viz" "voxels" "webgl" "")))
       (list "src" "test" "examples")))
