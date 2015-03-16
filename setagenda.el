(setq org-agenda-files
      (-mapcat
       (lambda (module)
         (let ((dir (concat default-directory module "/src/")))
           (-map (lambda (f) (concat dir f)) (directory-files dir nil "\\.org$"))))
       (list "geom-core" "geom-types" "geom-meshops" "geom-physics" "geom-svg" "geom-webgl" "")))

