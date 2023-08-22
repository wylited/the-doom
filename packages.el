;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; (package! ivy-postframe)

(package! typst-mode
  :recipe (:host github :repo "Ziqi-Yang/typst-mode.el" :files ("*.el" "dist")) )

(package! org-habit-stats)

(package! org-modern)

(package! org-super-agenda)

(package! org-pomodoro)

(package! org-ql)

(package! org-sidebar)

(package! org-roam)

(package! org-roam-ui)

(package! org-fancy-priorities)

(package! lsp-java)

;; (package! dash-functional)

(package! dash)

(package! yasnippet-snippets)

(package! elcord)
