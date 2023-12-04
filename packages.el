;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! typst-mode
  :recipe (:host github :repo "Ziqi-Yang/typst-mode.el" :files ("*.el" "dist")))

(package! typst-ts-mode
  :recipe (:host sourcehut :repo "meow_king/typst-ts-mode" :files ("*.el" "dist")))

(package! org-habit-stats)

(package! org-modern)

(package! org-super-agenda)

(package! org-pomodoro)

(package! org-ql)

(package! org-sidebar)

(package! org-roam)

(unpin! org-roam)

(package! org-roam-ui
  :recipe (:host github :repo "jgru/org-roam-ui" :branch "add-export-capability" :files ("*.el" "out")))

(package! lsp-java)

;; (package! dash-functional)

(package! dash)

(package! yasnippet-snippets)

(package! elcord)

(package! rust-mode)

(package! anki-editor)

(package! org-anki)

(package! anki-connect)

(package! nano-modeline)

(package! ox-json)

(package! yuck-mode)

(package! ement)

(package! platformio-mode)

(package! oj)
