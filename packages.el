;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! typst-mode
  :recipe (:host github :repo "Ziqi-Yang/typst-mode.el" :files ("*.el" "dist")) )
