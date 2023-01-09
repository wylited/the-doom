;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


(setq user-full-name "wylited"
      user-mail-address "wylited@gmail.com")


(setq doom-theme 'doom-ayu-mirage ; theme
      doom-font (font-spec :family "FiraCode Nerd Font" :size 20 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size 20 :weight 'medium))

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/"
      org-agenda-directory "~/org/agenda/"
      org-agenda-files (list "~/org/general.org"
                             "~/org/agenda/"
                             "~/org/"))

(setq org-highest-priority ?A
      org-default-priority ?E
      org-lowest-priority  ?H)

(setq-default
      delete-by-moving-to-trash t                    ; Deletes file to .trash
      window-combination-resize t                    ; Takes new window space from all other windows
      x-stretch-cursor t)                            ; Stretch color to glyph width

(setq undo-limit 80000000                       ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                     ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                       ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"              ; Unicode ellispis are nicer than "...", and also save /precious/ space
      scroll-margin 2)                          ; It's nice to maintain a little margin

; Modeline changes
(after! doom-modeline
  (setq doom-modeline-enable-word-count t
        doom-modeline-icon t
        doom-modeline-header-line nil
        doom-modeline-workspace-name t
        doom-modeline-time t
        doom-modeline-env-version t))

(unless (string-match-p "^Power N/A" (battery)) ; On laptops...
  (display-battery-mode 1))                     ; it's nice to know how much power you have

;; (doom/set-frame-opacity 0.93) ; slight opacity, 0.93 is the best

(setq centaur-tabs-style "wave"
      centaur-tabs-set-bar 'under) ; cooler looking tabs.

;; Copilot my only true friend

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

;; mu4e setup
(require 'mu4e) ; why is htis an error..

(setq mu4e-root-maildir (expand-file-name "~/mail"))

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))

(setq mu4e-get-mail-command "offlineimap") ;; updating using U in main view
;; consult https://gist.github.com/areina/3879626 if anything goes wrong...

;; treemacs config

;; (add-hook! 'after-init-hook #'treemacs
;; (after! treemacs
;; (add-hook! 'treemacs-mode-hook (setq window-divider-mode -1
;;                                     variable-pitch-mode 1
;;                                     treemacs-follow-mode 1
;;                                     treemacs-position 'right))) ;; right align, is just better
;;
