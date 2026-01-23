;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq vterm-module-cmake-args "")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; remove menu on splash screen
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(set-frame-parameter (selected-frame) 'alpha '(90 . 90)) (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(map! :leader
      :desc "Treemacs" "o e" #'treemacs)

;; Load all-the-icons and install fonts if needed
(use-package! all-the-icons
  :config
  ;; install fonts if not already installed
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

;; Enable treemacs with icons
(use-package! treemacs
  :after all-the-icons
  :config
  (require 'treemacs-all-the-icons)
  ;; enable all-the-icons theme
  (treemacs-load-theme "all-the-icons"))

(after! treemacs
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t))

(after! ligature
	(global-ligature-mode +1))

(setq doom-font (font-spec :family "0xProto Nerd Font" :size 14))

(after! projectile
  (setq projectile-switch-project-action
        (lambda ()
          (let ((project-root (projectile-project-root)))
            ;; only switch if the workspace system is ready
            (when (fboundp '+workspace-switch)
              (+workspace-switch project-root t))
            ;; add project to Treemacs
            (when (fboundp 'treemacs)
              (treemacs-add-project-to-workspace project-root))
            ;; fallback: open project root in Dired
            (dired project-root)))))

;;; EMMS YouTube + MPV Setup
(use-package! emms
  :defer t
  :config
  ;; Enable the core EMMS modules you need
  (require 'emms-setup)
  (require 'emms-player-mpv)
  
  ;; Setup MPV as player
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-player-mpv-parameters '("--no-video" "--no-terminal" "--quiet"))
  
  ;; Basic EMMS setup
  (emms-all)
  (emms-default-players))

;; Allow ctrl-shift-v to paste in vterm

(after! vterm
  (map! :map vterm-mode-map
        "C-S-v" #'vterm-yank))
;; Red DOOM logo
(custom-set-faces!
  '(doom-dashboard-banner
     :foreground "red"
     :weight bold))

;; Red footer text + icon
(custom-set-faces!
  '(doom-dashboard-loaded
     :foreground "red"
     :weight bold
     :height 1.1))

;; display time in the mode line
(setq display-time-24hr-format 1)
(setq display-time-day-and-date 1)
(display-time-mode 1)

(use-package ellama
  :ensure t
  :init
  (setq ellama-chat-model "qwen2.5-coder:7b"
        ellama-host "127.0.0.1:11434"
        ellama-memory-file (expand-file-name "~/llm/memory.org")
        ellama-sessions-directory (expand-file-name "~/llm/sessions/")))

(global-set-key (kbd "C-c c") 'ellama-chat)

(defun my/inject-memory (orig-fun &rest args)
  "Inject memory into all ellama prompts."
  (let* ((memory (with-temp-buffer
                   (insert-file-contents ellama-memory-file)
                   (buffer-string)))
         (original-prompt (car args))
         (new-prompt (format "[Context: %s]\n\n%s" memory original-prompt)))
    (apply orig-fun (cons new-prompt (cdr args)))))

(advice-add 'ellama-chat :around #'my/inject-memory)

;; Now you only need one keybinding
(map! :leader
      :prefix "n"
      :desc "Chat" "c" #'ellama-chat)

;; Links.Org
(setq my/links-file (expand-file-name "~/links.org"))

(defun my/open-link ()
  "Open link at point with appropriate app (elpher/emms)."
  (interactive)
  (let ((link (org-element-property :raw-link (org-element-context))))
    (cond
     ((string-prefix-p "gemini://" link)
      (elpher-go link))
     ((string-match-p "youtube.com\\|youtu.be" link)
      (emms-play-url link))
     (t (browse-url link)))))

(defun my/add-link ()
  "Add link to links.org under current heading."
  (interactive)
  (let ((url (read-string "URL: "))
        (title (read-string "Title: "))
        (category (completing-read "Category: " '("Gemini" "YouTube"))))
    (find-file my/links-file)
    (goto-char (point-min))
    (re-search-forward (concat "^\\* " category))
    (org-end-of-subtree)
    (insert (format "** [[%s][%s]]\n" url title))
    (save-buffer)))

(defun my/browse-links ()
  "Open links.org and jump to link selection."
  (interactive)
  (find-file my/links-file)
  (counsel-org-goto)) ; or consult-org-heading if you use consult

(map! :leader
      :prefix "n"
      :desc "Browse links" "l" #'my/browse-links
      :desc "Add link" "L" #'my/add-link)

;; In links.org, press RET on a link to open it
(add-hook 'org-mode-hook
          (lambda ()
            (when (string= (buffer-file-name) my/links-file)
              (local-set-key (kbd "RET") #'my/open-link))))

;; Usage

;; 1. **`SPC n l`** → opens links.org, fuzzy search headings
;; 2. **Navigate** → `RET` on a link opens with elpher/emms
;; 3. **`SPC n L`** → prompts for URL/title/category, auto-adds to file

;; Workflow

;;SPC n l → type "midnight" → RET → opens in elpher
;;SPC n L → paste gemini URL → "Cool Site" → "Gemini" → saved
