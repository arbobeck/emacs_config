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

(defun my/org-open-bandcamp-with-emms (url _)
  "Play Bandcamp URL via EMMS instead of opening a browser."
  (emms-play-url url))

(after! org
  (org-link-set-parameters
   "https"
   :follow
   (lambda (url arg)
     (if (string-match-p "bandcamp\\.com" url)
         (my/org-open-bandcamp-with-emms url arg)
       (browse-url url)))))

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

;; Require Elpher for Gemini and EMMS for Youtube
(use-package! elpher
  :commands (elpher-go elpher-mode))

(use-package! emms
  :config
  (require 'emms-setup)
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)))

;; Require Elpher for Gemini and EMMS for Youtube
(use-package! elpher
  :commands (elpher-go elpher-mode))

(use-package! emms
  :config
  (require 'emms-setup)
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)))

;; Links.Org
(setq my/links-file (expand-file-name "~/links.org"))

(defun my/open-link ()
  "Open link at point with appropriate app (elpher/emms)."
  (interactive)
  (let ((link (org-element-property :raw-link (org-element-context))))
    (message "Debug - Link found: %s" link)
    (if (not link)
        (message "No link at point")
      (cond
       ((string-prefix-p "gemini://" link)
        (message "Opening with elpher: %s" link)
        (elpher-go link))
       ((string-match-p "bandcamp.com" link)
        (message "Opening with emms: %s" link)
        (emms-play-url link))
       (t 
        (message "Opening with browse-url: %s" link)
        (browse-url link))))))

(defun my/add-link ()
  "Add link to links.org under current heading."
  (interactive)
  (let ((url (read-string "URL: "))
        (title (read-string "Title: "))
        (category (completing-read "Category: " '("Gemini" "Bandcamp"))))
    (find-file my/links-file)
    (goto-char (point-min))
    (re-search-forward (concat "^\\* " category))
    (org-end-of-subtree)
    (unless (bolp) (insert "\n"))
    (insert (format "** [[%s][%s]]\n" url title))
    (save-buffer)))

(defun my/browse-links ()
  "Open links.org and jump to link selection."
  (interactive)
  (find-file my/links-file)
  (consult-org-heading))

;; Advise org-open-at-point to intercept link opening in links.org
(defun my/advice-org-open-at-point (orig-fun &rest args)
  "Advice to intercept link opening in links.org."
  (if (and (buffer-file-name)
           (string= (buffer-file-name) my/links-file)
           (org-in-regexp org-link-any-re))
      (my/open-link)
    (apply orig-fun args)))

(advice-add 'org-open-at-point :around #'my/advice-org-open-at-point)

;; Setup keybindings for links.org
(defun my/setup-links-org-mode ()
  "Setup custom keybindings for links.org."
  (when (and (buffer-file-name) 
             (string= (buffer-file-name) my/links-file))
    (local-set-key (kbd "RET") #'my/open-link)
    (message "Custom link handlers enabled for links.org")))

(add-hook 'org-mode-hook #'my/setup-links-org-mode)

;; Leader keybindings
(map! :leader
      :prefix "n"
      :desc "Browse links" "l" #'my/browse-links
      :desc "Add link" "L" #'my/add-link)


;; Usage

;; 1. **`SPC n l`** → opens links.org, fuzzy search headings
;; 2. **Navigate** → `RET` on a link opens with elpher/emms
;; 3. **`SPC n L`** → prompts for URL/title/category, auto-adds to file

;; Workflow

;;SPC n l → type "midnight" → RET → opens in elpher
;;SPC n L → paste gemini URL → "Cool Site" → "Gemini" → saved

;; Override SPC f f to always start at home directory
(map! :leader
      :desc "Find file from home" "f f" 
      (lambda () (interactive) 
        (let ((default-directory "~/"))
          (call-interactively #'find-file))))


(after! pdf-tools
  ;; Force pdf-view-mode for PDFs
  (setf (alist-get "\\.pdf\\'" auto-mode-alist nil nil #'string-match-p)
        #'pdf-view-mode)

  ;; Enable night mode
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode))

(defun my/gemini-new-post ()
  (interactive)
  (let* ((category (completing-read "Category: " '("politics" "theology" "philosophy")))
         (title (read-string "Title: "))
         (slug (replace-regexp-in-string " " "-" (downcase title)))
         (date (format-time-string "%Y-%m-%d")))
    (find-file
     (format "~/gemini/blog/posts/%s/%s-%s.gmi" category date slug))
    (insert (format "# %s\n\n%s\n\n" title date))))

(defun my/gemini-publish-terminal ()
  (interactive)
  (let ((default-directory "~/gemini/blog/"))
    (ansi-term "/bin/bash" "gemini-publish")))

;; add margin and visual-line-mode to text files (like .gmi)
(after! text-mode
  (add-hook 'text-mode-hook
            (lambda ()
              (visual-line-mode 1)
              (setq-local fill-column 80))))
