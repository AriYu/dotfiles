;; cask and pallet
(when (or (require 'cask "~/.cask/cask.el" t)
	  (require 'cask nil t))
  (cask-initialize))

(require 'use-package)
(pallet-mode t)
