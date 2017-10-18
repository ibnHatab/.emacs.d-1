(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#373b41"))
 '(coffee-tab-width 4)
 '(custom-safe-themes
   (quote
    ("cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "f6a935e77513ba40014aa8467c35961fdb1fc936fa48407ed437083a7ad932de" "125fd2180e880802ae98b85f282b17f0aa8fa6cb9fc4f33d7fb19a38c40acef0" "dc9a8d70c4f94a28aafc7833f8d05667601968e6c9bf998791c39fcb3e4679c9" "5a970147df34752ed45bfdf0729233abfc085d9673ae7e40210c5e2d8f624b08" "65d9573b64ec94844f95e6055fe7a82451215f551c45275ca5b78653d505bc42" "7f3ef7724515515443f961ef87fee655750512473b1f5bf890e2dc7e065f240c" "2b6bd2ebad907ee42b3ffefa4831f348e3652ea8245570cdda67f0034f07db93" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(indent-tabs-mode nil)
 '(linum-format " %7i ")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-support-shift-select (quote always))
 '(package-selected-packages
   (quote
    (exec-path-from-shell buttercup bpr gtags evil-org evil-indent-textobject evil-surround evil-leader magit highlight-symbol which-key yaml-mode mmm-mode zenburn-theme color-theme-sanityinc-tomorrow gruvbox-theme sublime-themes web-mode comment-dwim-2 powerline-evil evil powerline ag wgrep-ag wgrep markdown-mode helm-projectile flycheck emmet-mode dictionary counsel helm coffee-mode elpy pug-mode s nov use-package fullframe)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pug-tab-width 2)
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values (quote ((no-byte-compile t))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(whitespace-style
   (quote
    (face trailing tabs spaces lines newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 16777215)) (:background "#282828" :foreground "#fdf4c1")) (((class color) (min-colors 255)) (:background "#262626" :foreground "#ffffaf")))))
