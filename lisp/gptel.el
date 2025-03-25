;; Copyright (C) 2024 Jamie Cui
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-package! gptel
  :config
  (setq gptel-model   'deepseek-r1
        gptel-default-mode 'org-mode
        gptel-org-branching-context 't
        gptel-log-level 'info
        gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "dashscope.aliyuncs.com/compatible-mode/v1"
          :endpoint "/chat/completions"
          :stream t
          :key "sk-********************"
          :models '(deepseek-r1)))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@jamie\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@remote-ai\n")

  (setq gptel-display-buffer-action
        '((display-buffer-in-side-window)
          (side . bottom)
          (window-height . 0.3)))
  (map!
   :leader
   :desc "Bring up (gptel)" "RET" #'gptel))
