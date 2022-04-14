(setq site-run-file nil ; Disable site-wide run-time initialization
      inhibit-default-init t)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      window-resize-pixelwise t
      inhibit-compacting-font-caches t ; Do not compact font caches during GC
      ;; Prefer new files to avoid loading stable bytecode
      load-prefer-newer t)

(provide 'early-init)
