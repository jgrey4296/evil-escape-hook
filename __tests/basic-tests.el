;;; basic-tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'buttercup)
(require 'evil-escape-hook)

(describe "sanity"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "has the escape test" (expect (functionp 'evil-escape-p) :to-be t))
)
