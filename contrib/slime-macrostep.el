(eval-and-compile
  (require 'slime)
  (eval-and-compile
    (require 'macrostep nil t)
    ;; fallback to the bundled version.
    (require 'macrostep "../lib/macrostep")))

(define-slime-contrib slime-macrostep
  "Interactive macro stepper."
  (:authors "Jonathan Oddie <j.j.oddie@gmail.com>"
            "Luis Oliveira <loliveira@common-lisp.net>")
  (:license "GPL")
  (:swank-dependencies swank-macrostep))



(provide 'slime-macrostep)
