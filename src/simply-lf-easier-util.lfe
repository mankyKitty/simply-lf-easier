(defmodule simply-lf-easier-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'simply-lf-easier))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(simply-lf-easier ,(get-version)))))
