(require 'jq)

(ert-deftest jq-test--jq ()
  (let* ((input "
[
  {
    \"name\": \"Ness\",
    \"age\": 12,
    \"origin\": { \"country\": \"Eagleland\", \"town\": \"Onett\" }
  },
  {
    \"name\": \"Paula\",
    \"age\": 11,
    \"origin\": { \"country\": \"Eagleland\", \"town\": \"Twoson\" }
  }
]
")
         (program ".[] | {town: .origin.town}")
         (actual (cl-loop for x iter-by (jq input program)
                          collect x)))
    (should (= (length actual) 2))
    (should (string= (cdr (assoc 'town (nth 0 actual))) "Onett"))
    (should (string= (cdr (assoc 'town (nth 1 actual))) "Twoson"))))

(ert-deftest jq-test--jq--signaling-on-invalid-json-string ()
  (let* ((input "
[
  {
    \"name\": \"Ness\",
    \"age\": 12
    \"origin\": { \"country\": \"Eagleland\", \"town\": \"Onett\" }
  }
]
")
         (program ".[] | {town: .origin.town}")
         err)
    (condition-case e
        (iter-next (jq input program))
      (error (setq err e)))

    (should (eq (car err) 'error))
    (should
     (string=
      (cdr err)
      "jq: error: Expected separator between values at line 6, column 12"))))
