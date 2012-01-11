(ns birdseye.test.core
  (:refer-clojure :exclude [sym])
  (:use [birdseye.core])
  (:use [clojure.test]))

(defn- assert-basic-site-map-props [sm]
  (is (every? map? (vals sm)))
  (is (every? keyword? (keys sm))))

(deftest test-defsite-map
  (let [sm (defsitemap
             home
             users
             users.$userid
             users.$userid.edit)]
    (assert-basic-site-map-props sm)
    (is (= 4 (-> sm keys count))))

  (let [sm (defsitemap
             home
             foo
             foo.bar
             foo.bar.asdf
             users
             users.$userid {:2 (+ 1 1)}
             users.$userid.edit {}
             )]
    (assert-basic-site-map-props sm)
    (every? empty? (dissoc sm :users.$userid))
    (is (= 2 (get-in sm [:users.$userid :2]))))

  (is (thrown-with-msg? Exception #"must not end in a dot"
        (defsitemap foo.bar.) ))

  (is (thrown-with-msg? Exception #"Invalid.*position 0"
        (defsitemap {} {}) ))

  (is (thrown-with-msg? Exception #"Invalid .* position 3"
        (defsitemap foo bar {} {}) ))

  (is (thrown-with-msg? Exception #"Invalid .* position 0"
        (defsitemap nil bar)))
  (is (thrown-with-msg? Exception #"Invalid .* position 1"
        (defsitemap bar nil)))
  (is (thrown-with-msg? Exception #"Invalid .* position 1"
        (defsitemap bar false)))

  (is (thrown-with-msg? Exception #"Parent node .* does not exist"
        (defsitemap foo.bar) ))
  )

(defmacro assert-form-match
  ([in out] `(assert-form-match ~in ~out nil))
  ([in out msg]
     `(do
        (is (= (match-forms ~in) ~out) ~msg)
        ;; test with trailing forms
        (is (= (match-forms (concat ~in [:trash :trash2])) ~out) ~msg)
        (is (= (match-forms (concat ~in [nil nil nil])) ~out) ~msg))))

(def nil-error-map {:error :nil
                    :message "nil is not a valid node-key"})

(deftest test-form-matching
  (assert-form-match [:foo] {:node-key :foo})
  (assert-form-match (list :foo) {:node-key :foo})
  (assert-form-match [:foo [:.bar] :bar]
                     {:node-key :foo :children [:.bar]})
  (assert-form-match [:foo {:a 1234}]
                     {:node-key :foo :context-map {:a 1234}})
  (assert-form-match [:foo {:a 1234} [:.bar]]
                     {:node-key :foo
                      :context-map {:a 1234}
                      :children [:.bar]})

  (assert-form-match [nil] nil-error-map)
  (assert-form-match ['error] {:error 'error})
  (assert-form-match [true] {:error true})
  (assert-form-match [:nil] nil-error-map))
