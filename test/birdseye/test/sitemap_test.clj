(ns birdseye.test.sitemap-test
  (:require [birdseye.sitemap :refer :all]
            [birdseye.macros :refer [defsitemap gen-sitemap]]
            [clojure.test :refer :all]))

(def nil-error-map {:error :nil
                    :message "nil is not a valid node-key"})

(defmacro assert-form-match
  ([in out] `(assert-form-match ~in ~out nil))
  ([in out msg]
     `(do
        (is (= (-match-sitemap-forms ~in) ~out) ~msg)
        ;; test with trailing forms
        (is (= (-match-sitemap-forms (concat ~in [:trash :trash2])) ~out) ~msg)
        (is (= (-match-sitemap-forms (concat ~in [nil nil nil])) ~out) ~msg))))

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

(defn- assert-basic-sitemap-props [sm]
  (is (sitemap? sm))
  (is (every? map? (vals sm)))
  (is (every? keyword? (keys sm))))

(deftest test-gen-sitemap
  (let [sm (gen-sitemap
             [home
             users
             users.$userid
             users.$userid.edit])]
    (assert-basic-sitemap-props sm)
    (is (= 4 (-> sm keys count))))

  (doseq [sm [(gen-sitemap
                [home
                foo
                foo.bar
                foo.bar.asdf
                users
                users.$userid {:2 (+ 1 1)}
                users.$userid.edit {}])
              (gen-sitemap
                [users ;; same as above but with relative sub-nodes.
                [.$userid {:2 (+ 1 1)}
                 .$userid.edit {}]])]]
    (do
      (assert-basic-sitemap-props sm)
      (is (every? empty? (vals (dissoc sm :users.$userid))))
      (is (= 2 (get-in sm [:users.$userid :2]))))))

(deftest test-relative-sub-maps
  ;; test map of relative sub-nodes
  ;; and insertion of that map into a parent map
  (let [sm (gen-sitemap
            [.foo
             .foo.bar
             .users
             .users.$userid {:2 (+ 1 1)}
             .users.$userid.edit])
        ;; TODO: convert this = to a syntax quote like in crux.
        sm2 (gen-sitemap [toplevel =sm])]
    (is (relative-sitemap? sm))
    (is (not (relative-sitemap? sm2)))
    (is (absolute-sitemap? sm2))
    (assert-basic-sitemap-props sm)
    (assert-basic-sitemap-props sm2)
    (is (= 2 (get-in sm [:.users.$userid :2])))
    (is (= 2 (get-in sm2 [:toplevel.users.$userid :2])))
    (is (= 5 (-> sm keys count)))
    (is (= 6 (-> sm2 keys count)))
    ;; (is false)
    ))

(deftest test-symbol-value-insertion
  (let [node-key :just-testing
        node-key2 :just-testing.foo
        context-map {:a 1}
        ;; TODO: convert this = to a syntax quote like in crux.
        sm (gen-sitemap [=node-key {} =node-key2 =context-map])]
    (assert-basic-sitemap-props sm)
    (is (= sm {:just-testing {}
               :just-testing.foo context-map}))))

(deftest test-invalid-map-exceptions
  (is (thrown-with-msg? Exception #"must not end in a dot"
        (gen-sitemap [foo.bar.])))

  (is (thrown-with-msg? Exception #"Invalid.*position 0"
        (gen-sitemap [{} {}])))

  (is (thrown-with-msg? Exception #"Invalid .* position 3"
        (gen-sitemap [foo bar {} {}]) ))

  (is (thrown-with-msg? Exception #"Invalid .* position 0"
        (gen-sitemap [nil bar])))
  (is (thrown-with-msg? Exception #"Invalid .* position 1"
        (gen-sitemap [bar nil])))
  (is (thrown-with-msg? Exception #"Invalid .* position 1"
        (gen-sitemap [bar false])))

  (is (thrown-with-msg? Exception #"Parent node .* does not exist"
        (gen-sitemap [foo.bar]))))
