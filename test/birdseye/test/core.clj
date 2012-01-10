(ns birdseye.test.core
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
             users nil                  ; nil allowed in place of {}
             users.$userid {:2 (+ 1 1)}
             users.$userid.edit {}
             )]
    (assert-basic-site-map-props sm)
    (every? empty? (dissoc sm :users.$userid))
    (is (= 2 (get-in sm [:users.$userid :2]))))

  (is (thrown-with-msg? Exception #"Was expecting"
        (defsitemap {} {}) ))

  (is (thrown-with-msg? Exception #"Was expecting .* position 3"
        (defsitemap foo bar {} {}) ))

  (is (thrown-with-msg? Exception #"Was expecting .* position 2"
        (defsitemap foo nil nil bar)))

  (is (thrown-with-msg? Exception #"Parent node .* does not exist"
        (defsitemap foo.bar) )))
