(ns birdseye.test.core
  (:refer-clojure :exclude [sym])
  (:require [birdseye.core :refer :all])
  (:require [clojure.test :refer :all]))

(def nil-error-map {:error :nil
                    :message "nil is not a valid node-key"})

(def match-sitemap-forms
  ;; is private
  (ns-resolve 'birdseye.core 'match-sitemap-forms))

(defmacro assert-form-match
  ([in out] `(assert-form-match ~in ~out nil))
  ([in out msg]
     `(do
        (is (= (match-sitemap-forms ~in) ~out) ~msg)
        ;; test with trailing forms
        (is (= (match-sitemap-forms (concat ~in [:trash :trash2])) ~out) ~msg)
        (is (= (match-sitemap-forms (concat ~in [nil nil nil])) ~out) ~msg))))

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
        sm2 (gen-sitemap [toplevel =sm])]
    (is (relative-sitemap? sm))
    (is (not (relative-sitemap? sm2)))
    (is (absolute-sitemap? sm2))
    (assert-basic-sitemap-props sm)
    (assert-basic-sitemap-props sm2)
    (is (= 2 (get-in sm [:.users.$userid :2])))
    (is (= 2 (get-in sm2 [:toplevel.users.$userid :2])))
    (is (= 5 (-> sm keys count)))
    (is (= 6 (-> sm2 keys count)))))

(deftest test-symbol-value-insertion
  (let [node-key :just-testing
        node-key2 :just-testing.foo
        context-map {:a 1}
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

(deftest test-node-to-url
  (let [sm (gen-sitemap
             [home
             users
             users.$userid
             users.$userid.edit
             users.$userid.comments
             users.$userid.comments.$cid]
             )
        ring-app (gen-ring-app sm)
        test-url (fn [url k m]
                   (= url (node-to-url ring-app k m)))]
    (is (test-url "/" :home {}))
    (is (test-url "/users/" :users {}))
    (is (test-url "/users/123/" :users.$userid
                  {:userid 123}))
    (is (test-url "/users/123/edit/"
                  :users.$userid.edit
                  {:userid 123}))
    (is (test-url "/users/123/comments/99/"
                  :users.$userid.comments.$cid
                  {:userid 123
                   :cid 99}))))

(defmacro assert-url-to-node [ring-app url node-key & params]
  `(is (= [~node-key ~(apply hash-map params)]
          (url-to-node ~ring-app ~url))))

(deftest test-url-to-node
  (let [sm (gen-sitemap
             [home
             users
             users.active
             users.active.by_join_date
             users.$userid
             users.$userid.edit
             users.$userid.comments
             users.$userid.comments.$cid]
             )
        ring-app (gen-ring-app sm)]
    (assert-url-to-node ring-app "/asdf" :404)
    (assert-url-to-node ring-app "/" :home)
    (assert-url-to-node ring-app "/users/" :users)
    (assert-url-to-node ring-app "/users/active/" :users.active)
    (assert-url-to-node ring-app "/users/active/by_join_date/"
                        :users.active.by_join_date)
    (assert-url-to-node
     ring-app "/users/1234/" :users.$userid :userid "1234")
    (assert-url-to-node
     ring-app "/users/1234/edit/" :users.$userid.edit
     :userid "1234")))

(deftest test-ring-routing

  (let [response-404 default-404-response
        sm (gen-sitemap
             [home
              users
              users.$userid {:any (fn [req]
                                    {:status 200
                                     :headers {"Content/Type" "text/txt"}
                                     :body "Hi!"})
                             :post (fn [req]
                                     {:status 403
                                      :headers {"Content/Type"
                                                "text/txt"}
                                      :body "Forbidden"
                                      })}
              users.$userid.edit
              users.$userid.comments {:breadcrumb "comments"}
              users.$userid.comments.$cid]             )
        ring-app (gen-ring-app sm)
        handler #(ring-app {:path-info % :http-method :get})
        gen-resp #(merge {:status 200
                          :headers {"Content/Type" "text/html"}}
                         %)]
    (is (= (handler "/nonexistent") response-404))
    (is (= (handler "/users/1234//") response-404))
    (is (= 403 (:status (ring-app {:path-info "/users/1234/"
                                   :http-method :post}))))
    (is (= 501 (:status (handler "/users/1234/edit/"))))
    (is (= 501 (:status (handler "/users/1234/comments/"))))
    (is (= (get-breadcrumb (-get-sitenode ring-app
                                          :users.$userid.comments)
                           {:userid 1234})
           "comments"))))
