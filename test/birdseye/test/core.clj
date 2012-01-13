(ns birdseye.test.core
  (:refer-clojure :exclude [sym])
  (:use [birdseye.core])
  (:use [clojure.test]))

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

(deftest test-defsitemap
  (let [sm (defsitemap
             home
             users
             users.$userid
             users.$userid.edit)]
    (assert-basic-sitemap-props sm)
    (is (= 4 (-> sm keys count))))

  (doseq [sm [(defsitemap
                home
                foo
                foo.bar
                foo.bar.asdf
                users
                users.$userid {:2 (+ 1 1)}
                users.$userid.edit {})
              (defsitemap
                users ;; same as above but with relative sub-nodes.
                [.$userid {:2 (+ 1 1)}
                 .$userid.edit {}])]]
    (do
      (assert-basic-sitemap-props sm)
      (is (every? empty? (vals (dissoc sm :users.$userid))))
      (is (= 2 (get-in sm [:users.$userid :2]))))))

(deftest test-relative-sub-maps
  ;; test map of relative sub-nodes
  ;; and insertion of that map into a parent map
  (let [sm (defsitemap
             .foo
             .foo.bar
             .users
             .users.$userid {:2 (+ 1 1)}
             .users.$userid.edit)
        sm2 (defsitemap toplevel =sm)]
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
        sm (defsitemap =node-key {} =node-key2 =context-map)]
    (assert-basic-sitemap-props sm)
    (is (= sm {:just-testing {}
               :just-testing.foo context-map}))))

(deftest test-invalid-map-exceptions
  (is (thrown-with-msg? Exception #"must not end in a dot"
        (defsitemap foo.bar.)))

  (is (thrown-with-msg? Exception #"Invalid.*position 0"
        (defsitemap {} {})))

  (is (thrown-with-msg? Exception #"Invalid .* position 3"
        (defsitemap foo bar {} {}) ))

  (is (thrown-with-msg? Exception #"Invalid .* position 0"
        (defsitemap nil bar)))
  (is (thrown-with-msg? Exception #"Invalid .* position 1"
        (defsitemap bar nil)))
  (is (thrown-with-msg? Exception #"Invalid .* position 1"
        (defsitemap bar false)))

  (is (thrown-with-msg? Exception #"Parent node .* does not exist"
        (defsitemap foo.bar))))

(deftest test-node-to-url
  (let [sm (defsitemap
             home
             users
             users.$userid
             users.$userid.edit
             users.$userid.comments
             users.$userid.comments.$cid
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
  (let [sm (defsitemap
             home
             users
             users.active
             users.active.by_join_date
             users.$userid
             users.$userid.edit
             users.$userid.comments
             users.$userid.comments.$cid
             )
        ring-app (gen-ring-app sm)]
    (assert-url-to-node ring-app "/asdf" nil)
    (assert-url-to-node ring-app "/" :home)
    (assert-url-to-node ring-app "/users/" :users)
    (assert-url-to-node ring-app "/users/active/" :users.active)
    (assert-url-to-node ring-app "/users/active/by_join_date/"
                        :users.active.by_join_date)
    (assert-url-to-node
     ring-app "/users/1234/" :users.$userid :userid "1234")
    (assert-url-to-node
     ring-app "/users/1234/edit/" :users.$userid.edit
     :userid "1234")
    )
  )
(deftest test-ring-routing

  (let [response-404 {:status 404
                      :headers {"Content/Type" "text/html"}
                      :body "Not Found"}
        sm (defsitemap
             home
             users
             users.$userid {:h (fn [req]
                                 {:status 200
                                  :headers {"Content/Type" "text/txt"}
                                  :body "Hi!"})
                            :v (fn [req resp] (assoc resp :body "HIII"))}
             users.$userid.edit
             users.$userid.comments {:breadcrumb "comments"}
             users.$userid.comments.$cid
             )
        ring-app (gen-ring-app sm)
        h #(handle-request ring-app {:path-info %})
        gen-resp #(merge {:status 200
                          :headers {"Content/Type" "text/html"}}
                         %)]
    (is (= (h "/nonexistent") response-404))
    (is (= (h "/users/1234//") response-404))
    (is (= (h "/users/1234/")
           (gen-resp {:headers {"Content/Type" "text/txt"}
                      :body "HIII"})))
    (is (= (h "/users/1234/edit/")
           (gen-resp {:body "default handler for users.$userid.edit"})))
    (is (= (h "/users/1234/comments/")
           (gen-resp {:body "default handler for users.$userid.comments"})))
    (is (= (get-breadcrumb (get-node-ctx ring-app
                                         :users.$userid.comments)
                           {:userid 1234})
           "comments"))))
