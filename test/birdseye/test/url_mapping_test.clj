(ns birdseye.test.url-mapping-test
  (:require [clojure.test :refer :all]
            [birdseye.macros :refer [gen-sitemap]]
            [birdseye.url-mapping :refer :all]))

(deftest test-node-to-url
  (let [sm (gen-sitemap
             [home
             users
             users.$userid
             users.$userid.edit
             users.$userid.comments
             users.$userid.comments.$cid]
             )
        url-mapper (gen-default-url-mapper sm)
        test-url (fn [url k m]
                   (= url (node-to-url url-mapper k m)))]
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

(defmacro assert-url-to-node [url-mapper url node-key & params]
  `(is (= [~node-key ~(apply hash-map params)]
          (url-to-node ~url-mapper ~url))))

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
        url-mapper (gen-default-url-mapper sm)]
    (assert-url-to-node url-mapper "/asdf" :http-404)
    (assert-url-to-node url-mapper "/" :home)
    (assert-url-to-node url-mapper "/users/" :users)
    (assert-url-to-node url-mapper "/users/active/" :users.active)
    (assert-url-to-node url-mapper "/users/active/by_join_date/"
                        :users.active.by_join_date)
    (assert-url-to-node
     url-mapper "/users/1234/" :users.$userid :userid "1234")
    (assert-url-to-node
     url-mapper "/users/1234/edit/" :users.$userid.edit
     :userid "1234")))
