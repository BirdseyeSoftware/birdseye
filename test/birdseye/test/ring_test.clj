(ns birdseye.test.ring-test
  (:require [birdseye.sitemap :refer :all]
            [birdseye.macros :refer [defsitemap gen-sitemap]]
            [birdseye.ring :refer :all]
            [clojure.test :refer :all]))

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
     :userid "1234"))
  ;; (is false)
  )

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
        handler #(ring-app {:path-info % :request-method :get})
        gen-resp #(merge {:status 200
                          :headers {"Content/Type" "text/html"}}
                         %)]
    (is (= (handler "/nonexistent") response-404))
    (is (= (handler "/users/1234//") response-404))
    (is (= 403 (:status (ring-app {:path-info "/users/1234/"
                                   :request-method :post}))))
    (is (= 501 (:status (handler "/users/1234/edit/"))))
    (is (= 501 (:status (handler "/users/1234/comments/"))))
    (is (= (get-breadcrumb (-get-sitenode ring-app
                                          :users.$userid.comments)
                           {:userid 1234})
           "comments"))))
