(ns birdseye.test.ring-test
  (:require [clojure.test :refer :all]

            [birdseye.sitemap :refer :all]
            [birdseye.macros :refer [sitemap]]
            [birdseye.url-mapping :refer :all]
            [birdseye.ring :refer :all]))

(deftest test-ring-routing
  (let [response-404 default-404-response
        sm (sitemap
            home
            users
            users.$userid {:any (fn [req]
                                  {:status 200
                                   :headers {"content-type" "text/txt"}
                                   :body "Hi!"})
                           :post (fn [req]
                                   {:status 403
                                    :headers {"content-type"
                                              "text/txt"}
                                    :body "Forbidden"
                                    })}
            users.$userid.edit
            users.$userid.comments {:breadcrumb "comments"}
            users.$userid.comments.$cid             )
        ring-app (gen-ring-app sm)
        sm (:sitemap ring-app) ; now has extra error handling keys
        handler #(ring-app {:path-info % :request-method :get})
        gen-resp #(merge {:status 200
                          :headers {"content-type" "text/html"}}
                         %)]
    (is (:birdseye/root-context sm))
    (is (lookup-context-in-hierarchy sm :users :birdseye/http-501))
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

(deftest test-set-default-handler
  (let [defh (fn [req]
               {:status 200
                :body "Yo"
                :headers {"content-type" "text/txt"}})
        sm (sitemap home a a.b)
        sm (set-default-handler sm defh)
        ring-app (gen-ring-app sm)
        handler #(ring-app {:path-info % :request-method :get})]
    (is (= "Yo" (:body (handler "/a/b/"))))))
