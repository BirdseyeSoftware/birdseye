(defproject com.birdseye-sw/birdseye "1.0.3"
  :description "A sitemap/url-routing system for Clojure/ring web apps."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.match "0.2.0-rc4"]
                 [ring "1.2.0"]
                 [clout "1.1.0"]]
  :exclusions [clj-stacktrace]
  :dev-dependencies [[ring-mock "0.1.5"]])
