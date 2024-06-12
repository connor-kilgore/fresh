(defproject fresh "1.1.3"
            :description "A library to keep your clojure runtime 'Fresh'."
            :dependencies [[org.clojure/clojure "1.11.3"]]
            :profiles {:dev {:dependencies [[speclj "3.4.8"]]
                             :resource-paths ["sample_src" "space directory"]}}
            :plugins [[speclj "3.4.8"]]
            :source-paths ["src"]
            :test-paths ["spec"])
