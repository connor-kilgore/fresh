(ns fresh.core-spec
  (:use
    [speclj.core]
    [fresh.core]
    [clojure.java.io :only (file copy make-input-stream delete-file make-parents)]))

(def sample-dir (.getCanonicalFile (file "sample_src")))
(def space-sample-dir (.getCanonicalFile (file "space directory")))

(defn clean-sample-files [directory]
  (let [all (remove #(= directory %) (file-seq directory))
        files (filter #(.isFile %) all)
        dirs (filter #(.isDirectory %) all)
        dirs (reverse (sort #(compare (.length (.getPath %1)) (.length (.getPath %2))) dirs))]
    (doseq [file files] (delete-file file))
    (doseq [dir dirs] (delete-file dir))))

(defn sample-file [dir name]
  (file dir name))

(defn write-file [dir name content]
  (let [file (sample-file dir name)]
    (make-parents file)
    (copy (make-input-stream (.getBytes content) {}) file)
    file))

(defn establish-sample-files []
  (clean-sample-files sample-dir)
  (clean-sample-files space-sample-dir)
  (write-file sample-dir "sample/core.clj" "(ns sample.core) (def sample-core :sample-core)")
  (write-file sample-dir "sample/a/one.clj" "(ns sample.a.one (:use [sample.core])) (def sample-a-one :sample-a-one)")
  (write-file sample-dir "sample/b/one.clj" "; comment\n(ns sample.b.one (:use [sample.core])) (def sample-b-one :sample-b-one)")
  (write-file space-sample-dir "space/core.clj" "(ns space.core) (def space-core :space-core)"))

(defn tweak-mod-time [file tweak]
  (let [mod-time (+ (.lastModified file) (* 1000 tweak))]
    (.setLastModified file mod-time)))

(describe "Fresh"

  (it "converts ns names into filenames"
    (should= ["foo.clj" "foo.cljc"] (ns-to-filenames "foo"))
    (should= ["foo.foo" "foo.bar"] (ns-to-filenames "foo" [".foo" ".bar"]))
    (should= ["bar.clj" "bar.cljc"] (ns-to-filenames "bar"))
    (should= ["foo/bar.clj" "foo/bar.cljc"] (ns-to-filenames "foo.bar"))
    (should= ["foo_bar.clj" "foo_bar.cljc"] (ns-to-filenames "foo-bar"))
    (should= ["foo_bar/fizz_bang.clj" "foo_bar/fizz_bang.cljc"] (ns-to-filenames "foo-bar.fizz-bang")))

  (it "recognizes ns forms"
    (should= true (ns-form? '(ns blah)))
    (should= true (ns-form? '(ns foo.bar
                               (:use [sample.core]))))
    (should= false (ns-form? '(not-ns blah)))
    (should= false (ns-form? [])))

  (it "pulls ns name from ns form"
    (should= 'foo (ns-name-from '(ns foo)))
    (should= 'foo.bar (ns-name-from '(ns foo.bar))))

  (it "pulls dependencies out of ns form"
    (should= '#{blah} (depending-ns-names-from '(ns foo
                                                  (:use [blah]))))
    (should= '#{bar} (depending-ns-names-from '(ns foo
                                                 (:use [bar]))))
    (should= '#{fizz} (depending-ns-names-from '(ns foo
                                                  (:use fizz))))
    (should= '#{fizz} (depending-ns-names-from '(ns foo
                                                  (:require fizz))))
    (should= '#{one two three} (depending-ns-names-from '(ns foo
                                                           (:use [one] [two] [three]))))
    (should= '#{one two three} (depending-ns-names-from '(ns foo
                                                           (:require [one]
                                                                     [two]
                                                                     [three]))))
    (should= '#{root.one root.two} (depending-ns-names-from '(ns foo
                                                               (:use [root [one] [two]]))))
    (should= '#{root.one root.two} (depending-ns-names-from '(ns foo
                                                               (:require [root [one] [two]]))))
    (should= '#{one two} (depending-ns-names-from '(ns foo
                                                     (:use [one :only (foo)] [two :exclude (bar)]))))
    (should= '#{one two} (depending-ns-names-from '(ns foo
                                                     (:require [one :as o]
                                                               [two :as t]))))
    (should= '#{one.two one.three} (depending-ns-names-from '(ns foo
                                                               (:use [one [two :only (foo)] [three :exclude (bar)]]))))
    (should= '#{one.two one.three} (depending-ns-names-from '(ns foo
                                                               (:require [one [two :as t] [three :as tr]]))))
    (should= '#{root.one.child.grandchild root.two} (depending-ns-names-from '(ns foo
                                                                                (:use [root [one [child [grandchild]]] [two]]))))
    (should= '#{fizz} (depending-ns-names-from '(ns foo
                                                  (:require [fizz]
                                                            :reload))))
    (should= '#{fizz} (depending-ns-names-from '(ns foo
                                                  (:use [fizz] :verbose)))))

  (context "using files"

    (before (clean-sample-files sample-dir)
            (clean-sample-files space-sample-dir))

    (it "reads no ns form from src files that don't contain them"
      (should= nil (read-ns-form (write-file sample-dir "test/one.clj" "()")))
      (should= nil (read-ns-form (write-file sample-dir "test/one.clj" "; hello")))
      (should= nil (read-ns-form (write-file sample-dir "test/one.clj" "; (ns blah)"))))

    (it "pulls read ns form from files"
      (should= '(ns blah) (read-ns-form (write-file sample-dir "test/one.clj" "(ns blah)")))
      (should= '(ns foo) (read-ns-form (write-file sample-dir "test/one.clj" "; blah\n(ns foo)")))
      (should= '(ns blah
                  (:use [foo])
                  (:require [bar])) (read-ns-form (write-file sample-dir "test/one.clj" "(ns blah (:use [foo])(:require [bar]))")))))

  (context "using sample files"

    (before (establish-sample-files))

    (it "should not include hidden files as clj files"
      (write-file sample-dir "sample/.hidden.clj" "I'm a hidden file!")
      (let [files (clj-files-in sample-dir)]
        (should-not (contains? (set (map #(.getName %) files)) ".hidden.clj"))))

    (it "finds cljc files by default"
      (write-file sample-dir "sample/portable.cljc" "I'm portable")
      (let [files (clj-files-in sample-dir)]
        (should-contain "portable.cljc" (set (map #(.getName %) files)))))

    (it "finds specified files by default"
      (write-file sample-dir "sample/portable.cljx" "I'm antiquated")
      (let [files (find-files-in #".*\.cljx" sample-dir)]
        (should-contain "portable.cljx" (set (map #(.getName %) files)))))

    (it "finds src files from ns name"
      (let [cljc-file (write-file sample-dir "sample/portable.cljc" "I'm portable")]
        (should= cljc-file (ns-to-file "sample.portable"))))

    (it "finds cljc src files from ns name"
      (should= (sample-file sample-dir "sample/core.clj") (ns-to-file "sample.core"))
      (should= (sample-file sample-dir "sample/a/one.clj") (ns-to-file "sample.a.one")))

    (it "finds depending files form ns form"
      (should= [] (depending-files-from '(ns foo)))
      (should= [] (depending-files-from '(ns foo
                                           (:use [clojure.set]))))
      (should= [(sample-file sample-dir "sample/core.clj")] (depending-files-from '(ns foo
                                                                          (:use [sample.core]))))
      (should= #{(sample-file sample-dir "sample/core.clj") (sample-file sample-dir "sample/a/one.clj")}
               (set (depending-files-from '(ns foo
                                             (:use [sample.core])
                                             (:require [sample.a.one]))))))

    (it "first freshening adds files to listing"
      (let [listing (atom {})]
        (make-fresh listing (clj-files-in sample-dir) (fn [_] true))
        (should= 3 (count @listing))
        (should= true (contains? @listing (sample-file sample-dir "sample/core.clj")))
        (should= true (contains? @listing (sample-file sample-dir "sample/a/one.clj")))
        (should= true (contains? @listing (sample-file sample-dir "sample/b/one.clj"))))

      (let [listing (atom {})]
        (make-fresh listing (clj-files-in space-sample-dir) (fn [_] true))
        (should= 1 (count @listing))
        (should (contains? @listing (sample-file space-sample-dir "space/core.clj")))))

    (it "new files are detected and added to listing"
      (let [listing (atom {})]
        (make-fresh listing (clj-files-in sample-dir) (fn [_] true))
        (write-file sample-dir "sample/a/two.clj" "(ns sample.a.two)")
        (make-fresh listing (clj-files-in sample-dir) (fn [_] true))
        (should= 4 (count @listing))
        (should= true (contains? @listing (sample-file sample-dir "sample/a/two.clj")))))

    (it "deleted files are removed from listing"
      (let [listing (atom {})]
        (make-fresh listing (clj-files-in sample-dir) (fn [_] true))
        (delete-file (sample-file sample-dir "sample/a/one.clj"))
        (make-fresh listing (clj-files-in sample-dir) (fn [_] true))
        (should= 2 (count @listing))
        (should-not (contains? @listing (sample-file sample-dir "sample/a/one.clj"))))

      (write-file space-sample-dir "space/new.clj" "")
      (let [listing (atom {})]
        (make-fresh listing (clj-files-in space-sample-dir) (fn [_] true))
        (should= 2 (count @listing))
        (delete-file (sample-file space-sample-dir "space/new.clj"))
        (make-fresh listing (clj-files-in space-sample-dir) (fn [_] true))
        (should= 1 (count @listing))
        (should-not (contains? @listing (sample-file space-sample-dir "space/new.clj")))))

    (context "with freshener"

      (with audit-value (atom true))
      (with refresh-sample (freshener #(clj-files-in (file sample-dir "sample")) (fn [_] @@audit-value)))
      (with refresh-space-sample (freshener #(clj-files-in (file space-sample-dir "space")) (fn [_] @@audit-value)))

      (it "reports new files in result map"
        (let [result (@refresh-sample)
              new-files #{(sample-file sample-dir "sample/core.clj")
                          (sample-file sample-dir "sample/a/one.clj")
                          (sample-file sample-dir "sample/b/one.clj")}]
          (should= new-files (:new result)))

        (let [result (@refresh-space-sample)
              new-files #{(sample-file space-sample-dir "space/core.clj")}]
          (should= new-files (:new result))))

      (it "includes empty files"
        (write-file sample-dir "sample/a/new.clj" "")
        (let [result (@refresh-sample)]
          (should (contains? (:new result) (sample-file sample-dir "sample/a/new.clj"))))

        (write-file space-sample-dir "space/new.clj" "")
        (let [result (@refresh-space-sample)]
          (should (contains? (:new result) (sample-file space-sample-dir "space/new.clj")))))

      (it "reports deleted files in result map"
        (@refresh-sample)
        (delete-file (sample-file sample-dir "sample/a/one.clj"))
        (let [result (@refresh-sample)]
          (should= #{(sample-file sample-dir "sample/a/one.clj")} (:deleted result)))

        (write-file space-sample-dir "space/new.clj" "")
        (@refresh-space-sample)
        (delete-file (sample-file space-sample-dir "space/new.clj"))
        (let [result (@refresh-space-sample)]
          (should= #{(sample-file space-sample-dir "space/new.clj")} (:deleted result))))

      (it "reports modified files in result map"
        (@refresh-sample)
        (tweak-mod-time (sample-file sample-dir "sample/a/one.clj") 1)
        (let [result (@refresh-sample)]
          (should= #{(sample-file sample-dir "sample/a/one.clj")} (:modified result)))

        (write-file space-sample-dir "space/new.clj" "")
        (@refresh-space-sample)
        (tweak-mod-time (sample-file space-sample-dir "space/new.clj") 1)
        (let [result (@refresh-space-sample)]
          (should= #{(sample-file space-sample-dir "space/new.clj")} (:modified result))))

      (it "reports reloaded files in result map"
        (let [result (@refresh-sample)
              reloaded [(sample-file sample-dir "sample/a/one.clj")
                        (sample-file sample-dir "sample/b/one.clj")
                        (sample-file sample-dir "sample/core.clj")]]
          (should= reloaded (:reloaded result))))

      (it "tracks dependencies not provided by provider"
        (write-file sample-dir "sample/a/one.clj" "(ns sample.a.one (:use [other.one]))")
        (write-file sample-dir "other/one.clj" "(ns other.one)")
        (let [result (@refresh-sample)]
          (should= true (contains? (:new result) (sample-file sample-dir "other/one.clj")))))

      (it "deletes unused dependencies not provided by provider"
        (write-file sample-dir "sample/a/one.clj" "(ns sample.a.one (:use [other.one]))")
        (write-file sample-dir "other/one.clj" "(ns other.one)")
        (@refresh-sample)
        (delete-file (sample-file sample-dir "sample/a/one.clj"))
        (let [result (@refresh-sample)]
          (should= #{(sample-file sample-dir "sample/a/one.clj")
                     (sample-file sample-dir "other/one.clj")}
                   (:deleted result))))

      (it "reloads new src files"
        (@refresh-sample)
        (should= :sample-core (eval '(do (require 'sample.core) sample.core/sample-core)))
        (should= :sample-a-one (eval '(do (require 'sample.a.one) sample.a.one/sample-a-one)))
        (should= :sample-b-one (eval '(do (require 'sample.b.one) sample.b.one/sample-b-one)))
        (should= :space-core (eval '(do (require 'space.core) space.core/space-core))))

      (it "reloads modified src files"
        (tweak-mod-time (sample-file sample-dir "sample/a/one.clj") -1)
        (@refresh-sample)
        (write-file sample-dir "sample/a/one.clj" "(ns sample.a.one (:use [sample.core])) (def sample-a-one :another-value)")
        (@refresh-sample)
        (should= :another-value (eval '(do (require 'sample.a.one) sample.a.one/sample-a-one)))

        (tweak-mod-time (sample-file space-sample-dir "space/core.clj") -1)
        (@refresh-space-sample)
        (write-file space-sample-dir "space/core.clj" "(ns space.core) (def space-core :another-value)")
        (@refresh-space-sample)
        (should= :another-value (eval '(do (require 'space.core) space.core/space-core)))
        )

      (it "reloads dependencies of modified files"
        (write-file sample-dir "sample/core.clj" "(ns sample.core (:use [other.one]))")
        (write-file sample-dir "other/one.clj" "(ns other.one) (def other-one :other)")
        (@refresh-sample)
        (write-file sample-dir "sample/a/one.clj" "(ns sample.a.one) (def sample-a-one :new-value)")
        (tweak-mod-time (sample-file sample-dir "other/one.clj") 1)
        (let [result (@refresh-sample)
              reloaded [(sample-file sample-dir "other/one.clj") (sample-file sample-dir "sample/a/one.clj")
                        (sample-file sample-dir "sample/b/one.clj") (sample-file sample-dir "sample/core.clj")]]
          (should= reloaded (:reloaded result))
          (should= :new-value (eval '(do (require 'sample.a.one) sample.a.one/sample-a-one)))))

      (it "unloads deleted files"
        (@refresh-sample)
        (delete-file (sample-file sample-dir "sample/a/one.clj"))
        (@refresh-sample)
        (should= false (contains? @@#'clojure.core/*loaded-libs* 'sample.a.one)))

      (it "wont modify any state if the auditor return false"
        (reset! @audit-value false)
        (let [new-tag (.substring (str (rand)) 2)
              new-ns (format "sample.new%s" new-tag)]
          (write-file sample-dir (format "sample/new%s.clj" new-tag) (format "(ns %s)" new-ns))
          (@refresh-sample)
          (should= false (contains? @@#'clojure.core/*loaded-libs* (symbol new-ns)))))
      )
    )

  )

(run-specs)
