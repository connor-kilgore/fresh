(ns fresh.core
  (:use
    [clojure.java.io :only (file)])
  (:require
    [clojure.set :as set]
    [clojure.string :as str])
  (:import
    [java.io PushbackReader FileReader File]
    [java.net URL]))

(defn find-files-in
  "Returns a seq of all files (matching the regex) contained in the given directories."
  [pattern & dirs]
  (let [dirs (map #(.getCanonicalFile %) dirs)
        files (reduce #(into %1 (file-seq (file %2))) [] dirs)
        files (remove #(.isHidden %) files)
        clj-files (filter #(re-matches pattern (.getName %)) files)]
    clj-files))

(def clj-file-regex #".*\.clj(c)?")
(defn clj-files-in
  "Returns a seq of all clojure source files contained in the given directories."
  [& dirs] (apply find-files-in clj-file-regex dirs))

;; Resolving ns names ---------------------------------------------------------------------------------------------------

(def clj-extensions [".clj" ".cljc"])

(defn ns-to-filenames
  "Converts the namespace name into a relative path for the corresponding clojure src file."
  ([ns] (ns-to-filenames ns clj-extensions))
  ([ns extensions] (map #(str (apply str (replace {\. \/ \- \_} (name ns))) %) extensions)))

(defn ns-to-file
  "Returns a java.io.File corresponding to the clojure src file for the
  given namespace.  nil is returned if the file is not found in the classpath
  or if the file is not a raw text file."
  ([ns] (ns-to-file ns clj-extensions))
  ([ns extensions]
   (let [relative-filenames (ns-to-filenames ns extensions)
         loader (clojure.lang.RT/baseLoader)
         url (first (filter identity (map #(.getResource loader %) relative-filenames)))
         url (URL. (str/replace (.toString url) "%20" " "))]
     (if (and url (= "file" (.getProtocol url)))
       (file (.getFile url))
       nil))))

(defn ns-form?
  "Returns true if the given form is a namespace form."
  [form]
  (and (list? form) (= 'ns (first form))))

(defn read-ns-form
  "Returns the namespace form on the specified clojure src file, nil if none is found."
  [file]
  (try
    (let [reader (PushbackReader. (FileReader. file))]
      (try
        (loop [form (read {:read-cond :allow} reader)]
          (if (ns-form? form)
            form
            (recur (read {:read-cond :allow} reader))))
        (finally (.close reader))))
    (catch Exception e nil)))
;
;; Parsing the ns form --------------------------------------------------------------------------------------------------
;
(defn- compose-ns [prefix lib]
  (if prefix
    (symbol (str prefix \. lib))
    lib))

(defn- ns-for-part [prefix arg]
  (cond
    (symbol? arg) (compose-ns prefix arg)
    (and (vector? arg) (or (nil? (second arg)) (keyword? (second arg)))) (compose-ns prefix (first arg))
    :else (map #(ns-for-part (compose-ns prefix (first arg)) %) (rest arg))))

(defn- depending-names-of-part [args]
  (map #(ns-for-part nil %) (filter (complement keyword?) (rest args))))

(defn depending-ns-names-from
  "Returns a seq of symbols that are the names of the namespaces that the provided
  namespace form depends on."
  [ns-form]
  (let [dependency-parts (filter #(and (list? %) (#{:use :require} (first %))) ns-form)
        ns-list (map #(depending-names-of-part %) dependency-parts)]
    (set (flatten ns-list))))

(defn depending-files-from
  "Returns a seq of java.io.File objects that the namespace form depends on."
  [ns-form]
  (if ns-form
    (let [dependency-names (depending-ns-names-from ns-form)
          dependency-filenames (map #(ns-to-file %) dependency-names)]
      (vec (filter identity dependency-filenames)))
    []))

(defn ns-name-from
  "Returns the name of the namespace form"
  [ns-form]
  (if ns-form
    (second ns-form)
    nil))
;
;; File tracking --------------------------------------------------------------------------------------------------------

(deftype FileTracker [ns mod-time dependencies]
  Object
  (toString [this] (str "ns: " ns " mod-time: " mod-time " dependencies: " dependencies)))

(defn- new-file-tracker [ns mod-time dependencies]
  (FileTracker. ns mod-time dependencies))

(defn- modified? [file tracker]
  (> (.lastModified file) (.mod-time tracker)))

(declare update-tracking-for-files)
(defn- update-tracking-for-file [listing file batch]
  (let [tracker (get listing file)
        no-update-required (not (or (nil? tracker) (modified? file tracker)))]
    (if no-update-required
      [listing batch]
      (let [ns-form (read-ns-form file)
            dependencies (depending-files-from ns-form)
            [listing batch] (update-tracking-for-files listing dependencies batch)
            ns (ns-name-from ns-form)
            updated-tracker (new-file-tracker ns (.lastModified file) dependencies)]
        [(assoc listing file updated-tracker) batch]))))

(defn- update-tracking-for-files
  ([listing files] (first (update-tracking-for-files listing files #{})))
  ([listing files batch]
    (loop [[listing batch] [listing batch] files files]
      (if (not (seq files))
        [listing batch]
        (let [file (first files)]
          (if (contains? batch file)
            (recur [listing batch] (rest files))
            (recur (update-tracking-for-file listing file (conj batch file)) (rest files))))))))

(defn- depends-on? [dependency listing dependent]
  (some (partial = dependency) (.dependencies (get listing dependent))))

(defn- has-dependent? [listing file]
  (some #(depends-on? file listing %) (keys listing)))

(defn- with-dependency [new-dependents dependents file tracker]
  (if (some dependents (.dependencies tracker))
    (conj new-dependents file)
    new-dependents))

(defn- dependents-of
  ([listing files] (dependents-of listing (set files) #{}))
  ([listing files dependents]
    (loop [files files dependents dependents]
      (let [new-dependents (reduce (fn [new-dependents [file tracker]] (with-dependency new-dependents files file tracker)) #{} listing)]
        (if (seq new-dependents)
          (recur new-dependents (into dependents new-dependents))
          dependents)))))

(defn- clean-deleted-files
  ([listing] (clean-deleted-files listing (filter #(not (.exists %)) (keys listing))))
  ([listing files-to-delete]
    (if (not (seq files-to-delete))
      listing
      (let [dependencies (reduce #(into %1 (.dependencies (get listing %2))) [] files-to-delete)
            listing (apply dissoc listing files-to-delete)
            unused-dependencies (filter #(not (has-dependent? listing %)) dependencies)]
        (clean-deleted-files listing unused-dependencies)))))

(defn- unload-nses [nses]
  (doseq [ns nses] (remove-ns ns))
  (dosync (alter @#'clojure.core/*loaded-libs* set/difference (set nses))))

(defn- load-nses [nses]
  (apply require nses))

(defn- doto-nses [listing files & actions]
  (let [trackers (vec (filter identity (map listing files)))
        nses (vec (filter identity (map #(.ns %) trackers)))]
    (when (seq nses)
      (doseq [action actions]
        (action nses)))))

(defn make-fresh
  "Does the work of freshener functions."
  [listing-atom files auditor]
  (let [listing (clean-deleted-files @listing-atom)
        tracked-files (set (keys listing))
        deleted (set/difference (set (keys @listing-atom)) tracked-files)
        new-tracked-files (set/difference (set files) tracked-files)
        modified-tracked-files (set (filter #(modified? % (get listing %)) tracked-files))
        updates (concat new-tracked-files modified-tracked-files)
        listing (update-tracking-for-files listing updates)
        new (set/difference (set (keys listing)) tracked-files)
        files-to-reload (sort (into (dependents-of listing updates) updates))
        result {:new new :deleted deleted :modified modified-tracked-files :reloaded files-to-reload}]
    (when (auditor result)
      (doto-nses @listing-atom deleted unload-nses)
      (reset! listing-atom listing)
      (doto-nses listing files-to-reload unload-nses load-nses))
    result))

(defn freshener
  "Returns a freshener function that, when invoked, will ensure
the freshness of all files provided by the provider function.
The provider must be a no-arg function that returns a seq of java.io.File
objects.  If any of the files have been modified, they (and all
their dependent files), will be reloaded. New files will be loaded and
tracked.  Deleted files will be unloaded along with any dependant files
that are no longer referenced. The freshener function returns a report map
of seqs containing File objects: {:new :modified :deleted :reloaded}.
The optional auditor function is called, passing in the report map,
before the state of the runtime has been modified.  Only when the auditor
returns a truthy value will the runtime be modified."
  ([provider] (freshener provider (fn [_] true)))
  ([provider auditor]
    (let [listing-atom (atom {})]
      (fn [] (make-fresh listing-atom (provider) auditor)))))
