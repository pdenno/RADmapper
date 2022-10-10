(ns rad-mapper.ast-test
  (:require
   [clojure.test           :refer  [deftest is testing]]
   #?(:clj  [datahike.api           :as d])
   #?(:clj  [datahike.pull-api      :as dp])
   #?(:cljs [datascript.core           :as d])
   #?(:cljs [datascript.pull-api      :as dp])
   #?(:clj [owl-db-tools.resolvers :refer [pull-resource]])
   [rad-mapper.builtins    :as bi]
   [rad-mapper.query       :as qu]
   [rad-mapper.rewrite     :as rew]
   [dev.dutil :refer-macros [run-test]]))

(deftest anytest
  (is (= true true)))

#_"
(
    $CropClassTable := {'corn'        : 'C',
                        'soybeans'    : 'S',
                        'alfalfa'     : 'ALF',
                        'cotton'      : 'TN',
                        'wheat'       : 'SW',
                        'springwheat' : 'SW',
                        'winterwheat' : 'WW'};

    $CropClassLookup := function($key) /* Lookup the crop code. */
                          { $lookup($CropClassTable, $lowercase($key)) or ''};

    $CropClassLookup('CORN')
)
"
