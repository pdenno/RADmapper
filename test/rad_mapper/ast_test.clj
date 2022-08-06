(ns rad-mapper.ast-test
  (:require
   [clojure.test           :refer  [deftest is testing]]
   [datahike.api           :as d]
   [datahike.pull-api      :as dp]
   [owl-db-tools.resolvers :refer [pull-resource]]
   [rad-mapper.builtins    :as bi]
   [rad-mapper.query       :as qu]
   [rad-mapper.rewrite     :as rew]
   [rad-mapper.devl.devl-util :refer [#_nicer nicer-sym]]))

"
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
