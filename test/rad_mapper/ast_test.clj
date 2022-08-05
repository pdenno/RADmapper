(
    $CropClassTable := {"corn"        : "C",
                        "soybeans"    : "S",
                        "alfalfa"     : "ALF",
                        "cotton"      : "TN",
                        "wheat"       : "SW",
                        "springwheat" : "SW",
                        "winterwheat" : "WW"};

    $CropClassLookup := function($key) /* Lookup the crop code. */
                          { $lookup($CropClassTable, $lowercase($key)) or ''};

    $CropClassLookup('CORN')
)
