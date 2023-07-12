(ns exerciser-app.components.examples)

(declare rm-examples elena-schemas)

(defn get-example [name]
  (some #(when (= name (:name %)) %) rm-examples))


;;; New elena schema are: urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2, etc.

;;; ($get ["schema/name" "urn:oagis-10.8.4:Nouns:Invoice"],  ["schema-object"])
(def rm-examples
  [{:name "Instance files"
    :code
"(
  $data := $get(['library_fn', 'bie-1-message'], ['fn_src']).fn_src ~> $eval();
  $mappingFn := $get(['library_fn', 'invoice-match-1->2-fn'], ['fn_src']).fn_src ~> $eval();
  $mappingFn($data)
)"}

   {:name "(1) The files"
    :code "$get(['list_id', 'cct_bie'], ['list_content']).list_content[$contains('elena')]"}

   {:name "(2): Differences"
    :code
   "(
  $schema1   := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
  $schema2   := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content']);
  $pcQuery   := $get(['library_fn', 'schemaParentChild'],['fn_exe']).fn_exe;
  $rootQuery := $get(['library_fn', 'schemaRoots'],['fn_exe']).fn_exe;
  $shape     := $get(['library_fn', 'schemaShape'],['fn_exe']).fn_exe;

  $schema1PC    := $pcQuery($schema1);     // Get parent-child relationships of each schema.
  $schema2PC    := $pcQuery($schema2);
  $schema1Roots := $rootQuery($schema1);   // Get root elements of each schema.
  $schema2Roots := $rootQuery($schema2);

  {'shape 1' : $shape($schema1Roots.?name[0], $schema1PC), // Show shapes
   'shape 2' : $shape($schema2Roots.?name[0], $schema2PC)}
)"}

   {:name "(3): LLM match 1->2"
    :code
    "// Call $llmMatch to generate the mapping function. Note use of {'as-fn?' true} in the call.

(
  $schema1   := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
  $schema2   := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content']);
  $pcQuery   := $get(['library_fn', 'schemaParentChild'],['fn_exe']).fn_exe;
  $rootQuery := $get(['library_fn', 'schemaRoots'],['fn_exe']).fn_exe;
  $shape     := $get(['library_fn', 'schemaShape'],['fn_exe']).fn_exe;

  $schema1PC    := $pcQuery($schema1);     // Get parent-child relationships of each schema.
  $schema2PC    := $pcQuery($schema2);
  $schema1Roots := $rootQuery($schema1);   // Get root elements of each schema.
  $schema2Roots := $rootQuery($schema2);

  $llmMatch($shape($schema1Roots.?name[0], $schema1PC), // [0] here is cheating a bit; there could be multiple roots.
            $shape($schema2Roots.?name[0], $schema2PC), // Call $llmMatch to do shape matching
            {'as-fn?' : true})
)"}


   {:name "(4) Get the validated invoice-match-1->2"
    :code "$get(['library_fn', 'invoice-match-1->2'], ['fn_src']).fn_src ~> $eval()"}

   {:name "Uses of $get"
    :code
    "{'1: Lists of lists'    : $get(['list_id', 'lists'],               ['list_content']),                        // Any of the values of 'list of lists can be used.
 '2: Library functions' : $get(['list_id', 'library_fn'],          ['list_content']),                        // This is one such example.
 '3: A DB connection'   : $get(['db_name', 'schemaDB'], ['db_connection']),                                  // This so you can do arbitrary query calls.
 '4: CCT schema'        : $get(['list_id', 'cct_messageSchema'], ['list_content']),                         // A list of all the ccts message schema.
 '5: A specific schema' : $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'],
                                                                   ['schema_content'])}                      // One such schema."}

   {:name "Store a library function"
    :code "$put(['library_fn', 'addTwo'],
     {'fn_name' : 'addTwo',
      'fn_doc'  : 'Add two to the argument',
      'fn_src'  : 'function($x){$x + 1}'})"}

   {:name "Get a library function"
    :code "$get(['library_fn', 'addOne'],['fn_name', 'fn_doc', 'fn_src','fn_exe'])"}

   {:name "Use a library function"
    :code "( $addOne := $get(['library_fn', 'addOne'],['fn_exe']).fn_exe;
  $addOne(3) );"}

   {:name "partial solution"
    :code "{'ProcessInvoice':
    {'ApplicationArea':
      {'CreationDateTime': '<creation-date-time-data>'},
      'DataArea'       :
      {'Invoice':
        {'InvoiceLine':
          {'BuyerParty':
            {'Location': {'Address': {'BuildingNumber': $llmExtract($src.ProcessInvoice.ApplicationArea.DataArea.Invoice.InvoiceLine.BuyerParty.Location.AddressLine, 'BuildingNumber'),
                                                  'CityName'      : $llmExtract($src.ProcessInvoice.ApplicationArea.DataArea.Invoice.InvoiceLine.BuyerParty.Location.AddressLine, 'City'),
                                                  'CountryCode'   : '<replace-me>',
                                                  'PostalCode'    : $llmExtract($src.ProcessInvoice.ApplicationArea.DataArea.Invoice.InvoiceLine.BuyerParty.Location.AddressLine, 'Zipcode')
                                                  'StreetName'    : $llmExtract($src.ProcessInvoice.ApplicationArea.DataArea.Invoice.InvoiceLine.BuyerParty.Location.AddressLine, 'StreetName')}}}
             'TaxIDSet': {'ID': '<id-data>'}},
             'Item'    : {'ManufacturingParty': {'Name': '<name-data>'}}}},
        'Process':      '<process-data>'}}"}

   {:name "Using library code"
    :code "($schema1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
 $schema2 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content']);
 $pcQuery   := $get(['library_fn' , 'schemaParentChild'], ['fn_src', 'fn_exe']).fn_exe;
 $rootQuery := $get(['library_fn' , 'schemaRoots'],       ['fn_src', 'fn_exe']).fn_exe;
 $shape     := $get(['library_fn' , 'schemaShape'],       ['fn_src', 'fn_exe']).fn_exe;

 $schema1PC    := $pcQuery($schema1);     // Call the two queries with the two schema.
 $schema2PC    := $pcQuery($schema2);     // The first two return binding sets for {?parent x ?child y}
 $schema1Roots := $rootQuery($schema1);   // The last two return binding sets for {?name} (of a root).
 $schema2Roots := $rootQuery($schema2);

 {'shape1' : $shape($schema1Roots.?name[0], $schema1PC),
  'shape2' : $shape($schema2Roots.?name[0], $schema2PC)}
)"}

   {:name "Try (-1) : JSONata-like"
    :code
    "( // Ordinary JSONata-like expressions: get the names of the two schema in the LHS pane:
  $s1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content', 'schema_name']);
  $s2 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content', 'schema_name']);
  $q := query{[?e :schema_name ?name]};
  $q($s1)  )"}

   #_{:name "So simple!"
    :code "1 + 2"}

   {:name "Try (1): Small remote query"
    :code "( $db  := $get(['db_name', 'schemaDB'], ['db_connection']);
  $qfn := query{[?e :schema_name ?name] [?e :schema_sdo ?sdo]};
  $qfn($db).?sdo ~> $distinct() ~> $sort()
   )"}

   {:name "Try (2) : Simple get"
    :code "( $schema := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
           $schema )"}

   {:name "Try (3) : JSONata-like"
    :code
    "( // Ordinary JSONata-like expressions: get the names of the two schema in the LHS pane:
  $s1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content', 'schema_name']);
  $s2 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content', 'schema_name']);

[$s1, $s2].`schema_name` )"}

   {:name "Try (4) : Simplest query"
    :code
"(
  $x := {'element_name' : 'foo'};
  $qf := query{[?x :element_name ?name]};
  $qf($x)
)"}

   {:name "Try (5): Simple query, complicated schema"
    :code "(
  // Small bug in the exerciser (because it combines data from the LHS pane):
  // currently comments have to be inside the open paren.
  // Here we put $s1 and $s2 into a vector, $db, so we can work on them together.
  // We could also just call the query on either $s1 or $s2, of course,
  // or don't create the $db and just call $qf with [$s1, $s2].

  $s1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_name', 'schema_content']);
  $s2 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_name', 'schema_content']);

 $db := [$s1, $s2];
 $qf := query{[?x :schema_name ?name]};
 $qf($db)
)" }

   #_{:name "Try (*): (aside) Query defines a function."
    :code
    "(
  // Remember: query and express are function defining.
  // If you run the following you just get <<function>>

  query{[?x :model_elementDef ?ed]}
)"}

   {:name "Try (6):  query :model_elementDef"
    :code
    "(
  // This example queries for all the element definitions.
  // The :model_elementDefs are objects (things inside curly brackets)
  // so the are represented by a unique integer (entity IDs).
  // They aren't too interesting; just used to navigate through the nested structure.
  // Note that the entity ID are small numbers because we aren't running RM in the server.
  // The only entities we know about are the ones in the LHS pane.

 $s1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
 $qf := query{[?x :model_elementDef ?ed]};
 $qf($s1)
)"}


   {:name "Try (7): Towards goal: query :element_name"
    :code
    "(
  // We'll start working towards something useful with the two schema.
  // In the next few examples, we'll discover how they differ.
  // Let's start by listing  all the element names in each schema.

  $s1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
  $s2 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content']);
  $qf := query{[?x :element_name ?name]};

  {'schema 1': $qf($s1),
   'schema 2': $qf($s2)}
)"}

   {:name "Try (8): Child elements"
    :code
    "(
  // Let's find the children of an element.
  // In the schema design, I tried to give things meaningful names
  // (of course CCT names didn't change).
  // 'model/sequence' is supposed to be the general notion of a sequence of things.
  // There are probably a few patterns in the schema data for getting parent/child relationships.
  // For this data, however, there is only one pattern;
  // it starts with :model_sequence and ends with :element_name.
  // We could use datalog rules to catch pattern... it is on my ToDo list.
  // With this pattern with might just do $query{(parentChild ?parent ?child)}.

  $s1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
  $s2 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content']);
  $qf := query{[?x :element_name ?parent]
               [?x :element_complexType ?cplx1]
               [?cplx1 :model_sequence ?def]      // The pattern 'starts over again'.
               [?def   :model_elementDef ?cplx2]
               [?cplx2 :element_name ?child]};

  {'schema 1': $qf($s1),
   'schema 2': $qf($s2)}
)"}

   {:name "Try (9): Roots"
    :code
    "(
  // The two lists we generated in (6) each have one less element than the lists
  // we generated in (5), where we were just pulling out :element/name, wherever it occurs.
  // Of course this is because root elements don't have parents.
  // I suppose there are two patterns in the schema for picking off roots:
  // (1) :schema_content ->                    :model_elementDef -> :element_name.
  // (2) :schema_content -> :model_sequence -> :model_elementDef -> :element_name.

  $s1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
  $s2 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content']);

  $qf1 := query{[?c :schema_content ?e]      // pattern 1
                [?e :model_elementDef ?d]
                [?d :element_name ?name]};

  $qf2 := query{[?c :schema_content ?e]      // pattern 2
                [?e :model_sequence ?s]
                [?s :model_elementDef ?d]
                [?d :element_name ?name]};


  {'pattern 1': {'schema 1': {'roots': $qf1($s1)},
                 'schema 2': {'roots': $qf1($s2)}},
   'pattern 2': {'schema 1': {'roots': $qf2($s1)},
                 'schema 2': {'roots': $qf2($s2)}}};
)"}

   #_{:name "2023-05-10, (8): Nested structure ('shape')"
    :code
    "(
  // The two lists we generated in (6) each have one less element than the lists
  // we generated in (5), where we were just pulling out :element/name, wherever it occurs.
  // Of course this is because root elements don't have parents.
  // I suppose there are two patterns in the schema for picking off roots:
  // (1) :schema_content ->                    :model_elementDef -> :element_name.
  // (2) :schema_content -> :model_sequence -> :model_elementDef -> :element_name.

  $s1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
  $s2 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content']);
  $qfRoots := query{[?c :schema_content ?e]      // pattern 1
                    [?e :model_elementDef ?d]
                    [?d :element_name ?name]};

    $roots1 := $qfRoots($s1).`?name`;
    $roots2 := $qfRoots($s2).`?name`;
    $roots := [$roots1, roots2];
    $roots := [['ProcessInvoice'], ['ProcessInvoice']];

)"}

   {:name "Try (10): Shape "
    :code
   "(
  $schema1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
  $schema2 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content']);

  $pcQuery := query{[?x     :element_name        ?parent] // pc = 'parent/child'
                    [?x     :element_complexType ?cplx1]
                    [?cplx1 :model_sequence      ?def]
                    [?def   :model_elementDef    ?cplx2]
                    [?cplx2 :element_name        ?child]};

  $rootQuery := query{[?c :schema_content   ?e]
                      [?e :model_elementDef ?d]
                      [?d :element_name     ?name]};

  // This function just gets the children for a parent.
  $children := function($spc, $p) { $spc[?parent = $p].?child };

  // This function calls itself recursively to build the schema shape, starting from the root.
  $shape := function($p, $spc) { $reduce($children($spc, $p),
                                         function($tree, $c) // Update the tree.
                                             { $update($tree,
                                                       $p,
                                                       function($x) { $assoc($x, $c, $lookup($shape($c, $spc), $c) or '<data>')}) },
                                         {})};

  $schema1PC    := $pcQuery($schema1);     // Call the two queries with the two schema.
  $schema2PC    := $pcQuery($schema2);     // The first two return binding sets for {?parent x ?child y}
  $schema1Roots := $rootQuery($schema1);   // The last two return binding sets for {?name} (of a root).
  $schema2Roots := $rootQuery($schema2);

  {'shape1' : $shape($schema1Roots.?name[0], $schema1PC),
   'shape2' : $shape($schema2Roots.?name[0], $schema2PC)}
)"}

   {:name "Try (11): LLM match"
    :code
   "(
  $schema1 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_1_v2'], ['schema_content']);
  $schema2 := $get(['schema_name', 'urn:oagi-10.:elena.2023-07-02.ProcessInvoice-BC_2_v2'], ['schema_content']);

  $pcQuery := query{[?x     :element_name        ?parent] // pc = 'parent/child'
                    [?x     :element_complexType ?cplx1]
                    [?cplx1 :model_sequence      ?def]
                    [?def   :model_elementDef    ?cplx2]
                    [?cplx2 :element_name        ?child]};

  $rootQuery := query{[?c :schema_content   ?e]
                      [?e :model_elementDef ?d]
                      [?d :element_name     ?name]};

  // This function just gets the children for a parent.
  $children := function($spc, $p) { $spc[?parent = $p].?child };

  // This function calls itself recursively to build the schema shape, starting from the root.
  $shape := function($p, $spc) { $reduce($children($spc, $p),
                                         function($tree, $c) // Update the tree.
                                             { $update($tree,
                                                       $p,
                                                       function($x) { $assoc($x, $c, $lookup($shape($c, $spc), $c) or '<data>')}) },
                                         {})};

  $schema1PC    := $pcQuery($schema1);     // Call the two queries with the two schema.
  $schema2PC    := $pcQuery($schema2);     // The first two return binding sets for {?parent x ?child y}
  $schema1Roots := $rootQuery($schema1);   // The last two return binding sets for {?name} (of a root).
  $schema2Roots := $rootQuery($schema2);

  $llmMatch($shape($schema1Roots.?name[0], $schema1PC), // [0] here is cheating a bit; there could be multiple roots.
            $shape($schema2Roots.?name[0], $schema2PC))
)"}

   {:name "Try (12): LLM extract"
    :code "$llmExtract('Acme Widgets, 100 Main Street, Bldg 123, Chicago, IL, 60610', 'building')"}

   {:name "2 Databases"
    :code
    "( $qFn :=  query(){[$DBa ?e1 :email ?id]
                   [$DBb ?e2 :id    ?id]
                   [$DBa ?e1 :name  ?name]
                   [$DBa ?e1 :aAttr ?aData]
                   [$DBb ?e2 :bAttr ?bData]};

  $bSet := $qFn($DBa, $DBb);

  $eFn := express(){{?id : {'name'  : ?name,
                            'aData' : ?aData,
                            'bData' : ?bData}}};

  $reduce($bSet, $eFn)
)"
    :data
    "  $DBa := [{'email' : 'bob@example.com', 'aAttr' : 'Bob-A-data',   'name' : 'Bob'},
           {'email' : 'alice@alice.org', 'aAttr' : 'Alice-A-data', 'name' : 'Alice'}];

  $DBb := [{'id' : 'bob@example.com', 'bAttr' : 'Bob-B-data'},
           {'id' : 'alice@alice.org', 'bAttr' : 'Alice-B-data'}];"}


;;;======================= Simple queries
   {:name "Simple queries"
    :code
    "( $DBa := [{'id' : 123, 'aAttr' : 'A-value'}];
  $DBb := [{'id' : 123, 'bAttr' : 'B-value'}];

  // query returns a function.
  $aFn := query{[?e :aAttr ?aData]};
  $bFn := query{[?e :bAttr ?bData]};

  // Call the function on data.
  $aRes := $aFn($DBa);
  $bRes := $bFn($DBb);

  // Get back binding sets.
  [$aRes, $bRes]
)"
    :data ""}])
