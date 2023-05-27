(ns rm-exerciser.app.components.examples)

(declare rm-examples elena-schemas)

(defn get-example [name]
  (some #(when (= name (:name %)) %) rm-examples))

;;; ($get [["schema/name" "urn:oagis-10.8.4:Nouns:Invoice"],  ["schema-object"]])
(def rm-examples
  [{:name "2023-05-17, (0): Schema list"
    :code "$get([['list/id', 'ccts/message-schema'], ['list/content']])"}

   {:name "2023-05-17, (1): Small remote query"
    :code "( $db  := $get([['db/name', 'schemaDB'], ['db/connection']]);
  $qfn := query{[?e :schema/name ?name]};
  $qfn($db) )"}

   {:name "2023-05-17, (2) : Simple get"
    :code "( $schema := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1'], ['schema/content']]);
           $schema )"}

   {:name "2023-05-17, (3) : JSONata-like"
    :code
    "( // Ordinary JSONata-like expressions: get the names of the two schema in the LHS pane:
  $s1 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1'], ['schema/content', 'schema/name']]);
  $s2 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_2'], ['schema/content', 'schema/name']]);

[$s1, $s2].`schema_name` )"}

   #_{:name "2023-05-17, (2) : Simplest query"
    :code
"(
  $x := {'element/name' : 'foo'};
  $qf := query{[?x :element/name ?name]};
  $qf($x)
)"}

   {:name "2023-05-17, (3): Simple query, complicated schema"
    :code "(
  // Small bug in the exerciser (because it combines data from the LHS pane):
  // currently comments have to be inside the open paren.
  // Here we put $s1 and $s2 into a vector, $db, so we can work on them together.
  // We could also just call the query on either $s1 or $s2, of course,
  // or don't create the $db and just call $qf with [$s1, $s2].

  $s1 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1'], ['schema/name', 'schema/content']]);
  $s2 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_2'], ['schema/name', 'schema/content']]);

 $db := [$s1, $s2];
 $qf := query{[?x :schema_name ?name]};
 $qf($db)
)" }

   #_{:name "2023-05-17, (*): (aside) Query defines a function."
    :code
    "(
  // Remember: query and express are function defining.
  // If you run the following you just get <<function>>

  query{[?x :model/elementDef ?ed]}
)"}

   {:name "2023-05-17, (4):  query :model/elementDef"
    :code
    "(
  // This example queries for all the element definitions.
  // The :model/elementDefs are objects (things inside curly brackets)
  // so the are represented by a unique integer (entity IDs).
  // They aren't too interesting; just used to navigate through the nested structure.
  // Note that the entity ID are small numbers because we aren't running RM in the server.
  // The only entities we know about are the ones in the LHS pane.

 $s1 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1'], ['schema/content']]);
 $qf := query{[?x :model_elementDef ?ed]};
 $qf($s1)
)"}


   {:name "2023-05-17, (5): Towards goal: query :element/name"
    :code
    "(
  // We'll start working towards something useful with the two schema.
  // In the next few examples, we'll discover how they differ.
  // Let's start by listing  all the element names in each schema.

  $s1 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1'], ['schema/content']]);
  $s2 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_2'], ['schema/content']]);
  $qf := query{[?x :element_name ?name]};

  {'schema 1': $qf($s1),
   'schema 2': $qf($s2)}
)"}

   {:name "2023-05-17, (6): Child elements"
    :code
    "(
  // Let's find the children of an element.
  // In the schema design, I tried to give things meaningful names
  // (of course CCT names didn't change).
  // 'model/sequence' is supposed to be the general notion of a sequence of things.
  // There are probably a few patterns in the schema data for getting parent/child relationships.
  // For this data, however, there is only one pattern;
  // it starts with :model/sequence and ends with :element/name.
  // We could use datalog rules to catch pattern... it is on my ToDo list.
  // With this pattern with might just do $query{(parentChild ?parent ?child)}.

  $s1 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1'], ['schema/content']]);
  $s2 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_2'], ['schema/content']]);
  $qf := query{[?x :element_name ?parent]
               [?x :element_complexType ?cplx1]
               [?cplx1 :model_sequence ?def]      // The pattern 'starts over again'.
               [?def   :model_elementDef ?cplx2]
               [?cplx2 :element_name ?child]};

  {'schema 1': $qf($s1),
   'schema 2': $qf($s2)}
)"}

   {:name "2023-05-17, (7): Roots"
    :code
    "(
  // The two lists we generated in (6) each have one less element than the lists
  // we generated in (5), where we were just pulling out :element/name, wherever it occurs.
  // Of course this is because root elements don't have parents.
  // I suppose there are two patterns in the schema for picking off roots:
  // (1) :schema/content ->                    :model/elementDef -> :element/name.
  // (2) :schema/content -> :model/sequence -> :model/elementDef -> :element/name.

  $s1 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1'], ['schema/content']]);
  $s2 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_2'], ['schema/content']]);

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
  // (1) :schema/content ->                    :model/elementDef -> :element/name.
  // (2) :schema/content -> :model/sequence -> :model/elementDef -> :element/name.

  $s1 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1'], ['schema/content']]);
  $s2 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_2'], ['schema/content']]);
  $qfRoots := query{[?c :schema_content ?e]      // pattern 1
                    [?e :model_elementDef ?d]
                    [?d :element_name ?name]};

    $roots1 := $qfRoots($s1).`?name`;
    $roots2 := $qfRoots($s2).`?name`;
    $roots := [$roots1, roots2];
    $roots := [['ProcessInvoice'], ['ProcessInvoice']];

)"}

   {:name "2023-05-17, (8): Shape "
    :code
   "(
  $schema1 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1'], ['schema/content']]);
  $schema2 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_2'], ['schema/content']]);

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

   {:name "2023-05-17, (9): Semantic match"
    :code
   "(
  $schema1 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_1'], ['schema/content']]);
  $schema2 := $get([['schema/name', 'urn:oagi-10.unknown:elena.2023-02-09.ProcessInvoice-BC_2'], ['schema/content']]);

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

  $semMatch($shape($schema1Roots.?name[0], $schema1PC), // [0] here is cheating a bit; there could be multiple roots.
            $shape($schema2Roots.?name[0], $schema2PC))
)"}


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
