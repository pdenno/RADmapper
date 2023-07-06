(ns rm-server.libcode
  "A collection of library functions to be pre-loaded into the codelib DB.")

(def library-code
  [{:fn_name "addOne"
    :fn_src  "function($x){$x + 1}"
    :fn_doc  "Add one to the (numeric) argument. This is just for testing, of course."}

   {:fn_name "schemaParentChild"
    :fn_src
    "query{[?x     :element_name        ?parent]
       [?x     :element_complexType ?cplx1]
       [?cplx1 :model_sequence      ?def]
       [?def   :model_elementDef    ?cplx2]
       [?cplx2 :element_name        ?child]}"
    :fn_doc "Query a standard schema for parent/child relationships"}

   {:fn_name "schemaRoots"
    :fn_src
    "query{[?c :schema_content   ?e]
       [?e :model_elementDef ?d]
       [?d :element_name     ?name]}"
    :fn_doc "Query a standard schema for top-level element_names"}

   {:fn_name "schemaShape"
    :fn_src
    "(// This function just gets the children for a parent.
      $children := function($spc, $p) { $spc[?parent = $p].?child };
      // This function calls itself recursively to build the schema shape, starting from the root.
      // Thus it is important to assign it to a variable named same as the recursive call in the code.
      $shape := function($p, $spc) { $reduce($children($spc, $p),
                                            function($tree, $c) // Update the tree.
                                            { $update($tree,
                                                      $p,
                                                      function($x) { $assoc($x, $c, $lookup($shape($c, $spc), $c) or '<data>')}) },
                                            {})})"
    :fn_doc "Return the schema shape (nesting structure of elements) as used by $llmMatch"}])
