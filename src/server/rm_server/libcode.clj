(ns rm-server.libcode
  "A collection of library functions to be pre-loaded into the codelib DB."
  (:require
   [rad-mapper.builtin :as bi]))

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
    :fn_doc "Return the schema shape (nesting structure of elements) as used by $llmMatch"}

   {:fn_name "invoice-match-1->2-pattern"
    :fn_src
"// Here we assume that we validated the $llmMatch result, and stored it as this.

 {'ProcessInvoice':
  {'ApplicationArea': {'CreationDateTime': 'ProcessInvoice.ApplicationArea.CreationDateTime'},
   'Process'        :  '<replace-me>',
   'DataArea'       : {'Invoice':
                        {'InvoiceLine':
                         {'BuyerParty':
                            {'Location' : {'Address'  : {'BuildingNumber':  {'extract-from': 'ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.AddressLine',
                                                                             'value'       : 'BuildingNumber'},
                                                         'CityName'      :  {'extract-from': 'ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.AddressLine',
                                                                             'value'       : 'CityName'},
                                                         'CountryCode'   :  '<replace-me>',
                                                         'PostalCode'    :  {'extract-from': 'ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.AddressLine',
                                                                             'value'       : 'PostalCode'},
                                                         'StreetName'    :  {'extract-from': 'ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.AddressLine',
                                                                             'value'       : 'StreetName'}}},
                             'TaxIDSet' : {'ID': '<replace-me>'}}},
                         'Item'                  : {'ManufacturingParty': {'Name': '<replace-me>'}},
                         'PurchaseOrderReference': {'ID': 'ProcessInvoice.DataArea.Invoice.InvoiceHeader.PurchaseOrderReference.ID'}}}}}"}

   {:fn_name "invoice-match-1->2-fn"
    :fn_src
    "// Here we assume that we validated the $llmMatch result, and stored it as this.

function($data){
 {'ProcessInvoice':
  {'DataArea':
    {'ApplicationArea':
      {'CreationDateTime': $data.ProcessInvoice.ApplicationArea.CreationDateTime},
      'Invoice'        :
      {'InvoiceLine':
        {'BuyerParty':
          {'Location':
            {'Address':
              {'BuildingNumber' :   $llmExtract($data.ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.AddressLine, 'BuildingNumber'),
                'CityName'      :   $llmExtract($data.ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.AddressLine, 'CityName'),
                'PostalCode'    :   $llmExtract($data.ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.AddressLine, 'PostalCode'),
                'StreetName'    :   $llmExtract($data.ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.AddressLine, 'StreetName')}},
            'TaxIDSet': {'ID': $data.ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.TaxIDSet.ID}},
          'Item' : {'ManufacturingParty': {'Name': $data.ProcessInvoice.DataArea.Invoice.InvoiceLine.Item.ManufacturingParty.Name}},
          'PurchaseOrderReference': {'ID': $data.ProcessInvoice.DataArea.Invoice.InvoiceHeader.PurchaseOrderReference.ID}},
        'Process' : $data.ProcessInvoice.DataArea.Process}}}}
  }"}

   {:fn_name "bie-1-message"
    :fn_src (-> "data/testing/json-for-bie/schema1.json" (bi/read-local  {}) bi/pprint-obj)}

   {:fn_name "bie-2-message"
    :fn_src (-> "data/testing/json-for-bie/schema2.json" (bi/read-local  {}) bi/pprint-obj)}])
