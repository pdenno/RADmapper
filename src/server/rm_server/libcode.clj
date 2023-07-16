(ns rm-server.libcode
  "A collection of library functions to be pre-loaded into the codelib DB."
  (:require
   [rad-mapper.builtin :as bi]))

(def schema1 {"ProcessInvoice"
              {"ApplicationArea"
               {"CreationDateTime" "2023-07-10"},
               "DataArea"  {"Invoice"
                            {"InvoiceHeader"  {"PurchaseOrderReference"  {"ID"  "PO-1234"}},
                             "InvoiceLine"    {"BuyerParty"  {"Location"  {"Address"
                                                                           {"AddressLine"  "123 Mockingbird Lane, Gaithersburg MD, 20878"}},
                                                              "TaxIDSet"  {"ID"  "tax-id-999"}},
                                               "Item"        {"ManufacturingParty"  {"Name"  "Acme Widget"}}}},
                            "Process"  "Hey new stuff!"}}})

(def schema2  {"ProcessInvoice"
               {"ApplicationArea"
                {"CreationDateTime"  "2023-07-10"},
                "DataArea"
                {"Invoice"
                 {"InvoiceLine"
                  {"BuyerParty"
                   {"Location"
                    {"Address"
                     {"BuildingNumber"  "111",
                      "CityName"  "South Windsor",
                      "CountryCode"  "US",
                      "PostalCode"  "06074",
                      "StreetName"  "Clinton Drive"}},
                    "TaxIDSet"             {"ID"  "Tax-id-123"}},
                   "Item"                     {"ManufacturingParty"  {"Name"  "Acme Widget"}},
                   "PurchaseOrderReference"  {"ID"  "PO-ref-number-1888"}}},
                 "Process"  "Some process data"}}})

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
    :fn_doc "This defines the validated result for mapping Elena's July schema1 to schema2."
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
    :fn_doc "This defines the function for mapping Elena's July schema1 to schema2."
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

   {:fn_name "invoice-match-2->1-fn"
    :fn_doc "This defines the function for mapping Elena's July schema2 to schema1."
    :fn_src "// Here we assume that we validated the $llmMatch result, and stored it as this.
function($data){
{'ProcessInvoice':
  {'ApplicationArea':
    {'CreationDateTime': $data.ProcessInvoice.ApplicationArea.CreationDateTime},
    'DataArea'       :
    {'Invoice':
      {'InvoiceLine':
        {'BuyerParty':
          {'Location':
            {'Address':
              {'AddressLine': $data.ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.BuildingNumber &
                              $data.ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.StreetName     &
                              $data.ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.Location.Address.CityName}}},
            'TaxIDSet': {'ID': $data.ProcessInvoice.DataArea.Invoice.InvoiceLine.BuyerParty.TaxIDSet.ID}},
          'Item'  : {'ManufacturingParty': {'Name': $data.ProcessInvoice.DataArea.Invoice.InvoiceLine.Item.ManufacturingParty.Name}},
          'PurchaseOrderReference': {'ID': $data.ProcessInvoice.DataArea.Invoice.InvoiceLine.PurchaseOrderReference.ID}}},
      'Process': $data.ProcessInvoice.DataArea.Process}}}"}

   {:fn_name "bie-1-data"
    :fn_doc "Example instance data for Elena's July schema 1"
    :fn_src (bi/pprint-obj schema1)}

   {:fn_name "bie-2-data"
    :fn_doc "Example instance data for Elena's July schema 2"
    :fn_src (bi/pprint-obj schema2)}

   {:fn_name "1->2results"
    :fn_doc "In case of networking problems, etc."
    :fn_src "{'ProcessInvoice':
  {'DataArea':
    {'ApplicationArea':
      {'CreationDateTime': '2023-07-10'},
      'Invoice'        :
      {'InvoiceLine':
        {'BuyerParty':
          {'Location':
            {'Address': {'BuildingNumber': '123', 'CityName': 'Gaithersburg', 'PostalCode': '20878', 'StreetName': 'Mockingbird Lane'}},
            'TaxIDSet':
            {'ID': 'tax-id-999'}},
          'Item'                  :
          {'ManufacturingParty': {'Name': 'Acme Widget'}},
          'PurchaseOrderReference':
          {'ID': 'PO-1234'}},
        'Process'    :
        'Hey new stuff!'}}}}"}])
