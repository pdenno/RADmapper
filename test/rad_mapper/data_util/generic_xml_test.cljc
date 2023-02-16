(ns rad-mapper.data-util.generic-xml-test
  "Functions to read XML to structures that the DB can use."
  (:require
   [clojure.test :refer [deftest is testing]]
   [rad-mapper.data-util.generic-xml :as gen-x]
   [rad-mapper.builtin :as bi]))


(def invoice-A
"<?xml version="1.0" encoding="UTF-8"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:vc="http://www.w3.org/2007/XMLSchema-versioning"
elementFormDefault="qualified" attributeFormDefault="unqualified" vc:minVersion="1.1">
        <xs:element name="Invoice">
                <xs:complexType>
                        <xs:sequence>
                                <xs:element name="Invoice_ID" type="xs:int" minOccurs="1" maxOccurs="2"/>
                                <xs:element name="Document_date" type="US_Date" minOccurs="1" maxOccurs="1"/>
                                <xs:element ref="Address" minOccurs="1" maxOccurs="1"/>
                        </xs:sequence>
                </xs:complexType>
        </xs:element>
        <xs:element name="Address">
                <xs:complexType>
                        <xs:sequence>
                                <xs:element name="Address_line_1" type="xs:string" minOccurs="1" maxOccurs="1"/>
                                <xs:element name="Address_line_2" type="xs:string" minOccurs="1" maxOccurs="1"/>
                        </xs:sequence>
                </xs:complexType>
        </xs:element>
        <xs:simpleType name="US_Date">
                <xs:restriction base="xs:string">
                        <xs:pattern value="(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])-\d{4}"/>
                </xs:restriction>
        </xs:simpleType>
</xs:schema>")


(defn tryme []
  (rewrite-xsd xmap :generic/xsd-file)
