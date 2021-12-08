# RADmapper
Exploratory implementation of an interoperable exchange form for mapping

This library is an implementation of a language designed to serve as an *interoperable exchange form* for expressing the intent of many mapping and data restructuring needs.
As an interoperable exchange form, it is intended that the language can be translated (by humans and machine) into mapping specification in other languages.
For example, it should be possible to translate statements in the exchange form into mapping specification used by commercial mapping tools.
The work is done in cooperation with the Open Application Group (OAGi).

The mapping language described borrows predominantly from the JSONata mapping language, but also includes provisions for 
mapping to/from forms other than JSON. Currently the only additional forms investigated/implemented are tables (e.g. Excel) and XML.
The goal of the  work is to describe the relationships between data in ways that are useful to human business analysts, machine translation, 
mapping tools, and integration platforms such as the Open Integration Hub. 
We are starting with a core capability based on JSONata, but we are considering a wider set of requirement including 
    
1. investigation of mapping to/from APIs, knowledge graphs, business terminology, and standards messaging such as OAS, OAGi, UBL, EDI, 
2. alternative query styles (e.g. like Datalog), and 
3. alternative restructuring styles including in-place updating, bi-directional updating, and constraint satisfaction.
    
We are considering use cases while experimenting with solutions. 
The engine/library in this repository is an implementation of whatever it is we conceive. 

To support networks of data, the mapping language borrows ideas from the Object Management Group's Queries, Views and Transformation relational (QVT-r)
mapping language, and Datalog.

## Status
The code is a Clojure library. Currently it implements a good portion of JSONata syntax and mapping from JSON, XML and Excel spreadsheets.
Short-term To Do List:

- Implement all of the JSONata built-in functions.
- Replace 'eval' with the Small Clojure Interpreter (SCI).
- Datalog functionality and demonstrations.
- A UI like try.jsonata.org. (It would be developed in another repository but usesthis one.)
