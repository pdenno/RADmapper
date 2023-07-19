# RADmapper
Exploratory implementation of an interoperable exchange form for mapping, and a web-based exerciser for the language.

This library is an implementation of a language designed to serve as an *interoperable exchange form* for expressing the intent of many mapping and data restructuring needs.
As an interoperable exchange form, it is intended that the language can be translated (by humans and machine) into mapping specification in other languages.
For example, it should be possible to translate statements in the exchange form into mapping specification used by commercial mapping tools.
The strategy for interoperability is to provide, in addition to a typical programming language grammar, a canonical serialization of abstract syntax trees (ASTs) of RADmapper code.
The canonical serialization of ASTs can be viewed as the exchange form, and the programming language as a means of expressing and validating mapping requirements.
The ASTs can be, for example,  consumed by other tools for translating into other languages or reasoned about in joint (human/AI) cognitive work; we will demonstrate this in our research.
The work is done in cooperation with the [Open Application Group](https://oagi.org/) (OAGi).

The language borrows predominantly from [JSONata](https://jsonata.org/) language, but also includes provisions for 
mapping to/from forms other than JSON. Currently the only additional forms investigated/implemented are tables (e.g. Excel) and XML.
The goal of the  work is to describe the relationships between data in ways that are useful to human business analysts, machine translation, 
mapping tools, and integration platforms such as the [Open Integration Hub](https://www.openintegrationhub.org/?lang=en). 
We are starting with a core capability based on JSONata, but we are considering a wider set of requirement including 
    
1. investigation of mapping to/from APIs, knowledge graphs, business terminology, and standards messaging such as OAS, OAGi, UBL, EDI, 
2. alternative query styles (e.g. like Datalog), and 
3. alternative restructuring styles including in-place updating, bi-directional updating, and constraint satisfaction.
    
We are considering use cases while experimenting with solutions. 
The engine/library in this repository is an implementation of whatever it is we conceive. 

To support networks of data, the mapping language borrows ideas from the Object Management Group's Queries, Views and Transformation relational (QVT-r)
mapping language, and Datalog.

## Maturity 
The software is currently undergoing active development. 
At points were is is most stable, Docker images of the exerciser are created: [here](https://hub.docker.com/repository/docker/podenno/rm-exerciser).
The code is a Clojure library. Currently it implements 
  - nearly all of the JSONata syntax and built-in functions,
  - constructs for datalog-like query of multiple data sources,
  - higher-order functions for templating queries and expression,
  - LLM calls for structure matching and text extraction. 
  - mapping from JSON, XML, and Excel spreadsheets.

## Contacts
  * Peter Denno (peter.denno@nist.gov)

## Learn More
  - 
A draft of the [user guide](https://github.com/pdenno/interop-mapping/blob/main/interop-mapping.pdf) is updated occassionally.

The Dockerized [exerciser](https://hub.docker.com/repository/docker/podenno/rm-exerciser).
