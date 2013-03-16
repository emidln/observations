# observations

Observations are simple factual statements that map into core.logic
relations and SQL tables.  The mapping is straightfoward, and allows
the same data model to be used for core.logic programs in memory, as
well as persistance in a database and later analysis and querying via
SQL.

## Usage

    (use 'observations.core)

	(defobs process-modified-path
	    [#^{:tag Integer :required true :index true} pid
 	     #^{:tag Pathname :required true :index true
			 :doc "A filename or a directory." } path]
        :doc "A pathname modified by a process, associated by the PID."
		:scope [:sample]
        :tags ["process" "file" "directory" "path"])

## License

Copyright © 2013 ThreatGRID

Distributed under the Eclipse Public License, the same as Clojure.
