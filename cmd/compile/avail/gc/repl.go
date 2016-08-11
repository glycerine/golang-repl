package gc

import (
	"github.com/glycerine/golang-repl/cmd/avail/obj/x86"
	"log"
)

// replInit duplicates most of compile/main.go's Main(), but preps for interaction instead.
func replInit() {

	// disable timestamps for reproducible output
	log.SetFlags(0)
	log.SetPrefix("compile: ")
	Thearch.LinkArch = &x86.Linkamd64
	initMain()
}
