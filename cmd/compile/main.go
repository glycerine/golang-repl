// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"fmt"
	"github.com/glycerine/golang-repl/cmd/avail/obj"
	"github.com/glycerine/golang-repl/cmd/compile/avail/amd64"
	"github.com/glycerine/golang-repl/cmd/compile/avail/x86"
	"log"
	"os"
)

func main() {
	// disable timestamps for reproducible output
	log.SetFlags(0)
	log.SetPrefix("compile: ")

	switch obj.Getgoarch() {
	default:
		fmt.Fprintf(os.Stderr, "compile: unknown architecture %q\n", obj.Getgoarch())
		os.Exit(2)
	case "386":
		x86.Main()
	case "amd64", "amd64p32":
		amd64.Main()
	}
}
