// Copyright 2015 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"github.com/glycerine/golang-repl/cmd/compile/avail/amd64"
	"github.com/glycerine/golang-repl/cmd/compile/avail/arm"
	"github.com/glycerine/golang-repl/cmd/compile/avail/arm64"
	"github.com/glycerine/golang-repl/cmd/compile/avail/mips64"
	"github.com/glycerine/golang-repl/cmd/compile/avail/ppc64"
	"github.com/glycerine/golang-repl/cmd/compile/avail/s390x"
	"github.com/glycerine/golang-repl/cmd/compile/avail/x86"
	"github.com/glycerine/golang-repl/cmd/avail/obj"
	"fmt"
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
	case "arm":
		arm.Main()
	case "arm64":
		arm64.Main()
	case "mips64", "mips64le":
		mips64.Main()
	case "ppc64", "ppc64le":
		ppc64.Main()
	case "s390x":
		s390x.Main()
	}
}
