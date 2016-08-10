// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	"crypto/sha256"
	"fmt"
	//"github.com/glycerine/golang-repl/cmd/avail/bio"
	"github.com/glycerine/golang-repl/cmd/avail/obj"
	"io"
	"strconv"
)

// architecture-independent object file output
const (
	ArhdrSize = 60
)

func Linksym(s *Sym) *obj.LSym {
	if s == nil {
		return nil
	}
	if s.Lsym != nil {
		return s.Lsym
	}
	var name string
	if isblanksym(s) {
		name = "_"
	} else if s.Linkname != "" {
		name = s.Linkname
	} else {
		name = s.Pkg.Prefix + "." + s.Name
	}

	ls := obj.Linklookup(Ctxt, name, 0)
	s.Lsym = ls
	return ls
}

func duintxx(s *Sym, off int, v uint64, wid int) int {
	return duintxxLSym(Linksym(s), off, v, wid)
}

func duintxxLSym(s *obj.LSym, off int, v uint64, wid int) int {
	// Update symbol data directly instead of generating a
	// DATA instruction that liblink will have to interpret later.
	// This reduces compilation time and memory usage.
	off = int(Rnd(int64(off), int64(wid)))

	return int(obj.Setuintxx(Ctxt, s, int64(off), v, int64(wid)))
}

func duint8(s *Sym, off int, v uint8) int {
	return duintxx(s, off, uint64(v), 1)
}

func duint16(s *Sym, off int, v uint16) int {
	return duintxx(s, off, uint64(v), 2)
}

func duint32(s *Sym, off int, v uint32) int {
	return duintxx(s, off, uint64(v), 4)
}

func duintptr(s *Sym, off int, v uint64) int {
	return duintxx(s, off, v, Widthptr)
}

// stringConstantSyms holds the pair of symbols we create for a
// constant string.
type stringConstantSyms struct {
	hdr  *obj.LSym // string header
	data *obj.LSym // actual string data
}

// stringConstants maps from the symbol name we use for the string
// contents to the pair of linker symbols for that string.
var stringConstants = make(map[string]stringConstantSyms, 100)

func stringsym(s string) (hdr, data *obj.LSym) {
	var symname string
	if len(s) > 100 {
		// Huge strings are hashed to avoid long names in object files.
		// Indulge in some paranoia by writing the length of s, too,
		// as protection against length extension attacks.
		h := sha256.New()
		io.WriteString(h, s)
		symname = fmt.Sprintf(".gostring.%d.%x", len(s), h.Sum(nil))
	} else {
		// Small strings get named directly by their contents.
		symname = strconv.Quote(s)
	}

	const prefix = "go.string."
	symdataname := prefix + symname

	// All the strings have the same prefix, so ignore it for map
	// purposes, but use a slice of the symbol name string to
	// reduce long-term memory overhead.
	key := symdataname[len(prefix):]

	if syms, ok := stringConstants[key]; ok {
		return syms.hdr, syms.data
	}

	symhdrname := "go.string.hdr." + symname

	symhdr := obj.Linklookup(Ctxt, symhdrname, 0)
	symdata := obj.Linklookup(Ctxt, symdataname, 0)

	stringConstants[key] = stringConstantSyms{symhdr, symdata}

	// string header
	off := 0
	off = dsymptrLSym(symhdr, off, symdata, 0)
	off = duintxxLSym(symhdr, off, uint64(len(s)), Widthint)
	ggloblLSym(symhdr, int32(off), obj.DUPOK|obj.RODATA|obj.LOCAL)

	// string data
	off = dsnameLSym(symdata, 0, s)
	ggloblLSym(symdata, int32(off), obj.DUPOK|obj.RODATA|obj.LOCAL)

	return symhdr, symdata
}

func Datastring(s string, a *obj.Addr) {
	_, symdata := stringsym(s)
	a.Type = obj.TYPE_MEM
	a.Name = obj.NAME_EXTERN
	a.Sym = symdata
	a.Offset = 0
	a.Etype = uint8(Simtype[TINT])
}

func datagostring(sval string, a *obj.Addr) {
	symhdr, _ := stringsym(sval)
	a.Type = obj.TYPE_MEM
	a.Name = obj.NAME_EXTERN
	a.Sym = symhdr
	a.Offset = 0
	a.Etype = uint8(TSTRING)
}
func dsnameLSym(s *obj.LSym, off int, t string) int {
	s.WriteString(Ctxt, int64(off), len(t), t)
	return off + len(t)
}

func dsymptr(s *Sym, off int, x *Sym, xoff int) int {
	return dsymptrLSym(Linksym(s), off, Linksym(x), xoff)
}

func dsymptrLSym(s *obj.LSym, off int, x *obj.LSym, xoff int) int {
	off = int(Rnd(int64(off), int64(Widthptr)))
	s.WriteAddr(Ctxt, int64(off), Widthptr, x, int64(xoff))
	off += Widthptr
	return off
}

func dsymptrOffLSym(s *obj.LSym, off int, x *obj.LSym, xoff int) int {
	s.WriteOff(Ctxt, int64(off), x, int64(xoff))
	off += 4
	return off
}
