// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

import (
	//"bufio"
	//"bytes"
	"fmt"
	//"github.com/glycerine/golang-repl/cmd/avail/bio"
	"sort"
	"unicode"
	"unicode/utf8"
)

var (
	//	newexport    bool // if set, use new export format
	Debug_export int // if set, print debugging information about export data
	exportsize   int
)

func exportf(format string, args ...interface{}) {
	n, _ := fmt.Fprintf(bout, format, args...)
	exportsize += n
	if Debug_export != 0 {
		fmt.Printf(format, args...)
	}
}

//jea from go.go
var exportlist []*Node

func exportname(s string) bool {
	if r := s[0]; r < utf8.RuneSelf {
		return 'A' <= r && r <= 'Z'
	}
	r, _ := utf8.DecodeRuneInString(s)
	return unicode.IsUpper(r)
}

func initname(s string) bool {
	return s == "init"
}

// exportedsym reports whether a symbol will be visible
// to files that import our package.
func exportedsym(sym *Sym) bool {
	// Builtins are visible everywhere.
	if sym.Pkg == builtinpkg || sym.Origpkg == builtinpkg {
		return true
	}

	return sym.Pkg == localpkg && exportname(sym.Name)
}
func dumppkg(p *Pkg) {
	if p == nil || p == localpkg || p.Exported || p == builtinpkg {
		return
	}
	p.Exported = true
	suffix := ""
	if !p.Direct {
		suffix = " // indirect"
	}
	exportf("\timport %s %q%s\n", p.Name, p.Path, suffix)
}

// Look for anything we need for the inline body
func reexportdeplist(ll Nodes) {
	for _, n := range ll.Slice() {
		reexportdep(n)
	}
}

func reexportdep(n *Node) {
	if n == nil {
		return
	}

	//print("reexportdep %+hN\n", n);
	switch n.Op {
	case ONAME:
		switch n.Class {
		// methods will be printed along with their type
		// nodes for T.Method expressions
		case PFUNC:
			if n.Left != nil && n.Left.Op == OTYPE {
				break
			}

			// nodes for method calls.
			if n.Type == nil || n.Type.Recv() != nil {
				break
			}
			fallthrough

		case PEXTERN:
			if n.Sym != nil && !exportedsym(n.Sym) {
				if Debug['E'] != 0 {
					fmt.Printf("reexport name %v\n", n.Sym)
				}
				exportlist = append(exportlist, n)
			}
		}

	// Local variables in the bodies need their type.
	case ODCL:
		t := n.Left.Type

		if t != Types[t.Etype] && t != idealbool && t != idealstring {
			if t.IsPtr() {
				t = t.Elem()
			}
			if t != nil && t.Sym != nil && t.Sym.Def != nil && !exportedsym(t.Sym) {
				if Debug['E'] != 0 {
					fmt.Printf("reexport type %v from declaration\n", t.Sym)
				}
				exportlist = append(exportlist, t.Sym.Def)
			}
		}

	case OLITERAL:
		t := n.Type
		if t != Types[n.Type.Etype] && t != idealbool && t != idealstring {
			if t.IsPtr() {
				t = t.Elem()
			}
			if t != nil && t.Sym != nil && t.Sym.Def != nil && !exportedsym(t.Sym) {
				if Debug['E'] != 0 {
					fmt.Printf("reexport literal type %v\n", t.Sym)
				}
				exportlist = append(exportlist, t.Sym.Def)
			}
		}
		fallthrough

	case OTYPE:
		if n.Sym != nil && n.Sym.Def != nil && !exportedsym(n.Sym) {
			if Debug['E'] != 0 {
				fmt.Printf("reexport literal/type %v\n", n.Sym)
			}
			exportlist = append(exportlist, n)
		}

	// for operations that need a type when rendered, put the type on the export list.
	case OCONV,
		OCONVIFACE,
		OCONVNOP,
		ORUNESTR,
		OARRAYBYTESTR,
		OARRAYRUNESTR,
		OSTRARRAYBYTE,
		OSTRARRAYRUNE,
		ODOTTYPE,
		ODOTTYPE2,
		OSTRUCTLIT,
		OARRAYLIT,
		OPTRLIT,
		OMAKEMAP,
		OMAKESLICE,
		OMAKECHAN:
		t := n.Type

		switch t.Etype {
		case TARRAY, TCHAN, TPTR32, TPTR64, TSLICE:
			if t.Sym == nil {
				t = t.Elem()
			}
		}
		if t != nil && t.Sym != nil && t.Sym.Def != nil && !exportedsym(t.Sym) {
			if Debug['E'] != 0 {
				fmt.Printf("reexport type for expression %v\n", t.Sym)
			}
			exportlist = append(exportlist, t.Sym.Def)
		}
	}

	reexportdep(n.Left)
	reexportdep(n.Right)
	reexportdeplist(n.List)
	reexportdeplist(n.Rlist)
	reexportdeplist(n.Ninit)
	reexportdeplist(n.Nbody)
}

// methodbyname sorts types by symbol name.
type methodbyname []*Field

func (x methodbyname) Len() int           { return len(x) }
func (x methodbyname) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }
func (x methodbyname) Less(i, j int) bool { return x[i].Sym.Name < x[j].Sym.Name }

func dumpexporttype(t *Type) {
	if t == nil {
		return
	}
	if t.Printed || t == Types[t.Etype] || t == bytetype || t == runetype || t == errortype {
		return
	}
	t.Printed = true

	if t.Sym != nil {
		dumppkg(t.Sym.Pkg)
	}

	switch t.Etype {
	case TSTRUCT, TINTER:
		for _, f := range t.Fields().Slice() {
			dumpexporttype(f.Type)
		}
	case TFUNC:
		dumpexporttype(t.Recvs())
		dumpexporttype(t.Results())
		dumpexporttype(t.Params())
	case TMAP:
		dumpexporttype(t.Val())
		dumpexporttype(t.Key())
	case TARRAY, TCHAN, TPTR32, TPTR64, TSLICE:
		dumpexporttype(t.Elem())
	}

	if t.Sym == nil {
		return
	}

	var m []*Field
	for _, f := range t.Methods().Slice() {
		dumpexporttype(f.Type)
		m = append(m, f)
	}
	sort.Sort(methodbyname(m))

	exportf("\ttype %v %v\n", sconv(t.Sym, FmtSharp), Tconv(t, FmtSharp|FmtLong))
	for _, f := range m {
		if f.Nointerface {
			exportf("\t//go:nointerface\n")
		}
		if f.Type.Nname() != nil && f.Type.Nname().Func.Inl.Len() != 0 { // nname was set by caninl

			// when lazily typechecking inlined bodies, some re-exported ones may not have been typechecked yet.
			// currently that can leave unresolved ONONAMEs in import-dot-ed packages in the wrong package
			if Debug['l'] < 2 {
				//jea typecheckinl(f.Type.Nname())
			}
			exportf("\tfunc %v %v %v { %v }\n", Tconv(f.Type.Recvs(), FmtSharp), sconv(f.Sym, FmtShort|FmtByte|FmtSharp), Tconv(f.Type, FmtShort|FmtSharp), hconv(f.Type.Nname().Func.Inl, FmtSharp|FmtBody))
			reexportdeplist(f.Type.Nname().Func.Inl)
		} else {
			exportf("\tfunc %v %v %v\n", Tconv(f.Type.Recvs(), FmtSharp), sconv(f.Sym, FmtShort|FmtByte|FmtSharp), Tconv(f.Type, FmtShort|FmtSharp))
		}
	}
}

// importsym declares symbol s as an imported object representable by op.
func importsym(s *Sym, op Op) {
	if s.Def != nil && s.Def.Op != op {
		pkgstr := fmt.Sprintf("during import %q", importpkg.Path)
		redeclare(s, pkgstr)
	}

	// mark the symbol so it is not reexported
	if s.Def == nil {
		if Debug['A'] != 0 || exportname(s.Name) || initname(s.Name) {
			s.Flags |= SymExport
		} else {
			s.Flags |= SymPackage // package scope
		}
	}
}

// pkgtype returns the named type declared by symbol s.
// If no such type has been declared yet, a forward declaration is returned.
func pkgtype(s *Sym) *Type {
	importsym(s, OTYPE)
	if s.Def == nil || s.Def.Op != OTYPE {
		t := typ(TFORW)
		t.Sym = s
		s.Def = typenod(t)
		s.Def.Name = new(Name)
	}

	if s.Def.Type == nil {
		Yyerror("pkgtype %v", s)
	}
	return s.Def.Type
}

// numImport tracks how often a package with a given name is imported.
// It is used to provide a better error message (by using the package
// path to disambiguate) if a package that appears multiple times with
// the same name appears in an error message.
var numImport = make(map[string]int)

func importimport(s *Sym, path string) {
	// Informational: record package name
	// associated with import path, for use in
	// human-readable messages.

	if isbadimport(path) {
		errorexit()
	}
	p := mkpkg(path)
	if p.Name == "" {
		p.Name = s.Name
		numImport[s.Name]++
	} else if p.Name != s.Name {
		Yyerror("conflicting names %s and %s for package %q", p.Name, s.Name, p.Path)
	}

	if incannedimport == 0 && myimportpath != "" && path == myimportpath {
		Yyerror("import %q: package depends on %q (import cycle)", importpkg.Path, path)
		errorexit()
	}
}

// importconst declares symbol s as an imported constant with type t and value n.
func importconst(s *Sym, t *Type, n *Node) {
	importsym(s, OLITERAL)
	n = convlit(n, t)

	if s.Def != nil { // TODO: check if already the same.
		return
	}

	if n.Op != OLITERAL {
		Yyerror("expression must be a constant")
		return
	}

	if n.Sym != nil {
		n1 := *n
		n = &n1
	}

	n.Orig = newname(s)
	n.Sym = s
	declare(n, PEXTERN)

	if Debug['E'] != 0 {
		fmt.Printf("import const %v\n", s)
	}
}

// importvar declares symbol s as an imported variable with type t.
func importvar(s *Sym, t *Type) {
	importsym(s, ONAME)
	if s.Def != nil && s.Def.Op == ONAME {
		if Eqtype(t, s.Def.Type) {
			return
		}
		Yyerror("inconsistent definition for var %v during import\n\t%v (in %q)\n\t%v (in %q)", s, s.Def.Type, s.Importdef.Path, t, importpkg.Path)
	}

	n := newname(s)
	s.Importdef = importpkg
	n.Type = t
	declare(n, PEXTERN)

	if Debug['E'] != 0 {
		fmt.Printf("import var %v %v\n", s, Tconv(t, FmtLong))
	}
}

// importtype and importer.importtype (bimport.go) need to remain in sync.
func importtype(pt *Type, t *Type) {
	// override declaration in unsafe.go for Pointer.
	// there is no way in Go code to define unsafe.Pointer
	// so we have to supply it.
	if incannedimport != 0 && importpkg.Name == "unsafe" && pt.Nod.Sym.Name == "Pointer" {
		t = Types[TUNSAFEPTR]
	}

	if pt.Etype == TFORW {
		n := pt.Nod
		copytype(pt.Nod, t)
		pt.Nod = n // unzero nod
		pt.Sym.Importdef = importpkg
		pt.Sym.Lastlineno = lineno
		declare(n, PEXTERN)
		checkwidth(pt)
	} else if !Eqtype(pt.Orig, t) {
		Yyerror("inconsistent definition for type %v during import\n\t%v (in %q)\n\t%v (in %q)", pt.Sym, Tconv(pt, FmtLong), pt.Sym.Importdef.Path, Tconv(t, FmtLong), importpkg.Path)
	}

	if Debug['E'] != 0 {
		fmt.Printf("import type %v %v\n", pt, Tconv(t, FmtLong))
	}
}
