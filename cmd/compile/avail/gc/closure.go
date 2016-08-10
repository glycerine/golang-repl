// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

// function literals aka closures
func closurehdr(ntype *Node) {
	var name *Node
	var a *Node

	n := Nod(OCLOSURE, nil, nil)
	n.Func.Ntype = ntype
	n.Func.Depth = Funcdepth
	n.Func.Outerfunc = Curfn

	funchdr(n)

	// steal ntype's argument names and
	// leave a fresh copy in their place.
	// references to these variables need to
	// refer to the variables in the external
	// function declared below; see walkclosure.
	n.List.Set(ntype.List.Slice())

	n.Rlist.Set(ntype.Rlist.Slice())
	ntype.List.Set(nil)
	ntype.Rlist.Set(nil)
	for _, n1 := range n.List.Slice() {
		name = n1.Left
		if name != nil {
			name = newname(name.Sym)
		}
		a = Nod(ODCLFIELD, name, n1.Right)
		a.Isddd = n1.Isddd
		if name != nil {
			name.Isddd = a.Isddd
		}
		ntype.List.Append(a)
	}
	for _, n2 := range n.Rlist.Slice() {
		name = n2.Left
		if name != nil {
			name = newname(name.Sym)
		}
		ntype.Rlist.Append(Nod(ODCLFIELD, name, n2.Right))
	}
}

func closurebody(body []*Node) *Node {
	if len(body) == 0 {
		body = []*Node{Nod(OEMPTY, nil, nil)}
	}

	func_ := Curfn
	func_.Nbody.Set(body)
	func_.Func.Endlineno = lineno
	funcbody(func_)

	// closure-specific variables are hanging off the
	// ordinary ones in the symbol table; see oldname.
	// unhook them.
	// make the list of pointers for the closure call.
	for _, v := range func_.Func.Cvars.Slice() {
		// Unlink from v1; see comment in syntax.go type Param for these fields.
		v1 := v.Name.Defn
		v1.Name.Param.Innermost = v.Name.Param.Outer

		// If the closure usage of v is not dense,
		// we need to make it dense; now that we're out
		// of the function in which v appeared,
		// look up v.Sym in the enclosing function
		// and keep it around for use in the compiled code.
		//
		// That is, suppose we just finished parsing the innermost
		// closure f4 in this code:
		//
		//	func f() {
		//		v := 1
		//		func() { // f2
		//			use(v)
		//			func() { // f3
		//				func() { // f4
		//					use(v)
		//				}()
		//			}()
		//		}()
		//	}
		//
		// At this point v.Outer is f2's v; there is no f3's v.
		// To construct the closure f4 from within f3,
		// we need to use f3's v and in this case we need to create f3's v.
		// We are now in the context of f3, so calling oldname(v.Sym)
		// obtains f3's v, creating it if necessary (as it is in the example).
		//
		// capturevars will decide whether to use v directly or &v.
		v.Name.Param.Outer = oldname(v.Sym)
	}

	return func_
}

func typecheckclosure(func_ *Node, top int) {
	for _, ln := range func_.Func.Cvars.Slice() {
		n := ln.Name.Defn
		if !n.Name.Captured {
			n.Name.Captured = true
			if n.Name.Decldepth == 0 {
				Fatalf("typecheckclosure: var %v does not have decldepth assigned", Nconv(n, FmtShort))
			}

			// Ignore assignments to the variable in straightline code
			// preceding the first capturing by a closure.
			if n.Name.Decldepth == decldepth {
				n.Assigned = false
			}
		}
	}

	for _, ln := range func_.Func.Dcl {
		if ln.Op == ONAME && (ln.Class == PPARAM || ln.Class == PPARAMOUT) {
			ln.Name.Decldepth = 1
		}
	}

	oldfn := Curfn
	func_.Func.Ntype = typecheck(func_.Func.Ntype, Etype)
	func_.Type = func_.Func.Ntype.Type
	func_.Func.Top = top

	// Type check the body now, but only if we're inside a function.
	// At top level (in a variable initialization: curfn==nil) we're not
	// ready to type check code yet; we'll check it later, because the
	// underlying closure function we create is added to xtop.
	if Curfn != nil && func_.Type != nil {
		Curfn = func_
		olddd := decldepth
		decldepth = 1
		typecheckslice(func_.Nbody.Slice(), Etop)
		decldepth = olddd
		Curfn = oldfn
	}

	// Create top-level function
	xtop = append(xtop, makeclosure(func_))
}

// closurename returns name for OCLOSURE n.
// It is not as simple as it ought to be, because we typecheck nested closures
// starting from the innermost one. So when we check the inner closure,
// we don't yet have name for the outer closure. This function uses recursion
// to generate names all the way up if necessary.

var closurename_closgen int

func closurename(n *Node) *Sym {
	if n.Sym != nil {
		return n.Sym
	}
	gen := 0
	outer := ""
	prefix := ""
	if n.Func.Outerfunc == nil {
		// Global closure.
		outer = "glob."

		prefix = "func"
		closurename_closgen++
		gen = closurename_closgen
	} else if n.Func.Outerfunc.Op == ODCLFUNC {
		// The outermost closure inside of a named function.
		outer = n.Func.Outerfunc.Func.Nname.Sym.Name

		prefix = "func"

		// Yes, functions can be named _.
		// Can't use function closgen in such case,
		// because it would lead to name clashes.
		if !isblank(n.Func.Outerfunc.Func.Nname) {
			n.Func.Outerfunc.Func.Closgen++
			gen = n.Func.Outerfunc.Func.Closgen
		} else {
			closurename_closgen++
			gen = closurename_closgen
		}
	} else if n.Func.Outerfunc.Op == OCLOSURE {
		// Nested closure, recurse.
		outer = closurename(n.Func.Outerfunc).Name

		prefix = ""
		n.Func.Outerfunc.Func.Closgen++
		gen = n.Func.Outerfunc.Func.Closgen
	} else {
		Fatalf("closurename called for %v", Nconv(n, FmtShort))
	}
	n.Sym = Lookupf("%s.%s%d", outer, prefix, gen)
	return n.Sym
}

func makeclosure(func_ *Node) *Node {
	// wrap body in external function
	// that begins by reading closure parameters.
	xtype := Nod(OTFUNC, nil, nil)

	xtype.List.Set(func_.List.Slice())
	xtype.Rlist.Set(func_.Rlist.Slice())

	// create the function
	xfunc := Nod(ODCLFUNC, nil, nil)

	xfunc.Func.Nname = newfuncname(closurename(func_))
	xfunc.Func.Nname.Sym.Flags |= SymExported // disable export
	xfunc.Func.Nname.Name.Param.Ntype = xtype
	xfunc.Func.Nname.Name.Defn = xfunc
	declare(xfunc.Func.Nname, PFUNC)
	xfunc.Func.Nname.Name.Funcdepth = func_.Func.Depth
	xfunc.Func.Depth = func_.Func.Depth
	xfunc.Func.Endlineno = func_.Func.Endlineno
	makefuncsym(xfunc.Func.Nname.Sym)

	xfunc.Nbody.Set(func_.Nbody.Slice())
	xfunc.Func.Dcl = append(func_.Func.Dcl, xfunc.Func.Dcl...)
	func_.Func.Dcl = nil
	if xfunc.Nbody.Len() == 0 {
		Fatalf("empty body - won't generate any code")
	}
	xfunc = typecheck(xfunc, Etop)

	xfunc.Func.Closure = func_
	func_.Func.Closure = xfunc

	func_.Nbody.Set(nil)
	func_.List.Set(nil)
	func_.Rlist.Set(nil)

	return xfunc
}

// hasemptycvars returns true iff closure func_ has an
// empty list of captured vars. OXXX nodes don't count.
func hasemptycvars(func_ *Node) bool {
	for _, v := range func_.Func.Cvars.Slice() {
		if v.Op == OXXX {
			continue
		}
		return false
	}
	return true
}

func walkclosure(func_ *Node, init *Nodes) *Node {
	// If no closure vars, don't bother wrapping.
	if hasemptycvars(func_) {
		if Debug_closure > 0 {
			Warnl(func_.Lineno, "closure converted to global")
		}
		return func_.Func.Closure.Func.Nname
	} else {
		//jea closuredebugruntimecheck(func_)
	}

	// Create closure in the form of a composite literal.
	// supposing the closure captures an int i and a string s
	// and has one float64 argument and no results,
	// the generated code looks like:
	//
	//	clos = &struct{.F uintptr; i *int; s *string}{func.1, &i, &s}
	//
	// The use of the struct provides type information to the garbage
	// collector so that it can walk the closure. We could use (in this case)
	// [3]unsafe.Pointer instead, but that would leave the gc in the dark.
	// The information appears in the binary in the form of type descriptors;
	// the struct is unnamed so that closures in multiple packages with the
	// same struct type can share the descriptor.

	typ := Nod(OTSTRUCT, nil, nil)

	typ.List.Set1(Nod(ODCLFIELD, newname(Lookup(".F")), typenod(Types[TUINTPTR])))
	var typ1 *Node
	for _, v := range func_.Func.Cvars.Slice() {
		if v.Op == OXXX {
			continue
		}
		typ1 = typenod(v.Type)
		if !v.Name.Byval {
			typ1 = Nod(OIND, typ1, nil)
		}
		typ.List.Append(Nod(ODCLFIELD, newname(v.Sym), typ1))
	}

	clos := Nod(OCOMPLIT, nil, Nod(OIND, typ, nil))
	clos.Esc = func_.Esc
	clos.Right.Implicit = true
	clos.List.Set(append([]*Node{Nod(OCFUNC, func_.Func.Closure.Func.Nname, nil)}, func_.Func.Enter.Slice()...))

	// Force type conversion from *struct to the func type.
	clos = Nod(OCONVNOP, clos, nil)

	clos.Type = func_.Type

	clos = typecheck(clos, Erv)

	// typecheck will insert a PTRLIT node under CONVNOP,
	// tag it with escape analysis result.
	clos.Left.Esc = func_.Esc

	/*jea
	// non-escaping temp to use, if any.
	// orderexpr did not compute the type; fill it in now.
	if x := prealloc[func_]; x != nil {
		x.Type = clos.Left.Left.Type
		x.Orig.Type = x.Type
		clos.Left.Right = x
		delete(prealloc, func_)
	}
	*/
	return walkexpr(clos, init)
}

func walkpartialcall(n *Node, init *Nodes) *Node {
	// Create closure in the form of a composite literal.
	// For x.M with receiver (x) type T, the generated code looks like:
	//
	//	clos = &struct{F uintptr; R T}{M.T·f, x}
	//
	// Like walkclosure above.

	if n.Left.Type.IsInterface() {
		// Trigger panic for method on nil interface now.
		// Otherwise it happens in the wrapper and is confusing.
		n.Left = cheapexpr(n.Left, init)

		checknil(n.Left, init)
	}

	typ := Nod(OTSTRUCT, nil, nil)
	typ.List.Set1(Nod(ODCLFIELD, newname(Lookup("F")), typenod(Types[TUINTPTR])))
	typ.List.Append(Nod(ODCLFIELD, newname(Lookup("R")), typenod(n.Left.Type)))

	clos := Nod(OCOMPLIT, nil, Nod(OIND, typ, nil))
	clos.Esc = n.Esc
	clos.Right.Implicit = true
	clos.List.Set1(Nod(OCFUNC, n.Func.Nname, nil))
	clos.List.Append(n.Left)

	// Force type conversion from *struct to the func type.
	clos = Nod(OCONVNOP, clos, nil)

	clos.Type = n.Type

	clos = typecheck(clos, Erv)

	// typecheck will insert a PTRLIT node under CONVNOP,
	// tag it with escape analysis result.
	clos.Left.Esc = n.Esc

	// non-escaping temp to use, if any.
	// orderexpr did not compute the type; fill it in now.
	/*ja if x := prealloc[n]; x != nil {
		x.Type = clos.Left.Left.Type
		x.Orig.Type = x.Type
		clos.Left.Right = x
		delete(prealloc, n)
	}
	*/
	return walkexpr(clos, init)
}
