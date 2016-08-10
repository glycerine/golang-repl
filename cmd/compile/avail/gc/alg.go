// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

// AlgKind describes the kind of algorithms used for comparing and
// hashing a Type.
type AlgKind int

const (
	// These values are known by runtime.
	ANOEQ AlgKind = iota
	AMEM0
	AMEM8
	AMEM16
	AMEM32
	AMEM64
	AMEM128
	ASTRING
	AINTER
	ANILINTER
	AFLOAT32
	AFLOAT64
	ACPLX64
	ACPLX128

	// Type can be compared/hashed as regular memory.
	AMEM AlgKind = 100

	// Type needs special comparison/hashing functions.
	ASPECIAL AlgKind = -1
)

// IsComparable reports whether t is a comparable type.
func (t *Type) IsComparable() bool {
	a, _ := algtype1(t)
	return a != ANOEQ
}

// IsRegularMemory reports whether t can be compared/hashed as regular memory.
func (t *Type) IsRegularMemory() bool {
	a, _ := algtype1(t)
	return a == AMEM
}

// IncomparableField returns an incomparable Field of struct Type t, if any.
func (t *Type) IncomparableField() *Field {
	for _, f := range t.FieldSlice() {
		if !f.Type.IsComparable() {
			return f
		}
	}
	return nil
}

// algtype is like algtype1, except it returns the fixed-width AMEMxx variants
// instead of the general AMEM kind when possible.
func algtype(t *Type) AlgKind {
	a, _ := algtype1(t)
	if a == AMEM {
		switch t.Width {
		case 0:
			return AMEM0
		case 1:
			return AMEM8
		case 2:
			return AMEM16
		case 4:
			return AMEM32
		case 8:
			return AMEM64
		case 16:
			return AMEM128
		}
	}

	return a
}

// algtype1 returns the AlgKind used for comparing and hashing Type t.
// If it returns ANOEQ, it also returns the component type of t that
// makes it incomparable.
func algtype1(t *Type) (AlgKind, *Type) {
	if t.Broke {
		return AMEM, nil
	}
	if t.Noalg {
		return ANOEQ, t
	}

	switch t.Etype {
	case TANY, TFORW:
		// will be defined later.
		return ANOEQ, t

	case TINT8, TUINT8, TINT16, TUINT16,
		TINT32, TUINT32, TINT64, TUINT64,
		TINT, TUINT, TUINTPTR,
		TBOOL, TPTR32, TPTR64,
		TCHAN, TUNSAFEPTR:
		return AMEM, nil

	case TFUNC, TMAP:
		return ANOEQ, t

	case TFLOAT32:
		return AFLOAT32, nil

	case TFLOAT64:
		return AFLOAT64, nil

	case TCOMPLEX64:
		return ACPLX64, nil

	case TCOMPLEX128:
		return ACPLX128, nil

	case TSTRING:
		return ASTRING, nil

	case TINTER:
		if t.IsEmptyInterface() {
			return ANILINTER, nil
		}
		return AINTER, nil

	case TSLICE:
		return ANOEQ, t

	case TARRAY:
		a, bad := algtype1(t.Elem())
		switch a {
		case AMEM:
			return AMEM, nil
		case ANOEQ:
			return ANOEQ, bad
		}

		switch t.NumElem() {
		case 0:
			// We checked above that the element type is comparable.
			return AMEM, nil
		case 1:
			// Single-element array is same as its lone element.
			return a, nil
		}

		return ASPECIAL, nil

	case TSTRUCT:
		fields := t.FieldSlice()

		// One-field struct is same as that one field alone.
		if len(fields) == 1 && !isblanksym(fields[0].Sym) {
			return algtype1(fields[0].Type)
		}

		ret := AMEM
		for i, f := range fields {
			// All fields must be comparable.
			a, bad := algtype1(f.Type)
			if a == ANOEQ {
				return ANOEQ, bad
			}

			// Blank fields, padded fields, fields with non-memory
			// equality need special compare.
			if a != AMEM || isblanksym(f.Sym) || ispaddedfield(t, i) {
				ret = ASPECIAL
			}
		}

		return ret, nil
	}

	Fatalf("algtype1: unexpected type %v", t)
	return 0, nil
}

// ispaddedfield reports whether the i'th field of struct type t is followed
// by padding.
func ispaddedfield(t *Type, i int) bool {
	if !t.IsStruct() {
		Fatalf("ispaddedfield called non-struct %v", t)
	}
	end := t.Width
	if i+1 < t.NumFields() {
		end = t.Field(i + 1).Offset
	}
	return t.Field(i).End() != end
}
