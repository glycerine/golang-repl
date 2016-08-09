// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

// Escape constants are numbered in order of increasing "escapiness"
// to help make inferences be monotonic. With the exception of
// EscNever which is sticky, eX < eY means that eY is more exposed
// than eX, and hence replaces it in a conservative analysis.
const (
	EscUnknown = iota
	EscNone    // Does not escape to heap, result, or parameters.
	EscReturn  // Is returned or reachable from returned.
	EscScope   // Allocated in an inner loop scope, assigned to an outer loop scope,
	// which allows the construction of non-escaping but arbitrarily large linked
	// data structures (i.e., not eligible for allocation in a fixed-size stack frame).
	EscHeap           // Reachable from the heap
	EscNever          // By construction will not escape.
	EscBits           = 3
	EscMask           = (1 << EscBits) - 1
	EscContentEscapes = 1 << EscBits // value obtained by indirect of parameter escapes to heap
	EscReturnBits     = EscBits + 1
	// Node.esc encoding = | escapeReturnEncoding:(width-4) | contentEscapes:1 | escEnum:3
)
