// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package gc

//	case OADD:
//		if(n->right->op == OLITERAL) {
//			v = n->right->vconst;
//			naddr(n->left, a, canemitcode);
//		} else
//		if(n->left->op == OLITERAL) {
//			v = n->left->vconst;
//			naddr(n->right, a, canemitcode);
//		} else
//			goto bad;
//		a->offset += v;
//		break;

// a function named init is a special case.
// it is called by the initialization before
// main is run. to make it unique within a
// package and also uncallable, the name,
// normally "pkg.init", is altered to "pkg.init.1".

var renameinit_initgen int

func renameinit() *Sym {
	renameinit_initgen++
	return LookupN("init.", renameinit_initgen)
}

// hand-craft the following initialization code
//      var initdone· uint8                             (1)
//      func init() {                                   (2)
//              if initdone· > 1 {                      (3)
//                      return                          (3a)
//              }
//              if initdone· == 1 {                     (4)
//                      throw()                         (4a)
//              }
//              initdone· = 1                           (5)
//              // over all matching imported symbols
//                      <pkg>.init()                    (6)
//              { <init stmts> }                        (7)
//              init.<n>() // if any                    (8)
//              initdone· = 2                           (9)
//              return                                  (10)
//      }
