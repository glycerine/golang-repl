package gc

import (
	"bufio"
	"bytes"
	cv "github.com/glycerine/goconvey/convey"
	"github.com/shurcooL/go-goon"
	"testing"
)

func Test001BasicParsing(t *testing.T) {
	cv.Convey("elementary variable declaration 'var a = ' followed by '10' on the next line should parse\n", t, func() {

		replInit()
		str := "package main; var abc = 10"
		s := bytes.NewBuffer([]byte(str))
		bin := bufio.NewReader(s)

		var indent []byte = nil
		p := newparser(bin, indent)

		p.package_()
		p.want(';')

		var nodes []*Node = nil
		nodes = p.xdcl_list()

		//P("nodes[0].Left.Sym.Name = %#v\n", nodes[0].Left.Sym.Name)
		if false {
			goon.Dump(nodes[0].Left.Sym.Name)
		}
		cv.So(nodes[0].Left.Sym.Name, cv.ShouldEqual, "abc")
	})
}
