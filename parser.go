package repl

import (
	"errors"
	"fmt"
	"io"
	"sync"
)

// ParseExpression(), ParseList(), ParseBlockComment(), and ParseArray()
// are examples of parsing (possibly mutually recursive) productions.
// ParseExpression is the top-most.

//
// Parser turns  input rune streams and into Abstract Syntax Trees
// of Node structs. The four channels ReqReset, AddInput, ParsedOutput,
// and LastErr provide new input (AddInput); or start over with a new line
// of input after aborting as with Ctrl-C (ReqReset). The Parser
// provides output via the ParsedOutput channel, and the last error
// encountered during parsing is available from the LastErr channel.
//
// Parser.Start() and Parser.Stop() are used to setup and
// shutdown a parser.
//
type Parser struct {
	lexer *Lexer
	env   *Env

	Done         chan bool
	reqStop      chan bool
	AddInput     chan io.RuneScanner
	ReqReset     chan io.RuneScanner
	ParsedOutput chan []ParserReply
	LastErr      chan error

	mut               sync.Mutex
	stopped           bool
	lastError         error
	sendMe            []ParserReply
	FlagSendNeedInput bool
}

// the output Abstract Syntax Tree
type Node struct{}

// NodeEnd is a special sentinel to indicate that there are no more Nodes.
var NodeEnd *Node = &Node{}

// the environment in which the parsing takes place
type Env struct{}

// ParserReply holds the output of the parser. The
// return channel ParsedOutput uses this to structure
// its output.
type ParserReply struct {
	Expr []*Node
	Err  error
}

// errors:

// ErrMoreInputeNeeded means that the parser is in the middle
// of a parse and cannot finish without more input.
var ErrMoreInputNeeded = fmt.Errorf("parser needs more input")

// ErrShuttingDown is returned to indicate that Parser.Stop()
// has been invoked and the Parser is in the middle of shutting down.
var ErrShuttingDown error = fmt.Errorf("lexer shutting down")

// ParserHaltRequested means the Parser is in the middle of
// shutting down afteer Parser.Stop() has been requested.
var ParserHaltRequested = fmt.Errorf("parser halt requested")

// ResetRequested means that parsing was interrupted by a
// client request to reset: stop parsing the old input, and
// begin again on some new input.
var ResetRequested = fmt.Errorf("parser reset requested")

// UnexpectedEnd means that the input was exhausted (EOF reached)
// during the middle of a parse.
var UnexpectedEnd error = errors.New("Unexpected end of input")

// NewParser creates a new Parser, but does not invoke Parser.Start().
func (env *Env) NewParser() *Parser {
	p := &Parser{
		env:          env,
		Done:         make(chan bool),
		reqStop:      make(chan bool),
		ReqReset:     make(chan io.RuneScanner),
		AddInput:     make(chan io.RuneScanner),
		ParsedOutput: make(chan []ParserReply),
		LastErr:      make(chan error),
		sendMe:       make([]ParserReply, 0, 1),
	}
	p.lexer = NewLexer(p)
	return p
}

// ParseTokens is the main service the Parser provides.
// Currently returns first error encountered, ignoring
// any expressions after that.
func (p *Parser) ParseTokens() ([]*Node, error) {
	select {
	case out := <-p.ParsedOutput:
		Q("ParseTokens got p.ParsedOutput out: '%#v'", out)
		r := make([]*Node, 0)
		for _, k := range out {
			r = append(r, k.Expr...)
			//Q("\n ParseTokens k.Expr = '%v'\n\n", (&SexpArray{Val: k.Expr, Env: p.env}).SexpString(nil))
			if k.Err != nil {
				return r, k.Err
			}
		}
		return r, nil
	case <-p.reqStop:
		return nil, ErrShuttingDown
	}
}

// Start begins the parsing on a separate background go-routine, by
// calling p.InfiniteParsingLoop().
func (p *Parser) Start() {
	go p.InfiniteParsingLoop()
}

// Stop shuts down a parser. This stops the worker go-routine,
// and p.Done will be closed after Stop returns. Currently
// Stop always returns nil.
func (p *Parser) Stop() error {
	p.mut.Lock()
	defer p.mut.Unlock()
	if p.stopped {
		return nil
	}
	p.stopped = true
	close(p.reqStop)
	<-p.Done
	return nil
}

// InfiniteParsingLoop is the background worker go-routine
// that calls Parser.GetMoreInput() and Parser.ParseExpression()
// in an infinite loop.
func (p *Parser) InfiniteParsingLoop() {
	defer close(p.Done)
	expressions := make([]*Node, 0, SliceDefaultCap)

	// maybe we already have input, be optimistic!
	// no need to call p.GetMoreInput() before staring
	// our loop.

	for {
		expr, err := p.ParseExpression(0)
		if err != nil || expr == NodeEnd {
			if err == ParserHaltRequested {
				return
			}
			err = p.GetMoreInput(expressions, err)
			if err == ParserHaltRequested {
				return
			}
			// GetMoreInput will have delivered what we gave them. Reset since we
			// don't own that memory any more.
			expressions = make([]*Node, 0, SliceDefaultCap)
		} else {
			// INVAR: err == nil && expr is not NodeEnd
			expressions = append(expressions, expr)
		}
	}
}

// This function should *return* when it has more input
// for the parser/lexer, which will call it when they get wedged.
//
// Listeners on p.ParsedOutput should know the Convention: sending
// a length 0 []ParserReply on p.ParsedOutput channel means: we need more
// input! They should send some in on p.AddInput channel; or request
// a reset and simultaneously give us new input with p.ReqReset channel.
func (p *Parser) GetMoreInput(deliverThese []*Node, errorToReport error) error {

	if len(deliverThese) == 0 && errorToReport == nil {
		p.FlagSendNeedInput = true
	} else {
		p.sendMe = append(p.sendMe,
			ParserReply{
				Expr: deliverThese,
				Err:  errorToReport,
			})
	}

	for {
		select {
		case <-p.reqStop:
			return ParserHaltRequested
		case input := <-p.AddInput:
			p.lexer.AddNextStream(input)
			p.FlagSendNeedInput = false
			return nil
		case input := <-p.ReqReset:
			p.lexer.Reset()
			p.lexer.AddNextStream(input)
			p.FlagSendNeedInput = false
			return ResetRequested
		case p.HaveStuffToSend() <- p.sendMe:
			p.sendMe = make([]ParserReply, 0, 1)
			p.FlagSendNeedInput = false
		case p.LastErr <- p.lastError:
			p.lastError = nil
		}
	}
}

// HaveStuffToSend returns a nil chan if the Parser has
// no buffered output waiting. If there is output waiting,
// or if the p.FlagSendNeedInput is set, then the p.ParsedOutput
// channel is returned.
func (p *Parser) HaveStuffToSend() chan []ParserReply {
	if len(p.sendMe) > 0 || p.FlagSendNeedInput {
		return p.ParsedOutput
	}
	return nil
}

// Reset() resets the parser but provides no new input.
// Typically the Parser.ResetAddNewInput() method would
// be used instead.
func (p *Parser) Reset() {
	select {
	case p.ReqReset <- nil:
	case <-p.reqStop:
	}
}

// NewInput queues up an additional input source, s,
// to be used after the previous io.RuneScanners have
// been exhausted.
func (p *Parser) NewInput(s io.RuneScanner) {
	select {
	case p.AddInput <- s:
	case <-p.reqStop:
	}
}

// ResetAddNewInput aborts the existing parse and provides
// a new source of runes to begin anew on.
func (p *Parser) ResetAddNewInput(s io.RuneScanner) {
	select {
	case p.ReqReset <- s:
	case <-p.reqStop:
	}
}

// SliceDefaultCap controls how large the internal expression buffers
// are made to begin with.
const SliceDefaultCap = 10

/*
func (parser *Parser) ParseList(depth int) (sx Node, err error) {
	lexer := parser.lexer
	var tok Token

tokFilled:
	for {
		tok, err = lexer.PeekNextToken()
		//Q("\n ParseList(depth=%d) got lexer.PeekNextToken() -> tok='%v' err='%v'\n", depth, tok, err)
		if err != nil {
			return NodeNull, err
		}
		if tok.typ != TokenEnd {
			break tokFilled
		}
		// instead of returning UnexpectedEnd, we:
		err = parser.GetMoreInput(nil, ErrMoreInputNeeded)
		//Q("\n ParseList(depth=%d) got back from parser.GetMoreInput(): '%v'\n", depth, err)
		switch err {
		case ParserHaltRequested:
			return NodeNull, err
		case ResetRequested:
			return NodeEnd, err
		}
		// have to still fill tok, so
		// loop to the top to PeekNextToken
	}

		if tok.typ == TokenRParen {
			_, _ = lexer.GetNextToken()
			return NodeNull, nil
		}

		var start = &SexpPair{}

		expr, err := parser.ParseExpression(depth + 1)
		if err != nil {
			return SexpNull, err
		}

		start.Head = expr

		tok, err = lexer.PeekNextToken()
		if err != nil {
			return SexpNull, err
		}

		// backslash '\' replaces dot '.' in zygomys
		if tok.typ == TokenBackslash {
			// eat up the backslash
			_, _ = lexer.GetNextToken()
			expr, err = parser.ParseExpression(depth + 1)
			if err != nil {
				return SexpNull, err
			}

			// eat up the end paren
			tok, err = lexer.GetNextToken()
			if err != nil {
				return SexpNull, err
			}
			// make sure it was actually an end paren
			if tok.typ != TokenRParen {
				return SexpNull, errors.New("extra value in dotted pair")
			}
			start.Tail = expr
			return start, nil
		}

		expr, err = parser.ParseList(depth + 1)
		if err != nil {
			return start, err
		}
		start.Tail = expr

	return start, nil
}
*/

/*
func (parser *Parser) ParseArray(depth int) (Sexp, error) {
	lexer := parser.lexer
	arr := make([]Sexp, 0, SliceDefaultCap)

	var tok Token
	var err error
	for {
	getTok:
		for {
			tok, err = lexer.PeekNextToken()
			if err != nil {
				return SexpEnd, err
			}

			if tok.typ == TokenComma {
				// pop off the ,
				_, _ = lexer.GetNextToken()
				continue getTok
			}

			if tok.typ != TokenEnd {
				break getTok
			} else {
				//instead of return SexpEnd, UnexpectedEnd
				// we ask for more, and then loop
				err = parser.GetMoreInput(nil, ErrMoreInputNeeded)
				switch err {
				case ParserHaltRequested:
					return SexpNull, err
				case ResetRequested:
					return SexpEnd, err
				}
			}
		}

		if tok.typ == TokenRSquare {
			// pop off the ]
			_, _ = lexer.GetNextToken()
			break
		}

		expr, err := parser.ParseExpression(depth + 1)
		if err != nil {
			return SexpNull, err
		}
		arr = append(arr, expr)
	}

	return &SexpArray{Val: arr, Env: parser.env}, nil
}
*/

// ParseExpression is the top-level entry point for parsing of one expression.
// It is called by the background-worker go-routine that is running
// Parser.InfiniteParsingLoop() to begin a top-level expression parse.
// It may be called recursively, either by itself or by one of its
// descendants on the call stack.
func (parser *Parser) ParseExpression(depth int) (res *Node, err error) {
	/*
			defer func() {
				if res != nil {
					//Q("returning from ParseExpression at depth=%v with res='%s'\n", depth, res.SexpString())
				} else {
					//Q("returning from ParseExpression at depth=%v, res = nil", depth)
				}
			}()

			lexer := parser.lexer
			env := parser.env

			//getAnother:
			tok, err := lexer.GetNextToken()
			if err != nil {
				return SexpEnd, err
			}

			switch tok.typ {
			case TokenLParen:
				exp, err := parser.ParseList(depth + 1)
				return exp, err
			case TokenLSquare:
				exp, err := parser.ParseArray(depth + 1)
				return exp, err
			case TokenLCurly:
				exp, err := parser.ParseInfix(depth + 1)
				return exp, err
			case TokenQuote:
				expr, err := parser.ParseExpression(depth + 1)
				if err != nil {
					return SexpNull, err
				}
				return MakeList([]Sexp{env.MakeSymbol("quote"), expr}), nil
			case TokenCaret:
				// '^' is now our syntax-quote symbol, not TokenBacktick, to allow go-style `string literals`.
				expr, err := parser.ParseExpression(depth + 1)
				if err != nil {
					return SexpNull, err
				}
				return MakeList([]Sexp{env.MakeSymbol("syntaxQuote"), expr}), nil
			case TokenTilde:
				expr, err := parser.ParseExpression(depth + 1)
				if err != nil {
					return SexpNull, err
				}
				return MakeList([]Sexp{env.MakeSymbol("unquote"), expr}), nil
			case TokenTildeAt:
				expr, err := parser.ParseExpression(depth + 1)
				if err != nil {
					return SexpNull, err
				}
				return MakeList([]Sexp{env.MakeSymbol("unquote-splicing"), expr}), nil
			case TokenFreshAssign:
				return env.MakeSymbol(tok.str), nil
			case TokenColonOperator:
				return env.MakeSymbol(tok.str), nil
			case TokenDollar:
				return env.MakeSymbol(tok.str), nil
			case TokenBool:
				return &SexpBool{Val: tok.str == "true"}, nil
			case TokenDecimal:
				i, err := strconv.ParseInt(tok.str, 10, SexpIntSize)
				if err != nil {
					return SexpNull, err
				}
				return &SexpInt{Val: i}, nil
			case TokenHex:
				i, err := strconv.ParseInt(tok.str, 16, SexpIntSize)
				if err != nil {
					return SexpNull, err
				}
				return &SexpInt{Val: i}, nil
			case TokenOct:
				i, err := strconv.ParseInt(tok.str, 8, SexpIntSize)
				if err != nil {
					return SexpNull, err
				}
				return &SexpInt{Val: i}, nil
			case TokenBinary:
				i, err := strconv.ParseInt(tok.str, 2, SexpIntSize)
				if err != nil {
					return SexpNull, err
				}
				return &SexpInt{Val: i}, nil
			case TokenChar:
				return &SexpChar{Val: rune(tok.str[0])}, nil
			case TokenString:
				return &SexpStr{S: tok.str}, nil
			case TokenBacktickString:
				return &SexpStr{S: tok.str, backtick: true}, nil
			case TokenFloat:
				f, err := strconv.ParseFloat(tok.str, SexpFloatSize)
				if err != nil {
					return SexpNull, err
				}
				return &SexpFloat{Val: f}, nil
			case TokenEnd:
				return SexpEnd, nil
			case TokenSymbol:
				return env.MakeSymbol(tok.str), nil
			case TokenSymbolColon:
				sym := env.MakeSymbol(tok.str)
				sym.colonTail = true
				return sym, nil
			case TokenDot:
				sym := env.MakeSymbol(tok.str)
				sym.isDot = true
				return sym, nil
			case TokenDotSymbol:
				sym := env.MakeSymbol(tok.str)
				sym.isDot = true
				return sym, nil
			case TokenComment:
				//Q("parser making SexpComment from '%s'", tok.str)
				return &SexpComment{Comment: tok.str}, nil
				// parser skips comments
				//goto getAnother
			case TokenBeginBlockComment:
				// parser skips comments
				return parser.ParseBlockComment(&tok)
				//parser.ParseBlockComment(&tok)
				//goto getAnother
			case TokenComma:
				return &SexpComma{}, nil
			case TokenSemicolon:
				return &SexpSemicolon{}, nil
			}
		return nil, fmt.Errorf("Invalid syntax, don't know what to do with %v '%v'", tok.typ, tok)
	*/
	return nil, fmt.Errorf("Invalid syntax, don't know what to do with token I received")
}

/*
func (parser *Parser) ParseBlockComment(start *Token) (sx Sexp, err error) {
	defer func() {
		if sx != nil {
			//Q("returning from ParseBlockComment with sx ='%v', err='%v'",
			//	sx.SexpString(), err)
		}
	}()
	lexer := parser.lexer
	var tok Token
	var block = &SexpComment{Block: true, Comment: start.str}

	for {
	tokFilled:
		for {
			tok, err = lexer.PeekNextToken()
			if err != nil {
				return SexpNull, err
			}
			if tok.typ != TokenEnd {
				break tokFilled
			}
			err = parser.GetMoreInput(nil, ErrMoreInputNeeded)
			switch err {
			case ParserHaltRequested:
				return SexpNull, err
			case ResetRequested:
				return SexpEnd, err
			}
			// have to still fill tok, so
			// loop to the top to PeekNextToken
		}

		// consume it

		//cons, err := lexer.GetNextToken()
		_, err := lexer.GetNextToken()
		if err != nil {
			return nil, err
		}
		//Q("parse block comment is consuming '%v'", cons)

		switch tok.typ {
		case TokenEndBlockComment:
			block.Comment += tok.str
			return block, nil
		case TokenComment:
			block.Comment += tok.str
		default:
			panic("internal error: inside a block comment, we should only see TokenComment and TokenEndBlockComment tokens")
		}
	}
	//return block, nil
}
*/
