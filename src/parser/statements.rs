use ast::{ModuleName, Statement, Import, Type, Variable, Block, Function, FunctionSignature,
    Argument, ExternalFunction, While, If, ElsePart, Struct, Union, UnionCase, Match, MatchCase,
    Return, Trait, GenericArgument};
use compileerror::{CompileResult, Pos, Span, ErrorCode, err};
use parser::{TokenQueue, TokenKind, Token, Operator, ParseMode, parse_expression};

fn parse_module_name(tq: &mut TokenQueue) -> CompileResult<ModuleName>
{
    let mut start_pos = Pos::zero();
    let mut parts = Vec::new();
    while tq.is_next_identifier()
    {
        let (m, span) = try!(tq.expect_identifier());
        if start_pos == Pos::zero() {
            start_pos = span.start;
        }

        parts.push(m);
        if tq.is_next(TokenKind::DoubleColon) {
            try!(tq.pop());
        } else {
            break;
        }
    }

    Ok(ModuleName::new(parts, Span::new(start_pos, tq.pos())))
}

fn parse_import(tq: &mut TokenQueue, pos: Pos) -> CompileResult<Statement>
{
    let mut modules = Vec::new();
    modules.push(try!(parse_module_name(tq)));
    while tq.is_next(TokenKind::Comma) {
        try!(tq.pop());
        if !tq.is_next_identifier() {break}

        modules.push(try!(parse_module_name(tq)));
    }

    Ok(Statement::Import(Import::new(modules, Span::new(pos, tq.pos()))))
}

fn is_primitive_type(name: &str) -> bool
{
    match name
    {
        "uint8" | "int8" | "char" | "byte" |
        "uint16" | "int16" | "uint32" | "int32" |
        "int" | "uint" | "uint64"| "bool" |
        "float" | "double" => true,
        _ => false,
    }
}


fn parse_type<MakeComplex>(tq: &mut TokenQueue, make_complex: MakeComplex) -> CompileResult<Type>
    where MakeComplex: Fn(String) -> Type
{
    if tq.is_next(TokenKind::Operator(Operator::Mul))
    {
        try!(tq.pop());
        let st = try!(parse_type(tq, make_complex));
        Ok(Type::Pointer(Box::new(st)))
    }
    else if tq.is_next(TokenKind::OpenBracket)
    {
        try!(tq.pop());
        let at = try!(parse_type(tq, make_complex));

        if tq.is_next(TokenKind::Comma)
        {
            try!(tq.pop());
            let (count, _) = try!(tq.expect_int());
            try!(tq.expect(TokenKind::CloseBracket));
            Ok(Type::Array(Box::new(at), count as usize))
        }
        else
        {
            try!(tq.expect(TokenKind::CloseBracket));
            Ok(Type::Slice(Box::new(at)))
        }
    }
    else
    {
        let (name, _pos) = try!(tq.expect_identifier());
        if is_primitive_type(&name) {
            Ok(Type::Primitive(name))
        } else {
            Ok(make_complex(name))
        }
    }

}

fn parse_optional_type(tq: &mut TokenQueue) -> CompileResult<Type>
{
    if tq.is_next(TokenKind::Colon)
    {
        // variable with type declaration
        try!(tq.pop());
        Ok(try!(parse_type(tq, |n| Type::Complex(n))))
    }
    else
    {
        Ok(Type::Unknown)
    }
}

fn parse_vars(tq: &mut TokenQueue, indent_level: usize, constants: bool, public: bool) -> CompileResult<Vec<Variable>>
{
    let mut vars = Vec::new();
    loop
    {
        if let Some(level) = tq.next_indent() {
            if level <= indent_level {
                break;
            }
        }

        let tok = try!(tq.pop());
        match tok.kind
        {
            TokenKind::Identifier(id) => {
                let type_of_var = try!(parse_optional_type(tq));
                try!(tq.expect(TokenKind::Operator(Operator::Assign)));
                let expr = try!(parse_expression(tq, indent_level));
                vars.push(
                    Variable::new(
                        id,
                        type_of_var,
                        constants,
                        public,
                        expr,
                        Span::new(tok.span.start, tq.pos()),
                ));
            },
            TokenKind::Comma | TokenKind::Indent(_) => continue,
            TokenKind::EOF => break,
            _ => {
                return err(tok.span.start, ErrorCode::UnexpectedToken, format!("Unexpected token {}", tok));
            }
        }
    }

    Ok(vars)
}

pub fn parse_block(tq: &mut TokenQueue, indent_level: usize, mode: ParseMode) -> CompileResult<Block>
{
    let mut statements = Vec::new();
    if tq.next_indent().is_none() {
        statements.push(try!(parse_statement(tq, indent_level, mode)));
    }

    loop {
        match tq.next_indent()
        {
            Some(lvl) if lvl > indent_level => {
                try!(tq.expect_indent());

                if tq.is_next(TokenKind::EOF) {
                    break;
                }

                statements.push(try!(parse_statement(tq, lvl, mode)));
            },
            _ => break,
        }
    }

    Ok(Block::new(statements))
}

fn parse_generic_argument_list(tq: &mut TokenQueue) -> CompileResult<Vec<GenericArgument>>
{
    let mut generic_args = Vec::new();
    try!(tq.expect(TokenKind::Operator(Operator::LessThan)));
    while !tq.is_next(TokenKind::Operator(Operator::GreaterThan))
    {
        let (name, _) = try!(tq.expect_identifier());
        try!(tq.expect(TokenKind::Colon));
        let constraint = try!(parse_type(tq, |t| Type::Trait(t)));
        try!(eat_comma(tq));
        generic_args.push(GenericArgument::new(name, constraint));
    }

    try!(tq.expect(TokenKind::Operator(Operator::GreaterThan)));
    Ok(generic_args)
}

fn parse_func_signature(tq: &mut TokenQueue, self_type: Type, pos: Pos) -> CompileResult<FunctionSignature>
{
    let (name, _) = try!(tq.expect_identifier());
    let mut args = Vec::new();

    let generic_args = if tq.is_next(TokenKind::Operator(Operator::LessThan)) {
        try!(parse_generic_argument_list(tq))
    } else {
        Vec::new()
    };

    try!(tq.expect(TokenKind::OpenParen));
    while !tq.is_next(TokenKind::CloseParen)
    {
        let const_arg = if tq.is_next(TokenKind::Var) {
            try!(tq.pop());
            false
        } else if tq.is_next(TokenKind::Const) {
            try!(tq.pop());
            true
        } else {
            true // const by default
        };

        let (arg_name, arg_span) = try!(tq.expect_identifier());

        if arg_name == "self" {
            if args.is_empty() {
                args.push(Argument::new(arg_name, Type::ptr(self_type.clone()), const_arg, arg_span));
            } else {
                return err(arg_span.start, ErrorCode::SelfNotAllowed, format!("self must be the first argument of a member function"));
            }
        } else {
            try!(tq.expect(TokenKind::Colon));
            let typ = try!(parse_type(tq, |n| Type::Complex(n)));
            args.push(Argument::new(arg_name, typ, const_arg, Span::new(arg_span.start, tq.pos())));
        }

        if !tq.is_next(TokenKind::Comma) {
            break;
        }

        try!(tq.expect(TokenKind::Comma));
    }

    try!(tq.expect(TokenKind::CloseParen));

    let ret_type = if tq.is_next(TokenKind::Operator(Operator::Arrow)) {
        try!(tq.pop());
        try!(parse_type(tq, |n| Type::Complex(n)))
    } else {
        Type::Void
    };

    let func_name = match self_type
    {
        Type::Complex(ref type_name) |
        Type::Trait(ref type_name) => format!("{}::{}", type_name, name),
        _ => name,
    };
    Ok(FunctionSignature{
        name: func_name,
        return_type: ret_type,
        args: args,
        generic_args: generic_args,
        span: Span::new(pos, tq.pos()),
    })
}

fn parse_func(tq: &mut TokenQueue, start_pos: Pos, indent_level: usize, public: bool, self_type: Type) -> CompileResult<Function>
{
    let sig = try!(parse_func_signature(tq, self_type, start_pos));
    try!(tq.expect(TokenKind::Colon));

    let block = try!(parse_block(tq, indent_level, ParseMode::Block));
    Ok(Function::new(
        sig,
        public,
        block,
        Span::new(start_pos, tq.pos())
    ))
}

fn parse_external_func(tq: &mut TokenQueue,  start_pos: Pos) -> CompileResult<ExternalFunction>
{
    try!(tq.expect(TokenKind::Func));
    let sig = try!(parse_func_signature(tq, Type::Void, start_pos));
    Ok(ExternalFunction::new(sig, Span::new(start_pos, tq.pos())))
}

fn parse_while(tq: &mut TokenQueue, indent_level: usize, pos: Pos) -> CompileResult<Statement>
{
    let cond = try!(parse_expression(tq, indent_level));
    try!(tq.expect(TokenKind::Colon));
    let block = try!(parse_block(tq, indent_level, ParseMode::Block));
    Ok(Statement::While(While::new(cond, block, Span::new(pos, tq.pos()))))
}

fn parse_else(tq: &mut TokenQueue, indent_level: usize) -> CompileResult<ElsePart>
{
    if tq.is_next(TokenKind::If)
    {
        let tok = try!(tq.pop());
        Ok(ElsePart::If(Box::new(try!(parse_if(tq, indent_level, tok.span.start)))))
    }
    else
    {
        try!(tq.expect(TokenKind::Colon));
        Ok(ElsePart::Block(try!(parse_block(tq, indent_level, ParseMode::Block))))
    }
}

fn parse_if(tq: &mut TokenQueue, indent_level: usize, pos: Pos) -> CompileResult<If>
{
    let cond = try!(parse_expression(tq, indent_level));
    try!(tq.expect(TokenKind::Colon));
    let if_block = try!(parse_block(tq, indent_level, ParseMode::Block));
    let mut else_part = ElsePart::Empty;

    if let Some(lvl) = tq.next_indent() {
        if lvl == indent_level && tq.is_next_at(1, TokenKind::Else) {
            try!(tq.pop()); // indent
            try!(tq.pop()); // else
            else_part = try!(parse_else(tq, indent_level));
        }
    }

    Ok(If::new(cond, if_block, else_part, Span::new(pos, tq.pos())))
}


fn parse_return(tq: &mut TokenQueue, indent_level: usize, pos: Pos) -> CompileResult<Statement>
{
    let e = try!(parse_expression(tq, indent_level));
    let span = Span::new(pos, tq.pos());
    Ok(Statement::Return(Return::new(e, span)))
}

fn parse_struct_member(s: &mut Struct, tq: &mut TokenQueue, indent_level: usize, public: bool) -> CompileResult<()>
{
    let tok = try!(tq.pop());
    match tok.kind
    {
        TokenKind::Pub => {
            return parse_struct_member(s, tq, indent_level, true);
        },
        TokenKind::Func => {
            let st = Type::Complex(s.name.clone());
            s.functions.push(try!(parse_func(tq, tok.span.start, indent_level, public, st)));
        },
        TokenKind::Var => {
            let vars = try!(parse_vars(tq, indent_level, false, public));
            s.variables.extend(vars.into_iter());
        },
        TokenKind::Const => {
            let vars = try!(parse_vars(tq, indent_level, true, public));
            s.variables.extend(vars.into_iter());
        },
        TokenKind::EOF => {},
        _ => {
            return err(tok.span.start, ErrorCode::UnexpectedToken, format!("Unexpected token {}", tok));
        },
    }

    Ok(())
}

fn parse_struct(tq: &mut TokenQueue, indent_level: usize, public: bool, pos: Pos) -> CompileResult<Struct>
{
    let (name, _) = try!(tq.expect_identifier());
    let mut s = Struct::new(name, public, Span::zero());
    if tq.is_next(TokenKind::Impl)
    {
        try!(tq.pop());
        while !tq.is_next(TokenKind::Colon)
        {
            let t = try!(parse_type(tq, |n| Type::Trait(n)));
            s.impls.push(t);
            try!(eat_comma(tq));
        }
    }

    try!(tq.expect(TokenKind::Colon));
    try!(parse_indented_block(tq, indent_level, |q, level| {
        parse_struct_member(&mut s, q, level, false)
    }));

    s.span = Span::new(pos, tq.pos());
    Ok(s)
}

fn eat_comma(tq: &mut TokenQueue) -> CompileResult<()>
{
    tq.pop_if(|tok| tok.kind == TokenKind::Comma).map(|_| ())
}

fn parse_union_case(tq: &mut TokenQueue) -> CompileResult<UnionCase>
{
    let (name, span) = try!(tq.expect_identifier());
    let mut uc = UnionCase::new(name, Span::zero());
    if tq.is_next(TokenKind::OpenParen)
    {
        try!(tq.pop());
        while !tq.is_next(TokenKind::CloseParen)
        {
            let (name, span) = try!(tq.expect_identifier());
            try!(tq.expect(TokenKind::Colon));
            let typ = try!(parse_type(tq, |n| Type::Complex(n)));
            uc.vars.push(Argument::new(name, typ, false, Span::new(span.start, tq.pos())));
            try!(eat_comma(tq));
        }

        try!(tq.expect(TokenKind::CloseParen));
    }

    try!(eat_comma(tq)); // Eat trailing comma
    uc.span = Span::new(span.start, tq.pos());
    Ok(uc)
}

fn parse_union_member(tq: &mut TokenQueue, indent_level: usize, public: bool, ut: Type) -> CompileResult<Function>
{
    let tok = try!(tq.pop());
    match tok.kind
    {
        TokenKind::Pub => parse_union_member(tq, indent_level, true, ut),
        TokenKind::Func => parse_func(tq, tok.span.start, indent_level, public, ut),
        _ => err(tok.span.start, ErrorCode::UnexpectedToken, format!("Unexpected token {}", tok)),
    }
}

fn parse_union(tq: &mut TokenQueue, indent_level: usize, public: bool) -> CompileResult<Union>
{
    let (name, name_span) = try!(tq.expect_identifier());
    let mut u = Union::new(name, public, Span::zero());
    try!(tq.expect(TokenKind::Colon));

    // We allow single line unions: union Foo: Bar, Baz, Bam
    while tq.next_indent().is_none() {
        if tq.is_next_identifier() {
            u.cases.push(try!(parse_union_case(tq)));
            try!(eat_comma(tq));
        } else {
            break;
        }
    }

    try!(parse_indented_block(tq, indent_level, |q, level| {
        loop {
            if q.is_next_identifier() {
                u.cases.push(try!(parse_union_case(q)));
            } else if q.is_next(TokenKind::Pub) || q.is_next(TokenKind::Func) {
                u.functions.push(try!(parse_union_member(q, level, false, Type::Complex(u.name.clone()))));
            } else {
                break;
            }
            try!(eat_comma(q));
        }
        Ok(())
    }));

    u.span = Span::new(name_span.start, tq.pos());
    Ok(u)
}

fn parse_match_case(tq: &mut TokenQueue, indent_level: usize) -> CompileResult<MatchCase>
{
    let (name, span) = try!(tq.expect_identifier());
    let mut bindings = Vec::new();
    if tq.is_next(TokenKind::OpenParen)
    {
        try!(tq.pop());
        while !tq.is_next(TokenKind::CloseParen)
        {
            let (name, _) = try!(tq.expect_identifier());
            bindings.push(name);
            try!(eat_comma(tq));
        }

        try!(tq.expect(TokenKind::CloseParen));
    }

    try!(tq.expect(TokenKind::Colon));
    let block = try!(parse_block(tq, indent_level, ParseMode::Block));
    Ok(MatchCase::new(name, bindings, block, Span::new(span.start, tq.pos())))
}

fn parse_match(tq: &mut TokenQueue, indent_level: usize, pos: Pos) -> CompileResult<Statement>
{
    let expr = try!(parse_expression(tq, indent_level));
    let mut m = Match::new(expr, Span::zero());
    try!(tq.expect(TokenKind::Colon));
    try!(parse_indented_block(tq, indent_level, |q, level| {
        m.cases.push(try!(parse_match_case(q, level)));
        Ok(())
    }));

    m.span = Span::new(pos, tq.pos());
    Ok(Statement::Match(m))
}

fn parse_trait(tq: &mut TokenQueue, indent_level: usize, pos: Pos, public: bool) -> CompileResult<Statement>
{
    let (name, _) = try!(tq.expect_identifier());
    try!(tq.expect(TokenKind::Colon));
    let mut funcs = Vec::new();

    try!(parse_indented_block(tq, indent_level, |q, _| {
        let pos = q.pos();
        try!(q.expect(TokenKind::Func));
        let sig = try!(parse_func_signature(q, Type::Trait(name.clone()), pos));
        funcs.push(sig);
        Ok(())
    }));

    Ok(Statement::Trait(Trait::new(name, public, funcs, Span::new(pos, tq.pos()))))
}

fn parse_indented_block<Op>(tq: &mut TokenQueue, indent_level: usize, mut parse_line: Op) -> CompileResult<()>
    where Op: FnMut(&mut TokenQueue, usize) -> CompileResult<()>
{
    while let Some(level) = tq.next_indent()
    {
        if level <= indent_level {break}
        try!(tq.pop()); // indent

        if tq.is_next(TokenKind::EOF) {break;}
        try!(parse_line(tq, level));
    }
    Ok(())
}

pub fn parse_statement(tq: &mut TokenQueue, indent_level: usize, mode: ParseMode) -> CompileResult<Statement>
{
    let tok = try!(tq.pop());
    if mode == ParseMode::Module {
        return parse_module_statement(tq, indent_level, tok);
    }

    match tok.kind
    {
        TokenKind::While => parse_while(tq, indent_level, tok.span.start),
        TokenKind::If => parse_if(tq, indent_level, tok.span.start).map(|i| Statement::If(i)),
        TokenKind::Return => parse_return(tq, indent_level, tok.span.start),
        TokenKind::Match => parse_match(tq, indent_level, tok.span.start),
        TokenKind::Identifier(id) => {
            tq.push_front(Token::new(TokenKind::Identifier(id), tok.span));
            parse_expression(tq, indent_level).map(|e| Statement::Expression(e))
        },
        _ => parse_module_statement(tq, indent_level, tok),
    }
}

pub fn parse_module_statement(tq: &mut TokenQueue, indent_level: usize, tok: Token) -> CompileResult<Statement>
{
    match tok.kind
    {
        TokenKind::Import => parse_import(tq, tok.span.start),
        TokenKind::Var => parse_vars(tq, indent_level, false, false).map(|v| Statement::Variable(v)),
        TokenKind::Const => parse_vars(tq, indent_level, true, false).map(|v| Statement::Variable(v)),
        TokenKind::Func => parse_func(tq, tok.span.start, indent_level, false, Type::Void).map(|f| Statement::Function(f)),
        TokenKind::Struct => parse_struct(tq, indent_level, false, tok.span.start).map(|s| Statement::Struct(s)),
        TokenKind::Union => parse_union(tq, indent_level, false).map(|u| Statement::Union(u)),
        TokenKind::Extern => parse_external_func(tq, tok.span.start).map(|f| Statement::ExternalFunction(f)),
        TokenKind::Trait => parse_trait(tq, indent_level, tok.span.start, false),
        TokenKind::Pub => {
            let next = try!(tq.pop());
            match next.kind
            {
                TokenKind::Var => parse_vars(tq, indent_level, false, true).map(|v| Statement::Variable(v)),
                TokenKind::Const => parse_vars(tq, indent_level, true, true).map(|v| Statement::Variable(v)),
                TokenKind::Func => parse_func(tq, next.span.start, indent_level, true, Type::Void).map(|f| Statement::Function(f)),
                TokenKind::Struct => parse_struct(tq, indent_level, true, next.span.start).map(|s| Statement::Struct(s)),
                TokenKind::Union => parse_union(tq, indent_level, true).map(|u| Statement::Union(u)),
                TokenKind::Trait => parse_trait(tq, indent_level, tok.span.start, true),
                _ => err(tok.span.start, ErrorCode::UnexpectedToken, format!("Unexpected token {}", next)),
            }
        },
        _ => err(tok.span.start, ErrorCode::UnexpectedToken, format!("Unexpected token {}", tok)),
    }
}
