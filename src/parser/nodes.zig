/// Contains all the node definitions and methods
const std = @import("std");
const token = @import("./token.zig");

const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;
const Token = token.Token;
const Allocator = std.mem.Allocator;

pub const VariableType = union(enum) {
    _null,
    boolean,
    int,
    number,
    bigint,
    char,
    string,
    function,
    array: struct { subTypes: ArrayList(VariableType) },
    object: struct { fields: StringHashMap(VariableType) },
    _union: struct { subTypes: ArrayList(VariableType) },
    _and: struct { subTypes: ArrayList(VariableType) },
};

pub const Environment = StringHashMap(VariableType);

pub const Literal = union(enum) {
    number: f64,
    string: []u8,
    boolean: bool,
    nullLit,
};

pub const TernaryExpr = struct {
    condition: *Expr, // condition expr
    branchTrue: *Expr, // if true, branch expr
    branchFalse: *Expr, // if false, branch expr
};

pub const BinaryExpr = struct {
    leftNode: *Expr, // left node
    operator: Token, // operator
    rightNode: *Expr, // right node
};

pub const GroupingExpr = struct {
    parent: *ScopeContainer,
    expr: *Expr,
};

pub const LiteralExpr = struct {
    parent: *ScopeContainer,
    literal: Literal,
};

pub const UnaryExpr = struct { op: Token, expr: *Expr };

pub const Variable = struct { token: *Token };

pub const AssignExpr = struct {
    left: union(enum) {
        paren: ParenExpr,
        lhs: LhsExpr,
    },
    right: Expr,
};

pub const Expr = union(enum) {
    primary_expr: *PrimaryExpr,
    assignment_expr: *AssignExpr,
    ternary: *TernaryExpr,
    binary: *BinaryExpr,
    grouping: *GroupingExpr,
    literal: *LiteralExpr,
    unary: *UnaryExpr,
    assign: *AssignExpr,
    variable: Token,
};

pub const ScopeContainer = struct {
    env: Environment,
    child: union(enum) { topLevel: *TopLevel, stmt: *Stmt },
};

pub const TopLevel = struct {
    statements: Statements,
    moduleName: []const u8,
};

// ----------- Statements ----------------
pub const Statements = std.ArrayList(Stmt);

pub const Stmt = union(enum) {
    exportStmt: *ExportStmt,
    importStmt: *ImportStmt,
    debuggerStmt: *DebuggerStmt,
    exprStmt: *ExprStmt,
    blockStmt: *BlockStmt,
    ifStmt: *IfStmt,
    switchStmt: *SwitchStmt,
    forStmt: *ForStmt,
    forInStmt: *ForInStmt,
    whileStmt: *WhileStmt,
    doStmt: *DoStmt,
    tryStmt: *TryStmt,
    withStmt: *WithStmt,
    breakStmt: *BreakStmt,
    continueStmt: *ContinueStmt,
    returnStmt: *ReturnStmt,
    throwStmt: *ThrowStmt,
    EmptyStmt,
    labeledStmt: *LabeledStmt,
    declaration: *Declaration,
};

// yo as Hello
const ImportExportSpecifier = struct {
    name: Token, // identifier
    alias: ?Token, // alias (identifier), can be null
};
const ImportExportSpecifiers = ArrayList(ImportExportSpecifier);

const ExportStmt = union(enum) {
    /// export * from 'hello.ts'; 'hello.ts' is the filename
    from_only: Token, // fileName
    /// export { foo, bar } from '';
    export_from: struct {
        export_specifier: ImportExportSpecifiers,
        file: Token,
    },
    /// export { foo, bar }; 
    export_only: struct {
        export_specifier: ImportExportSpecifiers,
    },
    /// export <statement>; || /// export default expr; 
    export_declaration: struct {
        isDefault: bool,
        stmt: ?Stmt, // for non-default export
        expr: ?Expr, // for default exports
    },
};

const ImportStmt = union(enum) {
    /// import default, { export1 } from 'module-name';  
    /// - import_specifier is empty if no such import is present
    /// - default is null if default is not present
    from_only: struct {
        default: ?Token,
        import_specifier: ImportExportSpecifiers,
        file: Token,
    },

    /// import defaultExport, * as name from 'module-name'
    /// - default is null if only `*` alias part is present  
    defaultImportStar: struct { default: ?Token, restImportAlias: Token, file: Token },
};

const DebuggerStmt = Token; // debugger token

const ExprStmt = struct {
    expr: Expr,
};

const BlockStmt = Statements;

const IfStmtsFiltered = union(enum) {
    debuggerStmt: DebuggerStmt,
    exprStmt: *ExprStmt,
    blockStmt: BlockStmt,
    ifStmt: *IfStmt,
    switchStmt: *SwitchStmt,
    forStmt: *ForStmt,
    forInStmt: *ForInStmt,
    whileStmt: *WhileStmt,
    doStmt: *DoStmt,
    tryStmt: *TryStmt,
    withStmt: *WithStmt,
    breakStmt: *BreakStmt,
    continueStmt: *ContinueStmt,
    returnStmt: *ReturnStmt,
    throwStmt: *ThrowStmt,
    EmptyStmt,
    labeledStmt: *LabeledStmt,
};

/// 
const IfStmt = struct {
    condition: ParenExpr,
    consequence: IfStmtsFiltered,
    alternative: ?IfStmtsFiltered,
};

/// Circular Reference Possible: seq_expr = (expr ',' (expr | seq_expr));
pub const SeqExpr = struct {
    left: Expr,
    right: union(enum) {
        seq_expr: *SeqExpr,
        expr: Expr,
    },
};

pub const Expressions = union(enum) {
    expr: Expr,
    seq_expr: SeqExpr,
};

pub const SwitchArm = union(enum) { switch_case: *SwitchCase, switch_default: *SwitchDefault };

pub const SwitchDefault = Statements;

pub const SwitchCase = struct {
    value: *Expressions,
    stmts: *Statements,
};

pub const SwitchBody = ArrayList(SwitchArm);

const SwitchStmt = union(enum) {
    value: *ParenExpr,
    body: *SwitchBody,
};

const ForStmt = struct {
    initalizer: union(enum) {
        lexicalDecl: *LexicalDecl,
        expr_stmt: *ExprStmt,
        EmptyStmt,
    },
    condition: union(enum) {
        expr_stmt: *ExprStmt,
        EmptyStmt,
    },
    increment: ?*Expressions,
    body: ?*IfStmtsFiltered,
};

const ForInStmt = struct {
    is_await: bool,
    for_header: struct {
        variable: union(enum) {
            left: *LhsExpr,
            header: *struct {
                isConst: bool,
                variable: union(enum) {
                    identifier: Token,
                    destructing_pattern: *DestructuringPattern,
                },
            },
        },
        isIn: bool, // 'of' or 'in'
        right: *Expr,
    },
    body: ?*IfStmtsFiltered,
};

const WhileStmt = struct {
    condition: *ParenExpr,
    body: *IfStmtsFiltered,
};

const DoStmt = struct {
    body: *IfStmtsFiltered,
    condition: *ParenExpr,
};

const TryStmt = struct {
    body: *BlockStmt,
    handler: ?*CatchClause,
    finalizer: ?*FinallyClause,
};

const WithStmt = struct {
    object: *ParenExpr,
    body: *Stmt,
};

const BreakStmt = ?Token; // label or null

const ContinueStmt = ?Token; // label or null

const ReturnStmt = ?*Expr; // void return or with Expr

const ThrowStmt = Expressions; // an expression

const LabeledStmt = struct {
    label: Token,
    statement: *Stmt,
};

const Declaration = union(enum) {
    func_decl: *FuncDecl,
    func_signature: *FuncSignature,
    generator_func_decl: *GeneratorFuncDecl,
    class_decl: *ClassDecl,
    abstract_class_decl: *AbstractClassDecl,
    type_alias_Decl: *TypeAliasDecl,
    enum_decl: *EnumDecl,
    interface_decl: *InterfaceDecl,
    lexical_decl: *LexicalDecl,
    variable_decl: *VarDecl,
};

const Func = struct {
    is_async: bool,
    call_signature: *CallSignature,
    body: *BlockStmt,
};

const FuncDecl = struct {
    is_async: bool,
    name: Token,
    call_signature: *CallSignature,
    body: BlockStmt,
};

const FuncSignature = struct {
    is_async: bool,
    name: Token,
    call_signature: *CallSignature,
};

const GeneratorFunc = Func;

const GeneratorFuncDecl = FuncDecl;

const ClassHeritage = union(enum) {
    /// comma separated list of classes/abstractclass
    extends: ?ArrayList(ExtendsClause),
    /// comma separated list of interfaces (Tokens)
    implements: ?ArrayList(Type),
};

const ClassDecl = struct {
    name: Token,
    type_params: ?ArrayList(TypeParam),
    /// heritage will give you inheritance and polymorphism
    class_heritage: ?ClassHeritage,
    body: *ClassBody,
};

const Class = struct {
    name: ?TypeIdentifier,
    type_params: ?ArrayList(TypeParam),
    class_heritage: ?ClassHeritage,
    body: *ClassBody,
};

const ExtendsClause = union(enum) {
    expr: *Expr,
    type_identifier: Token, // identifier
    nested_type_identifier: NestedTypeIdentifier,
    generic_type: *GenericType,
};

const ImplementsClause = Type;

const ClassField = union(enum) {
    method_def: *MethodDef,
    abstract_method_sign: *AbstractMethodSign,
    index_sign: *IndexSign,
    method_sign: *MethodSign,
    public_field_def: *PublicFieldDef,
};
const ClassBody = ArrayList(ClassField);

const AccessibilityMod = enum {
    Public,
    Private,
    Protected,
};

/// 'get', 'set' and '*' before methods
const GetSetStar = enum {
    Get,
    Set,
    Star,
};

const PropName = union(enum) {
    /// reserved identifiers as well
    identifier: Token,
    string: Token,
    number: Token,
    computed_prop_name: *Expr,
};

const TypePredicate = struct {
    /// 'this' token or 'identifier' token
    identifier: Token,
    _type: Type,
};

const Asserts = union(enum) {
    type_predicate: TypePredicate,
    identifier: Token,
    this: Token,
};

const Constraint = struct {
    extends: bool, // 'extends' | ':'
    _type: Type,
};

const TypeParam = struct {
    type_identifier: Token, // (identifier)
    constraint: ?Constraint, // (("extends" | ":") _type)
    default_type: ?Type, // ('=' _type)
};

const CallSignature = struct {
    type_params: ArrayList(TypeParam),
    params: ArrayList(FormalParam),
    return_type: union(enum) {
        type_annotation: *TypeAnnotation,
        asserts: Asserts,
        type_predicate_annotation: TypePredicate, // ':', type_predicate
    },
};

const ParamName = struct {
    accessibility_mod: ?AccessibilityMod,
    readonly: bool,
    patternOrThis: union(enum) {
        pattern: *Pattern,
        this: Token,
    },
};

/// ('=' expr)
const Initializer = Expr;

const RequiredParam = struct {
    param_name: ParamName,
    type_annotation: ?TypeAnnotation,
    initializer: ?Initializer,
};

const OptionalParam = struct {
    param_name: ParamName,
    type_annotation: ?TypeAnnotation,
    initializer: ?Initializer,
};

const FormalParam = union(enum) {
    required_param: RequiredParam,
    optional_param: OptionalParam,
};

const MethodDef = struct {
    accessibility_mod: ?AccessibilityMod,
    static: bool, // static present?
    readonly: bool, // readonly present,
    _async: bool, // async present
    get_set_star: GetSetStar,
    name: PropName,
    optional: bool, // '?' present
    call_sign: CallSignature,
    body: BlockStmt,
};

const LexicalDecl = struct {
    /// 'const' or 'let'
    isConst: bool,
    variable_decls: ArrayList(VarDeclarator),
};

const ObjectPattern = union(enum) {
    pair_pattern: PairPattern,
    rest_pattern: RestPattern,
    obj_assign_pattern: ObjAssignPattern,
    shorthand_prop_identifier_pattern: Token, // identifier or reserved identifier
};

const PairPattern = struct {
    key: PropName,
    value: *Pattern,
};

const ObjAssignPattern = struct {
    left: union(enum) {
        shorthand_prop_identifier_pattern: Token, // identifier or reserved identifier
        destructuring_pattern: DestructuringPattern,
    },
    right: *Expr,
};

/// rest_pattern = '...' (identifier |  destructing_pattern)
const RestPattern = union(enum) {
    identifier: Token,
    destructing_pattern: DestructuringPattern,
};

/// pattern = identifier | destructing_pattern | rest_pattern
const Pattern = union(enum) {
    identifier: Token, // also involves reserved identifier
    destructuring_pattern: DestructuringPattern,
    rest_pattern: RestPattern,
};

/// assing_pattern = (left: pattern) | (right: expr)
const AssignPattern = struct {
    left: Pattern,
    right: *Expr,
};

const ArrayPattern = union(enum) {
    pattern: *Pattern,
    assignPattern: AssignPattern,
};

const DestructuringPattern = union(enum) {
    obj_pattern: ArrayList(ObjectPattern), // { yo: yo }
    arr_pattern: ArrayList(ArrayPattern), // [ hello ]
};

const ArrowFunc = struct {
    is_async: bool,
    call_signature: union(enum) {
        identifier: Token,
        call_signature: *CallSignature,
    },
    body: union(enum) {
        block: *BlockStmt,
        expr: *Expr,
    },
};

const LhsExpr = union(enum) {
    member_expr: MemberExpr,
    subscript_expr: SubscriptExpr,
    identifier: Token,
    destructing_pattern: DestructuringPattern,
};

const ParenExpr = union(enum) {
    single_expr: struct {
        expr: Expr,
        type_annotation: TypeAnnotation,
    },
    seq_expr: SeqExpr,
};

const PrimaryExpr = union(enum) {
    subscript_expr: *SubscriptExpr,
    member_expr: *MemberExpr,
    paren_expr: *ParenExpr,
    identifier: Token,
    reserved_identifier: Token,
    this: Token,
    super: Token,
    number: Token,
    string: Token,
    template_string: Token,
    regex: Token,
    _true: Token,
    _false: Token,
    _null: Token,
    _undefined: Token,
    import: Token, // 'import'
    object: ArrayList(ObjectField),
    array: ArrayList(ArrayField),
    func: *Func,
    arrow_func: *ArrowFunc,
    generator_func: *GeneratorFunc,
    class: *Class,
    /// meta_prop = ('new' '.' 'target')
    meta_prop,
    call_expr: *CallExpr,

    /// expr! - officially not allowed in ekscript
    non_null_expression: *Expr,
};

const Pair = struct {
    key: PropName,
    value: *Expr,
};

const ObjectField = union(enum) {
    pair: Pair,
    /// ("..." expr)
    spread_element: *Expr,
    method_def: *MethodDef,
    /// (identifier | reserved_identifier)
    shorthand_prop_identifier: Token,
};

const ArrayField = union(enum) {
    expr: Expr, // (expr)
    spread_element: SpreadElement, // ("..." expr)
};

const MemberExpr = struct {
    object: union(enum) {
        expr: *Expr,
        primary_expr: *PrimaryExpr,
    },
    /// ('.' | '?.') - '?.' makes nullableAccess true
    nullCheckAccess: bool,
    property: Token, // identifier
};

/// SubscriptExpr = Expr | PrimaryExpr ("?.")? "[" Expr "]"
const SubscriptExpr = struct {
    object: union(enum) {
        expr: Expr,
        primary_expr: PrimaryExpr,
    },
    nullableAccess: bool, // ('?.')
    index: Expressions,
};

const CallExpr = union(enum) {
    call: struct {
        function: *Expr,
        arguments: union(enum) {
            args: Arguments,
            /// hello`this is a template string being called`
            template_string: Token,
        },
    },
    /// hello?.hello
    member: struct {
        function: PrimaryExpr,
        arguments: Arguments,
    },
};

/// SpreadElement = ('...' Expr)
const SpreadElement = Expr;

/// Argument = (Expr | SpreadElement)
const Argument = union(enum) {
    expr: Expr,
    spread_element: SpreadElement,
};

/// new_expr = ('new' Expr "(" "arguments" ")" ) ;
const NewExpr = struct {
    constructor: Expr,
    arguments: ?Arguments,
};

/// await_expr = "await" Expr
const AwaitExpr = Expr;

/// Type 
const Type = union(enum) {
    primary_type: *PrimaryType,
    union_type: *UnionType,
    intersection_type: *IntersectionType,
    function_type: *FunctionType,
    constructor_type: *ConstructorType,
    infer_type: *InferType,
};

const PrimaryType = union(enum) {
    paren_type: ParenType,
    predefined_type: PredefinedType,
    type_identifier: TypeIdentifier,
    nested_type_identifier: *NestedTypeIdentifier,
    generic_type: *GenericType,
    object_type: *ObjectType,
    array_type: *ArrayType,
    tuple_type: *TupleType,
    flow_maybe_type: *FlowMaybeType,
    type_query: *TypeQuery,
    index_type_query: *IndexTypeQuery,
    this: Token, // 'this'
    existential_type: Token, // '*'
    literal_type: *LiteralType,
    lookup_type: *LookupType,
    condition_type: *ConditionalType,
};

/// ParenType = '(' Type ')'
const ParenType = Type;

/// PredefinedType = any | number | boolean | string | symbol | void
const PredefinedType = enum {
    any,
    number,
    boolean,
    string,
    symbol,
    _void,
};

/// TypeIdentifier = identifier
const TypeIdentifier = Token;

/// NestedIdentifier = (identifier | nested_identifier) '.' identifier
const NestedIdentifier = struct {
    left: union(enum) {
        identifier: Token,
        nested_identifier: *NestedIdentifier,
    },
    right: Token, // identifier
};

/// NestedTypeIdentifier = (identifier | NestedIdentifier) "." TypeIdentifier  
const NestedTypeIdentifier = struct {
    module: union(enum) {
        identifier: Token,
        nested_identifier: NestedIdentifier,
    },
    name: TypeIdentifier,
};

/// GenericType = (type_identifier | nested_type_identifier) "<" ( Type ",")* ">"
const GenericType = struct {
    identifier: union(enum) {
        type_identifier: Token,
        nested_type_identifier: NestedTypeIdentifier,
    },
    type_arguments: ArrayList(Type),
};

/// TypeAnnotation = (":" Type)
pub const TypeAnnotation = Type;

///  ObjectTypeField = (ExportStmt | PropSign | CallSign | ConstructSign | IndexSign | MethodSign)
const ObjectTypeField = union(enum) {
    export_stmt: ExportStmt,
    prop_sign: PropSign,
    call_sign: CallSign,
    construct_sign: ConstructSign,
    index_sign: IndexSign,
    method_sign: MethodSign,
};

/// ObjectType = ("{", "{|") (ObjectTypeField ",")*  ("}", "|}")
pub const ObjectType = struct {
    bracesOnly: bool, // ('{'..'}') is true. ('{|'..'|}') is false.
    object_type_field: ArrayList(ObjectTypeField),
};

/// PropSign = AccessibilityMod 'static'? 'readonly'? PropName '?'? TypeAnnotation 
const PropSign = struct {
    accessibility_mod: ?AccessibilityMod,
    is_static: bool, // 'static'
    is_readonly: bool, // 'readonly'
    name: PropName, // 'name'
    is_optional: bool, // '?'
    _type: TypeAnnotation,
};

/// CallSign = TypeParams? FormalParams? (TypeAnnotation | Asserts | TypePredicateAnnotation)? 
const CallSign = struct {
    type_params: ?TypeParams,
    params: FormalParams,
    return_type: ?union(enum) {
        type_annotation: TypeAnnotation,
        asserts: Asserts,
        type_predicate_annotation: TypePredicateAnnotation,
    },
};

/// ConstructSign = "new" TypeParams? FormalParams TypeAnnotation? 
const ConstructSign = struct {
    type_params: ?TypeParams,
    formal_params: ?FormalParams,
    type_annotation: ?TypeAnnotation,
};

/// IndexSign = ("-"? "readonly")?
///    '['
///     ( ( (Identifier | ReservedIdentifier)? ':' Type) | MappedTypeClause)
///     ']' (TypeAnnotation | OmittingTypeAnnotation | OptingTypeAnnotation)
const IndexSign = struct {
    readonly: ?struct {
        sign: bool, // '-' is present?
    },
    prop: union(enum) {
        prop_field: struct {
            identifier: Token, // identifier | reserved_identifier
            _type: Type,
        },
        mapped_type_clause: MappedTypeClause,
    },
    _type: union(enum) {
        type_annotation: TypeAnnotation,
        omitting_type_annotation: OmittingTypeAnnotation,
        opting_type_annotation: OptingTypeAnnotation,
    },
};

/// OmittingTypeAnnotation = '-?:' Type
const OmittingTypeAnnotation = Type;

/// OptingTypeAnnotation = '?:' Type 
const OptingTypeAnnotation = Type;

/// MethodSign = AccessibilityMod? 'static'? 'readonly'? 'async'?  
const MethodSign = struct {
    accessibility_mod: ?AccessibilityMod,
    is_static: bool,
    is_readonly: bool,
    is_async: bool,
    get_set_star: ?GetSetStar,
    name: PropName,
    is_question: bool,
    call_sign: CallSign,
};

/// ArrayType = PrimaryType '[' ']'
const ArrayType = PrimaryType;

/// TupleType = '[' (TupleTypeMember)* ']'
const TupleType = ArrayList(TupleTypeMember);

/// TupleTypeMember = (TupleParam | OptionalTupleParam | OptionalType | RestType | Type)
const TupleTypeMember = union(enum) {
    required_param: TupleParam,
    optional_param: OptionalTupleParam,
    optional_type: OptionalType,
    rest_type: RestType,
    _type: Type,
};

/// TupleParam = (identifier | rest_pattern) TypeAnnotation
const TupleParam = struct {
    name: union(enum) {
        identifier: Token,
        rest_pattern: RestPattern,
    },
    type_annotation: TypeAnnotation,
};

/// OptionalTupleParam = (identifier) '?' (type_annotation)
const OptionalTupleParam = struct {
    identifier: Token,
    type_annotation: TypeAnnotation,
};

/// OptionalType = (Type) '?' 
const OptionalType = Type;

/// RestType = '...' Type
const RestType = Type;

/// FlowMaybeType = '?' PrimaryType 
const FlowMaybeType = PrimaryType;

/// TypeQuery = 'typeof' (PrimaryExpr | GenericType)
const TypeQuery = union(enum) {
    primary_expr: PrimaryExpr,
    generic_type: GenericType,
};

/// IndexTypeQuery = 'keyof' (generic_type | type_identifier | nested_type_identifier | type_query)
const IndexTypeQuery = union(enum) {
    generic_type: GenericType,
    type_identifier: TypeIdentifier,
    nested_type_identifier: NestedTypeIdentifier,
    type_query: TypeQuery,
};

/// LiteralType = UnaryExpr | Number | String | True | False 
const LiteralType = union(enum) {
    number: Token,
    int: Token,
    float: Token,
    string: Token,
    _true: Token,
    _false: Token,
};

/// LookupType = PrimaryType '[' Type ']'
const LookupType = struct {
    caller: PrimaryType,
    indexer: Type,
};

/// ConditionalType = Type 'extends' Type '?' Type ':' Type
const ConditionalType = struct {
    left: Type,
    right: Type,
    consequence: Type,
    alternative: Type,
};

/// UnionType = Type? '|' Type; 
///     - precendence follows left
const UnionType = struct {
    left: ?Type,
    right: Type,
};

/// IntersectionType = Type? '&' Type;
///     - precendence follows left
const IntersectionType = struct {
    left: ?Type,
    right: ?Type,
};

/// FunctionType = TypeParams? FormalParams '=>' (Type | TypePredicate)
const FunctionType = struct {
    type_params: ?TypeParams,
    formal_params: FormalParams,
    return_type: union(enum) {
        _type: Type,
        type_predicate: TypePredicate,
    },
};

/// ConstructorType = 'new' TypeParams? FormalParams '=>' Type
const ConstructorType = struct {
    type_params: ?TypeParams,
    formal_params: FormalParams,
    _type: ?Type,
};

/// InferType = 'infer' TypeIdentifier
const InferType = struct { type_identifier: TypeIdentifier };

/// TypeParams = '<' (TypeParam ',')* ','? '>'
const TypeParams = struct {
    params: ArrayList(TypeParam),
};

/// FormalParams = '(' (FormalParam ',')* ')'
const FormalParams = struct {
    params: ArrayList(FormalParam),
};

/// TypePredicateAnnotation = ':' TypePredicate
const TypePredicateAnnotation = struct {
    predicate: TypePredicate,
};

/// MappedTypeClause = TypeIdentifier 'in' Type
const MappedTypeClause = struct {
    type_identifier: TypeIdentifier,
    _type: Type,
};

/// AbstractMethodSign = AccessibilityMod 'abstract' GetSetStar PropName '?'? CallSign
const AbstractMethodSign = struct {
    accessibility_mod: AccessibilityMod,
    get_set_star: ?GetSetStar,
    name: PropName,
    is_optional: bool,
    call_sign: CallSign,
};

/// PublicFieldDef = Decorator 'static'? PropName Initializer?  
const PublicFieldDef = struct {
    // decorator: ArrayList(Decorator),
    is_static: bool,
    property: PropName,
    Initializer: Initializer,
};

/// Arguments = '(' (Argument)* ')'
const Arguments = struct {
    args: ArrayList(Argument),
};

/// VarDeclarator = ( ((Identifier | DestructuringPattern) TypeAnnotation? Initializer ) | (Identifier '!' TypeAnnotation) )  
const VarDeclarator = union(enum) {
    normal_decl: struct {
        name: union(enum) {
            identifier: Identifier,
            destructing_pattern: DestructuringPattern,
        },
        type: TypeAnnotation,
        Initializer: ?Initializer,
    },
    null_decl: struct {
        name: Identifier,
        type: TypeAnnotation,
    },
};

/// Identifier = Token
const Identifier = Token;

/// ReservedIdentifier
const ReservedIdentifier = Token;

/// CatchClause = ( 'catch' '(' (Identifier | DestructuringPattern)  ')' BlockStmt )
const CatchClause = struct {
    parameter: union(enum) {
        identifier: Identifier,
        destructing_pattern: DestructuringPattern,
    },
    body: BlockStmt,
};

/// FinallyClause = 'finally' BlockStmt
const FinallyClause = struct {
    finally: BlockStmt,
};

/// AbstractClassDecl = decorator* 'abstract' 'class' TypeIdentifier TypeParams? ClassHeritage? ClassBody  
const AbstractClassDecl = struct {
    // decorator: Decorator,
    name: TypeIdentifier,
    type_params: TypeParams,
    class_heritage: ?ClassHeritage,
    class_body: ClassBody,
};

/// TypeAliasDecl = 'type' TypeIdentifier TypeParams '=' Type ';'
const TypeAliasDecl = struct {
    name: TypeIdentifier,
    type_params: TypeParams,
    value: Type,
};

/// EnumDecl = 'const'? 'enum' Identifier EnumBody
const EnumDecl = struct {
    is_const: bool,
    name: Identifier,
    body: EnumBody,
};

/// EnumBody = '{' ((PropName | EnumAssign) ',')* '}'
const EnumBody = struct {
    prop_name: PropName,
    enum_assign: EnumAssign,
};

/// EnumAssign = PropName Initializer
const EnumAssign = struct {
    prop_name: PropName,
    initializer: Initializer,
};


/// InterfaceDecl = 'interface' TypeIdentifier TypeParams? ExtendsClause? ObjectType
const InterfaceDecl = struct {
    name: TypeIdentifier,
    type_params: ?TypeParams,
    extends: ?ExtendsClause,    
    body: ObjectType,
};

/// VarDecl = 'var' (VarDeclarator, ',') ';'
const VarDecl = struct {
    sub_decls: ArrayList(VarDeclarator)
};
