unit Triton.SymbolicExpression;

{$Z4}

interface
    uses System.SysUtils, Winapi.Windows,
         Triton.Register,
         Triton.MemoryAccess,
         Triton.AstNode,
         Triton.Define;

type

  symbolicExp = record
    private
      FHS            : HandleSharedSymbolicExpression;
      Ftipo          : expression_e;
      FAst           : AbstractNode;
      FnewAst        : AbstractNode;
      FCommento      : AnsiString;
      FFormatCommento: AnsiString;
      FFormatExpr    : AnsiString;
      FFormatid      : AnsiString;
      Fid            : usize;
      ForiginMemory  : MemAccess;
      ForiginRegister: Registro;
      FIsMemory      : Boolean;
      FIsRegiter     : Boolean;
      FisSymbolized  : Boolean;
      function getAst: AbstractNode;
      function getCommento: AnsiString;
      function getfmtCommento: AnsiString;
      function getfmtExpr: AnsiString;
      function getfmtId: AnsiString;
      function getIsMemory: Boolean;
      function getIsRegiter: Boolean;
      function getIsSymbolized: Boolean;
      function getNewAst: AbstractNode;
      function getoriginMem: MemAccess;
      function getoriginReg: Registro;
      function getType: expression_e;
      procedure setAst(const Value: AbstractNode);
      procedure setCommento(const Value: AnsiString);
      procedure setoriginMem(const Value: MemAccess);
      procedure setoriginReg(const Value: Registro);
      procedure setType(const Value: expression_e);
      function getId: usize;
      function Update: Boolean;
    public
      isTainnted : Boolean;
      procedure Create(node: AbstractNode; id: usize; tipo: expression_e; comment: PAnsiChar = nil); overload;
      procedure Create(other: symbolicExp);overload;
      procedure Free;
      function ToStr:string;

      class operator Explicit(hSym: HandleSharedSymbolicExpression): symbolicExp;
      class operator Explicit(rSym: symbolicExp): HandleSharedSymbolicExpression;
      class operator Implicit(hSym: HandleSharedSymbolicExpression): symbolicExp;
      class operator Implicit(rSym: symbolicExp): HandleSharedSymbolicExpression;

    property tipo        : expression_e       read Ftipo            write setType;
    property Ast         : AbstractNode       read FAst             write setAst;
    property newAst      : AbstractNode       read FnewAst;
    property Commento    : AnsiString         read FCommento        write setCommento ;
    property FmtCommento : AnsiString         read FFormatCommento ;
    property FmtExpr     : AnsiString         read FFormatExpr ;
    property id          : usize              read Fid;
    property FmtId       : AnsiString         read FFormatid ;
    property originMemory: MemAccess          read ForiginMemory    write setoriginMem;
    property originReg   : Registro           read ForiginRegister  write setoriginReg;
    property IsMemory    : Boolean            read FIsMemory ;
    property IsRegiter   : Boolean            read FIsRegiter ;
    property IsSymbolized: Boolean            read FisSymbolized ;
  end;

  (*  SymbolicExpression ============================================================================== *)

        //! Constructor.
        function SECreateSymbolicExpression(node:HandleAbstractNode; id: usize; TExceptionPointers:expression_e; comment : PAnsiChar = nil):HandleSharedSymbolicExpression; cdecl;  external Triton_dll Name 'SECreateSymbolicExpression';
        //! Constructor by copy.
        function SECreateSymbolicExpressionFrom(other: HandleSharedSymbolicExpression):HandleSharedSymbolicExpression; cdecl;  external Triton_dll Name 'SECreateSymbolicExpressionFrom';
        //! Destructor.
        procedure SEDelete(Handle: HandleSharedSymbolicExpression);  cdecl;  external Triton_dll Name 'SEDelete';
        //! Returns the symbolic expression id.
        function SEgetId(Handle: HandleSharedSymbolicExpression ): usize ;cdecl;  external Triton_dll Name 'SEgetId';
        //! Returns true if the symbolic expression is assigned to a memory.
        function SEisMemory(Handle: HandleSharedSymbolicExpression ): Boolean; cdecl;  external Triton_dll Name 'SEisMemory';
        //! Returns true if the symbolic expression is assigned to a register.
        function SEisRegister(Handle: HandleSharedSymbolicExpression): Boolean ;cdecl;  external Triton_dll Name 'SEisRegister';
        //! Returns true if the expression contains a symbolic variable.
        function SEisSymbolized(Handle:HandleSharedSymbolicExpression): Boolean ; cdecl;  external Triton_dll Name 'SEisSymbolized';
        //! Returns the type of the symbolic expression assignment.
        function SEgetType(Handle: HandleSharedSymbolicExpression):expression_e ; cdecl;  external Triton_dll Name 'SEgetType';
        //! Returns the SMT AST root node of the symbolic expression. This is the semantics.
        function SEgetAst(Handle: HandleSharedSymbolicExpression): HandleAbstractNode ;  cdecl;  external Triton_dll Name 'SEgetAst';
        //! Returns a new SMT AST root node of the symbolic expression. This new instance is a duplicate of the original node and may be changed without changing the original semantics.
        function SEgetNewAst(Handle: HandleSharedSymbolicExpression): HandleAbstractNode ; cdecl;  external Triton_dll Name 'SEgetNewAst';
        //! Returns the comment of the symbolic expression.
        procedure SEgetComment(Handle: HandleSharedSymbolicExpression; var comment: PAnsiChar) ;cdecl;  external Triton_dll Name 'SEgetComment';
        //! Returns the id as string of the symbolic expression according the mode of the AST representation.
        procedure SEgetFormattedId(Handle: HandleSharedSymbolicExpression; var frmId: PAnsiChar) ; cdecl;  external Triton_dll Name 'SEgetFormattedId';
        //! Returns the comment as string of the symbolic expression according the mode of the AST representation.
        procedure SEgetFormattedComment(Handle: HandleSharedSymbolicExpression; var frmcomment: PAnsiChar) ;cdecl;  external Triton_dll Name 'SEgetFormattedComment';
        //! Returns the symbolic expression representation as string according the mode of the AST representation.
        procedure SEgetFormattedExpression(Handle: HandleSharedSymbolicExpression; var frmExp: PAnsiChar) ; cdecl;  external Triton_dll Name 'SEgetFormattedExpression';
        //! Returns the origin memory access if `kind` is equal to `triton::engines::symbolic::MEM`, invalid memory otherwise.
        function SEgetOriginMemory(Handle:HandleSharedSymbolicExpression): HandleMemAcc ; cdecl;  external Triton_dll Name 'SEgetOriginMemory';
        //! Returns the origin register if `kind` is equal to `triton::engines::symbolic::REG`, `REG_INVALID` otherwise.
        function SEgetOriginRegister(Handle:HandleSharedSymbolicExpression): HandleReg ;cdecl;  external Triton_dll Name 'SEgetOriginRegister';
        //! Sets a root node.
        procedure SEsetAst(Handle:HandleSharedSymbolicExpression; node: HandleAbstractNode); cdecl;  external Triton_dll Name 'SEsetAst';
        //! Sets a comment to the symbolic expression.
        procedure SEsetComment(Handle:HandleSharedSymbolicExpression; comment: PAnsiChar); cdecl;  external Triton_dll Name 'SEsetComment';
        //! Sets the kind of the symbolic expression.
        procedure SEsetType(Handle:HandleSharedSymbolicExpression; tipo: expression_e);  cdecl;  external Triton_dll Name 'SEsetType';
        //! Sets the origin memory acccess.
        procedure SEsetOriginMemory(Handle:HandleSharedSymbolicExpression;mem: HandleMemAcc); cdecl;  external Triton_dll Name 'SEsetOriginMemory';
        //! Sets the origin register.
        procedure SEsetOriginRegister(Handle:HandleSharedSymbolicExpression;reg: HandleReg);  cdecl;  external Triton_dll Name 'SEsetOriginRegister';



implementation

{ symbolicExp }

procedure symbolicExp.Create(other: symbolicExp);
begin
    ZeroMemory(@self,SizeOf(symbolicExp));
    FHS := SECreateSymbolicExpressionFrom(HandleSharedSymbolicExpression(other));

    Update
end;

procedure symbolicExp.Create(node: AbstractNode; id: usize; tipo: expression_e;  comment: PAnsiChar);
begin
    ZeroMemory(@self,SizeOf(symbolicExp));
    FHS := SECreateSymbolicExpression(HandleAbstractNode(node),id,tipo,comment);

    Update
end;

procedure symbolicExp.Free;
begin
    SEDelete(FHS);
    ZeroMemory(@self,SizeOf(symbolicExp));
end;

function symbolicExp.Update:Boolean;
begin
    Result := True;
    try
      getAst;
      getCommento;
      getfmtCommento;
      getfmtExpr;
      getfmtId;
      getIsMemory;
      getIsRegiter;
      getIsSymbolized;
      getNewAst;
      getoriginMem;
      getoriginReg;
      getType;
      getId;
    except
      Result := False;
    end;
end;

class operator symbolicExp.Implicit(hSym: HandleSharedSymbolicExpression): symbolicExp;
begin
    ZeroMemory(@Result,SizeOf(symbolicExp));
    Result.FHS  := hSym;

    if not Result.Update then
       ZeroMemory(@Result,SizeOf(symbolicExp));
end;

class operator symbolicExp.Explicit(hSym: HandleSharedSymbolicExpression): symbolicExp;
begin
    ZeroMemory(@Result,SizeOf(symbolicExp));
    Result.FHS  := hSym;

    if not Result.Update then
       ZeroMemory(@Result,SizeOf(symbolicExp));
end;

class operator symbolicExp.Explicit(rSym: symbolicExp): HandleSharedSymbolicExpression;
begin
    Result := rSym.FHS;
end;

class operator symbolicExp.Implicit(rSym: symbolicExp): HandleSharedSymbolicExpression;
begin
    Result := rSym.FHS;
end;

function symbolicExp.getAst: AbstractNode;
begin
    Result := AbstractNode( SEgetAst(FHS) );

    FAst   := Result;
end;

procedure symbolicExp.setAst(const Value: AbstractNode);
begin
    SEsetAst(FHS, HandleAbstractNode(Value));

    FAst := Value;
end;

function symbolicExp.getCommento: AnsiString;
var
 pRes : PAnsiChar;
begin
    SEgetComment(FHS,pRes);
    Result := AnsiString(pRes);

    FCommento := Result;
end;

procedure symbolicExp.setCommento(const Value: AnsiString);
begin
    SEsetComment(FHS,PAnsiChar(Value));

    FCommento := Value;
end;

function symbolicExp.getType: expression_e;
begin
    Result := SEgetType(FHS);

    Ftipo := Result;
end;

procedure symbolicExp.setType(const Value: expression_e);
begin
    SEsetType(FHS,Value);

    Ftipo := Value;
end;

function symbolicExp.getoriginReg: Registro;
begin
    Result := Registro(SEgetOriginRegister(FHS)) ;

    ForiginRegister := Result;
end;

procedure symbolicExp.setoriginReg(const Value: Registro);
begin
    SEsetOriginRegister(FHS, HandleReg(Value));

    ForiginRegister := Value;
end;

function symbolicExp.getoriginMem: MemAccess;
begin
    Result := MemAccess(SEgetOriginMemory(FHS)) ;

    ForiginMemory := Result;
end;

procedure symbolicExp.setoriginMem(const Value: MemAccess);
begin
    SEsetOriginMemory(FHS, HandleMemAcc(Value));

    ForiginMemory := Value;
end;

function symbolicExp.getfmtCommento: AnsiString;
var
 pRes : PAnsiChar;
begin
    SEgetFormattedComment(FHS, pRes);
    Result := AnsiString(pRes);

    FFormatCommento := Result;
end;

function symbolicExp.getfmtExpr: AnsiString;
var
 pRes : PAnsiChar;
begin
   SEgetFormattedExpression(FHS, pRes);
   Result := AnsiString(pRes)  ;

   FFormatExpr := Result;
end;

function symbolicExp.getfmtId: AnsiString;
var
 pRes : PAnsiChar;
begin
    SEgetFormattedId(FHS, pRes);
    Result := AnsiString(pRes);

    FFormatid := Result;
end;

function symbolicExp.getId: usize;
begin
    Result := SEgetId(FHS) ;

    Fid := Result;
end;

function symbolicExp.getIsMemory: Boolean;
begin
    Result := SEisMemory(FHS);

    FIsMemory := Result;
end;

function symbolicExp.getIsRegiter: Boolean;
begin
    Result := SEisRegister(FHS);

    FIsRegiter := Result;
end;

function symbolicExp.getIsSymbolized: Boolean;
begin
    Result := SEisSymbolized(FHS) ;

    FisSymbolized := Result;
end;

function symbolicExp.getNewAst: AbstractNode;
begin
    Result := AbstractNode( SEgetNewAst(FHS) );

    FnewAst := Result;
end;

function symbolicExp.ToStr: string;
begin
   Result :=string( getfmtExpr );
end;

end.
