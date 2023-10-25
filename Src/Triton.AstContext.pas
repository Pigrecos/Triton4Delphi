unit Triton.AstContext;

{$Z4}

interface
    uses System.SysUtils, Winapi.Windows,
         Triton.SymbolicExpression,
         Triton.SymbolicVariable,
         Triton.AstNode,
         Triton.Define;

type
   hModes              = pointer;   //triton::modes::Modes
   hAstRepresentation  = Pointer;   //triton::ast::representations::AstRepresentation

   TModes = record
   private
      FHModes : hModes;
   public
      procedure Create; overload;
      procedure Create(other: TModes); overload;
      procedure Free;

      function  isModeEnabled(mode: mode_e): Boolean;
      procedure enableMode(mode: mode_e; flag: Boolean) ;
      class operator Explicit(hMod: hModes): TModes;
      class operator Explicit(rMod: TModes): hModes;
  end;

  TAstRepresentation = record
   private
      FHAstRap : hAstRepresentation;
   public
      procedure Create; overload;
      procedure Create(other:TAstRepresentation); overload;
      procedure Free;
      function  getMode: uint32 ;
      procedure setMode(mode: uint32);
      class operator Explicit(hAst_R: hAstRepresentation): TAstRepresentation;
      class operator Explicit(rAst_R: TAstRepresentation): hAstRepresentation;
  end;

  AstContext = record
    private
      FAstCtx           : HandleAstContext;

    public
      procedure Create(modes: TModes); overload;
      procedure Create(other: TAstRepresentation); overload;
      procedure Free;
      class operator Explicit(hAst: HandleAstContext): AstContext;
      class operator Implicit(hAst: HandleAstContext): AstContext;
      class operator Explicit(rAst: AstContext): HandleAstContext;

      function  assert_(expr: AbstractNode):AbstractNode;
      function  bv(value: uint64; size: uint32):AbstractNode;
      function  bvadd(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvand(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvashr(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvfalse:AbstractNode;
      function  bvlshr(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvmul(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvnand(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvneg(expr: AbstractNode):AbstractNode;
      function  bvnor(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvnot(expr: AbstractNode):AbstractNode;
      function  bvor(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvrol(expr: AbstractNode ;  rot: uint32):AbstractNode;overload;
      function  bvrol(expr: AbstractNode ;  rot :AbstractNode):AbstractNode;overload;
      function  bvror(expr: AbstractNode ;  rot: uint32):AbstractNode;overload;
      function  bvror(expr: AbstractNode ;  rot: AbstractNode):AbstractNode;overload;
      function  bvsdiv(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvsge(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvsgt(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvshl(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvsle(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvslt(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvsmod(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvsrem(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvsub(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvtrue():AbstractNode;
      function  bvudiv(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvuge(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvugt(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvule(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvult(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvurem(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvxnor(expr1, expr2: AbstractNode ):AbstractNode;
      function  bvxor(expr1, expr2: AbstractNode ):AbstractNode;
      function  concat(expr1, expr2: AbstractNode ):AbstractNode;
      function  declare(variable: AbstractNode):AbstractNode;
      function  distinct(expr1, expr2: AbstractNode ):AbstractNode;
      function  equal(expr1, expr2: AbstractNode ):AbstractNode;
      function  extract(high: uint32; low: uint32;  expr: AbstractNode):AbstractNode;
      function  iff(expr1, expr2: AbstractNode ):AbstractNode;
      function  integer(value: uint64):AbstractNode;
      function  ite(ifExpr, thenExpr, elseExpr: AbstractNode):AbstractNode;
      function  land(expr1, expr2: AbstractNode ):AbstractNode;
      function  let(alias: PAnsiChar; expr2, expr3: AbstractNode):AbstractNode;
      function  lnot(expr: AbstractNode):AbstractNode;
      function  lor(expr1, expr2: AbstractNode ):AbstractNode;
      function  reference(expr: symbolicExp):AbstractNode;
      function  &string(value: PAnsiChar):AbstractNode;
      function  sx(sizeExt: uint32;  expr: AbstractNode):AbstractNode;
      function  variable(symVar: SymbolicVar):AbstractNode;
      function  zx(sizeExt: uint32;  expr: AbstractNode):AbstractNode;
      procedure initVariable(name: PAnsiChar; value: uint64; node: AbstractNode);
      procedure updateVariable(name: PAnsiChar; value: uint64);
      function  getVariableNode(name: PAnsiChar):AbstractNode;
      function  getVariableValue(varName: PAnsiChar):uint64 ;
      procedure setRepresentationMode(mode: uint32);
      function  getRepresentationMode():uint32 ;
      function  Search(Node : AbstractNode; match: ast_e=ANY_NODE): TArray<AbstractNode>;
      function  unroll(node: AbstractNode): AbstractNode;
  end;

  function  Triton_Ast_unroll(node: AbstractNode): AbstractNode;

 (*  Modes ============================================================================== *)

        //! Constructor.
        function MCreateModes:hModes;cdecl;  external Triton_dll Name 'MCreateModes';
        //! Constructor.
        function MCreateModesFrom(hMode: hModes): hModes;cdecl;  external Triton_dll Name 'MCreateModesFrom';
        //! Destructor.
        procedure MDeleteModes(hMode: hModes); cdecl;  external Triton_dll Name 'MDeleteModes';
        //! Returns true if the mode is enabled.
        function MisModeEnabled(hMode: hModes; mode: mode_e): Boolean; cdecl;  external Triton_dll Name 'MisModeEnabled';
        //! Enables or disables a specific mode.
        procedure MenableMode(hMode: hModes; mode: mode_e; flag: Boolean);cdecl;  external Triton_dll Name 'MenableMode';

 (*  AstRapresentation ============================================================================== *)

        //! Constructor.
        function RAPCreate:hAstRepresentation; cdecl;  external Triton_dll Name 'RAPCreate';
        //! Constructor.
        function RAPCreateFrom(other: hAstRepresentation): hAstRepresentation; cdecl;  external Triton_dll Name 'RAPCreateFrom';
        //! Destructor.
        procedure RAPDelete(handle: hAstRepresentation);cdecl;  external Triton_dll Name 'RAPDelete';
        //! Returns the representation mode.
        function RAPgetMode(handle: hAstRepresentation): uint32 ; cdecl;  external Triton_dll Name 'RAPgetMode';
        //! Sets the representation mode.
        procedure RAPsetMode(handle: hAstRepresentation; mode: uint32); cdecl;  external Triton_dll Name 'RAPsetMode';

 (* ASTContext ============================================================================== *)

        //! Constructor
        function CtxCreate(modes: hModes): HandleAstContext;cdecl;  external Triton_dll Name 'CtxCreate';
        //! Constructor by copy
        function CtxCreateFrom(other: HandleAstContext): HandleAstContext; cdecl;  external Triton_dll Name 'CtxCreateFrom';
        //! Destructor
        procedure  CtxDelete(hCtx: HandleAstContext); cdecl;  external Triton_dll Name 'CtxDelete';
        //! AST C++ API - assert node builder
        function Ctxassert_(hCtx: HandleAstContext ;  expr: HandleAbstractNode):HandleAbstractNode;cdecl;  external Triton_dll Name 'assert_';
        //! AST C++ API - bv node builder { TODO -oMax -c : support uint512 28/07/2019 08:55:16 }
        function  Ctxbv(hCtx: HandleAstContext ;  value: uint64; size: uint32):HandleAbstractNode;cdecl;  external Triton_dll Name 'bv';
        //! AST C++ API - bvadd node builder
        function  Ctxbvadd(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvadd';
        //! AST C++ API - bvand node builder
        function  Ctxbvand(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvand';
        //! AST C++ API - bvashr node builder
        function  Ctxbvashr(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvashr';
        //! AST C++ API - bvfalse node builder
        function  Ctxbvfalse( hCtx: HandleAstContext):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvfalse';
        //! AST C++ API - bvlshr node builder
        function  Ctxbvlshr(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvlshr';
        //! AST C++ API - bvmul node builder
        function  Ctxbvmul(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvmul';
        //! AST C++ API - bvnand node builder
        function  Ctxbvnand(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvnand';
        //! AST C++ API - bvneg node builder
        function  Ctxbvneg(hCtx: HandleAstContext ;  expr: HandleAbstractNode):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvneg';
        //! AST C++ API - bvnor node builder
        function  Ctxbvnor(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvnor';
        //! AST C++ API - bvnot node builder
        function  Ctxbvnot(hCtx: HandleAstContext ;  expr: HandleAbstractNode):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvnot';
        //! AST C++ API - bvor node builder
        function  Ctxbvor(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvor';
        //! AST C++ API - bvrol node builder
        function  Ctxbvrol_u(hCtx: HandleAstContext ; expr: HandleAbstractNode ;  rot: uint32):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvrol_u';
        //! AST C++ API - bvrol node builder
        function  Ctxbvrol(hCtx: HandleAstContext ; expr: HandleAbstractNode ;  rot :HandleAbstractNode):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvrol';
        //! AST C++ API - bvror node builder
        function  Ctxbvror_u(hCtx: HandleAstContext ; expr: HandleAbstractNode ;  rot: uint32):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvror_u';
        //! AST C++ API - bvror node builder
        function  Ctxbvror(hCtx: HandleAstContext ; expr: HandleAbstractNode ;  rot: HandleAbstractNode):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvror';
        //! AST C++ API - bvsdiv node builder
        function  Ctxbvsdiv(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvsdiv';
        //! AST C++ API - bvsge node builder
        function  Ctxbvsge(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvsge';
        //! AST C++ API - bvsgt node builder
        function  Ctxbvsgt(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvsgt';
        //! AST C++ API - bvshl node builder
        function  Ctxbvshl(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvshl';
        //! AST C++ API - bvsle node builder
        function  Ctxbvsle(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvsle';
        //! AST C++ API - bvslt node builder
        function  Ctxbvslt(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvslt';
        //! AST C++ API - bvsmod node builder
        function  Ctxbvsmod(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvsmod';
        //! AST C++ API - bvsrem node builder
        function  Ctxbvsrem(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvsrem';
        //! AST C++ API - bvsub node builder
        function  Ctxbvsub(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode; cdecl;  external Triton_dll Name 'bvsub';
        //! AST C++ API - bvtrue node builder
        function  Ctxbvtrue(hCtx: HandleAstContext):HandleAbstractNode; cdecl;  external Triton_dll Name 'bvtrue';
        //! AST C++ API - bvudiv node builder
        function  Ctxbvudiv(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode; cdecl;  external Triton_dll Name 'bvudiv';
        //! AST C++ API - bvuge node builder
        function  Ctxbvuge(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvuge';
        //! AST C++ API - bvugt node builder
        function  Ctxbvugt(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode; cdecl;  external Triton_dll Name 'bvugt';
        //! AST C++ API - bvule node builder
        function  Ctxbvule(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode; cdecl;  external Triton_dll Name 'bvule';
        //! AST C++ API - bvult node builder
        function  Ctxbvult(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode; cdecl;  external Triton_dll Name 'bvult';
        //! AST C++ API - bvurem node builder
        function  Ctxbvurem(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvurem';
        //! AST C++ API - bvxnor node builder
        function  Ctxbvxnor(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'bvxnor';
        //! AST C++ API - bvxor node builder
        function  Ctxbvxor(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode; cdecl;  external Triton_dll Name 'bvxor';
        //! AST C++ API - concat node builder
        function  Ctxconcat(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'concat';
        //! AST C++ API - declare node builder
        function  Ctxdeclare(hCtx: HandleAstContext ; variable: HandleAbstractNode):HandleAbstractNode; cdecl;  external Triton_dll Name 'declare';
        //! AST C++ API - distinct node builder
        function  Ctxdistinct(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode; cdecl;  external Triton_dll Name 'distinct';
        //! AST C++ API - equal node builder
        function  Ctxequal(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode; cdecl;  external Triton_dll Name 'equal';
        //! AST C++ API - extract node builder
        function  Ctxextract(hCtx: HandleAstContext ; high: uint32; low: uint32;  expr: HandleAbstractNode):HandleAbstractNode;cdecl;  external Triton_dll Name 'extract';
        //! AST C++ API - iff node builder
        function  Ctxiff(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'iff';
        //! AST C++ API - integer node builder { TODO -oMax -c : support uint512 28/07/2019 08:55:16 }
        function  Ctxinteger(hCtx: HandleAstContext ;  value: uint64):HandleAbstractNode; cdecl;  external Triton_dll Name 'integer';
        //! AST C++ API - ite node builder
        function  Ctxite(hCtx: HandleAstContext ; ifExpr, thenExpr, elseExpr: HandleAbstractNode):HandleAbstractNode;cdecl;  external Triton_dll Name 'ite';
        //! AST C++ API - land node builder
        function  Ctxland(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'land';
        //! AST C++ API - let node builder
        function  Ctxlet(hCtx: HandleAstContext ; alias: PAnsiChar; expr2, expr3: HandleAbstractNode):HandleAbstractNode;cdecl;  external Triton_dll Name 'let';
        //! AST C++ API - lnot node builder
        function  Ctxlnot(hCtx: HandleAstContext ;  expr: HandleAbstractNode):HandleAbstractNode;cdecl;  external Triton_dll Name 'lnot';
        //! AST C++ API - lor node builder
        function  Ctxlor(hCtx: HandleAstContext ; expr1, expr2: HandleAbstractNode ):HandleAbstractNode;cdecl;  external Triton_dll Name 'lor';
        //! AST C++ API - reference node builder
        function  Ctxreference(hCtx: HandleAstContext ; expr: HandleSharedSymbolicExpression):HandleAbstractNode; cdecl;  external Triton_dll Name 'reference';
        //! AST C++ API - string node builder
        function  Ctxstring(hCtx: HandleAstContext ; value: PAnsiChar):HandleAbstractNode; cdecl;  external Triton_dll Name 'string';
        //! AST C++ API - sx node builder
        function  Ctxsx(hCtx: HandleAstContext ; sizeExt: uint32;  expr: HandleAbstractNode):HandleAbstractNode;cdecl;  external Triton_dll Name 'sx';
        //! AST C++ API - variable node builder
        function  Ctxvariable(hCtx: HandleAstContext ; symVar: HandleSharedSymbolicVariable):HandleAbstractNode; cdecl;  external Triton_dll Name 'variable';
        //! AST C++ API - zx node builder
        function  Ctxzx(hCtx: HandleAstContext ; sizeExt: uint32;  expr: HandleAbstractNode):HandleAbstractNode; cdecl;  external Triton_dll Name 'zx';
        //! Initializes a variable in the context { TODO -oMax -c : support uint512 28/07/2019 08:55:16 }
        procedure  CtxinitVariable(hCtx: HandleAstContext ; name: PAnsiChar; value: uint64; node: HandleAbstractNode); cdecl;  external Triton_dll Name 'initVariable';
        //! Updates a variable value in this context { TODO -oMax -c : support uint512 28/07/2019 08:55:16 }
        procedure  CtxupdateVariable(hCtx: HandleAstContext ; name: PAnsiChar; value: uint64);  cdecl;  external Triton_dll Name 'updateVariable';
        //! Gets a variable node from its name.
        function  CtxgetVariableNode(hCtx: HandleAstContext ; name: PAnsiChar):HandleAbstractNode;cdecl;  external Triton_dll Name 'getVariableNode';
        //! Gets a variable value from its name. { TODO -oMax -c : support uint512 28/07/2019 08:55:16 }
        function  CtxgetVariableValue(hCtx: HandleAstContext ; varName: PAnsiChar):uint64 ;  cdecl;  external Triton_dll Name 'getVariableValue';
        //! Sets the representation mode for this astContext
        procedure  CtxsetRepresentationMode(hCtx: HandleAstContext ; mode: uint32); cdecl;  external Triton_dll Name 'setRepresentationMode';
        //! Gets the representations mode of this astContext
        function  CtxgetRepresentationMode(hCtx: HandleAstContext):uint32 ;cdecl;  external Triton_dll Name 'getRepresentationMode';
        //! Returns a deque of collected matched nodes via a depth-first pre order traversal.
	      function Node_search(node: HandleAbstractNode; var outArray: PAHNode; match : ast_e = ANY_NODE): UInt32; cdecl;  external Triton_dll Name 'Node_search';
        //! AST C++ API - Unrolls the SSA form of a given AST.
        function Node_unroll(node: HandleAbstractNode): HandleAbstractNode; cdecl;  external Triton_dll Name 'Node_unroll';

implementation

{ Modes }

procedure TModes.Create;
begin
    ZeroMemory(@self,SizeOf(TModes));
    FHModes := MCreateModes;
end;

procedure TModes.Create(other: TModes);
begin
     ZeroMemory(@self,SizeOf(TModes));
     FHModes := MCreateModesFrom(hModes(other));
end;

procedure TModes.enableMode(mode: mode_e; flag: Boolean);
begin
    MenableMode(FHModes,mode,flag );
end;

function TModes.isModeEnabled(mode: mode_e): Boolean;
begin
    Result := MisModeEnabled(FHModes,mode)
end;

class operator TModes.Explicit(hMod: hModes): TModes;
begin
    ZeroMemory(@Result,SizeOf(TModes));
    Result.FHModes  := hMod;
end;

class operator TModes.Explicit(rMod: TModes): hModes;
begin
    Result := rMod.FHModes;
end;

procedure TModes.Free;
begin
    MDeleteModes(FHModes);
end;

{ AstRepresentation }

procedure TAstRepresentation.Create;
begin
    FHAstRap := RAPCreate;
end;

procedure TAstRepresentation.Create(other: TAstRepresentation);
begin
    FHAstRap := RAPCreateFrom(hAstRepresentation(other))
end;

procedure TAstRepresentation.Free;
begin
    RAPDelete(FHAstRap);
end;

class operator TAstRepresentation.Explicit(hAst_R: hAstRepresentation): TAstRepresentation;
begin
     ZeroMemory(@Result,SizeOf(TAstRepresentation));
     Result.FHAstRap := hAst_R

end;

class operator TAstRepresentation.Explicit(rAst_R: TAstRepresentation): hAstRepresentation;
begin
    Result := rAst_R.FHAstRap;
end;

function TAstRepresentation.getMode: uint32;
begin
    Result := RAPgetMode(FHAstRap)
end;

procedure TAstRepresentation.setMode(mode: uint32);
begin
    RAPsetMode(FHAstRap,mode);
end;

{ AstContext }

procedure AstContext.Create(modes: TModes);
begin
    FAstCtx := CtxCreate( hModes(modes))
end;

procedure AstContext.Create(other: TAstRepresentation);
begin
    FAstCtx := CtxCreateFrom( hAstRepresentation(other))
end;

procedure AstContext.Free;
begin
    CtxDelete(FAstCtx);
end;

class operator AstContext.Explicit(hAst: HandleAstContext): AstContext;
begin
    ZeroMemory(@Result,SizeOf(AstContext));
    Result.FAstCtx := hAst;
end;

class operator AstContext.Implicit(hAst: HandleAstContext): AstContext;
begin
    ZeroMemory(@Result,SizeOf(AstContext));
    Result.FAstCtx := hAst;
end;

class operator AstContext.Explicit(rAst: AstContext): HandleAstContext;
begin
    Result := rAst.FAstCtx;
end;

function AstContext.assert_(expr: AbstractNode): AbstractNode;
begin
    Result := Ctxassert_(FAstCtx,HandleAbstractNode(expr))
end;

function AstContext.bv(value: uint64; size: uint32): AbstractNode;
begin
    Result := Ctxbv(FAstCtx,value,size)
end;

function AstContext.bvand(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvand(FAstCtx,expr1,expr2)
end;

function AstContext.bvadd(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvadd(FAstCtx,expr1,expr2)
end;

function AstContext.bvashr(expr1,expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvashr(FAstCtx,expr1,expr2)
end;

function AstContext.bvfalse: AbstractNode;
begin
    Result := Ctxbvfalse(FAstCtx)
end;

function AstContext.bvlshr(expr1,expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvlshr(FAstCtx,expr1,expr2)
end;

function AstContext.bvmul(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxbvmul(FAstCtx,expr1,expr2)
end;

function AstContext.bvnand(expr1,expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxbvnand(FAstCtx,expr1,expr2)
end;

function AstContext.bvneg(expr: AbstractNode): AbstractNode;
begin
    Result := Ctxbvneg(FAstCtx,expr)
end;

function AstContext.bvnor(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvnor(FAstCtx,expr1,expr2)
end;

function AstContext.bvnot(expr: AbstractNode): AbstractNode;
begin
    Result := Ctxbvnot(FAstCtx,expr)
end;

function AstContext.bvor(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvor(FAstCtx,expr1, expr2)
end;

function AstContext.bvrol(expr, rot: AbstractNode): AbstractNode;
begin
    Result := Ctxbvrol(FAstCtx,expr,rot)
end;

function AstContext.bvrol(expr: AbstractNode;rot: uint32): AbstractNode;
begin
   Result := Ctxbvrol_u(FAstCtx,expr,rot)
end;

function AstContext.bvror(expr, rot: AbstractNode): AbstractNode;
begin
   Result := Ctxbvror(FAstCtx,expr,rot)
end;

function AstContext.bvror(expr: AbstractNode; rot: uint32): AbstractNode;
begin
    Result := Ctxbvror_u(FAstCtx,expr,rot)
end;

function AstContext.bvsdiv(expr1,expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvsdiv(FAstCtx,expr1,expr2)
end;

function AstContext.bvsge(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvsge(FAstCtx,expr1, expr2)
end;

function AstContext.bvsgt(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxbvsgt(FAstCtx,expr1, expr2)
end;

function AstContext.bvshl(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvshl(FAstCtx,expr1, expr2)
end;

function AstContext.bvsle(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvsle(FAstCtx,expr1, expr2)
end;

function AstContext.bvslt(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvslt(FAstCtx,expr1, expr2)
end;

function AstContext.bvsmod(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvsmod(FAstCtx,expr1, expr2)
end;

function AstContext.bvsrem(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxbvsrem(FAstCtx,expr1, expr2)
end;

function AstContext.bvsub(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvsub(FAstCtx,expr1, expr2)
end;

function AstContext.bvtrue: AbstractNode;
begin
   Result := Ctxbvtrue(FAstCtx)
end;

function AstContext.bvudiv(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxbvudiv(FAstCtx,expr1, expr2)
end;

function AstContext.bvuge(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxbvuge(FAstCtx,expr1, expr2)
end;

function AstContext.bvugt(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvugt(FAstCtx,expr1, expr2)
end;

function AstContext.bvule(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxbvule(FAstCtx,expr1, expr2)
end;

function AstContext.bvult(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxbvult(FAstCtx,expr1, expr2)
end;

function AstContext.bvurem(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvurem(FAstCtx,expr1, expr2)
end;

function AstContext.bvxnor(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxbvxnor(FAstCtx,expr1, expr2)
end;

function AstContext.bvxor(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxbvxor(FAstCtx,expr1, expr2)
end;

function AstContext.concat(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxconcat(FAstCtx,expr1, expr2)
end;

function AstContext.&string(value: PAnsiChar): AbstractNode;
begin
    Result := Ctxstring(FAstCtx,value)
end;

function AstContext.declare(variable: AbstractNode): AbstractNode;
begin
    Result := Ctxdeclare(FAstCtx,variable)
end;

function AstContext.distinct(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxdistinct(FAstCtx,expr1, expr2)
end;

function AstContext.equal(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxequal(FAstCtx,expr1, expr2)
end;

function AstContext.getRepresentationMode: uint32;
begin
    Result := CtxgetRepresentationMode(FAstCtx)
end;

function AstContext.getVariableNode(name: PAnsiChar): AbstractNode;
begin
   Result := CtxgetVariableNode(FAstCtx,name)
end;

function AstContext.getVariableValue(varName: PAnsiChar): uint64;
begin
    Result := CtxgetVariableValue(FAstCtx,varName)
end;

function AstContext.iff(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxiff(FAstCtx,expr1, expr2)
end;

procedure AstContext.initVariable(name: PAnsiChar; value: uint64; node: AbstractNode);
begin
    CtxinitVariable(FAstCtx,name,value,node);
end;

function AstContext.integer(value: uint64): AbstractNode;
begin
    Result := Ctxinteger(FAstCtx,value)
end;

function AstContext.ite(ifExpr, thenExpr,elseExpr: AbstractNode): AbstractNode;
begin
    Result := Ctxite(FAstCtx,ifExpr,thenExpr,elseExpr)
end;

function AstContext.land(expr1, expr2: AbstractNode): AbstractNode;
begin
   Result := Ctxland(FAstCtx,expr1, expr2)
end;

function AstContext.let(alias: PAnsiChar; expr2, expr3: AbstractNode): AbstractNode;
begin
   Result := Ctxlet(FAstCtx,alias,expr2, expr3)
end;

function AstContext.lnot(expr: AbstractNode): AbstractNode;
begin
   Result := Ctxlnot(FAstCtx,expr)
end;

function AstContext.lor(expr1, expr2: AbstractNode): AbstractNode;
begin
    Result := Ctxlor(FAstCtx,expr1, expr2)
end;

function AstContext.reference(expr: symbolicExp): AbstractNode;
begin
   Result := Ctxreference(FAstCtx,HandleSharedSymbolicExpression(expr))
end;

procedure AstContext.setRepresentationMode(mode: uint32);
begin
    CtxsetRepresentationMode(FAstCtx, mode);
end;

function AstContext.sx(sizeExt: uint32;expr: AbstractNode): AbstractNode;
begin
   Result := Ctxsx(FAstCtx,sizeExt,expr)
end;

procedure AstContext.updateVariable(name: PAnsiChar; value: uint64);
begin
    CtxupdateVariable(FAstCtx,name,value);
end;

function AstContext.variable(symVar: SymbolicVar): AbstractNode;
begin
    Result := Ctxvariable(FAstCtx, HandleSharedSymbolicVariable(symVar))
end;

function AstContext.zx(sizeExt: uint32; expr: AbstractNode): AbstractNode;
begin
    Result := Ctxzx(FAstCtx,sizeExt,expr)
end;

function AstContext.extract(high, low: uint32; expr: AbstractNode): AbstractNode;
begin
   Result := Ctxextract(FAstCtx,high,low,expr)
end;

function AstContext.Search(Node: AbstractNode; match: ast_e): TArray<AbstractNode>;
var
  outArray: PAHNode ;
  res     : TArray<AbstractNode>;
  n       : UInt32;
  i       : Int32;
begin
    outArray := nil;
    n :=  Node_search(Node,outArray,match);

    res := [];
   // if n < 1 then Exit;

    for i := 0 to n - 1 do
       res := res + [ AbstractNode(outArray[i]) ] ;

    Result := res;

end;

function AstContext.unroll(node: AbstractNode): AbstractNode;
begin
    Result := AbstractNode (Node_unroll(node));
end;

// emulate triton::ast::unroll
function  Triton_Ast_unroll(node: AbstractNode): AbstractNode;
begin
    Result := AbstractNode (Node_unroll(node));
end;

end.
