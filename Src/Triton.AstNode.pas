unit Triton.AstNode;

{$Z4}
{$POINTERMATH ON}

interface
    uses
       System.SysUtils, Winapi.Windows, System.Generics.Collections,
       Triton.Define;

type
  PAHNode = ^HandleAbstractNode;

  AbstractNode = record
    private
      FHandleAbstractNode : HandleAbstractNode;
      FChildrens          : TArray<AbstractNode> ;
      FParents            : TArray<AbstractNode> ;
      FTipo               : ast_e;
      FBitvectorSize      : uint32;
      FBitvectorMask      : UInt64;
      FisSigned           : Boolean;
      FisSymbolized       : Boolean;
      FisLogical          : Boolean;

      function  getChildren: TArray<AbstractNode>;
      function  getParents:  TArray<AbstractNode>;
      function  getContext: HandleAstContext ;
      procedure Update;
      function  IsNodeAssigned: Boolean;

    public
      procedure Create(tipo: ast_e; ctxt: HandleAstContext);
      procedure Free;

      //comparasion
      function Equal               (n1: AbstractNode): AbstractNode;
      function NotEqual            (n1: AbstractNode): AbstractNode;
      function LessThanOrEqual     (n1: AbstractNode): AbstractNode;
      function GreaterThanOrEqual  (n1: AbstractNode): AbstractNode;
      function LessThan            (n1: AbstractNode): AbstractNode;
      function GreaterThan         (n1: AbstractNode): AbstractNode;

      function  getType: ast_e ;
      function  getBitvectorSize: uint32 ;
      function  getBitvectorMask: uint64 ;
      function  isSigned: Boolean ;
      function  isSymbolized:Boolean ;
      function  isLogical: Boolean ;
      function  equalTo(other: AbstractNode): Boolean ;
      function  evaluate: uint64 ;
      procedure removeParent(p: AbstractNode);
      procedure setParent(p: AbstractNode);
      procedure setParents(p: PAHNode; size: uint32);
      procedure setBitvectorSize(size: uint32);
      procedure addChild(child: AbstractNode);
      procedure setChild(index: uint32; child: AbstractNode);
      function  unrollAst: AbstractNode;
      function  str: string;
      procedure init ;

      function    ToStr: string;

      class Operator Explicit(hNode: HandleAbstractNode):AbstractNode;
      class Operator Explicit(rNode: AbstractNode):HandleAbstractNode;
      class operator implicit(rNode: AbstractNode): HandleAbstractNode;
      class Operator Implicit(hNode: HandleAbstractNode):AbstractNode;

      (*As we can not overload all AST's operators only these following operators are overloaded:

      Python's Operator | e.g: SMT2-Lib format
      ------------------|---------------------
      a + b             | (bvadd a b)
      a - b             | (bvsub a b)
      a * b             | (bvmul a b)
      a / b             | (bvudiv a b)
      a | b             | (bvor a b)
      a & b             | (bvand a b)
      a ^ b             | (bvxor a b)
      a % b             | (bvurem a b)
      a << b            | (bvshl a b)
      a >> b            | (bvlshr a b)
      ~a                | (bvnot a)
      -a                | (bvneg a)
      a == b            | (= a b)
      a != b            | (not (= a b))
      a <= b            | (bvule a b)
      a >= b            | (bvuge a b)
      a < b             | (bvult a b)
      a > b             | (bvugt a b)    *)

      class Operator Add       (n1,n2: AbstractNode): AbstractNode;
      class Operator Subtract  (n1,n2: AbstractNode): AbstractNode;
      class Operator Multiply  (n1,n2: AbstractNode): AbstractNode;
      class Operator Divide    (n1,n2: AbstractNode): AbstractNode;
      class Operator LogicalOr (n1,n2: AbstractNode): AbstractNode;
      class Operator BitwiseAnd(n1,n2: AbstractNode): AbstractNode;
      class Operator BitwiseXor(n1,n2: AbstractNode): AbstractNode;
      class Operator Modulus   (n1,n2: AbstractNode): AbstractNode;
      class Operator LeftShift (n1,n2: AbstractNode): AbstractNode;
      class Operator RightShift(n1,n2: AbstractNode): AbstractNode;
      class Operator LogicalNot(n1   : AbstractNode): AbstractNode;
      class Operator Negative  (n1   : AbstractNode): AbstractNode;

    property  Childrens     : TArray<AbstractNode> read getChildren;
    property  Parents       : TArray<AbstractNode> read getParents;
    property  Context       : HandleAstContext     read getContext;
    property  Tipo          : ast_e                read FTipo;
    property  BitvectorSize : uint32               read FBitvectorSize;
    property  BitvectorMask : UInt64               read FBitvectorMask;
    property  Signed        : Boolean              read FisSigned;
    property  Symbolized    : Boolean              read FisSymbolized;
    property  Logical       : Boolean              read FisLogical;

  end;

  (*  Ast Node ============================================================================== *)
      //! Constructor.
      function Node_Create(tipo: ast_e; ctxt: HandleAstContext): HandleAbstractNode;cdecl;  external Triton_dll Name 'Node_Create';
      //! Destructor.
      procedure Node_Delete(Handle: HandleAbstractNode); cdecl;  external Triton_dll Name 'Node_Delete';
      //! Access to its context
      function Node_getContext(Handle: HandleAbstractNode): HandleAstContext ; cdecl;  external Triton_dll Name 'Node_getContext';
      //! Returns the type of the node.
      function Node_getType(Handle: HandleAbstractNode): ast_e ; cdecl;  external Triton_dll Name 'Node_getType';
      //! Returns the size of the node.
      function Node_getBitvectorSize(Handle: HandleAbstractNode): uint32 ; cdecl;  external Triton_dll Name 'Node_getBitvectorSize';
      //! Returns the vector mask according the size of the node.//todo add support uint512
      function Node_getBitvectorMask(Handle: HandleAbstractNode): uint64 ; cdecl;  external Triton_dll Name 'Node_getBitvectorMask';
      //! According to the size of the expression, returns true if the MSB is 1.
      function Node_isSigned(Handle: HandleAbstractNode): Boolean ; cdecl;  external Triton_dll Name 'Node_isSigned';
      //! Returns true if the tree contains a symbolic variable.
      function Node_isSymbolized(Handle: HandleAbstractNode):Boolean ;cdecl;  external Triton_dll Name 'Node_isSymbolized';
      //! Returns true if it's a logical node.
      function Node_isLogical(Handle: HandleAbstractNode): Boolean ; cdecl;  external Triton_dll Name 'Node_isLogical';
      //! Returns true if the current tree is equal to the second one.
      function Node_equalTo(Handle: HandleAbstractNode; other: HandleAbstractNode): Boolean ;cdecl;  external Triton_dll Name 'Node_equalTo';
      //! Evaluates the tree.// todo support uint512
      function Node_evaluate(Handle: HandleAbstractNode): uint64 ;cdecl;  external Triton_dll Name 'Node_evaluate';
      //! Returns the children of the node.
      function Node_getChildren(Handle: HandleAbstractNode; var outArray: PAHNode): uint32;cdecl;  external Triton_dll Name 'Node_getChildren';
      //! Returns the parents of node or an empty set if there is still no parent defined.
      function Node_getParents(Handle: HandleAbstractNode; var outArray: PAHNode):uint32;cdecl;  external Triton_dll Name 'Node_getParents';
      //! Removes a parent node.
      procedure Node_removeParent(Handle: HandleAbstractNode; p: HandleAbstractNode);cdecl;  external Triton_dll Name 'Node_removeParent';
      //! Sets a parent node.
      procedure Node_setParent(Handle: HandleAbstractNode; p: HandleAbstractNode); cdecl;  external Triton_dll Name 'Node_setParent';
      //! Sets the parent nodes.
      procedure Node_setParents(Handle: HandleAbstractNode; p: PAHNode; size: uint32); cdecl;  external Triton_dll Name 'Node_setParents';
      //! Sets the size of the node.
      procedure Node_setBitvectorSize(Handle: HandleAbstractNode; size: uint32); cdecl;  external Triton_dll Name 'Node_setBitvectorSize';
      //! Adds a child.
      procedure Node_addChild(Handle: HandleAbstractNode; child: HandleAbstractNode); cdecl;  external Triton_dll Name 'Node_addChild';
      //! Sets a child at an index.
      procedure Node_setChild(Handle: HandleAbstractNode; index: uint32; child: HandleAbstractNode);cdecl;  external Triton_dll Name 'Node_setChild';
      //! Returns the string representation of the node.
      procedure Node_str(Handle: HandleAbstractNode;var sStr: PAnsiChar); cdecl;  external Triton_dll Name 'Node_str';
      //! Init stuffs like size and eval.
      procedure  Node_init(Handle: HandleAbstractNode) ; cdecl;  external Triton_dll Name 'Node_init';
      //! AST C++ API - Unrolls the SSA form of a given AST.
      function Node_unrollAst(node: HandleAbstractNode): HandleAbstractNode; cdecl;  external Triton_dll Name 'unrollAst';
      // !Displays the node in ast representation.

	    procedure AstToStr(hnode: HandleAbstractNode; var sOut: PAnsiChar); cdecl;  external Triton_dll Name 'AstToStr';

implementation
    uses Triton.AstContext;

{ AbstractNode }

procedure AbstractNode.Create(tipo: ast_e;  ctxt: HandleAstContext);
begin
    FHandleAbstractNode      := Node_Create(tipo,ctxt);

    Update;
end;

procedure AbstractNode.Free;
begin
    Node_Delete(FHandleAbstractNode)
end;

class operator AbstractNode.implicit(rNode: AbstractNode): HandleAbstractNode;
begin
    Result := rNode.FHandleAbstractNode
end;

class operator AbstractNode.implicit(hNode: HandleAbstractNode): AbstractNode;
begin
    ZeroMemory(@Result,SizeOf(AbstractNode));
    Result.FHandleAbstractNode  := hNode ;
    Result.Update;

end;

class operator AbstractNode.Explicit(rNode: AbstractNode): HandleAbstractNode;
begin
    Result := rNode.FHandleAbstractNode
end;

class operator AbstractNode.Explicit(hNode: HandleAbstractNode): AbstractNode;
begin
    ZeroMemory(@Result,SizeOf(AbstractNode));
    Result.FHandleAbstractNode  := hNode ;
    Result.Update;

end;

procedure AbstractNode.Update;
begin
    FTipo          := getType;

    if FTipo in  [ASSERT_NODE..ZX_NODE] then
    begin
      FBitvectorSize := getBitvectorSize;
      FBitvectorMask := getBitvectorMask;
      FisSigned      := isSigned;
      FisSymbolized  := isSymbolized;
      FisLogical     := isLogical;
    end;
end;

function AbstractNode.equalTo(other: AbstractNode): Boolean;
begin
    Result := Node_equalTo(FHandleAbstractNode,HandleAbstractNode(other))
end;

function AbstractNode.evaluate: uint64;
begin
    Result := Node_evaluate(FHandleAbstractNode)
end;

function AbstractNode.getBitvectorMask: uint64;
begin
    Result := Node_getBitvectorMask(FHandleAbstractNode)
end;

function AbstractNode.getBitvectorSize: uint32;
begin
    Result := Node_getBitvectorSize(FHandleAbstractNode)
end;

function AbstractNode.getContext: HandleAstContext;
begin
     Result := Node_getContext(FHandleAbstractNode)
end;

function AbstractNode.getType: ast_e;
begin
     Result := Node_getType(FHandleAbstractNode)
end;

function AbstractNode.isLogical: Boolean;
begin
    Result := Node_isLogical(FHandleAbstractNode)
end;

function AbstractNode.isSigned: Boolean;
begin
    Result := Node_isSigned(FHandleAbstractNode)
end;

function AbstractNode.isSymbolized: Boolean;
begin
    Result := Node_isSymbolized(FHandleAbstractNode)
end;

function AbstractNode.getChildren: TArray<AbstractNode>;
var
  n,i       : Integer;
  outArray  : PAHNode ;

begin

    n :=  Node_getChildren(FHandleAbstractNode,outArray);

    FChildrens  := [];
    for i := 0 to n - 1 do
       FChildrens := FChildrens + [ AbstractNode( outArray[i]) ];

    Result :=  FChildrens;
end;

function AbstractNode.getParents: TArray<AbstractNode>;
var
  n,i     : Integer;
  outArray: PAHNode ;

begin
    outArray := nil;
    n :=  Node_getParents(FHandleAbstractNode,outArray);

    FParents := [];
    for i := 0 to n - 1 do
       FParents := FParents + [ AbstractNode(outArray[i]) ] ;

    Result := FParents;
end;

procedure AbstractNode.init;
begin
    Node_init(FHandleAbstractNode);
end;

function AbstractNode.IsNodeAssigned: Boolean;
begin
    Result := FHandleAbstractNode <> nil;
end;

class operator AbstractNode.Add(n1, n2: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned) and  (n2.IsNodeAssigned) then
     res :=  AstContext(n1.Context).bvadd(n1,n2);

    Result := res;
end;

class operator AbstractNode.Subtract(n1, n2: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned) and  (n2.IsNodeAssigned) then
     res :=  AstContext(n1.Context).bvsub(n1,n2);

    Result := res;
end;

class operator AbstractNode.Divide(n1, n2: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned) and  (n2.IsNodeAssigned) then
     res :=  AstContext(n1.Context).bvudiv(n1,n2);

    Result := res;
end;

class operator AbstractNode.Modulus(n1, n2: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned) and  (n2.IsNodeAssigned) then
     res :=  AstContext(n1.Context).bvurem(n1,n2);

    Result := res;
end;

class operator AbstractNode.Multiply(n1, n2: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned) and  (n2.IsNodeAssigned) then
     res :=  AstContext(n1.Context).bvmul(n1,n2);

    Result := res;
end;

class operator AbstractNode.BitwiseAnd(n1, n2: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned) and  (n2.IsNodeAssigned) then
     res :=  AstContext(n1.Context).bvand(n1,n2);

    Result := res;
end;

class operator AbstractNode.BitwiseXor(n1, n2: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned) and  (n2.IsNodeAssigned) then
     res :=  AstContext(n1.Context).bvxor(n1,n2);

    Result := res;
end;

class operator AbstractNode.LeftShift(n1, n2: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned) and  (n2.IsNodeAssigned) then
     res :=  AstContext(n1.Context).bvshl(n1,n2);

    Result := res;
end;

class operator AbstractNode.RightShift(n1, n2: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned) and  (n2.IsNodeAssigned) then
     res :=  AstContext(n1.Context).bvlshr(n1,n2);

    Result := res;

end;

class operator AbstractNode.LogicalOr(n1, n2: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned) and  (n2.IsNodeAssigned) then
     res :=  AstContext(n1.Context).bvor(n1,n2);

    Result := res;
end;

class operator AbstractNode.LogicalNot(n1: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned)  then
     res :=  AstContext(n1.Context).bvnot(n1);

    Result := res;
end;

class operator AbstractNode.Negative(n1: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned)  then
     res :=  AstContext(n1.Context).bvneg(n1);

    Result := res;
end;

function  AbstractNode.Equal(n1: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned)  then
     res :=  AstContext(n1.Context).equal(self,n1);

    Result := res;
end;

function AbstractNode.NotEqual(n1: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned)  then
    begin
        res :=  AstContext(n1.Context).equal(self,n1);
        res :=  AstContext(n1.Context).lnot(res);
    end;

    Result := res;
end;

function AbstractNode.LessThan(n1: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned)  then
     res :=  AstContext(n1.Context).bvult(self,n1);

    Result := res;
end;

function AbstractNode.LessThanOrEqual(n1: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned)  then
     res :=  AstContext(n1.Context).bvule(self,n1);

    Result := res;
end;

function AbstractNode.GreaterThan(n1: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned)  then
     res :=  AstContext(n1.Context).bvugt(self,n1);

    Result := res;
end;

function AbstractNode.GreaterThanOrEqual(n1: AbstractNode): AbstractNode;
var
  res : AbstractNode;
begin
    if (n1.IsNodeAssigned)  then
     res :=  AstContext(n1.Context).bvuge(self,n1);

    Result := res;
end;

procedure AbstractNode.addChild(child: AbstractNode);
begin
    Node_addChild(FHandleAbstractNode,HandleAbstractNode(child));

    FChildrens     := getChildren;
end;

procedure AbstractNode.removeParent(p: AbstractNode);
begin
    Node_removeParent(FHandleAbstractNode,HandleAbstractNode(p));

    FParents       := getParents;
end;

procedure AbstractNode.setBitvectorSize(size: uint32);
begin
    Node_setBitvectorSize(FHandleAbstractNode,size);

    FBitvectorSize := getBitvectorSize;
end;

procedure AbstractNode.setChild(index: uint32; child: AbstractNode);
begin
    Node_setChild(FHandleAbstractNode,index,HandleAbstractNode(child));

    FChildrens     := getChildren;
end;

procedure AbstractNode.setParent(p: AbstractNode);
begin
    Node_setParent(FHandleAbstractNode,HandleAbstractNode(p));

    FParents       := getParents;
end;

procedure AbstractNode.setParents(p: PAHNode; size: uint32);
begin
    Node_setParents(FHandleAbstractNode,p,size);

    FParents       := getParents;
end;

function AbstractNode.str: string;
var
  p : PAnsiChar;
begin
    Node_str(FHandleAbstractNode,p) ;
    Result := string(p);
end;

function AbstractNode.ToStr: string;
var
  p : PAnsiChar;
begin
    AstToStr(FHandleAbstractNode,p);
    Result := string(p);
end;

function AbstractNode.unrollAst: AbstractNode;
begin
    Result := AbstractNode (Node_unrollAst(FHandleAbstractNode));
end;

end.
