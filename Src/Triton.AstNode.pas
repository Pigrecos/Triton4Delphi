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
      FChildrens          : TArray<AbstractNode>;
      FParents            : TArray<AbstractNode>;

      function  getChildren: TArray<AbstractNode>;
      function  getParents:  TArray<AbstractNode>;
      function  getContext: HandleAstContext ;

    public
      procedure Create(tipo: ast_e; ctxt: HandleAstContext);
      procedure Free;

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

    property  Childrens  : TArray<AbstractNode> read getChildren;
    property  Parents    : TArray<AbstractNode> read getParents;
    property  Context    : HandleAstContext     read getContext;

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

{ AbstractNode }

procedure AbstractNode.Create(tipo: ast_e;  ctxt: HandleAstContext);
begin
    FHandleAbstractNode      := Node_Create(tipo,ctxt);
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
end;

class operator AbstractNode.Explicit(rNode: AbstractNode): HandleAbstractNode;
begin
    Result := rNode.FHandleAbstractNode
end;

class operator AbstractNode.Explicit(hNode: HandleAbstractNode): AbstractNode;
begin
    ZeroMemory(@Result,SizeOf(AbstractNode));
    Result.FHandleAbstractNode  := hNode ;
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

procedure AbstractNode.addChild(child: AbstractNode);
begin
    Node_addChild(FHandleAbstractNode,HandleAbstractNode(child));
end;

procedure AbstractNode.removeParent(p: AbstractNode);
begin
    Node_removeParent(FHandleAbstractNode,HandleAbstractNode(p));
end;

procedure AbstractNode.setBitvectorSize(size: uint32);
begin
    Node_setBitvectorSize(FHandleAbstractNode,size);
end;

procedure AbstractNode.setChild(index: uint32; child: AbstractNode);
begin
    Node_setChild(FHandleAbstractNode,index,HandleAbstractNode(child));
end;

procedure AbstractNode.setParent(p: AbstractNode);
begin
    Node_setParent(FHandleAbstractNode,HandleAbstractNode(p));
end;

procedure AbstractNode.setParents(p: PAHNode; size: uint32);
begin
    Node_setParents(FHandleAbstractNode,p,size);
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
