module Petsc where

import Parser
import Text.PrettyPrint
import Text.Printf

type VarType = String
type VInfo = (Variable, VarType, Dims)

-- For each variable, returns any data we have about it
-- May want to change to a hash table eventually
vartable :: Matlab -> [VInfo]
vartable [] = []
vartable ((Assign v e p):ms) =
    let ventry = case e of
            Initialize i (d1,d2) ->
                [(v, if d2 == 1 then "Vec" else "Mat", (d1,d2))]
            Scalar d -> [(v,"Scalar",(1,1))]
            _        -> [] -- only initialization adds entry to VInfo
    in ventry ++ (vartable ms)

vlookup :: Variable -> [VInfo] -> Maybe VInfo
vlookup _ [] = Nothing
vlookup v ((vname, vtype, dims):vs) = 
    if v == vname then Just (vname,vtype,dims)
    else vlookup v vs

-- Convert from pretty Matlab to an ugly C program calling PETSc
-- Convert from a Matlab program to a Makefile which can compile the
-- PETSc C program.
disfigure :: Matlab -> String
disfigure m =
    let vtable = vartable m in
    render (
        petscBoilerStart $$ 
        nest 2 (
          declare vtable $$
          helperVars vtable m $$
          disfigure' vtable m $$
          destroy vtable
        ) $$
        petscBoilerEnd
    )

declare vtable =
    vcat [(text vtype) <+> (text vn) <> semi | (vn,vtype,_) <- vtable]

-- The main code generation function
disfigure' :: [VInfo] -> Matlab -> Doc
disfigure' vtable [] = empty
disfigure' vtable ((Assign v e p):ms) =
    disexpr v vtable e $$
    case p of
        Output -> printVar vtable v
        Silent -> empty
    $$
    disfigure' vtable ms

printVar :: [VInfo] -> Variable -> Doc
printVar vtable v = 
    let vtype = case vlookup v vtable of
            Just (_,vt,_) -> vt
            Nothing -> error ("Undefined variable " ++ v)
    in
    text "PetscViewerPushFormat(PETSC_VIEWER_STDOUT_WORLD,PETSC_VIEWER_ASCII_MATLAB);"
    $$
    text (printf "%sView(%s,PETSC_VIEWER_STDOUT_WORLD);" vtype v)
    $$
    text "PetscViewerPopFormat(PETSC_VIEWER_STDOUT_WORLD);"

helperVars _ _ = text "PetscInt i,j;"
-- helperVars [] = empty
-- TODO: Remove helperVars so we don't get multiple ones, below is wrong
-- since it still may put many copies of the helper variables
-- helperVars ((Assign v (Initialize Ones (d1,d2) p):ms) = text "PetscInt i,j;"
-- helperVars (_:ms) = helperVars ms

destroy :: [VInfo] -> Doc
destroy [] = empty
destroy ((v,vtype,_):vs) =
    text (chkerr $ printf "%sDestroy(&%s);" vtype v) $$
    destroy vs



disexpr :: Variable -> [VInfo] -> Expression -> Doc
disexpr v vtable e = 
    let (vtype,dv1,dv2) = case vlookup v vtable of
             Just (_,vt,(d1,d2)) -> (vt,d1,d2)
             Nothing -> error ("Undefined variable " ++ v)
    in
    case e of
        Initialize i (d1,d2) ->
            -- Initialize the variable... better not do this twice in your code!!!
            hsep [(text $ "// " ++ v ++ " ="),
                  (text $ show i),
                  (integer d1), (text "x"), (integer d2)]
            $$ create vtype v
            $$ setName v
            $$ setSizes vtype dv1 dv2 v
            $$ setOptions vtype v
            $$ setUp vtype v
            $$ assemble vtype v dv1 dv2 i
        UnaryOp op e1 ->
            let edoc1 = disexpr v vtable e1 in
            (text "// UNIMPLEMENTED: Saw ") <+> (text $ show op) <+> edoc1
        -- TODO: May want to ensure we only have BinaryOps of 1 nesting level
        -- e.g. C = A * B is okay but D = A * B * C is not, otherwise we run into
        -- the issue of itermediate variables to store A * B (left-associative *)
        -- NOTE: PETSc actually has a MatMatMatMult (D = A * B * C)
        BinaryOp op e1 e2 -> binop vtable v op e1 e2
        Variable s -> text ("// UNIMPLEMENTED: Saw Variable " ++ s)
        Scalar d -> text "// UNIMPLEMENTED: Saw Double" <+> double d

-- Translate a binary operation into PETSc
-- This requires something be in "PETSc normal form". Doubt that one will go
-- in the textbooks, but eventually this should be any operation which is
-- supported by one of the PETSc operations; e.g. MatMult, VecAXPY, etc.
-- See docs/manualpages/Vec/index.html and docs/manualpages/Mat/index.html
binop :: [VInfo] -> Variable -> Operator -> Expression -> Expression -> Doc
binop vtable v op e1 e2 =
    let (lv,lvt,_) =
            case e1 of
            Variable var -> case vlookup var vtable of
                Just vi -> vi
                Nothing -> error ("Unknown variable " ++ var)
            _  -> error "m2petsc only supports expressions of the form Var Op Var"
        (rv,rvt,_) =
            case e2 of
            Variable var -> case vlookup var vtable of
                Just vi -> vi
                Nothing -> error ("Unknown variable " ++ var)
            _  -> error "m2petsc only supports expressions of the form Var Op Var"
        opfunction = case op of
            Times -> if rvt == "Mat"
                     -- MAT_INITIAL_MATRIX might be slow or cause memory leaks
                     then \(c,a,b) -> printf
                         "MatMatMult(%s,%s,MAT_INITIAL_MATRIX,PETSC_DEFAULT,&%s);"
                         a b c
                     else \(y,a,x) -> printf "MatMult(%s,%s,%s);" y a x
            Minus -> \(a,b,c) -> printf "// VecAXPY(%s, -1.0, %s) and %s??;" a b c
            Plus  -> \(a,b,c) -> printf "// VecAXPY(%s, 1.0, %s); and %s??;" a b c
    in text $ opfunction (v,lv,rv)

-- Helper functions for initializing PETSc Variables
create :: String -> String -> Doc
create vtype v = text $ chkerr $
    vtype ++ "Create(PETSC_COMM_WORLD,&" ++ v ++ ");"
setName :: String -> Doc
setName v = text $ chkerr $
    "PetscObjectSetName((PetscObject) "++v++", \""++v++"\");"
setSizes vtype d1 d2 v = text set
    where
    set = if vtype == "Vec"
        then chkerr $ "VecSetSizes("++v++",PETSC_DECIDE," ++ (show d1)++");"
        else chkerr $ "MatSetSizes("++v++",PETSC_DECIDE,PETSC_DECIDE," ++
            (show d1) ++ "," ++ (show d2) ++ ");"

setOptions vtype v = text $ chkerr $ vtype++"SetFromOptions("++v++");"
setUp vtype v = text $ chkerr $ vtype++"SetUp("++v++");"

assemble :: VarType -> Variable -> Integer -> Integer -> Initializer -> Doc
assemble vtype v d1 d2 Zeros = text ("// " ++ v ++ " automatically set to zeros")
assemble vtype v d1 d2 Ones =
    if vtype == "Mat"
    then vcat $ map text [
        "// WARNING: This is probably very inefficient",
        printf
            "for (i=0; i<%s-1; i++) {\n\
            \    for (j=0; j<%s-1; j++) {\n\
            \      ierr = MatSetValue(%s,i,j,(PetscScalar) 1.0,INSERT_VALUES);CHKERRQ(ierr);\n\
            \    }\n\
            \  }" (show d1) (show d2) v,
        printf "MatAssemblyBegin(%s,MAT_FINAL_ASSEMBLY);" v,
        printf "MatAssemblyEnd(%s,MAT_FINAL_ASSEMBLY);" v]
    else text $ chkerr (printf "VecSet(%s,%s);" v "(PetscScalar) 1.0")
assemble vtype v d1 d2 Eye =
    if vtype == "Mat"
    then vcat $ map (text . chkerr) [
        printf "MatShift(%s,(PetscScalar) 1.0);" v,
        printf "MatAssemblyBegin(%s,MAT_FINAL_ASSEMBLY);" v,
        printf "MatAssemblyEnd(%s,MAT_FINAL_ASSEMBLY);" v]
    else vcat $ map (text . chkerr) [
        printf "VecSetValue(%s,0,(PetscScalar) 1.0);" v,
        printf "VecAssemblyBegin(%s);" v,
        printf "VecAssemblyEnd(%s);" v]

-- Wrap a statement in error checking; do this for almost all PETSC statements
chkerr s = "ierr = " ++ s ++ "CHKERRQ(ierr);"

-- A collection of strings and constructors which represent boilerplate,
-- initialization, finalization, and so forth that you have to do in PETSc
petscBoilerStart = text "\
\static char help[] = \"Code automatically generated by m2petsc.\\n\\n\";\n\
\/*T\n\
\   Concepts: m2petsc^basic serial example;\n\
\   Processors: 1\n\
\T*/\n\
\/*\n\
\  Include \"petscksp.h\" so that we can use KSP solvers.  Note that this file\n\
\  automatically includes:\n\
\     petscsys.h    - base PETSc routines   petscvec.h - vectors\n\
\     petscmat.h    - matrices\n\
\     petscis.h     - index sets            petscksp.h - Krylov subspace methods\n\
\     petscviewer.h - viewers               petscpc.h  - preconditioners\n\
\  Note:  The corresponding uniprocessor example is ex23.c\n\
\*/\n\
\#include <petscksp.h>\n\
\int main(int argc,char **args)\n\
\{\n\
\  PetscErrorCode ierr;\n\ 
\  PetscMPIInt    size;\n\
\  ierr = PetscInitialize(&argc,&args,(char*)0,help);if (ierr) return ierr;\n\ 
\  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);\n\ 
\  if (size != 1) SETERRQ(PETSC_COMM_WORLD,1,\"This is a uniprocessor example only!\");\n"

petscBoilerEnd = text "\
\  ierr = PetscFinalize();\n\
\  return ierr;\n\
\}\n"

-- Makefile
build :: Matlab -> String
build m = render $ build' m
build' _ =
    instructions $$
    text "include ${PETSC_DIR}/lib/petsc/conf/variables" $$
    text "include ${PETSC_DIR}/lib/petsc/conf/rules\n" $$
    text "out: out.o" $$
    text "\t-${CLINKER} -o $@ $^ ${PETSC_KSP_LIB}" $$
    text "\t${RM} out.o" $$
    text "clean::" $$ -- XXX: Must do :: since the included makes define clean::
    text "\t${RM} out"
    

instructions = text "# Build PETSc using\n\
\#   http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-3.9.2.tar.gz\n\
\#   ./configure --with-cc=gcc-mp-6 --with-cxx=g++-mp-6 --with-fc=gfortran-mp-6 --download-mpich --download-fblaslapack\n\
\#   make all test\n\
\#   export PETSC_DIR=$(pwd)\n\
\#   e.g. for me it's\n\
\#   export PETSC_DIR=/Users/spollard/Documents/uo/research/matrix-free/petsc-3.8.3"

