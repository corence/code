data PHPValue = PHPString String
              | PHPInt Integer
              | PHPFloat Double
              | PHPBool Bool
              | PHPNull
 
data PHPExpr = Literal PHPValue
             | Variable PHPVariable
             | Assign PHPVariable PHPExpr
             | BinaryExpr BinOp PHPExpr PHPExpr
             | Call FunctionCall [PHPExpr]
             | Isset [PHPVariable]
             | Print PHPExpr
 
data PHPStmt = Seq [PHPStmt]
             | Expression PHPExpr
             | If PHPExpr PHPStmt (Maybe ElseExpr)
             | Function String [FunctionArgumentDef] PHPStmt
             | Return PHPExpr
             | For [PHPExpr] [PHPExpr] [PHPExpr] PHPStmt
             | Echo [PHPExpr]
             | Global PHPVariable
