(**
 * The exports of this module is automatically exported into the global namespace.
 * This means you can call e.g. "INC" without importing it, and without qualifying
 * the module.
 *)
MODULE STD;

EXTERN ASH*(x, n : LONGINT) : LONGINT
EXTERN CHR*(x : INTEGER) : CHAR
EXTERN COPY*(x : ARRAY OF CHAR; v : ARRAY OF CHAR)
EXTERN COPY*(x : STRING; v : ARRAY OF CHAR)
EXTERN ENTIER*(x : REAL) : LONGINT
EXTERN EXCL*(VAR v : SET; x : INTEGER)
EXTERN HALT*(n : INTEGER)
EXTERN INCL*(VAR v : SET; x : INTEGER)
EXTERN LEN*(v : ARRAY OF ANY; n : INTEGER) : INTEGER
EXTERN LONG*(x : SHORTINT) : INTEGER
EXTERN LONG*(x : INTEGER) : LONGINT
EXTERN LONG*(x : REAL) : LONGREAL
EXTERN MAX*(T : SET) : INTEGER
EXTERN MIN*(T : SET) : INTEGER
EXTERN NEW*(VAR v : POINTER TO ANY)
EXTERN ORD*(x : CHAR) : INTEGER
EXTERN SHORT*(x : LONGINT) : INTEGER
EXTERN SHORT*(x : INTEGER) : SHORTINT
EXTERN SHORT*(x : LONGREAL) : REAL
EXTERN SIZE*(T : ANY) : INTEGER
EXTERN PUTC(x : CHAR)

(* Absolute *)
PROCEDURE ABS*(x : SHORTINT) : SHORTINT;
BEGIN
    IF x < 0 THEN
        RETURN -x
    ELSE
        RETURN x
    END
END ABS

PROCEDURE ABS*(x : INTEGER) : INTEGER;
BEGIN
    IF x < 0 THEN
        RETURN -x
    ELSE
        RETURN x
    END
END ABS

PROCEDURE ABS*(x : LONGINT) : LONGINT;
BEGIN
    IF x < 0 THEN
        RETURN -x
    ELSE
        RETURN x
    END
END ABS

PROCEDURE ABS*(x : REAL) : REAL;
BEGIN
    IF x < 0 THEN
        RETURN -x
    ELSE
        RETURN x
    END
END ABS

PROCEDURE ABS*(x : LONGREAL) : LONGREAL;
BEGIN
    IF x < 0 THEN
        RETURN -x
    ELSE
        RETURN x
    END
END ABS

(**
 * Capitalize
 *
 * If x is a lower-case character 'a'..'z', return the upper-case character 'A'..'Z'
 *
 * @param x
 * @return Capitalized x
 *)
PROCEDURE CAP*(x : CHAR) : CHAR;
BEGIN
    CASE x OF
        'a'..'z': RETURN CHR(ORD(x) - ORD('a') + ORD('A'))
        ELSE RETURN x
    END
END CAP

(* Assert x. If ~x then Halt *)
PROCEDURE ASSERT*(x : BOOLEAN; n : INTEGER);
BEGIN
    IF ~x THEN
        HALT(n)
    END
END ASSERT

PROCEDURE ASSERT*(x : BOOLEAN);
BEGIN
    ASSERT(x, 0)
END ASSERT

(* Decrement x by 1 *)
PROCEDURE DEC*(VAR x : INTEGER);
BEGIN
    x := x - 1
END DEC

PROCEDURE DEC*(VAR x : INTEGER; n : INTEGER);
BEGIN
    x := x - n
END DEC

(* Increment x by 1 *)
PROCEDURE INC*(VAR x : INTEGER);
BEGIN
    x := x + 1
END INC

PROCEDURE INC*(VAR x : INTEGER; n : INTEGER);
BEGIN
    x := x + n
END INC

(* Length of first dimension of array *)
PROCEDURE LEN*(v : ARRAY OF ANY) : INTEGER;
BEGIN
    RETURN LEN(v, 0)
END LEN

(* True if x is odd *)
PROCEDURE ODD*(x : LONGINT) : BOOLEAN;
BEGIN
    RETURN (x MOD 2) = 1
END ODD

PROCEDURE WriteLn*(x : ARRAY OF CHAR);
VAR i : INTEGER;
BEGIN
    FOR i := 0 TO LEN(x) - 1 DO
        PUTC(x[i]);
    END
END WriteLn

END Std.
