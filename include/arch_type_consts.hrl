-if(?TARGET_ARCH =:= mips32).

-define(SIZEOF_CHAR,8).
-define(SIZEOF_SHORT,16).
-define(SIZEOF_INT,32).
-define(SIZEOF_L_INT,64).
-define(SIZEOF_FLOAT,32).
-define(SIZEOF_DOUBLE,64).
-define(SIZEOF_L_DOUBLE,64).
-define(SIZEOF_POINTER,32).

-elif(?TARGET_ARCH =:= amd64).

-define(SIZEOF_CHAR,8).
-define(SIZEOF_SHORT,32).
-define(SIZEOF_INT,64).
-define(SIZEOF_L_INT,128).
-define(SIZEOF_FLOAT,64).
-define(SIZEOF_DOUBLE,64).
-define(SIZEOF_L_DOUBLE,128).
-define(SIZEOF_POINTER,64).

-endif.
