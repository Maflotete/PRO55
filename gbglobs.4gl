DATABASE tbsfi
GLOBALS
   DEFINE  t0 RECORD LIKE gbpmt.*,
	   g1	RECORD LIKE gbfim.*,
	   g4   RECORD LIKE ipctl.*,
	   g8	RECORD LIKE iplvt.*,
	   g20  RECORD LIKE adusr.*,
           g2      ARRAY[99] OF RECORD
                   dcrg    CHAR(60), 
                   icrg    FLOAT 
           END RECORD,
	   g_nmod CHAR(30),
	   g_msj  CHAR(90),
	   g_titulo CHAR(50),
	   g_agencia CHAR(50),
	   g_ntraliq INTEGER,
	   g_ntrades INTEGER,
	   g_nprenue INTEGER,
	   gg_spool1 CHAR(10),
	   g_user CHAR(3),
	   g_hora CHAR(8)
END GLOBALS
