
proc RamDebugger::CreateImages {} {
    variable images
   
    set images(break) [image create photo -data {
	R0lGODlhCwALAIQAAMQ4OMQ6OsZBQchGRsZCQslLS9NsbNd7e9RubspNTcU6
	OuKfn+q5ueOiotRvb8dDQ/Xd3eu8vNl/f8lJSeSlpdVxccpPT///////////
	/////////////////////////yH5BAEKAB8ALAAAAAALAAsAAAU94CcCJCme
	ZYoCgTAQgUkGhXEgiZIKxsI0jkdqcGBAIpJJioBoRCgVIeDDSjgkFYtuSlI8
	Jo/tKjU9jUqnEAA7
    }]
    set images(dbreak) [image create photo -data {
	R0lGODdhCwALALMAANnZ2dieVuXGouKfn+q5ueOiouO6m/Xd3eu8vNSHU+Slpf//////////
	/////////ywAAAAACwALAAAEPBBIGKSEQEoY5AwAAEoDmFBICYOcYpBi5hTkICgSDFKKIpSY
	QAojoBAhBBBCCCmEAAMAAAY5AwRyhhDCjAA7
    }]
    set images(arrow) [image create photo -data {
	R0lGODlhCwALAIQAAM/Ptbq6ltjYw8zMsb6+nby8ma2thLa2kd7ezb6+nKys
	ga2tgrOzjM3Ns8DAn729m66uhLe3kt/fzrq6l9nZxdDQt///////////////
	/////////////////////////yH5BAEKAB8ALAAAAAALAAsAAAUn4CeOZGme
	H4CKgTAOREwUxoGIiaLvC6N8DYfQ8YBEJKgJZVVZOU0hADs=
    }]
    set images(linecontinue) [image create photo -data {
	    R0lGODlhEAAQAIQYAPwCBCyaXBRiNKzCtNzu3MTSzLzSxIzCjDSmXBxWJETOfBQ+FCSKFDSCNBxS
	    HHSmfJzOnISyjMTexCSCRKzWrFSyXDyKTBxmPP///////////////////////////////yH5BAEK
	    AB8ALAAAAAAQABAAAAVZ4CeOZGkKl2CWQruy7ju26hhcKX19U00KDIWiYllMeisgAkExMnw/xVKy
	    CDAC0JYUEqF+rA2sICBEEByJx+JrWCLK5lpizT64CQZowhYYEAoxMgFYMoWGhSEAOw==
	}]
    set images(arrowbreak) [image create photo -data {
	R0lGODlhCwALAKUAAMQ4OMQ6OsZBQchGRsZCQs/PtclLS9NsbNd7e9Rubrq6
	ltjYw8zMsb6+nby8ma2thLa2kd7ezb6+nKysga2tgrOzjM3Ns8DAn729m66u
	hLe3kt/fzspNTdRvb9l/f9Vxcbq6l9nZxcU6OsdDQ8lJSdDQt///////////
	////////////////////////////////////////////////////////////
	/////////////////////////////////yH5BAEKAD8ALAAAAAALAAsAAAZF
	wJ8QQCQKj8UkEhAQDAgFIzFgOCASikWR0eg2HA9IhCiZmM+UygRgubgvmIxm
	A/gxOR3PBxSSAkQjJCMlRkNJhUd2RUdBADs=
    }]
    set images(arrowdbreak) [image create photo -data {
	R0lGODdhCwALANUAANnZ2dieVs/PtdByUNNsbNd7e9Rubrq6ltjYw8zMsb6+nby8ma2thLa2
	kd7ezb6+nKysga2tgrOzjM3Ns8DAn729m66uhLe3kt/fzs1dT9Rvb9l/f9Vxcbq6l9nZxdSH
	U9DQt///////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////////////////ywAAAAACwALAAAG
	WECAEBgQCoEAoRAYEA4DAACAKBQIAcMBoWA4IIYJhUKhWDAaDuEDCBEOIxJIYEKhUCgVywUT
	AAQCGc2G0/EEAoBAIBD4BECBAAAAAAaEwwAQIBwGAoHAMAgAOw==
    }]
    set images(bookmark) [image create photo actbookmark16 -data {
	    R0lGODlhCwALANUAANnZ2SwqLJuam0RGRKeop5KRkgQCBHt7e6ChoJqZmp6dnpSTlGJhYoCA
	    gJSVlKSjpIiIiGhoaIaGhn5/foiJiIqLilhYWJGQkY+Pj4uLi4GAgVpbWnZ2dnZ3dlZXVlRU
	    VElJSf//////////////////////////////////////////////////////////////////
	    /////////////////////////////////////////////////////////yH5BAEAAAAALAAA
	    AAALAAsAAAZSQIAwIBQKhcJAQCgUCgEBQUAoBAwGgQChEBAKAYYDIqFYBIRCgIHRcDwgAaEQ
	    YIhIJpRKQCgEGICWCyajCQABQqFhw+l4DMOh4QMyDIdCg0EYBAA7
	}]
    
    set images(blank) [image create photo -data {
	R0lGODlhCwALAIAAAP///////yH5BAEKAAEALAAAAAALAAsAAAIKjI+py+0P
	o5yhAAA7
    }]
    set images(file_blue) [image create photo -data {
	R0lGODlhCwANAKEAAAAAAFl48v///////yH5BAEAAAMALAAAAAALAA0AAAIh
	nI8Gy3sQogRCyelgmwNeri0X0H1g0HikZ67ohpZmnKYFADs=
    }]
    set images(file_magenta) [image create photo -data {
	R0lGODlhCwANAKEAAAAAANw35f///////yH5BAEAAAMALAAAAAALAA0AAAIh
	nI8Gy3sQogRCyelgmwNeri0X0H1g0HikZ67ohpZmnKYFADs=
    }]
    set images(file_yellow) [image create photo -data {
	R0lGODlhCwANAKEAAAAAAOi/PP///////yH5BAEAAAMALAAAAAALAA0AAAIh
	nI8Gy3sQogRCyelgmwNeri0X0H1g0HikZ67ohpZmnKYFADs=
    }]
    image create photo filenew16 -data {
       R0lGODlhEAAQAIUAAPwCBFxaXNze3Ly2rJyanPz+/Ozq7GxqbPz6/GxubNTK
       xDQyNIyKhHRydERCROTi3PT29Pz29Pzy7PTq3My2pPzu5PTi1NS+rPTq5PTe
       zMyynPTm1Pz69OzWvMyqjPTu5PTm3OzOtOzGrMSehNTCtNS+tAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAZ/
       QAAgQCwWhUhhQMBkDgKEQFIpKFgLhgMiOl1eC4iEYrtIer+MxsFRRgYe3wLk
       MWC0qXE5/T6sfiMSExR8Z1YRFRMWF4RwYIcYFhkahH6AGBuRk2YCCBwSFZgd
       HR6UgB8gkR0hpJsSGCAZoiEiI4QKtyQlFBQeHrVmC8HCw21+QQAh/mhDcmVh
       dGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBEZXZlbENvciAx
       OTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpodHRwOi8vd3d3LmRl
       dmVsY29yLmNvbQA7
    }
    
    image create photo fileopen16 -data {
       R0lGODlhEAAQAIUAAPwCBAQCBOSmZPzSnPzChPzGhPyuZEwyHExOTFROTFxa
       VFRSTMSGTPT29Ozu7Nze3NTS1MzKzMTGxLy6vLS2tLSytDQyNOTm5OTi5Ly+
       vKyqrKSmpIyOjLR+RNTW1MzOzJyenGxqZBweHKSinJSWlExKTMTCxKyurGxu
       bBQSFAwKDJyanERCRERGRAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAaR
       QIBwGCgGhkhkEWA8HpNPojFJFU6ryitTiw0IBgRBkxsYFAiGtDodDZwPCERC
       EV8sEk0CI9FoOB4BEBESExQVFgEEBw8PFxcYEBIZGhscCEwdCxAPGA8eHxkU
       GyAhIkwHEREQqxEZExUjJCVWCBAZJhEmGRUnoygpQioZGxsnxsQrHByzQiJx
       z3EsLSwWpkJ+QQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9u
       IDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2
       ZWQuDQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo folderopen16 -data {
       R0lGODlhEAAQAIYAAPwCBAQCBExKTBQWFOzi1Ozq7ERCRCwqLPz+/PT29Ozu
       7OTm5FRSVHRydIR+fISCfMTCvAQ6XARqnJSKfIx6XPz6/MzKxJTa9Mzq9JzO
       5PTy7OzizJSOhIyCdOTi5Dy65FTC7HS2zMzm7OTSvNTCnIRyVNza3Dw+PASq
       5BSGrFyqzMyyjMzOzAR+zBRejBxqnBx+rHRmTPTy9IyqvDRylFxaXNze3DRu
       jAQ2VLSyrDQ2NNTW1NTS1AQ6VJyenGxqbMTGxLy6vGRiZKyurKyqrKSmpDw6
       PDw6NAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
       LAAAAAAQABAAAAfCgACCAAECg4eIAAMEBQYCB4mHAQgJCgsLDAEGDQGIkw4P
       BQkJBYwQnRESEREIoRMUE6IVChYGERcYGaoRGhsbHBQdHgu2HyAhGSK6qxsj
       JCUmJwARKCkpKsjKqislLNIRLS4vLykw2MkRMRAGhDIJMzTiLzDXETUQ0gAG
       CgU2HjM35N3AkYMdAB0EbCjcwcPCDBguevjIR0jHDwgWLACBECRIBB8GJekQ
       MiRIjhxEIlBMFOBADR9FIhiJ5OnAEQB+AgEAIf5oQ3JlYXRlZCBieSBCTVBU
       b0dJRiBQcm8gdmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBB
       bGwgcmlnaHRzIHJlc2VydmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20A
       Ow==
    }

    image create photo filesave16 -data {
       R0lGODlhEAAQAIUAAPwCBAQCBFRSVMTCxKyurPz+/JSWlFRWVJyenKSipJSS
       lOzu7ISChISGhIyOjHR2dJyanIyKjHx6fMzOzGRiZAQGBFxeXGRmZHRydGxq
       bAwODOTm5ExOTERGRExKTHx+fGxubNza3Dw+PDQ2NAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
       AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAaA
       QIAQECgOj0jBgFAoBpBHpaFAbRqRh0F1a30ClAhuNZHwZhViqgFhJizSjIZX
       QCAoHOKHYw5xRBiAElQTFAoVQgINFBYXGBkZFxYHGRqIDBQbmRwdHgKeH2Yg
       HpmkIR0HAhFeTqSZIhwCFIdIrBsjAgcPXlBERZ4Gu7xCRZVDfkEAIf5oQ3Jl
       YXRlZCBieSBCTVBUb0dJRiBQcm8gdmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3Ig
       MTk5NywxOTk4LiBBbGwgcmlnaHRzIHJlc2VydmVkLg0KaHR0cDovL3d3dy5k
       ZXZlbGNvci5jb20AOw==
    }

    image create photo acttick16 -data {
	R0lGODlhEAAQAIIAAPwCBMT+xATCBASCBARCBAQCBEQCBAAAACH5BAEAAAAA
	LAAAAAAQABAAAAM2CLrc/itAF8RkdVyVye4FpzUgJwijORCGUhDDOZbLG6Nd
	2xjwibIQ2y80sRGIl4IBuWk6Af4EACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYg
	UHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5OC4gQWxsIHJp
	Z2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }
    image create photo edit16 -data {
	R0lGODlhEAAQAIYAAPwCBFxaVMR+RPzKjNze3AQCBMR6RPzGjPyODPz+/MzO
	zPyKDPyKBPz29OTWzPyGDPyGBOx6BOza1OR2BKROBNSOXKRKBBwOBOzu7PTW
	xPzizOySZPyCDFxaXOy2lNRyRMxmJCQOBPTm1OzStPTKrMR+XIRWLFxGNCQS
	BDQyNIRSNDQuJERGRLyqlNzSvIx6ZKRuVEw6LLSyrLymhKSShBwaFFROTJyW
	jMS+vNzW1OTazNzKrHRqXOzezOTOpPTq3OzWvOTStLyedMS+rLy2pMSynMSu
	lAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAQABAAAAewgAAAAYSFhoQCA4IBBI2OjgUGBwiLBAmXlpcKkgsMlZcJ
	BA0JDpIPEBGVjwkSBgOnExSfmBIVBxAMExYXswkYGRobHLq8gh2PHhoeHyAW
	IYKzIiMkJSYnKCnQg5YNHtQqKywtK9qMBC4vMDEBMjIz2dCMDTQ1Njc4OToz
	5PEEOzw3ZPToMcLHO23HfogQ0QMIkCA+hPBbhAPHECJFjMyYIUQIvEUpUqwQ
	OXKkSEF+AgEAIf5oQ3JlYXRlZCBieSBCTVBUb0dJRiBQcm8gdmVyc2lvbiAy
	LjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBBbGwgcmlnaHRzIHJlc2VydmVk
	Lg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20AOw==
    }
    image create photo actcross16 -data {
	R0lGODlhEAAQAIIAAASC/PwCBMQCBEQCBIQCBAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAQABAAAAMuCLrc/hCGFyYLQjQsquLDQ2ScEEJjZkYfyQKlJa2j7AQn
	MM7NfucLze1FLD78CQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJz
	aW9uIDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVz
	ZXJ2ZWQuDQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo undo-16 -data {
	R0lGODlhEAAQAIUAAPwCBBxSHBxOHMTSzNzu3KzCtBRGHCSKFIzCjLzSxBQ2
	FAxGHDzCLCyeHBQ+FHSmfAwuFBxKLDSCNMzizISyjJzOnDSyLAw+FAQSDAQe
	DBxWJAwmDAQOBKzWrDymNAQaDAQODAwaDDyKTFSyXFTGTEy6TAQCBAQKDAwi
	FBQyHAwSFAwmHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAZ1
	QIBwSCwaj0hiQCBICpcDQsFgGAaIguhhi0gohIsrQEDYMhiNrRfgeAQC5fMC
	AolIDhD2hFI5WC4YRBkaBxsOE2l/RxsHHA4dHmkfRyAbIQ4iIyQlB5NFGCAA
	CiakpSZEJyinTgAcKSesACorgU4mJ6uxR35BACH+aENyZWF0ZWQgYnkgQk1Q
	VG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5OC4g
	QWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29t
	ADs=
    }
    image create photo editcut-16 -data {
	R0lGODlhEAAQAIEAAPwCBAQCBPz+/ISChCH5BAEAAAAALAAAAAAQABAAAAIw
	hI9pwaHrGFRBNDdPlYB3bWHQ1YXPtYln+iCpmqCDp6El7Ylsp6ssR1uYSKuW
	0V8AACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqp
	IERldmVsQ29yIDE5OTcsMTk5OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0
	dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }
    image create photo editcopy-16 -data {
	R0lGODlhEAAQAIUAAFxaXPwCBNze3GxubERCRPz+/Pz29Pzy5OTe3LS2tAQC
	BPTq3PTizLyulKyqrOzexLymhLy+vPTy9OzWvLyifMTCxHRydOzSrLyihPz6
	/OTKpLyabOzu7OTm5MS2nMSqjKSipDQyNJyenLSytOTi5NTS1JyanNTW1JSW
	lLy6vKyurAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAEALAAAAAAQABAAAAaU
	QIBwCAgYj0eAYLkcEJBIZWFaGBie0ICUOnBiowKq4YBIKIbJcGG8YDQUDoHT
	KGU/HhBFpHrVIiQHbQ8TFAoVBRZeSoEIgxcYhhkSAmZKghcXGht6EhwdDmcR
	Hh4NHxgbmwkcCwIgZwqwsbAhCR0CCiIKWQAOCQkjJAolJrpQShK2wicoxVEJ
	KSMqDiAizLuysiF+QQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJz
	aW9uIDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVz
	ZXJ2ZWQuDQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo editpaste-16 -data {
	R0lGODlhEAAQAIUAAPwCBCQiFHRqNIx+LFxSBDw6PKSaRPz+/NTOjKyiZDw+
	POTe3AQCBIR2HPT23Ly2dIR2FMTCxLS2tCQmJKSipExGLHx+fHR2dJyenJya
	nJSSlERCRGRmZNTW1ERGRNze3GxubBweHMzOzJSWlIyOjHRydPz29MzKzIyK
	jPTq3Ly2rLy+vISGhPzy5LymhISChPTizOzWvKyurPTexOzSrDQyNHx6fCwu
	LGxqbOzKpMSabAQGBMS2nLyulMSidAAAACH5BAEAAAAALAAAAAAQABAAAAa7
	QIBQGBAMCMMkoMAsGA6IBKFZECoWDEbDgXgYIIRIRDJZMigUMKHCrlgul7KC
	gcloNJu8fsMpFzoZgRoeHx0fHwsgGyEACiIjIxokhAeVByUmG0snkpIbC5YH
	F4obBREkJCgon5YmKQsqDAUrqiwsrAcmLSkpLrISLC/CrCYOKTAxvgUywhYv
	Gx+6xzM0vjUSNhdvn7zIMdUMNxw4IByKH8fINDk6DABZWTsbYzw9Li4+7UoA
	HvD+4X6CAAAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIu
	NQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQu
	DQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo colorize-16 -data {
	R0lGODlhEAAQAIQAAPz+BAQCBPz+/MTCxISC/AQChMTC/ERCBPyqXMRaBATC
	xASChPzerKSipMT+/MQCBATCBASCBARCRISChMT+xDQyNAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAVnICAGgWie
	ZyCQKGuqZPkGgwyv60gONRDvNlqt9Pv5BgRCYVj8IQ3L4qE4KFiZzRiP90Mg
	Ej+FYtGLIRiJQaMxUDjIxFoi8YBAIo3FQjIJ7iAUERNqDTI6PBGJfT0oFVps
	MSgzkSh+IQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIu
	NQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQu
	DQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo filenew22 -data {
	R0lGODlhFgAWAIUAAPwCBExOTERCRDw6PCwuLBwaHAwODAQCBOze1NTW1OTi
	5Nze3MTGxLS2tJyanPz+/Ozu7OTi3BQSFCwqLDw+PDQyNFRSVPTu7MzKxLyy
	rIR+fCQmJPz6/NTOxPz69Pzy7PTu5Pz29Pzu5PTq5PTm1My6pBQWFPTq3PTm
	3NS+rAwKDPTi1PTezOzWxMy2pPz27PTazOzSvMyynOzaxOzOtPTaxOzKrMyq
	jOzGpMymhPTizOTCpNzSzNTGvMymjMSihCH5BAEAAAAALAAAAAAWABYAAAbo
	QIBwSCwaiYGAYEAgFAqGg/Q4DCASCsTiymgcHAcqQLB4mM+QiIQBppLPcMjk
	wQ4bB2X4maKgt4sVCHpnFhQTElNFE3mDDxcYGRp2RBuMgxwIHX9EBZZwHh8g
	CBmTQ52NISEiIyQlpUImng8hHyInKAgprwAqgnC0IKwrLLpGB4wctLYkwy0u
	uwd9Z8AnJywsLcVFx2YcL7UnJCwwLTEy0GXJoSgrCCwzNDTnxgjeH9UrKzXw
	NDY36LRGhEOwLx4NHDmgJbh3QoeOgv127EhojEeHDj16pEhRQoZHHzl+QJNC
	sqTJSXaCAAAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIu
	NQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQu
	DQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo fileopen22 -data {
	R0lGODlhFgAWAIYAAPwCBAQCBCQWDCwaDDwmFPSubPzGhPzCfPy2dOSmZPzK
	lPzSnPzOlPzKjBQODPzChPzWnPy2bPSmXPyuZOyeXIRSLEQuFEwyHEQqFDQi
	FCweDKRuPFRSTPT29PTy9Ozq7OTi3Nze3NTW1MzOzMTGxMTCxLy6tLSytKyu
	rDQyNMzKxOTm5OTi5Nza1NTS1MTCvLS2tLSyrKSmpJyenJSWlIyKjHx+fFxe
	XBwaHKxuPMzKzLy6vIyOjHx6fDw6NPy6dGxubLy+vISChCQmJNza3KyqrBQS
	FLR2RKSinJyanGxqZAwGBJSSlCwqLAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAWABYAAAf/gACCg4SFhoeDAYqKiIeLj4wBjQCMhY+NkoiLk5qbhQID
	oJyGBAUGBwgEo4MECQoLDA2pDrS1tKQPEAwHERITE77AvxKqhAQNDA8UFRYX
	Fs8YBAQZGqGPxw0RGxwdHR4eHyAhIiMkJSYnKCgpBAYPEhcqHyssLS4kLzAx
	MjM0NTY3cBA4UCAHBw8gVnhgEcKFjhc7UPDj0cMHAAI/KFgY4YLFio/jRpTY
	sW8GDyCSCEQw2DChOHIqgsCQSEPIEEEEJFhAoUNECCJEyOk4d6KIyRtGcB7h
	IJKjixEjHu4oimSGEIs4d8IIUoKECnNB0ElMwkNJJgBLlJBAcQKGiR07KGAU
	RVGViY0mhIwwSTKjr99+THjUoIg0r48hTRIrRtxkiOMhDgrZCQQAIf5oQ3Jl
	YXRlZCBieSBCTVBUb0dJRiBQcm8gdmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3Ig
	MTk5NywxOTk4LiBBbGwgcmlnaHRzIHJlc2VydmVkLg0KaHR0cDovL3d3dy5k
	ZXZlbGNvci5jb20AOw==
    }
    image create photo filesave22 -data {
	R0lGODlhFgAWAIUAAPwCBGxqbAQCBLy+vERCRExKTHRydIyKjMTCxFxaXGRi
	ZFRSVFRWVPz6/Nze3Nzm5Pz+/JyanDw+PExOTHR2dMTGxBQWFLSytHx+fISC
	hOzy9Ly6vAQGBJSWlMzKzAwODJSSlHx6fIyOjOTi5DQ2NISGhGxubCwuLOzq
	7ERGRFxeXNTW1CwqLPT29Dw6PGRmZKSmpAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAb/
	QIBQGBAMj8ikUDAgFAzKKCBwQCQUCcICKh0SEAhGw5EIZAmBrgCxeDQgcDJW
	yz0GIggJfL+XGwQJRxNgC3yGDwwUFUZDFhdthnwMGAZNQwEZFwQakXANBBQb
	HIIdERIBnRAOiR4ERx8gsSEMBBmGCyEGG3YGBwcgIr8UCwQHECOgG4xCtRkE
	JAvBJRklJgkSFBQeJ68hJiEoESkFKiEZIbkGARsLlwEGExENGhorGSkpFAYm
	66NDLAECpGiBYsUIFA8wLHBBQMWLVkdUCFCwaYVFBOymkVCgYEMgOykEpICB
	ccMBAhhELFigTEqAAgIIwCiQ4eRKDyS6EAlJIAI0EpaudF4iIKDAAn9CkRT5
	eMROEAAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0K
	qSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpo
	dHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo wizard-22 -data {
	R0lGODlhFgAWAIQAAPwCBNzaTPz6BAQCBPz+BExKBMzOTPz+rPz+3ISCBPTy
	hISCLISChPz+xOTiVPz+/MTCxKSipKyqrExOTDw+PDQyNAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAV+ICCOZAkE
	ZqoKqoqKAzHAb1sIxhAQwWAQhRaJd0AcdgkhqaBAOBUL5WjAaD6TUqog0DAi
	ldqcg+cDtgaPACTCiM0AOhV6sG4DWOAHnf2uyfV1b1lsgVIwEgwTFHaGA2yK
	FYJgiJCSQo6JFJGGcJSalkKPn5wimZukAJWoIgYhACH+aENyZWF0ZWQgYnkg
	Qk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5
	OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3Iu
	Y29tADs=
    }
    image create photo actundo22 -data {
	R0lGODlhFgAWAIUAAPwCBCReDJzGjMzivOTu3PT69MTivHy+VJTWbIzKZEym
	JESmFESiHDyiHESqLAQCBFzKNGzWPFS2LNTmzCxqDDRqHPz+/KTGnBQqBAQO
	BAwaBESCHHy2XBxGDOzy7HTCTEyyJDSqFHzWTAwSBBQ6BIy+dESKJFySPFSS
	NAwiBCRGFBQmDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAal
	QIBwSCwaj8ikMsBkKotMwYAwEDiXgYLhwD0gCFZiQKxNKBYMRqPh+D6G16y5
	AYnYIxBJAyF4AwITTAUJdBESD4gPFBV6Fn6ABBcJDIYPGEQZGhQbHAIdfx4J
	Hw2VSBodGwWfAR4LDSALfkgYAQurBiAhICKfSSMkvQElGyYnGyi9Rxkdj4nO
	skUYyU9FpxnURikdGtjRKivdRKfQ2Inh5+jpRwZBACH+aENyZWF0ZWQgYnkg
	Qk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5
	OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3Iu
	Y29tADs=
    }
    image create photo editcut22 -data {
	R0lGODlhFgAWAIMAAPwCBAQCBAwCBPz+/OTi5JyanOzq7DQyNGxqbAAAAAAA
	AAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAARbEMhJq704gxBE
	0Bf3cZo4kRJqBQNRfBucyudgvJS6VaxLzyMa6/bLiWA9HOg4VIIkL5vzuRkc
	pkvRIIAorphJLzBW84WEuRZWp6uaT7J2Sh1Hit3OY/ZO7WvsEQAh/mhDcmVh
	dGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBEZXZlbENvciAx
	OTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpodHRwOi8vd3d3LmRl
	dmVsY29yLmNvbQA7
    }
    image create photo editcopy-22 -data {
	R0lGODlhFgAWAIUAAPwCBBQSFPz+/DQyNISChDw6PMzKzMTGxERGRIyKjFxa
	XMTCvKSmpHR2dPz6/Pz29PTq3MS2rPz69MTCxFxWVHx6dJyWjNzSzPz27Pzy
	7Pzu5PTm3NTKvIR+fJyGfHxuZHxqXNTCtPTq5PTi1PTezNS+rExOTFRORMyy
	lPTaxOzWxOzSvNze3NTOxMy2nMyulMyqjAQCBAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAbY
	QIBwSCwahYGkUnk0BgTQ6IAQaBKfUWhBYKhaAU+CgXAQIAyChLeJzSIQhcH6
	GFaM0QtGY5kstqEODw8QEQELAhJTc08KBBMEFBUWDRcBE1pca20SGBkaEBsc
	AY5maFRIAgoLHRQRHh8gIQFlZnByqA8ZGSIQIyQjJQEmYgJ5p2ACrK4gJx4g
	KIZZAgdeAQ4ZI9kjKSor0AwEjeAs1S0cHAslLi4vMDDRWeRIfEsxMeET4ATy
	VoYLC5fizXEiAR84BeMG+pEm8EsAFhAjSlR4hR6fLxiF0AkCACH+aENyZWF0
	ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5
	OTcsMTk5OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2
	ZWxjb3IuY29tADs=
    }
    image create photo editpaste22 -data {
	R0lGODlhFgAWAIYAAPwCBBQWFDw6FHRuFGRaBFxSBAQCBAQKBCQiBIx6HPz6
	/NTOfKyiXDQuFOTm5Pz+/Ozu7PTq5Pz63PTyxNTOjKSeRExGLMTGxMzKzNTS
	1NTW1Dw2NKSmpKyqrKSipJyanNzWlLy6ZLSuVIx6FISChIyKhJSSlCQiJLS2
	tDw6NDQyNCQiFCQmHBQSDGRiZHRydGxubHx6dGxqbFxeXGRmZFxaXCwuLOzq
	7KyurHx+fDwmFEQuFCweFCQWDBQODBwaHBweHKSinJSWlOTi5JyepHR2dDw6
	PBQSFNze3ERGRIyKjIyOjISGhPz29Pzy7MS2rMzOzFRWVHx2dHxybDQiFPz2
	7Pzu5PTq3PTm1NTCtJyGdHxuZHxqXPzq3PTaxNS6pFxWVFRKRNS2nPTi1PTS
	tNSulNzOxNSynMymhAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAWABYAAAf/gACCgwABAgMEBYSLggaOjgcICQoLDA2Pj4MGDg8QEZ4F
	DxITFBUWFxcYGRobjQ8cHR4fCQ8gCyEiFSMWJCUkJieNEB4dKB4pKissK8wr
	LS4vMDHBAAYQHx8dFx0fJDIzNDU0M+IyHzaNNyg43Ng5Ojs7Ojw9Pj9AMkCN
	DiZB/h9CSOx4QLCgihItqBkYgqIDESElitAYWJCgkQcXjjRCgi1Ihw4BB5LA
	QOLCgyQYHihpUU3DBw5ElpAgAYNixSRJjKjQaECDCRPZPDB5IbIGSQwKLnh4
	wbInLA4kmJB4oaPiAwVNnER40hRK1BIAaVatUZJEFCkmpmjgCeWDCalFe4q4
	oFKwSRUrEa5gycLzwq8lUnPQ4PEgSpYcUZ5o2cIlS1O/JHLEDdfjQZMIVrpg
	weLFy5e+M6WSmBGlxYMYYBRzCaOFi5imHWBIfOEiShLTVjaP6eyFTBmN1TA5
	OvLDjJksWb58OVMGDRqWjAYdmU79SIvpjqJr104nEAAh/mhDcmVhdGVkIGJ5
	IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5
	OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpodHRwOi8vd3d3LmRldmVsY29y
	LmNvbQA7
    }
    image create photo find-22 -data {
	R0lGODlhFgAWAIYAAPwCBBQSFJSWlFxaXJyanKy2tLzCxDw+PHyChNTa3Nzq
	7Nz29NTy9Mzu9OT29Nzi5Oz6/Mzy9Kzi7Jze5Lze5Nzy9JyenIyKjHR2dMTu
	9Kzm7JzW3ITW3ISChGxubERGROz+/Lzq9IzW5HzK1LTm7FRSVHTS3GS+zLTS
	1DQ2NMzOzMTq7Lzq7ITW5FS2vMTi5ExOTKTm7EzCzEy2vEyutJTa5FTK1ESi
	rGy+zMzi5ESerESmtITa5OTy9KTe5KyqrAz+/Azi3ASutERCRExKTFRWVAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAWABYAAAf/gACCggGFhYOIAAIDiYUEBQYFAoeJAweKhAgJCgsLDA0O
	CAGJg4wBCA8QDhENEhMUFaKDFhcYgwEJCxAQDBkaGxwSFaMAFh0eAx8AAQIK
	ECAODCEaIiMjJJMAtQMlygEGEBkVva4mJycTKKMYJd0pyyoOKyssGhMtIycu
	Gi/EMB/vlhmoQEFDjHsmZMygUUMdqWUCGgCbYMKEjRk3cEjI9jBADg3Wzs3Q
	sYOHA2IdO1SQkI/GjRMtFhBA2RFBDwk+OASrMPMHkIe3AhBA8QLFzAAHgvyg
	2dEQMSEHPCwltQgooSFSmVpSxKjjh6wPtwINgHVqsVpWESEFeywZ17RkED2s
	40YEgFirlIq4SwvUQCAAIf5oQ3JlYXRlZCBieSBCTVBUb0dJRiBQcm8gdmVy
	c2lvbiAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBBbGwgcmlnaHRzIHJl
	c2VydmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20AOw==
    }
    image create photo find-16 -data {
	R0lGODlhEAAQAIUAAPwCBCQmJDw+PAwODAQCBMza3NTm5MTW1ISChOTy9Mzq
	7Kze5Kzm7HyChOT29Oz6/Nzy9Lzu7JTW3GTCzERCRLza3NTy9Nz29Mzu9Kzq
	9Ize7HTGzHzK1AwKDMTq7JTi7HTW5HzGzKzS1IzW5Hza5FTK1ESyvLTa3HTK
	1GzG1DyqtLzu9IzK1AT+/Dw+RAQGBATCxHRydMTCxAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAaS
	QIAQEBAMhkjBUEAoGA4EJXKJSCQUC0ZDCmg0hATHAxKRTCIEYYMiMFYsF0xG
	s+FUOl0BJRAweCIRHyATIRhpRAEAHSILIyQgJSYhJ4dIBBEoISkmKiuVSQgR
	IyEsEQgELVNCLgQVCiJRLQSfli9pMAQxMrRcQ1G6tAC9AL+7al+qxALACG1K
	w8oxBGt7yWBpfkEAIf5oQ3JlYXRlZCBieSBCTVBUb0dJRiBQcm8gdmVyc2lv
	biAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBBbGwgcmlnaHRzIHJlc2Vy
	dmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20AOw==
    }
    image create photo player_end-22 -data {
	R0lGODlhFgAWAIUAAPwCBDQyNFxeXAQCBMTCxGReZBQSFOzm7AwKDKymrJSS
	lFRSVCwqLLy6vPTy9OTe5Ozq7CQiJLSytOTi5BwaHPz+/HRydMzKzKSepJSO
	lKSipJyanIyGjIyKjKyurISGhMzGzJyWnHR2dISChIyOjLSutDw+PERCRHx6
	fJSWlIR+hJyenGRmZHx2fAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAa6
	QIBwSCwaj8ikMBBQKgOCgRMZIBSkxYHWoDVWD9EigpBQLLBERsPxCA8NDUhj
	gTBGJJNGG02RVBQWdUV3FxgZe0IGfoAGdhoXGxwdiAYef4FGFBoeHB8dGSBR
	ihUhIo1FBhkbIyMkJRYmAwYal4JEBh2RChIWJ1IIGxUZFqdECCgkHR6wWAYp
	FR2YWSobvL5vFgfDaEMDIivMRBEsD9HcQgMWvecDLB0tZ0btsfJa9vLXU/X6
	/P3+b0EAACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41
	DQqpIERldmVsQ29yIDE5OTcsMTk5OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4N
	Cmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }
    image create photo player_stop-22 -data {
	R0lGODlhFgAWAIUAAPwCBAQCBAwKDBQSFBwaHCQmJJSSlISChJSOlJSWlGxq
	bGRiZNTS1PTy9Pz+/Ozm7OTi5FRSVIyKjOTe5MTCxIR+hExOTHR2dLy6vLSy
	tLy2vHRydFxWXIyGjIyOjPz2/FRWVHx6fExKTMzOzJyanKSmpKyqrKSipAQG
	BLSutHx2fDw6PAwODAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAa1
	QIBwSCwaj8ikcslsAgKCAWEQjQ4KgSwyYDAcugZEQqFYYJECA6PhaLcfEEUk
	gJZAGJB8fkKpWOhHAxcOGBQZGBoaGQgbHIBGAhUOGR0SBxISBh4Xf0iCHxQS
	lRIIXhsgj0UCIaCXmJgHGyKpRJ+hmB5dHQqOaCENIx0epBIkBhdzngoPGCQl
	JifQJBvJRygRKRcKGxcXGypys1srEREc5SLnICLiR1koLFVUWfRO9vf4+Uwy
	QQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0KqSBE
	ZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpodHRw
	Oi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo finish-22 -data {
	R0lGODlhFgAWAIUAAPwCBAw2VAQCBBxCXDR+nIS21Aw+XJTC1Nzu/KzO3Pz+
	/Nzq9Pz6/MTe7KTW5FzC1Nzu9CRKZMzi7IzK3Lzi7LTe7HzG3Gy+3AyuzAye
	xFzC3DRSbHy+1Dy61CSqzAySvAyStLze7IzO5AyGrEze7BRmjCTC1ETS3ETa
	5BTC3Bx2nAyWvAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAak
	QIBwSCwaj8hkMqBsBgTN5IAAJQqqykCBasUmDQcEV3gtBs7oATihGJeJgcOC
	QWc0HA8Ig/seRiQTFAsVFhcYGRp6VH1CGwscHQ8dGB4fIBkPIWKMAAMLIiAj
	IJcgH5gkGSWcARIiJicoJikpHikoHqqrKiW8JSogKymoqgCrV8cCARgkuFWc
	RwYeqVjPRgEExEPVRQbZ2l5IBuBRQ0zk5+hRBkEAIf5oQ3JlYXRlZCBieSBC
	TVBUb0dJRiBQcm8gdmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4
	LiBBbGwgcmlnaHRzIHJlc2VydmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5j
	b20AOw==
    }
    image create photo stop-22 -data {
	R0lGODlhFgAWAIUAAASC/CQKDBwKDCwODNyKjPzq7My+vIxiXAQCBOSOjPz6
	/OSelNySjNyGhMR+fLRaTGQ2LPz+/Nx+dNR2bNRybMxuZMxeXMxiZLQSFJQa
	HFwqJNyKhOSCfNyCfNR6dMxmXMxWVMRORLQODOR+fOSSjNR2dMQ2LJQWFMRW
	TLwWDNSCfMxeVLwaFKQODNR+fNx+fMxiXKQSDOSWlMRSTMxaVMQ6NMxORMQy
	JOTS1MxqXLwWFLRORMxKPMQaHMxKTLQWFCH5BAEAAAAALAAAAAAWABYAAAb2
	QIBwSCwajwGBcikIHIsDQmFKNRwQT2EgoVgsGOCG4wHBIgmRhWRCqVQsF0xG
	YyYGNgoGh9PpeCQfICEic3UAAWgLIxwRJBsbHSUREyYYJ3RDAQULexGejhue
	ESgpl3WaCxsqJKKsChEUKywtmFoFDC4vCayikzCyMbWHt38NCTKiHhUfMyzB
	dQIFKsodob0VNDWzwppuKxMRrx6iNjcitNA4bh+iEzkwojc66JkOOxcf7G35
	PBE9KS1MEUGgIQOIFfk++KjRw9wJgUUIZvhRoyLDFCliQDQisUWLGCJOeNx4
	hKCGkyhPGnqCoKVLl1liypyZxUAQACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYg
	UHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5OC4gQWxsIHJp
	Z2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }
    image create photo down-22 -data {
	R0lGODlhFgAWAIUAAPwCBAw2VCRKZDRSbBxCXJTC1Mzi7Nzq9NTm9Bx2nAQC
	BNzu9JzG3Hy+1HzG3IzO5BRmjPz6/LTe7Dy61AyStCTC1FzC1AyGrETS3ETC
	1ETa5BRulAyuzBRylAw+XMTe7Gy+3CSqzAyexBTC3DR+nIS21KTW5Nzu/KzO
	3FzC3Pz+/ByixEze7AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAWABYAAAaR
	QIBwSCwaj8ikcnkMBAQDgjPAFAYKhsMBkVBUAYEFo+F4QLzVQEQyoVTOX/XB
	csHA0+vMRbNBMwkRDhxuHX5GTlIeHh8gISIjFAEeiVRECiQlDAUmgxQjIhwi
	JHdFlycoKSIUFCEjGiGkRpcqCxYijxorsUezcxYsuoZJsxLAu0qXB7DCTJfH
	VQrMX9PU1Uh0QQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9u
	IDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2
	ZWQuDQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo playeject16 -data {
	R0lGODlhEAAQAIAAAPwCBAQCBCH5BAEAAAAALAAAAAAQABAAAAIbhI+py+0R
	3IFQUtruXVqn3kkWyIARR4rqKvoFACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYg
	UHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5OC4gQWxsIHJp
	Z2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }
    image create photo viewmag-16 -data {
	R0lGODlhEAAQAIUAAPwCBCQmJDw+PAwODAQCBMza3NTm5MTW1HyChOTy9Mzq
	7Kze5Kzm7OT29Oz6/Nzy9Lzu7JTW3GTCzLza3NTy9Nz29Ize7HTGzHzK1AwK
	DMTq7Kzq9JTi7HTW5HzGzMzu9KzS1IzW5Iza5FTK1ESyvLTa3HTK1GzGzGzG
	1DyqtIzK1AT+/AQGBATCxHRydMTCxAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAZ+
	QIAQEBAMhkikgFAwHAiC5FCASCQUCwYiKiU0HA9IRAIhSAcTSuXBsFwwk0wy
	YNBANpyOxPMxIzMgCyEiHSMkGCV+SAQQJicoJCllUgBUECEeKhAIBCuUSxMK
	IFArBIpJBCxmLQQuL6eUAFCusJSzr7Kmpl0CtLGLvbW2Zn5BACH+aENyZWF0
	ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5
	OTcsMTk5OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2
	ZWxjb3IuY29tADs=
    }
    image create photo viewmag+16 -data {
	R0lGODlhEAAQAIUAAPwCBCQmJDw+PAwODAQCBMza3NTm5MTW1HyChOTy9Mzq
	7Kze5Kzm7OT29Oz6/Nzy9Lzu7JTW3GTCzLza3NTy9Nz29Ize7HTGzHzK1AwK
	DMTq7Kzq9JTi7HTW5HzGzMzu9KzS1IzW5Iza5FTK1ESyvLTa3HTK1GzGzGzG
	1DyqtIzK1AT+/AQGBATCxHRydMTCxAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAaB
	QIAQEBAMhkikgFAwHAiC5FCASCQUCwYiKiU0HA9IRAIhSAcTSuXBsFwwk0wy
	YNBANpyOxPMxIzMgCyEiHSMkGCV+SAQQJicoJCllUgBUECEeKhAIBCuUSxMK
	IFArBIpJBCxmLQQuL6cAsLECrqeys7WxpqZdtK9Ct8C0fsHAZn5BACH+aENy
	ZWF0ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29y
	IDE5OTcsMTk5OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cu
	ZGV2ZWxjb3IuY29tADs=
    }
    image create photo navup16 -data {
	R0lGODlhEAAQAIUAAPwCBBRObAwSHBRSdISevBRWfAweLNzu/BSOrAQWLPz6
	/FzC3DzW5BxObHTS5ByyzAyixEze7BSStBRWdAyWvByixAQSHCQ2TAQCBBRG
	ZJze7CS61BSavAxefMzq9ETW3CSWtAwmPPz+/CzG1ITC3FyuxBSCnAQeLAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAZf
	QIBwSCwaj8hhQJAkDggFQxMQIBwQhUSyqlgwsFpjg6BwPCARySSstC4eFAqE
	URlYhoMLBpPRUDYcHXt7RgUeFB8gIU0BIoiKjAcUIwiLSQUkJRsmGIwJJwmE
	U6OkfkEAIf5oQ3JlYXRlZCBieSBCTVBUb0dJRiBQcm8gdmVyc2lvbiAyLjUN
	CqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBBbGwgcmlnaHRzIHJlc2VydmVkLg0K
	aHR0cDovL3d3dy5kZXZlbGNvci5jb20AOw==
    }
    image create photo navdown16 -data {
	R0lGODlhEAAQAIUAAPwCBBRObCRKZBxCXAwyTKTK3Ozy/NTm9GSivAQWHNzu
	/FzC3IzO5CySrAQOHAyuzETS3CSWtAyOtETa5Aw2VLze7ByWtBy61BSavAxW
	dBRCXAwqPAQCBDR+nKTe7FS+1Eze7ByixBRmjPz+/AyexAyixAQKFBRqjAQG
	DAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAZe
	QIBwSCwaj0hAYCkYEJLKguGASEADigWj4bgaHpBINykwSCYRa5HCFFQsF0xG
	o9lwhpSOwfORYC4gISJ3RAQdIyQYJSAlImNrh4uNJkl5CoKUUBQnjlB4KJ6h
	okN+QQAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJzaW9uIDIuNQ0K
	qSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVzZXJ2ZWQuDQpo
	dHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo navhome16 -data {
	R0lGODlhEAAQAIUAAPwCBDw6PBQWFCQiJAQCBFxeXMTCxJyanDwyLDQqLFRS
	VLSytJSSlISChCQmJERGRFRWVGxubKSmpJyenGRmZLy+vOzq7OTi5Ly6vGRi
	ZPTy9Pz6/OTm5ExOTPT29BwaHNza3NS6tJRqRGQqBNy6pIyKjDwGBPTe1JSW
	lDQyNOTGrNRiBGwmBIRaLNymdLxWBHxGFNySXCwqLKyqrNR6LKxGBNTS1NTW
	1Jw+BEweDDQ2NAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAao
	QIBwCAgIiEjAgAAoGA6I5DBBUBgWjIZDqnwYGgVIoTGQQgyRiGRCgZCR1nTF
	csFkHm9hBp2paDYbHAsZHW9eERkYGh4eGx4ag3gfSgMTIBshIiMkGyAlCCZT
	EpciJyQjGxcoKUQBEhcbIiorLB4XEltDrhcaLS4vtbcJra8bMDHAGrcyrTMX
	HjA0NSypEsO6EzY3IzU4OdoTzK0BCAkDMgkIOjJlAH5BACH+aENyZWF0ZWQg
	YnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcs
	MTk5OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxj
	b3IuY29tADs=
    }
    image create photo uppercase_lowercase -data {
	R0lGODlhEAAQAJEAANnZ2QAAAP///////yH5BAEAAAAALAAAAAAQABAAAAI9hI+py+1vSAgf
	MSIyjwQBAIigEH5A3AEFAAgAoAAQEQEEgGIDBABQAIAAAAoQABFAQAh2WAQEwcfU5fYnBQA7
    }
    image create photo fileclose16 -data {
	R0lGODlhEAAQAIYAAPwCBCwuLGRmZGReZFxaXFRWVFROVFRSVExOTCwqLFxeXNza3HRydHx6fIyG
	jISChIyOjJSSlJSWlJyanKSipMS+xDw6PNzW3CwmLExKTGxmbGxqbHR2dIR+hIyKjLy2vGRiZHx+
	fKyqrERCRMzOzNze3NTW1LSutFxWXPz+/OTm5OTi5JyWnISGhGxubOzm7Ozq7LSytKyurLS2tDQy
	NJSOlDQuNMTCxLy+vMzKzMzGzOTe5CQmJJyenMTGxNTO1CQeJBweHDQ2NCQiJAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5
	BAEAAAAALAAAAAAQABAAAAfngAAAAQIDBAQFBgcGCAYJggAKCwwNDg8QEBESExQVFoMXGBkHBAoa
	GxwdHR4QHwkgHRmlGxuUHSGsEiIjAyEHIAsktg8lJhETJxYoDgUbFyklHQ8qKSssFCIWBC0KLgwl
	1S8pMJwUMQEHEAK2KykpL9cUMjM0BzUbDdLi4xTYMRU2MkRgoO8djHEiYtzAkeBABFXgyFE4CKNC
	Dh2iJqx6sSOeCBgwbpjIwcNADwcRjvVLWMHHjx05gPCo4GHTyg83clxYkKLBIwsfRJxIOKPCDR06
	djQIAgmDhadCbARIMIRHEAyC/AQCACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24g
	Mi41DQqpIERldmVsQ29yIDE5OTcsMTk5OC4gQWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93
	d3cuZGV2ZWxjb3IuY29tADs=
    }
    image create photo colorize-22 -data {
	R0lGODlhFwAWAIYAAPz+BAQCBBweHPTy7Ozq5ExKRPTu5NzStLy2lCQmJAQG
	BOTezNTKrLyylNTKpOzm3NzWvOzm1MTCpNzaxISuvBSGvLy+pKSehOTexISu
	tKTa7IzS7GSarJTW9HTK7CyazBRypMzGpDw2LOTaxEyKpCSSxARGbERCROTi
	zMS6nDx2jARajIR+bOzi1OTi1MzCnLy6lOSupNx6dGS2rJS6pMy+pOS2pNxG
	TKwaLDSmpHTOzES2tIyuhLSmjPyqrMw6RLSOhLzSrGyqbFSmXJSyhNTOtGTC
	vLzm5CyOjEQ+NPRqbOxOVKyKfMzizKzepFyuZJSqlBR6fCRmZJSKdDw6NLxq
	ZLQiNIwyNLSejDSCRJzWnEyiVHySbBRiZDQyJMSijGS6ZHSCZJyafDRmPISK
	bJyifDw6LAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAXABYAAAf/gACCg4SFhoYBAgGHjIQBAwQEBYwBlYiQAwYGi4UBBwcI
	nIIJA6WRBAqdCwcMDA2pggEEDpUODwSiAAEQnwwHnI8RwBIGsLoLExQVFg4X
	iwEPD6IKGBjAExkaGxUcocfSgwHV1uITFR0eHyAhIroRERLAC/PVIxMkJSUg
	JvHH1RAKTixAgQzZhAkpVKwwwYKWrgnzFrRY4IIgBoMTRryQAGPjs4zzYsgY
	QdIgsgMzaNRw0CCBLgn2bNzAceMgSZITcujYwaNGj2sjcPjw8QNIECFDiEgo
	YuSIDiQOkoSTMAGHkiU4mAhp4uQJDyhRjOSQMiUXFQdVrOC4giWLFidbRri8
	QBKlyxQviIp8udKjRhYwYLKEedFAjJgvucI5cFBEgoMxkMm82PjFmCERix1w
	LCNmcg8viRGZ6aHZcQ8zoRtVWp16UCAAIf5oQ3JlYXRlZCBieSBCTVBUb0dJ
	RiBQcm8gdmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBBbGwg
	cmlnaHRzIHJlc2VydmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20AOw==
    }
    image create photo format-text-bold-16 -data {
	R0lGODlhEAAQAPYAAAAAACBKhyFMiSJMiiJNiiJNiyNOjCNPjCRPjCZTkSdT
	kidUkSlVkitWkSlWlCpXlStZlyxYlixamS1bmC1bmS1bmi9cmS9dmy1cnC9e
	njBgnzJgnTNhnzRinzVjnzJjojNkozRkoTRlozVmpDZmpDZnqTpmoDxppT9r
	pj5xtUBvrUFurEpxpkt1rEN3vUt9vkx8ukp9wEh+xUl/xl6Jv2KHuGWLvHeW
	vkqAxkuAxk+Dx1SHyVaIyVyLxViKylmLyl2Oy16Oy2SNwGeQwGCQy2OSzWaS
	ym+TwG6Vw2yXzW6a0HKay3Gcz3Wby32exnWf0n6k0YKiyYWn0Imr04it1Ymt
	2Yyw1o2x2JSvz52415u53qG+3KrA2azB26zF37PG3ajD4bDH5b/P4MXW7MbW
	7NDd7drl8efu9ujv98zMzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAGkA
	LAAAAAAQABAAAAergGmCg4SFhSAjIIqIIIaCIlFnaGhnTiKOHyRlYENDW18k
	H4YaNWZWISFUXS0ahh5cXkgdHUJTRxyFFSZiVygVFSdMTRcVhBY3WUsUEBAT
	PUQqEoMOEVhQNA7ZDjA8MQ8OggssUkpPVVphY2Q5MxkLggw2SUVBPzs6ODP6
	KQppBw1GgGw4QLAghhkyEhxAsMLHiwICCAko4GJGCQMEAmiMWGiAxgADHIkc
	KTIQADs=
    }
    image create photo format-text-italic-16 -data {
	R0lGODlhEAAQAPYAAAAAACFIhSBKhyRJhiFLiSBNiSFMiSNOiiNOjCNPjCRJ
	kiVQjiVRjidSjydTkSZTkipVkilWlC5RlyhYlSpYlytYlixYlSxZli1Zli1a
	lylcmSxbmS5amC1cmS5dnDNfmzBenCRtkjBgnjFhnjJgnzNmmS5oojJioTJk
	oTNkozZloTdmozVmpDZmpThopkNqoUFspUFup0VyrUp3sU57uFB9s1R8skl/
	xlmAs12Cs0qAxlWIyWGQzGaUzW2SwGiTymmWzmyYz3aYwn2cxnaf03ii0oKj
	zYOo2I+v1o6v2Zey05Ky3Ja23Jm01pi43Zu63aK94aW/4q7F37LH4bjO5sPU
	6MPV6cfY683c7d3o88zMzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAFoA
	LAAAAAAQABAAAAePgFqCg4SFhoUoLCkpLYeDJi5SWVcpjlohK0pVNVQnlipC
	WDEiTyKOIzhYNiQeRR6HHTBWPiAlG0EbhhcfU0YcGloVPBWFEhhNSBkTghE7
	EYQKFkNOMw7WDhQ6DoQQOUw9QERHS1BRNwyDCy9JMgju7w83CIIFDT80CQOF
	BjcGggcCBBAIYMiAAH+WEioUFAgAOw==
    }
    image create photo format-text-underline-16 -data {
	R0lGODlhEAAQAPYAAAAAACBKhyFMiSJMiSJMiiJNiyNNiiNOiyNOjCNPjCVQ
	jCZTkSdTkidUkipWkylXlSpYlitZlyxalytZmC1bmi5bmi1cmi5cmi9cmi1c
	nDBblTBfnDJgnzZjnzFhoDNkozVkoDRlozZkoTZlozVmpDdnpTZnqTtooT5q
	oz5xtUJup0Zzrkhzqkh1r0p1rUN3vUp3sFh9r0h+xVqBtF+GtleGwl6MxlqL
	ym6XynWaxXmbxnGb0HGb0n6hyn+hy3mi03yj1H2k1IajyYanzYOp04Op146w
	2pm11Zy536C93qa/3KrB26nC47LI4bXK4rXM5MPU6M/d7dHg7t3n88zMzAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAFQA
	LAAAAAAQABAAAAeZgFSCgh8kH4Ufg4qDITpTUzohi4ofJVBDQ04liZNUHjNS
	IyNPLh6dVCBLRyAgPTkgnRYnUSwWFipEGxaTGEJKFxMTFTgwFIsPEk00EoIR
	KzYQD4oNMUkoDdgNIjccDYoOPkA7P0VGSEw8KQyDCRpBHQnx8hkyCwmCCi01
	BwKKAgUvTCAQZCBAgAGTCBgkcKqhwYcQI0qcGDEQADs=
    }
    image create photo reload-16 -data {
	R0lGODlhEAAQAIYAAPwCBDzSdDzGdGTWlETOfEzSfGTKjFzOhFTOhFzSjEzS
	hJz2vCyqXHTWlPT+/NT65MT23Lz21KzyxKTuvIzmrDS+bCSiVHzapITytHzy
	rFzulEzmjETafDTCbDSyZCyuXBR+PHTWnMz65HTupGTunFTmjDzafETOdBSC
	RARCHHzipLz61CSaVByOTAx2PCSmVAQ6HJTyvKz6zDyGXAQKBAQeDAxmLCx2
	TEzGfBSGRAQqFARWLByGPBxuNMTu1HzGlITipFS6fGzWlByWTAxSJFy6hFTW
	hDzKdCSeVByKRBx6PBx2PAxeNAReLARKJARGHAQiDAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAA
	LAAAAAAQABAAAAe1gACCg4MBhIeIAgOHBAUGBwcICQUICgsMhA0ODw8QERIT
	FBQNFRaDFw8YGRkaGxwBHR4eHyCCISIjJCUbJicVHgwMKCmCKiskHSwtKCgu
	Li8oMIMxMiYGMzSCNc42LjeDFDgUOTWCOjs2PCg9gj4/QCZBQgUFpRZDNkSC
	RUABAkYFcPzyYIGHDWIAUjA4UgrJkCQolLhYwgRhQgs5bNhownGHxyZOCD1x
	UQ6RyRRQTKpcOchPIAAh/mhDcmVhdGVkIGJ5IEJNUFRvR0lGIFBybyB2ZXJz
	aW9uIDIuNQ0KqSBEZXZlbENvciAxOTk3LDE5OTguIEFsbCByaWdodHMgcmVz
	ZXJ2ZWQuDQpodHRwOi8vd3d3LmRldmVsY29yLmNvbQA7
    }
    image create photo down-16 -data {
	R0lGODlhEAAQAIQAAPwCBHyq9Fyi/FyGzHSq9KTK/ESC7AQ2rFSS9FSO9Dx6
	5EyK9Dx25FSCzFSa/DRy5DyC7DR25Cxm1ESG7Dx67DR67DRqzCRazAQCxAAA
	AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAVEICCOZGme
	aIoGQisMKlHMxqEGBZIodionC0av1CAQHIWF4XFoDkWD2QICiRRqJ8lkS6kw
	UxZDxfs8XciqEaacbrvf/hAAIf5oQ3JlYXRlZCBieSBCTVBUb0dJRiBQcm8g
	dmVyc2lvbiAyLjUNCqkgRGV2ZWxDb3IgMTk5NywxOTk4LiBBbGwgcmlnaHRz
	IHJlc2VydmVkLg0KaHR0cDovL3d3dy5kZXZlbGNvci5jb20AOw==
    }
    image create photo up-16 -data {
	R0lGODlhEAAQAIQAAPwCBGyq/Aw6tIy+/Hyy/HSq/IS+/GSi9FSS7KTO/LTW
	/HSu/Jy+/Eya/JzC9GSe9EyC7FSS9FSK9ER65EyG7ESC7DRy5GyS5AAAAAAA
	AAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAAALAAAAAAQABAAAAVFICCOZGme
	aKoGgjoGA9GqhWEQbBrcRHEgs1IgoVgEfgxGUNRoNhwPBERAXY6eEcnEWmow
	KBULl/R0OC7jUbXqarvfI38IACH+aENyZWF0ZWQgYnkgQk1QVG9HSUYgUHJv
	IHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5OC4gQWxsIHJpZ2h0
	cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }
    image create photo actbookmark22 -data {
	R0lGODlhFgAWAIQAAPwCBCQmJCwqLMTGxAQCBBwaHMTCxLSytERGRFRWVLy+vKyqrKSmpHR2dJSS
	lJyanISGhGxubIyOjKyurGxqbFxeXGRmZHx+fKSipLy6vGRiZLS2tFRSVHRydJSWlHx6fCH5BAEA
	AAAALAAAAAAWABYAAAWWICCOZGmewamaQrq+wUC8azHINGocOI38iIRAceDNaISFYklkGHOEhoNB
	fUAOhuOLEJE8HoPiRKFdESiQBqViuTDIUAsEcyAeGJmyiqC5RCwJGg0YcEh9D0V3Dxt6JwQVDRYV
	HBUdi40mjw0PTgwQHgeYJQQJfxUXFxAOoTkFpQ0fsRSimQkWEQ0VtI62HLt7vjl7JQYhACH+aENy
	ZWF0ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5OC4g
	QWxsIHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }
    image create photo navback22 -data {
	R0lGODlhFgAWAIUAAPwCBAw2VCRGZAxCZGyavExmjHyatOTy9CxihISevPz+/KzO3BRylAw+XAQC
	BDRWbPz6/FzC3CSuzDyexJzO5Mzq9CxSdAQOFISmxNzu9HTS5BSmxAyexDSuzJTa7Mzu9Kzi7GS2
	1CRmjAQOHHSWtLze7AyWvHzG3BRihAQKFCTO3BS+1AyixBSWvBSOtBSStAQWJBSixDzW5BTC3BSq
	zBS21CTC1ETW3AQSHEze7BRqlBRmjAQCDBR+pBRefBRSdCH5BAEAAAAALAAAAAAWABYAAAalQIBw
	SCwaj8ikMqBcMpvHgGAANQYIhWdVGDAcENQtIJBQLBgNx0MQaDuQXcghIplQDhBIxXKJYiAZGhsc
	HR4VHyAhIiNWJBklGhIbJoQnFCcTKIxFKSgbKissJi0mJi4vLiYoMEcXKDEyMzQ1Nje2NisoOEg4
	KDU5K6g6OwwoKAN9SCOeMmgwz884PEq9PT4NYkPLP9jZQikN3d4AKVrjKePp3gZBACH+aENyZWF0
	ZWQgYnkgQk1QVG9HSUYgUHJvIHZlcnNpb24gMi41DQqpIERldmVsQ29yIDE5OTcsMTk5OC4gQWxs
	IHJpZ2h0cyByZXNlcnZlZC4NCmh0dHA6Ly93d3cuZGV2ZWxjb3IuY29tADs=
    }    
}

